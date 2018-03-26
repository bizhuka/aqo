*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_table_alv IMPLEMENTATION.
  METHOD get_instance.
    IF mo_instance IS INITIAL.
      CREATE OBJECT mo_instance.
    ENDIF.
    ro_instance = mo_instance.
  ENDMETHOD.

  METHOD call_screen.
    DATA:
      lr_data       TYPE REF TO data,
      lo_tab_desc   TYPE REF TO cl_abap_tabledescr,
      lo_struc_desc TYPE REF TO cl_abap_structdescr,
      lr_type       TYPE REF TO data.
    FIELD-SYMBOLS:
      <lt_table_src>  TYPE ANY TABLE,
      <lt_table_dest> TYPE STANDARD TABLE,
      <ls_src>        TYPE any,
      <ls_dest>       TYPE any.

    " Field description
    mv_refresh = abap_true.
    ms_fld_opt = is_fld_opt.

    " Source table
    lr_data = go_opt->get_field_data( ms_fld_opt->name ).
    ASSIGN lr_data->* TO <lt_table_src>.

    " Destination structure
    CLEAR mr_table.
*    IF go_opt->mo_ui_ext IS NOT INITIAL.
*      TRY.
*          mr_table = go_opt->mo_ui_ext->create_field_type(
*           iv_name = ms_fld_opt->name
*           iv_type = zif_prog_params_ui_ext=>mc_type_ui ).
*        CATCH cx_sy_dyn_call_illegal_method.
*          CLEAR mr_table.
*      ENDTRY.
*    ENDIF.

    " Destination structure (based on source)
    IF mr_table IS INITIAL.
      lo_tab_desc ?= cl_abap_tabledescr=>describe_by_data( <lt_table_src> ).
      lo_struc_desc ?= lo_tab_desc->get_table_line_type( ).
      CREATE DATA lr_type TYPE HANDLE lo_struc_desc.

      ASSIGN lr_type->* TO <ls_dest>.
      CREATE DATA mr_table LIKE STANDARD TABLE OF <ls_dest>.
    ENDIF.

    " Create standard table for alv editing
    ASSIGN mr_table->* TO <lt_table_dest>.

    " Copy row by row
    LOOP AT <lt_table_src> ASSIGNING <ls_src>.
      APPEND INITIAL LINE TO <lt_table_dest> ASSIGNING <ls_dest>.
      MOVE-CORRESPONDING <ls_src> TO <ls_dest>.
    ENDLOOP.

    " Show screen
    CALL SCREEN 200 STARTING AT 5 1.
  ENDMETHOD.                    "call_screen

  METHOD pbo.
    DATA:
      lr_cont     TYPE REF TO cl_gui_custom_container,
      lt_fieldcat TYPE lvc_t_fcat,
      ls_fieldcat TYPE REF TO lvc_s_fcat,
      ls_layout   TYPE lvc_s_layo,
      lv_text     TYPE string,
      ls_variant  TYPE disvariant,
      lv_ind      TYPE i,
      lv_sum      TYPE num4,
      lt_code     TYPE STANDARD TABLE OF syucomm,
      lt_subcomps TYPE zcl_aqo_util=>tt_comp,
      ls_subcomp  TYPE REF TO zcl_aqo_util=>ts_comp,
      lv_ok       TYPE ABAP_BOOL,
      lv_cnt      TYPE i.
    FIELD-SYMBOLS:
      <lt_table> TYPE STANDARD TABLE.

    " 2 buttons
    IF go_opt->is_editable( ms_fld_opt->edit ) <> abap_true.
      APPEND 'OK' TO lt_code.
    ENDIF.
    SET PF-STATUS 'OK_CANCEL' EXCLUDING lt_code.

    IF ms_fld_opt->text IS NOT INITIAL.
      lv_text = ms_fld_opt->text.
    ENDIF.
    SET TITLEBAR 'ST_MAIN' WITH lv_text.

    " One time only
    IF mo_grid IS INITIAL.
      " Header and grid
      CREATE OBJECT:
       lr_cont
        EXPORTING
          container_name = 'EMPTY_200',

      " Show at first SCREEN
       mo_grid
        EXPORTING
          i_parent = lr_cont
        EXCEPTIONS
          OTHERS   = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RETURN.
      ENDIF.

*      SET HANDLER:
*       on_data_changed  FOR mo_grid,
*       on_hotspot_click FOR mo_grid.

      mo_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
    ENDIF.

    " Update data
    CHECK mv_refresh = abap_true.
    mv_refresh = abap_false.

    " Get field catalog
    ASSIGN mr_table->* TO <lt_table>.
    zcl_aqo_util=>create_field_catalog(
     IMPORTING
       et_fieldcat = lt_fieldcat
     CHANGING
       ct_table    = <lt_table> ).

*    " Fill virtual fields
*    IF go_opt->mo_ui_ext IS NOT INITIAL.
*      TRY.
*          go_opt->mo_ui_ext->table_data_changed(
*            iv_name = ms_fld_opt->name
*            ir_data = mr_table ).
*        CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*      ENDTRY.
*    ENDIF.

    " Table to show
    zcl_aqo_util=>from_json(
     EXPORTING
      iv_json = ms_fld_opt->subcomps
     IMPORTING
      ev_ok   = lv_ok
      ex_data = lt_subcomps ).
    IF lv_ok <> abap_true.
      MESSAGE s017(zaqo_mes) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Change field catalog
    SORT lt_subcomps BY name.
    LOOP AT lt_fieldcat REFERENCE INTO ls_fieldcat.
      ls_fieldcat->edit = go_opt->is_editable( ms_fld_opt->edit ).

      " Change based options
      READ TABLE lt_subcomps REFERENCE INTO ls_subcomp
       WITH KEY name = ls_fieldcat->fieldname BINARY SEARCH.
      IF sy-subrc = 0.
        " Change text
        IF ls_subcomp->text IS NOT INITIAL.
          ls_fieldcat->coltext = ls_subcomp->text.
        ENDIF.

        " For F4
        IF ls_subcomp->rollname CP '*-*'.
          SPLIT ls_subcomp->rollname AT '-' INTO
           ls_fieldcat->ref_table
           ls_fieldcat->ref_field.
        ENDIF.
      ENDIF.

*      " Change based extension
*      CHECK go_opt->mo_ui_ext IS NOT INITIAL.
*      TRY.
*          go_opt->mo_ui_ext->change_field_catalog(
*           EXPORTING
*             iv_name     = ms_fld_opt->name
*           CHANGING
*             cs_fieldcat = ls_fieldcat->* ).
*        CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*      ENDTRY.
    ENDLOOP.

    " Prepare layout
    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode   = 'C'.

    " Variant
    ls_variant-report  = p_object.

    lv_sum = 0.
    lv_cnt = strlen( ms_fld_opt->name ).
    DO lv_cnt TIMES.
      lv_ind = sy-index - 1.
      lv_sum = lv_sum + cl_abap_conv_out_ce=>uccpi( ms_fld_opt->name+lv_ind(1) ).
    ENDDO.
    lv_sum = 9999 - lv_sum.
    ls_variant-handle  = lv_sum.

    mo_grid->set_table_for_first_display(
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = <lt_table>
        it_fieldcatalog               = lt_fieldcat
      EXCEPTIONS
        OTHERS                        = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "pbo

  METHOD pai.
    DATA:
      lr_data       TYPE REF TO data,
      lv_cmd        TYPE syucomm,
      lo_tab_desc   TYPE REF TO cl_abap_tabledescr,
      lo_struc_desc TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS:
      <lt_table_src>  TYPE STANDARD TABLE,
      <lt_table_dest> TYPE ANY TABLE,
      <lt_temp>       TYPE ANY TABLE,
      <ls_src>        TYPE any,
      <ls_dest>       TYPE any.

    " Save & clear
    lv_cmd = cv_cmd.
    CLEAR cv_cmd.

    " Write data back
    mo_grid->check_changed_data( ).

    CASE lv_cmd.
      WHEN 'OK'.
        " Source
        ASSIGN mr_table->* TO <lt_table_src>.

        " Destination
        lr_data = go_opt->get_field_data( ms_fld_opt->name ).
        ASSIGN lr_data->* TO <lt_table_dest>.

        " Create new table for safety
        lo_tab_desc ?= cl_abap_typedescr=>describe_by_data( <lt_table_dest> ).
        CREATE DATA lr_data TYPE HANDLE lo_tab_desc .
        ASSIGN lr_data->* TO <lt_temp>.

        " Field
        lo_struc_desc ?= lo_tab_desc->get_table_line_type( ).
        CREATE DATA lr_data TYPE HANDLE lo_struc_desc.
        ASSIGN lr_data->* TO <ls_dest>.

        " And copy back
        LOOP AT <lt_table_src>  ASSIGNING <ls_src>.
          CLEAR <ls_dest>.
          MOVE-CORRESPONDING <ls_src> TO <ls_dest>.
          INSERT <ls_dest> INTO TABLE <lt_temp>.
        ENDLOOP.

        IF lines( <lt_table_src> ) = lines( <lt_temp> ).
          <lt_table_dest> = <lt_temp>.

          " Go baack
          MESSAGE s004(zaqo_mes).
          LEAVE TO SCREEN 0.
        ELSE.
          MESSAGE s005(zaqo_mes) WITH ms_fld_opt->name DISPLAY LIKE 'E'.
        ENDIF.

      WHEN 'CANCEL'.
        MESSAGE s130(ed) WITH TEXT-edt DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0.

    ENDCASE.
  ENDMETHOD.                    "pai

*  METHOD on_data_changed.
*    " Fill virtual fields
*    CHECK go_opt->mo_ui_ext IS NOT INITIAL.
*
*    TRY.
*        go_opt->mo_ui_ext->table_data_changed(
*          iv_name         = ms_fld_opt->name
*          ir_data         = mr_table
*          io_data_changed = er_data_changed ).
*      CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*    ENDTRY.
*  ENDMETHOD.
*
*  METHOD on_hotspot_click.
*    " Fill virtual fields
*    CHECK go_opt->mo_ui_ext IS NOT INITIAL.
*
*    TRY.
*        go_opt->mo_ui_ext->table_hotspot_click(
*         EXPORTING
*          iv_name         = ms_fld_opt->name
*          ir_data         = mr_table
*          io_gui_alv_grid = sender
*          iv_column_id    = e_column_id
*          is_row_no       = es_row_no ).
*      CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*    ENDTRY.
*  ENDMETHOD.
ENDCLASS.                    "lcl_table_alv IMPLEMENTATION

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pbo_0200 OUTPUT.
  DATA:
    go_table_alv TYPE REF TO lcl_table_alv.
  go_table_alv = lcl_table_alv=>get_instance( ).
  go_table_alv->pbo( ).
ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_0200 INPUT.
  go_table_alv = lcl_table_alv=>get_instance( ).
  go_table_alv->pai(
   CHANGING
     cv_cmd = gv_ok_code ).
ENDMODULE.
