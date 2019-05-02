*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_logs_alv IMPLEMENTATION.
  METHOD get_instance.
    IF mo_instance IS INITIAL.
      CREATE OBJECT mo_instance.
    ENDIF.
    ro_instance = mo_instance.
  ENDMETHOD.

  METHOD call_screen.
    DATA:
      ls_history_value TYPE zcl_aqo_helper=>ts_history_value,
      lr_history_value TYPE REF TO zcl_aqo_helper=>ts_history_value,
      lt_field_desc    TYPE zcl_aqo_helper=>tt_field_desc,
      ls_field_desc    LIKE LINE OF lt_field_desc,
      ls_field_desc_ui LIKE LINE OF lt_field_desc,
      lo_struc         TYPE REF TO cl_abap_structdescr,
      lr_type          TYPE REF TO data,
      lo_error         TYPE REF TO zcx_aqo_exception.
    FIELD-SYMBOLS:
      <ls_item>       TYPE any,
      <lv_value>      TYPE any,
      <lt_hist_table> TYPE STANDARD TABLE.

    " Field description
    mv_refresh = abap_true.

    " Make copy
    CREATE DATA ms_fld_value.
    ms_fld_value->*           = is_fld_value->*.
    ms_fld_value->is_editable = abap_undefined.

    TRY.
        " 1-st
        ls_field_desc = zcl_aqo_helper=>get_field_desc(
           iv_data       = ls_history_value-changed
           iv_field_name = 'CHANGED' ).
        INSERT ls_field_desc INTO TABLE lt_field_desc.

        " 2-nd
        ls_field_desc = zcl_aqo_helper=>get_field_desc(
           iv_data       = ls_history_value-login
           iv_field_name = 'LOGIN' ).
        INSERT ls_field_desc INTO TABLE lt_field_desc.

        " 3-rd
        ls_field_desc = ms_fld_value->field_desc.
        ls_field_desc-name = '_VALUE'.
        INSERT ls_field_desc INTO TABLE lt_field_desc.

        " 4-th
        IF ls_field_desc-ui_type <> zcl_aqo_helper=>mc_ui_range AND
           ls_field_desc-ui_type <> zcl_aqo_helper=>mc_ui_table.
          CLEAR ls_field_desc.
        ELSE.
          ls_field_desc_ui = zcl_aqo_helper=>get_field_desc(
             iv_data       = ls_history_value-h_value
             iv_field_name = '_VALUE_UI' ).
          INSERT ls_field_desc_ui INTO TABLE lt_field_desc.
        ENDIF.

        " Create structure
        lo_struc = zcl_aqo_helper=>create_structure( it_field_desc = lt_field_desc ).
        CREATE DATA lr_type TYPE HANDLE lo_struc.
        ASSIGN lr_type->* TO <ls_item>.
      CATCH zcx_aqo_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Main table
    CREATE DATA mr_hist_table LIKE STANDARD TABLE OF <ls_item>.
    ASSIGN mr_hist_table->* TO <lt_hist_table>.

    " Fill table
    LOOP AT ms_fld_value->value REFERENCE INTO lr_history_value.
      CLEAR <ls_item>.
      MOVE-CORRESPONDING lr_history_value->* TO <ls_item>.

      " From json
      ASSIGN COMPONENT '_VALUE' OF STRUCTURE <ls_item> TO <lv_value>.
      zcl_aqo_helper=>from_json(
       EXPORTING
        iv_json = lr_history_value->h_value
       IMPORTING
        ex_data = <lv_value> ).

      " Newest on the top
      INSERT <ls_item> INTO <lt_hist_table> INDEX 1.
    ENDLOOP.

    " Table or range
    IF ls_field_desc IS NOT INITIAL.
      lcl_table_alv=>refresh_sub_fields(
       ir_table     = mr_hist_table
       is_sub_field = ls_field_desc ).
    ENDIF.

    " Show screen
    CALL SCREEN 500 STARTING AT 5 1.
  ENDMETHOD.                    "call_screen

  METHOD pbo.
    DATA:
      lr_cont      TYPE REF TO cl_gui_custom_container,
      lt_fieldcat  TYPE lvc_t_fcat,
      ls_fieldcat  TYPE REF TO lvc_s_fcat,
      ls_layout    TYPE lvc_s_layo,
      lv_text      TYPE string,
      lt_code      TYPE STANDARD TABLE OF syucomm.
    FIELD-SYMBOLS:
      <lt_hist_table> TYPE STANDARD TABLE.

    " 1 button
    APPEND 'CANCEL' TO lt_code.
    SET PF-STATUS 'OK_CANCEL' EXCLUDING lt_code.

    IF ms_fld_value->label IS NOT INITIAL.
      lv_text = ms_fld_value->label.
    ENDIF.
    SET TITLEBAR 'ST_MAIN' WITH lv_text.

    " One time only
    IF mo_grid IS INITIAL.
      " Header and grid
      CREATE OBJECT:
       lr_cont
        EXPORTING
          container_name = 'EMPTY_500',

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

      mo_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
    ENDIF.

    " Update data
    CHECK mv_refresh = abap_true.
    mv_refresh = abap_false.

    " Get field catalog
    ASSIGN mr_hist_table->* TO <lt_hist_table>.
    zcl_aqo_helper=>create_field_catalog(
     IMPORTING
       et_fieldcat = lt_fieldcat
     CHANGING
       ct_table    = <lt_hist_table> ).

    " Change field catalog
    LOOP AT lt_fieldcat REFERENCE INTO ls_fieldcat.
      " Show as link
      IF ls_fieldcat->fieldname = '_VALUE_UI'.
        ls_fieldcat->hotspot = abap_true.
      ENDIF.
    ENDLOOP.

    " Prepare layout
    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode   = 'C'.

    " Events
    SET HANDLER:
     on_hotspot_click  FOR mo_grid.

    mo_grid->set_table_for_first_display(
      EXPORTING
        i_save                        = 'A'
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = <lt_hist_table>
        it_fieldcatalog               = lt_fieldcat
      EXCEPTIONS
        OTHERS                        = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "pbo

  METHOD on_hotspot_click.
*    DATA:
*      ls_fld_value      TYPE REF TO lcl_opt=>ts_fld_value,.
    FIELD-SYMBOLS:
      <lt_hist_table> TYPE STANDARD TABLE,
      <ls_hist_table> TYPE ANY,
      <mv_value>      TYPE ANY.

    ASSIGN mr_hist_table->* to <lt_hist_table>.
    CHECK sy-subrc = 0.

    " Current item
    READ TABLE <lt_hist_table> ASSIGNING <ls_hist_table> INDEX es_row_no-row_id.
    CHECK sy-subrc = 0.

    " Get reafarence to data (ms_fld_value is copy to data)
    ASSIGN COMPONENT '_VALUE' OF STRUCTURE <ls_hist_table> to <mv_value>.
    GET REFERENCE OF <mv_value> INTO ms_fld_value->cur_value.

    go_table_alv = lcl_table_alv=>get_instance( 99 ).
    go_table_alv->call_screen( ms_fld_value ).
  ENDMETHOD.

  METHOD pai.
    DATA:
      lv_cmd        LIKE cv_cmd.

    " Save & clear
    lv_cmd = cv_cmd.
    CLEAR cv_cmd.

    CASE lv_cmd.
      WHEN 'OK' OR 'CANCEL'.
        LEAVE TO SCREEN 0.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pbo_0500 OUTPUT.
  go_logs_alv = lcl_logs_alv=>get_instance( ).
  go_logs_alv->pbo( ).
ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_0500 INPUT.
  go_logs_alv = lcl_logs_alv=>get_instance( ).
  go_logs_alv->pai(
   CHANGING
     cv_cmd = gv_ok_code ).
ENDMODULE.
