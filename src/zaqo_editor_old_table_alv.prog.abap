*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_table_alv IMPLEMENTATION.
  METHOD get_instance.
    IF iv_level IS INITIAL.
      iv_level = sy-dynnr - 200 + 1.
    ENDIF.

    ro_instance ?= lcl_nested_instance=>get_instance_by_level(
       iv_cl_name = 'LCL_TABLE_ALV'
       iv_level   = iv_level ).
  ENDMETHOD.

  METHOD call_screen.
    DATA:
      lo_struc_desc TYPE REF TO cl_abap_structdescr,
      lr_type       TYPE REF TO data,
      lv_ok         TYPE abap_bool,
      lo_err        TYPE REF TO zcx_aqo_exception,
      ls_sub_field  TYPE REF TO zcl_aqo_helper=>ts_field_desc,
      ls_ui_ext     TYPE zcl_aqo_helper=>ts_field_desc,
      lv_fld_name   TYPE string,
      lt_sub_field  TYPE STANDARD TABLE OF zcl_aqo_helper=>ts_field_desc,
      lv_tabix      TYPE sytabix,
      lv_dynnr      TYPE dynpronr.
    FIELD-SYMBOLS:
      <lt_table_src>  TYPE ANY TABLE,
      <lt_table_dest> TYPE STANDARD TABLE,
      <ls_src>        TYPE any,
      <ls_dest>       TYPE any.

    " Field description
    mv_refresh = abap_true.
    ms_fld_value = is_fld_value.

    " Table to show
    CLEAR mt_sub_field.
    zcl_aqo_helper=>from_json(
     EXPORTING
      iv_json = ms_fld_value->sub_fdesc
     IMPORTING
      ev_ok   = lv_ok
      ex_data = lt_sub_field ).
    IF lv_ok <> abap_true.
      MESSAGE s017(zaqo_message) WITH ms_fld_value->name DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Create new structure
    CLEAR mt_sub_field.
    TRY.
        " Add new field for each table and range
        LOOP AT lt_sub_field REFERENCE INTO ls_sub_field
           WHERE ui_type = zcl_aqo_helper=>mc_ui_table
              OR ui_type = zcl_aqo_helper=>mc_ui_range.
          lv_tabix = sy-tabix + 1.

          " New string field
          CONCATENATE ls_sub_field->name `_UI` INTO lv_fld_name.
          ls_ui_ext = zcl_aqo_helper=>get_field_desc(
           iv_data       = lv_fld_name " type string
           iv_field_name = lv_fld_name ).

          " Add as new subfield
          ls_ui_ext-label = ls_sub_field->label.
          INSERT ls_ui_ext INTO lt_sub_field INDEX lv_tabix.
        ENDLOOP.

        " Add all
        INSERT LINES OF lt_sub_field INTO TABLE mt_sub_field.
        lo_struc_desc = zcl_aqo_helper=>create_structure( it_field_desc = mt_sub_field ).
      CATCH zcx_aqo_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Destination structure (based on source)
    CREATE DATA lr_type TYPE HANDLE lo_struc_desc.
    ASSIGN lr_type->* TO <ls_dest>.
    CREATE DATA mr_table LIKE STANDARD TABLE OF <ls_dest>.

    " Create standard table for alv editing
    ASSIGN mr_table->* TO <lt_table_dest>.

    " Copy row by row form source
    ASSIGN ms_fld_value->cur_value->* TO <lt_table_src>.
    LOOP AT <lt_table_src> ASSIGNING <ls_src>.
      APPEND INITIAL LINE TO <lt_table_dest> ASSIGNING <ls_dest>.
      MOVE-CORRESPONDING <ls_src> TO <ls_dest>.
    ENDLOOP.

    refresh_sub_fields(
     ir_table     = mr_table
     it_sub_field = mt_sub_field ).

    " Show screen
    lv_dynnr = 200 + me->mv_level - 1.
    CALL SCREEN lv_dynnr STARTING AT 5 1.
  ENDMETHOD.                    "call_screen

  METHOD refresh_sub_fields.
    DATA:
      lt_sub_field LIKE it_sub_field,
      ls_sub_field TYPE REF TO zcl_aqo_helper=>ts_field_desc,
      lv_fld_name  TYPE string,
      lv_beg_txt   TYPE string,
      lv_end_txt   TYPE string,
      BEGIN OF ls_range,
        sign   TYPE char1,
        option TYPE char2,
        low    TYPE text255,
        high   TYPE text255,
      END OF ls_range.
    FIELD-SYMBOLS:
      <lt_table_dest> TYPE STANDARD TABLE,
      <ls_dest>       TYPE any,
      <lv_ui_ext>     TYPE string,
      <lt_sub_table>  TYPE ANY TABLE,
      <ls_sub_src>    TYPE any.

    " Create standard table for alv editing
    ASSIGN ir_table->* TO <lt_table_dest>.

    " Show info about sub field
    LOOP AT it_sub_field REFERENCE INTO ls_sub_field
         WHERE ui_type = zcl_aqo_helper=>mc_ui_table
            OR ui_type = zcl_aqo_helper=>mc_ui_range.   "#EC CI_HASHSEQ
      INSERT ls_sub_field->* INTO TABLE lt_sub_field.
    ENDLOOP.

    IF is_sub_field IS NOT INITIAL.
      INSERT is_sub_field INTO TABLE lt_sub_field.
    ENDIF.

    " No need
    CHECK lt_sub_field IS NOT INITIAL.

    " Update each row
    LOOP AT <lt_table_dest> ASSIGNING <ls_dest>.
      LOOP AT lt_sub_field REFERENCE INTO ls_sub_field.
        " Source table
        ASSIGN COMPONENT ls_sub_field->name OF STRUCTURE <ls_dest> TO <lt_sub_table>.

        " Destination string
        CONCATENATE ls_sub_field->name `_UI` INTO lv_fld_name.
        ASSIGN COMPONENT lv_fld_name OF STRUCTURE <ls_dest> TO <lv_ui_ext>.

        " No text
        CLEAR <lv_ui_ext>.

        CASE ls_sub_field->ui_type.
            " Show count
          WHEN zcl_aqo_helper=>mc_ui_table.
            IF <lt_sub_table>[] IS NOT INITIAL.
              <lv_ui_ext> = lines( <lt_sub_table> ).
              CONCATENATE 'Count'(cnt) <lv_ui_ext> INTO <lv_ui_ext> SEPARATED BY space.
            ENDIF.

            " Show all values in range
          WHEN zcl_aqo_helper=>mc_ui_range.
            LOOP AT <lt_sub_table> ASSIGNING <ls_sub_src>.
              MOVE-CORRESPONDING <ls_sub_src> TO ls_range.

              " Do not show SIGN
              IF ls_range-sign = 'I'.
                CLEAR lv_beg_txt.
              ELSE.
                CONCATENATE ls_range-sign `:` INTO lv_beg_txt.
              ENDIF.

              " Do not show HIGH
              IF ls_range-high IS INITIAL.
                CLEAR lv_end_txt.
              ELSE.
                CONCATENATE `:` ls_range-high INTO lv_end_txt.
              ENDIF.

              CONCATENATE <lv_ui_ext> ` ` lv_beg_txt ls_range-option `:` ls_range-low lv_end_txt INTO <lv_ui_ext>.
            ENDLOOP.

            " Delete first ` `
            IF sy-subrc = 0.
              <lv_ui_ext> = <lv_ui_ext>+1.
            ENDIF.

        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    CHECK io_grid IS NOT INITIAL.
    io_grid->refresh_table_display( ).
  ENDMETHOD.

  METHOD show_range.
    DATA:
      ls_tabfld  TYPE rstabfield,
      lv_title   TYPE sytitle,
      lv_display TYPE abap_bool.
    FIELD-SYMBOLS:
      <lt_range>     TYPE STANDARD TABLE.

    " Table name and field
    zcl_aqo_helper=>split_type(
     EXPORTING
       iv_datatype = is_fld_value->rollname
     IMPORTING
       ev_table    = ls_tabfld-tablename
       ev_field    = ls_tabfld-fieldname ).
    CHECK ls_tabfld-fieldname IS NOT INITIAL.

    " Range table
    ASSIGN is_fld_value->cur_value->* TO <lt_range>.

    " Show ranges
    lv_title = is_fld_value->label.
    IF lcl_opt=>is_editable( is_fld_value->is_editable ) <> abap_true.
      lv_display = abap_true.
    ENDIF.

    CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
      EXPORTING
        title         = lv_title
        tab_and_field = ls_tabfld
        just_display  = lv_display
      TABLES
        range         = <lt_range>
      EXCEPTIONS
        OTHERS        = 1.
    CHECK sy-subrc = 0.

    " ok
    rv_update = abap_true.
  ENDMETHOD.

  METHOD pbo.
    DATA:
      lr_cont      TYPE REF TO cl_gui_custom_container,
      lv_name      TYPE text40,
      lt_fieldcat  TYPE lvc_t_fcat,
      ls_fieldcat  TYPE REF TO lvc_s_fcat,
      ls_layout    TYPE lvc_s_layo,
      lv_text      TYPE string,
      ls_variant   TYPE disvariant,
      lv_ind       TYPE i,
      lv_sum       TYPE num4,
      lt_code      TYPE STANDARD TABLE OF syucomm,
      ls_sub_field TYPE REF TO zcl_aqo_helper=>ts_field_desc,
      lv_cnt       TYPE i.
    FIELD-SYMBOLS:
      <lt_table> TYPE STANDARD TABLE.

    " 2 buttons
    IF lcl_opt=>is_editable( ms_fld_value->is_editable ) <> abap_true.
      APPEND 'OK' TO lt_code.
    ENDIF.
    SET PF-STATUS 'OK_CANCEL' EXCLUDING lt_code.

    IF ms_fld_value->label IS NOT INITIAL.
      lv_text = ms_fld_value->label.
    ENDIF.
    SET TITLEBAR 'ST_MAIN' WITH lv_text.

    " One time only
    IF mo_grid IS INITIAL.
      " Header and grid
      CONCATENATE 'EMPTY_' sy-dynnr INTO lv_name.
      CREATE OBJECT:
       lr_cont
        EXPORTING
          container_name = lv_name,

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
    ASSIGN mr_table->* TO <lt_table>.
    zcl_aqo_helper=>create_field_catalog(
     IMPORTING
       et_fieldcat = lt_fieldcat
     CHANGING
       ct_table    = <lt_table> ).

    " Change field catalog
    LOOP AT lt_fieldcat REFERENCE INTO ls_fieldcat.
      ls_fieldcat->edit = lcl_opt=>is_editable( ms_fld_value->is_editable ).

      " Change based options
      READ TABLE mt_sub_field REFERENCE INTO ls_sub_field
       WITH TABLE KEY name = ls_fieldcat->fieldname.
      CHECK sy-subrc = 0.

      " Change text
      IF ls_sub_field->label IS NOT INITIAL.
        ls_fieldcat->coltext = ls_sub_field->label.
      ENDIF.

      " For F4
      IF ls_sub_field->rollname CP '*-*'.
        SPLIT ls_sub_field->rollname AT '-' INTO
         ls_fieldcat->ref_table
         ls_fieldcat->ref_field.
      ENDIF.

      " Show as link
      IF ls_sub_field->ui_type = zcl_aqo_helper=>mc_ui_string.
        ls_fieldcat->hotspot = abap_true.
      ENDIF.
    ENDLOOP.

    " Prepare layout
    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode   = 'C'.

    " Variant
    CONCATENATE p_pack p_opt_id INTO ls_variant-report.

    lv_sum = 0.
    lv_cnt = strlen( ms_fld_value->name ).
    DO lv_cnt TIMES.
      lv_ind = sy-index - 1.
      lv_sum = lv_sum + cl_abap_conv_out_ce=>uccpi( ms_fld_value->name+lv_ind(1) ).
    ENDDO.
    lv_sum = 9999 - lv_sum.
    ls_variant-handle  = lv_sum.

    " Events
    SET HANDLER:
     on_hotspot_click  FOR mo_grid.

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

  METHOD on_hotspot_click.
    DATA:
      lv_level     TYPE i,
      ls_fld_value TYPE REF TO lcl_opt=>ts_fld_value,
      lv_name      TYPE abap_attrname,
      lv_len       TYPE i,
      lv_refresh   TYPE abap_bool.
    FIELD-SYMBOLS:
      <ls_sub_field> LIKE LINE OF mt_sub_field,
      <lt_table>     TYPE STANDARD TABLE,
      <ls_item>      TYPE any,
      <lv_value>     TYPE any.

    " Get current row
    ASSIGN mr_table->* TO <lt_table>.
    READ TABLE <lt_table> ASSIGNING <ls_item> INDEX es_row_no-row_id.
    CHECK sy-subrc = 0.

    " Get from field catalog
    READ TABLE mt_sub_field ASSIGNING <ls_sub_field>
     WITH TABLE KEY name = e_column_id.
    CHECK sy-subrc = 0.

    " Is table in table?
    IF <ls_sub_field>-name CP `*_UI` AND <ls_sub_field>-ui_type = zcl_aqo_helper=>mc_ui_string.
      " Get length by old abap syntax
      lv_len = strlen( <ls_sub_field>-name ).
      lv_len = lv_len - 3.
      lv_name = <ls_sub_field>-name(lv_len).
      READ TABLE mt_sub_field ASSIGNING <ls_sub_field>
       WITH TABLE KEY name = lv_name.
    ENDIF.

    " Next level
    lv_level = me->mv_level + 1.

    " Create reference to data in table
    CREATE DATA ls_fld_value.
    MOVE-CORRESPONDING <ls_sub_field> TO ls_fld_value->*.

    " Edit or not
    ls_fld_value->is_editable = ms_fld_value->is_editable.

    " Data
    ASSIGN COMPONENT ls_fld_value->name OF STRUCTURE <ls_item> TO <lv_value>.
    CHECK <lv_value> IS ASSIGNED.
    GET REFERENCE OF <lv_value> INTO ls_fld_value->cur_value.


    CASE <ls_sub_field>-ui_type.
**********************************************************************
        " Edit string in memo editor
      WHEN zcl_aqo_helper=>mc_ui_string.
        " show editor
        go_string_memo = lcl_string_memo=>get_instance( lv_level ).
        go_string_memo->call_screen( ls_fld_value ).

        " update in table
        IF go_string_memo->mv_last_cmd = 'OK'.
          lv_refresh = abap_true.
        ENDIF.
        go_string_memo = lcl_string_memo=>get_instance( mv_level ).

**********************************************************************
        " Edit sub table in alv editor
      WHEN zcl_aqo_helper=>mc_ui_table.
        " show editor
        go_table_alv = lcl_table_alv=>get_instance( lv_level ).
        go_table_alv->call_screen( ls_fld_value ).

        " update in table
        IF go_table_alv->mv_last_cmd = 'OK'.
          lv_refresh = abap_true.
        ENDIF.

**********************************************************************
        " Show range for table item
      WHEN zcl_aqo_helper=>mc_ui_range.
        lv_refresh = show_range( ls_fld_value ).

      WHEN OTHERS.
        RETURN.
    ENDCASE.

    " Set current again
    go_table_alv = lcl_table_alv=>get_instance( mv_level ).

    " Update value
    IF lv_refresh = abap_true.
      go_table_alv->refresh_sub_fields(
       ir_table     = mr_table
       it_sub_field = mt_sub_field
       io_grid      = mo_grid ).
    ENDIF.
  ENDMETHOD.

  METHOD pai.
    DATA:
      lr_data       TYPE REF TO data,
      lo_tab_desc   TYPE REF TO cl_abap_tabledescr,
      lo_struc_desc TYPE REF TO cl_abap_structdescr,
      lv_close      TYPE abap_bool.
    FIELD-SYMBOLS:
      <lt_table_src>  TYPE STANDARD TABLE,
      <lt_table_dest> TYPE ANY TABLE,
      <lt_temp>       TYPE ANY TABLE,
      <ls_src>        TYPE any,
      <ls_dest>       TYPE any.

    " Save & clear
    mv_last_cmd = cv_cmd.
    CLEAR cv_cmd.

    " Write data back
    mo_grid->check_changed_data( ).

    CASE mv_last_cmd.
      WHEN 'OK'.
        " Source
        ASSIGN mr_table->* TO <lt_table_src>.

        " Destination
        ASSIGN ms_fld_value->cur_value->* TO <lt_table_dest>.

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
          MESSAGE s004(zaqo_message).
          lv_close = abap_true.
        ELSE.
          MESSAGE s005(zaqo_message) WITH ms_fld_value->name DISPLAY LIKE 'E'.
        ENDIF.

      WHEN 'CANCEL'.
        MESSAGE s130(ed) WITH 'Edit'(edt) DISPLAY LIKE 'E'.
        lv_close = abap_true.

    ENDCASE.

    IF lv_close = abap_true.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pbo_0200 OUTPUT.
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
