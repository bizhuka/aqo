*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_field_setting DEFINITION INHERITING FROM lcl_tab FINAL FRIENDS zcl_eui_event_caller.
  PUBLIC SECTION.
    METHODS:

      pbo REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      _fill_table       REDEFINITION,
      _get_layout       REDEFINITION,
      _get_catalog      REDEFINITION,
      _get_toolbar      REDEFINITION,
      _on_hotspot_click REDEFINITION,
      _on_user_command  REDEFINITION,
      _on_app_event     REDEFINITION,
      _on_data_changed  REDEFINITION,

      _check_data
        CHANGING
          cv_ok TYPE abap_bool.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF mc_button,
        "delete_field  TYPE syucomm VALUE 'DELETE_FIELD',
        add_new_field TYPE syucomm VALUE 'ADD_NEW_FIELD',
        show_all      TYPE syucomm VALUE 'SHOW_ALL',
      END OF mc_button.

*    TYPES:
    METHODS:
      _check_has_history
        CHANGING
          cs_catalog TYPE lvc_s_fcat,

      _add_new_field
        IMPORTING
          io_grid TYPE REF TO cl_gui_alv_grid,

      _edit_1_value
        IMPORTING
          ir_fld_value TYPE REF TO lcl_editor=>ts_fld_value.
ENDCLASS.

CLASS lcl_field_setting IMPLEMENTATION.
  METHOD pbo.
    GET REFERENCE OF go_editor->mt_fld_value INTO mr_table.
    super->pbo( ).
  ENDMETHOD.

  METHOD _get_layout.
    rs_layout = super->_get_layout( ).
    rs_layout-sel_mode = 'A'.
    rs_layout-grid_title = go_editor->get_title( ).
  ENDMETHOD.

  METHOD _fill_table.
    DATA lo_grid TYPE REF TO cl_gui_alv_grid.
    lo_grid = mo_alv->get_grid( ).
    CHECK lo_grid IS NOT INITIAL.

    lo_grid->set_frontend_layout( _get_layout( ) ).

    DATA lt_catalog TYPE lvc_t_fcat.
    lo_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_catalog ).

    DATA: lt_field TYPE STANDARD TABLE OF fieldname,
          lv_field TYPE fieldname.
    SPLIT 'HISTORY_LOGS;LABEL;IS_EDITABLE;ROLLNAME' AT ';' INTO TABLE lt_field.
    LOOP AT lt_field INTO lv_field.
      FIELD-SYMBOLS <ls_catalog> LIKE LINE OF lt_catalog.
      READ TABLE lt_catalog ASSIGNING <ls_catalog>
       WITH KEY fieldname = lv_field.
      CHECK sy-subrc = 0.

      IF <ls_catalog>-fieldname = 'HISTORY_LOGS'.
        _check_has_history( CHANGING cs_catalog = <ls_catalog> ).
        CONTINUE.
      ENDIF.

      <ls_catalog>-edit = go_editor->is_editable( ).
    ENDLOOP.
    lo_grid->set_frontend_fieldcatalog( lt_catalog ).
  ENDMETHOD.

  METHOD _check_has_history.
    cs_catalog-no_out = cs_catalog-tech = abap_true.

    FIELD-SYMBOLS <ls_fld_value> LIKE LINE OF go_editor->mt_fld_value.
    LOOP AT go_editor->mt_fld_value ASSIGNING <ls_fld_value>.
      CHECK lines( <ls_fld_value>-value ) > 1.
      cs_catalog-no_out = cs_catalog-tech = abap_false.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD _get_catalog.
    DATA lv_editable TYPE abap_bool.
    DATA lr_catalog  TYPE REF TO lvc_s_fcat.
    lv_editable = go_editor->is_editable( ).

    add_fcat_field 'ICON' '---'.
    lr_catalog->outputlen = 3.                           "#EC NUMBER_OK

    add_fcat_field 'NAME' ''.
    lr_catalog->outputlen = 15.                          "#EC NUMBER_OK

    add_fcat_field 'UI_TYPE' 'Kind'(knd).
    lr_catalog->outputlen = 8.                           "#EC NUMBER_OK

    add_fcat_field 'LABEL' ''.
    lr_catalog->edit     = lv_editable.
    lr_catalog->col_pos  = 11.                           "#EC NUMBER_OK
    lr_catalog->outputlen = 24.                          "#EC NUMBER_OK

    add_fcat_field 'IS_EDITABLE' ''.
    lr_catalog->edit     = lv_editable.
    lr_catalog->col_pos  = 11.                           "#EC NUMBER_OK
    lr_catalog->outputlen = 12.                          "#EC NUMBER_OK

    add_fcat_field 'ROLLNAME' ''.
    lr_catalog->edit     = lv_editable.
    lr_catalog->col_pos  = 13.                           "#EC NUMBER_OK
    lr_catalog->outputlen = 25.                          "#EC NUMBER_OK

    " Now show for all types
    add_fcat_field 'VALUE_BUTTON' 'Quick edit'(qed).
    lr_catalog->hotspot = abap_true.
    lr_catalog->outputlen = 14.                          "#EC NUMBER_OK
    IF go_editor->mv_is_dev <> abap_true.
      lr_catalog->tech = abap_true.
    ENDIF.

    " If have 'TABLE'
    add_fcat_field 'CATALOG' 'DDIC or Catalog'(cat).
    lr_catalog->hotspot   = abap_true.
    lr_catalog->outputlen = 14.                          "#EC NUMBER_OK

    " If have history
    add_fcat_field 'HISTORY_LOGS'  'View logs'(log).
    lr_catalog->hotspot   = abap_true.
    lr_catalog->outputlen = 16.                          "#EC NUMBER_OK
    _check_has_history( CHANGING cs_catalog = lr_catalog->* ).

    " tech fields
    add_fcat_field '+' ''. " Begin of group
    lr_catalog->tech = abap_true.

    add_fcat_field '+SYS_TYPE'    ''.  add_fcat_field '+LENGTH'      ''.  add_fcat_field '+DECIMALS'    ''.
    add_fcat_field '+TABLE_KIND'  ''.  add_fcat_field '+UNIQUE'      ''.  add_fcat_field '+KEY'         ''.
    add_fcat_field '+KEY_DEFKIND' ''.  add_fcat_field '+SUB_FDESC'   ''.  add_fcat_field '+F4_TABLE'    ''.
  ENDMETHOD.

  METHOD _on_hotspot_click.
    DATA lr_fld_value TYPE REF TO lcl_editor=>ts_fld_value.
    READ TABLE go_editor->mt_fld_value REFERENCE INTO lr_fld_value INDEX e_row_id-index.
    CHECK sy-subrc = 0.

    CASE e_column_id-fieldname.
      WHEN 'CATALOG'.
        IF lr_fld_value->ui_type <> zcl_eui_type=>mc_ui_type-table.
          zcl_aqo_helper=>drill_down( lr_fld_value->rollname ).
          RETURN.
        ENDIF.
        CHECK lr_fld_value->ui_type = zcl_eui_type=>mc_ui_type-table.

        " Prepare IMPORTING params
        DATA: lr_field_desc TYPE REF TO zcl_eui_type=>ts_field_desc,
              lv_editable   TYPE abap_bool.
        GET REFERENCE OF lr_fld_value->field_desc INTO lr_field_desc.
        lv_editable = go_editor->is_editable( lr_fld_value->is_editable ).
        IF go_editor->mv_is_dev <> abap_true.
          lv_editable = abap_false.
        ENDIF.

        DATA lo_table_comp_alv TYPE REF TO lcl_table_comp_alv.
        CREATE OBJECT lo_table_comp_alv
          EXPORTING
            ir_field_desc = lr_field_desc
            iv_editable   = lv_editable.
        CHECK lo_table_comp_alv->show( ) = 'OK'.
        go_editor->sync_screen_ui( iv_message = '' ).

      WHEN 'VALUE_BUTTON'.
        _edit_1_value( lr_fld_value ).

      WHEN 'HISTORY_LOGS'.
        CHECK lines( lr_fld_value->value ) > 1.

        DATA lo_history_logs TYPE REF TO lcl_history_logs.
        CREATE OBJECT lo_history_logs
          EXPORTING
            ir_fld_value = lr_fld_value.
        lo_history_logs->show( ).

    ENDCASE.
  ENDMETHOD.

  METHOD _edit_1_value.
    DATA: lv_editable TYPE abap_bool,
          lv_ok       TYPE abap_bool.
    lv_editable = go_editor->is_editable( ir_fld_value->is_editable ).

    CASE ir_fld_value->ui_type.

      WHEN zcl_eui_type=>mc_ui_type-range.
        " Select option
        DATA lv_read_only TYPE abap_bool.
        IF lv_editable <> abap_true.
          lv_read_only = abap_true.
        ENDIF.
        lv_ok = zcl_eui_screen=>show_range(
          is_field_desc = ir_fld_value->field_desc
          ir_cur_value  = ir_fld_value->cur_value
          iv_read_only  = lv_read_only ).

      WHEN zcl_eui_type=>mc_ui_type-table.
        DATA lo_table_alv TYPE REF TO lcl_table_alv.
        CREATE OBJECT lo_table_alv
          EXPORTING
            ir_fld_value = ir_fld_value.

        CHECK lo_table_alv->show( iv_skip_alv_check = abap_true ) = 'OK'.
        lv_ok = abap_true.

      WHEN zcl_eui_type=>mc_ui_type-string.
        DATA lr_text     TYPE REF TO string.
        DATA lo_memo     TYPE REF TO zcl_eui_memo.

        lr_text ?= ir_fld_value->cur_value.
        CREATE OBJECT lo_memo
          EXPORTING
            ir_text     = lr_text
            iv_editable = lv_editable.
        lo_memo->popup( ).
        CHECK lo_memo->show( ) = 'OK'.
        lv_ok = abap_true.

        " Parameters
      WHEN OTHERS.
        FIELD-SYMBOLS <lv_value> TYPE any.
        ASSIGN ir_fld_value->cur_value->* TO <lv_value>.
        zcl_eui_screen=>edit_in_popup(
         EXPORTING iv_label      = ir_fld_value->label
                   iv_editable   = lv_editable
         CHANGING  cv_ok         = lv_ok
                   cv_value      = <lv_value> ).
    ENDCASE.

    CHECK lv_ok = abap_true.
    go_editor->sync_screen_ui( iv_message = '' ).
  ENDMETHOD.

  METHOD _get_toolbar.
    FIELD-SYMBOLS <ls_button> LIKE LINE OF rt_toolbar.

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function  = mc_button-add_new_field.
    <ls_button>-icon      = icon_insert_row.
    <ls_button>-text      = 'Add new field'(anf).

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    " Use standard ALV button intead of mc_button-delete_field.
    <ls_button>-function  = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    <ls_button>-icon      = icon_delete_row.
    <ls_button>-text      = 'Delete field'(dlf).

    IF go_editor->is_editable( ) <> abap_true.
      LOOP AT rt_toolbar ASSIGNING <ls_button>.
        <ls_button>-disabled = abap_true.
      ENDLOOP.
    ENDIF.

    CHECK go_editor->mv_is_dev = abap_true.
    INSERT INITIAL LINE INTO rt_toolbar INDEX 1 ASSIGNING <ls_button>.
    <ls_button>-butn_type = cntb_btype_sep.

    INSERT INITIAL LINE INTO rt_toolbar INDEX 1 ASSIGNING <ls_button>.
    <ls_button>-function  = mc_button-show_all.
    IF go_editor->is_editable(  ) = abap_true.
      <ls_button>-icon = icon_change.
      <ls_button>-text = 'Edit all'(eda).
    ELSE.
      <ls_button>-icon = 'ICON_DISPLAY'.
      <ls_button>-text = 'View all'(via).
    ENDIF.
  ENDMETHOD.

  METHOD _on_user_command.
    CASE e_ucomm.
      WHEN cl_gui_alv_grid=>mc_fc_loc_delete_row. " mc_button-delete_field.
        go_editor->sync_screen_ui( iv_message = '' ).

      WHEN mc_button-add_new_field.
        _add_new_field( sender ).

      WHEN mc_button-show_all.
        go_editor->make_screen( iv_check_dev = abap_false ).
        CHECK go_editor->show_all( iv_ok_as_save = abap_false ) = 'OK'.
        go_editor->sync_screen_ui( iv_message = '' ).
    ENDCASE.
  ENDMETHOD.

  METHOD _add_new_field.
    DATA lr_data        TYPE REF TO data.
    DATA ls_field_value TYPE zcl_aqo_helper=>ts_field_value.
    DATA lo_err         TYPE REF TO cx_root.
    DATA lv_fname       TYPE zcl_eui_type=>ts_field_desc-name.
    " Get full description
    go_editor->add_new_field_screen(
     IMPORTING
       er_data       = lr_data
       es_field_desc = ls_field_value-field_desc ).
    CHECK ls_field_value-field_desc IS NOT INITIAL.

    " Already exist
    lv_fname = ls_field_value-field_desc-name.
    READ TABLE go_editor->mt_fld_value TRANSPORTING NO FIELDS
     WITH KEY name = lv_fname.
    IF sy-subrc = 0.
      MESSAGE s022(zaqo_message) WITH lv_fname DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    TRY.
        go_editor->add_one_field(
         is_field_value = ls_field_value
         ir_data        = lr_data ).
      CATCH cx_root INTO lo_err.                         "#EC CATCH_ALL
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    io_grid->refresh_table_display( ).
    MESSAGE s032(zaqo_message) WITH lv_fname INTO sy-msgli.
    go_editor->sync_screen_ui( iv_message = sy-msgli ).
  ENDMETHOD.

  METHOD _on_data_changed.
    super->_on_data_changed( sender          = sender
                             er_data_changed = er_data_changed ).
    " Call checks manually. For sync with 'Edit data' tab only
    CHECK er_data_changed IS NOT INITIAL.
    go_editor->sync_screen_ui( iv_message = '' ).
  ENDMETHOD.

  METHOD _on_app_event.
    super->_on_app_event( iv_origin = iv_origin
                          cv_ok     = cv_ok ).

    CHECK iv_origin = mc_event-before_save.
    _check_data( CHANGING cv_ok = cv_ok->* ).
  ENDMETHOD.

  METHOD _check_data.
    DATA lo_log TYPE REF TO zcl_eui_logger.
    CREATE OBJECT lo_log.

    DATA ls_fld_value TYPE REF TO lcl_editor=>ts_fld_value.
    LOOP AT go_editor->mt_fld_value REFERENCE INTO ls_fld_value.
      IF ls_fld_value->label IS INITIAL.
        MESSAGE e001(zaqo_message) WITH ls_fld_value->name INTO sy-msgli.
        lo_log->add( ).
      ENDIF.

**      DATA lt_unq TYPE zcl_eui_type=>tt_unique_type. "#EC NEEDED (just insert)
**      DATA lv_unq TYPE string.
      CHECK ls_fld_value->ui_type <> zcl_eui_type=>mc_ui_type-table
        AND ls_fld_value->ui_type <> zcl_eui_type=>mc_ui_type-string.

      " Have unique name
      TRY.
          " The check is only for FREE SELECTION dialog
***        lv_unq = ls_fld_value->rollname.
***        INSERT lv_unq INTO TABLE lt_unq.
          IF " sy-subrc <> 0 OR
             " Is table and field name
             ls_fld_value->rollname NP '*-*' OR
             " In dictionary
             zcl_eui_type=>create_type_descr( iv_rollname = ls_fld_value->rollname ) IS INITIAL.

            " Specify unique table-field name
            MESSAGE e002(zaqo_message) WITH ls_fld_value->name INTO sy-msgli.
            lo_log->add( ).
          ENDIF.

          DATA lo_error TYPE REF TO zcx_eui_exception.
        CATCH zcx_eui_exception INTO lo_error.
          " Error during ROLLNAME creation
          lo_log->add_exception( io_exception = lo_error ).
      ENDTRY.
    ENDLOOP.

    CHECK lo_log->has_messages( iv_msg_types = zcl_eui_logger=>mc_msg_types-error ) = abap_true.
    cv_ok = abap_false.
    lo_log->show( iv_profile = zcl_eui_logger=>mc_profile-popup ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

MODULE pbo_072 OUTPUT.
  DATA go_field_setting TYPE REF TO lcl_field_setting.  "#EC DECL_MODUL
  IF go_field_setting IS INITIAL.
    CREATE OBJECT go_field_setting.
  ENDIF.

  go_field_setting->pbo( ).
ENDMODULE.

*MODULE pai_072 INPUT.
*  go_field_setting->pai( ).
*ENDMODULE.
