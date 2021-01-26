*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_opt IMPLEMENTATION.
  METHOD initialization.
    DATA lv_command TYPE syucomm.

    " se38 or se80 (todo ZAQO_EDITOR)
    IF sy-tcode CP 'SE*'.
      zcx_aqo_exception=>raise_dump( iv_message = 'Please use ZAQO* transactions instead!'(ms2) ).
    ENDIF.

    GET PARAMETER ID 'ZAQO_PACKAGE_ID' FIELD p_pack.
    GET PARAMETER ID 'ZAQO_OPTION_ID'  FIELD p_opt_id.
    GET PARAMETER ID 'ZAQO_COMMAND'    FIELD lv_command.

    " 1 time only
    SET PARAMETER ID 'ZAQO_COMMAND' FIELD ''.

    CHECK p_pack IS NOT INITIAL AND p_opt_id IS NOT INITIAL.

    pai( CHANGING cv_cmd = lv_command ).
  ENDMETHOD.

  METHOD pbo.
    CONSTANTS:
      c_ui_app TYPE sy-cprog VALUE 'SAPLZFG_EUI_SCREEN'.

    CASE sy-dynnr.
      WHEN 1000.
        IF zcl_aqo_helper=>is_in_editor( iv_is_viewer = abap_true ) = abap_true.
          SET TITLEBAR 'TITLE_100' OF PROGRAM c_ui_app WITH 'View option'(vop).
        ELSE.
          SET TITLEBAR 'TITLE_100' OF PROGRAM c_ui_app WITH 'Edit option'(eop).
        ENDIF.

      WHEN 1010 OR 1020 OR 1030 OR 1040.
        zcl_eui_screen=>top_pbo( ).
    ENDCASE.
  ENDMETHOD.

  METHOD pai.
    DATA:
      lo_err TYPE REF TO zcx_aqo_exception.

    " Change menu
    mo_eui_menu = zcl_aqo_option=>get_menu(
      iv_package_id = p_pack
      iv_option_id  = p_opt_id ).

    TRY.
        CASE cv_cmd.
          WHEN 'SAVE'.
            do_save( ).

          WHEN mc_action-new_option.
            start_of_selection( mc_action-new_option ).

          WHEN OTHERS.
            RETURN.
        ENDCASE.

      CATCH zcx_aqo_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    CLEAR cv_cmd.
  ENDMETHOD.

  METHOD start_of_selection.
    " Try to create
    DATA lo_err TYPE REF TO zcx_aqo_exception.
    TRY.
        mo_option = zcl_aqo_option=>create(
           iv_package_id  = p_pack
           iv_option_id   = p_opt_id ).
      CATCH zcx_aqo_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Mandt is open
    mv_is_dev = zcl_aqo_helper=>is_dev_mandt( ).

    IF mo_option->lock( ) <> abap_true.
      mv_read_only = abap_true.

      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " Just show values
    IF zcl_aqo_helper=>is_in_editor( iv_is_viewer = abap_true ) = abap_true.
      mv_read_only = abap_true.
    ENDIF.

    fill_fields( ).

    find_f4_tables( ).

    launch_action( iv_action ).
  ENDMETHOD.

  METHOD fill_fields.
    " Create new table
    CLEAR mt_fld_value.

    DATA lt_skip_field  TYPE stringtab.
    DATA ls_field_value TYPE REF TO zcl_aqo_helper=>ts_field_value.
    DATA lo_err         TYPE REF TO zcx_aqo_exception.

    LOOP AT mo_option->mt_field_value REFERENCE INTO ls_field_value.
      TRY.
          add_one_field( ls_field_value->* ).
        CATCH zcx_aqo_exception INTO lo_err.
          APPEND ls_field_value->name TO lt_skip_field.
          MESSAGE lo_err TYPE 'I'.
      ENDTRY.
    ENDLOOP.

    " Any error during importing?
    CHECK lt_skip_field IS NOT INITIAL.

    DATA lv_fields TYPE string.
    CONCATENATE LINES OF lt_skip_field INTO lv_fields SEPARATED BY `, `.
    MESSAGE s040(zaqo_message) WITH lv_fields DISPLAY LIKE 'E'.
  ENDMETHOD.

  METHOD find_f4_tables.
    DATA ls_field_value TYPE REF TO zcl_aqo_helper=>ts_field_value.

    LOOP AT mo_option->mt_field_value REFERENCE INTO ls_field_value
                                      WHERE ui_type     = zcl_eui_type=>mc_ui_type-table
                                        AND ( table_kind  = cl_abap_tabledescr=>tablekind_sorted
                                           OR table_kind  = cl_abap_tabledescr=>tablekind_hashed )
                                        AND key_defkind = cl_abap_tabledescr=>keydefkind_user
                                        AND unique      = abap_true.  "#EC CI_HASHSEQ
      " Only for relations 1 - 1
      CHECK lines( ls_field_value->key[] ) = 1.

      DATA lr_ft_table TYPE REF TO lvc_s_dral.
      APPEND INITIAL LINE TO mt_f4_tables REFERENCE INTO lr_ft_table.
      lr_ft_table->handle    = 154.
      lr_ft_table->int_value = ls_field_value->name.
      CONCATENATE ls_field_value->name ` - ` ls_field_value->label INTO lr_ft_table->value.
    ENDLOOP.
  ENDMETHOD.

  METHOD launch_action.
    " Choose action
    DATA lv_action LIKE iv_action.
    lv_action = iv_action.
    IF lv_action IS INITIAL.
      CASE mv_is_dev.
        WHEN abap_true.
          lv_action = mc_action-tech_view.

          " Show immediately values
        WHEN abap_false.
          lv_action = mc_action-edit_values.
      ENDCASE.
    ENDIF.

    " Create new option
    IF lv_action = mc_action-new_option.
      lv_action = mc_action-tech_view.
      MESSAGE 'Create new option'(crt) TYPE 'S'.
      " Is not new ?
      IF mo_option->ms_db_item-fields IS NOT INITIAL.
        MESSAGE 'Option already exist'(oae) TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.

    " Decide what to do
    DATA lo_fld_value_alv TYPE REF TO lcl_fld_value_alv.
    lo_fld_value_alv = lcl_fld_value_alv=>get_instance( ).
    CASE lv_action.
      WHEN mc_action-tech_view.
        lo_fld_value_alv->call_screen( ).

      WHEN mc_action-edit_values.
        IF mo_option->ms_db_item-fields IS INITIAL.
          MESSAGE 'Option do not exist'(odn) TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
        " Custom checks
        CHECK lo_fld_value_alv->data_check( ) = abap_true.
        lo_fld_value_alv->sel_screen_show( ).
    ENDCASE.
  ENDMETHOD.

  METHOD add_one_field.
    " Get current value
    DATA lr_value TYPE REF TO data.
    IF ir_data IS NOT INITIAL.
      lr_value = ir_data.
    ELSE.
      lr_value = mo_option->get_field_value( is_field_value-name ).
    ENDIF.

    DATA ls_fld_value     TYPE REF TO ts_fld_value.
    " Paste new data
    APPEND INITIAL LINE TO mt_fld_value REFERENCE INTO ls_fld_value.
    MOVE-CORRESPONDING is_field_value TO ls_fld_value->*.
    ls_fld_value->cur_value = lr_value.

    " Quick edit for all type of fields
    ls_fld_value->value_button = icon_display_more.

    set_icons(
     EXPORTING
       iv_ui_type = ls_fld_value->ui_type
     IMPORTING
       ev_icon    = ls_fld_value->icon
       ev_catalog = ls_fld_value->catalog ).

    " Show history
    IF lines( ls_fld_value->value ) > 1.
      ls_fld_value->history_logs = icon_protocol.
    ENDIF.
  ENDMETHOD.

  METHOD set_icons.
    CLEAR:
     ev_icon,
     ev_catalog.

    CASE iv_ui_type.
      WHEN zcl_eui_type=>mc_ui_type-string.
        ev_icon = icon_change_text.

      WHEN zcl_eui_type=>mc_ui_type-range.
        ev_icon = icon_interval_include_green.

      WHEN zcl_eui_type=>mc_ui_type-table.
        ev_icon = icon_wd_table.
        ev_catalog = icon_catalog. " <--- show field catalog

      WHEN OTHERS.
        ev_icon = icon_equal_green.
    ENDCASE.
  ENDMETHOD.

  METHOD is_editable.
    CHECK mv_read_only <> abap_true
      AND iv_editable  <> abap_undefined.

    IF iv_editable = abap_true OR mv_is_dev = abap_true.
      rv_editable = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD do_save.
    DATA:
      ls_fld_value   TYPE REF TO lcl_opt=>ts_fld_value,
      ls_field_value TYPE zcl_aqo_helper=>ts_field_value,
      lo_err         TYPE REF TO zcx_aqo_exception,
      lv_new_value   TYPE string,
      lv_info        TYPE string.
    FIELD-SYMBOLS:
      <lv_value> TYPE any.
    " IF locked by another user
    CHECK lcl_opt=>mv_read_only <> abap_true.

    CLEAR mo_option->mt_field_value.
    LOOP AT mt_fld_value REFERENCE INTO ls_fld_value.
      CLEAR ls_field_value.
      MOVE-CORRESPONDING ls_fld_value->* TO ls_field_value.

      " As JSON string
      ASSIGN ls_fld_value->cur_value->* TO <lv_value>.
      lv_new_value = zcl_eui_conv=>to_json( <lv_value> ).

      " Add new value
      mo_option->add_history_value(
       EXPORTING
         iv_value       = lv_new_value
       CHANGING
         cs_field_value = ls_field_value ).

      INSERT ls_field_value INTO TABLE mo_option->mt_field_value.
    ENDLOOP.

    TRY.
        lv_info = mo_option->save( ).
        IF lv_info IS NOT INITIAL.
          MESSAGE lv_info TYPE 'S'.
        ENDIF.
      CATCH zcx_aqo_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD on_f4.
    DATA:
      lt_ret  TYPE STANDARD TABLE OF ddshretval,
      ls_ret  TYPE REF TO ddshretval,
      lt_dynp TYPE STANDARD TABLE OF dynpread,
      ls_dynp TYPE REF TO dynpread.

    " Just check code existance
    DO 1 TIMES.
      CHECK iv_code_scan = abap_true.

      " Read from memory
      GET PARAMETER ID 'ZAQO_PACKAGE_ID' FIELD p_pack.
      GET PARAMETER ID 'ZAQO_OPTION_ID'  FIELD p_opt_id.

      CHECK p_pack IS NOT INITIAL AND p_opt_id IS NOT INITIAL.
      code_scan_f4( ).
      RETURN.
    ENDDO.

    " Show SH
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname    = ''             " No need returns all fields in SH exit   "#EC NOTEXT
        fieldname  = ''             "#EC NOTEXT
        searchhelp = 'ZHAQO_OPTION' "#EC NOTEXT
        dynpprog   = sy-repid
        dynpnr     = sy-dynnr
      TABLES
        return_tab = lt_ret
      EXCEPTIONS
        OTHERS     = 5.
    CHECK sy-subrc = 0.

    " Write back
    CLEAR lt_dynp.
    LOOP AT lt_ret REFERENCE INTO ls_ret WHERE fieldname = 'PACKAGE_ID' OR fieldname = 'OPTION_ID'.
      " Update 2 fields
      APPEND INITIAL LINE TO lt_dynp REFERENCE INTO ls_dynp.
      ls_dynp->fieldvalue = ls_ret->fieldval.

      CASE ls_ret->fieldname.
        WHEN 'PACKAGE_ID'.                                  "#EC NOTEXT
          p_pack   = ls_ret->fieldval.
          SET PARAMETER ID 'ZAQO_PACKAGE_ID' FIELD ls_ret->fieldval.
          ls_dynp->fieldname  = 'P_PACK'.

        WHEN 'OPTION_ID'.                                   "#EC NOTEXT
          p_opt_id = ls_ret->fieldval.
          SET PARAMETER ID 'ZAQO_OPTION_ID'  FIELD ls_ret->fieldval.
          ls_dynp->fieldname  = 'P_OPT_ID'.
      ENDCASE.
    ENDLOOP.

    " Update both fields
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-cprog
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_dynp.
  ENDMETHOD.

  METHOD code_scan_f4.
    DATA:
      lt_ret   TYPE STANDARD TABLE OF ddshretval,
      ls_ret   TYPE REF TO ddshretval,
      ls_usage TYPE zcl_aqo_helper=>ts_usage.
    MESSAGE 'Check existence of option by code scanning. For search help use upper field!'(ms1) TYPE 'S' DISPLAY LIKE 'W'.

    " Show SH
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname    = 'ZSAQO_USER_SH'    " No need returns all fields in SH exit   "#EC NOTEXT
        fieldname  = 'INDEX'            "#EC NOTEXT
        searchhelp = 'ZHAQO_USAGE'      "#EC NOTEXT
        dynpprog   = sy-repid
        dynpnr     = sy-dynnr
      TABLES
        return_tab = lt_ret
      EXCEPTIONS
        OTHERS     = 5.
    CHECK sy-subrc = 0.

    " Write back
    LOOP AT lt_ret REFERENCE INTO ls_ret WHERE fieldname = 'INCLUDE' OR fieldname = 'LINE'.
      CASE ls_ret->fieldname.
        WHEN 'INCLUDE'.
          ls_usage-include = ls_ret->fieldval.
        WHEN 'LINE'.
          ls_usage-line = ls_ret->fieldval.
      ENDCASE.
    ENDLOOP.

    IF ls_usage-include IS INITIAL OR ls_usage-line IS INITIAL.
      MESSAGE s015(zaqo_message) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Drilldown
    zcl_aqo_helper=>navigate_to(
     iv_include  = ls_usage-include
     iv_position = ls_usage-line ).
  ENDMETHOD.

  METHOD set_menu_visible.
    CHECK mo_eui_menu IS NOT INITIAL.

    DATA lo_container TYPE REF TO cl_gui_container.
    lo_container = mo_eui_menu->get_container( ).

    CHECK lo_container IS NOT INITIAL.
    lo_container->set_visible( iv_visible ).
  ENDMETHOD.
ENDCLASS.                    "LCL_MAIN IMPLEMENTATION
