*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_opt IMPLEMENTATION.
  METHOD pbo.
    DATA:
      ls_icon TYPE smp_dyntxt.

    " Macro for creating icons
    DEFINE get_icon.
      CLEAR ls_icon.

      ls_icon-icon_id   = &1.
      ls_icon-text      = &2.
      ls_icon-icon_text = &2.
      ls_icon-quickinfo = &3.
      &4                = ls_icon.
    END-OF-DEFINITION.

    get_icon icon_reference_list
             'Where-Used List'(wul) ''
              sscrfields-functxt_01.

    get_icon icon_export
             ''  'Export option'(exp)
             sscrfields-functxt_02.

    IF zcl_aqo_helper=>is_dev_mandt( ) = abap_true.
      get_icon icon_import
               '' 'Import option'(imp)
               sscrfields-functxt_03.
    ENDIF.

    get_icon icon_change_text
             '' 'Change description'(ctd)
             sscrfields-functxt_04.
  ENDMETHOD.

  METHOD pai.
    DATA:
      lo_err TYPE REF TO zcx_aqo_exception.

    TRY.
        CASE cv_cmd.
          WHEN 'SAVE'.
            do_save( iv_mandt = sy-mandt ).

          WHEN 'TRANSPORT'.
            mo_option->transport( ).

          WHEN 'DEL_OPT'.
            mo_option->delete( ).

          WHEN 'FC01'.
            start_of_selection( mc_action_show_usage ).

          WHEN 'FC02'.
            start_of_selection( mc_action_export ).

          WHEN 'FC03'.
            start_of_selection( mc_action_import ).

          WHEN 'FC04'.
            start_of_selection( mc_action_change_description ).

          WHEN OTHERS.
            RETURN.
        ENDCASE.

      CATCH zcx_aqo_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    CLEAR cv_cmd.
  ENDMETHOD.

  METHOD start_of_selection.
    DATA:
      lo_err           TYPE REF TO zcx_aqo_exception,
      ls_field_value   TYPE REF TO zcl_aqo_helper=>ts_field_value,
      lo_fld_value_alv TYPE REF TO lcl_fld_value_alv,
      lv_action        LIKE iv_action.

    " Try to create
    TRY.
        mo_option = zcl_aqo_option=>create(
           iv_package_id  = p_pack
           iv_option_id   = p_opt_id ).

        " Is new
        IF mo_option->ms_db_item-fields IS INITIAL.
          MESSAGE 'Create new option'(crt) TYPE 'S'.
        ENDIF.

        " Mandt is open
        mv_is_dev = zcl_aqo_helper=>is_dev_mandt( ).

        IF mo_option->lock( ) <> abap_true.
          mv_read_only = abap_true.

          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        " Create new table
        CLEAR mt_fld_value.
        LOOP AT mo_option->mt_field_value REFERENCE INTO ls_field_value.
          add_one_field( ls_field_value->* ).
        ENDLOOP.

      CATCH zcx_aqo_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Choose action
    lv_action = iv_action.
    IF lv_action IS INITIAL.
      CASE mv_is_dev.
        WHEN abap_true.
          lv_action = mc_action_tech_view.

          " Show immediately values
        WHEN abap_false.
          lv_action = mc_action_edit_values.
      ENDCASE.
    ENDIF.

    " Decide what to do
    lo_fld_value_alv = lcl_fld_value_alv=>get_instance( ).
    CASE lv_action.
      WHEN mc_action_tech_view.
        lo_fld_value_alv->call_screen( ).

      WHEN mc_action_edit_values.
        " Custom checks
        CHECK lo_fld_value_alv->data_check( ) = abap_true.
        lo_fld_value_alv->sel_screen_show( ).

      WHEN mc_action_show_usage.
        lo_fld_value_alv->find_ref( ).

      WHEN mc_action_export.
        lo_fld_value_alv->export( ).

      WHEN mc_action_import.
        lo_fld_value_alv->import( ).

      WHEN mc_action_change_description.
        lo_fld_value_alv->change_description( ).

    ENDCASE.

  ENDMETHOD.                    "START_OF_SELECTION

  METHOD add_one_field.
    DATA:
      ls_fld_value     TYPE REF TO ts_fld_value.

    APPEND INITIAL LINE TO mt_fld_value REFERENCE INTO ls_fld_value.
    MOVE-CORRESPONDING is_field_value TO ls_fld_value->*.

    " Get current value
    IF ir_data IS INITIAL.
      ls_fld_value->cur_value = mo_option->get_field_value( ls_fld_value->name ).
    ELSE.
      ls_fld_value->cur_value = ir_data.
    ENDIF.

    CASE ls_fld_value->ui_type.
      WHEN zcl_aqo_helper=>mc_ui_string.
        ls_fld_value->icon = icon_change_text.
        ls_fld_value->value_button = icon_display_more.

      WHEN zcl_aqo_helper=>mc_ui_range.
        ls_fld_value->icon = icon_interval_include_green.

      WHEN zcl_aqo_helper=>mc_ui_table.
        ls_fld_value->icon = icon_wd_table.
        ls_fld_value->value_button = icon_display_more.

      WHEN OTHERS.
        ls_fld_value->icon = icon_equal_green.
    ENDCASE.

    " Field catalog
    IF ls_fld_value->ui_type = zcl_aqo_helper=>mc_ui_table.
      ls_fld_value->catalog = icon_catalog.
    ENDIF.

    " Show history
    IF lines( ls_fld_value->value ) > 1.
      ls_fld_value->history_logs = icon_protocol.
    ENDIF.
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
      lv_new_value   TYPE string.
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
      lv_new_value = zcl_aqo_helper=>to_json( <lv_value> ).

      " Add new value
      mo_option->add_history_value(
       EXPORTING
         iv_value       = lv_new_value
       CHANGING
         cs_field_value = ls_field_value ).

      INSERT ls_field_value INTO TABLE mo_option->mt_field_value.
    ENDLOOP.

    TRY.
        mo_option->save( iv_mandt = iv_mandt ).
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
ENDCLASS.                    "LCL_MAIN IMPLEMENTATION
