*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_opt IMPLEMENTATION.
  METHOD pbo. "#EC NEEDED

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
      lo_err TYPE REF TO zcx_aqo_exception.

    TRY.
        lcl_opt=>do_create( ).
      CATCH zcx_aqo_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD do_create.
    DATA:
      ls_field_value   TYPE REF TO zcl_aqo_helper=>ts_field_value,
      lo_fld_value_alv TYPE REF TO lcl_fld_value_alv.

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
    LOOP AT mo_option->mt_field_value REFERENCE INTO ls_field_value.
      add_one_field( ls_field_value->* ).
    ENDLOOP.

    " Show immediately
    lo_fld_value_alv = lcl_fld_value_alv=>get_instance( ).
    IF mv_is_dev = abap_true.
      lo_fld_value_alv->call_screen( ).
    ELSE.
      " Custom checks
      CHECK lo_fld_value_alv->data_check( ) = abap_true.
      lo_fld_value_alv->sel_screen_show( ).
    ENDIF.
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
  ENDMETHOD.

  METHOD is_editable.
    CHECK mv_read_only <> abap_true.

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
ENDCLASS.                    "LCL_MAIN IMPLEMENTATION
