*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_opt IMPLEMENTATION.
  METHOD pbo.
    DATA:
      ls_button TYPE smp_dyntxt.

    " Make like obligatory
    LOOP AT SCREEN.
      CHECK screen-group1 = 'OBL'.
      screen-required = '2'. " recommended
      MODIFY SCREEN.
    ENDLOOP.

    ls_button-icon_id     = icon_refresh.
    ls_button-icon_text   = TEXT-ui5.
    sscrfields-functxt_01 = ls_button.
  ENDMETHOD.

  METHOD pai.
    CASE cv_cmd.
      WHEN 'ONLI'.
        IF p_object IS INITIAL OR p_sub_ob IS INITIAL.
          MESSAGE e055(00).
        ENDIF.

      WHEN 'FC01'. " Switch to NEW ui
        " Start new one
        zcl_aqo_util=>edit_transaction(
         iv_object    = p_object
         iv_subobject = p_sub_ob
         iv_new_ui    = abap_true ).

      WHEN OTHERS.
        RETURN.
    ENDCASE.

    CLEAR cv_cmd.
  ENDMETHOD.

  METHOD on_f4.
    TYPES:
      BEGIN OF ts_f4,
        object    TYPE ztaqo_data-object,
        subobject TYPE ztaqo_data-subobject,
        uname     TYPE ztaqo_data-uname,
        udate     TYPE ztaqo_data-udate,
        utime     TYPE ztaqo_data-utime,
      END OF ts_f4.
    DATA:
      lt_f4   TYPE STANDARD TABLE OF ts_f4,
      lt_ret  TYPE STANDARD TABLE OF ddshretval,
      ls_ret  TYPE REF TO ddshretval,
      lt_dynp TYPE STANDARD TABLE OF dynpread,
      ls_dynp TYPE REF TO dynpread.

    " Unique items
    SELECT DISTINCT object subobject uname udate utime INTO CORRESPONDING FIELDS OF TABLE lt_f4
    FROM ztaqo_data.
    SORT lt_f4 BY object subobject.

    " Show search help
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield         = iv_field
        dynprofield      = iv_dynpro
        dynpprog         = sy-cprog
        dynpnr           = sy-dynnr
        value_org        = 'S'
        callback_program = sy-repid
        callback_form    = 'CALLBACK_ON_F4'
      TABLES
        value_tab        = lt_f4
        return_tab       = lt_ret
      EXCEPTIONS
        OTHERS           = 3.
    CHECK sy-subrc = 0.

    " Write back
    LOOP AT lt_ret REFERENCE INTO ls_ret.
      APPEND INITIAL LINE TO lt_dynp REFERENCE INTO ls_dynp.
      ls_dynp->fieldvalue = ls_ret->fieldval.

      CASE ls_ret->fieldname.
        WHEN 'F0001'.
          p_object = ls_ret->fieldval.
          SET PARAMETER ID 'ZAQO_OBJECT' FIELD p_object.
          ls_dynp->fieldname = 'P_OBJECT'.

        WHEN 'F0002'.
          p_sub_ob = ls_ret->fieldval.
          SET PARAMETER ID 'ZAQO_SUBOBJECT' FIELD p_sub_ob.
          ls_dynp->fieldname = 'P_SUB_OB'.
      ENDCASE.
    ENDLOOP.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-cprog
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_dynp.
  ENDMETHOD.

  METHOD constructor.
    DATA:
      ls_field_opt   TYPE REF TO zcl_aqo_util=>ts_field_opt,
      ls_fld_opt     TYPE REF TO ts_fld_opt,
      lo_fld_opt_alv TYPE REF TO lcl_fld_opt_alv.

    super->constructor(
      EXPORTING
       iv_object         = p_object
       iv_subobject      = p_sub_ob
       iv_save_last_call = abap_false
      EXCEPTIONS
       OTHERS       = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    " Mandt is open
    mv_is_dev = zcl_aqo_util=>is_dev_mandt( ).

    IF lock( ) <> abap_true.
      mv_read_only = abap_true.

      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " Read saved data
    read( ).

    " Create new table
    LOOP AT mt_field_opt REFERENCE INTO ls_field_opt.
      APPEND INITIAL LINE TO mt_fld_opt REFERENCE INTO ls_fld_opt.
      MOVE-CORRESPONDING ls_field_opt->* TO ls_fld_opt->*.

      CASE ls_fld_opt->kind.
        WHEN zcl_aqo_util=>mc_kind_parameter.
          ls_fld_opt->icon = icon_equal_green.

        WHEN zcl_aqo_util=>mc_kind_select_option.
          ls_fld_opt->icon = icon_interval_include_green.

        WHEN zcl_aqo_util=>mc_kind_table.
          ls_fld_opt->icon = icon_wd_table. "icon_view_table.

        WHEN zcl_aqo_util=>mc_kind_memo.
          ls_fld_opt->icon = icon_change_text.

        WHEN OTHERS.
          MESSAGE e007(zaqo_mes) WITH ls_field_opt->name RAISING unknown_type.

      ENDCASE.

      IF ls_fld_opt->kind = zcl_aqo_util=>mc_kind_table OR
         ls_fld_opt->kind = zcl_aqo_util=>mc_kind_memo.
        ls_fld_opt->value_button = icon_display_more.
      ENDIF.

      " Red color for obsolete fields
      IF ls_fld_opt->is_old = abap_true.
        ls_fld_opt->color_line = 'C610'.
      ENDIF.
    ENDLOOP.

    " Show immediately
    lo_fld_opt_alv = lcl_fld_opt_alv=>get_instance( ).
    IF mv_is_dev = abap_true.
      lo_fld_opt_alv->call_screen( ).
    ELSE.
      " Custom checks
      CHECK lo_fld_opt_alv->data_check( ) = abap_true.
      lo_fld_opt_alv->sel_screen_show( ).
    ENDIF.
  ENDMETHOD.                    "START_OF_SELECTION

  METHOD is_editable.
    CHECK mv_read_only <> abap_true.

    IF iv_editable = abap_true OR mv_is_dev = abap_true.
      rv_editable = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD save.
    DATA:
      ls_fld_opt   TYPE REF TO lcl_opt=>ts_fld_opt,
      ls_field_opt TYPE REF TO zcl_aqo_util=>ts_field_opt.

    CLEAR go_opt->mt_field_opt.
    LOOP AT go_opt->mt_fld_opt REFERENCE INTO ls_fld_opt.
      APPEND INITIAL LINE TO go_opt->mt_field_opt REFERENCE INTO ls_field_opt.
      MOVE-CORRESPONDING ls_fld_opt->* TO ls_field_opt->*.
    ENDLOOP.

    super->save( iv_mandt = iv_mandt ).
  ENDMETHOD.
ENDCLASS.                    "LCL_MAIN IMPLEMENTATION


FORM callback_on_f4
  TABLES   record_tab  STRUCTURE seahlpres
  CHANGING shlp        TYPE shlp_descr
           callcontrol TYPE ddshf4ctrl.
  DATA:
    ls_intf TYPE REF TO ddshiface.

  " Overwrite selectable fields on search help
  CLEAR shlp-interface.

  APPEND INITIAL LINE TO shlp-interface REFERENCE INTO ls_intf.
  ls_intf->shlpfield = 'F0001'.
  ls_intf->valfield  = 'OBJECT'.
  ls_intf->f4field   = abap_true.
  GET PARAMETER ID 'ZAQO_OBJECT' FIELD ls_intf->value.

  APPEND INITIAL LINE TO shlp-interface REFERENCE INTO ls_intf.
  ls_intf->shlpfield = 'F0002'.
  ls_intf->valfield  = 'SUBOBJECT'.
  ls_intf->f4field   = abap_true.
  GET PARAMETER ID 'ZAQO_SUBOBJECT' FIELD ls_intf->value.
ENDFORM.
