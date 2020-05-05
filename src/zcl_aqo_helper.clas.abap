class ZCL_AQO_HELPER definition
  public
  final
  create private .

public section.
*"* public components of class ZCL_AQO_HELPER
*"* do not include other source files here!!!
  type-pools ABAP .

  types:
    abap_attrname_tab TYPE HASHED TABLE OF abap_attrname WITH UNIQUE KEY table_line .
  types:
    BEGIN OF ts_history_value,
        changed TYPE sydatum,
        login   TYPE syuname,
        h_value TYPE string,
      END OF ts_history_value .
  types:
    tt_history_value TYPE SORTED TABLE OF ts_history_value WITH UNIQUE KEY changed .
  types:
    BEGIN OF ts_field_value,
        value       TYPE tt_history_value,
        is_editable TYPE zdaqo_edit_in_prod.
        INCLUDE TYPE zcl_eui_type=>ts_field_desc AS field_desc.
    TYPES:
    END OF ts_field_value .
  types:
    tt_field_value TYPE HASHED TABLE OF ts_field_value WITH UNIQUE KEY name .
  types:
    BEGIN OF ts_usage,
        index      TYPE syindex,
        package_id TYPE devclass,
        option_id  TYPE zdaqo_option_id,
        include    TYPE wbcrossgt-include,
        line       TYPE i,
        meth       TYPE seocpdname,
        uname      TYPE syuname, "wbcrossgt-uname,
        udate      TYPE sydatum, "wbcrossgt-udate,
        uzeit      TYPE syuzeit, "wbcrossgt-uzeit,
        found      TYPE xsdboolean,
      END OF ts_usage .
  types:
    tt_usage TYPE STANDARD TABLE OF ts_usage WITH DEFAULT KEY .
  types:
    tt_e071 TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY .

  class-methods IS_DEV_MANDT
    returning
      value(RV_EDITABLE) type ABAP_BOOL .
  class-methods IS_IN_EDITOR
    importing
      value(IV_ANY) type ABAP_BOOL default ABAP_TRUE
      !IV_IS_VIEWER type ABAP_BOOL optional
      !IV_IS_SAPUI5 type ABAP_BOOL optional
      !IV_TCODE type SYTCODE optional
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods DRILL_DOWN
    importing
      !IV_DATATYPE type CSEQUENCE .
  class-methods NAVIGATE_TO
    importing
      !IV_INCLUDE type CSEQUENCE
      !IV_POSITION type I
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods GET_USAGE
    returning
      value(RT_USAGE) type TT_USAGE .
  class-methods GET_LAST_CALL_INFO
    importing
      !IS_LAST_CALL type ABAP_CALLSTACK_LINE
    exporting
      !EV_NAME type CSEQUENCE
      !EV_IS_CLASS type ABAP_BOOL .
  class-methods SH_SORT_ORDER
    importing
      !IV_VALUE type CSEQUENCE optional
    exporting
      !EV_VALUE type CSEQUENCE .
  class-methods MESSAGE_WITH_FIELDS
    importing
      !IT_FIELD type ABAP_ATTRNAME_TAB
      !IV_NUMBER type SYMSGNO
    returning
      value(RV_INFO) type STRING .
protected section.
private section.

  class-data MV_CODE type SYTCODE .

  class-methods GET_POSITION
    importing
      !IV_INCLUDE type CSEQUENCE
      !IV_PACKAGE type ZTAQO_OPTION-PACKAGE_ID
      !IV_OPTION type ZTAQO_OPTION-OPTION_ID
    exporting
      !EV_LINE type I
      !EV_FOUND type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_AQO_HELPER IMPLEMENTATION.


METHOD drill_down.
  DATA lv_tab TYPE dd02v-tabname.
  DATA lv_fld TYPE d021s-fnam.

  zcl_eui_type=>split_type(
   EXPORTING
     iv_datatype = iv_datatype
   IMPORTING
     ev_table    = lv_tab
     ev_field    = lv_fld ).
  CHECK lv_fld IS NOT INITIAL.

  CALL FUNCTION 'RS_DD_STRU_EDIT'
    EXPORTING
      objname   = lv_tab
      fname     = lv_fld
      edit_mode = 'S'
    EXCEPTIONS
      OTHERS    = 5.

  " Show as error
  CHECK sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDMETHOD.


METHOD get_last_call_info.
  DATA:
    lv_len TYPE i.

  ev_name     = is_last_call-mainprogram.
  ev_is_class = abap_false.

  " Is class
  IF is_last_call-blocktype = 'METHOD' AND is_last_call-include CP '*=CM*'.
    ev_is_class = abap_true.

    " Delete 'CP' at the end
    lv_len = strlen( ev_name ) - 2.
    ev_name = ev_name(lv_len).

    " Delete all '='
    REPLACE ALL OCCURRENCES OF `=` IN ev_name WITH ``.
  ENDIF.
ENDMETHOD.


METHOD get_position.
  DATA:
    lt_report TYPE stringtab,
    lt_result TYPE match_result_tab,
    ls_result TYPE REF TO match_result,
    lv_string TYPE string,
    lv_from   TYPE i,
    lv_index  TYPE syindex,
    lv_ok_cnt TYPE i.

  CLEAR:
    ev_line,
    ev_found.

  " Read whole text
  READ REPORT iv_include INTO lt_report.

  " First occurance
  DO 2 TIMES.
    CASE sy-index.
      WHEN 1.
        lv_string = iv_package.
      WHEN 2.
        lv_string = 'IV_PACKAGE_ID'.
      WHEN OTHERS.
    ENDCASE.

    REPLACE ALL OCCURRENCES OF '$' IN lv_string WITH ''.
    CONCATENATE '\b' lv_string '\b' INTO lv_string.
    FIND FIRST OCCURRENCE OF REGEX lv_string IN TABLE lt_report IGNORING CASE RESULTS lt_result.

    " Found or not
    READ TABLE lt_result INDEX 1 REFERENCE INTO ls_result.
    IF sy-subrc = 0.
      ADD 1 TO lv_ok_cnt.
    ENDIF.
  ENDDO.

  " Second one
  CHECK ls_result IS NOT INITIAL AND lv_ok_cnt = 2.
  lv_from = ls_result->line - 4.
  IF lv_from <= 0.
    lv_from = 1.
  ENDIF.

  DO 2 TIMES.
    lv_index = sy-index.
    CASE lv_index.
      WHEN 1.
        lv_string = iv_option.
      WHEN 2.
        lv_string = 'IV_OPTION_ID'.
      WHEN OTHERS.
    ENDCASE.

    CONCATENATE '\b' lv_string '\b' INTO lv_string.
    FIND FIRST OCCURRENCE OF REGEX lv_string IN TABLE lt_report FROM lv_from IGNORING CASE.
    CHECK sy-subrc = 0.

    ev_line = ls_result->line - 1.
    IF lv_index = 1.
      ev_found = abap_true.
    ENDIF.
    EXIT.
  ENDDO.
ENDMETHOD.


METHOD get_usage.
  DATA:
    ls_opt        TYPE ztaqo_option,
    ls_usage      TYPE REF TO ts_usage,
    lv_len        TYPE i,
    lv_class_name TYPE seoclskey,
    lv_rem        TYPE string,
    lo_clif       TYPE REF TO if_oo_clif_incl_naming,
    lo_cldesc     TYPE REF TO if_oo_class_incl_naming,
    lt_meth       TYPE seop_methods_w_include,
    ls_meth       TYPE REF TO seop_method_w_include.

  " Get from memory
  GET PARAMETER ID:
   'ZAQO_PACKAGE_ID' FIELD ls_opt-package_id,
   'ZAQO_OPTION_ID'  FIELD ls_opt-option_id.

  " Index for Global Types - Where-Used List Workbench
  SELECT * INTO CORRESPONDING FIELDS OF TABLE rt_usage
  FROM wbcrossgt
  WHERE otype = 'ME'
    AND name  = 'ZCL_AQO_OPTION\ME:CREATE'.

  LOOP AT rt_usage REFERENCE INTO ls_usage.
    IF ls_opt-package_id IS NOT INITIAL AND ls_opt-option_id IS NOT INITIAL.
      get_position(
       EXPORTING
         iv_include   = ls_usage->include
         iv_package   = ls_opt-package_id
         iv_option    = ls_opt-option_id
       IMPORTING
         ev_line      = ls_usage->line
         ev_found     = ls_usage->found ).
      IF ls_usage->found = abap_true.
        ls_usage->package_id = ls_opt-package_id.
        ls_usage->option_id  = ls_opt-option_id.
      ENDIF.
    ENDIF.

    " Is class
    lv_len = strlen( ls_usage->include ).
    CHECK lv_len = 35.
    lv_class_name = ls_usage->include(30).
    SPLIT lv_class_name AT '=' INTO lv_class_name lv_rem.

    " Try to get methods
    cl_oo_include_naming=>get_instance_by_cifkey(
      EXPORTING
       cifkey = lv_class_name
      RECEIVING
       cifref = lo_clif
      EXCEPTIONS
        OTHERS = 1 ).
    CHECK sy-subrc = 0.
    lo_cldesc ?= lo_clif.

    " Find name
    lt_meth = lo_cldesc->get_all_method_includes( ).
    READ TABLE lt_meth REFERENCE INTO ls_meth
     WITH KEY incname = ls_usage->include.
    CHECK sy-subrc = 0.

    ls_usage->meth = ls_meth->cpdkey-cpdname.
  ENDLOOP.

  SORT rt_usage STABLE BY found DESCENDING.

  " Unique number
  LOOP AT rt_usage REFERENCE INTO ls_usage.
    ls_usage->index = sy-tabix.
  ENDLOOP.
ENDMETHOD.


METHOD is_dev_mandt.
  DATA:
    lv_cccoractiv TYPE t000-cccoractiv.

  " Check client
  SELECT SINGLE cccoractiv INTO lv_cccoractiv
  FROM t000
  WHERE mandt = sy-mandt.
  CHECK lv_cccoractiv = '1'.

  rv_editable = abap_true.
ENDMETHOD.


METHOD is_in_editor.
  DATA:
    lv_tcode TYPE sytcode.

  " for BSP
  IF iv_tcode IS NOT INITIAL.
    mv_code = iv_tcode.
    RETURN.
  ENDIF.

  " Use from memory
  lv_tcode = mv_code.
  IF lv_tcode IS INITIAL.
    lv_tcode = sy-tcode.
  ENDIF.

  " if any is supplied ignore
  IF iv_is_sapui5 = abap_true OR iv_is_viewer = abap_true.
    iv_any = abap_false.
  ENDIF.

  IF ( iv_is_sapui5 = abap_true OR iv_any = abap_true ) AND
     ( lv_tcode = 'ZAQO_VIEWER' OR lv_tcode = 'ZAQO_EDITOR'
      OR sy-xform = 'ZFM_POST_ACTION' ).
    rv_ok = abap_true.
    RETURN.
  ENDIF.

  IF ( iv_is_viewer = abap_true OR iv_any = abap_true ) AND
     ( lv_tcode = 'ZAQO_VIEWER' OR lv_tcode = 'ZAQO_VIEWER_OLD' ).
    rv_ok = abap_true.
    RETURN.
  ENDIF.

  " If also check old UI
  IF iv_any = abap_true AND
    ( lv_tcode = 'ZAQO_VIEWER_OLD' OR lv_tcode = 'ZAQO_EDITOR_OLD' ).
    rv_ok = abap_true.
  ENDIF.
ENDMETHOD.


METHOD message_with_fields.
  DATA:
    BEGIN OF ls_string,
      part1 TYPE symsgv,
      part2 TYPE symsgv,
      part3 TYPE symsgv,
      part4 TYPE symsgv,
    END OF ls_string,

    lv_field TYPE string,
    lv_text  TYPE text255.

  " 1 long string
  LOOP AT it_field INTO lv_field.
    CONCATENATE ls_string ',' lv_field INTO ls_string.
  ENDLOOP.

  " Delete first comma
  IF sy-subrc = 0.
    ls_string = ls_string+1.
  ENDIF.

  MESSAGE ID 'ZAQO_MESSAGE' TYPE 'S' NUMBER iv_number WITH
   ls_string-part1
   ls_string-part2
   ls_string-part3
   ls_string-part4 INTO lv_text.
  rv_info = lv_text.
ENDMETHOD.


METHOD navigate_to.
  " No need
  CHECK iv_include IS NOT INITIAL.

  CALL FUNCTION 'RS_TOOL_ACCESS'
    EXPORTING
      operation   = 'SHOW'
      object_name = iv_include
      object_type = 'REPS'
      position    = iv_position
    EXCEPTIONS
      OTHERS      = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

  " All ok
  rv_ok = abap_true.
ENDMETHOD.


METHOD sh_sort_order.
  CLEAR ev_value.

  IF iv_value IS SUPPLIED.
    SET PARAMETER ID 'ZAQO_SH_SORT_ORDER' FIELD iv_value.
  ELSEIF ev_value IS REQUESTED.
    GET PARAMETER ID 'ZAQO_SH_SORT_ORDER' FIELD ev_value.
  ENDIF.
ENDMETHOD.
ENDCLASS.
