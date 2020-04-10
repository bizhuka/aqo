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
  types TS_OAOR_FILE type ZSAQO_OAOR_FILE .
  types:
    tt_oaor_file TYPE STANDARD TABLE OF ts_oaor_file WITH DEFAULT KEY .

  constants MC_OAOR_OTHER type BAPIBDS01-CLASSTYPE value 'OT' ##NO_TEXT.

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
  class-methods CHECK_IN_REQUEST
    importing
      !IV_TABLE_NAME type CSEQUENCE optional
      !IV_KEY1 type CLIKE optional
      !IV_KEY2 type CLIKE optional
      !IV_KEY3 type CLIKE optional
      !IS_OAOR_FILE type TS_OAOR_FILE optional
    changing
      !CV_TASK type E070-TRKORR optional
      !CV_OK_MESSAGE type CSEQUENCE optional
    raising
      ZCX_AQO_EXCEPTION .
  class-methods OAOR_CHECK_EXISTS
    importing
      !IV_PACK_ID type ZTAQO_OPTION-PACKAGE_ID
      !IV_OPTION_ID type ZTAQO_OPTION-OPTION_ID
    exporting
      !EV_TASK type E070-TRKORR
      !EV_OK_MESSAGE type CSEQUENCE .
  class-methods OAOR_GET_FILES
    importing
      !IV_PACK_ID type ZTAQO_OPTION-PACKAGE_ID
      !IV_OPTION_ID type ZTAQO_OPTION-OPTION_ID
      !IV_FILENAME type CSEQUENCE optional
    exporting
      !ES_OAOR_LAST type TS_OAOR_FILE
      !ET_OAOR_FILE type TT_OAOR_FILE .
  class-methods OAOR_DELETE_FILE
    importing
      !IV_PACK_ID type ZTAQO_OPTION-PACKAGE_ID
      !IV_OPTION_ID type ZTAQO_OPTION-OPTION_ID
      !IS_OAOR_FILE type TS_OAOR_FILE
    changing
      !CV_TASK type E070-TRKORR .
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


METHOD check_in_request.
  DATA:
    lt_e071  TYPE STANDARD TABLE OF e071  WITH DEFAULT KEY,
    lt_e071k TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY,
    ls_e071  TYPE e071,
    lr_e071  TYPE REF TO e071,
    ls_e071k TYPE e071k,
    lv_index TYPE char1,
    lv_name  TYPE string,
    lv_len   TYPE i,
    lv_off   TYPE i,
    lv_where TYPE string.
  FIELD-SYMBOLS:
    <lv_key> TYPE clike.

  " Always the same
  ls_e071-pgmid = ls_e071k-pgmid = 'R3TR'.

  " Current user ( e070~as4user = sy-uname AND ) + Not released (trstatus<>R) + strkorr <> space Task or sub-request has parent item
  " ? e070~trfunction = 'S'
  lv_where = `( e070~trstatus = 'D' OR e070~trstatus = 'L' ) AND e070~strkorr <> space`.

**********************************************************************
  " Table row
**********************************************************************
  DO 1 TIMES.
    CHECK iv_table_name IS SUPPLIED.

    " Create key for table
    CLEAR lv_off.
    DO 3 TIMES.
      " Create name
      lv_index = sy-index.
      CONCATENATE 'IV_KEY' lv_index INTO lv_name.

      " Is supplied
      ASSIGN (lv_name) TO <lv_key>.
      CHECK sy-subrc = 0 AND <lv_key> IS NOT INITIAL.

      " Create key
      DESCRIBE FIELD <lv_key> LENGTH lv_len IN CHARACTER MODE.
      ls_e071k-tabkey+lv_off(lv_len)  = <lv_key>.

      " Move to next
      ADD lv_len TO lv_off.
    ENDDO.

    " Struc 1
    ls_e071-object      = 'TABU'.
    ls_e071-obj_name    = iv_table_name.
    ls_e071-objfunc     = 'K'.

    " Struc 2
    ls_e071k-object     = ls_e071k-mastertype = 'TABU'.
    ls_e071k-objname    = ls_e071k-mastername = iv_table_name.

    APPEND:
     ls_e071  TO lt_e071,
     ls_e071k TO lt_e071k.

    " Check in DB
    CHECK cv_task IS INITIAL.

    " Try to find
    SELECT SINGLE e070~trkorr INTO cv_task " e070~strkorr cv_request
    FROM e070 INNER JOIN e071k ON e071k~trkorr     = e070~trkorr
                              AND e071k~pgmid      = ls_e071k-pgmid
                              AND e071k~object     = ls_e071k-object
                              AND e071k~objname    = ls_e071k-objname
                              AND e071k~mastertype = ls_e071k-mastertype
                              AND e071k~mastername = ls_e071k-mastername
                              AND e071k~tabkey     = ls_e071k-tabkey
    WHERE (lv_where).
  ENDDO.

**********************************************************************
  " OAOR file
**********************************************************************
  DO 1 TIMES.
    CHECK is_oaor_file IS SUPPLIED.

    " Logical information object for BDS
    ls_e071-object      = 'SBDL'.
    ls_e071-obj_name    = is_oaor_file-doc_id+10(32).
    APPEND ls_e071 TO lt_e071.

    " Physical information object
    ls_e071-object      = 'SBDP'.
    ls_e071-obj_name    = is_oaor_file-objid.
    APPEND ls_e071 TO lt_e071.

    " Check in DB
    CHECK cv_task IS INITIAL.

    " Try to find
    LOOP AT lt_e071 REFERENCE INTO lr_e071.
      SELECT SINGLE e070~trkorr INTO cv_task
      FROM e070 INNER JOIN e071 ON e071~trkorr = e070~trkorr
      WHERE e071~pgmid    = lr_e071->pgmid
        AND e071~object   = lr_e071->object
        AND e071~obj_name = lr_e071->obj_name
        AND (lv_where).

      CHECK cv_task IS NOT INITIAL.
      EXIT.
    ENDLOOP.
  ENDDO.

  " Show dialog if task is empty and in old editor
  DO 1 TIMES.
    CHECK cv_task IS INITIAL
      AND zcl_aqo_helper=>is_in_editor(  ) = abap_true
      AND zcl_aqo_helper=>is_in_editor( iv_is_sapui5 = abap_true ) <> abap_true
      AND zcl_aqo_helper=>is_dev_mandt( )  = abap_true.

    " select request/task
    CALL FUNCTION 'TR_ORDER_CHOICE_CORRECTION'
      EXPORTING
        iv_category = 'SYST'                                "#EC NOTEXT
      IMPORTING
        " ev_order    = cv_request
        ev_task     = cv_task " Can be empty
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc <> 0.
      zcx_aqo_exception=>raise_sys_error( ).
    ENDIF.

    " include data to request
    CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
      EXPORTING
        wi_trkorr = cv_task
      TABLES
        wt_e071   = lt_e071
        wt_e071k  = lt_e071k
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      zcx_aqo_exception=>raise_sys_error( ).
    ENDIF.

    " TODO test
    CHECK is_oaor_file IS SUPPLIED.

*    " Show dialog or not
*    IF cv_request IS NOT INITIAL.
*      lv_suppress_dialog = abap_true.  " do not show dialog
*    ENDIF.

    CALL FUNCTION 'TR_REQUEST_CHOICE'
      EXPORTING
        iv_suppress_dialog = abap_false " lv_suppress_dialog
        " iv_request         = cv_request
        iv_request_types   = 'K'
        it_e071            = lt_e071[]
        iv_start_column    = 3
        iv_start_row       = 7
        iv_with_error_log  = 'X'
      EXCEPTIONS
        OTHERS             = 8.
    IF sy-subrc <> 0.
      zcx_aqo_exception=>raise_sys_error( ).
    ENDIF.
  ENDDO.

  " Ok 1
  IF iv_table_name IS SUPPLIED AND cv_task IS NOT INITIAL.
    MESSAGE s023(zaqo_message) WITH iv_key1 iv_key2 iv_key3 cv_task INTO cv_ok_message.
    RETURN.
  ENDIF.

  " Ok 2
  IF is_oaor_file IS SUPPLIED AND cv_task IS NOT INITIAL.
    MESSAGE s039(zaqo_message) WITH is_oaor_file-file_name is_oaor_file-doc_ver_no cv_task INTO cv_ok_message.
    RETURN.
  ENDIF.
ENDMETHOD.


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


METHOD oaor_check_exists.
  DATA:
    ls_bds_locl TYPE bds_locl,
    lo_error    TYPE REF TO zcx_aqo_exception.

  " select request/task
  CLEAR ev_task.
  TRY.
      zcl_aqo_helper=>check_in_request(
       EXPORTING
         iv_table_name = 'BDS_LOCL'
         iv_key1       = iv_pack_id
         iv_key2       = mc_oaor_other
      CHANGING
         cv_task       = ev_task
         " cv_request    = rv_request
         cv_ok_message = ev_ok_message
         ).
    CATCH zcx_aqo_exception INTO lo_error.
      CLEAR: "rv_request,
       ev_task.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

  " If ok
  CHECK ev_task IS NOT INITIAL.

  " exist ?
  SELECT SINGLE * INTO ls_bds_locl
  FROM bds_locl
  WHERE classname = iv_pack_id
    AND classtype = mc_oaor_other.
  CHECK sy-subrc <> 0.

  " Create new
  ls_bds_locl-classname = iv_pack_id.
  ls_bds_locl-classtype = mc_oaor_other.
  ls_bds_locl-lo_class  = 'BDS_LOC2'.
  ls_bds_locl-ph_class  = 'BDS_POC2'.
  ls_bds_locl-re_class  = 'BDS_REC2'.
  ls_bds_locl-tabname   = 'BDS_CONN05'.
  ls_bds_locl-log_level = 0. " Or 2 ?
  ls_bds_locl-crea_user = sy-uname.
  CONCATENATE sy-datum sy-uzeit INTO ls_bds_locl-crea_time.

  " Update DB
  INSERT bds_locl FROM ls_bds_locl.
ENDMETHOD.


METHOD oaor_delete_file.
  DATA:
    lv_key           TYPE sbdst_object_key,
    lt_bds_signature TYPE sbdst_signature,
    ls_bds_signature TYPE bapisignat,
    lo_error         TYPE REF TO zcx_aqo_exception.

  " Subfolder in OAOR (and classname = package_id)
  lv_key = iv_option_id.

  " Prepare signature
  MOVE-CORRESPONDING is_oaor_file TO ls_bds_signature.
  APPEND ls_bds_signature TO lt_bds_signature.

  cl_bds_document_set=>delete(
    EXPORTING
      classname      = iv_pack_id
      classtype      = mc_oaor_other
      object_key     = lv_key
      x_force_delete = abap_true
    CHANGING
      signature      = lt_bds_signature
    EXCEPTIONS
      OTHERS         = 7 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  " Delete old file
  TRY.
      check_in_request(
         EXPORTING
          is_oaor_file = is_oaor_file
         CHANGING
          cv_task      = cv_task ).
    CATCH zcx_aqo_exception INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.
ENDMETHOD.


METHOD oaor_get_files.
  DATA:
    lv_key               TYPE sbdst_object_key,
    lt_sbdst_signature   TYPE sbdst_signature,
    ls_sbdst_signature   TYPE REF TO bapisignat,
    lt_sbdst_components2 TYPE sbdst_components2,
    ls_sbdst_components2 TYPE REF TO bapicompo2,
*    lt_connect           TYPE srm_bdsconn_t,
*    ls_connect           TYPE REF TO bapiconnec,
    ls_oaor_file         TYPE REF TO ts_oaor_file,
    lt_doc_id            TYPE SORTED TABLE OF ts_oaor_file-doc_id WITH UNIQUE KEY table_line,
    lv_field             TYPE string.
  FIELD-SYMBOLS:
    <l_val>              TYPE any.

  CLEAR:
    es_oaor_last,
    et_oaor_file.

  " Subfolder in OAOR (and classname = package_id)
  lv_key = iv_option_id.

  " Finding the right documents
  cl_bds_document_set=>get_info(
   EXPORTING
    classname           = iv_pack_id
    classtype           = mc_oaor_other
    object_key          = lv_key
   IMPORTING
    extended_components = lt_sbdst_components2
    " connections         = lt_connect
   CHANGING
    signature           = lt_sbdst_signature
   EXCEPTIONS
    OTHERS              = 7 ).
  CHECK sy-subrc = 0.

  " lt_sbdst_signature structure is complex
  LOOP AT lt_sbdst_signature REFERENCE INTO ls_sbdst_signature.

    AT NEW doc_count.
      " Create new item
      APPEND INITIAL LINE TO et_oaor_file REFERENCE INTO ls_oaor_file.
      ls_oaor_file->doc_id     = ls_sbdst_signature->doc_id.
      ls_oaor_file->doc_ver_no = ls_sbdst_signature->doc_ver_no.
      ls_oaor_file->doc_var_id = ls_sbdst_signature->doc_var_id.
      ls_oaor_file->comp_count = ls_sbdst_signature->comp_count.

      " Concatenate 2 tables
      READ TABLE lt_sbdst_components2 REFERENCE INTO ls_sbdst_components2
       INDEX ls_sbdst_signature->doc_count.
      ls_oaor_file->objid     = ls_sbdst_components2->objid.
      ls_oaor_file->file_name = ls_sbdst_components2->file_name.
      ls_oaor_file->class     = ls_sbdst_components2->class.
    ENDAT.

    " Find field by name
    lv_field = ls_sbdst_signature->prop_name.
    IF lv_field = 'BDS_KEYWORD'.
      lv_field = ls_sbdst_signature->prop_value.
    ENDIF.

    ASSIGN COMPONENT lv_field OF STRUCTURE ls_oaor_file->* TO <l_val>.
    CHECK sy-subrc = 0.

    " Set flag
    IF ls_sbdst_signature->prop_name = 'BDS_KEYWORD'.
      <l_val> = abap_true.
    ELSE.
      <l_val> = ls_sbdst_signature->prop_value.
    ENDIF.
  ENDLOOP.

  " Apply filters
  IF iv_filename IS NOT INITIAL.
    DELETE et_oaor_file WHERE file_name <> iv_filename.
  ENDIF.

  " Max versions first
  SORT et_oaor_file BY doc_id doc_ver_no DESCENDING.

  " Fill other fields
  LOOP AT et_oaor_file REFERENCE INTO ls_oaor_file.
    " Mark current version
    READ TABLE lt_doc_id TRANSPORTING NO FIELDS
     WITH TABLE KEY table_line = ls_oaor_file->doc_id.
    IF sy-subrc <> 0.
      INSERT ls_oaor_file->doc_id INTO TABLE lt_doc_id.
      ls_oaor_file->last_version = abap_true.
    ENDIF.
  ENDLOOP.

  " Max versions last
  SORT et_oaor_file BY doc_id doc_ver_no ASCENDING.

  LOOP AT et_oaor_file REFERENCE INTO ls_oaor_file.
    ls_oaor_file->tabix = sy-tabix.

    " Create at
    ls_oaor_file->created_at_date      = ls_oaor_file->created_at(8).
    ls_oaor_file->created_at_time      = ls_oaor_file->created_at+8(6).

    " Changed at
    ls_oaor_file->last_changed_at_date = ls_oaor_file->last_changed_at(8).
    ls_oaor_file->last_changed_at_time = ls_oaor_file->last_changed_at+8(6).
  ENDLOOP.

  " Get last version for 1 file
  CHECK iv_filename IS NOT INITIAL
    AND es_oaor_last IS REQUESTED.

  READ TABLE et_oaor_file INTO es_oaor_last WITH KEY last_version = abap_true.
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
