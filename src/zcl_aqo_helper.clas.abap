class ZCL_AQO_HELPER definition
  public
  final
  create private .

public section.
*"* public components of class ZCL_AQO_HELPER
*"* do not include other source files here!!!
  type-pools ABAP .
  class ZCL_EUI_TYPE definition load .

  types:
    BEGIN OF ts_db_key,
        package_id TYPE ztaqo_option-package_id,
        option_id  TYPE ztaqo_option-option_id,
      END OF ts_db_key .
  types:
    tt_db_key  TYPE STANDARD TABLE OF ts_db_key WITH DEFAULT KEY .
  types:
    BEGIN OF ts_command.
        INCLUDE TYPE ts_db_key AS db_key.
      TYPES:
        ucomm TYPE syucomm,
      END OF ts_command .
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
        found      TYPE os_boolean,
      END OF ts_usage .
  types:
    tt_usage TYPE STANDARD TABLE OF ts_usage WITH DEFAULT KEY .
  types:
    tt_e071 TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY .
  types:
    tt_e071k TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY .

  constants:
    BEGIN OF mc_prog,
        editor       TYPE syrepid         VALUE 'ZAQO_EDITOR3',
        editor_tcode TYPE sytcode         VALUE 'ZAQO_EDITOR_OLD',
        viewer_tcode TYPE sytcode         VALUE 'ZAQO_VIEWER_OLD',
        default_id   TYPE zdaqo_option_id VALUE 'DEFAULT',
      END OF mc_prog .
  constants:
    BEGIN OF mc_menu_mode,
        view TYPE zdaqo_menu_mode VALUE 0,
        edit TYPE zdaqo_menu_mode VALUE 1,
        hide TYPE zdaqo_menu_mode VALUE 2,
      END OF mc_menu_mode .

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
    importing
      !IS_DB_KEY type TS_DB_KEY
    returning
      value(RT_USAGE) type TT_USAGE .
  class-methods GET_LAST_CALL_INFO
    importing
      !IS_LAST_CALL type ABAP_CALLSTACK_LINE
    exporting
      !EV_NAME type CSEQUENCE
      !EV_IS_CLASS type ABAP_BOOL .
  class-methods FIND_REQUEST
    importing
      !IV_TABLE_NAME type CSEQUENCE
      !IV_KEY1 type CLIKE
      !IV_KEY2 type CLIKE optional
      !IV_KEY3 type CLIKE optional
    changing
      !CV_TASK type E070-TRKORR
      !CV_OK_MESSAGE type CSEQUENCE .
  class-methods PUT_2_REQUEST
    importing
      !IT_E071 type TT_E071
      !IT_E071K type TT_E071K optional
    exporting
      !EV_REQUEST type E070-TRKORR
    changing
      !CV_TASK type E070-TRKORR
      !CV_OK_MESSAGE type CSEQUENCE .
  class-methods GET_REQUEST_INFO
    importing
      !IV_TABLE_NAME type CSEQUENCE
      !IV_KEY1 type CLIKE optional
      !IV_KEY2 type CLIKE optional
      !IV_KEY3 type CLIKE optional
    exporting
      !ES_E071 type E071
      !ES_E071K type E071K .
  class-methods EXCHANGE_COMMAND
    importing
      !IS_COMMAND type TS_COMMAND optional
    exporting
      !ES_COMMAND type TS_COMMAND .
  class-methods ADD_MENU
    importing
      !IS_DB_OPT type ZTAQO_OPTION .
  class-methods GET_DB_KEYS
    importing
      !IV_WHERE type CSEQUENCE optional
      !IV_PARAM1 type ANY optional
      !IV_COUNT type ANY optional
      !IV_ORDER_BY type CSEQUENCE default 'PACKAGE_ID OPTION_ID'
    returning
      value(RT_DB_KEY) type TT_DB_KEY .
  class-methods GET_BY_KEY
    importing
      !IS_DB_KEY type TS_DB_KEY
    changing
      !CS_DB_ITEM type ANY .
protected section.
private section.

  types:
*"* private components of class ZCL_AQO_HELPER
*"* do not include other source files here!!!
    TT_MENU_SH type standard table of zsaqo_option .

  class-data MV_CODE type SYTCODE .
  class-data MO_MENU type ref to ZCL_EUI_MENU .
  class-data MT_MENU_SH type TT_MENU_SH .

  class-methods GET_CLASS_NAME
    importing
      !IV_INCLUDE type INCLUDE
    returning
      value(RV_CLASS) type SEOCLSNAME .
ENDCLASS.



CLASS ZCL_AQO_HELPER IMPLEMENTATION.


METHOD add_menu.
  CHECK is_db_opt-menu_mode <> mc_menu_mode-hide.

  " 1 time only
  READ TABLE mt_menu_sh TRANSPORTING NO FIELDS
   WITH KEY package_id = is_db_opt-package_id
            option_id  = is_db_opt-option_id.
  CHECK sy-subrc <> 0.

  DATA ls_menu_sh LIKE LINE OF mt_menu_sh.
  MOVE-CORRESPONDING is_db_opt TO ls_menu_sh.
  ls_menu_sh-tabix = lines( mt_menu_sh ) + 1.
  APPEND ls_menu_sh TO mt_menu_sh.

  CHECK mo_menu IS INITIAL.

  DATA lo_handler TYPE REF TO lcl_handler.
  CREATE OBJECT:
   lo_handler,

   mo_menu
    EXPORTING
     io_handler = lo_handler.

  DATA lt_menu TYPE zcl_eui_menu=>tt_menu.
  DATA lr_menu TYPE REF TO zcl_eui_menu=>ts_menu.
  APPEND INITIAL LINE TO lt_menu REFERENCE INTO lr_menu.
  lr_menu->icon = icon_tools.
  CONCATENATE is_db_opt-package_id ` - ` is_db_opt-option_id INTO lr_menu->quickinfo.

  mo_menu->create_toolbar(
   it_menu = lt_menu ).
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


METHOD exchange_command.
  CLEAR es_command.

  IF is_command IS SUPPLIED.
    SET PARAMETER ID: 'ZAQO_PACKAGE_ID' FIELD is_command-package_id,
                      'ZAQO_OPTION_ID'  FIELD is_command-option_id,
                      'ZAQO_COMMAND'    FIELD is_command-ucomm.
    IF is_command-ucomm IS NOT INITIAL.
      " send PAI command
      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode = is_command-ucomm
        EXCEPTIONS
          OTHERS       = 0.
    ENDIF.

    RETURN.
  ENDIF.

  CHECK es_command IS REQUESTED.
  GET PARAMETER ID: 'ZAQO_PACKAGE_ID' FIELD es_command-package_id,
                    'ZAQO_OPTION_ID'  FIELD es_command-option_id,
                    'ZAQO_COMMAND'    FIELD es_command-ucomm.

  " Set empty command
  DATA ls_command LIKE is_command.
  exchange_command( is_command = ls_command ).
ENDMETHOD.


METHOD find_request.
  DATA: ls_e071  TYPE e071, ls_e071k TYPE e071k.
  get_request_info( EXPORTING iv_table_name = iv_table_name
                              iv_key1       = iv_key1
                              iv_key2       = iv_key2
                              iv_key3       = iv_key3
                    IMPORTING es_e071       = ls_e071
                              es_e071k      = ls_e071k ).

  " Find new task or this is OK?
  IF cv_task IS INITIAL.
    " Try to find
    SELECT SINGLE e070~trkorr INTO cv_task " e070~strkorr cv_request
    FROM e070 INNER JOIN e071k ON e071k~trkorr     = e070~trkorr
                              AND e071k~pgmid      = ls_e071k-pgmid
                              AND e071k~object     = ls_e071k-object
                              AND e071k~objname    = ls_e071k-objname
                              AND e071k~mastertype = ls_e071k-mastertype
                              AND e071k~mastername = ls_e071k-mastername
                              AND e071k~tabkey     = ls_e071k-tabkey
    " Current user ( e070~as4user = sy-uname AND ) + Not released (trstatus<>R) + strkorr <> space Task or sub-request has parent item
    WHERE ( e070~trstatus = 'D' OR e070~trstatus = 'L' ) AND e070~strkorr <> space. " ? e070~trfunction = 'S'
  ENDIF.

  DATA:
    lt_e071  TYPE tt_e071,
    lt_e071k TYPE tt_e071k.

  APPEND:
   ls_e071  TO lt_e071,
   ls_e071k TO lt_e071k.

  DATA lv_request TYPE e070-trkorr.
  put_2_request( EXPORTING it_e071       = lt_e071
                           it_e071k      = lt_e071k
                 IMPORTING ev_request    = lv_request
                 CHANGING  cv_task       = cv_task
                           cv_ok_message = cv_ok_message ).
  " Ok
  CHECK lv_request IS NOT INITIAL AND cv_task IS NOT INITIAL.
  MESSAGE s023(zaqo_message) WITH iv_key1 iv_key2 iv_key3 lv_request INTO cv_ok_message.
ENDMETHOD.


METHOD get_by_key.
  CLEAR cs_db_item.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF cs_db_item
  FROM ztaqo_option
  WHERE package_id = is_db_key-package_id
    AND option_id  = is_db_key-option_id.
ENDMETHOD.


METHOD get_class_name.
  CHECK iv_include+30(2) = 'CM'.

  " Is method include?
  DATA lv_len TYPE i.
  lv_len = strlen( iv_include ).
  CHECK lv_len = 35.                                     "#EC NUMBER_OK

  DATA lv_class_name TYPE seoclsname.
  lv_class_name = iv_include(30).

  DATA lv_rem TYPE string.                                  "#EC NEEDED
  SPLIT lv_class_name AT '=' INTO rv_class lv_rem.
ENDMETHOD.


METHOD get_db_keys.
  SELECT package_id option_id UP TO iv_count ROWS INTO TABLE rt_db_key
  FROM ztaqo_option
  WHERE (iv_where)
  ORDER BY (iv_order_by).
ENDMETHOD.


METHOD get_last_call_info.
  ev_name     = is_last_call-mainprogram.
  ev_is_class = abap_false.

  " Is class 1
  CHECK   is_last_call-blocktype = 'METHOD'
      AND is_last_call-mainprogram(30) = is_last_call-include(30)
      AND is_last_call-mainprogram+30(2) EQ 'CP'.

  DATA lv_class_name TYPE seoclsname.
  lv_class_name = get_class_name( is_last_call-include ).
  CHECK lv_class_name IS NOT INITIAL.

  ev_is_class = abap_true.
  ev_name     = lv_class_name.
ENDMETHOD.


METHOD get_request_info.
  CLEAR: es_e071,
         es_e071k.

  " Always the same
  es_e071-pgmid = es_e071k-pgmid = 'R3TR'.

  " Create key for table
  DATA: lv_off   TYPE i, lv_index TYPE char1, lv_name TYPE string.
  DO 3 TIMES.
    " Create name
    lv_index = sy-index.
    CONCATENATE 'IV_KEY' lv_index INTO lv_name.

    " Is supplied
    FIELD-SYMBOLS <lv_key> TYPE clike.
    ASSIGN (lv_name) TO <lv_key>.
    CHECK sy-subrc = 0 AND <lv_key> IS NOT INITIAL.

    " Create key
    DATA lv_len TYPE i.
    DESCRIBE FIELD <lv_key> LENGTH lv_len IN CHARACTER MODE.
    es_e071k-tabkey+lv_off(lv_len)  = <lv_key>.

    " Move to next
    ADD lv_len TO lv_off.
  ENDDO.

  " Struc 1
  es_e071-object      = 'TABU'.
  es_e071-obj_name    = iv_table_name.
  es_e071-objfunc     = 'K'.

  " Struc 2
  es_e071k-object     = es_e071k-mastertype = 'TABU'.
  es_e071k-objname    = es_e071k-mastername = iv_table_name.
ENDMETHOD.


METHOD get_usage.
  CHECK is_db_key IS NOT INITIAL.

  " Index for Global Types - Where-Used List Workbench
  SELECT * INTO CORRESPONDING FIELDS OF TABLE rt_usage
  FROM wbcrossgt
  WHERE otype = 'ME'
    AND name  = 'ZCL_AQO_OPTION\ME:CREATE'.

  DATA ls_usage TYPE REF TO ts_usage.
  LOOP AT rt_usage REFERENCE INTO ls_usage.
    DATA lo_scanner TYPE REF TO lcl_scanner.
    CREATE OBJECT lo_scanner
      EXPORTING
        iv_include = ls_usage->include.

    lo_scanner->get_position( EXPORTING iv_text1 = `ZCL_AQO_OPTION=>CREATE`
                              IMPORTING ev_line  = ls_usage->line ).
    CHECK ls_usage->line > 0.

    DATA lv_count TYPE i.
    lo_scanner->get_position( EXPORTING iv_text1 = 'IV_PACKAGE_ID'
                                        iv_text2 = is_db_key-package_id
                                        iv_from  = ls_usage->line
                              IMPORTING ev_count = lv_count ).
    CASE lv_count.
      WHEN 2.
        ls_usage->package_id = is_db_key-package_id.
      WHEN 0.
        ls_usage->package_id = lo_scanner->get_package( ).
      WHEN OTHERS.
        CLEAR ls_usage->package_id.
    ENDCASE.
    CHECK ls_usage->package_id IS NOT INITIAL.

    lo_scanner->get_position( EXPORTING iv_text1 = 'IV_OPTION_ID'
                                        iv_text2 = is_db_key-option_id
                                        iv_from  = ls_usage->line
                              IMPORTING ev_count = lv_count ).
    CASE lv_count.
      WHEN 2.
        ls_usage->option_id = is_db_key-option_id.
      WHEN 0.
        ls_usage->option_id = zcl_aqo_helper=>mc_prog-default_id.
      WHEN OTHERS.
        CLEAR ls_usage->option_id.
    ENDCASE.
    CHECK ls_usage->option_id IS NOT INITIAL.

    CHECK is_db_key-package_id = ls_usage->package_id
      AND is_db_key-option_id  = ls_usage->option_id.
    ls_usage->found = abap_true.

    CHECK lo_scanner->mv_class IS NOT INITIAL.
    ls_usage->meth = lo_scanner->get_method_name( ).
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
  IF iv_tcode IS NOT INITIAL. " IS SUPPLIED ?
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
    ( lv_tcode = 'ZAQO_VIEWER_OLD' OR lv_tcode = 'ZAQO_EDITOR_OLD' OR lv_tcode = 'ZAQO' ).
    rv_ok = abap_true.
  ENDIF.
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


METHOD put_2_request.
  CLEAR ev_request.

  DATA lt_e071  LIKE it_e071.
  DATA lt_e071k LIKE it_e071k.
  lt_e071  = it_e071[].
  lt_e071k = it_e071k[].

  DO 1 TIMES.
    " Show dialog if task is empty and in old editor
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
  ENDDO.

  " include data to request
  CHECK cv_task IS NOT INITIAL.
  CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
    EXPORTING
      wi_trkorr = cv_task
    TABLES
      wt_e071   = lt_e071
      wt_e071k  = lt_e071k
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc <> 0.
    " zcx_aqo_exception=>raise_sys_error( ).
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO cv_ok_message.
    RETURN.
  ENDIF.

  " Find request
  SELECT SINGLE strkorr INTO ev_request
  FROM e070
  WHERE trkorr = cv_task.
  IF ev_request IS INITIAL.
    ev_request = cv_task.
  ENDIF.
ENDMETHOD.
ENDCLASS.
