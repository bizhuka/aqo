class ZCL_AQO_MENU_HANDLER definition
  public
  final
  create private

  global friends ZCL_EUI_EVENT_CALLER .

public section.

  types TS_OAOR_FILE type ZSAQO_OAOR_FILE .
  types:
    tt_oaor_file TYPE STANDARD TABLE OF ts_oaor_file WITH DEFAULT KEY .

  class-methods GET_EUI_MENU
    importing
      !IV_PACKAGE_ID type CSEQUENCE
      !IV_OPTION_ID type CSEQUENCE
    returning
      value(RO_EUI_MENU) type ref to ZCL_EUI_MENU .
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
protected section.
private section.

  types:
    BEGIN OF ts_unq_menu,
        unq_id   TYPE string,
        unq_menu TYPE REF TO ZCL_AQO_MENU_HANDLER,
      END OF ts_unq_menu .
  types:
    tt_unq_menu TYPE SORTED TABLE OF ts_unq_menu WITH UNIQUE KEY unq_id .
  types:
    BEGIN OF ts_visible,
        pack    TYPE ztaqo_option-package_id,
        opt     TYPE ztaqo_option-option_id,
        visible TYPE abap_bool,
      END OF ts_visible .
  types:
    tt_visible   TYPE SORTED TABLE OF ts_visible WITH UNIQUE KEY pack opt .

  constants:
    BEGIN OF mc,
        oaor_other TYPE bapibds01-classtype VALUE 'OT',
        is_editor  TYPE string VALUE 'IS_EDITOR',
        delimiter  TYPE char1 VALUE '^',
      END OF mc .
  constants:
    BEGIN OF mc_prog,
        editor       TYPE syrepid VALUE 'ZAQO_EDITOR_OLD',
        editor_tcode TYPE sytcode VALUE 'ZAQO_EDITOR_OLD',
        viewer_tcode TYPE sytcode VALUE 'ZAQO_VIEWER_OLD',
      END OF mc_prog .
  constants:
    BEGIN OF mc_menu_mode,
        view TYPE zdaqo_menu_mode VALUE 0,
        edit TYPE zdaqo_menu_mode VALUE 1,
        hide TYPE zdaqo_menu_mode VALUE 2,
      END OF mc_menu_mode .
  constants:
    BEGIN OF mc_action,
        base           TYPE ui_func VALUE '_BASE',

        new            TYPE ui_func VALUE '_NEW',

        change         TYPE ui_func VALUE '_CHANGE',
        view           TYPE ui_func VALUE '_VIEW',

        transport_root TYPE ui_func VALUE '_TR_ROOT',
        export         TYPE ui_func VALUE '_EXPORT',
        import         TYPE ui_func VALUE '_IMPORT',
        delete         TYPE ui_func VALUE '_DELETE',

        last_code      TYPE ui_func VALUE '_LAST_CODE',

        about_sep      TYPE ui_func VALUE '_ABOUT_SEP',
        " -------
        attach_root    TYPE ui_func VALUE '_OAOR_ROOT',
        attach_import  TYPE ui_func VALUE '_OAOR_IMPORT',
        attach_show    TYPE ui_func VALUE '_OAOR_SHOW',
        attach_delete  TYPE ui_func VALUE '_OAOR_DELETE',
        " -------
        about          TYPE ui_func VALUE '_ABOUT',
      END OF mc_action .
  constants:
    BEGIN OF mc_oaor,
        new_file       TYPE string VALUE 'NEW_FILE',
        new_version    TYPE string VALUE 'NEW_VERSION',
        update_version TYPE string VALUE 'UPDATE_VERSION',
      END OF mc_oaor .
  class-data MT_VISIBLE type TT_VISIBLE .
  class-data MT_UNQ_MENU type TT_UNQ_MENU .
  class-data MT_ALL_MENU type ZCL_EUI_MENU=>TT_MENU .
  data MV_IN_EDITOR type ABAP_BOOL .
  data MV_PACKAGE_ID type ZTAQO_OPTION-PACKAGE_ID .
  data MV_OPTION_ID type ZTAQO_OPTION-OPTION_ID .
  data MV_TCODE type SYTCODE .
  data MO_OPTION type ref to ZCL_AQO_OPTION .
  data MO_EUI_MENU type ref to ZCL_EUI_MENU .

  class-methods READ_LOCKS
    importing
      !IV_PACKAGE_ID type CSEQUENCE
      !IV_OPTION_ID type CSEQUENCE
    returning
      value(RV_LOCKED_TEXT) type STB_BUTTON-QUICKINFO .
  class-methods _MENU_ADD_TO_ALL
    importing
      !IT_MENU type ZCL_EUI_MENU=>TT_MENU
    returning
      value(RO_EUI_MENU) type ref to ZCL_EUI_MENU .
  class-methods _MENU_GET_BUTTONS
    importing
      !IV_PACKAGE_ID type CSEQUENCE
      !IV_OPTION_ID type CSEQUENCE
      !IV_IN_EDITOR type ABAP_BOOL
    exporting
      !EV_TCODE type SYTCODE
      !ET_MENU type ZCL_EUI_MENU=>TT_MENU .
  class-methods _OAOR_CHECK_EXISTS
    importing
      !IV_PACK_ID type ZTAQO_OPTION-PACKAGE_ID
      !IV_OPTION_ID type ZTAQO_OPTION-OPTION_ID
    exporting
      !EV_TASK type E070-TRKORR
      !EV_OK_MESSAGE type CSEQUENCE .
  class-methods _OAOR_GET_FILES
    importing
      !IV_PACK_ID type ZTAQO_OPTION-PACKAGE_ID
      !IV_OPTION_ID type ZTAQO_OPTION-OPTION_ID
      !IV_FILENAME type CSEQUENCE optional
      !IV_LAST_ONLY type ABAP_BOOL
    exporting
      !ES_OAOR_LAST type TS_OAOR_FILE
      !ET_OAOR_FILE type TT_OAOR_FILE .
  class-methods _OAOR_DELETE_FILE
    importing
      !IV_PACK_ID type ZTAQO_OPTION-PACKAGE_ID
      !IV_OPTION_ID type ZTAQO_OPTION-OPTION_ID
      !IS_OAOR_FILE type TS_OAOR_FILE .
  class-methods _OAOR_CHANGE_FILE_NAME
    importing
      !IV_FILE_NAME type CSEQUENCE
    changing
      !CS_FILE type BAPIFILES
    raising
      ZCX_EUI_EXCEPTION .
  class-methods _OAOR_HAS_VISIBLE_FILES
    importing
      !IV_PACK type ZTAQO_OPTION-PACKAGE_ID
      !IV_OPT type ZTAQO_OPTION-OPTION_ID
    returning
      value(RV_VISIBLE) type ABAP_BOOL .
  methods _OAOR_DILOAG_SCREEN
    importing
      !IV_DOC_VER_NO type TS_OAOR_FILE-DOC_VER_NO
      !IT_LISTBOX type VRM_VALUES
      !IV_TITLE type CSEQUENCE
    exporting
      !EV_OK type ABAP_BOOL
    changing
      !CV_FILE_NAME type TS_OAOR_FILE-FILE_NAME
      !CV_DESCRIPTION type TS_OAOR_FILE-DESCRIPTION
      !CV_VISIBLE type TS_OAOR_FILE-VISIBLE .
  methods _MENU_FUNCTION_SELECTED
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  methods CONSTRUCTOR
    importing
      !IV_IN_EDITOR type ABAP_BOOL
      !IV_PACKAGE_ID type CSEQUENCE
      !IV_OPTION_ID type CSEQUENCE .
  methods _ABOUT_ON_PAI
    for event PAI_EVENT of ZIF_EUI_MANAGER
    importing
      !SENDER
      !IV_COMMAND
      !CV_CLOSE .
  methods _ABOUT_DO_UPDATE
    importing
      !IV_SET type STRING
      !IV_FIELDS type ZTAQO_OPTION-FIELDS optional
      !IV_DESCRIPTION type ZTAQO_OPTION-DESCRIPTION optional
      !IV_PREV_COUNT type ZTAQO_OPTION-PREV_VALUE_CNT optional
      !IV_MENU_MODE type ZTAQO_OPTION-MENU_MODE optional
    returning
      value(RV_OK) type ABAP_BOOL .
  methods _EXPORT
    returning
      value(RV_UPDATE) type ABAP_BOOL .
  methods _IMPORT
    returning
      value(RV_UPDATE) type ABAP_BOOL .
  methods _LAST_CODE
    returning
      value(RV_UPDATE) type ABAP_BOOL .
  methods _VIEW
    returning
      value(RV_UPDATE) type ABAP_BOOL
    raising
      ZCX_AQO_EXCEPTION .
  methods _CHANGE
    importing
      !IV_COMMAND type SYUCOMM optional
    returning
      value(RV_UPDATE) type ABAP_BOOL
    raising
      ZCX_AQO_EXCEPTION .
  methods _DELETE
    returning
      value(RV_UPDATE) type ABAP_BOOL
    raising
      ZCX_AQO_EXCEPTION .
  methods _NEW
    returning
      value(RV_UPDATE) type ABAP_BOOL
    raising
      ZCX_AQO_EXCEPTION .
  methods _ABOUT
    returning
      value(RV_UPDATE) type ABAP_BOOL
    raising
      ZCX_AQO_EXCEPTION .
  methods _OAOR_IMPORT
    returning
      value(RV_UPDATE) type ABAP_BOOL
    raising
      ZCX_AQO_EXCEPTION .
  methods _OAOR_IMPORT_GET_VERSION
    exporting
      !ET_FILE type SBDST_FILES
      !ES_OAOR_FILE type ZCL_AQO_MENU_HANDLER=>TS_OAOR_FILE
      !EV_OAOR_MODE type STRING
      !EV_EXT type STRING .
  methods _OAOR_SHOW
    importing
      !IV_VIS_ONLY type ABAP_BOOL optional
      !IV_DELETE type ABAP_BOOL optional
    returning
      value(RV_UPDATE) type ABAP_BOOL
    raising
      ZCX_AQO_EXCEPTION .
  methods _OAOR_DELETE
    returning
      value(RV_UPDATE) type ABAP_BOOL
    raising
      ZCX_AQO_EXCEPTION .
ENDCLASS.



CLASS ZCL_AQO_MENU_HANDLER IMPLEMENTATION.


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
  lv_where = `( e070~trstatus = 'D' OR e070~trstatus = 'L' ) AND e070~strkorr <> space`. "#EC NOTEXT

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

    " Find new task or this is OK?
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

    DATA ls_bds       TYPE bapibds01.
    DATA lt_entry     TYPE STANDARD TABLE OF bapicomfil WITH DEFAULT KEY.
    DATA lr_entry     TYPE REF TO bapicomfil.
    DATA lt_signature TYPE STANDARD TABLE OF bapisignat WITH DEFAULT KEY.
    DATA lr_signature TYPE REF TO bapisignat.

    ls_bds-classname = is_oaor_file-package_id.
    ls_bds-classtype = mc-oaor_other.
    ls_bds-client    = sy-mandt.
    ls_bds-objkey    = is_oaor_file-option_id.

    APPEND INITIAL LINE TO lt_signature REFERENCE INTO lr_signature.
    lr_signature->doc_count  = is_oaor_file-comp_count. " c_index.
    lr_signature->doc_id     = is_oaor_file-doc_id.
    lr_signature->doc_ver_no = is_oaor_file-doc_ver_no.
    lr_signature->doc_var_id = is_oaor_file-doc_var_id.
    lr_signature->doc_var_tg = 'OR'.

    CALL FUNCTION 'BDS_BUSINESSDOCUMENT_GET_TP'
      EXPORTING
        logical_system   = ls_bds-log_system
        classname        = ls_bds-classname
        classtype        = ls_bds-classtype
        client           = ls_bds-client
        object_key       = ls_bds-objkey
      TABLES
        signature        = lt_signature
        commfile_entries = lt_entry
      EXCEPTIONS
        OTHERS           = 7.
    IF sy-subrc <> 0.
      zcx_aqo_exception=>raise_sys_error( ).
    ENDIF.

    LOOP AT lt_entry REFERENCE INTO lr_entry.
      MOVE-CORRESPONDING lr_entry->* TO ls_e071.
      APPEND ls_e071 TO lt_e071.
    ENDLOOP.

***      " Logical information object for BDS
***      ls_e071-object      = 'SBDL'.
***      ls_e071-obj_name    = is_oaor_file-doc_id+10(32).
***      APPEND ls_e071 TO lt_e071.
***
***      " Physical information object
***      ls_e071-object      = 'SBDP'.
***      ls_e071-obj_name    = is_oaor_file-objid.
***      APPEND ls_e071 TO lt_e071.

    " Find new task or this is OK?
    CHECK cv_task IS INITIAL.

    " Find first
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

***    " TODO test
***    CHECK is_oaor_file IS SUPPLIED.
***
****    " Show dialog or not
****    IF cv_request IS NOT INITIAL.
****      lv_suppress_dialog = abap_true.  " do not show dialog
****    ENDIF.
***
***    CALL FUNCTION 'TR_REQUEST_CHOICE'
***      EXPORTING
***        iv_suppress_dialog = abap_false " lv_suppress_dialog
***        " iv_request         = cv_request
***        iv_request_types   = 'K'
***        it_e071            = lt_e071[]
***        iv_start_column    = 3
***        iv_start_row       = 7
***        iv_with_error_log  = 'X'
***      EXCEPTIONS
***        OTHERS             = 8.
***    IF sy-subrc <> 0.
***      zcx_aqo_exception=>raise_sys_error( ).
***    ENDIF.
  ENDDO.

  IF cv_task IS NOT INITIAL.
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
  ENDIF.

  CHECK cv_task IS NOT INITIAL.

  " Find request
  DATA lv_request TYPE e070-strkorr.
  SELECT SINGLE strkorr INTO lv_request
  FROM e070
  WHERE trkorr = cv_task.
  IF lv_request IS INITIAL.
    lv_request = cv_task.
  ENDIF.

  " Ok 1
  IF iv_table_name IS SUPPLIED AND cv_task IS NOT INITIAL.
    MESSAGE s023(zaqo_message) WITH iv_key1 iv_key2 iv_key3 lv_request INTO cv_ok_message.
    RETURN.
  ENDIF.

  " Ok 2
  IF is_oaor_file IS SUPPLIED AND cv_task IS NOT INITIAL.
    MESSAGE s039(zaqo_message) WITH is_oaor_file-file_name is_oaor_file-doc_ver_no lv_request INTO cv_ok_message.
    RETURN.
  ENDIF.
ENDMETHOD.


METHOD constructor.
  mv_in_editor  = iv_in_editor.
  mv_package_id = iv_package_id.
  mv_option_id  = iv_option_id.
ENDMETHOD.


METHOD get_eui_menu.
  DATA lv_in_editor TYPE abap_bool.
  DATA ls_unq_menu  TYPE ts_unq_menu.
  DATA lr_unq_menu  TYPE REF TO ts_unq_menu.
  DATA lo_unq_menu  TYPE REF TO zcl_aqo_menu_handler.
  DATA lt_menu      TYPE zcl_eui_menu=>tt_menu.
  DATA ls_menu      TYPE REF TO zcl_eui_menu=>ts_menu.
  DATA lo_toolbar   TYPE REF TO cl_gui_toolbar.
  DATA lv_enabled   TYPE abap_bool.

  lv_in_editor = zcl_aqo_helper=>is_in_editor( ).
  IF lv_in_editor = abap_true.
    ls_unq_menu-unq_id = mc-is_editor.
  ELSE.
    CONCATENATE iv_package_id '-' iv_option_id INTO ls_unq_menu-unq_id.
  ENDIF.

  " Check menu existence
  READ TABLE mt_unq_menu REFERENCE INTO lr_unq_menu
   WITH TABLE KEY unq_id = ls_unq_menu-unq_id.

  " Create new GOS menu
  IF sy-subrc <> 0.
    CREATE OBJECT ls_unq_menu-unq_menu
      EXPORTING
        iv_in_editor  = lv_in_editor
        iv_package_id = iv_package_id
        iv_option_id  = iv_option_id.
    INSERT ls_unq_menu INTO TABLE mt_unq_menu REFERENCE INTO lr_unq_menu.
  ENDIF.
  lo_unq_menu = lr_unq_menu->unq_menu.

  " If new option
  IF lv_in_editor = abap_true.
    CLEAR lo_unq_menu->mv_tcode.
  ENDIF.

  IF lo_unq_menu->mv_tcode IS INITIAL.
    " prepare buttons
    _menu_get_buttons(
     EXPORTING
      iv_package_id = iv_package_id
      iv_option_id  = iv_option_id
      iv_in_editor  = lv_in_editor
     IMPORTING
      ev_tcode      = lo_unq_menu->mv_tcode
      et_menu       = lt_menu ).

**********************************************************************
    "№1 - Edit existing in editor
    IF lv_in_editor = abap_true AND lo_unq_menu->mo_eui_menu IS NOT INITIAL.
      lo_unq_menu->mv_package_id = iv_package_id.
      lo_unq_menu->mv_option_id  = iv_option_id.

      lo_toolbar = lo_unq_menu->mo_eui_menu->get_toolbar( ).
      LOOP AT lt_menu REFERENCE INTO ls_menu WHERE function IS NOT INITIAL.
        " Is now enabled ?
        IF ls_menu->disabled = abap_true.
          lv_enabled = abap_false.
        ELSE.
          lv_enabled = abap_true.
        ENDIF.

        lo_toolbar->set_enable(
         EXPORTING
          enable = lv_enabled
         EXCEPTIONS
          OTHERS = 0 ).
      ENDLOOP.
    ENDIF.

**********************************************************************
    "№2 - Edit existing in Z* transaction
    IF lv_in_editor <> abap_true.
      ro_eui_menu = _menu_add_to_all( it_menu = lt_menu ).

      " Add another listner
      IF ro_eui_menu IS NOT INITIAL.
        ro_eui_menu->add_handler( lo_unq_menu ).
        RETURN.
      ENDIF.
    ENDIF.

    " User interface class
    IF lo_unq_menu->mo_eui_menu IS INITIAL.
      CREATE OBJECT lo_unq_menu->mo_eui_menu.
      lo_unq_menu->mo_eui_menu->create_toolbar(
       it_menu = lt_menu ).
    ENDIF.
  ENDIF.

  " And return
  ro_eui_menu = lo_unq_menu->mo_eui_menu.
  ro_eui_menu->add_handler( lo_unq_menu ).
ENDMETHOD.


METHOD read_locks.
  DATA:
    lv_garg TYPE seqg3-garg,
    lt_lock TYPE STANDARD TABLE OF seqg3 WITH DEFAULT KEY.
  FIELD-SYMBOLS:
   <ls_lock> LIKE LINE OF lt_lock.

  lv_garg+0(30)  = iv_package_id.
  lv_garg+30(30) = iv_option_id.
  CALL FUNCTION 'ENQUEUE_READ'
    EXPORTING
      gname                 = 'ZTAQO_OPTION'
      garg                  = lv_garg
      guname                = space " by all users
    TABLES
      enq                   = lt_lock
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      OTHERS                = 3.
  CHECK sy-subrc = 0.

  READ TABLE lt_lock ASSIGNING <ls_lock> INDEX 1.
  CHECK sy-subrc = 0.

  MESSAGE s034(zaqo_message) WITH <ls_lock>-guname INTO rv_locked_text.
ENDMETHOD.


METHOD _about.
  " Show in screen PARAMETERS:
  DATA ls_dyn_scr  TYPE REF TO zsaqo_about_dialog.
  DATA lo_screen   TYPE REF TO zcl_eui_screen.
  DATA lo_err      TYPE REF TO zcx_eui_exception.
  DATA lv_input    TYPE screen-input.
  DATA lv_cmd      TYPE syucomm.

  " Where to store data
  CREATE DATA ls_dyn_scr.

  " Previous values
  ls_dyn_scr->p_2_pack = mo_option->ms_db_item-package_id.
  ls_dyn_scr->p_2_opt  = mo_option->ms_db_item-option_id.
  ls_dyn_scr->p_2_date = mo_option->ms_db_item-created_date.
  ls_dyn_scr->p_2_ntxt = mo_option->ms_db_item-created_name_txt.
  " Editable
  ls_dyn_scr->p_2_desc = mo_option->ms_db_item-description.
  ls_dyn_scr->p_2_prev = mo_option->ms_db_item-prev_value_cnt.
  ls_dyn_scr->p_2_menu = mo_option->ms_db_item-menu_mode.

  " Create screen manager
  TRY.
      CREATE OBJECT lo_screen
        EXPORTING
          iv_dynnr        = '1020'
          iv_cprog        = mc_prog-editor
          ir_context      = ls_dyn_scr
          " Set pf-status & text
          iv_status_name  = 'ABOUT_STATUS'
          iv_status_prog  = mc_prog-editor
          iv_status_title = 'Enter option description'(eod).
    CATCH zcx_eui_exception INTO lo_err.
      MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  " Exclude buttons
  IF me->mv_tcode = mc_prog-editor_tcode AND zcl_aqo_helper=>is_dev_mandt( ) = abap_true.
    lv_input = '1'.
    APPEND 'USER_INFO'   TO lo_screen->ms_status-exclude.
  ELSE.
    lv_input = '0'.
    APPEND 'MODIFY'      TO lo_screen->ms_status-exclude.
    APPEND 'DEV_INFO'    TO lo_screen->ms_status-exclude.

    " Also hide
    IF _oaor_has_visible_files(
        iv_pack = ls_dyn_scr->p_2_pack
        iv_opt  = ls_dyn_scr->p_2_opt ) <> abap_true.
      APPEND 'USER_INFO' TO lo_screen->ms_status-exclude.
    ENDIF.
  ENDIF.

  " Static PF status no need on_pbo_event.
  lo_screen->customize( name = 'P_2_PACK'   input = '0' ).
  lo_screen->customize( name = 'P_2_OPT'    input = '0' ).
  lo_screen->customize( name = 'P_2_DATE'   input = '0' ).
  lo_screen->customize( name = 'P_2_NTXT'   input = '0' ).

  lo_screen->customize( name = 'P_2_DESC'   input = lv_input required = '1' ).
  lo_screen->customize( name = 'P_2_PREV'   input = lv_input required = '1' ).
  lo_screen->customize( name = 'P_2_MENU'   input = lv_input ).

  " As popup
  lo_screen->popup( ).

  " Process action
  lv_cmd = lo_screen->show(
   io_handler      = me
   iv_handlers_map = '_ABOUT_ON_PAI' ).
  CHECK lv_cmd = zif_eui_manager=>mc_cmd-ok.

  rv_update = abap_true.
ENDMETHOD.


METHOD _about_do_update.
  UPDATE ztaqo_option
   SET (iv_set)
  WHERE package_id = mo_option->ms_db_item-package_id
    AND option_id  = mo_option->ms_db_item-option_id.

  IF sy-subrc = 0.
    MESSAGE 'Data updated'(upd) TYPE 'S'.
    rv_ok = abap_true.
  ELSE.
    MESSAGE 'Error during updating!'(edu) TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDMETHOD.


METHOD _about_on_pai.
  DATA lo_screen   TYPE REF TO zcl_eui_screen.
  DATA ls_dyn_scr  TYPE REF TO zsaqo_about_dialog.
  DATA lo_err      TYPE REF TO zcx_aqo_exception.

  " Get data
  lo_screen ?= sender.
  ls_dyn_scr ?= lo_screen->get_context( ).

  CASE iv_command.
    WHEN 'MODIFY'.
      IF ls_dyn_scr->p_2_prev > 7 OR ls_dyn_scr->p_2_prev < 1.
        MESSAGE 'Previous values count have to be from 1 to 7'(nir) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      _about_do_update(
       iv_set         = `DESCRIPTION = IV_DESCRIPTION PREV_VALUE_CNT = IV_PREV_COUNT MENU_MODE = IV_MENU_MODE`
       iv_description = ls_dyn_scr->p_2_desc
       iv_prev_count  = ls_dyn_scr->p_2_prev
       iv_menu_mode   = ls_dyn_scr->p_2_menu ).

      cv_close->* = abap_true.

    WHEN 'DEV_INFO'.
      cv_close->* = abap_true.

      " Show online documentation in browser
      CALL FUNCTION 'CALL_BROWSER'
        EXPORTING
          url    = 'https://github.com/bizhuka/aqo/wiki' "#EC NOTEXT
        EXCEPTIONS
          OTHERS = 6.
      CHECK sy-subrc = 0.

    WHEN 'USER_INFO'.
      cv_close->* = abap_true.

      " Check in attachments
      TRY.
          _oaor_show( iv_vis_only = abap_true ).
        CATCH zcx_aqo_exception INTO lo_err.
          MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

      " Ok or cancel ?
    WHEN OTHERS.
      RETURN.
  ENDCASE.
ENDMETHOD.


METHOD _change.
  SET PARAMETER ID:
    'ZAQO_PACKAGE_ID' FIELD mo_option->ms_db_item-package_id,
    'ZAQO_OPTION_ID'  FIELD mo_option->ms_db_item-option_id,
    'ZAQO_COMMAND'    FIELD iv_command.

  CALL TRANSACTION me->mv_tcode " WITH AUTHORITY-CHECK "#EC CI_CALLTA
    AND SKIP FIRST SCREEN.
ENDMETHOD.


METHOD _delete.
  " $ & transport
  DATA lv_message TYPE string.
  lv_message = mo_option->delete( ).
  IF lv_message IS NOT INITIAL.
    MESSAGE lv_message TYPE 'S'.
  ENDIF.

  rv_update = abap_true.
ENDMETHOD.


METHOD _export.
  DATA lv_file_name  TYPE string.
  DATA lo_error      TYPE REF TO zcx_eui_exception.

  CONCATENATE mo_option->ms_db_item-package_id `-` mo_option->ms_db_item-option_id
              `-` sy-mandt `-` sy-datum `-` sy-uzeit `.aqob` "#EC NOTEXT
   INTO lv_file_name.

  " Save to file
  DATA lo_file TYPE REF TO zcl_eui_file.
  CREATE OBJECT lo_file
    EXPORTING
      iv_xstring = mo_option->ms_db_item-fields.
  TRY.
      lo_file->download(
       iv_full_path    = lv_file_name
       iv_save_dialog  = abap_true
       iv_window_title = 'Save option values'(sov) ).
    CATCH zcx_eui_exception INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDMETHOD.


METHOD _import.
  DATA lo_file  TYPE REF TO zcl_eui_file.
  DATA lo_error TYPE REF TO zcx_eui_exception.

  CREATE OBJECT lo_file.
  TRY.
      lo_file->import_from_file(
         iv_default_extension = 'aqob' ). "#EC NOTEXT
    CATCH zcx_eui_exception INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  rv_update = _about_do_update(
   iv_set    = `FIELDS = IV_FIELDS`
   iv_fields = lo_file->mv_xstring ).
ENDMETHOD.


METHOD _last_code.
  DATA:
    lv_ok TYPE abap_bool.
  IF mo_option->ms_db_item-mainprogram IS INITIAL.
    MESSAGE 'No previous call was found'(ncl) TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  " Try to launch by save first save of option
  lv_ok = zcl_aqo_helper=>navigate_to(
     iv_include  = mo_option->ms_db_item-include
     iv_position = mo_option->ms_db_item-line ).

  " if not OK
  CHECK lv_ok <> abap_true
    AND mo_option->ms_db_item-package_id IS NOT INITIAL
    AND mo_option->ms_db_item-option_id  IS NOT INITIAL.

  " Pass params
  SET PARAMETER ID:
    'ZAQO_PACKAGE_ID' FIELD mo_option->ms_db_item-package_id,
    'ZAQO_OPTION_ID'  FIELD mo_option->ms_db_item-option_id.

  " Second attempt by code scan
  PERFORM call_by_name IN PROGRAM zaqo_editor_old
    USING 'CODE_SCAN_F4'.
ENDMETHOD.


METHOD _menu_add_to_all.
  DATA          lv_cnt       TYPE i.
  DATA          lr_unq_menu  TYPE REF TO ts_unq_menu.
  FIELD-SYMBOLS <ls_menu>    LIKE LINE OF mt_all_menu.

  IF mt_all_menu IS INITIAL.
    APPEND INITIAL LINE TO mt_all_menu ASSIGNING <ls_menu>.
    <ls_menu>-icon         = icon_activity. " icon_system_mark. "icon_tools.
    <ls_menu>-butn_type    = cntb_btype_menu.
    <ls_menu>-function     = mc_action-base.
  ENDIF.

  " Position of newly inserted items
  lv_cnt = lines( mt_all_menu ).
  ADD 1 TO lv_cnt.

  " Add all
  APPEND LINES OF it_menu TO mt_all_menu.

  " Change first item
  READ TABLE mt_all_menu ASSIGNING <ls_menu> INDEX lv_cnt.
  CHECK sy-subrc = 0.
  <ls_menu>-par_function = mc_action-base.
  <ls_menu>-text         = <ls_menu>-quickinfo.

  " Second time or more
  CHECK lv_cnt > 2.

  " Just return updated
  LOOP AT mt_unq_menu REFERENCE INTO lr_unq_menu.
    " 1-st not empty
    CHECK lr_unq_menu->unq_menu->mo_eui_menu IS NOT INITIAL.

    " Change existing toolbar
    ro_eui_menu = lr_unq_menu->unq_menu->mo_eui_menu->create_toolbar(
     it_menu = mt_all_menu ).
    RETURN.
  ENDLOOP.
ENDMETHOD.


METHOD _menu_function_selected.
  DATA lv_index      TYPE syindex.
  DATA lv_create     TYPE abap_bool.
  DATA lo_err        TYPE REF TO zcx_aqo_exception.
  DATA lo_error      TYPE REF TO cx_root.
  DATA lv_update     TYPE abap_bool.
  DATA lv_package_id TYPE ztaqo_option-package_id.
  DATA lv_option_id  TYPE ztaqo_option-option_id.
  DATA lv_method     TYPE string.

  " Get function to launch
  SPLIT fcode AT mc-delimiter INTO lv_package_id lv_option_id lv_method.

  " No need
  IF me->mv_in_editor <> abap_true.
    CHECK mv_package_id = lv_package_id
     AND  mv_option_id  = lv_option_id.
  ENDIF.

  IF mv_in_editor = abap_true AND lv_method IS INITIAL.
    lv_package_id = me->mv_package_id.
    lv_option_id  = me->mv_option_id.
    lv_method     = fcode.
  ENDIF.

  " Set no error mode
  zcl_aqo_helper=>is_in_editor( iv_tcode = mv_tcode ).

  DO 2 TIMES.
    lv_index = sy-index.

    " First time
    IF me->mo_option IS INITIAL.
      lv_create = abap_true.
    ENDIF.

    " Change in EDITOR
    IF me->mv_in_editor = abap_true AND
           me->mo_option IS NOT INITIAL AND
      (    me->mo_option->ms_db_item-package_id <> lv_package_id
        OR me->mo_option->ms_db_item-option_id  <> lv_option_id ).
      lv_create = abap_true.
    ENDIF.

    IF lv_create = abap_true.
      TRY.
          me->mo_option = zcl_aqo_option=>create(
             iv_package_id  = lv_package_id
             iv_option_id   = lv_option_id ).
        CATCH zcx_aqo_exception INTO lo_err.
          MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
      ENDTRY.
    ENDIF.

    " Call method 1 time
    CHECK lv_index = 1.

    TRY.
        CALL METHOD me->(lv_method)
          RECEIVING
            rv_update = lv_update.

        " Option was changed
        IF lv_update = abap_true.
          CLEAR me->mo_option.
          " Second loop for changing option
          CONTINUE.
        ENDIF.

        " All ok
        RETURN.
      CATCH cx_root INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
  ENDDO.
ENDMETHOD.


METHOD _menu_get_buttons.
  " Delete prev menu & create new one
  DATA:
    lv_is_dev        TYPE abap_bool,

    lv_in_viewer     TYPE abap_bool,
    lv_lock_info     TYPE stb_button-quickinfo,
    lv_view_only     TYPE abap_bool,

    lv_option_exist  TYPE abap_bool VALUE abap_false,
    lv_package_exist TYPE abap_bool VALUE abap_false,

    " For separators only
    lv_prev_hide     TYPE abap_bool,

    lv_menu_mode     TYPE ztaqo_option-menu_mode,
    lv_root_func     TYPE ui_func,
    lv_devclass      TYPE tdevc-devclass.
  FIELD-SYMBOLS:
   <ls_menu> LIKE LINE OF et_menu.

  CLEAR:
   ev_tcode,
   et_menu.

  lv_is_dev    = zcl_aqo_helper=>is_dev_mandt( ).
  lv_lock_info = read_locks(
   iv_package_id = iv_package_id
   iv_option_id  = iv_option_id ).

  " Exist ?
  SELECT SINGLE menu_mode INTO lv_menu_mode
  FROM ztaqo_option
  WHERE package_id = iv_package_id
    AND option_id  = iv_option_id.
  IF sy-subrc = 0.
    lv_option_exist = abap_true.
  ELSE.
    SELECT SINGLE devclass INTO lv_devclass
    FROM tdevc
    WHERE devclass = iv_package_id.
    IF sy-subrc = 0.
      lv_package_exist = abap_true.
    ENDIF.
  ENDIF.

  IF iv_in_editor = abap_true.
    lv_in_viewer = zcl_aqo_helper=>is_in_editor( iv_is_viewer = abap_true ).
    ev_tcode     = sy-tcode.
  ELSEIF lv_option_exist = abap_true.

    CASE lv_menu_mode.
      WHEN mc_menu_mode-hide.
        RETURN.

      WHEN mc_menu_mode-view.
        " Change tooltip
        IF lv_lock_info IS INITIAL.
          MESSAGE s034(zaqo_message) WITH 'settings'(stt) INTO lv_lock_info.
        ENDIF.
        ev_tcode = mc_prog-viewer_tcode.

      WHEN mc_menu_mode-edit.
        " Do nothing

      WHEN OTHERS.
        zcx_aqo_exception=>raise_dump( iv_message = 'Unknown MENU_MODE'(umm) ).
    ENDCASE.

    " If not locked by user or settings
    IF lv_lock_info IS INITIAL.
      ev_tcode = mc_prog-editor_tcode.
    ENDIF.
  ENDIF.

**********************************************************************
  " INITIALIZE the menu
**********************************************************************

  IF iv_in_editor <> abap_true. " Is not in editor transaction
    APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.

    " Always has parent ID ?
    CONCATENATE iv_package_id mc-delimiter iv_option_id INTO lv_root_func.

    " Just tooltip
    CONCATENATE iv_package_id ` - ` iv_option_id INTO <ls_menu>-quickinfo.
    <ls_menu>-icon         = icon_tools.
    <ls_menu>-butn_type    = cntb_btype_menu.
    <ls_menu>-text         = <ls_menu>-quickinfo.
    <ls_menu>-function     = lv_root_func.
  ENDIF.

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = 'New'(crt).
  <ls_menu>-icon         = icon_create.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-new.
  <ls_menu>-par_function = lv_root_func.

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = 'View'(viw).
  <ls_menu>-quickinfo    = lv_lock_info.
  <ls_menu>-icon         = icon_display.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-view.
  <ls_menu>-par_function = lv_root_func.

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = <ls_menu>-quickinfo    = 'Edit'(edt).
  <ls_menu>-icon         = icon_change.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-change.
  <ls_menu>-par_function = lv_root_func.

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-butn_type    = cntb_btype_sep.
  <ls_menu>-par_function = lv_root_func.

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = 'Transport'(trn).
  <ls_menu>-quickinfo    = lv_lock_info.
  <ls_menu>-icon         = icon_transport.
  <ls_menu>-butn_type    = cntb_btype_menu.
  <ls_menu>-function     = mc_action-transport_root.
  <ls_menu>-par_function = lv_root_func.

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-butn_type    = cntb_btype_sep.
  <ls_menu>-par_function = mc_action-transport_root.

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = <ls_menu>-quickinfo    = 'Export'(exp).
  <ls_menu>-icon         = icon_export.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-export.
  <ls_menu>-par_function = mc_action-transport_root.

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = 'Import'(imp).
  <ls_menu>-quickinfo    = lv_lock_info.
  <ls_menu>-icon         = icon_import.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-import.
  <ls_menu>-par_function = mc_action-transport_root.

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-butn_type    = cntb_btype_sep.
  <ls_menu>-par_function = mc_action-transport_root.

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = 'Delete'(del).
  <ls_menu>-quickinfo    = lv_lock_info.
  <ls_menu>-icon         = icon_delete.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-delete.
  <ls_menu>-par_function = mc_action-transport_root.

**********************************************************************
  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = <ls_menu>-quickinfo    = 'Where-Used List'(wul).
  <ls_menu>-icon         = icon_reference_list.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-last_code.
  <ls_menu>-par_function = lv_root_func.

**********************************************************************

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-butn_type    = cntb_btype_sep.
  <ls_menu>-par_function = lv_root_func.
  <ls_menu>-function     = mc_action-about_sep. " Always show


  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = <ls_menu>-quickinfo    = 'Attach'(att).
  <ls_menu>-icon         = icon_attachment.
  <ls_menu>-butn_type    = cntb_btype_menu.
  <ls_menu>-function     = mc_action-attach_root.
  <ls_menu>-par_function = lv_root_func.

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = <ls_menu>-quickinfo    = 'Add attachment'(aat).
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-attach_import.
  <ls_menu>-par_function = mc_action-attach_root.

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = <ls_menu>-quickinfo    = 'Show attachments'(sat).
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-attach_show.
  <ls_menu>-par_function = mc_action-attach_root.

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = <ls_menu>-quickinfo    = 'Delete attachment'(dat).
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-attach_delete.
  <ls_menu>-par_function = mc_action-attach_root.

  APPEND INITIAL LINE TO et_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = <ls_menu>-quickinfo    = 'About'(abo).
  <ls_menu>-icon         = icon_tools.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-about.
  <ls_menu>-par_function = lv_root_func.

**********************************************************************
  " Change visibility
  IF lv_in_viewer = abap_true OR lv_lock_info IS NOT INITIAL.
    lv_view_only = abap_true.
  ENDIF.

  " 0 - lv_is_dev, 1 - iv_in_editor, 2 - lv_view_only, 3 - lv_option_exist
  LOOP AT et_menu ASSIGNING <ls_menu>.
    CASE <ls_menu>-function.
*        " Just skip
*      WHEN mc_action-base.

      WHEN mc_action-attach_root OR mc_action-last_code.
        IF lv_option_exist <> abap_true OR lv_is_dev <> abap_true.
          <ls_menu>-hide = abap_true.
        ENDIF.

      WHEN mc_action-transport_root.
        IF lv_option_exist <> abap_true. " Show EXPORT OR lv_is_dev <> abap_true.
          <ls_menu>-hide = abap_true.
        ENDIF.

      WHEN mc_action-new.
        IF   lv_option_exist = abap_true
          OR lv_view_only = abap_true.
          <ls_menu>-hide = abap_true.
        ENDIF.

        IF lv_is_dev <> abap_true.
          <ls_menu>-disabled = abap_true.
          <ls_menu>-quickinfo = 'Only in DEV!'(oid).
        ENDIF.

        " For info
        IF lv_option_exist <> abap_true AND lv_package_exist <> abap_true.
          <ls_menu>-disabled = abap_true.
          <ls_menu>-quickinfo = 'The package do not exist'(tpd).
        ENDIF.

        IF <ls_menu>-quickinfo IS INITIAL.
          <ls_menu>-quickinfo = 'SE38->ZAQO_TESTER is simpler'(se3).
        ENDIF.

      WHEN mc_action-change.
        IF    lv_option_exist <> abap_true
           OR iv_in_editor = abap_true
           OR lv_view_only = abap_true.
          <ls_menu>-hide = abap_true.
        ENDIF.

      WHEN mc_action-view.
        IF    lv_option_exist <> abap_true
           OR iv_in_editor = abap_true
           OR lv_view_only <> abap_true.
          <ls_menu>-hide = abap_true.
        ENDIF.

      WHEN mc_action-import OR mc_action-delete " OR mc_action-transport  OR mc_action-save_in
           OR mc_action-attach_import OR mc_action-attach_delete.
        IF lv_option_exist <> abap_true.
          <ls_menu>-hide = abap_true.
        ENDIF.

        IF lv_is_dev <> abap_true
           OR iv_in_editor <> abap_true
           OR lv_view_only = abap_true.
          <ls_menu>-disabled = abap_true.
          <ls_menu>-quickinfo = 'In DEV editor only!'(ido).
        ENDIF.

      WHEN mc_action-export OR mc_action-attach_show.

      WHEN mc_action-about OR mc_action-about_sep.
        IF lv_option_exist <> abap_true.
          <ls_menu>-hide = abap_true.
        ENDIF.

      WHEN OTHERS.
        IF <ls_menu>-butn_type = cntb_btype_sep.
          <ls_menu>-hide = lv_prev_hide.
        ELSEIF <ls_menu>-function <> lv_root_func.
          zcx_aqo_exception=>raise_dump( iv_message = 'Unknouwn button'(unb) ).
        ENDIF.
    ENDCASE.

    IF <ls_menu>-disabled = abap_true OR <ls_menu>-hide = abap_true.
      lv_prev_hide = abap_true.
    ELSE.
      lv_prev_hide = abap_false.
    ENDIF.

    " Add info about current option
    IF <ls_menu>-function IS NOT INITIAL.
      IF <ls_menu>-function NS lv_root_func.
        CONCATENATE lv_root_func mc-delimiter <ls_menu>-function INTO <ls_menu>-function.
      ENDIF.
    ENDIF.

    " Same for parent function
    IF <ls_menu>-par_function IS NOT INITIAL.
      IF <ls_menu>-par_function NS lv_root_func.
        CONCATENATE lv_root_func mc-delimiter <ls_menu>-par_function INTO <ls_menu>-par_function.
      ENDIF.
    ENDIF.

***      " Has no parent
***      IF iv_in_editor = abap_true AND <ls_menu>-par_function = lv_root_func.
***        CLEAR <ls_menu>-par_function.
***      ENDIF.

    " No need text
    CHECK <ls_menu>-par_function IS INITIAL.
    CLEAR <ls_menu>-text.
  ENDLOOP.
ENDMETHOD.


METHOD _new.
  _change( iv_command  = '_NEW_OPTION' ).
  rv_update = abap_true.
ENDMETHOD.


METHOD _oaor_change_file_name.
  DATA lo_file       TYPE REF TO zcl_eui_file.
  DATA lv_temp_path  TYPE string.
  DATA lv_full_path  TYPE string.
  DATA lv_sep        TYPE char1.
  DATA lv_len        TYPE i.

  " 1-st upload file
  CREATE OBJECT lo_file.
  CONCATENATE cs_file-directory cs_file-filename INTO lv_full_path.
  lo_file->import_from_file( iv_full_path = lv_full_path ).

  " Save to temp dir No need to clean files (cl_gui_frontend_services=>file_delete). SAP gui cleans 'SAP GUI\tmp\' automatically
  cl_gui_frontend_services=>get_temp_directory(
   CHANGING
     temp_dir = lv_temp_path
   EXCEPTIONS
     OTHERS = 1 ).
  IF sy-subrc <> 0.
    zcx_eui_exception=>raise_sys_error( ).
  ENDIF.

  " Add file separator
  cl_gui_frontend_services=>get_file_separator(
   CHANGING
     file_separator = lv_sep ).
  cl_gui_cfw=>flush( ).

  lv_len = strlen( lv_temp_path ) - 1.
  IF lv_temp_path+lv_len(1) <> lv_sep.
    CONCATENATE lv_temp_path lv_sep INTO lv_temp_path.
  ENDIF.

  " Save to sub folter of temp
  lv_full_path = zcl_eui_conv=>guid_create( ).
  CONCATENATE lv_temp_path lv_full_path lv_sep INTO lv_temp_path.

  " 2-nd download with new name
  CONCATENATE lv_temp_path iv_file_name INTO lv_full_path.
  lo_file->download(
   iv_full_path   = lv_full_path
   iv_save_dialog = abap_false ).

  " Change if all ok
  cs_file-directory = lv_temp_path.
  cs_file-filename  = iv_file_name.
ENDMETHOD.


METHOD _oaor_check_exists.
  DATA:
    ls_bds_locl TYPE bds_locl,
    lo_error    TYPE REF TO zcx_aqo_exception.

  " select request/task
  CLEAR ev_task.
  TRY.
      zcl_aqo_menu_handler=>check_in_request(
       EXPORTING
         iv_table_name = 'BDS_LOCL'
         iv_key1       = iv_pack_id
         iv_key2       = mc-oaor_other
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
    AND classtype = mc-oaor_other.
  CHECK sy-subrc <> 0.

  " Create new
  ls_bds_locl-classname = iv_pack_id.
  ls_bds_locl-classtype = mc-oaor_other.
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


METHOD _oaor_delete.
  CHECK zcl_eui_screen=>confirm(
      iv_title    = 'Confirmation'(cnf)
      iv_question = 'Deleting file is irreversible. Continue?'(def)
      iv_icon_1   = 'ICON_DELETE_TEMPLATE' ) = abap_true.

  " Delete in attachments
  _oaor_show( iv_delete = abap_true ).
ENDMETHOD.


METHOD _oaor_delete_file.
  DATA:
    lv_key           TYPE sbdst_object_key,
    lt_bds_signature TYPE sbdst_signature,
    ls_bds_signature TYPE bapisignat.

  " Subfolder in OAOR (and classname = package_id)
  lv_key = iv_option_id.

  " Prepare signature
  MOVE-CORRESPONDING is_oaor_file TO ls_bds_signature.
  APPEND ls_bds_signature TO lt_bds_signature.

  cl_bds_document_set=>delete(
    EXPORTING
      classname      = iv_pack_id
      classtype      = mc-oaor_other
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
ENDMETHOD.


METHOD _oaor_diloag_screen.
  " Show in screen PARAMETERS:
  DATA ls_dyn_scr  TYPE REF TO zsaqo_oaor_dialog.
  DATA lo_screen   TYPE REF TO zcl_eui_screen.
  DATA lo_err      TYPE REF TO zcx_eui_exception.
  DATA lv_input    TYPE screen-input.
  DATA lv_cmd      TYPE syucomm.

  CLEAR ev_ok.

  " Fill scrren with values
  CREATE DATA ls_dyn_scr.
  ls_dyn_scr->p_3_pack   = mv_package_id.
  ls_dyn_scr->p_3_opt    = mv_option_id.
  ls_dyn_scr->p_3_file   = cv_file_name.
  ls_dyn_scr->p_3_vers   = iv_doc_ver_no.
  " Editable
  ls_dyn_scr->p_3_desc   = cv_description.
  ls_dyn_scr->p_3_vis    = cv_visible.

  " Create screen manager
  TRY.
      CREATE OBJECT lo_screen
        EXPORTING
          iv_dynnr        = '1030'
          iv_cprog        = mc_prog-editor
          ir_context      = ls_dyn_scr
          iv_status_title = iv_title. " 'New file info'(nfi). " Set pf-status & text
    CATCH zcx_eui_exception INTO lo_err.
      MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  " Ok & Cancel
  lv_input = '1'.
  IF me->mv_tcode <> mc_prog-editor_tcode OR zcl_aqo_helper=>is_dev_mandt( ) <> abap_true.
    APPEND 'OK' TO lo_screen->ms_status-exclude.
    lv_input = '0'.
  ENDIF.

  " Static PF status no need on_pbo_event.
  lo_screen->customize( name = 'P_3_PACK'   input = '0' ).
  lo_screen->customize( name = 'P_3_OPT'    input = '0' ).
  lo_screen->customize( name = 'P_3_FILE'   required   = '1'
                                            it_listbox = it_listbox ).
  lo_screen->customize( name = 'P_3_VERS'   input = '0' ).
  lo_screen->customize( name = 'P_3_DESC'   input = lv_input ).
  lo_screen->customize( name = 'P_3_VIS'    input = lv_input ).

  " As popup
  lo_screen->popup( iv_col_end = 118 ).

  " Process action
  lv_cmd = lo_screen->show( ).
  CHECK lv_cmd = zif_eui_manager=>mc_cmd-ok.

  cv_description = ls_dyn_scr->p_3_desc.
  cv_visible     = ls_dyn_scr->p_3_vis.
  " !now index in list box!
  cv_file_name   = ls_dyn_scr->p_3_file.
  ev_ok                    = abap_true.
ENDMETHOD.


METHOD _oaor_get_files.
  DATA:
    lv_key               TYPE sbdst_object_key,
    lt_sbdst_signature   TYPE sbdst_signature,
    ls_sbdst_signature   TYPE REF TO bapisignat,
    lt_sbdst_components2 TYPE sbdst_components2,
    ls_sbdst_components2 TYPE REF TO bapicompo2,
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
    classtype           = mc-oaor_other
    object_key          = lv_key
   IMPORTING
    extended_components = lt_sbdst_components2
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

  " Only last
  IF iv_last_only = abap_true.
    DELETE et_oaor_file WHERE last_version <> abap_true.
  ENDIF.

  " Max versions last
  SORT et_oaor_file BY doc_id doc_ver_no ASCENDING.

  LOOP AT et_oaor_file REFERENCE INTO ls_oaor_file.
    ls_oaor_file->tabix      = sy-tabix.
    ls_oaor_file->package_id = iv_pack_id.
    ls_oaor_file->option_id	 = iv_option_id.

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

  READ TABLE et_oaor_file INTO es_oaor_last
   WITH KEY last_version = abap_true
            file_name    = iv_filename.
ENDMETHOD.


METHOD _oaor_has_visible_files.
  DATA:
    ls_visible   TYPE ts_visible,
    lr_visible   TYPE REF TO ts_visible,
    lt_oaor_file TYPE zcl_aqo_menu_handler=>tt_oaor_file.

  READ TABLE mt_visible REFERENCE INTO lr_visible
   WITH TABLE KEY pack = iv_pack
                  opt  = iv_opt.

  " Serach for the first time
  IF sy-subrc <> 0.
    ls_visible-pack = iv_pack.
    ls_visible-opt  = iv_opt.

    " Get all files
    zcl_aqo_menu_handler=>_oaor_get_files(
     EXPORTING
       iv_pack_id   = ls_visible-pack
       iv_option_id = ls_visible-opt
       iv_last_only = abap_true
     IMPORTING
       et_oaor_file = lt_oaor_file ).

    " Only visible and new files
    DELETE lt_oaor_file WHERE visible <> abap_true.
    IF lt_oaor_file IS NOT INITIAL.
      ls_visible-visible = abap_true.
    ENDIF.

    " Insert for speed
    INSERT ls_visible INTO TABLE mt_visible REFERENCE INTO lr_visible.
  ENDIF.

  " And return
  rv_visible = lr_visible->visible.
ENDMETHOD.


METHOD _oaor_import.
  DATA lt_file           TYPE sbdst_files.
  DATA lr_file           TYPE REF TO bapifiles.
  DATA ls_oaor_file      TYPE zcl_aqo_menu_handler=>ts_oaor_file.
  DATA lv_oaor_mode      TYPE STRING.
  DATA lv_ext            TYPE STRING.
  DATA lt_property       TYPE STANDARD TABLE OF bapiproper WITH DEFAULT KEY.
  DATA lr_property       TYPE REF TO bapiproper.
  DATA lv_ar_object      TYPE toadv-ar_object.
  DATA lt_bds_signature  TYPE sbdst_signature.
  DATA ls_bds_signature  TYPE bapisignat.
  DATA lv_key            TYPE sbdst_object_key.
  DATA lv_new_doc_ver_no TYPE sbdst_doc_ver_no.
  DATA lv_task           TYPE e070-trkorr.
  DATA lv_message        TYPE string.
  DATA lv_last_index     TYPE i.

  " Get new file info
  _oaor_import_get_version(
   IMPORTING
     et_file      = lt_file
     es_oaor_file = ls_oaor_file " new file
     ev_oaor_mode = lv_oaor_mode
     ev_ext       = lv_ext ).
  CHECK ls_oaor_file IS NOT INITIAL.

  " Only 1 file
  READ TABLE lt_file REFERENCE INTO lr_file INDEX 1.
  ls_bds_signature-doc_count = lr_file->doc_count.

  " always equal to file name (Case sensetive)
  APPEND INITIAL LINE TO lt_property REFERENCE INTO lr_property.
  lr_property->prop_name  = 'DESCRIPTION'.
  lr_property->prop_value = ls_oaor_file-description.

  APPEND INITIAL LINE TO lt_property REFERENCE INTO lr_property.
  lr_property->prop_name  = 'BDS_DOCUMENTCLASS'.
  lr_property->prop_value = lv_ext.

  APPEND INITIAL LINE TO lt_property REFERENCE INTO lr_property.
  lr_property->prop_name  = 'LANGUAGE'.
  lr_property->prop_value = sy-langu.

  " Add as keyword
  IF ls_oaor_file-visible = abap_true.
    APPEND INITIAL LINE TO lt_property REFERENCE INTO lr_property.
    lr_property->prop_name  = 'BDS_KEYWORD'.
    lr_property->prop_value = 'VISIBLE'.
  ENDIF.

**********************************************************************
  " Subfolder in OAOR (and classname = package_id)
  lv_key = mv_option_id. " mo_option->ms_db_item-option_id

  CASE lv_oaor_mode.

      " First version
    WHEN mc_oaor-new_file.

      " Detect folder  'BDS_ATTACH' is first
      SELECT SINGLE ar_object INTO lv_ar_object
      FROM toadv
      WHERE standard = abap_true.
      CHECK sy-subrc = 0.

      APPEND INITIAL LINE TO lt_property REFERENCE INTO lr_property.
      lr_property->prop_name  = 'BDS_DOCUMENTTYPE'.
      lr_property->prop_value = lv_ar_object.

      " Convert to another table
      LOOP AT lt_property REFERENCE INTO lr_property.
        MOVE-CORRESPONDING lr_property->* TO ls_bds_signature.
        APPEND ls_bds_signature TO lt_bds_signature.
      ENDLOOP.

      cl_bds_document_set=>create_with_files(
         EXPORTING
           classname       = mo_option->ms_db_item-package_id
           classtype       = mc-oaor_other
         CHANGING
           object_key      = lv_key
           signature       = lt_bds_signature
           files           = lt_file
         EXCEPTIONS
           OTHERS          = 7 ).
      IF sy-subrc = 0.
        " new version
        zcl_aqo_menu_handler=>_oaor_get_files(
         EXPORTING
          iv_pack_id   = mo_option->ms_db_item-package_id
          iv_option_id = mo_option->ms_db_item-option_id
          iv_filename  = lr_file->filename
          iv_last_only = abap_true
         IMPORTING
          es_oaor_last = ls_oaor_file ).
      ELSE.
        zcx_aqo_exception=>raise_sys_error( ).
      ENDIF.

**********************************************************************
      " Set new version
    WHEN mc_oaor-new_version.
      cl_bds_document_set=>create_version_with_files(
         EXPORTING
           classname       = mo_option->ms_db_item-package_id
           classtype       = mc-oaor_other
           object_key      = lv_key
           doc_id          = ls_oaor_file-doc_id
           doc_ver_no      = ls_oaor_file-doc_ver_no
           doc_var_id      = ls_oaor_file-doc_var_id
         IMPORTING
           new_doc_ver_no  = lv_new_doc_ver_no
         CHANGING
           files           = lt_file
           properties      = lt_property
         EXCEPTIONS
           OTHERS          = 7 ).
      IF sy-subrc = 0.
        ls_oaor_file-doc_ver_no = lv_new_doc_ver_no.
      ELSE.
        zcx_aqo_exception=>raise_sys_error( ).
      ENDIF.

**********************************************************************
      " Update existing
    WHEN mc_oaor-update_version.
      cl_bds_document_set=>update_with_files(
       EXPORTING
        classname       = mo_option->ms_db_item-package_id
        classtype       = mc-oaor_other
        object_key       = lv_key
        doc_id          = ls_oaor_file-doc_id
        doc_ver_no      = ls_oaor_file-doc_ver_no
        doc_var_id      = ls_oaor_file-doc_var_id
        x_force_update  = abap_true
       CHANGING
        files           = lt_file
        properties      = lt_property
       EXCEPTIONS
        OTHERS          = 7 ).
      IF sy-subrc <> 0.
        zcx_aqo_exception=>raise_sys_error( ).
      ENDIF.

    WHEN OTHERS.
      zcx_aqo_exception=>raise_dump( iv_message = 'Please check OAOR mode'(poa) ).
  ENDCASE.

  " Put new file in transport
  zcl_aqo_menu_handler=>check_in_request(
   EXPORTING
     is_oaor_file  = ls_oaor_file
   CHANGING
     cv_task       = lv_task
     cv_ok_message = lv_message ).
  IF lv_message IS NOT INITIAL.
    MESSAGE lv_message TYPE 'S'.
  ENDIF.

**********************************************************************

  " Read previous
  DATA lt_all_version TYPE zcl_aqo_menu_handler=>tt_oaor_file.
  zcl_aqo_menu_handler=>_oaor_get_files(
   EXPORTING
    iv_pack_id   = mo_option->ms_db_item-package_id
    iv_option_id = mo_option->ms_db_item-option_id
    iv_filename  = lr_file->filename
    iv_last_only = abap_false
   IMPORTING
    et_oaor_file = lt_all_version ).

  " Delete obselete data
  lv_last_index = lines( lt_all_version ).
  lv_last_index = lv_last_index - mo_option->ms_db_item-prev_value_cnt.
  DO lv_last_index TIMES.
    READ TABLE lt_all_version INTO ls_oaor_file INDEX sy-index.
    CHECK sy-subrc = 0.

    zcl_aqo_menu_handler=>_oaor_delete_file(
      iv_pack_id   = mo_option->ms_db_item-package_id
      iv_option_id = mo_option->ms_db_item-option_id
      is_oaor_file = ls_oaor_file ).
  ENDDO.
ENDMETHOD.


METHOD _oaor_import_get_version.
  DATA ls_result_oaor    LIKE es_oaor_file.
  DATA lt_many_file      TYPE zcl_aqo_menu_handler=>tt_oaor_file.
  DATA lv_key            TYPE sbdst_object_key.
  DATA lv_task           TYPE e070-trkorr.
  DATA lv_message        TYPE string.
  DATA lr_file           TYPE REF TO bapifiles.
  DATA ls_screen_file    TYPE zcl_aqo_menu_handler=>ts_oaor_file.
  DATA lt_file_table     TYPE filetable.
  DATA ls_file_table     TYPE REF TO file_table.
  DATA lv_rc             TYPE i.
  DATA lv_action         TYPE i.
  DATA lv_ok             TYPE abap_bool.
  DATA lt_listbox        TYPE vrm_values.
  DATA lv_listbox_ind    TYPE num2.
  DATA lo_error          TYPE REF TO zcx_eui_exception.
  DATA lv_title          TYPE lvc_s_layo-grid_title.

  FIELD-SYMBOLS:
    <ls_result_oaor> TYPE zcl_aqo_menu_handler=>ts_oaor_file,
    <ls_listbox>     TYPE vrm_value.

  " All is empty
  CLEAR:
    et_file,
    es_oaor_file,
    ev_oaor_mode,
    ev_ext.

  " Subfolder in OAOR (and classname = package_id)
  lv_key = mo_option->ms_db_item-option_id.

  " Get file info
  cl_gui_frontend_services=>file_open_dialog(
   EXPORTING
     multiselection = abap_false
   CHANGING
     file_table     = lt_file_table
     rc             = lv_rc
     user_action    = lv_action
   EXCEPTIONS
     OTHERS      = 1 ).
  CHECK sy-subrc = 0 AND lt_file_table[] IS NOT INITIAL.

  " Get 1-st
  READ TABLE lt_file_table REFERENCE INTO ls_file_table INDEX 1.
  CHECK sy-subrc = 0.

  " Extract info
  APPEND INITIAL LINE TO et_file REFERENCE INTO lr_file.
  zcl_eui_file=>split_file_path(
   EXPORTING
     iv_fullpath  = ls_file_table->filename
   IMPORTING
     ev_path      = lr_file->directory
     ev_filename  = lr_file->filename
     ev_extension = ev_ext ).

  " Create or not
  zcl_aqo_menu_handler=>_oaor_check_exists(
   EXPORTING
     iv_pack_id    = mo_option->ms_db_item-package_id
     iv_option_id  = mo_option->ms_db_item-option_id
   IMPORTING
     ev_task       = lv_task
     ev_ok_message = lv_message ).
  IF lv_message IS NOT INITIAL.
    MESSAGE lv_message TYPE 'S'.
  ENDIF.

  " Nope
  CHECK lv_task IS NOT INITIAL.

  " Always 1 file
  lr_file->comp_count = lr_file->doc_count = 1.
  " Always in UPPER CASE (ID). TODO mac or linux ? (case sensitive)
  TRANSLATE lr_file->filename TO UPPER CASE.

  " Read many files. Only last
  zcl_aqo_menu_handler=>_oaor_get_files(
   EXPORTING
    iv_pack_id   = mo_option->ms_db_item-package_id
    iv_option_id = mo_option->ms_db_item-option_id
    " All last files iv_filename  = ls_file-filename
    iv_last_only = abap_true
   IMPORTING
    et_oaor_file = lt_many_file ).

**********************************************************************
  " Prepare listbox
**********************************************************************

  " Always 1-st position
  APPEND INITIAL LINE TO lt_listbox ASSIGNING <ls_listbox>.
  <ls_listbox>-key  = '01'.
  <ls_listbox>-text = lr_file->filename.

  " Other files list
  LOOP AT lt_many_file ASSIGNING <ls_result_oaor> WHERE file_name <> lr_file->filename.
    " Add as pair
    APPEND INITIAL LINE TO lt_listbox ASSIGNING <ls_listbox>.
    lv_listbox_ind    = sy-tabix.
    <ls_listbox>-key  = lv_listbox_ind.
    <ls_listbox>-text = <ls_result_oaor>-file_name.
  ENDLOOP.

**********************************************************************
  " Detect MODE
**********************************************************************

  " New or existeng item
  READ TABLE lt_many_file INTO ls_result_oaor
   WITH KEY file_name = lr_file->filename.
  IF ls_result_oaor IS INITIAL.
    ls_result_oaor-description = ls_result_oaor-file_name = lr_file->filename.
    ev_oaor_mode = mc_oaor-new_file.
    MESSAGE s036(zaqo_message) INTO lv_title.

  ELSEIF sy-datum = ls_result_oaor-last_changed_at_date.
    ev_oaor_mode = mc_oaor-update_version.
    MESSAGE s038(zaqo_message) WITH ls_result_oaor-doc_ver_no INTO lv_title.

  ELSE.
    ev_oaor_mode = mc_oaor-new_version.
    MESSAGE s037(zaqo_message) WITH ls_result_oaor-doc_ver_no INTO lv_title.
  ENDIF.

  " Also show as message
  MESSAGE lv_title TYPE 'S'.
**********************************************************************

  " Only now SET index in LSITBOX
  ls_result_oaor-file_name = '01'.

  ls_screen_file = ls_result_oaor.
  _oaor_diloag_screen(
   EXPORTING
     iv_doc_ver_no  = ls_screen_file-doc_ver_no
     it_listbox     = lt_listbox[]
     iv_title       = lv_title
   IMPORTING
     ev_ok          = lv_ok
   CHANGING
     cv_file_name   = ls_screen_file-file_name
     cv_description = ls_screen_file-description
     cv_visible     = ls_screen_file-visible ).
  CHECK lv_ok = abap_true.

  " get from index
  DO 1 TIMES.
    " Get new name
    lv_listbox_ind = ls_screen_file-file_name.
    READ TABLE lt_listbox ASSIGNING <ls_listbox>
     WITH KEY key = lv_listbox_ind.

    " Get name by index
    CHECK sy-subrc  = 0.
    ls_result_oaor-file_name = <ls_listbox>-text.

**********************************************************************
    " Choosed another file name
**********************************************************************
    CHECK lv_listbox_ind <> '01'.

    " Read again with a new name
    READ TABLE lt_many_file INTO ls_result_oaor
     WITH KEY file_name = <ls_listbox>-text.
    CHECK sy-subrc = 0.

    IF sy-datum = ls_result_oaor-last_changed_at_date.
      ev_oaor_mode = mc_oaor-update_version.
      MESSAGE s038(zaqo_message) WITH ls_result_oaor-doc_ver_no.

    ELSE.
      ev_oaor_mode = mc_oaor-new_version.
      MESSAGE s037(zaqo_message) WITH ls_result_oaor-doc_ver_no.
    ENDIF.

    " Rename file
    TRY.
        _oaor_change_file_name(
          EXPORTING
            iv_file_name = ls_result_oaor-file_name
          CHANGING
            cs_file      = lr_file->* ).
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
  ENDDO.

  " Get modified
  ls_result_oaor-description = ls_screen_file-description.
  ls_result_oaor-visible     = ls_screen_file-visible.
  " Return result in the end
  es_oaor_file               = ls_result_oaor.
ENDMETHOD.


METHOD _oaor_show.
  DATA:
    lt_oaor_file TYPE zcl_aqo_menu_handler=>tt_oaor_file,
    ls_oaor_file TYPE REF TO zcl_aqo_menu_handler=>ts_oaor_file,
    lt_return    TYPE STANDARD TABLE OF ddshretval WITH DEFAULT KEY,
    ls_return    TYPE REF TO ddshretval,
    ls_object_id TYPE sdokobject,
    lt_info      TYPE STANDARD TABLE OF sdokfilaci,
    ls_info      TYPE REF TO sdokfilaci,
    lt_text      TYPE STANDARD TABLE OF sdokcntasc,
    lt_bin       TYPE STANDARD TABLE OF sdokcntbin,
    lv_filetype  TYPE char10,
    lv_file_size TYPE i,
    lv_index     TYPE i,
    lv_message   TYPE string,
    lv_task      TYPE e070-trkorr,
    lo_file      TYPE REF TO zcl_eui_file,
    lo_error     TYPE REF TO zcx_eui_exception.
  FIELD-SYMBOLS:
   <lt_table>   TYPE STANDARD TABLE.

  zcl_aqo_menu_handler=>_oaor_get_files(
   EXPORTING
     iv_pack_id   = mo_option->ms_db_item-package_id
     iv_option_id = mo_option->ms_db_item-option_id
     iv_last_only = abap_false
   IMPORTING
     et_oaor_file = lt_oaor_file ).

  " Visible for end users
  DO 1 TIMES.
    CHECK iv_vis_only = abap_true.

    " Hide tech files
    DELETE lt_oaor_file WHERE visible <> abap_true OR last_version <> abap_true.
    lv_index = lines( lt_oaor_file ).

    " Oops!
    CHECK lv_index = 0.
    MESSAGE 'No user guide was found'(nug) TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDDO.

  " Just read first item
  IF lv_index = 1.
    READ TABLE lt_oaor_file REFERENCE INTO ls_oaor_file INDEX 1.
  ELSE.
    " Show dialog
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        ddic_structure   = 'ZSAQO_OAOR_FILE_F4'
        retfield         = 'TABIX'
        callback_program = 'SAPLZFG_AQO_MENU'
        callback_form    = 'CALLBACK_OAOR_F4'
        value_org        = 'S'
      TABLES
        value_tab        = lt_oaor_file
        return_tab       = lt_return
      EXCEPTIONS
        OTHERS           = 3.
    CHECK sy-subrc = 0.

    " First item
    READ TABLE lt_return REFERENCE INTO ls_return INDEX 1.
    CHECK sy-subrc = 0.

    " Read by TABIX in table
    lv_index = ls_return->fieldval.
    READ TABLE lt_oaor_file REFERENCE INTO ls_oaor_file
      WITH KEY tabix = lv_index.
  ENDIF.

  " Is Ok
  CHECK sy-subrc = 0.

  " delete
  IF iv_delete = abap_true.
    " Request for deleting file
    zcl_aqo_menu_handler=>_oaor_check_exists(
     EXPORTING
       iv_pack_id    = mo_option->ms_db_item-package_id
       iv_option_id  = mo_option->ms_db_item-option_id
     IMPORTING
       ev_task       = lv_task
       ev_ok_message = lv_message ).
    IF lv_message IS NOT INITIAL.
      MESSAGE lv_message TYPE 'S'.
    ENDIF.

    IF lv_task IS NOT INITIAL.
      zcl_aqo_menu_handler=>_oaor_delete_file(
         iv_pack_id   = mo_option->ms_db_item-package_id
         iv_option_id = mo_option->ms_db_item-option_id
         is_oaor_file = ls_oaor_file->*  ).
    ENDIF.

    " Go out
    RETURN.
  ENDIF.

  MOVE-CORRESPONDING ls_oaor_file->* TO ls_object_id.
  CALL FUNCTION 'SDOK_PHIO_LOAD_CONTENT'
    EXPORTING
      object_id           = ls_object_id
      text_as_stream      = abap_true
    TABLES
      file_access_info    = lt_info
      file_content_ascii  = lt_text
      file_content_binary = lt_bin
    EXCEPTIONS
      OTHERS              = 5.
  CHECK sy-subrc = 0.

  READ TABLE lt_info REFERENCE INTO ls_info INDEX 1.
  CHECK sy-subrc = 0.

  " Text or binary
  IF lt_bin[] IS NOT INITIAL.
    ASSIGN lt_bin  TO <lt_table>.
    lv_filetype  = 'BIN'.
    lv_file_size = ls_info->file_size.
  ELSE.
    ASSIGN lt_text TO <lt_table>.
    lv_filetype  = 'ASC'.
  ENDIF.

  " Download and open
  CREATE OBJECT lo_file.
  TRY.
      lo_file->import_from_binary(
       it_table  = <lt_table>
       iv_length = lv_file_size ).

      lo_file->download(
        iv_full_path   = ls_oaor_file->file_name
        iv_filetype    = lv_filetype
        iv_save_dialog = abap_true ).

      lo_file->open( ).
    CATCH zcx_eui_exception INTO lo_error.
      zcx_aqo_exception=>raise_sys_error( io_error = lo_error ).
  ENDTRY.
ENDMETHOD.


METHOD _view.
  _change( ).
  rv_update = abap_true.
ENDMETHOD.
ENDCLASS.
