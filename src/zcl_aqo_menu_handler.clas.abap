class ZCL_AQO_MENU_HANDLER definition
  public
  final
  create private

  global friends ZCL_EUI_EVENT_CALLER .

public section.
  type-pools ABAP .

  types:
    BEGIN OF ts_state,
        in_editor     TYPE abap_bool,
        is_dev        TYPE abap_bool,
        in_viewer     TYPE abap_bool,
        lock_info     TYPE stb_button-quickinfo,
        view_only     TYPE abap_bool,

        option_exist  TYPE abap_bool,
        package_exist TYPE abap_bool,
        tcode         TYPE sytcode,

        menu_mode     TYPE ztaqo_option-menu_mode,
      END OF ts_state .

  constants:
    BEGIN OF mc_prog,
        editor       TYPE syrepid VALUE 'ZAQO_EDITOR_OLD',
        editor_tcode TYPE sytcode VALUE 'ZAQO_EDITOR_OLD',
        viewer_tcode TYPE sytcode VALUE 'ZAQO_VIEWER_OLD',
      END OF mc_prog .

  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_EUI_MENU
    importing
      !IV_PACKAGE_ID type CSEQUENCE
      !IV_OPTION_ID type CSEQUENCE
    returning
      value(RO_EUI_MENU) type ref to ZCL_EUI_MENU .
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
    tt_aqo_menu_ext TYPE STANDARD TABLE OF REF TO ZIF_AQO_MENU_EXT .

  constants:
    BEGIN OF mc,
        is_editor  TYPE string VALUE 'IS_EDITOR',
        delimiter  TYPE char1 VALUE '^',
      END OF mc .
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
        about          TYPE ui_func VALUE '_ABOUT',
      END OF mc_action .
  class-data MT_UNQ_MENU type TT_UNQ_MENU .
  class-data MT_ALL_MENU type ZCL_EUI_MENU=>TT_MENU .
  class-data MT_AQO_MENU_EXT type TT_AQO_MENU_EXT .
  data MV_IN_EDITOR type ABAP_BOOL .
  data MV_PACKAGE_ID type ZTAQO_OPTION-PACKAGE_ID .
  data MV_OPTION_ID type ZTAQO_OPTION-OPTION_ID .
  data MV_TCODE type SYTCODE .
  data MO_OPTION type ref to ZCL_AQO_OPTION .
  data MO_EUI_MENU type ref to ZCL_EUI_MENU .

  class-methods _GET_STATE_INFO
    importing
      !IV_PACKAGE_ID type CSEQUENCE
      !IV_OPTION_ID type CSEQUENCE
      !IV_IN_EDITOR type ABAP_BOOL
    returning
      value(RS_STATE) type TS_STATE .
  class-methods _READ_LOCKS
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
      !IS_STATE type TS_STATE
    returning
      value(RT_MENU) type ZCL_EUI_MENU=>TT_MENU .
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
ENDCLASS.



CLASS ZCL_AQO_MENU_HANDLER IMPLEMENTATION.


METHOD class_constructor.
  DATA lt_ext TYPE STANDARD TABLE OF seometarel-clsname.
  SELECT DISTINCT clsname INTO TABLE lt_ext "#EC CI_GENBUFF "#EC CI_BYPASS
  FROM seometarel
  WHERE refclsname = 'ZIF_AQO_MENU_EXT'
    AND state      = '1'
    AND reltype    = '1'.

  DATA lv_class_name LIKE LINE OF lt_ext.
  DATA lo_menu_ext   TYPE REF TO zif_aqo_menu_ext.
  LOOP AT lt_ext INTO lv_class_name.
    CREATE OBJECT lo_menu_ext TYPE (lv_class_name).
    APPEND lo_menu_ext TO mt_aqo_menu_ext.
  ENDLOOP.
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
    DATA ls_state TYPE ts_state.
    ls_state = _get_state_info( iv_package_id = iv_package_id
                                iv_option_id  = iv_option_id
                                iv_in_editor  = lv_in_editor ).
    lo_unq_menu->mv_tcode = ls_state-tcode.

    " prepare buttons
    lt_menu = _menu_get_buttons( iv_package_id = iv_package_id
                                 iv_option_id  = iv_option_id
                                 is_state      = ls_state ).

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

        lo_toolbar->set_enable( EXPORTING  enable = lv_enabled
                                EXCEPTIONS OTHERS = 0 ).
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
  CHECK ro_eui_menu IS NOT INITIAL.
  ro_eui_menu->add_handler( lo_unq_menu ).
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
      DATA lv_prog TYPE sycprog.
      CONCATENATE mc_prog-editor `ABOUT_SCR` INTO lv_prog.
      CREATE OBJECT lo_screen
        EXPORTING
          iv_dynnr   = zcl_eui_screen=>mc_dynnr-dynamic
          iv_cprog   = lv_prog
          ir_context = ls_dyn_scr.

      DATA ls_status TYPE zcl_eui_manager=>ts_status.
      ls_status-name  = 'ABOUT_STATUS'.
      ls_status-prog  = mc_prog-editor.
      ls_status-title = 'Enter option description'(eod).
      lo_screen->set_status( ls_status ).

    CATCH zcx_eui_exception INTO lo_err.
      MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  " Exclude buttons
  IF me->mv_tcode = mc_prog-editor_tcode AND zcl_aqo_helper=>is_dev_mandt( ) = abap_true.
    lv_input = '1'.
  ELSE.
    lv_input = '0'.
    APPEND 'MODIFY'      TO lo_screen->ms_status-exclude.
    APPEND 'DEV_INFO'    TO lo_screen->ms_status-exclude.
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
  lo_screen->popup( iv_col_end = 114 ).

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


METHOD _get_state_info.
  rs_state-in_editor = iv_in_editor.
  rs_state-is_dev    = zcl_aqo_helper=>is_dev_mandt( ).
  rs_state-lock_info = _read_locks(
   iv_package_id = iv_package_id
   iv_option_id  = iv_option_id ).

  " Exist ?
  SELECT SINGLE menu_mode INTO rs_state-menu_mode
  FROM ztaqo_option
  WHERE package_id = iv_package_id
    AND option_id  = iv_option_id.
  IF sy-subrc = 0.
    rs_state-option_exist = abap_true.
  ELSE.
    DATA lv_devclass TYPE tdevc-devclass.
    SELECT SINGLE devclass INTO lv_devclass
    FROM tdevc
    WHERE devclass = iv_package_id.
    IF sy-subrc = 0.
      rs_state-package_exist = abap_true.
    ENDIF.
  ENDIF.

  IF iv_in_editor = abap_true.
    rs_state-in_viewer = zcl_aqo_helper=>is_in_editor( iv_is_viewer = abap_true ).
    rs_state-tcode     = sy-tcode.
  ELSEIF rs_state-option_exist = abap_true.

    CASE rs_state-menu_mode.
      WHEN mc_menu_mode-hide.
        RETURN.

      WHEN mc_menu_mode-view.
        " Change tooltip
        IF rs_state-lock_info IS INITIAL.
          MESSAGE s034(zaqo_message) WITH 'settings'(stt) INTO rs_state-lock_info.
        ENDIF.
        rs_state-tcode = mc_prog-viewer_tcode.

      WHEN mc_menu_mode-edit.
        " Do nothing

      WHEN OTHERS.
        zcx_aqo_exception=>raise_dump( iv_message = 'Unknown MENU_MODE'(umm) ).
    ENDCASE.

    " If not locked by user or settings
    IF rs_state-lock_info IS INITIAL.
      rs_state-tcode = mc_prog-editor_tcode.
    ENDIF.
  ENDIF.

  " Change visibility
  IF rs_state-in_viewer = abap_true OR rs_state-lock_info IS NOT INITIAL.
    rs_state-view_only = abap_true.
  ENDIF.
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
        DO 1 TIMES.
          CHECK lv_method CP '_EXT*'.

          DATA lo_menu_ext TYPE REF TO zif_aqo_menu_ext.
          LOOP AT mt_aqo_menu_ext INTO lo_menu_ext.
            CHECK lo_menu_ext->button_pressed( iv_command = fcode
                                               is_db_item = mo_option->ms_db_item
                                               iv_tcode   = mv_tcode ) = abap_true.
            " lv_update?
            RETURN.
          ENDLOOP.
        ENDDO.

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
  DATA lv_root_func TYPE ui_func.
**********************************************************************
  " INITIALIZE the menu
**********************************************************************
  FIELD-SYMBOLS <ls_menu> LIKE LINE OF rt_menu.
  IF is_state-in_editor <> abap_true. " Is not in editor transaction
    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.

    " Always has parent ID ?
    CONCATENATE iv_package_id mc-delimiter iv_option_id INTO lv_root_func.

    " Just tooltip
    CONCATENATE iv_package_id ` - ` iv_option_id INTO <ls_menu>-quickinfo.
    <ls_menu>-icon         = icon_tools.
    <ls_menu>-butn_type    = cntb_btype_menu.
    <ls_menu>-text         = <ls_menu>-quickinfo.
    <ls_menu>-function     = lv_root_func.
  ENDIF.

  APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = 'New'(crt).
  <ls_menu>-icon         = icon_create.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-new.
  <ls_menu>-par_function = lv_root_func.

  APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = 'View'(viw).
  <ls_menu>-quickinfo    = is_state-lock_info.
  <ls_menu>-icon         = icon_display.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-view.
  <ls_menu>-par_function = lv_root_func.

  APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = <ls_menu>-quickinfo    = 'Edit'(edt).
  <ls_menu>-icon         = icon_change.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-change.
  <ls_menu>-par_function = lv_root_func.

  APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
  <ls_menu>-butn_type    = cntb_btype_sep.
  <ls_menu>-par_function = lv_root_func.

  APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = 'Transport'(trn).
  <ls_menu>-quickinfo    = is_state-lock_info.
  <ls_menu>-icon         = icon_transport.
  <ls_menu>-butn_type    = cntb_btype_menu.
  <ls_menu>-function     = mc_action-transport_root.
  <ls_menu>-par_function = lv_root_func.

  APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
  <ls_menu>-butn_type    = cntb_btype_sep.
  <ls_menu>-par_function = mc_action-transport_root.

  APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = <ls_menu>-quickinfo    = 'Export'(exp).
  <ls_menu>-icon         = icon_export.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-export.
  <ls_menu>-par_function = mc_action-transport_root.

  APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = 'Import'(imp).
  <ls_menu>-quickinfo    = is_state-lock_info.
  <ls_menu>-icon         = icon_import.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-import.
  <ls_menu>-par_function = mc_action-transport_root.

  APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
  <ls_menu>-butn_type    = cntb_btype_sep.
  <ls_menu>-par_function = mc_action-transport_root.

  APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = 'Delete'(del).
  <ls_menu>-quickinfo    = is_state-lock_info.
  <ls_menu>-icon         = icon_delete.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-delete.
  <ls_menu>-par_function = mc_action-transport_root.

**********************************************************************
  APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = <ls_menu>-quickinfo    = 'Where-Used List'(wul).
  <ls_menu>-icon         = icon_reference_list.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-last_code.
  <ls_menu>-par_function = lv_root_func.

**********************************************************************
  DATA lo_menu_ext TYPE REF TO zif_aqo_menu_ext.
  LOOP AT mt_aqo_menu_ext INTO lo_menu_ext.
    lo_menu_ext->add_buttons( EXPORTING iv_root_func = lv_root_func
                                        is_state     = is_state
                              CHANGING  ct_menu      = rt_menu ).
  ENDLOOP.
**********************************************************************

  APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
  <ls_menu>-butn_type    = cntb_btype_sep.
  <ls_menu>-par_function = lv_root_func.
  <ls_menu>-function     = mc_action-about_sep. " Always show

  APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
  <ls_menu>-text         = <ls_menu>-quickinfo    = 'About'(abo).
  <ls_menu>-icon         = icon_tools.
  <ls_menu>-butn_type    = cntb_btype_button.
  <ls_menu>-function     = mc_action-about.
  <ls_menu>-par_function = lv_root_func.

  " 0 - is_state-is_dev, 1 - is_state-in_editor, 2 - is_state-view_only, 3 - is_state-option_exist
  LOOP AT rt_menu ASSIGNING <ls_menu>.
    CASE <ls_menu>-function.
*        " Just skip
*      WHEN mc_action-base.

      WHEN mc_action-last_code.
        IF is_state-option_exist <> abap_true OR is_state-is_dev <> abap_true.
          <ls_menu>-hide = abap_true.
        ENDIF.

      WHEN mc_action-transport_root.
        IF is_state-option_exist <> abap_true. " Show EXPORT OR is_state-is_dev <> abap_true.
          <ls_menu>-hide = abap_true.
        ENDIF.

      WHEN mc_action-new.
        IF   is_state-option_exist = abap_true
          OR is_state-view_only = abap_true.
          <ls_menu>-hide = abap_true.
        ENDIF.

        IF is_state-is_dev <> abap_true.
          <ls_menu>-disabled = abap_true.
          <ls_menu>-quickinfo = 'Only in DEV!'(oid).
        ENDIF.

        " For info
        IF is_state-option_exist <> abap_true AND is_state-package_exist <> abap_true.
          <ls_menu>-disabled = abap_true.
          <ls_menu>-quickinfo = 'The package do not exist'(tpd).
        ENDIF.

        IF <ls_menu>-quickinfo IS INITIAL.
          <ls_menu>-quickinfo = 'SE38->ZAQO_TESTER is simpler'(se3).
        ENDIF.

      WHEN mc_action-change.
        IF    is_state-option_exist <> abap_true
           OR is_state-in_editor = abap_true
           OR is_state-view_only = abap_true.
          <ls_menu>-hide = abap_true.
        ENDIF.

      WHEN mc_action-view.
        IF    is_state-option_exist <> abap_true
           OR is_state-in_editor = abap_true
           OR is_state-view_only <> abap_true.
          <ls_menu>-hide = abap_true.
        ENDIF.

      WHEN mc_action-import OR mc_action-delete. " OR mc_action-transport  OR mc_action-save_in
        IF is_state-option_exist <> abap_true.
          <ls_menu>-hide = abap_true.
        ENDIF.

        IF is_state-is_dev <> abap_true
           OR is_state-in_editor <> abap_true
           OR is_state-view_only = abap_true.
          <ls_menu>-disabled = abap_true.
          <ls_menu>-quickinfo = 'In DEV editor only!'(ido).
        ENDIF.

      WHEN mc_action-export.

      WHEN mc_action-about OR mc_action-about_sep.
        IF is_state-option_exist <> abap_true.
          <ls_menu>-hide = abap_true.
        ENDIF.

      WHEN OTHERS.
        " For separators only
        DATA lv_prev_hide TYPE abap_bool.
        IF <ls_menu>-butn_type = cntb_btype_sep.
          <ls_menu>-hide = lv_prev_hide.
        ELSEIF <ls_menu>-function <> lv_root_func AND <ls_menu>-function NP '_EXT*'.
          zcx_aqo_exception=>raise_dump( iv_message = 'Unknown button'(unb) ).
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
***      IF is_state-in_editor = abap_true AND <ls_menu>-par_function = lv_root_func.
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


METHOD _read_locks.
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


METHOD _view.
  _change( ).
  rv_update = abap_true.
ENDMETHOD.
ENDCLASS.
