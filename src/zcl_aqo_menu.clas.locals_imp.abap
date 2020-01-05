*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_aqo_option IMPLEMENTATION.
  METHOD get_menu.
    DATA:
      lv_in_editor TYPE abap_bool,
      ls_unq_menu  TYPE ts_unq_menu,
      lr_unq_menu  TYPE REF TO ts_unq_menu,
      lt_menu      TYPE zcl_aqo_menu=>tt_menu.

    lv_in_editor = zcl_aqo_helper=>is_in_editor( ).
    IF lv_in_editor = abap_true.
      ls_unq_menu-unq_id = 'ZFM_AQO_MENU_INIT'.
    ELSE.
      CONCATENATE iv_package_id '-' iv_option_id INTO ls_unq_menu-unq_id.
    ENDIF.

    " Check menu existence
    READ TABLE mt_unq_menu REFERENCE INTO lr_unq_menu
     WITH TABLE KEY unq_id = ls_unq_menu-unq_id.

    " Create new GOS menu
    IF sy-subrc <> 0.
      CREATE OBJECT ls_unq_menu-menu.
      INSERT ls_unq_menu INTO TABLE mt_unq_menu REFERENCE INTO lr_unq_menu.
    ENDIF.

    " Result
    ro_menu = lr_unq_menu->menu.

    " Init or not
    CHECK lr_unq_menu->package_id <> iv_package_id
       OR lr_unq_menu->option_id  <> iv_option_id.

    " If new option
    lr_unq_menu->package_id = iv_package_id.
    lr_unq_menu->option_id  = iv_option_id.

    " prepare buttons
    lt_menu = get_buttons(
     is_unq_menu  = lr_unq_menu
     iv_in_editor = lv_in_editor ).

    " And init
    ro_menu->construct_buttons( lt_menu ).
  ENDMETHOD.

  METHOD get_buttons.
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
     <ls_menu> LIKE LINE OF rt_menu.

    lv_is_dev    = zcl_aqo_helper=>is_dev_mandt( ).
    lv_lock_info = read_locks( is_unq_menu ).

    " Exist ?
    SELECT SINGLE menu_mode INTO lv_menu_mode
    FROM ztaqo_option
    WHERE package_id = is_unq_menu->package_id
      AND option_id  = is_unq_menu->option_id.
    IF sy-subrc = 0.
      lv_option_exist = abap_true.
    ELSE.
      SELECT SINGLE devclass INTO lv_devclass
      FROM tdevc
      WHERE devclass = is_unq_menu->package_id.
      IF sy-subrc = 0.
        lv_package_exist = abap_true.
      ENDIF.
    ENDIF.

    IF iv_in_editor = abap_true.
      lv_in_viewer       = zcl_aqo_helper=>is_in_editor( iv_is_viewer = abap_true ).
      is_unq_menu->tcode = sy-tcode.
    ELSEIF lv_option_exist = abap_true.

      CASE lv_menu_mode.
        WHEN mc_menu_mode-hide.
          RETURN.

        WHEN mc_menu_mode-view.
          " Change tooltip
          IF lv_lock_info IS INITIAL.
            MESSAGE s034(zaqo_message) WITH 'settings' INTO lv_lock_info.
          ENDIF.
          is_unq_menu->tcode = 'ZAQO_VIEWER_OLD'.

        WHEN mc_menu_mode-edit.
          " Do nothing

        WHEN OTHERS.
          MESSAGE 'Unknown MENU_MODE' TYPE 'X'.
      ENDCASE.

      " If not locked by user or settings
      IF lv_lock_info IS INITIAL.
        is_unq_menu->tcode = 'ZAQO_EDITOR_OLD'.
      ENDIF.
    ENDIF.

**********************************************************************
    " INITIALIZE the menu
**********************************************************************
    IF iv_in_editor <> abap_true. " Is not in editor transaction
      APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.

      " Just tooltip
      CONCATENATE is_unq_menu->package_id ` - ` is_unq_menu->option_id INTO <ls_menu>-quickinfo.
      <ls_menu>-icon         = icon_tools.
      <ls_menu>-butn_type    = cntb_btype_menu.
      <ls_menu>-function     = lv_root_func = mc_action-base.
    ENDIF.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = 'New'(crt).
    <ls_menu>-icon         = icon_create.
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-new.
    <ls_menu>-par_function = lv_root_func.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = 'View'(viw).
    <ls_menu>-quickinfo    = lv_lock_info.
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
    <ls_menu>-quickinfo    = lv_lock_info.
    <ls_menu>-icon         = icon_transport.
    <ls_menu>-butn_type    = cntb_btype_menu.
    <ls_menu>-function     = mc_action-transport_root.
    <ls_menu>-par_function = lv_root_func.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = 'Save in mandant'(svi).
    <ls_menu>-quickinfo    = <ls_menu>-text.
    <ls_menu>-icon         = icon_save_as_template.
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-save_in.
    <ls_menu>-par_function = mc_action-transport_root. " mc_action-save_root.

*    SAVE in DEV always calls TRANSPORT method
*    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
*    <ls_menu>-text         = 'Transport'(trn).
*    <ls_menu>-quickinfo    = lv_lock_info.
*    <ls_menu>-icon         = icon_transport.
*    <ls_menu>-butn_type    = cntb_btype_button.
*    <ls_menu>-function     = mc_action-transport.
*    <ls_menu>-par_function = mc_action-transport_root.

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
    <ls_menu>-quickinfo    = lv_lock_info.
    <ls_menu>-icon         = icon_import.
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-import.
    <ls_menu>-par_function = mc_action-transport_root.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-butn_type    = cntb_btype_sep.
    <ls_menu>-par_function = mc_action-transport_root.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = 'Delete'(del).
    <ls_menu>-quickinfo    = lv_lock_info.
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

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-butn_type    = cntb_btype_sep.
    <ls_menu>-par_function = lv_root_func.
    <ls_menu>-function     = mc_action-about_sep. " Always show


    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = <ls_menu>-quickinfo    = 'Attach'(att).
    <ls_menu>-icon         = icon_attachment.
    <ls_menu>-butn_type    = cntb_btype_menu.
    <ls_menu>-function     = mc_action-attach_root.
    <ls_menu>-par_function = lv_root_func.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = <ls_menu>-quickinfo    = 'Add attachment'(aat).
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-attach_import.
    <ls_menu>-par_function = mc_action-attach_root.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = <ls_menu>-quickinfo    = 'Show attachments'(sat).
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-attach_show.
    <ls_menu>-par_function = mc_action-attach_root.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = <ls_menu>-quickinfo    = 'Delete attachment'(dat).
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-attach_delete.
    <ls_menu>-par_function = mc_action-attach_root.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
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
    LOOP AT rt_menu ASSIGNING <ls_menu>.
      CASE <ls_menu>-function.

          " Just skip
        WHEN mc_action-base.

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
            <ls_menu>-quickinfo = 'Only in DEV!'.
          ENDIF.

          " For info
          IF lv_option_exist <> abap_true AND lv_package_exist <> abap_true.
            <ls_menu>-disabled = abap_true.
            <ls_menu>-quickinfo = `The package don't exist`.
          ENDIF.

          IF <ls_menu>-quickinfo IS INITIAL.
            <ls_menu>-quickinfo = 'SE38->ZAQO_TESTER is simpler'.
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

        WHEN mc_action-save_in OR mc_action-import OR mc_action-delete " OR mc_action-transport
             OR mc_action-attach_import OR mc_action-attach_delete.
          IF lv_option_exist <> abap_true.
            <ls_menu>-hide = abap_true.
          ENDIF.

          IF lv_is_dev <> abap_true
             OR iv_in_editor <> abap_true
             OR lv_view_only = abap_true.
            <ls_menu>-disabled = abap_true.
            <ls_menu>-quickinfo = 'In DEV editor only!'.
          ENDIF.

        WHEN mc_action-export OR mc_action-attach_show.

        WHEN mc_action-about OR mc_action-about_sep.
          IF lv_option_exist <> abap_true.
            <ls_menu>-hide = abap_true.
          ENDIF.

        WHEN OTHERS.
          IF <ls_menu>-butn_type = cntb_btype_sep.
            <ls_menu>-hide = lv_prev_hide.
          ELSE.
            MESSAGE 'Unknouwn button' TYPE 'X'.
          ENDIF.
      ENDCASE.

      IF <ls_menu>-disabled = abap_true OR <ls_menu>-hide = abap_true.
        lv_prev_hide = abap_true.
      ELSE.
        lv_prev_hide = abap_false.
      ENDIF.

      CHECK <ls_menu>-par_function IS INITIAL.
      CLEAR <ls_menu>-text.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_locks.
    DATA:
      lv_garg TYPE seqg3-garg,
      lt_lock TYPE STANDARD TABLE OF seqg3 WITH DEFAULT KEY.
    FIELD-SYMBOLS:
     <ls_lock> LIKE LINE OF lt_lock.

    lv_garg+0(3)   = sy-mandt.
    lv_garg+3(30)  = is_unq_menu->package_id.
    lv_garg+33(30) = is_unq_menu->option_id.
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

  METHOD _export.
    DATA:
      lv_title     TYPE string,
      lv_file_name TYPE string,
      lv_path      TYPE string,
      lv_filename  TYPE string,
      lv_fullpath  TYPE string.

    lv_title     = 'Save option values'(sov).
    CONCATENATE io_option->ms_db_item-package_id `-` io_option->ms_db_item-option_id `-` sy-mandt `-` sy-datum `-` sy-uzeit `.aqob` INTO lv_file_name.
    cl_gui_frontend_services=>file_save_dialog(
     EXPORTING
       window_title      = lv_title
       default_file_name = lv_file_name
     CHANGING
       path         = lv_path
       filename     = lv_filename
       fullpath     = lv_fullpath
     EXCEPTIONS
       OTHERS       = 1 ).
    CHECK sy-subrc = 0 AND lv_fullpath IS NOT INITIAL.

    " Save to file
    zcl_aqo_helper=>download(
     iv_xcontent = io_option->ms_db_item-fields
     iv_filename = lv_fullpath ).
  ENDMETHOD.

  METHOD _about.
    DATA:
      lv_ok_code TYPE syucomm,
      ls_db_item TYPE ztaqo_option.

    " Previous values
    ls_db_item = io_option->ms_db_item.

    CALL FUNCTION 'ZFM_AQO_ABOUT_1010'
      IMPORTING
        ev_last_command = lv_ok_code
      CHANGING
        cs_db_item      = ls_db_item.

    CASE lv_ok_code.
      WHEN 'EDIT'.
        rv_update = do_update(
         io_option      = io_option
         iv_set         = `DESCRIPTION = IV_DESCRIPTION PREV_VALUE_CNT = IV_PREV_COUNT MENU_MODE = IV_MENU_MODE`
         iv_description = ls_db_item-description
         iv_prev_count  = ls_db_item-prev_value_cnt
         iv_menu_mode   = ls_db_item-menu_mode ).

      WHEN 'DEV_INFO'.
        " Show online documentation in browser
        CALL FUNCTION 'CALL_BROWSER'
          EXPORTING
            url    = 'https://github.com/bizhuka/aqo/wiki'
          EXCEPTIONS
            OTHERS = 6.
        CHECK sy-subrc = 0.

      WHEN 'USER_INFO'.
        " Check in attachments
        _attach_show( io_option   = io_option
                      is_unq_menu = is_unq_menu
                      iv_vis_only = abap_true ).

        " Ok or cancel
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.

  METHOD _import.
    DATA:
      lt_file  TYPE filetable,
      ls_file  TYPE REF TO file_table,
      lv_file  TYPE string,
      lv_rc    TYPE i,
      lt_data  TYPE solix_tab,
      lv_len   TYPE i,
      lv_xdata TYPE xstring.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        " window_title   =
        multiselection    = abap_false
        file_filter       = '*.aqob'
        default_extension = 'aqob'
      CHANGING
        file_table        = lt_file
        rc                = lv_rc ).
    CHECK lt_file[] IS NOT INITIAL.

    " 1 file only
    READ TABLE lt_file REFERENCE INTO ls_file INDEX 1.
    CHECK sy-subrc = 0.

    lv_file = ls_file->filename.
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename   = lv_file
        filetype   = 'BIN'
      IMPORTING
        filelength = lv_len
      TABLES
        data_tab   = lt_data
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    " TODO check data structure
    lv_xdata = zcl_aqo_helper=>binary_to_xstring(
     it_table  = lt_data
     iv_length = lv_len ).

    rv_update = do_update(
     io_option = io_option
     iv_set    = `FIELDS = IV_FIELDS`
     iv_fields = lv_xdata ).
  ENDMETHOD.

  METHOD do_update.
    UPDATE ztaqo_option
     SET (iv_set)
    WHERE package_id = io_option->ms_db_item-package_id
      AND option_id  = io_option->ms_db_item-option_id.

    IF sy-subrc = 0.
      MESSAGE 'Data updated'(upd) TYPE 'S'.
      rv_ok = abap_true.
    ELSE.
      MESSAGE 'Error during updating!'(edu) TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD _last_code.
    DATA:
      lv_ok TYPE abap_bool.
    IF io_option->ms_db_item-mainprogram IS INITIAL.
      MESSAGE 'No previous call was found'(ncl) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Try to launch by save first save of option
    lv_ok = zcl_aqo_helper=>navigate_to(
       iv_include  = io_option->ms_db_item-include
       iv_position = io_option->ms_db_item-line ).

    " if not OK
    CHECK lv_ok <> abap_true
      AND io_option->ms_db_item-package_id IS NOT INITIAL
      AND io_option->ms_db_item-option_id  IS NOT INITIAL.

    " Pass params
    SET PARAMETER ID:
      'ZAQO_PACKAGE_ID' FIELD io_option->ms_db_item-package_id,
      'ZAQO_OPTION_ID'  FIELD io_option->ms_db_item-option_id.

    " Second attempt by code scan
    PERFORM call_by_name IN PROGRAM zaqo_editor_old
      USING 'CODE_SCAN_F4'.
  ENDMETHOD.

  METHOD _delete.
    " $ & transport
    rv_update = io_option->delete( ).
  ENDMETHOD.

  METHOD _view.
    rv_update = _change(
     is_unq_menu = is_unq_menu
     io_option   = io_option ).
  ENDMETHOD.

  METHOD _new.
    rv_update = _change(
     is_unq_menu = is_unq_menu
     io_option   = io_option
     iv_command  = '_NEW_OPTION' ).
  ENDMETHOD.

  METHOD _change.
    SET PARAMETER ID:
      'ZAQO_PACKAGE_ID' FIELD io_option->ms_db_item-package_id,
      'ZAQO_OPTION_ID'  FIELD io_option->ms_db_item-option_id,
      'ZAQO_COMMAND'    FIELD iv_command.

    CALL TRANSACTION is_unq_menu->tcode " WITH AUTHORITY-CHECK "#EC CI_CALLTA
      AND SKIP FIRST SCREEN.
  ENDMETHOD.

  METHOD _save_in.
    DATA:
      lv_mandt TYPE symandt,
      lv_ok    TYPE abap_bool.

    " Save in another mandant
    lv_mandt = sy-mandt.
    zcl_aqo_helper=>edit_in_popup(
     EXPORTING
       iv_type  = 'T001-MANDT'
       iv_title = 'Specify the client number'(cln)
     CHANGING
       cv_value = lv_mandt
       cv_ok    = lv_ok ).
    CHECK lv_ok = abap_true.

    " check client
    IF lv_mandt = sy-mandt.
      MESSAGE s035(zaqo_message) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

**   check unsaved data exist
*    IF check_unsaved_data( ) EQ abap_true.
**     save data
*      data_save( ).
*    ENDIF.

    "lcl_opt=>do_save( iv_mandt = lv_mandt ).
    io_option->save( iv_mandt = lv_mandt ).
  ENDMETHOD.

  METHOD _attach_import.
    DATA:
      lt_file           TYPE sbdst_files,
      ls_file           TYPE bapifiles,
      lt_oaor_file      TYPE zcl_aqo_helper=>tt_oaor_file,
      ls_oaor_file      TYPE zcl_aqo_helper=>ts_oaor_file,
      lt_property       TYPE STANDARD TABLE OF bapiproper WITH DEFAULT KEY,
      lr_property       TYPE REF TO bapiproper,
      lt_bds_signature  TYPE sbdst_signature,
      ls_bds_signature  TYPE bapisignat,
      lv_key            TYPE sbdst_object_key,
      lt_file_table     TYPE filetable,
      ls_file_table     TYPE REF TO file_table,
      lv_rc             TYPE i,
      lv_action         TYPE i,
      lv_ext            TYPE string,
      lv_new_doc_ver_no TYPE sbdst_doc_ver_no,
      lv_last_index     TYPE i,
      lv_ok             TYPE abap_bool,
      lv_ar_object      TYPE toadv-ar_object,
      lv_task           TYPE e070-trkorr,
      lv_oaor_mode      TYPE string.

    " Subfolder in OAOR (and classname = package_id)
    lv_key = io_option->ms_db_item-option_id.

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

    " Create or not
    zcl_aqo_helper=>oaor_check_exists(
     EXPORTING
       iv_pack_id   = io_option->ms_db_item-package_id
       iv_option_id = io_option->ms_db_item-option_id
     IMPORTING
       ev_task      = lv_task ).
    CHECK lv_task IS NOT INITIAL.

    " First file
    READ TABLE lt_file_table REFERENCE INTO ls_file_table INDEX 1.
    CHECK sy-subrc = 0.

    " Extract info & add
    zcl_aqo_helper=>split_file_path(
     EXPORTING
       iv_fullpath  = ls_file_table->filename
     IMPORTING
       ev_path      = ls_file-directory
       ev_filename  = ls_file-filename
       ev_extension = lv_ext ).

    " Always 1 file
    ls_file-comp_count = ls_file-doc_count = ls_bds_signature-doc_count  = 1.
    " Always in UPPER CASE (ID)
    TRANSLATE ls_file-filename TO UPPER CASE.
    " And add
    APPEND ls_file TO lt_file.

    " Read previous
    zcl_aqo_helper=>oaor_get_files(
     EXPORTING
      iv_pack_id   = io_option->ms_db_item-package_id
      iv_option_id = io_option->ms_db_item-option_id
      iv_filename  = ls_file-filename
     IMPORTING
      es_oaor_last = ls_oaor_file
      et_oaor_file = lt_oaor_file ).

    " New or existeng item
    IF ls_oaor_file IS INITIAL.
      ls_oaor_file-description = ls_oaor_file-file_name = ls_file-filename.
      lv_oaor_mode = zcl_aqo_menu=>mc_oaor_new_file.
      MESSAGE s036(zaqo_message).

    ELSEIF sy-datum = ls_oaor_file-last_changed_at_date.
      lv_oaor_mode = zcl_aqo_menu=>mc_oaor_update_version.
      MESSAGE s038(zaqo_message) WITH ls_oaor_file-doc_ver_no.

    ELSE.
      lv_oaor_mode = zcl_aqo_menu=>mc_oaor_new_version.
      MESSAGE s037(zaqo_message) WITH ls_oaor_file-doc_ver_no.
    ENDIF.

    CALL FUNCTION 'ZFM_AQO_OAOR_FILE_1020'
      EXPORTING
        iv_package_id = io_option->ms_db_item-package_id
        iv_option_id  = io_option->ms_db_item-option_id
      IMPORTING
        ev_ok         = lv_ok
      CHANGING
        cs_info       = ls_oaor_file-f4.
    CHECK lv_ok = abap_true.

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
    CASE lv_oaor_mode.

        " First version
      WHEN zcl_aqo_menu=>mc_oaor_new_file.

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
             classname       = io_option->ms_db_item-package_id
             classtype       = zcl_aqo_helper=>mc_oaor_other
           CHANGING
             object_key      = lv_key
             signature       = lt_bds_signature
             files           = lt_file
           EXCEPTIONS
             OTHERS          = 7 ).
        IF sy-subrc = 0.
          " new version
          zcl_aqo_helper=>oaor_get_files(
           EXPORTING
            iv_pack_id   = io_option->ms_db_item-package_id
            iv_option_id = io_option->ms_db_item-option_id
            iv_filename  = ls_file-filename
           IMPORTING
            es_oaor_last = ls_oaor_file ).
        ENDIF.

**********************************************************************
        " Set new version
      WHEN zcl_aqo_menu=>mc_oaor_new_version.
        cl_bds_document_set=>create_version_with_files(
           EXPORTING
             classname       = io_option->ms_db_item-package_id
             classtype       = zcl_aqo_helper=>mc_oaor_other
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
        ENDIF.

**********************************************************************
        " Update existing
      WHEN zcl_aqo_menu=>mc_oaor_update_version.
        cl_bds_document_set=>update_with_files(
         EXPORTING
          classname       = io_option->ms_db_item-package_id
          classtype       = zcl_aqo_helper=>mc_oaor_other
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

      WHEN OTHERS.
        MESSAGE 'Please check OAOR mode' TYPE 'X'.
    ENDCASE.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Put new file
    zcl_aqo_helper=>check_in_request(
     EXPORTING
       is_oaor_file = ls_oaor_file
     CHANGING
       cv_task      = lv_task ).

    " Delete obselete data
    lv_last_index = lines( lt_oaor_file ) + 1.
    lv_last_index = lv_last_index - io_option->ms_db_item-prev_value_cnt.
    DO lv_last_index TIMES.
      READ TABLE lt_oaor_file INTO ls_oaor_file INDEX sy-index.
      CHECK sy-subrc = 0.

      zcl_aqo_helper=>oaor_delete_file(
       EXPORTING
        iv_pack_id   = io_option->ms_db_item-package_id
        iv_option_id = io_option->ms_db_item-option_id
        is_oaor_file = ls_oaor_file
       CHANGING
        cv_task      = lv_task ).
    ENDDO.
  ENDMETHOD.

  METHOD _attach_show.
    DATA:
      lt_oaor_file TYPE zcl_aqo_helper=>tt_oaor_file,
      ls_oaor_file TYPE REF TO zcl_aqo_helper=>ts_oaor_file,
      lt_return    TYPE STANDARD TABLE OF ddshretval WITH DEFAULT KEY,
      ls_return    TYPE REF TO ddshretval,
      ls_object_id TYPE sdokobject,
      lt_info      TYPE STANDARD TABLE OF sdokfilaci,
      ls_info      TYPE REF TO sdokfilaci,
      lt_text      TYPE STANDARD TABLE OF sdokcntasc,
      lt_bin       TYPE STANDARD TABLE OF sdokcntbin,
      lv_filetype  TYPE char10,
      lv_file_size TYPE i,
      lv_path      TYPE string,
      lv_sep       TYPE char1,
      lv_filename  TYPE string,
      lv_ext       TYPE string,
      lv_len       TYPE i,
      lv_index     TYPE i,
      lv_task      TYPE e070-trkorr.
    FIELD-SYMBOLS:
     <lt_table>   TYPE STANDARD TABLE.

    zcl_aqo_helper=>oaor_get_files(
     EXPORTING
       iv_pack_id   = io_option->ms_db_item-package_id
       iv_option_id = io_option->ms_db_item-option_id
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
      MESSAGE 'No user guide was found' TYPE 'S' DISPLAY LIKE 'W'.
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
      zcl_aqo_helper=>oaor_check_exists(
       EXPORTING
         iv_pack_id   = io_option->ms_db_item-package_id
         iv_option_id = io_option->ms_db_item-option_id
       IMPORTING
         ev_task      = lv_task ).

      IF lv_task IS NOT INITIAL.
        zcl_aqo_helper=>oaor_delete_file(
         EXPORTING
           iv_pack_id   = io_option->ms_db_item-package_id
           iv_option_id = io_option->ms_db_item-option_id
           is_oaor_file = ls_oaor_file->*
         CHANGING
           cv_task      = lv_task ).
      ENDIF.
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

    " No need to clean files (cl_gui_frontend_services=>file_delete). SAP gui cleans 'SAP GUI\tmp\' automatically
    cl_gui_frontend_services=>get_temp_directory( CHANGING temp_dir = lv_path EXCEPTIONS OTHERS = 1 ).
    CHECK sy-subrc = 0.

    " Add file separator
    cl_gui_frontend_services=>get_file_separator(
     CHANGING
       file_separator = lv_sep ).
    cl_gui_cfw=>flush( ).

    " Extract info & add
    zcl_aqo_helper=>split_file_path(
     EXPORTING
       iv_fullpath       = ls_oaor_file->file_name
     IMPORTING
       ev_filename_noext = lv_filename
       ev_extension      = lv_ext ).

    " Whole path
    lv_len = strlen( lv_path ) - 1.
    IF lv_path+lv_len(1) <> lv_sep.
      CONCATENATE lv_path lv_sep INTO lv_path.
    ENDIF.
    CONCATENATE lv_path lv_filename ` ` sy-datum `-` sy-uzeit `.` lv_ext INTO lv_path.

    " Download
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename      = lv_path
        filetype      = lv_filetype
        bin_filesize  = lv_file_size
      CHANGING
        data_tab      = <lt_table>
      EXCEPTIONS
        OTHERS        = 24 ).
    CHECK sy-subrc = 0.

    cl_gui_cfw=>flush( ).
    cl_gui_frontend_services=>execute(
     EXPORTING
      document               = lv_path
      operation              = 'OPEN'
     EXCEPTIONS
      OTHERS                 = 1 ).
    CHECK sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
  ENDMETHOD.

  METHOD _attach_delete.
    CHECK zcl_aqo_helper=>confirm(
        iv_title    = 'Confirmation'(cnf)
        iv_question = 'Deleting file is irreversible. Continue?'(def) ) = abap_true.

    " Delete in attachments
    _attach_show( io_option   = io_option
                  is_unq_menu = is_unq_menu
                  iv_delete   = abap_true ).
  ENDMETHOD.

ENDCLASS.
