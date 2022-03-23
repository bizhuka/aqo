*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

TYPE-POOLS:
 abap,
 icon,
 cntb,
 cntl.

CONTROLS:
  tabs TYPE TABSTRIP.

CONSTANTS:
  BEGIN OF mc_pai_cmd,
    tab_edit_data      TYPE syucomm VALUE 'TABS_0071',
    tab_field_settings TYPE syucomm VALUE 'TABS_0072',
    tab_transport      TYPE syucomm VALUE 'TABS_0073',
    tab_usage_in_code  TYPE syucomm VALUE 'TABS_0074',
    tab_attachments    TYPE syucomm VALUE 'TABS_0075',
    tab_general_info   TYPE syucomm VALUE 'TABS_0076',

    open_option        TYPE syucomm VALUE 'OPEN_OPTION',
    new_option         TYPE syucomm VALUE 'NEW_OPTION',
    save               TYPE syucomm VALUE 'SAVE',
    exit               TYPE syucomm VALUE 'EXIT',
    show_user_prefs    TYPE syucomm VALUE 'SHOW_USER_PREFS',
  END OF mc_pai_cmd,

  BEGIN OF mc_event,
    open        TYPE string VALUE 'OPEN',
    close       TYPE string VALUE 'CLOSE',
    before_save TYPE string VALUE 'BEFORE_SAVE',
    after_save  TYPE string VALUE 'AFTER_SAVE',
  END OF mc_event.

DATA:
  BEGIN OF g_tabs,
    subscreen   TYPE sydynnr,
    prog        TYPE syrepid VALUE zcl_aqo_helper=>mc_prog-editor,
    pressed_tab TYPE syucomm VALUE mc_pai_cmd-tab_edit_data,
  END OF g_tabs,
  tabs_tab1 TYPE icons-text,
  ok_code   TYPE syucomm.

TYPES:
  ts_db_key  TYPE zcl_aqo_helper=>ts_db_key,
  ts_command TYPE zcl_aqo_helper=>ts_command.

CLASS lcl_tree          DEFINITION DEFERRED.
CLASS lcl_user_prefs    DEFINITION DEFERRED.
CLASS lcl_field_setting DEFINITION DEFERRED.
CLASS lcl_editor        DEFINITION DEFERRED.

DATA go_editor TYPE REF TO lcl_editor.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_editor DEFINITION FINAL
   INHERITING FROM zcl_aqo_option " Access to protected methods
   CREATE PUBLIC
   FRIENDS zcl_eui_event_caller
           lcl_field_setting.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_fld_value,
        icon         TYPE icon_d.
        INCLUDE      TYPE zcl_aqo_helper=>ts_field_value AS _field_value.
      TYPES:
        cur_value    TYPE REF TO data,
        catalog      TYPE icon_d,
        value_button TYPE icon_d,
        history_logs TYPE icon_d,
      END OF ts_fld_value.

    DATA:
      mo_option    TYPE REF TO zcl_aqo_option READ-ONLY,
      mo_screen    TYPE REF TO zcl_eui_screen READ-ONLY,
      mo_prefs     TYPE REF TO lcl_user_prefs READ-ONLY,
      mo_tree      TYPE REF TO lcl_tree       READ-ONLY,

      " Flags TODO usage
      mv_read_only TYPE abap_bool             READ-ONLY, " Highest priority
      mv_is_dev    TYPE abap_bool             READ-ONLY,

      mt_fld_value TYPE STANDARD TABLE OF ts_fld_value WITH DEFAULT KEY READ-ONLY,
      mt_f4_tables TYPE lvc_t_dral                                      READ-ONLY.

    METHODS:
      constructor,

      start_of_selection,

      pbo,

      pai
        CHANGING
          cv_ok_code TYPE syucomm,

      sync_screen_ui
        IMPORTING
          iv_message TYPE csequence,

      do_open
        IMPORTING
          is_db_key      TYPE ts_db_key
          iv_true_editor TYPE abap_bool DEFAULT abap_true
          iv_check_decl  TYPE abap_bool DEFAULT abap_true,

      show_all
        IMPORTING
                  iv_ok_as_save       TYPE abap_bool
        RETURNING VALUE(rv_close_cmd) TYPE syucomm,

      set_top_screen,

      make_screen
        IMPORTING
          iv_check_dev TYPE abap_bool,

      do_delete
        IMPORTING
          is_db_key TYPE ts_db_key,

      do_save
        IMPORTING
          iv_confirm TYPE abap_bool DEFAULT abap_true,

      add_new_field_screen
        EXPORTING
          er_data       TYPE REF TO data
          es_field_desc TYPE zcl_eui_type=>ts_field_desc,

      add_one_field
        IMPORTING
          is_field_value TYPE zcl_aqo_helper=>ts_field_value
          ir_data        TYPE REF TO data OPTIONAL,

      is_editable
        IMPORTING
          iv_editable        TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(rv_editable) TYPE abap_bool,

      set_icons
        IMPORTING
          is_field_desc TYPE zcl_eui_type=>ts_field_desc
        EXPORTING
          ev_icon       TYPE icon_d
          ev_catalog    TYPE icon_d,

      get_title
        IMPORTING
                  iv_add_opt_info TYPE abap_bool OPTIONAL
        RETURNING VALUE(rv_title) TYPE string,

      do_export,
      do_import
        IMPORTING
          is_db_key TYPE ts_db_key,

      skip_message
        IMPORTING
          io_msg_manager TYPE REF TO zcl_eui_msg_manager.

    EVENTS:
      app_event
       EXPORTING VALUE(iv_origin) TYPE string
                 VALUE(cv_ok)     TYPE REF TO abap_bool OPTIONAL.

  PRIVATE SECTION.
    DATA:
      mv_initial_hash TYPE char16.

    METHODS:
      _set_tab1_icon,
      _set_status,
      _set_titlebar,
      _make_tree,

      _set_flags,

      _fill_fields,

      _find_f4_tables,

      _on_pbo_menu_screen FOR EVENT pbo_event OF zif_eui_manager
        IMPORTING
          sender,
      _on_pai_menu_screen FOR EVENT pai_event OF zif_eui_manager
        IMPORTING
          iv_command,

      _is_saved
        RETURNING VALUE(rv_ok) TYPE abap_bool,

      _calculate_hash
        RETURNING VALUE(rv_hash) LIKE mv_initial_hash,

      _check_declaration
        IMPORTING
          is_db_key TYPE ts_db_key,

      _is_file_name_ok
        IMPORTING
          io_file TYPE REF TO zcl_eui_file
        RAISING
          zcx_eui_exception.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

DEFINE add_fcat_field.
  APPEND INITIAL LINE TO rt_catalog REFERENCE INTO lr_catalog.
  lr_catalog->fieldname = &1.

  IF &2 IS NOT INITIAL.
    lr_catalog->scrtext_s = lr_catalog->scrtext_m = lr_catalog->scrtext_l =
           lr_catalog->reptext = lr_catalog->coltext = &2.
  ENDIF.
END-OF-DEFINITION.
