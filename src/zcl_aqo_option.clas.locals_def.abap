*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_helper DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      compare_2_fields
        IMPORTING
          is_new     TYPE zcl_eui_type=>ts_field_desc
          iv_repair  TYPE abap_bool
          cs_old     TYPE REF TO zcl_eui_type=>ts_field_desc
        CHANGING
          cv_changed TYPE abap_bool
        RAISING
          zcx_aqo_exception,

      check_abap_declaration
        IMPORTING
          io_option      TYPE REF TO zcl_aqo_option
          ir_data        TYPE REF TO data
          io_data        TYPE REF TO object
          iv_repair      TYPE abap_bool
        CHANGING
          ct_field_value LIKE io_option->mt_field_value
          cv_changed     TYPE abap_bool
        RAISING
          zcx_aqo_exception .
ENDCLASS.

CLASS lcl_unq_menu DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_unq_menu,
        unq_id   TYPE string,
        unq_menu TYPE REF TO lcl_unq_menu,
      END OF ts_unq_menu,
      tt_unq_menu TYPE SORTED TABLE OF ts_unq_menu WITH UNIQUE KEY unq_id,

      BEGIN OF ts_visible,
        pack    TYPE ztaqo_option-package_id,
        opt     TYPE ztaqo_option-option_id,
        visible TYPE abap_bool,
      END OF ts_visible,
      tt_visible TYPE SORTED TABLE OF ts_visible WITH UNIQUE KEY pack opt.

    CLASS-DATA:
     mt_visible TYPE tt_visible.
    CLASS-DATA:
      mt_unq_menu TYPE tt_unq_menu.

    CLASS-METHODS:
      get_eui_menu
        IMPORTING
                  iv_package_id      TYPE csequence
                  iv_option_id       TYPE csequence
        RETURNING VALUE(ro_eui_menu) TYPE REF TO zcl_eui_menu.

    DATA:
      package_id TYPE ztaqo_option-package_id,
      option_id  TYPE ztaqo_option-option_id,
      option     TYPE REF TO zcl_aqo_option,

      " What transaction to launch
      tcode      TYPE sytcode,

      eui_menu   TYPE REF TO zcl_eui_menu.

    CONSTANTS:
      BEGIN OF mc_prog,
        editor       TYPE syrepid VALUE 'ZAQO_EDITOR_OLD',
        editor_tcode TYPE sytcode VALUE 'ZAQO_EDITOR_OLD',
        viewer_tcode TYPE sytcode VALUE 'ZAQO_VIEWER_OLD',
      END OF mc_prog,

      BEGIN OF mc_menu_mode,
        view TYPE zdaqo_menu_mode VALUE 0,
        edit TYPE zdaqo_menu_mode VALUE 1,
        hide TYPE zdaqo_menu_mode VALUE 2,
      END OF mc_menu_mode,

      BEGIN OF mc_action,
        base           TYPE ui_func VALUE '_BASE',

        new            TYPE ui_func VALUE '_NEW',

        change         TYPE ui_func VALUE '_CHANGE',
        view           TYPE ui_func VALUE '_VIEW',

        transport_root TYPE ui_func VALUE '_TRANSPORT_ROOT',
*        save_in        TYPE ui_func VALUE '_SAVE_IN',
        export         TYPE ui_func VALUE '_EXPORT',
        import         TYPE ui_func VALUE '_IMPORT',
        delete         TYPE ui_func VALUE '_DELETE',

        last_code      TYPE ui_func VALUE '_LAST_CODE',

        about_sep      TYPE ui_func VALUE '_ABOUT_SEP',
        " -------
        attach_root    TYPE ui_func VALUE '_ATTACH_ROOT',
        attach_import  TYPE ui_func VALUE '_ATTACH_IMPORT',
        attach_show    TYPE ui_func VALUE '_ATTACH_SHOW',
        attach_delete  TYPE ui_func VALUE '_ATTACH_DELETE',
        " -------
        about          TYPE ui_func VALUE '_ABOUT',
      END OF mc_action,

      BEGIN OF mc_oaor,
        new_file       TYPE string VALUE 'NEW_FILE',
        new_version    TYPE string VALUE 'NEW_VERSION',
        update_version TYPE string VALUE 'UPDATE_VERSION',
      END OF mc_oaor.

    METHODS:
      get_buttons
        IMPORTING
                  iv_in_editor   TYPE abap_bool
        RETURNING VALUE(rt_menu) TYPE zcl_eui_menu=>tt_menu,

      read_locks
        RETURNING VALUE(rv_locked_text) TYPE stb_button-quickinfo,

      on_function_selected FOR EVENT function_selected OF cl_gui_toolbar
        IMPORTING fcode,

      do_update
        IMPORTING
                  iv_set         TYPE string
                  iv_fields      TYPE ztaqo_option-fields         OPTIONAL
                  iv_description TYPE ztaqo_option-description    OPTIONAL
                  iv_prev_count  TYPE ztaqo_option-prev_value_cnt OPTIONAL
                  iv_menu_mode   TYPE ztaqo_option-menu_mode      OPTIONAL
        RETURNING VALUE(rv_ok)   TYPE abap_bool,

      has_visible_files
        IMPORTING
                  iv_pack           TYPE ztaqo_option-package_id
                  iv_opt            TYPE ztaqo_option-option_id
        RETURNING VALUE(rv_visible) TYPE abap_bool,

      on_about_pai FOR EVENT pai_event OF zif_eui_manager
        IMPORTING
            sender
            iv_command
            cv_close,

      show_new_version_diloag
        EXPORTING
          ev_ok        TYPE abap_bool
        CHANGING
          cs_oaor_file TYPE zcl_aqo_helper=>ts_oaor_file,

      _export
        RETURNING VALUE(rv_update) TYPE abap_bool,

      _import
        RETURNING VALUE(rv_update) TYPE abap_bool,

      _last_code
        RETURNING VALUE(rv_update) TYPE abap_bool,

      _view
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _change
        IMPORTING
                  iv_command       TYPE syucomm OPTIONAL
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _delete
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _new
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _about
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _attach_import
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _attach_show
        IMPORTING
                  iv_vis_only      TYPE abap_bool OPTIONAL
                  iv_delete        TYPE abap_bool OPTIONAL
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _attach_delete
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception.
ENDCLASS.

CLASS zcl_aqo_option DEFINITION LOCAL FRIENDS lcl_unq_menu.
CLASS zcl_aqo_option DEFINITION LOCAL FRIENDS lcl_helper.
