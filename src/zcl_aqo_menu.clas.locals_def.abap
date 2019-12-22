*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_aqo_option DEFINITION INHERITING FROM zcl_aqo_option ABSTRACT FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_unq_menu,
        unq_id     TYPE string,
        package_id TYPE ztaqo_option-package_id,
        option_id  TYPE ztaqo_option-option_id,
        " What transaction to launch
        tcode      TYPE sytcode,

        menu       TYPE REF TO zcl_aqo_menu,
        option     TYPE REF TO zcl_aqo_option,
      END OF ts_unq_menu,
      tt_unq_menu  TYPE SORTED TABLE OF ts_unq_menu WITH UNIQUE KEY unq_id.

    CLASS-DATA:
      mt_unq_menu TYPE tt_unq_menu.

    CONSTANTS:
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
        save_in        TYPE ui_func VALUE '_SAVE_IN',
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
      END OF mc_action.

    CLASS-METHODS:
      get_menu
        IMPORTING
                  iv_package_id  TYPE csequence
                  iv_option_id   TYPE csequence
        RETURNING VALUE(ro_menu) TYPE REF TO zcl_aqo_menu,

      get_buttons
        IMPORTING
                  is_unq_menu    TYPE REF TO ts_unq_menu
                  iv_in_editor   TYPE abap_bool
        RETURNING VALUE(rt_menu) TYPE zcl_aqo_menu=>tt_menu,

      read_locks
        IMPORTING
                  is_unq_menu           TYPE REF TO ts_unq_menu
        RETURNING VALUE(rv_locked_text) TYPE stb_button-quickinfo,

      do_update
        IMPORTING
                  io_option      TYPE REF TO zcl_aqo_option
                  iv_set         TYPE string
                  iv_fields      TYPE ztaqo_option-fields         OPTIONAL
                  iv_description TYPE ztaqo_option-description    OPTIONAL
                  iv_prev_count  TYPE ztaqo_option-prev_value_cnt OPTIONAL
                  iv_menu_mode   TYPE ztaqo_option-menu_mode      OPTIONAL
        RETURNING VALUE(rv_ok)   TYPE abap_bool,

      _export
        IMPORTING
                  io_option        TYPE REF TO zcl_aqo_option
                  is_unq_menu      TYPE REF TO ts_unq_menu
        RETURNING VALUE(rv_update) TYPE abap_bool,

      _import
        IMPORTING
                  io_option        TYPE REF TO zcl_aqo_option
                  is_unq_menu      TYPE REF TO ts_unq_menu
        RETURNING VALUE(rv_update) TYPE abap_bool,

      _last_code
        IMPORTING
                  io_option        TYPE REF TO zcl_aqo_option
                  is_unq_menu      TYPE REF TO ts_unq_menu
        RETURNING VALUE(rv_update) TYPE abap_bool,

      _view
        IMPORTING
                  io_option        TYPE REF TO zcl_aqo_option
                  is_unq_menu      TYPE REF TO ts_unq_menu
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _change
        IMPORTING
                  io_option        TYPE REF TO zcl_aqo_option
                  is_unq_menu      TYPE REF TO ts_unq_menu
                  iv_command       TYPE syucomm OPTIONAL
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _delete
        IMPORTING
                  io_option        TYPE REF TO zcl_aqo_option
                  is_unq_menu      TYPE REF TO ts_unq_menu
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _new
        IMPORTING
                  io_option        TYPE REF TO zcl_aqo_option
                  is_unq_menu      TYPE REF TO ts_unq_menu
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _save_in
        IMPORTING
                  io_option        TYPE REF TO zcl_aqo_option
                  is_unq_menu      TYPE REF TO ts_unq_menu
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _about
        IMPORTING
                  io_option        TYPE REF TO zcl_aqo_option
                  is_unq_menu      TYPE REF TO ts_unq_menu
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _attach_import
        IMPORTING
                  io_option        TYPE REF TO zcl_aqo_option
                  is_unq_menu      TYPE REF TO ts_unq_menu
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _attach_show
        IMPORTING
                  io_option        TYPE REF TO zcl_aqo_option
                  is_unq_menu      TYPE REF TO ts_unq_menu
                  iv_vis_only      TYPE abap_bool OPTIONAL
                  iv_delete        TYPE abap_bool OPTIONAL
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception,

      _attach_delete
        IMPORTING
                  io_option        TYPE REF TO zcl_aqo_option
                  is_unq_menu      TYPE REF TO ts_unq_menu
        RETURNING VALUE(rv_update) TYPE abap_bool
        RAISING   zcx_aqo_exception.
ENDCLASS.
