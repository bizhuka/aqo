*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_opt DEFINITION INHERITING FROM zcl_aqo_option ABSTRACT FINAL FRIENDS
   lcl_fld_value_alv lcl_table_alv lcl_scr_free_sel lcl_table_comp_alv lcl_string_memo lcl_logs_alv.
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF mc_action,
        tech_view   TYPE string VALUE '_TECH_VIEW',
        edit_values TYPE string VALUE '_EDIT_VALUES',
        new_option  TYPE string VALUE '_NEW_OPTION',
      END OF mc_action.

    TYPES:
      BEGIN OF ts_fld_value,
        icon TYPE icon_d.
        INCLUDE TYPE zcl_aqo_helper=>ts_field_value.
    TYPES:
      cur_value    TYPE REF TO data,
      catalog      TYPE icon_d,
      value_button TYPE icon_d,
      history_logs TYPE icon_d,
      color_line   TYPE char4,
      END OF ts_fld_value.

    CLASS-DATA:
      mo_option    TYPE REF TO zcl_aqo_option,
      mt_fld_value TYPE STANDARD TABLE OF ts_fld_value WITH DEFAULT KEY,

      mv_read_only TYPE abap_bool, " Highest priority
      mv_is_dev    TYPE abap_bool,

      mo_menu      TYPE REF TO zcl_aqo_menu.

    CLASS-METHODS:
      add_one_field
        IMPORTING
          is_field_value TYPE zcl_aqo_helper=>ts_field_value
          ir_data        TYPE REF TO data OPTIONAL
        RAISING
          zcx_aqo_exception,

      set_icons
        IMPORTING
          iv_ui_type TYPE zcl_aqo_helper=>ts_field_value-ui_type
        EXPORTING
          ev_icon    TYPE icon_d
          ev_catalog TYPE icon_d,

      is_editable
        IMPORTING
          iv_editable        TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(rv_editable) TYPE abap_bool,

      do_save,

      initialization,

      start_of_selection
        IMPORTING
          iv_action TYPE string OPTIONAL,

      pbo,

      pai
        CHANGING
          cv_cmd TYPE syucomm,

      on_f4
        IMPORTING
          iv_code_scan TYPE abap_bool OPTIONAL,

      code_scan_f4.
ENDCLASS.                    "LCL_MAIN DEFINITION

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_nested_instance DEFINITION.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_instance,
        cl_name  TYPE string,
        level    TYPE i,
        instance TYPE REF TO lcl_nested_instance,
      END OF ts_instance.

    CLASS-DATA:
     mt_instance TYPE SORTED TABLE OF ts_instance WITH UNIQUE KEY cl_name level.

    " Current level
    DATA:
      mv_level    TYPE i,
      mv_last_cmd TYPE syucomm.

    CLASS-METHODS:
      get_instance_by_level
        IMPORTING
                  iv_cl_name         TYPE string
                  iv_level           TYPE i
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_nested_instance.
ENDCLASS.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_fld_value_alv DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-DATA:
      mo_instance TYPE REF TO lcl_fld_value_alv.

    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_fld_value_alv,

      get_exclude_toolbar
        IMPORTING
                  iv_editable          TYPE abap_bool
        RETURNING VALUE(rt_toolbar_ex) TYPE ui_functions,

      add_new_field
        EXPORTING
          er_data       TYPE REF TO data
          es_field_desc TYPE zcl_aqo_helper=>ts_field_desc.

    METHODS:
      call_screen,

      pbo,

      pai
        CHANGING
          cv_cmd TYPE syucomm,

      pbo_0920,
      pai_0920
        CHANGING
          cv_cmd TYPE syucomm,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
            e_object,

      on_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
            e_ucomm,

      on_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
            e_dyndoc_id,

      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
            e_column_id es_row_no,

      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING
            e_column e_row,

      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING sender er_data_changed,

      data_check
        RETURNING VALUE(rv_ok) TYPE abap_bool,

      sel_screen_show,

      get_text_value
        IMPORTING
          is_data        TYPE any
          iv_field       TYPE csequence OPTIONAL
        RETURNING
          VALUE(rv_text) TYPE rsdsselopt-low,

      edit_parameter
        IMPORTING
          is_fld_value TYPE REF TO lcl_opt=>ts_fld_value.

    DATA:
      mo_grid        TYPE REF TO cl_gui_alv_grid,
      mo_header_cont TYPE REF TO cl_gui_container.
ENDCLASS.                    "lcl_fld_value_alv DEFINITION

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_scr_free_sel DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      mo_instance TYPE REF TO lcl_scr_free_sel.

    CLASS-METHODS:
      get_instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_scr_free_sel.

    METHODS:
      pbo
        IMPORTING
          it_dsfldnum TYPE tt_rsdsfldnum,

      pai
        IMPORTING
          it_dsfldnum TYPE tt_rsdsfldnum
        CHANGING
          cv_cmd      TYPE syucomm.
ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_table_comp_alv DEFINITION INHERITING FROM lcl_nested_instance FINAL.
  PUBLIC SECTION.

    CLASS-METHODS:
      get_instance
        IMPORTING
                  VALUE(iv_level)    TYPE i OPTIONAL
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_table_comp_alv.

    TYPES:
      BEGIN OF ts_sub_fld_desc,
        icon TYPE icon_d.
        INCLUDE TYPE zcl_aqo_helper=>ts_field_desc AS field_desc.
    TYPES:
      catalog TYPE icon_d,
      END OF ts_sub_fld_desc.

    DATA:
      ms_field_desc   TYPE REF TO zcl_aqo_helper=>ts_field_desc,
      mt_sub_fld_desc TYPE STANDARD TABLE OF ts_sub_fld_desc WITH DEFAULT KEY,
      mv_editable     TYPE abap_bool,

      mv_refresh      TYPE abap_bool,
      mr_grid         TYPE REF TO cl_gui_alv_grid.

    METHODS:
      call_screen
        IMPORTING
          is_field_desc TYPE REF TO zcl_aqo_helper=>ts_field_desc
          iv_editable   TYPE abap_bool,

      get_field_catalog
        RETURNING VALUE(rt_fieldcat) TYPE lvc_t_fcat,

      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
            e_column_id es_row_no,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
            e_object,

      on_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
            e_ucomm,

      pbo,

      pai
        CHANGING
          cv_cmd TYPE syucomm.
ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_table_alv DEFINITION INHERITING FROM lcl_nested_instance FINAL.
  PUBLIC SECTION.

    CLASS-METHODS:
      get_instance
        IMPORTING
                  VALUE(iv_level)    TYPE i OPTIONAL
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_table_alv,

      refresh_sub_fields
        IMPORTING
          ir_table     TYPE REF TO data
          it_sub_field TYPE zcl_aqo_helper=>tt_field_desc OPTIONAL
          is_sub_field TYPE zcl_aqo_helper=>ts_field_desc OPTIONAL
          io_grid      TYPE REF TO cl_gui_alv_grid        OPTIONAL,

      show_range
        IMPORTING
                  is_fld_value     TYPE REF TO lcl_opt=>ts_fld_value
        RETURNING VALUE(rv_update) TYPE abap_bool.

    DATA:
      mr_table     TYPE REF TO data,
      ms_fld_value TYPE REF TO lcl_opt=>ts_fld_value,
      mv_refresh   TYPE abap_bool,
      mo_grid      TYPE REF TO cl_gui_alv_grid,
      mt_sub_field TYPE zcl_aqo_helper=>tt_field_desc.

    METHODS:
      call_screen
        IMPORTING
          is_fld_value TYPE REF TO lcl_opt=>ts_fld_value,

      pbo,

      pai
        CHANGING
          cv_cmd TYPE syucomm,

      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
            e_column_id es_row_no.
ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_string_memo DEFINITION INHERITING FROM lcl_nested_instance FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      mo_last_instance TYPE REF TO lcl_string_memo.

    CLASS-METHODS:
      get_instance
        IMPORTING
                  iv_level           TYPE i OPTIONAL
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_string_memo.

    DATA:
      mv_memo      TYPE string,
      mo_textedit  TYPE REF TO cl_gui_textedit,
      ms_fld_value TYPE REF TO lcl_opt=>ts_fld_value,
      mv_refresh   TYPE abap_bool.

    METHODS:
      call_screen
        IMPORTING
          is_fld_value TYPE REF TO lcl_opt=>ts_fld_value,

      pbo,

      pai
        CHANGING
          cv_cmd TYPE syucomm.
ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_logs_alv DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      mo_instance TYPE REF TO lcl_logs_alv.

    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_logs_alv.

    DATA:
      mr_hist_table TYPE REF TO data,
      ms_fld_value  TYPE REF TO lcl_opt=>ts_fld_value,
      mv_refresh    TYPE abap_bool,
      mo_grid       TYPE REF TO cl_gui_alv_grid.

    METHODS:
      call_screen
        IMPORTING
          is_fld_value TYPE REF TO lcl_opt=>ts_fld_value,

      pbo,

      pai
        CHANGING
          cv_cmd TYPE syucomm,

      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
            es_row_no.
ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_grid DEFINITION INHERITING FROM cl_gui_alv_grid FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      " Get control
      set_err_cells
        IMPORTING
          io_grid     TYPE REF TO cl_gui_alv_grid
          io_protocol TYPE REF TO cl_alv_changed_data_protocol.
ENDCLASS.
