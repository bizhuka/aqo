
TABLES:
  sscrfields.

TYPE-POOLS:
  abap,
  icon,
  sscr,
  cndp.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_opt                  DEFINITION DEFERRED.
CLASS lcl_fld_value_alv        DEFINITION DEFERRED.
CLASS lcl_scr_free_sel         DEFINITION DEFERRED.
CLASS lcl_table_comp_alv       DEFINITION DEFERRED.
CLASS lcl_table_alv            DEFINITION DEFERRED.
CLASS lcl_string_memo          DEFINITION DEFERRED.
CLASS lcl_logs_alv             DEFINITION DEFERRED.


DATA:
  " Cannot use static =>mo_instance in old versions
  go_fld_value_alv TYPE REF TO lcl_fld_value_alv,
  go_string_memo   TYPE REF TO lcl_string_memo,
  go_table_alv     TYPE REF TO lcl_table_alv,
  go_logs_alv      TYPE REF TO lcl_logs_alv.

TYPES:
  tt_rsdsfldnum TYPE STANDARD TABLE OF rsdsfldnum.

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
        icon         TYPE icon_d.
        INCLUDE TYPE zcl_aqo_helper=>ts_field_value.
      TYPES:
        cur_value    TYPE REF TO data,
        catalog      TYPE icon_d,
        value_button TYPE icon_d,
        history_logs TYPE icon_d,
      END OF ts_fld_value.

    CLASS-DATA:
      mo_option    TYPE REF TO zcl_aqo_option,
      mt_fld_value TYPE STANDARD TABLE OF ts_fld_value WITH DEFAULT KEY,

      mv_read_only TYPE abap_bool, " Highest priority
      mv_is_dev    TYPE abap_bool,
      mo_eui_menu  TYPE REF TO zcl_eui_menu,
      mt_f4_tables TYPE lvc_t_dral.

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

      fill_fields,

      find_f4_tables,

      launch_action
        IMPORTING
          iv_action TYPE string,

      pbo,

      pai
        CHANGING
          cv_cmd TYPE syucomm,

      on_f4
        IMPORTING
          iv_code_scan TYPE abap_bool OPTIONAL,

      code_scan_f4,

      set_menu_visible
        IMPORTING
          iv_visible TYPE abap_bool.
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
    DATA mv_level TYPE i.

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

    DATA:
      mv_editable TYPE abap_bool.

    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_fld_value_alv.

    METHODS:
      call_screen,

      get_title
        RETURNING VALUE(rv_title) TYPE string,

      set_exclude_toolbar
        IMPORTING
          iv_editable TYPE abap_bool
        CHANGING
          ct_toolbar  TYPE ttb_button,

      add_new_field
        EXPORTING
          er_data       TYPE REF TO data
          es_field_desc TYPE zcl_eui_type=>ts_field_desc,

      on_pai_event FOR EVENT pai_event OF zif_eui_manager   "#EC CALLED
        IMPORTING
          sender
          iv_command,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid       "#EC CALLED
        IMPORTING
          e_object,

      on_user_command FOR EVENT user_command OF cl_gui_alv_grid "#EC CALLED
        IMPORTING
          sender
          e_ucomm,

      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid "#EC CALLED
        IMPORTING
          e_column_id es_row_no,

      on_double_click FOR EVENT double_click OF cl_gui_alv_grid "#EC CALLED
        IMPORTING
          e_column e_row,

      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid "#EC CALLED
        IMPORTING sender er_data_changed,

      data_check
        IMPORTING
                  io_grid      TYPE REF TO cl_gui_alv_grid OPTIONAL
        RETURNING VALUE(rv_ok) TYPE abap_bool,

      sel_screen_show.
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
          it_dsfldnum TYPE tt_rsdsfldnum, "#EC NEEDED

      pai
        IMPORTING
          it_dsfldnum TYPE tt_rsdsfldnum  "#EC NEEDED
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
        icon    TYPE icon_d.
        INCLUDE TYPE zcl_eui_type=>ts_field_desc AS field_desc.
      TYPES:
        catalog TYPE icon_d,
      END OF ts_sub_fld_desc,
      tt_sub_fld_desc TYPE STANDARD TABLE OF ts_sub_fld_desc WITH DEFAULT KEY.

    DATA:
      ms_field_desc   TYPE REF TO zcl_eui_type=>ts_field_desc,
      mt_sub_fld_desc TYPE tt_sub_fld_desc,
      mv_editable     TYPE abap_bool.

    METHODS:
      call_screen
        IMPORTING
          is_field_desc TYPE REF TO zcl_eui_type=>ts_field_desc
          iv_editable   TYPE abap_bool,

      change_key,

      on_pbo_event FOR EVENT pbo_event OF zif_eui_manager   "#EC CALLED
        IMPORTING
          sender,

      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid "#EC CALLED
        IMPORTING
          e_column_id es_row_no,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid       "#EC CALLED
        IMPORTING
          e_object,

      on_user_command FOR EVENT user_command OF cl_gui_alv_grid "#EC CALLED
        IMPORTING
          sender
          e_ucomm,

      on_pai_event FOR EVENT pai_event OF zif_eui_manager   "#EC CALLED
        IMPORTING
          iv_command.
ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_table_alv DEFINITION INHERITING FROM lcl_nested_instance FINAL.
  PUBLIC SECTION.

    CLASS-METHODS:
      get_instance
        IMPORTING
                  VALUE(iv_level)    TYPE i OPTIONAL
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_table_alv.

    DATA:
      ms_fld_value TYPE REF TO lcl_opt=>ts_fld_value.

    METHODS:
      call_screen
        IMPORTING
          is_fld_value TYPE REF TO lcl_opt=>ts_fld_value,

      _get_f4_catalog
        EXPORTING
          et_catalog  TYPE lvc_t_fcat
          et_f4_table TYPE zcl_eui_alv=>tt_f4_table.
ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_string_memo DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-DATA:
      mo_instance TYPE REF TO lcl_string_memo.

    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_string_memo.

    METHODS:
      call_screen
        IMPORTING
                  is_fld_value        TYPE REF TO lcl_opt=>ts_fld_value
        RETURNING VALUE(rv_close_cmd) TYPE syucomm.
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
      ms_fld_value  TYPE REF TO lcl_opt=>ts_fld_value.

    METHODS:
      call_screen
        IMPORTING
          is_fld_value TYPE REF TO lcl_opt=>ts_fld_value,

      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid "#EC CALLED
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

DEFINE add_fcat_field.
  APPEND INITIAL LINE TO lt_fieldcat REFERENCE INTO ls_fieldcat.
  ls_fieldcat->fieldname = &1.

  IF &2 IS NOT INITIAL.
    ls_fieldcat->scrtext_s = ls_fieldcat->scrtext_m = ls_fieldcat->scrtext_l =
           ls_fieldcat->reptext = ls_fieldcat->coltext = &2.
  ENDIF.
END-OF-DEFINITION.
