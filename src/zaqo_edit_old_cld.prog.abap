*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_opt DEFINITION INHERITING FROM zcl_aqo FINAL FRIENDS
   lcl_fld_opt_alv lcl_table_alv lcl_scr_free_sel lcl_table_comp_alv lcl_string_memo lcl_where_used.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_fld_opt,
        icon TYPE icon_d.
        INCLUDE TYPE zcl_aqo_util=>ts_field_opt.
    TYPES:
      tab_rollname TYPE string, " Only for runtime
      value_button TYPE icon_d,
      color_line   TYPE char4,
      END OF ts_fld_opt.

    DATA:
      mt_fld_opt   TYPE STANDARD TABLE OF ts_fld_opt WITH DEFAULT KEY,
      mv_read_only TYPE abap_bool, " Highest priority
      mv_is_dev    TYPE abap_bool.

    METHODS:
      constructor
        EXCEPTIONS
          unknown_type,

      is_editable
        IMPORTING
          iv_editable        TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(rv_editable) TYPE abap_bool,

      save REDEFINITION.

    CLASS-METHODS:
      pbo,

      pai
        CHANGING
          cv_cmd TYPE syucomm,

      on_f4
        IMPORTING
          iv_field  TYPE dfies-fieldname
          iv_dynpro TYPE help_info-dynprofld.

ENDCLASS.                    "LCL_MAIN DEFINITION

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_fld_opt_alv DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-DATA:
      mo_instance TYPE REF TO lcl_fld_opt_alv.

    CLASS-METHODS:
      get_instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_fld_opt_alv.

    METHODS:
      call_screen,

      pbo,

      pai
        CHANGING
          cv_cmd TYPE syucomm,

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

      copy_2_client,

      find_ref.

    DATA:
      mo_grid        TYPE REF TO cl_gui_alv_grid,
      mo_header_cont TYPE REF TO cl_gui_container.
ENDCLASS.                    "lcl_fld_opt_alv DEFINITION

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
          it_seldyn   TYPE rsseldyn_tab                     "#EC NEEDED
          it_dsfldnum TYPE tt_rsdsfldnum,

      pai
        IMPORTING
          it_seldyn   TYPE rsseldyn_tab                     "#EC NEEDED
          it_dsfldnum TYPE tt_rsdsfldnum
        CHANGING
          cv_cmd      TYPE syucomm.
ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_table_comp_alv DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-DATA:
      mo_instance TYPE REF TO lcl_table_comp_alv.

    CLASS-METHODS:
      get_instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_table_comp_alv.

    DATA:
      ms_fld_opt    TYPE REF TO lcl_opt=>ts_fld_opt,
      mt_components TYPE zcl_aqo_util=>tt_comp,

      mv_refresh    TYPE abap_bool,
      mr_grid       TYPE REF TO cl_gui_alv_grid.

    METHODS:
      call_screen
        IMPORTING
          is_fld_opt TYPE REF TO lcl_opt=>ts_fld_opt,

      pbo,

      pai
        CHANGING
          cv_cmd TYPE syucomm.
ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_table_alv DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      mo_instance TYPE REF TO lcl_table_alv.

    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_table_alv.

    DATA:
      mr_table   TYPE REF TO data,
      ms_fld_opt TYPE REF TO lcl_opt=>ts_fld_opt,
      mv_refresh TYPE abap_bool,
      mo_grid    TYPE REF TO cl_gui_alv_grid.

    METHODS:
      call_screen
        IMPORTING
          is_fld_opt TYPE REF TO lcl_opt=>ts_fld_opt,

      pbo,

      pai
        CHANGING
          cv_cmd TYPE syucomm.
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

    DATA:
      mv_memo     TYPE string,
      mo_textedit TYPE REF TO cl_gui_textedit,
      ms_fld_opt  TYPE REF TO lcl_opt=>ts_fld_opt,
      mv_refresh  TYPE abap_bool.

    METHODS:
      call_screen
        IMPORTING
          is_fld_opt TYPE REF TO lcl_opt=>ts_fld_opt,

      pbo,

      pai
        CHANGING
          cv_cmd TYPE syucomm.
ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_where_used DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      mo_instance TYPE REF TO lcl_where_used.

    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_where_used.

    METHODS:
      call_screen,

      pbo,

      pai
        CHANGING
          cv_cmd TYPE syucomm,

      deep_scan.
ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_grid DEFINITION INHERITING FROM cl_gui_alv_grid.
  PUBLIC SECTION.
    CLASS-METHODS:

      " Get control
      set_err_cells
        IMPORTING
          io_grid          TYPE REF TO cl_gui_alv_grid
          io_protocol      TYPE REF TO cl_alv_changed_data_protocol.
ENDCLASS.
