*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_helper DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      create
        IMPORTING
          iv_is_class  TYPE abap_bool
        EXPORTING
          es_last_call TYPE abap_callstack_line
          eo_opt       TYPE REF TO zcl_aqo_option
        CHANGING
          cs_db_key    TYPE zcl_aqo_helper=>ts_db_key,

      check_package_exist
        IMPORTING
          iv_package_id TYPE tadir-devclass,

      get_class_fields
        IMPORTING io_data                  TYPE REF TO object
        RETURNING VALUE(rt_declared_field) TYPE zcl_aqo_helper=>abap_attrname_tab,

      get_struc_fields
        IMPORTING ir_data                  TYPE REF TO data
        RETURNING VALUE(rt_declared_field) TYPE zcl_aqo_helper=>abap_attrname_tab,

      get_default_package
        IMPORTING
                  is_stack          TYPE abap_callstack_line
                  iv_is_class       TYPE abap_bool
        RETURNING VALUE(rv_package) TYPE tadir-devclass.

**********************************************************************
**********************************************************************
    TYPES:
      BEGIN OF ts_badi_cache,
        class_name TYPE zsaqo_f4-badi_class_impl,
        class_ref  TYPE REF TO data,
      END OF ts_badi_cache.

    CLASS-DATA:
      mt_badi_cache TYPE HASHED TABLE OF ts_badi_cache WITH UNIQUE KEY class_name.
    DATA:
      mt_badi_class_impl TYPE HASHED TABLE OF zsaqo_f4-badi_class_impl WITH UNIQUE KEY table_line.
    METHODS:
      add_badi_class IMPORTING is_line TYPE any
                     CHANGING  ct_badi TYPE STANDARD TABLE.
ENDCLASS.

CLASS zcl_aqo_option DEFINITION LOCAL FRIENDS lcl_helper.
