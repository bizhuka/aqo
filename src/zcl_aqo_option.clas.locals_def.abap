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

CLASS zcl_aqo_option DEFINITION LOCAL FRIENDS lcl_helper.
