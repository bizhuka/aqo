*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zaqo_badi.


CLASS lcl_tester DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_filter,
        bukrs TYPE bukrs,
      END OF ts_filter.

    METHODS:
      start_of_selection.
ENDCLASS.


CLASS lcl_tester IMPLEMENTATION.
  METHOD start_of_selection.
    DATA:
      ls_filter TYPE ts_filter,
      lt_badi   TYPE STANDARD TABLE OF REF TO zif_aqo_badi_test,
      lo_badi   TYPE REF TO zif_aqo_badi_test.

    ls_filter-bukrs = '0001'.

    zcl_aqo_option=>find_badi(
      EXPORTING
        iv_interface = 'ZIF_AQO_BADI_TEST'
        is_filter    = ls_filter
      IMPORTING
        et_badi      = lt_badi ).

    " Call all implementations found by filter
    LOOP AT lt_badi INTO lo_badi.
      lo_badi->some_method( ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

DATA:
 go_tester TYPE REF TO lcl_tester.

INITIALIZATION.
  CREATE OBJECT go_tester.

START-OF-SELECTION.
  go_tester->start_of_selection( ).
