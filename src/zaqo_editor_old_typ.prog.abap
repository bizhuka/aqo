*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_opt              DEFINITION DEFERRED.
CLASS lcl_fld_value_alv    DEFINITION DEFERRED.
CLASS lcl_scr_free_sel     DEFINITION DEFERRED.
CLASS lcl_table_comp_alv   DEFINITION DEFERRED.
CLASS lcl_table_alv        DEFINITION DEFERRED.
CLASS lcl_string_memo      DEFINITION DEFERRED.
CLASS lcl_where_used       DEFINITION DEFERRED.

TABLES:
  sscrfields.

TYPE-POOLS:
  abap,
  icon,
  sscr,
  cndp.

DATA:
  " OK_CODE 4 screens
  gv_ok_code        TYPE syucomm,

  " TODO static ?
  go_table_comp_alv TYPE REF TO lcl_table_comp_alv,
  go_fld_value_alv  TYPE REF TO lcl_fld_value_alv,
  go_string_memo    TYPE REF TO lcl_string_memo,
  go_table_alv      TYPE REF TO lcl_table_alv,
  go_where_used     TYPE REF TO lcl_where_used.

TYPES:
  tt_rsdsfldnum TYPE STANDARD TABLE OF rsdsfldnum.
