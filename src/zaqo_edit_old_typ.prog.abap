*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_opt              DEFINITION DEFERRED.
CLASS lcl_fld_opt_alv      DEFINITION DEFERRED.
CLASS lcl_scr_free_sel     DEFINITION DEFERRED.
CLASS lcl_table_comp_alv   DEFINITION DEFERRED.
CLASS lcl_table_alv        DEFINITION DEFERRED.
CLASS lcl_string_memo      DEFINITION DEFERRED.
CLASS lcl_where_used       DEFINITION DEFERRED.

TABLES:
  sscrfields.

TYPE-POOLS:
  icon.

DATA:
  " OK_CODE 4 screens
  gv_ok_code TYPE syucomm,
  go_opt     TYPE REF TO lcl_opt.

TYPES:
  tt_rsdsfldnum TYPE STANDARD TABLE OF rsdsfldnum.
