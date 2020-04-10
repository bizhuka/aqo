FUNCTION zfm_aqo_option_exit.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"--------------------------------------------------------------------

  TYPES:
    BEGIN OF ts_item,
      option_size    TYPE zdaqo_option_size,
      call_blockname TYPE zdaqo_call_blockname.
      INCLUDE TYPE ztaqo_option.
  TYPES:
    END OF ts_item.

  DATA:
    lt_item       TYPE STANDARD TABLE OF ts_item,
    ls_item       TYPE REF TO ts_item,
    ls_x030l      TYPE x030l,
    lv_escape     TYPE abap_bool,
    lv_where      TYPE string,
    lt_fielddescr TYPE ddfields,
    lv_order_by   TYPE text255.


  " Return all fields
  IF zcl_aqo_helper=>is_in_editor( ) = abap_true.
    " TODO When ?
    callcontrol-retallflds = abap_true.
  ENDIF.

  " Only for data selection
  CHECK callcontrol-step = 'SELECT'.

  " Use copy
  lt_fielddescr = shlp-fielddescr.
  CALL FUNCTION 'DDIF_NAMETAB_GET'
    EXPORTING
      tabname   = shlp-intdescr-selmethod
    IMPORTING
      x030l_wa  = ls_x030l
    TABLES
      dfies_tab = lt_fielddescr " shlp-fielddescr[]
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc = 0.
    PERFORM check_escape_allowed IN PROGRAM saplsdh3
     USING ls_x030l CHANGING lv_escape.
  ENDIF.

  " Get condition
  CALL FUNCTION 'F4_CONV_SELOPT_TO_WHERECLAUSE'
    EXPORTING
      escape_allowed = lv_escape
    IMPORTING
      where_clause   = lv_where
    TABLES
      selopt_tab     = shlp-selopt.

  " Search as OR
  IF zcl_aqo_helper=>is_in_editor( iv_is_sapui5 = abap_true ) = abap_true.
    REPLACE ALL OCCURRENCES OF ` AND ` IN lv_where WITH ` OR `.
  ENDIF.

  " Main select
  zcl_aqo_helper=>sh_sort_order(
   IMPORTING
     ev_value = lv_order_by ).

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_item
  FROM (shlp-intdescr-selmethod)
  WHERE (lv_where)
  ORDER BY (lv_order_by).

  " Size in Kb (Use another field)
  LOOP AT lt_item REFERENCE INTO ls_item.
    ls_item->call_blockname = ls_item->blockname.
    ls_item->option_size = xstrlen( ls_item->fields ) / 1024.

    " Class or program name
    zcl_aqo_helper=>get_last_call_info(
     EXPORTING
       is_last_call = ls_item->last_call
     IMPORTING
       ev_name      = ls_item->mainprogram ).
  ENDLOOP.

  " Write back
  f4ut_results_map lt_item.

  " Just display
  callcontrol-step = 'DISP'.
ENDFUNCTION.
