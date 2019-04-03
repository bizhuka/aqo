FUNCTION zfm_package_exit.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------
  TYPES:
    BEGIN OF ts_item.
      INCLUDE TYPE zvaqo_package.
  TYPES:
    ctext TYPE tdevct-ctext,
    END OF ts_item.

  DATA:
    lt_item   TYPE STANDARD TABLE OF ts_item WITH DEFAULT KEY,
    ls_item   TYPE REF TO ts_item,
    lt_tdevct TYPE SORTED TABLE OF tdevct WITH UNIQUE KEY devclass,
    ls_tdevct TYPE REF TO tdevct.

  " Only for data selection
  CHECK callcontrol-step = 'SELECT'.

  " For simplicity
  CALL FUNCTION 'F4IF_SELECT_VALUES'
    EXPORTING
      shlp           = shlp
      call_shlp_exit = abap_false " No recursion
    TABLES
      record_tab     = record_tab.

  " Get all packages
  f4ut_parameter_value_get 'DEVCLASS' lt_item.
  CHECK lt_item IS NOT INITIAL.

  " Get texts
  SELECT devclass ctext INTO CORRESPONDING FIELDS OF TABLE lt_tdevct
  FROM  tdevct
  FOR ALL ENTRIES IN lt_item
  WHERE devclass = lt_item-devclass AND spras = sy-langu.
  CHECK lt_tdevct[] IS NOT INITIAL.

  LOOP AT lt_item REFERENCE INTO ls_item.
    " Get text
    READ TABLE lt_tdevct REFERENCE INTO ls_tdevct
     WITH TABLE KEY devclass = ls_item->devclass.
    CHECK sy-subrc = 0.

    ls_item->ctext = ls_tdevct->ctext.
  ENDLOOP.

  " Write back
  f4ut_parameter_results_put 'CTEXT' lt_item.
  callcontrol-step = 'DISP'.
ENDFUNCTION.
