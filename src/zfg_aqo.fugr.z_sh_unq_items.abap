FUNCTION Z_SH_UNQ_ITEMS.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"--------------------------------------------------------------------

  CHECK callcontrol-step = 'DISP'.

  SORT record_tab.
  DELETE ADJACENT DUPLICATES FROM record_tab COMPARING ALL FIELDS.

*  CALL FUNCTION 'F4UT_PARAMETER_VALUE_GET'
*  CALL FUNCTION 'F4UT_PARAMETER_RESULTS_PUT'
ENDFUNCTION.
