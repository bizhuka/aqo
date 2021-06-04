FUNCTION zfm_aqo_usage_exit.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"--------------------------------------------------------------------
  DATA:
    lt_usage TYPE zcl_aqo_helper=>tt_usage.

  " Return all fields
  IF zcl_aqo_helper=>is_in_editor( ) = abap_true.
    " TODO When ?
    callcontrol-retallflds = abap_true.
  ENDIF.

  " Only for data selection
  CHECK callcontrol-step = 'SELECT'.

  " All usage
  lt_usage = zcl_aqo_helper=>get_usage( ).

  " Write back
  f4ut_results_map lt_usage.

  " Just display
  callcontrol-step = 'DISP'.
ENDFUNCTION.
