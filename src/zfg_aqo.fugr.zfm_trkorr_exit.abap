FUNCTION zfm_trkorr_exit.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  " Only for data selection
  CHECK callcontrol-step = 'SELECT'.
  DATA:
   ls_selopt TYPE REF TO ddshselopt.

  DEFINE add_default.
    READ TABLE shlp-selopt REFERENCE INTO ls_selopt
     WITH KEY shlpfield = &1.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO shlp-selopt REFERENCE INTO ls_selopt.
      ls_selopt->shlpname  = 'E070'.
      ls_selopt->shlpfield = &1.
      ls_selopt->sign      = &2.
      ls_selopt->option    = 'EQ'.
      ls_selopt->low       = &3.
    ENDIF.
  END-OF-DEFINITION.

  " Filters as in SH declaration
  add_default 'TRSTATUS'   'I' 'D'.
  add_default 'TRFUNCTION' 'E' 'K'.
  add_default 'AS4USER'    'I' sy-uname.

  " For simplicity (+ texts from E07T)
  CALL FUNCTION 'F4IF_SELECT_VALUES'
    EXPORTING
      shlp           = shlp
      call_shlp_exit = abap_false " No recursion
    TABLES
      record_tab     = record_tab.

  " Just display
  callcontrol-step = 'DISP'.
ENDFUNCTION.
