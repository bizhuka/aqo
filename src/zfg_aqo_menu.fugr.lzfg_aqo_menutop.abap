FUNCTION-POOL zfg_aqo_menu.                 "MESSAGE-ID ..

*INCLUDE lzfg_aqo_menu...                  " Local class definition

DATA:
  gv_ok_code TYPE syucomm.

**********************************************************************
* For OAOR F4
**********************************************************************

FORM callback_oaor_f4
                 TABLES   record_tab  STRUCTURE seahlpres
                 CHANGING shlp        TYPE      shlp_descr
                          callcontrol LIKE      ddshf4ctrl.
  DATA:
    ls_selopt TYPE ddshselopt.

  " Set the restriction for ZSAQO_OAOR_FILE_F4-LAST_VERSION field
  ls_selopt-shlpfield = 'LAST_VERSION'.
  ls_selopt-sign      = 'I'.
  ls_selopt-option    = 'EQ'.
  ls_selopt-low       = 'X'.
  APPEND ls_selopt TO shlp-selopt.
ENDFORM.

**********************************************************************
* SH exits
**********************************************************************
  DEFINE f4ut_parameter_value_get.
    CALL FUNCTION 'F4UT_PARAMETER_VALUE_GET'
      EXPORTING
        parameter   = &1
        fieldname   = &1
      TABLES
        shlp_tab    = shlp_tab
        record_tab  = record_tab
        results_tab = &2
      CHANGING
        shlp        = shlp
        callcontrol = callcontrol.
  END-OF-DEFINITION.

  DEFINE f4ut_parameter_results_put.
    CALL FUNCTION 'F4UT_PARAMETER_RESULTS_PUT'
      EXPORTING
        parameter   = &1
        fieldname   = &1
      TABLES
        shlp_tab    = shlp_tab
        record_tab  = record_tab
        source_tab  = &2
      CHANGING
        shlp        = shlp
        callcontrol = callcontrol.
  END-OF-DEFINITION.

  DEFINE f4ut_results_map.
    CALL FUNCTION 'F4UT_RESULTS_MAP'
*    EXPORTING
*      source_structure   = &1
*      apply_restrictions = 'X'
      TABLES
        shlp_tab           = shlp_tab
        record_tab         = record_tab
        source_tab         = &1
      CHANGING
        shlp               = shlp
        callcontrol        = callcontrol.
  END-OF-DEFINITION.
