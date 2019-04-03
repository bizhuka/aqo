FUNCTION-POOL zfg_aqo.                      "MESSAGE-ID ..

* INCLUDE LZFG_AQOD...                       " Local class definition

TYPE-POOLS:
 abap.

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
