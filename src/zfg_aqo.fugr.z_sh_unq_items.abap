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
DATA:
    lt_disp TYPE STANDARD TABLE OF ztaqo_data.
  DEFINE read_field.
    CALL FUNCTION 'F4UT_PARAMETER_VALUE_GET'
      EXPORTING
        parameter         = &1
        fieldname         = &1
      TABLES
        shlp_tab          = shlp_tab
        record_tab        = record_tab
        results_tab       = lt_disp
      CHANGING
        shlp              = shlp
        callcontrol       = callcontrol
      EXCEPTIONS
        parameter_unknown = 1
        OTHERS            = 2.
    CHECK sy-subrc = 0.
  END-OF-DEFINITION.

  " Only for display
  CHECK callcontrol-step = 'DISP'.

  SORT record_tab.
  DELETE ADJACENT DUPLICATES FROM record_tab COMPARING ALL FIELDS.

  read_field 'OBJECT'.
  read_field 'SUBOBJECT'.
  READ TABLE lt_disp TRANSPORTING NO FIELDS BINARY SEARCH
   WITH KEY object = '$TMP' subobject = 'ZAQO_EDIT'.
  CHECK sy-subrc = 0.

  " Hide program options
  DELETE record_tab INDEX sy-tabix.

*  CALL FUNCTION 'F4UT_PARAMETER_RESULTS_PUT'





ENDFUNCTION.
