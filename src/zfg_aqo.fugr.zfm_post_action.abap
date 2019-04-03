FUNCTION zfm_post_action.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  STRINGVAL
*"     VALUE(IV_IN) TYPE  STRINGVAL
*"  EXPORTING
*"     VALUE(EV_DATA) TYPE  STRINGVAL
*"----------------------------------------------------------------------

  PERFORM post_action IN PROGRAM zaqo_editor
   USING
     iv_action
     iv_in
   CHANGING
     ev_data.
ENDFUNCTION.
