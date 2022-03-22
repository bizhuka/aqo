interface ZIF_AQO_EXT
  public .

  type-pools ABAP .

  class-methods BEFORE_OPTION_SAVE
    importing
      !IO_OPTION type ref to ZCL_AQO_OPTION
      !IV_IN_EDITOR type ABAP_BOOL
    changing
      !CV_ERROR_TEXT type CSEQUENCE .
endinterface.
