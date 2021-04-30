interface ZIF_AQO_MENU_EXT
  public .


  methods ADD_BUTTONS
    importing
      !IV_ROOT_FUNC type UI_FUNC
      !IS_STATE type ZCL_AQO_MENU_HANDLER=>TS_STATE
    changing
      !CT_MENU type ZCL_EUI_MENU=>TT_MENU .
  methods BUTTON_PRESSED
    importing
      !IV_COMMAND type CSEQUENCE
      !IS_DB_ITEM type ZTAQO_OPTION
      !IV_TCODE type SYTCODE
    returning
      value(RV_OK) type ABAP_BOOL
    raising
      ZCX_AQO_EXCEPTION .
endinterface.
