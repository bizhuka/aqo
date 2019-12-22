FUNCTION ZFM_AQO_ABOUT_1010.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EV_LAST_COMMAND) TYPE  SYUCOMM
*"  CHANGING
*"     REFERENCE(CS_DB_ITEM) TYPE  ZTAQO_OPTION
*"----------------------------------------------------------------------

  " Previous values
  p_1_desc  = cs_db_item-description.
  p_1_prev  = cs_db_item-prev_value_cnt.
  p_1_menu  = cs_db_item-menu_mode.

  " Gray fields
  p_1_pack   = cs_db_item-package_id.
  p_1_opt    = cs_db_item-option_id.
  p_1_date   = cs_db_item-created_date.
*  p_1_name   = cs_db_item-created_uname.
  p_1_ntxt   = cs_db_item-created_name_text.

  " Show screen
  gv_ok_code = sy-ucomm = ''.
  CALL SELECTION-SCREEN 1010 STARTING AT 5 1.
  ev_last_command = gv_ok_code.

  " Pressed OK or cancel do nothing
  IF sy-subrc <> 0 OR p_1_prev IS INITIAL.
    CLEAR cs_db_item.
    RETURN.
  ENDIF.

  " For EDIT command
  cs_db_item-description    = p_1_desc.
  cs_db_item-prev_value_cnt = p_1_prev.
  cs_db_item-menu_mode      = p_1_menu.
ENDFUNCTION.
