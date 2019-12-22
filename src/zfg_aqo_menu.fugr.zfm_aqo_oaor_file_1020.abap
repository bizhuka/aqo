FUNCTION ZFM_AQO_OAOR_FILE_1020.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_PACKAGE_ID) TYPE  ZTAQO_OPTION-PACKAGE_ID
*"     REFERENCE(IV_OPTION_ID) TYPE  ZTAQO_OPTION-OPTION_ID
*"  EXPORTING
*"     REFERENCE(EV_OK) TYPE  ABAP_BOOL
*"  CHANGING
*"     REFERENCE(CS_INFO) TYPE  ZSAQO_OAOR_FILE_F4
*"--------------------------------------------------------------------
  p_2_pack = iv_package_id.
  p_2_opt  = iv_option_id.

  p_2_file = cs_info-file_name.
  p_2_vers = cs_info-doc_ver_no.
  p_2_desc = cs_info-description.
  p_2_vis  = cs_info-visible.

  CALL SELECTION-SCREEN 1020 STARTING AT 5 1.
  IF p_2_pack IS INITIAL.
    ev_ok = abap_false.
    RETURN.
  ENDIF.

  " Return result
  ev_ok               = abap_true.
  cs_info-description = p_2_desc.
  cs_info-visible     = p_2_vis.
ENDFUNCTION.
