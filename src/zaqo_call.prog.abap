REPORT  zaqo_call.

PARAMETERS: p_pack TYPE ztaqo_option-package_id OBLIGATORY,
            p_opt  TYPE ztaqo_option-option_id  OBLIGATORY,
            p_mode TYPE ztaqo_option-menu_mode  OBLIGATORY.

START-OF-SELECTION.
  DATA:
    ls_db_opt TYPE zsaqo_option.

  ls_db_opt-package_id = p_pack.
  ls_db_opt-option_id  = p_opt.
  ls_db_opt-menu_mode  = p_mode.

  PERFORM start_editor IN PROGRAM (zcl_aqo_helper=>mc_prog-editor)
   USING ls_db_opt.
