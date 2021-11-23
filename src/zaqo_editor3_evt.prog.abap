*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

INITIALIZATION.
  CREATE OBJECT go_editor.

START-OF-SELECTION.
  go_editor->start_of_selection( ).

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*

MODULE pbo_070 OUTPUT.
  go_editor->pbo( ).
ENDMODULE.

MODULE pai_exit INPUT.
  PERFORM pai_exit.
ENDMODULE.

MODULE pai_070 INPUT.
  go_editor->pai( CHANGING cv_ok_code = ok_code ).
ENDMODULE.

MODULE f4_free_search INPUT.
  go_editor->mo_tree->f4_free_search( ).
ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM pai_exit.
  " go_editor->pai( CHANGING cv_ok_code = ok_code ).
  CLEAR ok_code.
  DATA ls_command TYPE ts_command.
  ls_command-ucomm = mc_pai_cmd-exit.
  zcl_aqo_helper=>exchange_command( is_command = ls_command ).
ENDFORM.

FORM start_editor USING is_db_opt TYPE zsaqo_option. "#EC CALLED  External call in GOS menu
  DATA ls_db_key TYPE ts_db_key.
  MOVE-CORRESPONDING is_db_opt TO ls_db_key.

  DATA lv_tcode TYPE sytcode.
  CASE is_db_opt-menu_mode.
    WHEN zcl_aqo_helper=>mc_menu_mode-edit.
      lv_tcode = zcl_aqo_helper=>mc_prog-editor_tcode.
    WHEN zcl_aqo_helper=>mc_menu_mode-view.
      lv_tcode = zcl_aqo_helper=>mc_prog-viewer_tcode.
  ENDCASE.
  zcl_aqo_helper=>is_in_editor( iv_tcode = lv_tcode ).

  CREATE OBJECT go_editor.
  go_editor->do_open( is_db_key      = ls_db_key
                      iv_true_editor = abap_false ).
  go_editor->show_all( iv_ok_as_save = abap_true ).
ENDFORM.
