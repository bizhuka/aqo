*&---------------------------------------------------------------------*
*  AQO - ABAP quick options
*  @see https://github.com/bizhuka/aqo
*  @version: 1.0
*&---------------------------------------------------------------------*
REPORT zaqo_edit.

INCLUDE zaqo_edit_cld.
INCLUDE zaqo_edit_cli.
INCLUDE zaqo_edit_gui_html_viewer.

INITIALIZATION.
  lcl_opt=>initialization( ).

START-OF-SELECTION.
  CALL SCREEN 100.
