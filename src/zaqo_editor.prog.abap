*&---------------------------------------------------------------------*
*& Report  ZAQO_EDITOR
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zaqo_editor.

INCLUDE zaqo_editor_cld.
INCLUDE zaqo_editor_cli.
INCLUDE zaqo_editor_html_viewer.

START-OF-SELECTION.
  CALL SCREEN 100.

* For node.js testing
FORM post_action
   USING
     iv_method TYPE string
     iv_in     TYPE string
   CHANGING
     cv_out    TYPE string.

  DATA:
    lt_param      TYPE tihttpnvp.

  zcl_aqo_helper=>from_json(
   EXPORTING
    iv_json = iv_in
   IMPORTING
    ex_data = lt_param ).

  " All in 1 call method
  cv_out = lcl_gui_html_viewer=>call_by_name(
    iv_method = iv_method
    it_param  = lt_param ).
ENDFORM.

* For BSP
FORM call_by_name
   USING
     iv_method TYPE string
     it_param  TYPE tihttpnvp
   CHANGING
     cv_out    TYPE string.

  " All in 1 call method
  cv_out = lcl_gui_html_viewer=>call_by_name(
    iv_method = iv_method
    it_param  = it_param ).
ENDFORM.
