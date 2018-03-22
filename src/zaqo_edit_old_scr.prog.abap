*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

" Options
SELECTION-SCREEN BEGIN OF BLOCK bl_main WITH FRAME.
PARAMETERS : p_object TYPE zsaqo_search_help-object     OBLIGATORY MEMORY ID zaqo_object,
             p_sub_ob TYPE zsaqo_search_help-subobject  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl_main.



SELECTION-SCREEN BEGIN OF SCREEN 1010.
SELECTION-SCREEN BEGIN OF BLOCK bl_where_used.

PARAMETERS:
  p_main   TYPE abap_callstack_line-mainprogram MODIF ID par,
  p_incl   TYPE abap_callstack_line-include     MODIF ID par,
  p_line   TYPE abap_callstack_line-line        MODIF ID par,
  p_bl_typ TYPE abap_callstack_line-blocktype   MODIF ID par,
  p_bl_nam TYPE abap_callstack_line-blockname   MODIF ID par.

SELECTION-SCREEN END OF BLOCK bl_where_used.
SELECTION-SCREEN END OF SCREEN 1010.

AT SELECTION-SCREEN OUTPUT.
  CASE sy-dynnr.
    WHEN 1010.
      lcl_where_used=>get_instance( )->pbo( ).
  ENDCASE.

AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN 1010.
      lcl_where_used=>get_instance( )->pai(
       CHANGING
         cv_cmd = sy-ucomm ).
  ENDCASE.

START-OF-SELECTION.
  CREATE OBJECT go_opt.

*----------------------------------------------------------------------*
*  PAI_EXIT
*----------------------------------------------------------------------*
MODULE pai_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                    "pai_exit INPUT
