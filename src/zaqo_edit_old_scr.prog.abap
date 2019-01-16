*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

" Options
SELECTION-SCREEN BEGIN OF BLOCK bl_main WITH FRAME.
PARAMETERS : p_object TYPE ztaqo_data-object     MODIF ID obl MEMORY ID zaqo_object,
             p_sub_ob TYPE ztaqo_data-subobject  MODIF ID obl MEMORY ID zaqo_subobject.
SELECTION-SCREEN END OF BLOCK bl_main.

" Where used
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

SELECTION-SCREEN FUNCTION KEY 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_object.
  lcl_opt=>on_f4( iv_field = 'OBJECT'    iv_dynpro = 'P_OBJECT' ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sub_ob.
  lcl_opt=>on_f4( iv_field = 'SUBOBJECT' iv_dynpro = 'P_SUB_OB' ).

AT SELECTION-SCREEN OUTPUT.
  CASE sy-dynnr.
    WHEN 1000.
      lcl_opt=>pbo( ).
    WHEN 1010.
      go_where_used = lcl_where_used=>get_instance( ).
      go_where_used->pbo( ).
  ENDCASE.

AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN 1000.
      lcl_opt=>pai(
       CHANGING
         cv_cmd = sy-ucomm ).

    WHEN 1010.
      go_where_used = lcl_where_used=>get_instance( ).
      go_where_used->pai(
       CHANGING
         cv_cmd = sy-ucomm ).
  ENDCASE.

START-OF-SELECTION.
  lcl_opt=>start_of_selection( ).

*----------------------------------------------------------------------*
*  PAI_EXIT
*----------------------------------------------------------------------*
MODULE pai_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                    "pai_exit INPUT
