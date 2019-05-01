*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

" Options
SELECTION-SCREEN BEGIN OF BLOCK bl_main WITH FRAME.
PARAMETERS : p_pack   TYPE ztaqo_option-package_id OBLIGATORY MEMORY ID zaqo_package_id,
             p_opt_id TYPE ztaqo_option-option_id  OBLIGATORY MEMORY ID zaqo_option_id.
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

SELECTION-SCREEN BEGIN OF SCREEN 1020.
SELECTION-SCREEN BEGIN OF BLOCK bl_new_field.

PARAMETERS:
  p_name TYPE zcl_aqo_helper=>ts_field_desc-name      MODIF ID obl,
  p_type TYPE zcl_aqo_helper=>ts_field_desc-rollname  MODIF ID obl.

SELECTION-SCREEN END OF BLOCK bl_new_field.
SELECTION-SCREEN END OF SCREEN 1020.

SELECTION-SCREEN FUNCTION KEY:
 1,
 2,
 3,
 4.




" SH deleted from dictionary (for easy activation)

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pack.
  lcl_opt=>on_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_opt_id.
  lcl_opt=>on_f4( ).


AT SELECTION-SCREEN OUTPUT.
  CASE sy-dynnr.
    WHEN 1000.
      lcl_opt=>pbo( ).

    WHEN 1010.
      go_where_used = lcl_where_used=>get_instance( ).
      go_where_used->pbo( ).

    WHEN 1020.
      go_fld_value_alv = lcl_fld_value_alv=>get_instance( ).
      go_fld_value_alv->pbo_1020( ).
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

    WHEN 1020.
      go_fld_value_alv = lcl_fld_value_alv=>get_instance( ).
      go_fld_value_alv->pai_1020(
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
