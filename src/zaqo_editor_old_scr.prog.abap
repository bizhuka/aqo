*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

" Options
SELECTION-SCREEN BEGIN OF BLOCK bl_main WITH FRAME.
PARAMETERS : p_pack   TYPE ztaqo_option-package_id OBLIGATORY MEMORY ID zaqo_package_id,
             p_opt_id TYPE ztaqo_option-option_id  OBLIGATORY MEMORY ID zaqo_option_id.
SELECTION-SCREEN END OF BLOCK bl_main.

**********************************************************************

" SH deleted from dictionary (for easy activation)

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pack.
  lcl_opt=>on_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_opt_id.
  lcl_opt=>on_f4( abap_true ).

INITIALIZATION.
  lcl_opt=>initialization( ).

AT SELECTION-SCREEN OUTPUT.
  CASE sy-dynnr.
    WHEN 1000.
      lcl_opt=>pbo( ).
  ENDCASE.

AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN 1000.
      lcl_opt=>pai(
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


**********************************************************************
* Flex method. @see usage
FORM call_by_name
   USING
     iv_method TYPE csequence.

  " Call by name
  CALL METHOD lcl_opt=>(iv_method).
ENDFORM.
