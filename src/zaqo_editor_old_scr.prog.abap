*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

" Options
SELECTION-SCREEN BEGIN OF BLOCK bl_main WITH FRAME.
PARAMETERS : p_pack   TYPE ztaqo_option-package_id OBLIGATORY MEMORY ID zaqo_package_id,
             p_opt_id TYPE ztaqo_option-option_id  OBLIGATORY MEMORY ID zaqo_option_id.
SELECTION-SCREEN END OF BLOCK bl_main.


**********************************************************************
* Add new field
**********************************************************************
SELECTION-SCREEN BEGIN OF SCREEN 1010 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK bl_1010 WITH FRAME TITLE TEXT-010.

PARAMETERS p_fname TYPE zsaqo_new_field-p_fname.
PARAMETERS p_ftype TYPE zsaqo_new_field-p_ftype.

SELECTION-SCREEN END OF BLOCK bl_1010.
SELECTION-SCREEN END OF SCREEN 1010.


**********************************************************************
* About dialog
**********************************************************************
SELECTION-SCREEN BEGIN OF SCREEN 1020 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK bl_1020 WITH FRAME TITLE TEXT-020.

PARAMETERS:
  p_2_pack TYPE zsaqo_about_dialog-p_2_pack,   " MODIF ID gry,
  p_2_opt  TYPE zsaqo_about_dialog-p_2_opt,    " MODIF ID gry,
  p_2_date TYPE zsaqo_about_dialog-p_2_date,   " MODIF ID gry,
*  p_2_name TYPE zsaqo_about_dialog-p_2_name,   " MODIF ID gry,
  p_2_ntxt TYPE zsaqo_about_dialog-p_2_ntxt,   " MODIF ID gry,

  p_2_desc TYPE zsaqo_about_dialog-p_2_desc,   "  MODIF ID deo, " DEV + OBL
  p_2_prev TYPE zsaqo_about_dialog-p_2_prev,   "  MODIF ID deo, " DEV + OBL
  p_2_menu TYPE zsaqo_about_dialog-p_2_menu.   "  MODIF ID dev.

SELECTION-SCREEN END OF BLOCK bl_1020.
SELECTION-SCREEN END OF SCREEN 1020.

**********************************************************************
* New OAOR file version dialog
**********************************************************************
SELECTION-SCREEN BEGIN OF SCREEN 1030 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK bl_1030 WITH FRAME TITLE TEXT-030.

PARAMETERS:
  p_3_pack TYPE ztaqo_option-package_id,       " MODIF ID gry,
  p_3_opt  TYPE ztaqo_option-option_id,        " MODIF ID gry,
  p_3_file TYPE zsaqo_oaor_dialog-p_3_file,    " MODIF ID gry,
  p_3_vers TYPE zsaqo_oaor_dialog-p_3_vers,    " MODIF ID gry,

  p_3_desc TYPE zsaqo_oaor_dialog-p_3_desc,    " MODIF ID dev,
  p_3_vis  TYPE zsaqo_oaor_dialog-p_3_vis AS CHECKBOX. " MODIF ID dev.

SELECTION-SCREEN END OF BLOCK bl_1030.
SELECTION-SCREEN END OF SCREEN 1030.


**********************************************************************
* Table key
**********************************************************************

SELECTION-SCREEN BEGIN OF SCREEN 1040 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK bl_1040 WITH FRAME TITLE TEXT-010.

PARAMETERS:
  p_4_kind TYPE zsaqo_table_key_dialog-p_4_kind AS LISTBOX VISIBLE LENGTH 50.
SELECT-OPTIONS:
  s_4_key  FOR zsaqo_keydescr-low NO INTERVALS.
PARAMETERS:
  p_4_unq  TYPE zsaqo_table_key_dialog-p_4_unq AS CHECKBOX,
  p_4_keyd TYPE zsaqo_table_key_dialog-p_4_keyd AS LISTBOX VISIBLE LENGTH 50.

SELECTION-SCREEN END OF BLOCK bl_1040.
SELECTION-SCREEN END OF SCREEN 1040.


" SH deleted from dictionary (for easy activation)

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pack.
  lcl_opt=>on_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_opt_id.
  lcl_opt=>on_f4( abap_true ).

INITIALIZATION.
  lcl_opt=>initialization( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_opt=>pbo( ).

AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN 1000.
      lcl_opt=>pai(
       CHANGING
         cv_cmd = sy-ucomm ).
  ENDCASE.

START-OF-SELECTION.
  lcl_opt=>start_of_selection( ).

**********************************************************************
* Flex method. @see usage
FORM call_by_name
   USING
     iv_method TYPE csequence.

  " Call by name
  CALL METHOD lcl_opt=>(iv_method).
ENDFORM.
