*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_scr_free_sel IMPLEMENTATION.
  METHOD get_instance.
    IF mo_instance IS INITIAL.
      CREATE OBJECT mo_instance.
    ENDIF.
    ro_instance = mo_instance.
  ENDMETHOD.                    "get_instance

  METHOD pbo.
    DATA:
      ls_fld_value TYPE REF TO lcl_opt=>ts_fld_value,
      ls_screen  TYPE screen,
      lv_edit    TYPE abap_bool,
      lv_show    TYPE abap_bool.

    " Prepare screen
    LOOP AT SCREEN.
      " Do not edit ICON for masks
      CHECK screen-group3 <> 'OPU'.

      READ TABLE lcl_opt=>mt_fld_value REFERENCE INTO ls_fld_value INDEX screen-group1.
      CHECK sy-subrc = 0.

      " Change one time only
      ls_screen = screen.
      lv_edit = lv_show = abap_undefined.

      IF lcl_opt=>is_editable( ls_fld_value->is_editable ) <> abap_true AND ls_screen-group3 <> 'VPU'. " But not for tables
        lv_edit = abap_false.
      ENDIF.

      IF ls_fld_value->ui_type = zcl_aqo_helper=>mc_ui_table OR
         ls_fld_value->ui_type = zcl_aqo_helper=>mc_ui_string.
        IF ls_screen-group3 = 'LOW' OR ls_screen-group3 = 'TOT' OR ls_screen-group3 = 'HGH'.
          lv_show = abap_false.
        ENDIF.
      ENDIF.

      " Hide elements
      CASE lv_show.
        WHEN abap_false.
          ls_screen-active    = '0'.
          ls_screen-invisible = '1'.
          lv_edit = abap_false.
      ENDCASE.

      " Do not edit
      CASE lv_edit.
        WHEN abap_false.
          ls_screen-input     = '0'.
      ENDCASE.

      " And update
      CHECK ls_screen <> screen.
      screen = ls_screen.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.                    "pbo

  METHOD pai.
    DATA:
      lv_grp_n       TYPE numc3,
      ls_fld_value     TYPE REF TO lcl_opt=>ts_fld_value,
      lo_table_alv   TYPE REF TO lcl_table_alv,
      lo_string_memo TYPE REF TO lcl_string_memo.

    " Only push button of range
    CHECK cv_cmd(1) = '%'.

    TRY.
        lv_grp_n = ( cv_cmd+1 - 2 ) / 2.
      CATCH cx_sy_conversion_no_number.
        RETURN.
    ENDTRY.

    " Show ALG grid
    READ TABLE lcl_opt=>mt_fld_value REFERENCE INTO ls_fld_value INDEX lv_grp_n.
    CHECK sy-subrc = 0.

    " Only for tables
    CASE ls_fld_value->ui_type.
      WHEN zcl_aqo_helper=>mc_ui_table.
        lo_table_alv = lcl_table_alv=>get_instance( ).
        lo_table_alv->call_screen( ls_fld_value ).

      WHEN zcl_aqo_helper=>mc_ui_string.
        lo_string_memo = lcl_string_memo=>get_instance( ).
        lo_string_memo->call_screen( ls_fld_value ).

      WHEN OTHERS.
        RETURN.
    ENDCASE.

    " Hide range screen
    CLEAR cv_cmd.
  ENDMETHOD.                    "pai
ENDCLASS.


*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM sel_screen_pbo TABLES ct_seldyn STRUCTURE rsseldyn
                           ct_fldnum STRUCTURE rsdsfldnum.  "#EC CALLED
  DATA:
    lo_scr_free_sel TYPE REF TO lcl_scr_free_sel.

  SORT ct_fldnum BY group1.
  lo_scr_free_sel = lcl_scr_free_sel=>get_instance( ).
  lo_scr_free_sel->pbo(
   it_seldyn    = ct_seldyn[]
   it_dsfldnum  = ct_fldnum[] ).
ENDFORM.

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM sel_screen_pai TABLES ct_seldyn STRUCTURE rsseldyn
                           ct_fldnum STRUCTURE rsdsfldnum.  "#EC CALLED
  DATA:
    lo_scr_free_sel TYPE REF TO lcl_scr_free_sel.
  FIELD-SYMBOLS:
    <ls_sscrfields> TYPE sscrfields.

  " Current command
  ASSIGN ('(SAPLSSEL)SSCRFIELDS') TO <ls_sscrfields>.
  CHECK <ls_sscrfields> IS ASSIGNED.

  SORT ct_fldnum BY group1.
  lo_scr_free_sel = lcl_scr_free_sel=>get_instance( ).
  lo_scr_free_sel->pai(
   EXPORTING
     it_seldyn    = ct_seldyn[]
     it_dsfldnum  = ct_fldnum[]
   CHANGING
     cv_cmd         = <ls_sscrfields>-ucomm ).
ENDFORM.
