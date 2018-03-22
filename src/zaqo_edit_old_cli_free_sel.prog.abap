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
      ls_fld_opt TYPE REF TO lcl_opt=>ts_fld_opt,
      ls_screen  TYPE screen,
      lv_edit    TYPE abap_bool,
      lv_show    TYPE abap_bool.

    " Prepare screen
    LOOP AT SCREEN.
      " Do not edit ICON for masks
      CHECK screen-group3 <> 'OPU'.

      READ TABLE go_opt->mt_fld_opt REFERENCE INTO ls_fld_opt INDEX screen-group1.
      CHECK sy-subrc = 0.

      " Change one time only
      ls_screen = screen.
      lv_edit = lv_show = abap_undefined.

      IF go_opt->is_editable( ls_fld_opt->edit ) <> abap_true AND ls_screen-group3 <> 'VPU'. " But not for tables
        lv_edit = abap_false.
      ENDIF.

      if ls_fld_opt->kind = zcl_aqo=>mc_kind_table OR
         ls_fld_opt->kind = zcl_aqo=>mc_kind_memo.
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

*      " Prepare 1 field
*      IF go_opt->mo_ui_ext IS NOT INITIAL.
*        TRY.
*            go_opt->mo_ui_ext->pbo(
*             EXPORTING
*              iv_field_name = ls_fld_opt->name
*             CHANGING
*              cs_screen     = ls_screen ).
*          CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*
*        ENDTRY.
*      ENDIF.

      " And update
      CHECK ls_screen <> screen.
      screen = ls_screen.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.                    "pbo

  METHOD pai.
    DATA:
      lv_grp_n   TYPE numc3,
      ls_fld_opt TYPE REF TO lcl_opt=>ts_fld_opt.

    " Only push button of range
    CHECK cv_cmd(1) = '%'.

    TRY.
        lv_grp_n = ( cv_cmd+1 - 2 ) / 2.
      CATCH cx_sy_conversion_no_number.
        RETURN.
    ENDTRY.

    " Show ALG grid
    READ TABLE go_opt->mt_fld_opt REFERENCE INTO ls_fld_opt INDEX lv_grp_n.
    CHECK sy-subrc = 0.

    " Only for tables
    CASE ls_fld_opt->kind.
      WHEN zcl_aqo=>mc_kind_table.
        lcl_table_alv=>get_instance( )->call_screen( ls_fld_opt ).

      WHEN zcl_aqo=>mc_kind_memo.
        lcl_string_memo=>get_instance( )->call_screen( ls_fld_opt ).

      WHEN OTHERS.
        RETURN.
    ENDCASE.

    " Hide range screen
    CLEAR cv_cmd.
  ENDMETHOD.                    "pai

*  METHOD on_f1.
*    DATA:
*      ls_fld_opt TYPE REF TO lcl_opt=>ts_fld_opt.
*
*    READ TABLE go_opt->mt_fld_opt REFERENCE INTO ls_fld_opt INDEX is_fldnum-group1.
*    CHECK sy-subrc = 0.
*
*    " F1 pressed
*    IF go_opt->mo_ui_ext IS NOT INITIAL.
*      TRY.
*          go_opt->mo_ui_ext->on_f1(
*           EXPORTING
*            iv_field_name = ls_fld_opt->name ).
*        CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*
*      ENDTRY.
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD on_f4.
*    DATA:
*      ls_fld_opt TYPE REF TO lcl_opt=>ts_fld_opt.
*
*    READ TABLE go_opt->mt_fld_opt REFERENCE INTO ls_fld_opt INDEX is_fldnum-group1.
*    CHECK sy-subrc = 0.
*
*    " F4 pressed
*    IF go_opt->mo_ui_ext IS NOT INITIAL.
*      TRY.
*          go_opt->mo_ui_ext->on_f4(
*           EXPORTING
*            iv_field_name = ls_fld_opt->name
*            is_f4info     = is_f4info
*           CHANGING
*            cv_value      = cv_value ).
*        CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*
*      ENDTRY.
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD at_selection_screen_on.
*    DATA:
*      ls_fld_opt TYPE REF TO lcl_opt=>ts_fld_opt.
*
*    READ TABLE go_opt->mt_fld_opt REFERENCE INTO ls_fld_opt INDEX is_fldnum-group1.
*    CHECK sy-subrc = 0.
*
*    " AT SELECTION-SCREEN ON
*    IF go_opt->mo_ui_ext IS NOT INITIAL.
*      TRY.
*          go_opt->mo_ui_ext->at_selection_screen_on(
*           EXPORTING
*            iv_field_name = ls_fld_opt->name
*            it_seldyn     = it_seldyn ).
*        CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*
*      ENDTRY.
*    ENDIF.
*  ENDMETHOD.
ENDCLASS.                    "lcl_free_sel IMPLEMENTATION


*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM sel_screen_pbo TABLES ct_seldyn STRUCTURE rsseldyn
                           ct_fldnum STRUCTURE rsdsfldnum.  "#EC CALLED
  SORT ct_fldnum BY group1.
  lcl_scr_free_sel=>get_instance( )->pbo(
   it_seldyn    = ct_seldyn[]
   it_dsfldnum  = ct_fldnum[] ).
ENDFORM.                    "SEL_SCREEN_PBO

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM sel_screen_pai TABLES ct_seldyn STRUCTURE rsseldyn
                           ct_fldnum STRUCTURE rsdsfldnum.  "#EC CALLED
  FIELD-SYMBOLS:
    <ls_sscrfields> TYPE sscrfields.

  " Current command
  ASSIGN ('(SAPLSSEL)SSCRFIELDS') TO <ls_sscrfields>.
  CHECK <ls_sscrfields> IS ASSIGNED.

  SORT ct_fldnum BY group1.
  lcl_scr_free_sel=>get_instance( )->pai(
   EXPORTING
     it_seldyn    = ct_seldyn[]
     it_dsfldnum  = ct_fldnum[]
   CHANGING
     cv_cmd         = <ls_sscrfields>-ucomm ).
ENDFORM.                    "SEL_SCREEN_PAI

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*

FORM sel_screen_at_scr TABLES ct_seldyn STRUCTURE rsseldyn
                       USING  is_fldnum TYPE rsdsfldnum.    "#EC CALLED

*  lcl_scr_free_sel=>get_instance( )->at_selection_screen_on(
*   is_fldnum = is_fldnum
*   it_seldyn = ct_seldyn[] ).
ENDFORM.

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM sel_screen_f1
  USING  is_fldnum TYPE rsdsfldnum.                         "#EC CALLED

*  lcl_scr_free_sel=>get_instance( )->on_f1(
*   is_fldnum = is_fldnum ).
ENDFORM.

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM sel_screen_f4
  USING  is_fldnum TYPE rsdsfldnum
         is_f4info TYPE rsdsf4info
         iv_value  TYPE any.                                "#EC CALLED

*  lcl_scr_free_sel=>get_instance( )->on_f4(
*   EXPORTING
*    is_fldnum = is_fldnum
*    is_f4info = is_f4info
*   CHANGING
*    cv_value  = iv_value ).
ENDFORM.
