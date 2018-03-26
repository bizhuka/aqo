*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_table_comp_alv IMPLEMENTATION.
  METHOD get_instance.
    IF mo_instance IS INITIAL.
      CREATE OBJECT mo_instance.
    ENDIF.
    ro_instance = mo_instance.
  ENDMETHOD.                    "get_instance

  METHOD call_screen.
    DATA:
      lv_ok TYPE abap_bool.

    " Field description
    ms_fld_opt    = is_fld_opt.

    " Table to show
    zcl_aqo_util=>from_json(
     EXPORTING
      iv_json = ms_fld_opt->subcomps
     IMPORTING
      ev_ok   = lv_ok
      ex_data = mt_components ).
    IF lv_ok <> abap_true.
      MESSAGE s017(zaqo_mes) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    mv_refresh    = abap_true.

    " Show screen
    CALL SCREEN 300 STARTING AT 5 1.
  ENDMETHOD.                    "call_screen

  METHOD pbo.
    DATA:
      lr_cont     TYPE REF TO cl_gui_custom_container,
      lt_fieldcat TYPE lvc_t_fcat,
      ls_fieldcat TYPE REF TO lvc_s_fcat,
      ls_layout   TYPE lvc_s_layo,
      ls_variant  TYPE disvariant,
      lv_text     TYPE string VALUE '-'.

*    " Own buttons
    SET PF-STATUS 'OK_CANCEL'.

    IF ms_fld_opt->text IS NOT INITIAL.
      lv_text = ms_fld_opt->text.
    ENDIF.
    SET TITLEBAR 'ST_MAIN' WITH lv_text.

    " One time only
    IF mr_grid IS NOT INITIAL.
      IF mv_refresh = abap_true.
        mr_grid->refresh_table_display( ).
      ENDIF.
      mv_refresh = abap_false.

      RETURN.
    ENDIF.

    " Header and grid
    CREATE OBJECT:
     lr_cont
      EXPORTING
        container_name = 'EMPTY_300',

    " Show at first SCREEN
     mr_grid
      EXPORTING
        i_parent = lr_cont
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    " Get field catalog
    zcl_aqo_util=>create_field_catalog(
     IMPORTING
       et_fieldcat = lt_fieldcat
     CHANGING
       ct_table    = mt_components ).

    " Addtional options
    IF go_opt->is_editable( ms_fld_opt->edit ) = abap_true.
*      ls_layout-edit = abap_true.
      LOOP AT lt_fieldcat REFERENCE INTO ls_fieldcat.
        CASE ls_fieldcat->fieldname.
          WHEN 'ROLLNAME' OR 'TEXT'.
            ls_fieldcat->edit = abap_true.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    " Prepare layout
    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode   = 'C'.
    ls_layout-no_toolbar = abap_true.

    " Variant
    ls_variant-report  = p_object.
    ls_variant-handle  = '0003'.

    mr_grid->set_table_for_first_display(
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
*        it_toolbar_excluding          = lt_toolbar_ex
      CHANGING
        it_outtab                     = mt_components
        it_fieldcatalog               = lt_fieldcat
      EXCEPTIONS
        OTHERS                        = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "pbo

  METHOD pai.
    DATA:
      lv_cmd  TYPE syucomm,
      lv_exit TYPE abap_bool.

    " Save & clear
    lv_cmd = cv_cmd.
    CLEAR cv_cmd.

    " Write data back
    mr_grid->check_changed_data( ).

    CASE lv_cmd.
      WHEN 'OK'.
        ms_fld_opt->subcomps = zcl_aqo_util=>to_json( mt_components ).
        lv_exit = abap_true.
        MESSAGE s004(zaqo_mes).

      WHEN 'CANCEL'.
        lv_exit = abap_true.
        MESSAGE s130(ed) WITH TEXT-edt DISPLAY LIKE 'E'.
    ENDCASE.

    IF lv_exit = abap_true.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDMETHOD.                    "pai
ENDCLASS.


*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.
  go_table_comp_alv = lcl_table_comp_alv=>get_instance( ).
  go_table_comp_alv->pbo( ).
ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_0300 INPUT.
  go_table_comp_alv = lcl_table_comp_alv=>get_instance( ).
  go_table_comp_alv->pai(
   CHANGING
     cv_cmd = gv_ok_code ).
ENDMODULE.
