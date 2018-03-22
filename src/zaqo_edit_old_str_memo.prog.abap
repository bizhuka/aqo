*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_string_memo IMPLEMENTATION.
  METHOD get_instance.
    IF mo_instance IS INITIAL.
      CREATE OBJECT mo_instance.
    ENDIF.
    ro_instance = mo_instance.
  ENDMETHOD.

  METHOD call_screen.
    DATA:
      lr_data       TYPE REF TO data.
    FIELD-SYMBOLS:
      <lv_val>      TYPE string.

    " Field description
    mv_refresh = abap_true.
    ms_fld_opt = is_fld_opt.

    " Source
    lr_data = go_opt->get_field_data( ms_fld_opt->name ).
    ASSIGN lr_data->* TO <lv_val>.
    mv_memo = <lv_val>.

    " Show screen
    CALL SCREEN 400 STARTING AT 5 1.
  ENDMETHOD.                    "call_screen

  METHOD pbo.
    DATA:
      lr_cont TYPE REF TO cl_gui_custom_container,
      lv_text TYPE string,
      lv_mode TYPE i,
      lt_code TYPE STANDARD TABLE OF syucomm.

    " 2 buttons
    IF go_opt->is_editable( ms_fld_opt->edit ) <> abap_true.
      APPEND 'OK' TO lt_code.
    ENDIF.
    SET PF-STATUS 'OK_CANCEL' EXCLUDING lt_code.

    IF ms_fld_opt->text IS NOT INITIAL.
      lv_text = ms_fld_opt->text.
    ENDIF.
    SET TITLEBAR 'ST_MAIN' WITH lv_text.

    " One time only
    IF mo_textedit IS INITIAL.
      " Header and grid
      CREATE OBJECT:
       lr_cont
        EXPORTING
          container_name = 'EMPTY_400',

       mo_textedit
        EXPORTING
          parent  = lr_cont
        EXCEPTIONS
          OTHERS  = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    " Update data
    CHECK mv_refresh = abap_true.
    mv_refresh = abap_false.

    " Update data
    mo_textedit->set_textstream( mv_memo ).
    IF go_opt->is_editable( ms_fld_opt->edit ) = abap_true.
      lv_mode = cl_gui_textedit=>false.
    ELSE.
      lv_mode = cl_gui_textedit=>true.
    ENDIF.
    mo_textedit->set_readonly_mode( lv_mode ).
  ENDMETHOD.                    "pbo

  METHOD pai.
    DATA:
      lv_cmd  TYPE syucomm,
      lv_exit TYPE abap_bool,
      lr_data TYPE REF TO data.
    FIELD-SYMBOLS:
      <lv_val>      TYPE string.

    " Save & clear
    lv_cmd = cv_cmd.
    CLEAR cv_cmd.

    " Write data back
    "mo_textedit->check_changed_data( ).

    CASE lv_cmd.
      WHEN 'OK'.
        " Destination
        lr_data = go_opt->get_field_data( ms_fld_opt->name ).
        ASSIGN lr_data->* TO <lv_val>.

        mo_textedit->get_textstream(
         IMPORTING
           text = <lv_val> ).

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
ENDCLASS.                    "IMPLEMENTATION

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pbo_0400 OUTPUT.
  lcl_string_memo=>get_instance( )->pbo( ).
ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_0400 INPUT.
  lcl_string_memo=>get_instance( )->pai(
   CHANGING
     cv_cmd = gv_ok_code ).
ENDMODULE.
