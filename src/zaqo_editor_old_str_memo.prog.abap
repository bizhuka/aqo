*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_string_memo IMPLEMENTATION.
  METHOD get_instance.
    " Create by name
    IF iv_level IS SUPPLIED.
      mo_last_instance ?= lcl_nested_instance=>get_instance_by_level(
       iv_cl_name = 'LCL_STRING_MEMO'
       iv_level   = iv_level ).
    ENDIF.

    ro_instance = mo_last_instance.
  ENDMETHOD.

  METHOD call_screen.
    FIELD-SYMBOLS:
      <lv_val>      TYPE string.

    " Field description
    mv_refresh = abap_true.
    ms_fld_value = is_fld_value.

    " Source
    ASSIGN ms_fld_value->cur_value->* TO <lv_val>.
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
    IF lcl_opt=>is_editable( ms_fld_value->is_editable ) <> abap_true.
      APPEND 'OK' TO lt_code.
    ENDIF.
    SET PF-STATUS 'OK_CANCEL' EXCLUDING lt_code.

    IF ms_fld_value->label IS NOT INITIAL.
      lv_text = ms_fld_value->label.
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
    IF lcl_opt=>is_editable( ms_fld_value->is_editable ) = abap_true.
      lv_mode = cl_gui_textedit=>false.
    ELSE.
      lv_mode = cl_gui_textedit=>true.
    ENDIF.
    mo_textedit->set_readonly_mode( lv_mode ).
  ENDMETHOD.                    "pbo

  METHOD pai.
    DATA:
      lv_exit TYPE abap_bool.
    FIELD-SYMBOLS:
      <lv_val>      TYPE string.

    " Save & clear
    mv_last_cmd = cv_cmd.
    CLEAR cv_cmd.

    " Write data back
    "mo_textedit->check_changed_data( ).

    CASE mv_last_cmd.
      WHEN 'OK'.
        " Destination
        ASSIGN ms_fld_value->cur_value->* TO <lv_val>.

        mo_textedit->get_textstream(
         IMPORTING
           text = <lv_val> ).
        cl_gui_cfw=>flush( ).

        lv_exit = abap_true.
        MESSAGE s004(zaqo_message).

      WHEN 'CANCEL'.
        lv_exit = abap_true.
        MESSAGE s130(ed) WITH 'Edit'(edt) DISPLAY LIKE 'E'.
    ENDCASE.

    IF lv_exit = abap_true.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDMETHOD.                    "pai
ENDCLASS.                    "IMPLEMENTATION

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pbo_0400 OUTPUT.
  go_string_memo = lcl_string_memo=>get_instance( ).
  go_string_memo->pbo( ).
ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_0400 INPUT.
  go_string_memo = lcl_string_memo=>get_instance( ).
  go_string_memo->pai(
   CHANGING
     cv_cmd = gv_ok_code ).
ENDMODULE.
