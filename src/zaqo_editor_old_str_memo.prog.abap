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
    DATA lr_text     TYPE REF TO string.
    DATA lv_editable TYPE abap_bool.
    DATA lo_memo     TYPE REF TO zcl_eui_memo.

    lr_text ?= is_fld_value->cur_value.
    lv_editable = lcl_opt=>is_editable( is_fld_value->is_editable ).

    CREATE OBJECT lo_memo
      EXPORTING
        ir_text     = lr_text
        iv_editable = lv_editable.
    lo_memo->popup( ).
    rv_close_cmd = lo_memo->show( ). "io_handler = me.
  ENDMETHOD.
ENDCLASS.
