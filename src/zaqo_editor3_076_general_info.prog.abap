*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
TABLES:
  zsaqo3_general_info.

CLASS lcl_general_info DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
     pbo,
     pai.
ENDCLASS.


CLASS lcl_general_info IMPLEMENTATION.
  METHOD pbo.
    CHECK sy-dynnr = '0076'.
    DATA: lo_screen TYPE REF TO zcl_eui_screen,
          lo_error  TYPE REF TO zcx_eui_exception.
    TRY.
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr = sy-dynnr.
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    DATA lv_input TYPE screen-input VALUE '0'.
    IF go_editor->is_editable( ) = abap_true.
      lv_input = '1'.
    ENDIF.
    lo_screen->customize( group1   = 'MOD'
                          required = '2' " <-- recommended
                          input    = lv_input ).
    lo_screen->pbo( ).
  ENDMETHOD.

  METHOD pai.
    IF zsaqo3_general_info-prev_value_cnt > 7 OR zsaqo3_general_info-prev_value_cnt < 1.
      MESSAGE 'Previous values count has to be from 1 to 7'(m07) TYPE 'E'.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

MODULE pbo_076 OUTPUT.
  DATA go_general_info TYPE REF TO lcl_general_info. "#EC DECL_MODUL
  IF go_general_info IS INITIAL.
    CREATE OBJECT go_general_info.
  ENDIF.

  go_general_info->pbo( ).
ENDMODULE.

MODULE pai_076 INPUT.
  go_general_info->pai( ).
ENDMODULE.
