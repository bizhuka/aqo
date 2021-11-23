*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_online_help DEFINITION INHERITING FROM lcl_tab FINAL FRIENDS zcl_eui_event_caller.
  PUBLIC SECTION.
    METHODS:
      pbo REDEFINITION.
    DATA:
      mo_html_viewer TYPE REF TO cl_gui_html_viewer,
      mv_first_time  TYPE abap_bool VALUE abap_true.
ENDCLASS.

CLASS lcl_online_help IMPLEMENTATION.
  METHOD pbo.
    CHECK mo_html_viewer IS INITIAL.

    DO 1 TIMES.
      CHECK mv_first_time = abap_true.
      CLEAR mv_first_time.

      DATA ls_start_command TYPE ts_command.
      zcl_aqo_helper=>exchange_command( IMPORTING es_command = ls_start_command ).
      IF ls_start_command IS NOT INITIAL.
        " Put back
        zcl_aqo_helper=>exchange_command( is_command = ls_start_command ).
        RETURN.
      ENDIF.
    ENDDO.

    " If need online help
    CHECK go_editor           IS NOT INITIAL
      AND go_editor->mo_prefs IS NOT INITIAL
      AND go_editor->mo_prefs->s_opt-v_show_help = abap_true.

    CREATE OBJECT mo_html_viewer
      EXPORTING
        parent = _create_container( )
      EXCEPTIONS
        OTHERS = 1.
    CHECK sy-subrc = 0.

    DATA lv_url TYPE text255 VALUE 'https://bizhuka.github.io/aqo'. "#EC NOTEXT
    IF sy-langu = 'R'.
      lv_url = 'https://bizhuka.github.io/ru/aqo/'.
    ENDIF.
    mo_html_viewer->show_url(
      EXPORTING  url    = lv_url
      EXCEPTIONS OTHERS = 0 ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

MODULE pbo_071 OUTPUT.
  DATA go_online_help TYPE REF TO lcl_online_help.      "#EC DECL_MODUL
  IF go_online_help IS INITIAL.
    CREATE OBJECT go_online_help.
  ENDIF.

  go_online_help->pbo( ).
ENDMODULE.

*MODULE pai_071 INPUT.
*  go_online_help->pai( ).
*ENDMODULE.
