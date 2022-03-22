*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_usage_in_code DEFINITION INHERITING FROM lcl_tab FINAL FRIENDS zcl_eui_event_caller.
  PUBLIC SECTION.
    METHODS:
      pbo REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      _fill_table       REDEFINITION,
      _get_layout       REDEFINITION,
      _get_catalog      REDEFINITION,
      _get_toolbar      REDEFINITION,
      _on_hotspot_click REDEFINITION,
      _on_user_command  REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF mc_button,
        scan TYPE syucomm VALUE '_SCAN',
      END OF mc_button.

    TYPES:
      BEGIN OF ts_1st_call,
        field TYPE fieldname,
        value TYPE fieldvalue,
      END OF ts_1st_call,
      tt_1st_call TYPE STANDARD TABLE OF ts_1st_call WITH DEFAULT KEY.

    DATA:
      mt_1st_call  TYPE tt_1st_call.
ENDCLASS.

CLASS lcl_usage_in_code IMPLEMENTATION.
  METHOD pbo.
    GET REFERENCE OF mt_1st_call INTO mr_table.
    super->pbo( ).
  ENDMETHOD.

  METHOD _get_layout.
    rs_layout = super->_get_layout( ).
    rs_layout-sel_mode   = 'A'.
    rs_layout-grid_title = 'First creation place (Could have been changed)'(fcp).
  ENDMETHOD.

  METHOD _fill_table.
    CLEAR mt_1st_call[].

    " BLOCKNAME is STRING in ABAP_CALLSTACK_LINE structure
    DATA ls_callstack TYPE zsaqo3_abap_callstack.
    MOVE-CORRESPONDING go_editor->mo_option->ms_db_item TO ls_callstack. "#EC ENHOK

    DATA lo_struc TYPE REF TO cl_abap_structdescr.
    lo_struc ?= cl_abap_structdescr=>describe_by_data( ls_callstack ).

    DATA lr_comp TYPE REF TO abap_compdescr.
    LOOP AT lo_struc->components REFERENCE INTO lr_comp.
      FIELD-SYMBOLS <ls_1st_call> LIKE LINE OF mt_1st_call.
      APPEND INITIAL LINE TO mt_1st_call ASSIGNING <ls_1st_call>.

      <ls_1st_call>-field = lr_comp->name.

      FIELD-SYMBOLS <lv_value> TYPE any.
      ASSIGN COMPONENT lr_comp->name OF STRUCTURE ls_callstack TO <lv_value>.
      <ls_1st_call>-value = <lv_value>.
      CONDENSE <ls_1st_call>-value.
    ENDLOOP.
  ENDMETHOD.

  METHOD _get_catalog.
    DATA lr_catalog TYPE REF TO lvc_s_fcat.
    APPEND INITIAL LINE TO rt_catalog REFERENCE INTO lr_catalog.
    lr_catalog->fieldname = 'VALUE'.
    lr_catalog->hotspot   = abap_true.
    lr_catalog->outputlen = 50.                          "#EC NUMBER_OK
  ENDMETHOD.

  METHOD _get_toolbar.
    FIELD-SYMBOLS <ls_button> LIKE LINE OF rt_toolbar.

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-text         = <ls_button>-quickinfo    = 'Scan ABAP code'(sca).
    <ls_button>-icon         = icon_extended_search.
    <ls_button>-function     = mc_button-scan.
  ENDMETHOD.

  METHOD _on_hotspot_click.
    DATA ls_db_item TYPE ztaqo_option.
    ls_db_item = go_editor->mo_option->ms_db_item.

    IF ls_db_item-mainprogram IS INITIAL.
      MESSAGE 'No previous call was found. Use code scanning'(ncl) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Try to launch by save first save of option
    zcl_aqo_helper=>navigate_to(
      iv_include  = ls_db_item-include
      iv_position = ls_db_item-line ).
  ENDMETHOD.

  METHOD _on_user_command.
    CHECK e_ucomm = mc_button-scan
      AND ms_db_key IS NOT INITIAL.

    " All usage
    DATA lt_usage TYPE zcl_aqo_helper=>tt_usage.
    lt_usage = zcl_aqo_helper=>get_usage( ms_db_key ).

    DATA ls_usage TYPE zcl_aqo_helper=>ts_usage.
    DO 1 TIMES.
      DELETE lt_usage WHERE found <> abap_true.
      CHECK lt_usage[] IS NOT INITIAL.

      IF lines( lt_usage ) = 1.
        READ TABLE lt_usage INTO ls_usage INDEX 1.
        EXIT.
      ENDIF.

      " Several creation of same option ?
      DATA lt_return TYPE STANDARD TABLE OF ddshretval.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield   = 'INDEX'
          dynpprog   = sy-repid
          dynpnr     = sy-dynnr
          value_org  = 'S'
        TABLES
          value_tab  = lt_usage
          return_tab = lt_return
        EXCEPTIONS
          OTHERS     = 3.
      IF sy-subrc <> 0 OR lt_return[] IS INITIAL.
        RETURN.
      ENDIF.

      DATA lr_return TYPE REF TO ddshretval.
      READ TABLE lt_return REFERENCE INTO lr_return INDEX 1.
      CHECK sy-subrc = 0.

      READ TABLE lt_usage INTO ls_usage WITH KEY index = lr_return->fieldval.
    ENDDO.

    IF ls_usage-include IS INITIAL OR ls_usage-line IS INITIAL.
      MESSAGE s015(zaqo_message) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Drilldown
    zcl_aqo_helper=>navigate_to(
     iv_include  = ls_usage-include
     iv_position = ls_usage-line ).
  ENDMETHOD.

ENDCLASS.


*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

MODULE pbo_074 OUTPUT.
  DATA go_usage_in_code TYPE REF TO lcl_usage_in_code.  "#EC DECL_MODUL
  IF go_usage_in_code IS INITIAL.
    CREATE OBJECT go_usage_in_code.
  ENDIF.

  go_usage_in_code->pbo( ).
ENDMODULE.

*MODULE pai_074 INPUT.
*  go_usage_in_code->pai( ).
*ENDMODULE.
