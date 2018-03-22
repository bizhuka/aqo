*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_where_used IMPLEMENTATION.
  METHOD get_instance.
    IF mo_instance IS INITIAL.
      CREATE OBJECT mo_instance.
    ENDIF.
    ro_instance = mo_instance.
  ENDMETHOD.

  METHOD call_screen.
    IF go_opt->ms_last_call IS INITIAL.
      p_main = TEXT-ncl.
    ELSE.
      p_main   = go_opt->ms_last_call-mainprogram.
      p_incl   = go_opt->ms_last_call-include.
      p_line   = go_opt->ms_last_call-line.
      p_bl_typ = go_opt->ms_last_call-blocktype.
      p_bl_nam = go_opt->ms_last_call-blockname.
    ENDIF.

    CALL SELECTION-SCREEN 1010 STARTING AT 5 1.
  ENDMETHOD.                    "call_screen

  METHOD pbo.
    DATA:
      lt_exclude TYPE STANDARD TABLE OF syucomm.

    " Chaneg title
    SET TITLEBAR 'ST_MAIN' WITH TEXT-wul.

    " No previos call
    IF go_opt->ms_last_call IS INITIAL.
      APPEND 'LAST_CALL' TO lt_exclude.
    ENDIF.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = '1010'
      TABLES
        p_exclude = lt_exclude.

    " Make paramaters gray
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'PAR'.
          screen-input = '0'.

      ENDCASE.

      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.                    "pbo

  METHOD pai.
    CASE cv_cmd.
      WHEN 'CANCEL'.
        LEAVE TO SCREEN 0.

      WHEN 'LAST_CALL'.
        zcl_aqo_util=>navigate_to(
         iv_include  = p_incl
         iv_position = p_line ).

      WHEN 'SCAN'.
        deep_scan( ).
    ENDCASE.
  ENDMETHOD.                    "pai

  METHOD deep_scan.
    DATA:
      lt_usage  TYPE zcl_aqo_util=>tt_usage,
      ls_usage  TYPE REF TO zcl_aqo_util=>ts_usage,
      lt_return TYPE STANDARD TABLE OF ddshretval,
      ls_return TYPE REF TO ddshretval,
      lv_index  TYPE zcl_aqo_util=>ts_usage-index.

    " All includes
    lt_usage = zcl_aqo_util=>get_usage( iv_object    = go_opt->ms_key-object
                                        iv_subobject = go_opt->ms_key-subobject ).

    " Show all list
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield   = 'INDEX'
        dynpprog   = sy-cprog
        dynpnr     = sy-dynnr
        value_org  = 'S'
      TABLES
        value_tab  = lt_usage
        return_tab = lt_return.

    " Fisrt include
    READ TABLE lt_return REFERENCE INTO ls_return INDEX 1.
    CHECK sy-subrc = 0.

    lv_index = ls_return->fieldval.
    READ TABLE lt_usage REFERENCE INTO ls_usage
     WITH KEY index = lv_index.
    CHECK sy-subrc = 0.

    zcl_aqo_util=>navigate_to(
     iv_include  = ls_usage->include
     iv_position = ls_usage->line ).

*    MESSAGE s015(zaqo_mes) DISPLAY LIKE 'E'.
  ENDMETHOD.

ENDCLASS.                    "IMPLEMENTATION
