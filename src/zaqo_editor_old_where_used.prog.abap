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
    IF lcl_opt=>mo_option->ms_db_item-mainprogram IS INITIAL.
      p_main = 'No previous call was found'(ncl).
    ELSE.
      p_main   = lcl_opt=>mo_option->ms_db_item-mainprogram.
      p_incl   = lcl_opt=>mo_option->ms_db_item-include.
      p_line   = lcl_opt=>mo_option->ms_db_item-line.
      p_bl_typ = lcl_opt=>mo_option->ms_db_item-blocktype.
      p_bl_nam = lcl_opt=>mo_option->ms_db_item-blockname.
    ENDIF.

    CALL SELECTION-SCREEN 1010 STARTING AT 5 1.
  ENDMETHOD.                    "call_screen

  METHOD pbo.
    DATA:
      lt_exclude TYPE STANDARD TABLE OF syucomm.

    " Chaneg title
    SET TITLEBAR 'ST_MAIN' WITH 'Where-Used List'(wul).

    " No previos call
    IF lcl_opt=>mo_option->ms_db_item-mainprogram IS INITIAL.
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
        zcl_aqo_helper=>navigate_to(
         iv_include  = p_incl
         iv_position = p_line ).

      WHEN 'SCAN'.
        deep_scan( ).
    ENDCASE.
  ENDMETHOD.                    "pai

  METHOD deep_scan.
    DATA:
      lt_usage  TYPE zcl_aqo_helper=>tt_usage,
      ls_usage  TYPE REF TO zcl_aqo_helper=>ts_usage,
      lt_return TYPE STANDARD TABLE OF ddshretval,
      ls_return TYPE REF TO ddshretval,
      lv_index  TYPE zcl_aqo_helper=>ts_usage-index.

    " All includes
    lt_usage = zcl_aqo_helper=>get_usage( iv_package = lcl_opt=>mo_option->ms_db_item-package_id
                                          iv_option  = lcl_opt=>mo_option->ms_db_item-option_id ).

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

    zcl_aqo_helper=>navigate_to(
     iv_include  = ls_usage->include
     iv_position = ls_usage->line ).

*    MESSAGE s015(zaqo_message) DISPLAY LIKE 'E'.
  ENDMETHOD.

ENDCLASS.                    "IMPLEMENTATION
