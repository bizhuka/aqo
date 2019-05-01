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
      lt_ret   TYPE STANDARD TABLE OF ddshretval,
      ls_ret   TYPE REF TO ddshretval,
      ls_usage TYPE zcl_aqo_helper=>ts_usage.

    " Show SH
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname    = 'ZSAQO_USER_SH'    " No need returns all fields in SH exit   "#EC NOTEXT
        fieldname  = 'INDEX'            "#EC NOTEXT
        searchhelp = 'ZHAQO_USAGE'      "#EC NOTEXT
        dynpprog   = sy-repid
        dynpnr     = sy-dynnr
      TABLES
        return_tab = lt_ret
      EXCEPTIONS
        OTHERS     = 5.
    CHECK sy-subrc = 0.

    " Write back
    LOOP AT lt_ret REFERENCE INTO ls_ret WHERE fieldname = 'INCLUDE' OR fieldname = 'LINE'.
      CASE ls_ret->fieldname.
        WHEN 'INCLUDE'.
          ls_usage-include = ls_ret->fieldval.
        WHEN 'LINE'.
          ls_usage-line = ls_ret->fieldval.
      ENDCASE.
    ENDLOOP.

    IF ls_usage-include IS INITIAL OR ls_usage-line IS INITIAL.
      MESSAGE s015(zaqo_message) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Drilldown
    zcl_aqo_helper=>navigate_to(
     iv_include  = ls_usage-include
     iv_position = ls_usage-line ).
  ENDMETHOD.

ENDCLASS.                    "IMPLEMENTATION
