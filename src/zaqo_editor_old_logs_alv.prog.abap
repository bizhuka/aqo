*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_logs_alv IMPLEMENTATION.
  METHOD get_instance.
    IF mo_instance IS INITIAL.
      CREATE OBJECT mo_instance.
    ENDIF.
    ro_instance = mo_instance.
  ENDMETHOD.

  METHOD call_screen.
    DATA:
      ls_history_value TYPE zcl_aqo_helper=>ts_history_value,
      lr_history_value TYPE REF TO zcl_aqo_helper=>ts_history_value,
      lt_field_desc    TYPE zcl_eui_type=>tt_field_desc,
      ls_field_desc    LIKE LINE OF lt_field_desc,
      ls_field_desc_ui LIKE LINE OF lt_field_desc,
      lo_struc         TYPE REF TO cl_abap_structdescr,
      lr_type          TYPE REF TO data,
      lo_error         TYPE REF TO zcx_eui_exception.
    FIELD-SYMBOLS:
      <ls_item>       TYPE any,
      <lv_value>      TYPE any,
      <lt_hist_table> TYPE STANDARD TABLE.

    " Make copy
    CREATE DATA ms_fld_value.
    ms_fld_value->*           = is_fld_value->*.
    ms_fld_value->is_editable = abap_undefined.

    TRY.
        " 1-st
        ls_field_desc = zcl_eui_type=>get_field_desc(
           iv_data       = ls_history_value-changed
           iv_field_name = 'CHANGED' ).
        INSERT ls_field_desc INTO TABLE lt_field_desc.

        " 2-nd
        ls_field_desc = zcl_eui_type=>get_field_desc(
           iv_data       = ls_history_value-login
           iv_field_name = 'LOGIN' ).
        INSERT ls_field_desc INTO TABLE lt_field_desc.

        " 3-rd
        ls_field_desc = ms_fld_value->field_desc.
        ls_field_desc-name = '_VALUE'.
        INSERT ls_field_desc INTO TABLE lt_field_desc.

        " 4-th
        IF ls_field_desc-ui_type <> zcl_eui_type=>mc_ui_type-range AND
           ls_field_desc-ui_type <> zcl_eui_type=>mc_ui_type-table.
          CLEAR ls_field_desc.
        ELSE.
          ls_field_desc_ui = zcl_eui_type=>get_field_desc(
             iv_data       = ls_history_value-h_value
             iv_field_name = '_VALUE_UI' ).
          INSERT ls_field_desc_ui INTO TABLE lt_field_desc.
        ENDIF.

        " Create structure
        lo_struc = zcl_eui_type=>create_structure( it_field_desc = lt_field_desc ).
        CREATE DATA lr_type TYPE HANDLE lo_struc.
        ASSIGN lr_type->* TO <ls_item>.
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Main table
    CREATE DATA mr_hist_table LIKE STANDARD TABLE OF <ls_item>.
    ASSIGN mr_hist_table->* TO <lt_hist_table>.

    " Fill table
    LOOP AT ms_fld_value->value REFERENCE INTO lr_history_value.
      CLEAR <ls_item>.
      MOVE-CORRESPONDING lr_history_value->* TO <ls_item>.

      " From json
      ASSIGN COMPONENT '_VALUE' OF STRUCTURE <ls_item> TO <lv_value>.
      zcl_eui_conv=>from_json(
       EXPORTING
        iv_json = lr_history_value->h_value
        iv_mode = zcl_eui_conv=>mc_json_mode-safe
       IMPORTING
        ex_data = <lv_value> ).

      " Newest on the top
      INSERT <ls_item> INTO <lt_hist_table> INDEX 1.
    ENDLOOP.

    " Table or range
    IF ls_field_desc IS NOT INITIAL.
      zcl_eui_alv=>update_complex_fields(
       ir_table     = mr_hist_table
       is_sub_field = ls_field_desc ).
    ENDIF.

**********************************************************************
    " Layout
**********************************************************************
    DATA ls_layout TYPE lvc_s_layo.
    CONCATENATE 'View logs of'(clo) ms_fld_value->name INTO ls_layout-grid_title SEPARATED BY space.
    ls_layout-smalltitle = abap_true.

**********************************************************************
    " Catalog
**********************************************************************
    DATA lt_fieldcat TYPE lvc_t_fcat.
    DATA ls_fieldcat TYPE REF TO lvc_s_fcat.
    APPEND INITIAL LINE TO lt_fieldcat REFERENCE INTO ls_fieldcat.
    ls_fieldcat->fieldname = '_VALUE_UI'.
    ls_fieldcat->hotspot = abap_true.

**********************************************************************
    " Show by ALV manager
**********************************************************************
    DATA lo_eui_alv TYPE REF TO zif_eui_manager.
    DATA ls_status  TYPE REF TO lo_eui_alv->ts_status.

    " Pass by reference
    CREATE OBJECT lo_eui_alv TYPE zcl_eui_alv
      EXPORTING
        ir_table       = mr_hist_table
        " grid parameters
        is_layout      = ls_layout
        it_mod_catalog = lt_fieldcat.

    " In popup
    lo_eui_alv->popup( ).

    " Static PF status no need on_pbo_event.
    GET REFERENCE OF lo_eui_alv->ms_status INTO ls_status.
    ls_status->is_fixed = abap_true.
    IF ms_fld_value->label IS NOT INITIAL.
      ls_status->title = ms_fld_value->label.
    ENDIF.

    " 1 button left = CANCEL
    APPEND 'OK' TO ls_status->exclude.

    " Instead of set handler
    lo_eui_alv->show( io_handler = me ).
  ENDMETHOD.                    "call_screen

  METHOD on_hotspot_click.
    DATA:
      lv_read_only    TYPE abap_bool.
    FIELD-SYMBOLS:
      <lt_hist_table> TYPE STANDARD TABLE,
      <ls_hist_table> TYPE any,
      <mv_value>      TYPE any.

    ASSIGN mr_hist_table->* TO <lt_hist_table>.
    CHECK sy-subrc = 0.

    " Current item
    READ TABLE <lt_hist_table> ASSIGNING <ls_hist_table> INDEX es_row_no-row_id.
    CHECK sy-subrc = 0.

    " Get reafarence to data (ms_fld_value is copy to data)
    ASSIGN COMPONENT '_VALUE' OF STRUCTURE <ls_hist_table> TO <mv_value>.
    GET REFERENCE OF <mv_value> INTO ms_fld_value->cur_value.

    CASE ms_fld_value->ui_type.
        " Change logs of table
      WHEN zcl_eui_type=>mc_ui_type-table.
        go_table_alv = lcl_table_alv=>get_instance( 6 ). " 6
        go_table_alv->call_screen( ms_fld_value ).

        " Change logs of range
      WHEN zcl_eui_type=>mc_ui_type-range.
        IF lcl_opt=>is_editable( ms_fld_value->is_editable ) <> abap_true.
          lv_read_only = abap_true.
        ENDIF.
        zcl_eui_screen=>show_range(
          is_field_desc = ms_fld_value->field_desc
          ir_cur_value  = ms_fld_value->cur_value
          iv_read_only  = lv_read_only ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
