*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_history_logs DEFINITION INHERITING FROM lcl_tab FINAL FRIENDS zcl_eui_event_caller.
  PUBLIC SECTION.
    METHODS:
      constructor
       IMPORTING
         ir_fld_value TYPE REF TO lcl_editor=>ts_fld_value.

  PROTECTED SECTION.
    DATA:
      mr_fld_copy TYPE REF TO lcl_editor=>ts_fld_value.

    METHODS:
      _fill_table       REDEFINITION,
      _get_layout       REDEFINITION,
      _get_status       REDEFINITION,
      _get_catalog      REDEFINITION,
      _on_hotspot_click REDEFINITION.
ENDCLASS.


CLASS lcl_history_logs IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    CREATE DATA mr_fld_copy.
    mr_fld_copy->* = ir_fld_value->*.
    mr_fld_copy->is_editable = abap_undefined.
  ENDMETHOD.

  METHOD _fill_table.
    DATA ls_history_value TYPE zcl_aqo_helper=>ts_history_value.
    DATA lt_field_desc    TYPE zcl_eui_type=>tt_field_desc.
    DATA ls_field_desc    LIKE LINE OF lt_field_desc.
    DATA ls_field_desc_ui LIKE LINE OF lt_field_desc.
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
        ls_field_desc = mr_fld_copy->field_desc.
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
        FIELD-SYMBOLS: <ls_item> TYPE any.

        DATA: lo_struc TYPE REF TO cl_abap_structdescr,
              lr_type  TYPE REF TO data,
              lo_error TYPE REF TO zcx_eui_exception.
        lo_struc = zcl_eui_type=>create_structure( it_field_desc = lt_field_desc ).
        CREATE DATA lr_type TYPE HANDLE lo_struc.
        ASSIGN lr_type->* TO <ls_item>.
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Main table
    FIELD-SYMBOLS <lt_hist_table> TYPE STANDARD TABLE.
    CREATE DATA mr_table LIKE STANDARD TABLE OF <ls_item>.
    ASSIGN mr_table->* TO <lt_hist_table>.

    " Fill table
    DATA lr_history_value TYPE REF TO zcl_aqo_helper=>ts_history_value.
    LOOP AT mr_fld_copy->value REFERENCE INTO lr_history_value.
      CLEAR <ls_item>.
      MOVE-CORRESPONDING lr_history_value->* TO <ls_item>.

      " From json
      FIELD-SYMBOLS <lv_value> TYPE any.
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
       ir_table     = mr_table
       is_sub_field = ls_field_desc ).
    ENDIF.
  ENDMETHOD.

  METHOD _get_layout.
    rs_layout = super->_get_layout( ).
    CONCATENATE 'View logs of'(vlo) mr_fld_copy->name INTO rs_layout-grid_title SEPARATED BY space.
  ENDMETHOD.

  METHOD _get_status.
    CONCATENATE 'Filed'(fld) mr_fld_copy->label INTO rs_status-title SEPARATED BY space.
  ENDMETHOD.

  METHOD _get_catalog.
    DATA ls_fieldcat TYPE REF TO lvc_s_fcat.
    APPEND INITIAL LINE TO rt_catalog REFERENCE INTO ls_fieldcat.
    ls_fieldcat->fieldname = '_VALUE_UI'.
    ls_fieldcat->hotspot   = abap_true.
  ENDMETHOD.

  METHOD _on_hotspot_click.
    FIELD-SYMBOLS:
      <lt_hist_table> TYPE STANDARD TABLE,
      <ls_hist_table> TYPE any,
      <mv_value>      TYPE any.

    ASSIGN mr_table->* TO <lt_hist_table>.
    CHECK sy-subrc = 0.

    " Current item
    READ TABLE <lt_hist_table> ASSIGNING <ls_hist_table> INDEX e_row_id-index.
    CHECK sy-subrc = 0.

    " Get reafarence to data (mr_fld_copy is copy to data)
    ASSIGN COMPONENT '_VALUE' OF STRUCTURE <ls_hist_table> TO <mv_value>.
    GET REFERENCE OF <mv_value> INTO mr_fld_copy->cur_value.

    CASE mr_fld_copy->ui_type.
        " Change logs of table
      WHEN zcl_eui_type=>mc_ui_type-table.
        DATA lo_table_alv TYPE REF TO lcl_table_alv.
        CREATE OBJECT lo_table_alv
          EXPORTING
            ir_fld_value = mr_fld_copy.
        lo_table_alv->show( ).

        " Change logs of range
      WHEN zcl_eui_type=>mc_ui_type-range.
*        IF go_editor->is_editable( mr_fld_copy->is_editable ) <> abap_true.
*          lv_read_only = abap_true.
*        ENDIF.
        zcl_eui_screen=>show_range(
          is_field_desc = mr_fld_copy->field_desc
          ir_cur_value  = mr_fld_copy->cur_value
          iv_read_only  = abap_true ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
