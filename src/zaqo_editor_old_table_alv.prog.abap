*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_table_alv IMPLEMENTATION.
  METHOD get_instance.
    IF iv_level IS INITIAL.
      iv_level = sy-dynnr - 200 + 1.
    ENDIF.

    ro_instance ?= lcl_nested_instance=>get_instance_by_level(
       iv_cl_name = 'LCL_TABLE_ALV'
       iv_level   = iv_level ).
  ENDMETHOD.

  METHOD call_screen.
**********************************************************************
    " Layout
    DATA ls_layout TYPE lvc_s_layo.
    DATA lv_read_only TYPE abap_bool.

    " Can edit ?
    ls_layout-edit = lcl_opt=>is_editable( is_fld_value->is_editable ).
    IF ls_layout-edit <> abap_true.
      lv_read_only = abap_true.
    ENDIF.

    CONCATENATE `Edit values of ` is_fld_value->name INTO ls_layout-grid_title.
    ls_layout-smalltitle = abap_true.

**********************************************************************
    " Variant
    DATA ls_variant TYPE disvariant.
    " Kind of program name
    CONCATENATE p_pack p_opt_id INTO ls_variant-report.

**********************************************************************
    " Show by ALV manager
**********************************************************************
    DATA lo_eui_alv TYPE REF TO zcl_eui_alv.
    DATA ls_field_desc TYPE REF TO zcl_eui_type=>ts_field_desc.

    " Pass by reference
    GET REFERENCE OF is_fld_value->field_desc INTO ls_field_desc.
    CREATE OBJECT lo_eui_alv
      EXPORTING
        ir_table      = is_fld_value->cur_value
        " grid parameters
        is_layout     = ls_layout
        is_variant    = ls_variant
        is_field_desc = ls_field_desc
        iv_read_only  = lv_read_only.

    lo_eui_alv->show( ).
  ENDMETHOD.
ENDCLASS.
