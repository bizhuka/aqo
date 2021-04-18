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
    ms_fld_value = is_fld_value.
**********************************************************************
    " Layout
    DATA ls_layout TYPE lvc_s_layo.
    " Can edit ?
    ls_layout-edit = lcl_opt=>is_editable( is_fld_value->is_editable ).

    IF ls_layout-edit <> abap_true.
      ls_layout-grid_title = 'View'(vew).
      ls_layout-zebra      = abap_true. " for view only
    ELSE.
      ls_layout-grid_title = 'Edit'(edt).
    ENDIF.
    CONCATENATE ls_layout-grid_title 'values of'(vof) is_fld_value->name INTO ls_layout-grid_title SEPARATED BY space.
    ls_layout-smalltitle = abap_true.
**********************************************************************
    " Variant
    DATA ls_variant TYPE disvariant.
    " Kind of program name
    CONCATENATE p_pack p_opt_id INTO ls_variant-report.

**********************************************************************
    " Field catalog
    DATA lt_catalog  TYPE lvc_t_fcat.
    DATA lt_f4_table TYPE zcl_eui_alv=>tt_f4_table.
    _get_f4_catalog( IMPORTING et_catalog  = lt_catalog
                               et_f4_table = lt_f4_table ).

**********************************************************************
    " Show by ALV manager
    DATA lo_alv TYPE REF TO zcl_eui_alv.
    CREATE OBJECT lo_alv
      EXPORTING
        ir_table       = is_fld_value->cur_value
        " grid parameters
        is_layout      = ls_layout
        is_variant     = ls_variant
        it_mod_catalog = lt_catalog.

    " Pass by reference
    DATA ls_field_desc TYPE REF TO zcl_eui_type=>ts_field_desc.
    GET REFERENCE OF is_fld_value->field_desc INTO ls_field_desc.
    lo_alv->set_field_desc( ls_field_desc ).
    lo_alv->set_f4_table( lt_f4_table ).

    lo_alv->show( io_handler = me ).
  ENDMETHOD.

  METHOD _get_f4_catalog.
    CLEAR: et_catalog,
           et_f4_table.
    CHECK lcl_opt=>mt_f4_tables IS NOT INITIAL.

    DATA lt_sub_fld  TYPE zcl_eui_type=>tt_field_desc.
    DATA lr_sub_fld  TYPE REF TO zcl_eui_type=>ts_field_desc.
    DATA ls_f4_table LIKE LINE OF et_f4_table.

    lt_sub_fld = zcl_eui_type=>get_sub_field_desc( ms_fld_value->field_desc ).
    LOOP AT lt_sub_fld REFERENCE INTO lr_sub_fld WHERE f4_table IS NOT INITIAL. "#EC CI_HASHSEQ
      " From current values
      DATA lr_fld_value TYPE REF TO lcl_opt=>ts_fld_value.
      READ TABLE lcl_opt=>mt_fld_value REFERENCE INTO lr_fld_value
       WITH KEY name = lr_sub_fld->f4_table.
      CHECK sy-subrc = 0.

      " Data
      ls_f4_table-field = lr_sub_fld->name.
      ls_f4_table-tab   = lr_fld_value->cur_value.
      INSERT ls_f4_table INTO TABLE et_f4_table.

      " Preapre catalog
      DATA lr_catalog TYPE REF TO lvc_s_fcat.
      APPEND INITIAL LINE TO et_catalog REFERENCE INTO lr_catalog.
      lr_catalog->fieldname  = lr_sub_fld->name.
      lr_catalog->f4availabl = abap_true.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
