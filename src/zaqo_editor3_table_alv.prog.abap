*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_table_alv DEFINITION INHERITING FROM lcl_tab FINAL FRIENDS zcl_eui_event_caller.
  PUBLIC SECTION.
    METHODS:
      constructor
       IMPORTING
         ir_fld_value TYPE REF TO lcl_editor=>ts_fld_value.

  PROTECTED SECTION.
    DATA:
      mr_fld_value TYPE REF TO lcl_editor=>ts_fld_value,
      mt_f4_table  TYPE zcl_eui_alv=>tt_f4_table.

    METHODS:
      _fill_table       REDEFINITION,
      _create_alv       REDEFINITION,
      _get_layout       REDEFINITION,
      _get_status       REDEFINITION,
      _get_catalog      REDEFINITION,
      _on_toolbar       REDEFINITION,

      _get_f4_catalog
        EXPORTING
          et_catalog  TYPE lvc_t_fcat
          et_f4_table TYPE zcl_eui_alv=>tt_f4_table.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_table_alv IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mr_fld_value = ir_fld_value.
  ENDMETHOD.

  METHOD _fill_table.
    mr_table = mr_fld_value->cur_value.
  ENDMETHOD.

  METHOD _create_alv.
    super->_create_alv( iv_for_field = iv_for_field ).

    " Pass by reference
    DATA ls_field_desc TYPE REF TO zcl_eui_type=>ts_field_desc.
    GET REFERENCE OF mr_fld_value->field_desc INTO ls_field_desc.
    mo_alv->set_field_desc( ls_field_desc ).

    mo_alv->set_f4_table( mt_f4_table ). " TODO test  update !!!!!!!!!!!!!!
  ENDMETHOD.

  METHOD _get_layout.
    rs_layout = super->_get_layout( ).
    rs_layout-edit = go_editor->is_editable( mr_fld_value->is_editable ).

    IF rs_layout-edit <> abap_true.
      rs_layout-grid_title = 'View'(vew).
      rs_layout-zebra      = abap_true. " for view only
    ELSE.
      rs_layout-grid_title = 'Edit'(edt).
    ENDIF.
    CONCATENATE rs_layout-grid_title 'values of'(vof) mr_fld_value->name INTO rs_layout-grid_title SEPARATED BY space.
  ENDMETHOD.

  METHOD _get_status.
    CONCATENATE 'Filed'(fld) mr_fld_value->label INTO rs_status-title SEPARATED BY space.
  ENDMETHOD.

  METHOD _get_catalog.
    _get_f4_catalog( IMPORTING et_catalog  = rt_catalog
                               et_f4_table = mt_f4_table ).
  ENDMETHOD.

  METHOD _get_f4_catalog.
    CLEAR: et_catalog,
           et_f4_table.
    CHECK go_editor->mt_f4_tables IS NOT INITIAL.

    DATA lt_sub_fld  TYPE zcl_eui_type=>tt_field_desc.
    DATA lr_sub_fld  TYPE REF TO zcl_eui_type=>ts_field_desc.
    DATA ls_f4_table LIKE LINE OF et_f4_table.

    lt_sub_fld = zcl_eui_type=>get_sub_field_desc( mr_fld_value->field_desc ).
    LOOP AT lt_sub_fld REFERENCE INTO lr_sub_fld WHERE f4_table IS NOT INITIAL. "#EC CI_HASHSEQ
      " From current values
      DATA lr_fld_value TYPE REF TO lcl_editor=>ts_fld_value.
      READ TABLE go_editor->mt_fld_value REFERENCE INTO lr_fld_value
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

  METHOD _on_toolbar.
  ENDMETHOD.
ENDCLASS.
