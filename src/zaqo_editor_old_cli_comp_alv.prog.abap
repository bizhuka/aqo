*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_table_comp_alv IMPLEMENTATION.
  METHOD get_instance.
    IF iv_level IS INITIAL.
      iv_level = sy-dynnr - 300 + 1.
    ENDIF.

    ro_instance ?= lcl_nested_instance=>get_instance_by_level(
       iv_cl_name = 'LCL_TABLE_COMP_ALV'
       iv_level   = iv_level ).
  ENDMETHOD.                    "get_instance

  METHOD call_screen.
    " Field description
    ms_field_desc = is_field_desc.

    " Can edit ?
    DATA lv_read_only TYPE abap_bool.
    mv_editable   = iv_editable.
    IF mv_editable <> abap_true.
      lv_read_only = abap_true.
    ENDIF.

    " Table to show
    DATA lv_ok TYPE abap_bool.
    zcl_eui_conv=>from_json(
     EXPORTING
      iv_json = ms_field_desc->sub_fdesc
     IMPORTING
      ev_ok   = lv_ok
      ex_data = mt_sub_fld_desc ).
    IF lv_ok <> abap_true.
      MESSAGE s017(zaqo_message) WITH ms_field_desc->name DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

**********************************************************************
    " Main table
    DATA lr_table TYPE REF TO data.
    GET REFERENCE OF mt_sub_fld_desc INTO lr_table.

**********************************************************************
    " Prepare layout
    DATA ls_layout TYPE lvc_s_layo.
    " ls_layout-no_toolbar = abap_true.
    CONCATENATE `Field catalog of ` ms_field_desc->name INTO ls_layout-grid_title.
    ls_layout-smalltitle = abap_true.

**********************************************************************
    " Variant
    DATA ls_variant TYPE disvariant.
    CONCATENATE p_pack p_opt_id INTO ls_variant-report.
    ls_variant-handle  = '0003'.

**********************************************************************
    " Get field catalog
    DATA lt_fieldcat     TYPE lvc_t_fcat.
    DATA ls_fieldcat     TYPE REF TO lvc_s_fcat.
    DATA ls_sub_fld_desc TYPE REF TO ts_sub_fld_desc.

    " Editable fields
    add_fcat_field '+' ''.
    ls_fieldcat->edit = mv_editable.
    add_fcat_field '+ROLLNAME' ''.    add_fcat_field '+LABEL' ''.

    " Hide table specific fields
    add_fcat_field '+' ''.
    ls_fieldcat->tech = abap_true.
    add_fcat_field '+TABLE_KIND' ''.  add_fcat_field '+UNIQUE' ''.  add_fcat_field '+KEY_DEFKIND' ''.   add_fcat_field '+SUB_FDESC' ''.
    add_fcat_field '+SYS_TYPE' ''.    add_fcat_field '+LENGTH' ''.  add_fcat_field '+DECIMALS' ''.

    " Icon of catalog
    add_fcat_field 'CATALOG' 'Catalog'(cat).
    ls_fieldcat->hotspot = abap_true.

    " Only 1 type of icons
    ls_fieldcat->tech = abap_true.
    LOOP AT mt_sub_fld_desc REFERENCE INTO ls_sub_fld_desc.
      lcl_opt=>set_icons(
       EXPORTING
         iv_ui_type = ls_sub_fld_desc->ui_type
       IMPORTING
         ev_icon    = ls_sub_fld_desc->icon
         ev_catalog = ls_sub_fld_desc->catalog ).

      CHECK ls_sub_fld_desc->ui_type = zcl_eui_type=>mc_ui_type-table.
      ls_fieldcat->tech = abap_false.
    ENDLOOP.

**********************************************************************
    " Toolbar
    DATA lt_toolbar TYPE ttb_button.
    DATA ls_toolbar TYPE stb_button.

    " Only if editable
    IF mv_editable = abap_true.
      ls_toolbar-function  = 'CHANGE_KEY'.
      ls_toolbar-icon      = icon_foreign_key.
      ls_toolbar-text      = 'Change key'(chk).
      INSERT ls_toolbar INTO TABLE lt_toolbar.

      ls_toolbar-function  = 'ADD_NEW_FIELD'.
      ls_toolbar-icon      = icon_insert_row.
      ls_toolbar-text      = 'Add new field'(anf).
      INSERT ls_toolbar INTO TABLE lt_toolbar.
    ENDIF.

**********************************************************************
    " Show by ALV manager
**********************************************************************
    DATA lo_eui_alv TYPE REF TO zif_eui_manager.
    DATA ls_status  TYPE REF TO lo_eui_alv->ts_status.

    " Pass by reference
    CREATE OBJECT lo_eui_alv TYPE zcl_eui_alv
      EXPORTING
        ir_table       = lr_table
        " grid parameters
        is_layout      = ls_layout
        it_mod_catalog = lt_fieldcat
        it_toolbar     = lt_toolbar
        iv_read_only   = lv_read_only.

    " Instead of set handler
    lo_eui_alv->popup( ).

    " Static PF status no need on_pbo_event.
    GET REFERENCE OF lo_eui_alv->ms_status INTO ls_status.
    ls_status->is_fixed = abap_true.
    ls_status->title    = ms_field_desc->label.

    lo_eui_alv->show( io_handler = me ).
  ENDMETHOD.                    "call_screen

  METHOD on_toolbar.
    go_fld_value_alv = lcl_fld_value_alv=>get_instance( ).
    go_fld_value_alv->set_exclude_toolbar(
     EXPORTING
      iv_editable = mv_editable
     CHANGING
      ct_toolbar  = e_object->mt_toolbar ).
  ENDMETHOD.

  METHOD on_user_command.
    DATA lr_data         TYPE REF TO data.
    DATA ls_sub_fld_desc TYPE ts_sub_fld_desc.
    DATA lv_fname        TYPE zcl_eui_type=>ts_field_desc-name.

    CASE e_ucomm.

        " Add field to field catalog
      WHEN 'ADD_NEW_FIELD'.
        " Get full description
        go_fld_value_alv = lcl_fld_value_alv=>get_instance( ).
        go_fld_value_alv->add_new_field(
         IMPORTING
           er_data       = lr_data
           es_field_desc = ls_sub_fld_desc-field_desc ).
        CHECK ls_sub_fld_desc-field_desc IS NOT INITIAL.

        " Already exist
        lv_fname = ls_sub_fld_desc-field_desc-name.
        READ TABLE mt_sub_fld_desc TRANSPORTING NO FIELDS
         WITH KEY name = lv_fname.
        IF sy-subrc = 0.
          MESSAGE s002(zaqo_message) WITH lv_fname DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        " Just add to the end
        APPEND ls_sub_fld_desc TO mt_sub_fld_desc.
        sender->refresh_table_display( ).

      WHEN 'CHANGE_KEY'.
        change_key( ).

    ENDCASE.
  ENDMETHOD.

  METHOD change_key.
    " Show in screen
    DATA ls_dyn_scr  TYPE REF TO zsaqo_table_key_dialog. " PARAMETERS & SELECT-OPTIONS
    DATA lo_screen   TYPE REF TO zcl_eui_screen.
    DATA lo_err      TYPE REF TO cx_root.
    DATA lr_key_desr TYPE REF TO abap_keydescr.
    DATA ls_key_desr TYPE abap_keydescr.
    DATA ls_key      LIKE LINE OF s_4_key.

    " Where to store data
    CREATE DATA ls_dyn_scr.
    ls_dyn_scr->p_4_kind = ms_field_desc->table_kind.
    ls_dyn_scr->p_4_unq  = ms_field_desc->unique.
    ls_dyn_scr->p_4_keyd = ms_field_desc->key_defkind.
    LOOP AT ms_field_desc->key REFERENCE INTO lr_key_desr.
      ls_key-sign   = 'I'.
      ls_key-option = 'EQ'.
      ls_key-low    = lr_key_desr->name.
      APPEND ls_key TO ls_dyn_scr->s_4_key[].
    ENDLOOP.

    " Create screen manager
    TRY.
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr   = '1040'
            iv_cprog   = sy-cprog
            ir_context = ls_dyn_scr.
      CATCH zcx_eui_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

*    " Static PF status no need on_pbo_event.
    lo_screen->customize( iv_fieldname = 'P_4_KIND' required = '1' ).
    lo_screen->customize( iv_fieldname = 'P_4_KEYD' required = '1' ). " input = '0' ?

    " Set text
    lo_screen->ms_status-is_fixed = abap_true.
    lo_screen->ms_status-title = 'Change declaration and run again is much easier!'(010).

    " Ok & Cancel
    IF mv_editable <> abap_true.
      APPEND 'OK' TO lo_screen->ms_status-exclude.
    ENDIF.

    " As popup
    lo_screen->popup( iv_col_beg  = 1
                       iv_row_beg  = 1
                       iv_col_end  = 118
                       iv_row_end  = 30 ).

    " Check OK pressed
    CHECK lo_screen->show( ) = 'OK'.

    " Copy back
    ms_field_desc->table_kind  = ls_dyn_scr->p_4_kind.
    ms_field_desc->unique      = ls_dyn_scr->p_4_unq.
    ms_field_desc->key_defkind = ls_dyn_scr->p_4_keyd.
    " Field by field
    CLEAR ms_field_desc->key.
    LOOP AT ls_dyn_scr->s_4_key INTO ls_key.
      ls_key_desr-name = ls_key-low.
      INSERT ls_key_desr INTO TABLE ms_field_desc->key.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_hotspot_click.
    DATA:
      ls_sub_fld_desc   TYPE REF TO ts_sub_fld_desc,
      lo_table_comp_alv LIKE me,
      lr_field_desc     TYPE REF TO zcl_eui_type=>ts_field_desc,
      lv_level          TYPE i.

    " Current item
    READ TABLE mt_sub_fld_desc REFERENCE INTO ls_sub_fld_desc INDEX es_row_no-row_id.
    CHECK sy-subrc = 0.

    CASE e_column_id.
      WHEN 'CATALOG'.
        CHECK ls_sub_fld_desc->ui_type = zcl_eui_type=>mc_ui_type-table.

        " Create new instance
        lv_level = me->mv_level + 1.
        lo_table_comp_alv = lcl_table_comp_alv=>get_instance( lv_level ). " lv_level

        " Show catalog again
        GET REFERENCE OF ls_sub_fld_desc->field_desc INTO lr_field_desc.
        lo_table_comp_alv->call_screen(
         is_field_desc = lr_field_desc
         iv_editable   = mv_editable ).

    ENDCASE.
  ENDMETHOD.

  METHOD on_pai_event.
    DATA:
      lv_cmd          TYPE syucomm,
      " SRC
      ls_sub_fld_desc TYPE REF TO ts_sub_fld_desc,
      " DEST
      lt_field_desc   TYPE STANDARD TABLE OF zcl_eui_type=>ts_field_desc WITH DEFAULT KEY,
      lr_field_desc   TYPE REF TO zcl_eui_type=>ts_field_desc.

    " Save & clear
    lv_cmd = iv_command.

    CASE lv_cmd.
      WHEN zif_eui_manager=>mc_cmd-ok.
        LOOP AT mt_sub_fld_desc REFERENCE INTO ls_sub_fld_desc.
          APPEND INITIAL LINE TO lt_field_desc REFERENCE INTO lr_field_desc.
          MOVE-CORRESPONDING ls_sub_fld_desc->* TO lr_field_desc->*.
        ENDLOOP.

        ms_field_desc->sub_fdesc = zcl_eui_conv=>to_json( lt_field_desc[] ).
        MESSAGE s004(zaqo_message).

      WHEN zif_eui_manager=>mc_cmd-cancel.
        MESSAGE s130(ed) WITH 'Edit'(edt) DISPLAY LIKE 'E'.
    ENDCASE.
  ENDMETHOD.                    "pai
ENDCLASS.
