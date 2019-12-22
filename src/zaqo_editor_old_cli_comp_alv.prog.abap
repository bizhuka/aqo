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
    DATA:
      lv_ok    TYPE abap_bool,
      lv_dynnr TYPE dynpronr.

    " Field description
    ms_field_desc = is_field_desc.
    mv_editable   = iv_editable.

    " Table to show
    zcl_aqo_helper=>from_json(
     EXPORTING
      iv_json = ms_field_desc->sub_fdesc
     IMPORTING
      ev_ok   = lv_ok
      ex_data = mt_sub_fld_desc ).
    IF lv_ok <> abap_true.
      MESSAGE s017(zaqo_message) WITH ms_field_desc->name DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    mv_refresh    = abap_true.

    " Show screen
    lv_dynnr = 300 + me->mv_level - 1.
    CALL SCREEN lv_dynnr STARTING AT 5 1.
  ENDMETHOD.                    "call_screen

  METHOD pbo.
    DATA:
      lr_cont       TYPE REF TO cl_gui_custom_container,
      lt_fieldcat   TYPE lvc_t_fcat,
      ls_layout     TYPE lvc_s_layo,
      ls_variant    TYPE disvariant,
      lv_text       TYPE string VALUE '-',
      lv_name       TYPE text40,
      lt_toolbar_ex TYPE ui_functions.

*    " Own buttons
    SET PF-STATUS 'OK_CANCEL'.

    IF ms_field_desc->label IS NOT INITIAL.
      lv_text = ms_field_desc->label.
    ENDIF.
    SET TITLEBAR 'ST_MAIN' WITH lv_text.

    " Update field catalog & layout
    IF mr_grid IS INITIAL OR mv_refresh = abap_true.
      lt_fieldcat = get_field_catalog( ).

      " Prepare layout
      ls_layout-cwidth_opt = abap_true.
      ls_layout-sel_mode   = 'C'.
      " ls_layout-no_toolbar = abap_true.

      " Variant
      CONCATENATE p_pack p_opt_id INTO ls_variant-report.
      ls_variant-handle  = '0003'.
    ENDIF.

    " One time only
    IF mr_grid IS NOT INITIAL.
      IF mv_refresh = abap_true.
        " Set new catalog & layout
        mr_grid->set_frontend_fieldcatalog( lt_fieldcat ).
        mr_grid->set_frontend_layout( ls_layout ).

        mr_grid->refresh_table_display( ).
      ENDIF.
      mv_refresh = abap_false.

      RETURN.
    ENDIF.

    " Header and grid
    CONCATENATE 'EMPTY_' sy-dynnr INTO lv_name.
    CREATE OBJECT:
     lr_cont
      EXPORTING
        container_name = lv_name,

    " Show at first SCREEN
     mr_grid
      EXPORTING
        i_parent = lr_cont
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    " Add & delete new fiels
    SET HANDLER:
     on_toolbar       FOR mr_grid,
     on_user_command  FOR mr_grid,
     on_hotspot_click FOR mr_grid.

    lt_toolbar_ex = lcl_fld_value_alv=>get_exclude_toolbar( mv_editable ).
    mr_grid->set_table_for_first_display(
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_ex
      CHANGING
        it_outtab                     = mt_sub_fld_desc[]
        it_fieldcatalog               = lt_fieldcat
      EXCEPTIONS
        OTHERS                        = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "pbo

  METHOD on_toolbar.
    DATA:
      ls_toolbar TYPE stb_button.
    " Only if editable
    CHECK mv_editable = abap_true.

    ls_toolbar-function  = 'ADD_NEW_FIELD'.
    ls_toolbar-icon      = icon_insert_row.
    ls_toolbar-text      = 'Add new field'(anf).

    INSERT ls_toolbar INTO TABLE e_object->mt_toolbar.
  ENDMETHOD.

  METHOD on_user_command.
    DATA:
      lr_data         TYPE REF TO data,
      ls_sub_fld_desc TYPE ts_sub_fld_desc.
    CHECK e_ucomm = 'ADD_NEW_FIELD'.

    " Get full description
    lcl_fld_value_alv=>add_new_field(
     IMPORTING
       er_data       = lr_data
       es_field_desc = ls_sub_fld_desc-field_desc ).
    CHECK ls_sub_fld_desc-field_desc IS NOT INITIAL.

    " Already exist
    READ TABLE mt_sub_fld_desc TRANSPORTING NO FIELDS
     WITH KEY name = zsaqo_new_field-f_name.
    IF sy-subrc = 0.
      MESSAGE s002(zaqo_message) WITH zsaqo_new_field-f_name DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Just add to the end
    APPEND ls_sub_fld_desc TO mt_sub_fld_desc.

    " TODO
    mv_refresh = abap_true.
    pbo( ).
  ENDMETHOD.

  METHOD on_hotspot_click.
    DATA:
      ls_sub_fld_desc   TYPE REF TO ts_sub_fld_desc,
      lo_table_comp_alv LIKE me,
      lr_field_desc     TYPE REF TO zcl_aqo_helper=>ts_field_desc,
      lv_level          TYPE i.

    " Current item
    READ TABLE mt_sub_fld_desc REFERENCE INTO ls_sub_fld_desc INDEX es_row_no-row_id.
    CHECK sy-subrc = 0.

    CASE e_column_id.
      WHEN 'CATALOG'.
        CHECK ls_sub_fld_desc->ui_type = zcl_aqo_helper=>mc_ui_table.

        " Create new instance
        lv_level = me->mv_level + 1.
        lo_table_comp_alv = lcl_table_comp_alv=>get_instance( lv_level ).

        " Show catalog again
        GET REFERENCE OF ls_sub_fld_desc->field_desc INTO lr_field_desc.
        lo_table_comp_alv->call_screen(
         is_field_desc = lr_field_desc
         iv_editable   = mv_editable ).

    ENDCASE.
  ENDMETHOD.

  METHOD get_field_catalog.
    DATA:
      ls_fieldcat     TYPE REF TO lvc_s_fcat,
      ls_sub_fld_desc TYPE REF TO ts_sub_fld_desc.

    " Get field catalog
    zcl_aqo_helper=>create_field_catalog(
     IMPORTING
       et_fieldcat = rt_fieldcat
     CHANGING
       ct_table    = mt_sub_fld_desc[] ).

*      ls_layout-edit = abap_true.
    LOOP AT rt_fieldcat REFERENCE INTO ls_fieldcat.
      CASE ls_fieldcat->fieldname.
        WHEN 'ROLLNAME' OR 'LABEL'.
          ls_fieldcat->edit = mv_editable.

          " Hide table specific fields
        WHEN 'TABLE_KIND' OR 'UNIQUE' OR 'KEY_DEFKIND' OR 'SUB_FDESC' OR
             " Rollname has priority
             'SYS_TYPE' OR 'LENGTH' OR 'DECIMALS'.
          ls_fieldcat->tech = abap_true.

        WHEN 'CATALOG'.
          ls_fieldcat->hotspot = abap_true.
          ls_fieldcat->scrtext_s = ls_fieldcat->scrtext_m = ls_fieldcat->scrtext_l =
                 ls_fieldcat->reptext = ls_fieldcat->coltext = 'Catalog'(cat).

          " Only 1 type of icons
          ls_fieldcat->tech = abap_true.
          LOOP AT mt_sub_fld_desc REFERENCE INTO ls_sub_fld_desc.
            lcl_opt=>set_icons(
             EXPORTING
               iv_ui_type = ls_sub_fld_desc->ui_type
             IMPORTING
               ev_icon    = ls_sub_fld_desc->icon
               ev_catalog = ls_sub_fld_desc->catalog ).

            CHECK ls_sub_fld_desc->ui_type = zcl_aqo_helper=>mc_ui_table.
            ls_fieldcat->tech = abap_false.
          ENDLOOP.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD pai.
    DATA:
      lv_cmd          TYPE syucomm,
      lv_exit         TYPE abap_bool,
      " SRC
      ls_sub_fld_desc TYPE REF TO ts_sub_fld_desc,
      " DEST
      lt_field_desc   TYPE STANDARD TABLE OF zcl_aqo_helper=>ts_field_desc WITH DEFAULT KEY,
      lr_field_desc   TYPE REF TO zcl_aqo_helper=>ts_field_desc.

    " Save & clear
    lv_cmd = cv_cmd.
    CLEAR cv_cmd.

    " Write data back
    mr_grid->check_changed_data( ).

    CASE lv_cmd.
      WHEN 'OK'.
        LOOP AT mt_sub_fld_desc REFERENCE INTO ls_sub_fld_desc.
          APPEND INITIAL LINE TO lt_field_desc REFERENCE INTO lr_field_desc.
          MOVE-CORRESPONDING ls_sub_fld_desc->* TO lr_field_desc->*.
        ENDLOOP.

        ms_field_desc->sub_fdesc = zcl_aqo_helper=>to_json( lt_field_desc[] ).
        lv_exit = abap_true.
        MESSAGE s004(zaqo_message).

      WHEN 'CANCEL'.
        lv_exit = abap_true.
        MESSAGE s130(ed) WITH 'Edit'(edt) DISPLAY LIKE 'E'.
    ENDCASE.

    IF lv_exit = abap_true.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDMETHOD.                    "pai
ENDCLASS.


*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.
  go_table_comp_alv = lcl_table_comp_alv=>get_instance( ).
  go_table_comp_alv->pbo( ).
ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_0300 INPUT.
  go_table_comp_alv = lcl_table_comp_alv=>get_instance( ).
  go_table_comp_alv->pai(
   CHANGING
     cv_cmd = gv_ok_code ).
ENDMODULE.
