*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_table_comp_alv DEFINITION INHERITING FROM lcl_tab FINAL FRIENDS zcl_eui_event_caller.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          ir_field_desc TYPE REF TO zcl_eui_type=>ts_field_desc
          iv_editable   TYPE abap_bool.

  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF mc_button,
        "delete_field  TYPE syucomm VALUE 'DELETE_FIELD',
        add_new_field TYPE syucomm VALUE 'ADD_NEW_FIELD',
        change_key    TYPE syucomm VALUE 'CHANGE_KEY',
      END OF mc_button.

    TYPES:
      BEGIN OF ts_sub_fld_desc,
        icon    TYPE icon_d.
        INCLUDE TYPE zcl_eui_type=>ts_field_desc AS field_desc.
      TYPES:
        catalog TYPE icon_d,
      END OF ts_sub_fld_desc,
      tt_sub_fld_desc TYPE STANDARD TABLE OF ts_sub_fld_desc WITH DEFAULT KEY.

    DATA:
      mr_field_desc   TYPE REF TO zcl_eui_type=>ts_field_desc,
      mt_sub_fld_desc TYPE tt_sub_fld_desc,
      mv_editable     TYPE abap_bool.

    METHODS:
      _fill_table       REDEFINITION,
      _get_layout       REDEFINITION,
      _get_status       REDEFINITION,
      _get_catalog      REDEFINITION,
      _get_toolbar      REDEFINITION,
      _on_user_command  REDEFINITION,
      _on_hotspot_click REDEFINITION,

      _change_key,

      _on_pbo_event FOR EVENT pbo_event OF zif_eui_manager  "#EC CALLED
        IMPORTING
          sender,

      _on_pai_event FOR EVENT pai_event OF zif_eui_manager  "#EC CALLED
        IMPORTING
          iv_command.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_table_comp_alv IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mr_field_desc = ir_field_desc.
    mv_editable   = iv_editable.
  ENDMETHOD.

  METHOD _fill_table.
    CLEAR mt_sub_fld_desc.
    GET REFERENCE OF mt_sub_fld_desc INTO mr_table.

    " Table to show
    DATA lt_sub_fld TYPE zcl_eui_type=>tt_field_desc.
    lt_sub_fld = zcl_eui_type=>get_sub_field_desc( mr_field_desc->* ).
    CHECK lt_sub_fld IS NOT INITIAL.

    FIELD-SYMBOLS <ls_sub_fld> LIKE LINE OF lt_sub_fld.
    DATA ls_sub_fld_desc TYPE REF TO ts_sub_fld_desc.
    LOOP AT lt_sub_fld ASSIGNING <ls_sub_fld>.
      APPEND INITIAL LINE TO mt_sub_fld_desc REFERENCE INTO ls_sub_fld_desc.
      MOVE-CORRESPONDING <ls_sub_fld> TO ls_sub_fld_desc->*.

      go_editor->set_icons( EXPORTING is_field_desc = ls_sub_fld_desc->field_desc
                            IMPORTING ev_icon       = ls_sub_fld_desc->icon
                                      ev_catalog    = ls_sub_fld_desc->catalog ).
    ENDLOOP.
  ENDMETHOD.

  METHOD _get_layout.
    rs_layout = super->_get_layout( ).
    " rs_layout-no_toolbar = abap_true.
    CONCATENATE 'Field catalog of'(fco) mr_field_desc->name INTO rs_layout-grid_title SEPARATED BY space.
  ENDMETHOD.

  METHOD _get_status.
    CONCATENATE 'Filed'(fld) mr_field_desc->label INTO rs_status-title SEPARATED BY space.
  ENDMETHOD.

  METHOD _get_catalog.
    DATA lr_catalog TYPE REF TO lvc_s_fcat.

    " Editable fields
    add_fcat_field '+' ''.
    lr_catalog->edit = mv_editable.
    add_fcat_field '+ROLLNAME' ''. add_fcat_field '+LABEL'    ''.

    add_fcat_field 'UI_TYPE'  ''.
    lr_catalog->edit      = mv_editable.
    lr_catalog->ref_table = 'ZSAQO_F4'.
    lr_catalog->ref_field = 'UI_TYPE'.

    " has f4 tables?
    LOOP AT go_editor->mt_f4_tables TRANSPORTING NO FIELDS WHERE int_value <> mr_field_desc->name.
      add_fcat_field 'F4_TABLE' ''.
      lr_catalog->edit       = mv_editable.
      lr_catalog->drdn_hndl  = 154.                      "#EC NUMBER_OK
      lr_catalog->drdn_alias = abap_true.
      EXIT.
    ENDLOOP.

    " Hide table specific fields
    add_fcat_field '+' ''.
    lr_catalog->tech = abap_true.
    add_fcat_field '+TABLE_KIND' ''.  add_fcat_field '+UNIQUE' ''.  add_fcat_field '+KEY_DEFKIND' ''.   add_fcat_field '+SUB_FDESC' ''.
    add_fcat_field '+SYS_TYPE' ''.    add_fcat_field '+LENGTH' ''.  add_fcat_field '+DECIMALS' ''.

    " Icon of catalog
    add_fcat_field 'CATALOG' 'DDIC or Catalog'(cat).
    lr_catalog->hotspot = abap_true.
  ENDMETHOD.

  METHOD _get_toolbar.
    FIELD-SYMBOLS <ls_button> LIKE LINE OF rt_toolbar.

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    " Use standard ALV button intead of mc_button-delete_field.
    <ls_button>-function  = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    <ls_button>-icon      = icon_delete_row.
    <ls_button>-text      = 'Delete field'(dlf).

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function  = mc_button-add_new_field.
    <ls_button>-icon      = icon_insert_row.
    <ls_button>-text      = 'Add new field'(anf).

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function  = mc_button-change_key.
    <ls_button>-icon      = icon_foreign_key.
    <ls_button>-text      = 'Change key'(chk).

    CHECK mv_editable <> abap_true.
    LOOP AT rt_toolbar ASSIGNING <ls_button>.
      <ls_button>-disabled = abap_true.
    ENDLOOP.
  ENDMETHOD.

  METHOD _on_pbo_event.
    CHECK mo_alv = sender.

    DATA lo_grid TYPE REF TO cl_gui_alv_grid.
    lo_grid = mo_alv->get_grid( ).
    CHECK lo_grid IS NOT INITIAL.

    " Make copy
    DATA lt_drop_down LIKE lcl_editor=>mt_f4_tables .
    lt_drop_down = go_editor->mt_f4_tables.
    DELETE lt_drop_down WHERE int_value = mr_field_desc->name.

    lo_grid->set_drop_down_table( it_drop_down_alias = lt_drop_down ).
    lo_grid->refresh_table_display( ).
  ENDMETHOD.

  METHOD _on_user_command.
    DATA ls_sub_fld_desc TYPE ts_sub_fld_desc.
    DATA lv_fname        TYPE zcl_eui_type=>ts_field_desc-name.

    CASE e_ucomm.

        " Add field to field catalog
      WHEN mc_button-add_new_field.
        " Get full description
        go_editor->add_new_field_screen(
         IMPORTING "er_data       = lr_data
                    es_field_desc = ls_sub_fld_desc-field_desc ).
        CHECK ls_sub_fld_desc-field_desc IS NOT INITIAL.

        " Already exist
        lv_fname = ls_sub_fld_desc-field_desc-name.
        READ TABLE mt_sub_fld_desc TRANSPORTING NO FIELDS
         WITH KEY name = lv_fname.
        IF sy-subrc = 0.
          MESSAGE s022(zaqo_message) WITH lv_fname DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        " Just add to the end
        APPEND ls_sub_fld_desc TO mt_sub_fld_desc.
        sender->refresh_table_display( ).

      WHEN mc_button-change_key.
        _change_key( ).

    ENDCASE.
  ENDMETHOD.

  METHOD _change_key.
    " Show in screen
    DATA ls_dyn_scr  TYPE REF TO zsaqo_table_key_dialog. " PARAMETERS & SELECT-OPTIONS
    DATA lo_screen   TYPE REF TO zcl_eui_screen.
    DATA lo_err      TYPE REF TO cx_root.
    DATA lr_key_desr TYPE REF TO abap_keydescr.
    DATA ls_key_desr TYPE abap_keydescr.
    DATA ls_key      LIKE LINE OF ls_dyn_scr->s_4_key.

    " Where to store data
    CREATE DATA ls_dyn_scr.
    ls_dyn_scr->p_4_kind = mr_field_desc->table_kind.
    ls_dyn_scr->p_4_unq  = mr_field_desc->unique.
    ls_dyn_scr->p_4_keyd = mr_field_desc->key_defkind.
    LOOP AT mr_field_desc->key REFERENCE INTO lr_key_desr.
      ls_key-sign   = 'I'.
      ls_key-option = 'EQ'.
      ls_key-low    = lr_key_desr->name.
      APPEND ls_key TO ls_dyn_scr->s_4_key[].
    ENDLOOP.

    " Create screen manager
    TRY.
        DATA lv_prog TYPE sycprog.
        CONCATENATE sy-cprog `CHANGE_KEY` INTO lv_prog.
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr   = zcl_eui_screen=>mc_dynnr-dynamic
            iv_cprog   = lv_prog
            ir_context = ls_dyn_scr.
      CATCH zcx_eui_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

*    " Static PF status no need on_pbo_event.
    lo_screen->customize( name = 'P_4_KIND' required = '1' ).
    lo_screen->customize( name = 'P_4_KEYD' required = '1' ). " input = '0' ?

    " Set text
    lo_screen->ms_status-is_fixed = abap_true.
    lo_screen->ms_status-title = 'Change declaration and run again is much easier!'(ad1).

    " Ok & Cancel
    IF mv_editable <> abap_true.
      APPEND 'OK' TO lo_screen->ms_status-exclude.
    ENDIF.

    " As popup
    DATA lv_col_end TYPE i.
    lo_screen->get_dimension( IMPORTING ev_col_end = lv_col_end ).
    lo_screen->popup( iv_col_end  = lv_col_end ).

    " Check OK pressed
    CHECK lo_screen->show( ) = 'OK'.

    " Copy back
    mr_field_desc->table_kind  = ls_dyn_scr->p_4_kind.
    mr_field_desc->unique      = ls_dyn_scr->p_4_unq.
    mr_field_desc->key_defkind = ls_dyn_scr->p_4_keyd.
    " Field by field
    CLEAR mr_field_desc->key.
    LOOP AT ls_dyn_scr->s_4_key INTO ls_key.
      ls_key_desr-name = ls_key-low.
      INSERT ls_key_desr INTO TABLE mr_field_desc->key.
    ENDLOOP.
  ENDMETHOD.

  METHOD _on_hotspot_click.
    DATA:
      ls_sub_fld_desc   TYPE REF TO ts_sub_fld_desc,
      lo_table_comp_alv LIKE me,
      lr_field_desc     TYPE REF TO zcl_eui_type=>ts_field_desc.

    " Current item
    READ TABLE mt_sub_fld_desc REFERENCE INTO ls_sub_fld_desc INDEX e_row_id-index.
    CHECK sy-subrc = 0.

    CASE e_column_id.
      WHEN 'CATALOG'.
        IF ls_sub_fld_desc->ui_type <> zcl_eui_type=>mc_ui_type-table.
          zcl_aqo_helper=>drill_down( ls_sub_fld_desc->rollname ).
          RETURN.
        ENDIF.

        " Create new instance
        GET REFERENCE OF ls_sub_fld_desc->field_desc INTO lr_field_desc.
        CREATE OBJECT lo_table_comp_alv
          EXPORTING
            ir_field_desc = lr_field_desc
            iv_editable   = mv_editable.

        " Show catalog again
        lo_table_comp_alv->show( ).
    ENDCASE.
  ENDMETHOD.

  METHOD _on_pai_event.
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

        mr_field_desc->sub_fdesc = zcl_eui_conv=>to_json( lt_field_desc[] ).
        MESSAGE s004(zaqo_message).

      WHEN zif_eui_manager=>mc_cmd-cancel.
        MESSAGE s130(ed) WITH 'Edit'(edt) DISPLAY LIKE 'E'.
    ENDCASE.
  ENDMETHOD.                    "pai
ENDCLASS.
