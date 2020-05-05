*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_fld_value_alv IMPLEMENTATION.
  METHOD get_instance.
    IF mo_instance IS INITIAL.
      CREATE OBJECT mo_instance.
    ENDIF.
    ro_instance = mo_instance.
  ENDMETHOD.                    "get_instance

  METHOD call_screen.
    DATA lv_read_only TYPE abap_bool.

    " Can edit ?
    mv_editable = lcl_opt=>is_editable( ).
    IF mv_editable <> abap_true.
      lv_read_only = abap_true.
    ENDIF.
**********************************************************************
    " Main table
    DATA lr_table TYPE REF TO data.
    GET REFERENCE OF lcl_opt=>mt_fld_value INTO lr_table.

**********************************************************************
    " Prepare layout
    " Just few fileds are edtiable ---> ls_layout-edit = abap_false

**********************************************************************
    " Variant
    DATA ls_variant TYPE disvariant.
    CONCATENATE p_pack p_opt_id INTO ls_variant-report.
    ls_variant-handle  = '0001'.

**********************************************************************
    " Get field catalog
    DATA lt_fieldcat     TYPE lvc_t_fcat.
    DATA ls_fieldcat     TYPE REF TO lvc_s_fcat.
    DATA ls_fld_value    TYPE REF TO lcl_opt=>ts_fld_value.

    " All fields are hidden
    add_fcat_field 'LABEL' ''.
    ls_fieldcat->edit     = mv_editable.
    ls_fieldcat->col_pos  = 11.

    add_fcat_field 'IS_EDITABLE' ''.
    ls_fieldcat->edit     = mv_editable.
    ls_fieldcat->col_pos  = 12.

    add_fcat_field 'ROLLNAME' ''.
    ls_fieldcat->edit     = mv_editable.
    ls_fieldcat->col_pos  = 13.

    add_fcat_field 'ICON' '---'.

    " Now show for all types
    add_fcat_field 'VALUE_BUTTON' 'Quick edit'(qed).
    ls_fieldcat->hotspot   = abap_true.

    " If have 'TABLE'
    add_fcat_field 'CATALOG' 'Catalog'(cat).
    ls_fieldcat->hotspot   = abap_true.
    ls_fieldcat->tech = abap_true.
    LOOP AT lcl_opt=>mt_fld_value TRANSPORTING NO FIELDS
      WHERE ui_type = zcl_eui_type=>mc_ui_type-table.
      ls_fieldcat->tech = abap_false.
      EXIT.
    ENDLOOP.

    " If have history
    add_fcat_field 'HISTORY_LOGS'  'Сhange logs'(log).
    ls_fieldcat->hotspot   = abap_true.
    ls_fieldcat->tech = abap_true.
    LOOP AT lcl_opt=>mt_fld_value REFERENCE INTO ls_fld_value.
      CHECK lines( ls_fld_value->value ) > 1.
      ls_fieldcat->tech = abap_false.
      EXIT.
    ENDLOOP.

    " tech fields
    add_fcat_field '+' ''. " Begin of group
    ls_fieldcat->tech = abap_true.

    add_fcat_field '+SYS_TYPE'    ''.  add_fcat_field '+LENGTH'      ''.  add_fcat_field '+DECIMALS'    ''.  add_fcat_field '+TABLE_KIND'  ''.
    add_fcat_field '+UNIQUE'      ''.  add_fcat_field '+KEY'         ''.  add_fcat_field '+KEY_DEFKIND' ''.  add_fcat_field '+SUB_FDESC'   ''.
**********************************************************************
    " Toolbar
    DATA lt_toolbar TYPE ttb_button.
    DATA ls_toolbar TYPE stb_button.

    " Only if editable
    IF mv_editable = abap_true.
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
    DATA lv_desc    TYPE ztaqo_option-description.

    " Pass by reference
    CREATE OBJECT lo_eui_alv TYPE zcl_eui_alv
      EXPORTING
        ir_table       = lr_table
        " grid parameters
      " is_layout      = ls_layout
        it_mod_catalog = lt_fieldcat
        it_toolbar     = lt_toolbar
        iv_read_only   = lv_read_only.

    " Static PF status no need on_pbo_event.
    GET REFERENCE OF lo_eui_alv->ms_status INTO ls_status.
    ls_status->is_fixed = abap_true.

    " Set pf-status
    ls_status->prog     = sy-cprog.
    ls_status->name     = 'MAIN_100'.

    " Own buttons
    IF mv_editable = abap_true.
      APPEND 'VIEW'       TO ls_status->exclude.
      ls_status->title = 'Edit option'(eop).
    ELSE.
      APPEND 'EDIT'       TO ls_status->exclude.
      ls_status->title = 'View option'(vop).
    ENDIF.

    " Add tech info
    CONCATENATE ls_status->title ` ` lcl_opt=>mo_option->ms_db_item-package_id ` - ` lcl_opt=>mo_option->ms_db_item-option_id
     INTO ls_status->title.

    " Add texts info
    lv_desc = get_title( ).
    IF lv_desc IS NOT INITIAL.
      CONCATENATE ls_status->title ` (` lv_desc `)` INTO ls_status->title.
    ENDIF.

    " Instead of set handler
    lo_eui_alv->show(
      io_handler      = me
      " Could be omited. But do not auto call on_data_changed!
      iv_handlers_map = `ON_PAI_EVENT;ON_TOOLBAR;ON_USER_COMMAND;ON_HOTSPOT_CLICK;ON_DOUBLE_CLICK`
    ).
  ENDMETHOD.                    "call_screen

  METHOD get_title.
    rv_title = lcl_opt=>mo_option->ms_db_item-description.
    CHECK rv_title IS INITIAL.

    DATA lv_ctext TYPE tdevct-ctext.
    SELECT SINGLE ctext INTO lv_ctext
    FROM tdevct
    WHERE devclass = lcl_opt=>mo_option->ms_db_item-package_id
      AND spras    = sy-langu.
    rv_title = lv_ctext.
  ENDMETHOD.

  METHOD set_exclude_toolbar.
    " Disable buttons
    IF  iv_editable <> abap_true.
      DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    ENDIF.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_graph.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_info.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_loc_copy.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_loc_cut.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_loc_paste.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_loc_undo.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_print.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_refresh.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_mb_export.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_mb_view.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_views.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_check.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_call_master_data.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_call_more.
    DELETE ct_toolbar WHERE function = cl_gui_alv_grid=>mc_fc_call_lineitems.
  ENDMETHOD.

  METHOD add_new_field.
    " Show in screen
    DATA ls_dyn_scr TYPE REF TO zsaqo_new_field. " PARAMETERS:
    DATA lo_screen  TYPE REF TO zcl_eui_screen.
    DATA lo_type    TYPE REF TO cl_abap_datadescr.
    DATA lo_err     TYPE REF TO cx_root.
    FIELD-SYMBOLS <lv_data> TYPE any.

    CLEAR:
      er_data,
      es_field_desc.

    " Where to store data
    CREATE DATA ls_dyn_scr.
    ls_dyn_scr->p_fname = 'INDEX'.
    ls_dyn_scr->p_ftype = 'SYST-INDEX'.

    " Create screen manager
    TRY.
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr   = '1010'
            iv_cprog   = sy-cprog
            ir_context = ls_dyn_scr.
      CATCH zcx_eui_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Static PF status no need on_pbo_event.
    lo_screen->customize( name = 'P_FNAME' required = '1' ).
    lo_screen->customize( name = 'P_FTYPE' required = '1' ).

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

    " Capital case
    TRANSLATE:
     ls_dyn_scr->p_fname TO UPPER CASE,
     ls_dyn_scr->p_ftype TO UPPER CASE.

    TRY.
        " Create by text description
        lo_type = zcl_eui_type=>create_type_descr( iv_rollname = ls_dyn_scr->p_ftype ).
        CREATE DATA er_data TYPE HANDLE lo_type.

        ASSIGN er_data->* TO <lv_data>.
        es_field_desc = zcl_eui_type=>get_field_desc(
         iv_field_name = ls_dyn_scr->p_fname
         iv_data       = <lv_data> ).
      CATCH cx_root INTO lo_err.                         "#EC CATCH_ALL
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD on_toolbar.
    set_exclude_toolbar(
     EXPORTING
      iv_editable = mv_editable
     CHANGING
      ct_toolbar  = e_object->mt_toolbar ).
  ENDMETHOD.

  METHOD on_user_command.
    DATA lr_data        TYPE REF TO data.
    DATA ls_field_value TYPE zcl_aqo_helper=>ts_field_value.
    DATA lo_err         TYPE REF TO cx_root.
    DATA lv_fname       TYPE zcl_eui_type=>ts_field_desc-name.

    " Add field to the main option
    CHECK e_ucomm = 'ADD_NEW_FIELD'.

    " Get full description
    add_new_field(
     IMPORTING
       er_data       = lr_data
       es_field_desc = ls_field_value-field_desc ).
    CHECK ls_field_value-field_desc IS NOT INITIAL.

    " Already exist
    lv_fname = ls_field_value-field_desc-name.
    READ TABLE lcl_opt=>mt_fld_value TRANSPORTING NO FIELDS
     WITH KEY name = lv_fname.
    IF sy-subrc = 0.
      MESSAGE s002(zaqo_message) WITH lv_fname DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    TRY.
        lcl_opt=>add_one_field(
         is_field_value = ls_field_value
         ir_data        = lr_data ).
      CATCH cx_root INTO lo_err.                         "#EC CATCH_ALL
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    sender->refresh_table_display( ).
    MESSAGE s032(zaqo_message) WITH lv_fname.
  ENDMETHOD.

  METHOD on_hotspot_click.
    DATA:
      ls_fld_value      TYPE REF TO lcl_opt=>ts_fld_value,
      lo_table_comp_alv TYPE REF TO lcl_table_comp_alv,
      lo_table_alv      TYPE REF TO lcl_table_alv,
      lr_field_desc     TYPE REF TO zcl_eui_type=>ts_field_desc,
      lv_editable       TYPE abap_bool,
      lv_read_only      TYPE abap_bool.
    FIELD-SYMBOLS <lv_value> TYPE any.

    " Current item
    READ TABLE lcl_opt=>mt_fld_value REFERENCE INTO ls_fld_value INDEX es_row_no-row_id.
    CHECK sy-subrc = 0.

    CASE e_column_id.
      WHEN 'CATALOG'.
        CHECK ls_fld_value->ui_type = zcl_eui_type=>mc_ui_type-table.
        lo_table_comp_alv = lcl_table_comp_alv=>get_instance( 1 ). " 1

        " Prepare IMPORTING params
        GET REFERENCE OF ls_fld_value->field_desc INTO lr_field_desc.
        lv_editable = lcl_opt=>is_editable( ls_fld_value->is_editable ).

        lo_table_comp_alv->call_screen(
         is_field_desc = lr_field_desc
         iv_editable   = lv_editable ).

      WHEN 'VALUE_BUTTON'.
        " Only for tables
        CASE ls_fld_value->ui_type.

          WHEN zcl_eui_type=>mc_ui_type-range.
            " Select option
            IF lcl_opt=>is_editable( ls_fld_value->is_editable ) <> abap_true.
              lv_read_only = abap_true.
            ENDIF.
            zcl_eui_screen=>show_range(
              is_field_desc = ls_fld_value->field_desc
              ir_cur_value  = ls_fld_value->cur_value
              iv_read_only  = lv_read_only ).

          WHEN zcl_eui_type=>mc_ui_type-table.
            lo_table_alv = lcl_table_alv=>get_instance( 1 ). " 1
            lo_table_alv->call_screen( ls_fld_value ).

          WHEN zcl_eui_type=>mc_ui_type-string.
            go_string_memo = lcl_string_memo=>get_instance( ). " 1
            go_string_memo->call_screen( ls_fld_value ).

            " Parameters
          WHEN OTHERS.
            ASSIGN ls_fld_value->cur_value->* TO <lv_value>.
            zcl_eui_screen=>edit_in_popup(
             EXPORTING
              iv_label      = ls_fld_value->label
              " iv_rollname   = ls_fld_value->rollname
             CHANGING
               cv_value     = <lv_value> ).
        ENDCASE.

      WHEN 'HISTORY_LOGS'.
        CHECK lines( ls_fld_value->value ) > 1.
        go_logs_alv = lcl_logs_alv=>get_instance( ).
        go_logs_alv->call_screen( ls_fld_value ).

    ENDCASE.
  ENDMETHOD.

  METHOD on_double_click.
    CHECK e_column = 'ROLLNAME'.

    " Read current item
    DATA ls_fld_value TYPE REF TO lcl_opt=>ts_fld_value.
    READ TABLE lcl_opt=>mt_fld_value REFERENCE INTO ls_fld_value INDEX e_row-index.
    CHECK sy-subrc = 0.

    " Show table' fieldname
    zcl_aqo_helper=>drill_down( ls_fld_value->rollname ).
  ENDMETHOD.

  METHOD on_pai_event.
    DATA lo_eui_alv TYPE REF TO zcl_eui_alv.
    DATA lo_grid TYPE REF TO cl_gui_alv_grid.

    " Get grid if it already exist
    lo_eui_alv ?= sender.
    lo_grid = lo_eui_alv->get_grid( ).

    " Custom checks
    CHECK data_check( lo_grid ) = abap_true OR iv_command = 'VIEW'.

    CASE iv_command.
      WHEN 'SAVE'.
        lcl_opt=>pai(
         CHANGING
           cv_cmd = iv_command ).

      WHEN 'VIEW' OR 'EDIT'.
        me->sel_screen_show( ).

    ENDCASE.
  ENDMETHOD.                    "pai

  METHOD on_data_changed.
    DATA:
      ls_fld_value TYPE REF TO lcl_opt=>ts_fld_value,
      lv_row       TYPE i,
      lt_unq       TYPE zcl_eui_type=>tt_unique_type,
      lv_unq       TYPE string.

    LOOP AT lcl_opt=>mt_fld_value REFERENCE INTO ls_fld_value.
      lv_row = sy-tabix.

      IF ls_fld_value->label IS INITIAL.
        MESSAGE e001(zaqo_message) WITH ls_fld_value->name INTO sy-msgli.
        er_data_changed->add_protocol_entry(
          i_msgid     = sy-msgid
          i_msgno     = sy-msgno
          i_msgty     = sy-msgty
          i_msgv1     = sy-msgv1
          i_fieldname = 'LABEL'
          i_row_id    = lv_row ).
      ENDIF.

      IF ls_fld_value->ui_type <> zcl_eui_type=>mc_ui_type-table AND
         ls_fld_value->ui_type <> zcl_eui_type=>mc_ui_type-string.

        lv_unq = ls_fld_value->rollname.
        INSERT lv_unq INTO TABLE lt_unq.

        " Have unique name
        CLEAR sy-msgli.
        TRY.
            IF sy-subrc <> 0 OR

               " Is table and field name
               ls_fld_value->rollname NP '*-*' OR

               " In dictionary
               zcl_eui_type=>create_type_descr( iv_rollname = ls_fld_value->rollname ) IS INITIAL.

              " Specify unique table name
              MESSAGE e002(zaqo_message) WITH ls_fld_value->name INTO sy-msgli.
            ENDIF.
          CATCH zcx_eui_exception.
            " Error during ROLLNAME creation
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
        ENDTRY.

        IF sy-msgli IS NOT INITIAL.
          er_data_changed->add_protocol_entry(
              i_msgid     = sy-msgid
              i_msgno     = sy-msgno
              i_msgty     = sy-msgty
              i_msgv1     = sy-msgv1
              i_fieldname = 'ROLLNAME'
              i_row_id    = lv_row ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Set errors and show
    lcl_grid=>set_err_cells(
     io_grid     = sender
     io_protocol = er_data_changed ).
  ENDMETHOD.

  METHOD data_check.
    DATA:
      lr_data_changed TYPE REF TO cl_alv_changed_data_protocol, "lr_msglist TYPE REF TO if_reca_message_list,
      ls_log          TYPE bal_s_log,
      ls_msg          TYPE bal_s_msg,
      lt_hnd          TYPE bal_t_logh,
      lv_hnd          TYPE balloghndl,
      ls_prof         TYPE bal_s_prof,
      ls_message      TYPE REF TO lvc_s_msg1.

    " Create protocol
    CREATE OBJECT lr_data_changed
      EXPORTING
        i_calling_alv = io_grid.

    " Use defined catalog
    IF io_grid IS NOT INITIAL.
      io_grid->get_backend_fieldcatalog(
       IMPORTING
         et_fieldcatalog = lr_data_changed->mt_fieldcatalog ).
    ENDIF.

    " Customs checks
    on_data_changed(
     sender          = io_grid
     er_data_changed = lr_data_changed ).

    " Show without grid
    IF io_grid IS INITIAL AND lr_data_changed->mt_protocol IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_CREATE'
        EXPORTING
          i_s_log      = ls_log
        IMPORTING
          e_log_handle = lv_hnd.
      LOOP AT lr_data_changed->mt_protocol REFERENCE INTO ls_message.
        MOVE-CORRESPONDING ls_message->* TO ls_msg.

        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_s_msg      = ls_msg
            i_log_handle = lv_hnd.
      ENDLOOP.

      CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
        IMPORTING
          e_s_display_profile = ls_prof.

      ls_prof-use_grid = abap_true.
      ls_prof-title    = 'Fix errors in DEV mandt first'(dis).
      MESSAGE ls_prof-title TYPE 'S' DISPLAY LIKE 'E'.

      INSERT lv_hnd INTO TABLE lt_hnd.
      CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
        EXPORTING
          i_t_log_handle      = lt_hnd
          i_s_display_profile = ls_prof.
    ENDIF.

    CHECK lr_data_changed->mt_protocol IS INITIAL.

    " Checks ok
    rv_ok = abap_true.
  ENDMETHOD.

  METHOD sel_screen_show.
    DATA lr_dyn_screen       TYPE REF TO data.
    DATA lt_sub_field        TYPE zcl_eui_type=>tt_field_desc.
    DATA ls_sub_field        TYPE zcl_eui_type=>ts_field_desc.
    DATA ls_fld_value        TYPE REF TO lcl_opt=>ts_fld_value.
    DATA lo_err              TYPE REF TO zcx_eui_exception.
    DATA lo_struc_desc       TYPE REF TO cl_abap_structdescr.
    DATA lo_screen           TYPE REF TO zcl_eui_screen.
    DATA lv_cmd              TYPE syucomm.
    DATA lv_input            TYPE screen-input.
    FIELD-SYMBOLS <ls_dest>  TYPE any.
    FIELD-SYMBOLS <lv_dest>  TYPE any.
    FIELD-SYMBOLS <lv_src>   TYPE any.

    " Create structure for screen
    LOOP AT lcl_opt=>mt_fld_value REFERENCE INTO ls_fld_value.
      MOVE-CORRESPONDING ls_fld_value->* TO ls_sub_field.
      INSERT ls_sub_field INTO TABLE lt_sub_field.
    ENDLOOP.

    " Create new structure
    TRY.
        lo_struc_desc = zcl_eui_type=>create_structure( it_field_desc = lt_sub_field ).
      CATCH zcx_eui_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Screen for FREE SELECTION
    CREATE DATA lr_dyn_screen TYPE HANDLE lo_struc_desc.
    ASSIGN lr_dyn_screen->* TO <ls_dest>.

    LOOP AT lcl_opt=>mt_fld_value REFERENCE INTO ls_fld_value.
      ASSIGN COMPONENT ls_fld_value->name OF STRUCTURE <ls_dest> TO <lv_dest>.
      ASSIGN ls_fld_value->cur_value->* TO <lv_src>.
      <lv_dest> = <lv_src>.
    ENDLOOP.

    " Create screen manager
    TRY.
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr     = zcl_eui_screen=>mc_dynnr-free_sel
            ir_context   = lr_dyn_screen
            iv_read_only = lcl_opt=>mv_read_only. " NOT mv_editable ?
      CATCH zcx_eui_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    lo_screen->ms_status-is_fixed = abap_true.
    lo_screen->ms_status-title = get_title( ). " 'Maintenance parameters'(tit).

    " Ok & Cancel
    IF lcl_opt=>mv_read_only = abap_true.
      APPEND 'OK' TO lo_screen->ms_status-exclude.
    ENDIF.

    " Editable or not
    LOOP AT lcl_opt=>mt_fld_value REFERENCE INTO ls_fld_value.
      lv_input = '0'.
      IF ls_fld_value->is_editable = abap_true OR mv_editable = abap_true.
        lv_input = '1'.
      ENDIF.

      " TODO required ?
      lo_screen->customize(
       name         = ls_fld_value->name
       input        = lv_input
       iv_label     = ls_fld_value->label
       iv_sub_fdesc = ls_fld_value->sub_fdesc ).
    ENDLOOP.

    " Always as popup ?
    lo_screen->popup( iv_col_beg  = 1
                      iv_row_beg  = 1
                      iv_col_end  = 118
                      iv_row_end  = 30 ).

    " Hide
    lcl_opt=>set_menu_visible( abap_false ).

    " Check OK pressed
    lv_cmd = lo_screen->show( ).

    " Show again
    lcl_opt=>set_menu_visible( abap_true ).

    " Ok pressed
    CHECK lv_cmd = 'OK'.

    " Copy back
    LOOP AT lcl_opt=>mt_fld_value REFERENCE INTO ls_fld_value.
      ASSIGN COMPONENT ls_fld_value->name OF STRUCTURE <ls_dest> TO <lv_src>.
      ASSIGN ls_fld_value->cur_value->* TO <lv_dest>.
      <lv_dest> = <lv_src>.
    ENDLOOP.

    " And save
    lcl_opt=>do_save( ).
  ENDMETHOD.                    "sel_screen_show
ENDCLASS.
