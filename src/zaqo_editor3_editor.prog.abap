*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_editor IMPLEMENTATION.
  METHOD constructor. " INITIALIZATION event
    super->constructor( ).

    " Mandt is open to change
    mv_is_dev = zcl_aqo_helper=>is_dev_mandt( ).

    " Do not launch via se38 or se80
    CHECK zcl_aqo_helper=>is_in_editor( ) <> abap_true.
    zcx_aqo_exception=>raise_dump( iv_message = 'Please use ZAQO* transactions instead!'(ms1) ).
  ENDMETHOD.

  METHOD start_of_selection.
    CALL SCREEN 70.
  ENDMETHOD.

  METHOD sync_screen_ui.
    DATA: lv_ucomm TYPE syucomm VALUE '-'.

    DO 1 TIMES.
      CHECK iv_message <> abap_undefined.

      DATA lv_message TYPE string.
      lv_message = 'Exit for regenerating "Option data" screen'(ad3).

      " Messages as popup
      IF iv_exit = abap_true.
        DATA lo_logger TYPE REF TO zcl_eui_logger.
        CREATE OBJECT lo_logger.
        lo_logger->add_text( iv_text = iv_message ).
        lo_logger->add_text( iv_text = lv_message iv_msgty = 'W' ).
        lo_logger->show( iv_profile = zcl_eui_logger=>mc_profile-popup ).
        lv_ucomm = mc_pai_cmd-exit.
        EXIT.
      ENDIF.

      " Informative messages
      IF iv_message IS NOT INITIAL.
        CONCATENATE iv_message lv_message INTO lv_message SEPARATED BY ` - `.
      ENDIF.
      MESSAGE lv_message TYPE 'S'.
    ENDDO.

    cl_gui_cfw=>set_new_ok_code( lv_ucomm ).
  ENDMETHOD.

  METHOD pbo.
    tabs-activetab   = g_tabs-pressed_tab.
    g_tabs-prog      = zcl_aqo_helper=>mc_prog-editor.
    g_tabs-subscreen = g_tabs-pressed_tab+5.

    DO 1 TIMES.
      CHECK tabs-activetab = mc_pai_cmd-tab_edit_data
        AND mo_screen IS NOT INITIAL.
      g_tabs-prog      = mo_screen->ms_screen-prog.
      g_tabs-subscreen = mo_screen->ms_screen-dynnr.
    ENDDO.

    _set_tab1_icon( ).
    _set_status( ).
    _set_titlebar( ).
    _make_tree( ).
  ENDMETHOD.

  METHOD _set_tab1_icon.
    DATA lv_icon TYPE text20.
    DATA lv_text TYPE text100.

    IF mv_read_only <> abap_true.
      lv_icon = 'ICON_CHANGE'.
      lv_text = 'Edit data'(edd).
    ELSE.
      lv_icon = 'ICON_DISPLAY'.
      lv_text = 'View data'(vid).
    ENDIF.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name   = lv_icon
        text   = lv_text
      IMPORTING
        result = tabs_tab1
      EXCEPTIONS
        OTHERS = 0.
  ENDMETHOD.

  METHOD _set_status.
    DATA lt_exclude TYPE STANDARD TABLE OF syucomm.
    IF mo_option IS INITIAL OR mv_read_only = abap_true OR mo_screen IS INITIAL OR mv_initial_hash = _calculate_hash( ).
      APPEND mc_pai_cmd-save TO lt_exclude.
    ENDIF.
    SET PF-STATUS 'MAIN_STATUS' EXCLUDING lt_exclude.
  ENDMETHOD.

  METHOD _set_titlebar.
    DATA lv_title TYPE string.
    lv_title = _get_title( ).
    SET TITLEBAR 'TITLE_100' OF PROGRAM 'SAPLZFG_EUI_SCREEN' WITH lv_title.
  ENDMETHOD.

  METHOD _make_tree.
    " 1 time only
    CHECK mo_tree IS INITIAL.
    CREATE OBJECT:
      mo_prefs,
      mo_tree
       EXPORTING
         io_prefs = mo_prefs.

    mo_tree->make_gui( ).
    mo_tree->fill( ).
  ENDMETHOD.

  METHOD _get_title.
    CHECK mo_option IS NOT INITIAL.

    IF mv_read_only <> abap_true.
      rv_title = 'Edit mode'(eop).
    ELSE.
      rv_title = 'View mode'(vop).
    ENDIF.

    IF iv_add_opt_info = abap_true.
      CONCATENATE rv_title ` - ` zsaqo3_general_info-package_id ` - ` zsaqo3_general_info-option_id INTO rv_title.
    ENDIF.

    DATA lv_desc TYPE string.
    lv_desc = mo_option->ms_db_item-description.
    IF lv_desc IS INITIAL.
      DATA lv_ctext TYPE tdevct-ctext.
      SELECT SINGLE ctext INTO lv_ctext
      FROM tdevct
      WHERE devclass = mo_option->ms_db_item-package_id
        AND spras    = sy-langu.
      lv_desc = lv_ctext.
    ENDIF.

    CHECK lv_desc IS NOT INITIAL.
    CONCATENATE rv_title ` - ` lv_desc INTO rv_title.
  ENDMETHOD.

  METHOD pai.
    " Use copy
    DATA lv_ok_code LIKE cv_ok_code.
    lv_ok_code = cv_ok_code.
    CLEAR cv_ok_code.

    DATA ls_command TYPE ts_command.
    zcl_aqo_helper=>exchange_command( IMPORTING es_command = ls_command ).

    CASE lv_ok_code.
      WHEN mc_pai_cmd-open_option
        OR mc_pai_cmd-new_option. " TODO detect creation mode?

        CHECK _is_saved( ) = abap_true.
        do_open( is_db_key = ls_command-db_key ).

        IF lv_ok_code = mc_pai_cmd-new_option.
          g_tabs-pressed_tab = mc_pai_cmd-tab_field_settings.
          " Defaults for new option
          zsaqo3_general_info-package_id     = ls_command-package_id.
          zsaqo3_general_info-option_id      = ls_command-option_id.
          zsaqo3_general_info-prev_value_cnt = mo_option->ms_db_item-prev_value_cnt.
        ENDIF.

        RAISE EVENT app_event EXPORTING iv_origin = mc_event-open.

      WHEN mc_pai_cmd-show_user_prefs.
        mo_prefs->show_screen( ).

      WHEN mc_pai_cmd-exit.
        CHECK _is_saved( ) = abap_true.
        LEAVE TO SCREEN 0.

      WHEN mc_pai_cmd-save.
        do_save( ).

      WHEN OTHERS.
        CHECK zsaqo3_general_info IS NOT INITIAL.
        IF lv_ok_code CP 'TABS_*'.
          g_tabs-pressed_tab = lv_ok_code.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD do_open.
    IF mo_option IS NOT INITIAL.
      RAISE EVENT app_event EXPORTING iv_origin = mc_event-close.
      mo_option->lock( iv_unlock = abap_true ).
    ENDIF.

    CLEAR: mo_option,
           mo_screen,
           zsaqo3_general_info,
           mv_initial_hash.

    DATA lo_err TYPE REF TO zcx_aqo_exception.
    TRY.
        " TODO create new option
        mo_option = zcl_aqo_option=>create(
           iv_package_id  = is_db_key-package_id
           iv_option_id   = is_db_key-option_id ).
      CATCH zcx_aqo_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    _set_flags( ).
    zsaqo3_general_info = _get_general_info( ).
    _fill_fields( ).
    _find_f4_tables( ).

    " Active tab is 'Edit data'
    g_tabs-pressed_tab = mc_pai_cmd-tab_edit_data.
    mo_screen = _make_screen( iv_menu_mode = iv_menu_mode ).
    CHECK mo_screen IS NOT INITIAL.

    mv_initial_hash = _calculate_hash( ).

    CHECK mo_tree IS NOT INITIAL.
    mo_prefs->add_opened( is_db_key ).
    mo_tree->add_opened( is_db_key ).
  ENDMETHOD.

  METHOD _set_flags.
    mv_read_only = abap_false.
    IF mo_option->lock( ) <> abap_true.
      mv_read_only = abap_true.

      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " Just show values
    IF zcl_aqo_helper=>is_in_editor( iv_is_viewer = abap_true ) = abap_true.
      mv_read_only = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD is_editable.
    CHECK mv_read_only <> abap_true
      AND iv_editable  <> abap_undefined.

    IF iv_editable = abap_true OR mv_is_dev = abap_true.
      rv_editable = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD do_delete.
    IF zcl_eui_screen=>confirm(
           iv_title    = 'Delete'(del)
           iv_question = 'Operation irreversible. Continue?'(irr)
           iv_icon_1   = 'ICON_DELETE' ) <> abap_true.
      MESSAGE s130(ed) WITH 'Delete'(del) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " $ & transport
    DATA lo_error TYPE REF TO zcx_aqo_exception.
    TRY.
        DATA lv_message TYPE string.
        lv_message = mo_option->delete( ).
      CATCH zcx_aqo_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    IF lv_message IS NOT INITIAL.
      MESSAGE lv_message TYPE 'S'.
    ENDIF.

    mo_tree->delete_from( iv_parent_node = mo_tree->ms_node-rec_opened
                          is_db_key      = is_db_key ).
    " Also in created?
    mo_tree->delete_from( iv_parent_node = mo_tree->ms_node-rec_created
                          is_db_key      = is_db_key
                          iv_refresh     = abap_true ).

    mo_prefs->add_opened( is_db_key = is_db_key
                          iv_insert = abap_false ).
    do_open( is_db_key ).
  ENDMETHOD.

  METHOD _get_general_info.
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF rs_info
    FROM ztaqo_option
    WHERE package_id = mo_option->ms_db_item-package_id
      AND option_id  = mo_option->ms_db_item-option_id.

    CHECK sy-subrc <> 0.
    " Or something like that SY-SYSID <> 'DEV' IF zcl_aqo_helper=>is_dev_mandt( ) <> abap_true.
    MESSAGE s006(zaqo_message) WITH mo_option->ms_db_item-package_id mo_option->ms_db_item-option_id DISPLAY LIKE 'E'. " TODO Move to =>create( )
  ENDMETHOD.

  METHOD _fill_fields.
    " Create new table
    CLEAR mt_fld_value.

    DATA lt_skip_field  TYPE stringtab.
    DATA ls_field_value TYPE REF TO zcl_aqo_helper=>ts_field_value.
    DATA lo_err         TYPE REF TO zcx_aqo_exception.

    LOOP AT mo_option->mt_field_value REFERENCE INTO ls_field_value.
      TRY.
          add_one_field( ls_field_value->* ).
        CATCH zcx_aqo_exception INTO lo_err.
          APPEND ls_field_value->name TO lt_skip_field.
          MESSAGE lo_err TYPE 'I'.
      ENDTRY.
    ENDLOOP.

    " Any error during importing?
    CHECK lt_skip_field IS NOT INITIAL.

    DATA lv_fields TYPE string.
    CONCATENATE LINES OF lt_skip_field INTO lv_fields SEPARATED BY `, `.
    MESSAGE s040(zaqo_message) WITH lv_fields DISPLAY LIKE 'E'.
  ENDMETHOD.

  METHOD _find_f4_tables.
    DATA ls_field_value TYPE REF TO zcl_aqo_helper=>ts_field_value.

    LOOP AT mo_option->mt_field_value REFERENCE INTO ls_field_value
                                      WHERE ui_type     = zcl_eui_type=>mc_ui_type-table
                                        AND ( table_kind  = cl_abap_tabledescr=>tablekind_sorted
                                           OR table_kind  = cl_abap_tabledescr=>tablekind_hashed )
                                        AND key_defkind = cl_abap_tabledescr=>keydefkind_user
                                        AND unique      = abap_true. "#EC CI_HASHSEQ
      " Only for relations 1 - 1
      CHECK lines( ls_field_value->key[] ) = 1.

      DATA lr_ft_table TYPE REF TO lvc_s_dral.
      APPEND INITIAL LINE TO mt_f4_tables REFERENCE INTO lr_ft_table.
      lr_ft_table->handle    = 154.
      lr_ft_table->int_value = ls_field_value->name.
      CONCATENATE ls_field_value->name ` - ` ls_field_value->label INTO lr_ft_table->value.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_one_field.
    " Get current value
    DATA lr_value TYPE REF TO data.
    IF ir_data IS NOT INITIAL.
      lr_value = ir_data.
    ELSE.
      lr_value = mo_option->get_field_value( is_field_value-name ).
    ENDIF.

    DATA ls_fld_value TYPE REF TO ts_fld_value.
    " Paste new data
    APPEND INITIAL LINE TO mt_fld_value REFERENCE INTO ls_fld_value.
    MOVE-CORRESPONDING is_field_value TO ls_fld_value->*.
    ls_fld_value->cur_value = lr_value.

    " Quick edit for all type of fields
    ls_fld_value->value_button = icon_display_more.

    set_icons( EXPORTING is_field_desc = ls_fld_value->field_desc
               IMPORTING ev_icon       = ls_fld_value->icon
                         ev_catalog    = ls_fld_value->catalog ).

    " Show history
    IF lines( ls_fld_value->value ) > 1.
      ls_fld_value->history_logs = icon_protocol.
    ENDIF.
  ENDMETHOD.

  METHOD add_new_field_screen.
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
        DATA lv_prog TYPE sycprog.
        CONCATENATE sy-cprog `ADD_FIELD_SCR` INTO lv_prog.
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr   = zcl_eui_screen=>mc_dynnr-dynamic
            iv_cprog   = lv_prog
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
    lo_screen->ms_status-title = 'Change declaration and run again is much easier!'(ad1).

    " Ok & Cancel
    IF is_editable( ) <> abap_true.
      APPEND 'OK' TO lo_screen->ms_status-exclude.
    ENDIF.

    " As popup
    DATA lv_col_end TYPE i.
    lo_screen->get_dimension( IMPORTING ev_col_end = lv_col_end ).
    lo_screen->popup( iv_col_end  = lv_col_end ).

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

  METHOD set_icons.
    CLEAR: ev_icon,
           ev_catalog.
    IF is_field_desc-rollname CP '*-*'.
      ev_catalog = icon_ps_relationship. " <--- show DDIC
    ENDIF.

    CASE is_field_desc-ui_type.
      WHEN zcl_eui_type=>mc_ui_type-string.
        ev_icon = icon_change_text.
        " CLEAR ev_catalog. no need

      WHEN zcl_eui_type=>mc_ui_type-range.
        ev_icon = icon_interval_include_green.

      WHEN zcl_eui_type=>mc_ui_type-table.
        ev_icon    = icon_wd_table.
        ev_catalog = icon_catalog. " <--- show field catalog

      WHEN OTHERS.
        ev_icon = icon_equal_green.
    ENDCASE.
  ENDMETHOD.

  METHOD do_save.
    DATA:
      ls_fld_value   TYPE REF TO ts_fld_value,
      ls_field_value TYPE zcl_aqo_helper=>ts_field_value,
      lo_err         TYPE REF TO zcx_aqo_exception,
      lv_new_value   TYPE string,
      lv_info        TYPE string.
    FIELD-SYMBOLS: <ls_context> TYPE any,
                   <lv_dest>    TYPE any,
                   <lv_src>     TYPE any,
                   <lv_value>   TYPE any.

    " IF locked by another user
    CHECK mv_read_only <> abap_true
      AND zsaqo3_general_info-package_id IS NOT INITIAL
      AND zsaqo3_general_info-option_id  IS NOT INITIAL.

    DATA lr_ok TYPE REF TO abap_bool.
    CREATE DATA lr_ok.
    lr_ok->* = abap_true.

    RAISE EVENT app_event
     EXPORTING
       iv_origin = mc_event-before_save
       cv_ok     = lr_ok.
    CHECK lr_ok->* = abap_true.

    " Copy back.
    IF mo_screen IS NOT INITIAL.
      DATA lr_context TYPE REF TO data.
      lr_context = mo_screen->get_context( ).
      ASSIGN lr_context->* TO <ls_context>.

      LOOP AT mt_fld_value REFERENCE INTO ls_fld_value.
        ASSIGN COMPONENT ls_fld_value->name OF STRUCTURE <ls_context> TO <lv_src>.
        ASSIGN ls_fld_value->cur_value->* TO <lv_dest>.
        <lv_dest> = <lv_src>.
      ENDLOOP.
    ENDIF.

    CLEAR mo_option->mt_field_value.
    LOOP AT mt_fld_value REFERENCE INTO ls_fld_value.
      CLEAR ls_field_value.
      MOVE-CORRESPONDING ls_fld_value->* TO ls_field_value.

      " As JSON string
      ASSIGN ls_fld_value->cur_value->* TO <lv_value>.
      lv_new_value = zcl_eui_conv=>to_json( <lv_value> ).

      " Add new value
      mo_option->add_history_value(
       EXPORTING
         iv_value       = lv_new_value
       CHANGING
         cs_field_value = ls_field_value ).

      INSERT ls_field_value INTO TABLE mo_option->mt_field_value.
    ENDLOOP.

    TRY.
        lv_info = mo_option->save( is_db      = zsaqo3_general_info
                                   iv_confirm = iv_confirm ).
        IF lv_info IS NOT INITIAL.
          MESSAGE lv_info TYPE 'S'.
        ENDIF.
      CATCH zcx_aqo_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    RAISE EVENT app_event EXPORTING iv_origin = mc_event-after_save.

    " Re generate screen
    DATA ls_db_key TYPE ts_db_key.
    MOVE-CORRESPONDING zsaqo3_general_info TO ls_db_key.
    do_open( ls_db_key ).
  ENDMETHOD.

  METHOD _make_screen.
    DATA lr_dyn_screen       TYPE REF TO data.
    DATA lt_sub_field        TYPE zcl_eui_type=>tt_field_desc.
    DATA ls_sub_field        TYPE zcl_eui_type=>ts_field_desc.
    DATA ls_fld_value        TYPE REF TO ts_fld_value.
    DATA lo_err              TYPE REF TO zcx_eui_exception.
    DATA lo_struc_desc       TYPE REF TO cl_abap_structdescr.
    DATA lv_input            TYPE screen-input.
    FIELD-SYMBOLS <ls_dest>  TYPE any.
    FIELD-SYMBOLS <lv_dest>  TYPE any.
    FIELD-SYMBOLS <lv_src>   TYPE any.

    " Create structure for screen
    CHECK mt_fld_value[] IS NOT INITIAL.
    LOOP AT mt_fld_value REFERENCE INTO ls_fld_value.
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

    LOOP AT mt_fld_value REFERENCE INTO ls_fld_value.
      ASSIGN COMPONENT ls_fld_value->name OF STRUCTURE <ls_dest> TO <lv_dest>.
      ASSIGN ls_fld_value->cur_value->* TO <lv_src>.
      <lv_dest> = <lv_src>.
    ENDLOOP.

    DATA: lv_unq_prog TYPE programm, lo_crc64 TYPE REF TO zcl_eui_crc64, lv_pack TYPE c LENGTH 9.
    CREATE OBJECT lo_crc64.
    lo_crc64->add_to_hash( mo_option->ms_db_item-package_id ).
    lo_crc64->add_to_hash( mo_option->ms_db_item-option_id ).

    lv_unq_prog = lo_crc64->get_hash( ).
    lv_pack     = mo_option->ms_db_item-package_id+1. " No more than 9
    CONCATENATE `ZAQO` lv_pack lv_unq_prog INTO lv_unq_prog.

    " Create screen manager
    TRY.
        DATA lv_editable TYPE abap_bool.
        IF mv_read_only <> abap_true.
          lv_editable = abap_true.
        ENDIF.

        CREATE OBJECT ro_screen
          EXPORTING
            iv_dynnr    = zcl_eui_screen=>mc_dynnr-dynamic
            iv_cprog    = lv_unq_prog
            ir_context  = lr_dyn_screen
            iv_editable = lv_editable. " NOT mv_editable ?
      CATCH zcx_eui_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    "ro_screen->ms_status-title = 'Option data'.

    " Editable or not
    LOOP AT mt_fld_value REFERENCE INTO ls_fld_value.
      lv_input = '0'.
      IF is_editable( ls_fld_value->is_editable )  = abap_true.
        lv_input = '1'.
      ENDIF.

      " TODO required ?
      ro_screen->customize(
       name         = ls_fld_value->name
       input        = lv_input
       iv_label     = ls_fld_value->label
       iv_sub_fdesc = ls_fld_value->sub_fdesc ).
    ENDLOOP.

    CHECK iv_menu_mode <> abap_true.
    ro_screen->pbo( ).
    CALL FUNCTION 'ZFM_EUI_NEXT_SCREEN'
      EXPORTING
        io_scr_manager = ro_screen
        iv_is_top      = abap_true.
  ENDMETHOD.

  METHOD _on_pbo_menu_screen.
    sender->ms_status-title = _get_title( iv_add_opt_info = abap_true ).
  ENDMETHOD.

  METHOD _on_pai_menu_screen.
    CHECK iv_command = 'OK' AND mv_read_only <> abap_true.
    do_save( ).
  ENDMETHOD.

  METHOD _is_saved.
    rv_ok = abap_true.
    CHECK mo_option        IS NOT INITIAL
      AND mo_screen        IS NOT INITIAL
      AND mv_initial_hash IS NOT INITIAL.

    CHECK mv_initial_hash <> _calculate_hash( ).

    DATA lv_answer TYPE abap_bool.
    lv_answer = zcl_eui_screen=>confirm(
        iv_title          = 'Confirmation'(cnf)
        iv_question       = 'Save data before exit?'(sdb)
        iv_icon_1         = 'ICON_SYSTEM_SAVE'
        iv_icon_2         = 'ICON_SYSTEM_END'
        iv_text_2         = 'Discards all changes'(dis)
        iv_display_cancel = abap_true ).

    CASE lv_answer.
      WHEN abap_true.
        do_save( iv_confirm = abap_false ).
        rv_ok = abap_true.

      WHEN abap_false.     " Exit without saving data
        rv_ok = abap_true.

      WHEN abap_undefined. " Cancel
        rv_ok = abap_false.
    ENDCASE.
  ENDMETHOD.

  METHOD _calculate_hash.
    CHECK mo_screen IS NOT INITIAL.

    DATA lr_context TYPE REF TO data.
    lr_context = mo_screen->get_context( ).

    FIELD-SYMBOLS <ls_context> TYPE any.
    ASSIGN lr_context->* TO <ls_context>.

    DATA lo_crc64 TYPE REF TO zcl_eui_crc64.
    CREATE OBJECT lo_crc64.
    lo_crc64->add_to_hash( <ls_context> ).
    lo_crc64->add_to_hash( zsaqo3_general_info ).
    lo_crc64->add_to_hash( mt_fld_value ).

    rv_hash = lo_crc64->get_hash( ).
  ENDMETHOD.

ENDCLASS.
