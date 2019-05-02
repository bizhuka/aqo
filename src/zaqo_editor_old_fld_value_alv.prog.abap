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
    " Show screen
    CALL SCREEN 100.
  ENDMETHOD.                    "call_screen

  METHOD pbo.
    DATA:
      lr_cont        TYPE REF TO cl_gui_custom_container,
      lr_splitter    TYPE REF TO cl_gui_splitter_container,
      lr_alv_cont    TYPE REF TO cl_gui_container,
      lt_fieldcat    TYPE lvc_t_fcat,
      ls_fieldcat    TYPE REF TO lvc_s_fcat,
      ls_layout      TYPE lvc_s_layo,
      lt_toolbar_ex  TYPE ui_functions,
      ls_variant     TYPE disvariant,
      lr_dd_doc      TYPE REF TO cl_dd_document,
      lt_exclude     TYPE STANDARD TABLE OF syucomm,
      lv_column_text TYPE string.

    " Own buttons
    IF lcl_opt=>is_editable( ) = abap_true.
      APPEND 'VIEW'       TO lt_exclude.
    ELSE.
      APPEND: 'CHANGE'    TO lt_exclude,
              'TRANSPORT' TO lt_exclude,
              'CLNT_COPY' TO lt_exclude,
              'DEL_OPT'   TO lt_exclude,
              'FIND_REF'  TO lt_exclude.
    ENDIF.

    SET:
      PF-STATUS '0100'    EXCLUDING lt_exclude,
      TITLEBAR  'ST_MAIN' WITH 'Maintenance parameters'(tit).

    " Create 1 time only
    CHECK mo_grid IS INITIAL.

    " Header and grid
    CREATE OBJECT:
     lr_cont
      EXPORTING
        container_name = 'EMPTY_100',

     lr_splitter
      EXPORTING
        parent  = lr_cont " cl_gui_container=>screen0
        rows    = 2
        columns = 1.

    lr_splitter->set_row_height(
         id     = 1
         height = 23 ).
    mo_header_cont = lr_splitter->get_container(
         row       = 1
         column    = 1 ).

    lr_alv_cont = lr_splitter->get_container(
         row       = 2
         column    = 1 ).

    " Show at first SCREEN
    CREATE OBJECT mo_grid
      EXPORTING
        i_parent = lr_alv_cont
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    " Get field catalog
    zcl_aqo_helper=>create_field_catalog(
     IMPORTING
       et_fieldcat = lt_fieldcat
     CHANGING
       ct_table    = lcl_opt=>mt_fld_value ).

    " Addtional options
    LOOP AT lt_fieldcat REFERENCE INTO ls_fieldcat.
      CLEAR lv_column_text.
      CASE ls_fieldcat->fieldname.
        WHEN 'NAME'.
          ls_fieldcat->scrtext_s = ls_fieldcat->scrtext_m = ls_fieldcat->scrtext_l =
            ls_fieldcat->reptext = ls_fieldcat->coltext = 'Name'(nam).

        WHEN 'UI_TYPE'.
          ls_fieldcat->scrtext_s = ls_fieldcat->scrtext_m = ls_fieldcat->scrtext_l =
            ls_fieldcat->reptext = ls_fieldcat->coltext = 'Kind'(knd).

        WHEN 'LABEL'.
          ls_fieldcat->edit     = lcl_opt=>is_editable( ).
          ls_fieldcat->col_pos  = 11.

        WHEN 'IS_EDITABLE'.
          ls_fieldcat->edit     = abap_true.
          ls_fieldcat->checkbox = abap_true.
          ls_fieldcat->col_pos  = 12.

        WHEN 'ROLLNAME'.
          ls_fieldcat->edit     = lcl_opt=>is_editable( ).
          ls_fieldcat->col_pos  = 13.

        WHEN 'ICON'.
          lv_column_text = '---'.

        WHEN 'CATALOG'.
          lv_column_text = 'Catalog'(cat).

        WHEN 'VALUE_BUTTON'.
          lv_column_text = 'Quick edit'(qed).

          " Have 'T' or 'M'
          LOOP AT lcl_opt=>mt_fld_value TRANSPORTING NO FIELDS
            WHERE ui_type = zcl_aqo_helper=>mc_ui_table OR ui_type = zcl_aqo_helper=>mc_ui_string.
            EXIT.
          ENDLOOP.

          " No need
          IF sy-subrc <> 0.
            ls_fieldcat->tech = abap_true.
          ENDIF.

        WHEN 'HISTORY_LOGS'.
          lv_column_text = 'Сhange logs'(log).

        WHEN OTHERS.
          ls_fieldcat->tech     = abap_true.
      ENDCASE.

      " As hotspot
      IF lv_column_text IS NOT INITIAL.
        ls_fieldcat->style   = cl_gui_alv_grid=>mc_style_hotspot.
        ls_fieldcat->scrtext_s = ls_fieldcat->scrtext_m = ls_fieldcat->scrtext_l =
         ls_fieldcat->reptext = ls_fieldcat->coltext = lv_column_text.
      ENDIF.

      " Do not edit
      IF lcl_opt=>is_editable( ) <> abap_true.
        CLEAR ls_fieldcat->edit.
      ENDIF.
    ENDLOOP.

    " Show protocol error
    SORT lt_fieldcat STABLE BY col_pos.
    LOOP AT lt_fieldcat REFERENCE INTO ls_fieldcat.
      ls_fieldcat->col_id = ls_fieldcat->col_pos = sy-tabix.
    ENDLOOP.

    " Prepare layout
    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode   = 'C'.
    ls_layout-info_fname = 'COLOR_LINE'.

    " Disable buttons
    IF lcl_opt=>is_editable( ) <> abap_true.
      APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO lt_toolbar_ex.
    ENDIF.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_graph             TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_info              TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_loc_move_row      TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_print             TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_refresh           TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_mb_export            TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_mb_view              TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_views             TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_check             TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_call_master_data  TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_call_more         TO lt_toolbar_ex.
    APPEND cl_gui_alv_grid=>mc_fc_call_lineitems    TO lt_toolbar_ex.

    " Variant
    CONCATENATE p_pack p_opt_id INTO ls_variant-report.
    ls_variant-handle  = '0001'.

    " Events
    " mo_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ).
    SET HANDLER:
     on_toolbar        FOR mo_grid,
     on_user_command   FOR mo_grid,
     on_top_of_page    FOR mo_grid,
     on_hotspot_click  FOR mo_grid,
     on_double_click   FOR mo_grid.

    mo_grid->set_table_for_first_display(
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_ex
      CHANGING
        it_outtab                     = lcl_opt=>mt_fld_value
        it_fieldcatalog               = lt_fieldcat
      EXCEPTIONS
        OTHERS                        = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    " First ALV
    IF lcl_opt=>is_editable( ) = abap_true.
      mo_grid->set_ready_for_input( 1 ).
    ENDIF.

    CREATE OBJECT lr_dd_doc
      EXPORTING
        background_color = cl_dd_document=>col_background_level1.
    lr_dd_doc->initialize_document( ).

    mo_grid->list_processing_events(
      i_event_name = 'TOP_OF_PAGE'
      i_dyndoc_id  = lr_dd_doc ).
  ENDMETHOD.                    "pbo

  METHOD on_toolbar.
    DATA:
      ls_toolbar TYPE stb_button.
    " Only if editable
    CHECK lcl_opt=>is_editable( ) = abap_true.

    ls_toolbar-function  = 'ADD_NEW_FIELD'.
    ls_toolbar-icon      = icon_insert_row.
    ls_toolbar-text      = 'Add new field'(anf).

    INSERT ls_toolbar INTO TABLE e_object->mt_toolbar.
  ENDMETHOD.

  METHOD on_user_command.
    DATA:
      ls_field_value TYPE zcl_aqo_helper=>ts_field_value,
      lo_type        TYPE REF TO cl_abap_datadescr,
      lr_data        TYPE REF TO data,
      lo_err         TYPE REF TO cx_root.
    FIELD-SYMBOLS:
      <lv_data> TYPE any.

    CHECK e_ucomm = 'ADD_NEW_FIELD'.

    " Show screen
    CALL SELECTION-SCREEN 1020 STARTING AT 5 1.
    CHECK p_name IS NOT INITIAL AND p_type IS NOT INITIAL.

    " Capital case
    TRANSLATE:
     p_name TO UPPER CASE,
     p_type TO UPPER CASE.

    " Already exist
    READ TABLE lcl_opt=>mt_fld_value TRANSPORTING NO FIELDS
     WITH KEY name = p_name.
    IF sy-subrc = 0.
      MESSAGE s002(zaqo_message) WITH p_name DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    TRY.
        " Create by text description
        lo_type = zcl_aqo_helper=>create_type_descr( iv_rollname = p_type ).
        CREATE DATA lr_data TYPE HANDLE lo_type.

        " Get full description
        ASSIGN lr_data->* TO <lv_data>.
        ls_field_value-field_desc = zcl_aqo_helper=>get_field_desc(
         iv_field_name = p_name
         iv_data       = <lv_data> ).

        lcl_opt=>add_one_field(
         is_field_value = ls_field_value
         ir_data        = lr_data ).
      CATCH cx_root INTO lo_err.                         "#EC CATCH_ALL
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    mo_grid->refresh_table_display( ).
    MESSAGE s032(zaqo_message) WITH p_name.
  ENDMETHOD.

  METHOD pbo_1020.
    DATA:
      lt_exclude TYPE STANDARD TABLE OF syucomm.

    " Chaneg title
    SET TITLEBAR 'ST_MAIN' WITH 'Add new field'(anf).

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = 'OK_CANCEL'
      TABLES
        p_exclude = lt_exclude.

    " Make like obligatory
    LOOP AT SCREEN.
      CHECK screen-group1 = 'OBL'.
      screen-required = '2'. " recommended
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD pai_1020.
    DATA:
      lv_exit TYPE abap_bool.

    CASE cv_cmd.
      WHEN 'OK'.
        IF p_name IS INITIAL OR p_type IS INITIAL.
          MESSAGE e055(00).
          RETURN.
        ENDIF.
        lv_exit = abap_true.

      WHEN 'CANCEL'.
        lv_exit = abap_true.
        MESSAGE s130(ed) WITH 'Add new field'(anf) DISPLAY LIKE 'E'.
        CLEAR:
         p_name,
         p_type.
    ENDCASE.

    IF lv_exit = abap_true.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDMETHOD.

  METHOD on_top_of_page.
    DATA:
      lr_table      TYPE REF TO cl_dd_table_element,
      lr_col_01     TYPE REF TO cl_dd_area,
      lr_col_02     TYPE REF TO cl_dd_area,
      lr_struc_desc TYPE REF TO cl_abap_structdescr,
      ls_comp       TYPE REF TO abap_compdescr,
      lv_text       TYPE sdydo_text_element,
      lt_option_db  TYPE STANDARD TABLE OF ztaqo_option WITH DEFAULT KEY,
      lt_fieldcat   TYPE lvc_t_fcat,
      ls_fieldcat   TYPE REF TO lvc_s_fcat.
    FIELD-SYMBOLS:
      <lv_fld>          TYPE any.

    " Options' description
    e_dyndoc_id->add_table( EXPORTING no_of_columns = 2
                                      border        = '0'
                                      width         = '70%'
                            IMPORTING table         = lr_table ).

    lr_table->add_column(
      EXPORTING
        width   = '35%'
      IMPORTING
        column  = lr_col_01 ).
    lr_table->add_column(
      EXPORTING
        width   = '65%'
      IMPORTING
        column  = lr_col_02 ).

    " Fields of cluster
    lr_struc_desc ?= cl_abap_structdescr=>describe_by_data( lcl_opt=>mo_option->ms_db_item ).
    zcl_aqo_helper=>create_field_catalog(
     EXPORTING
       iv_sort     = abap_true
     IMPORTING
       et_fieldcat = lt_fieldcat
     CHANGING
       ct_table    = lt_option_db ).

    LOOP AT lr_struc_desc->components REFERENCE INTO ls_comp WHERE
        name = 'PACKAGE_ID'        OR                       "#EC NOTEXT
        name = 'OPTION_ID'         OR                       "#EC NOTEXT
        name = 'CREATED_DATE'      OR                       "#EC NOTEXT
        name = 'CREATED_UNAME'     OR                       "#EC NOTEXT
        name = 'CREATED_NAME_TEXT' OR                       "#EC NOTEXT
        name = 'DESCRIPTION'       OR                       "#EC NOTEXT
        name = 'PREV_VALUE_CNT'.                            "#EC NOTEXT

      " Get text
      READ TABLE lt_fieldcat REFERENCE INTO ls_fieldcat BINARY SEARCH
       WITH KEY fieldname = ls_comp->name.
      CHECK sy-subrc = 0.

      " Text
      CONCATENATE ls_fieldcat->scrtext_l `:` INTO lv_text.
      lr_col_01->add_text(
       text         = lv_text
       sap_emphasis = cl_dd_document=>strong ).

      " Value
      ASSIGN COMPONENT ls_comp->name OF STRUCTURE lcl_opt=>mo_option->ms_db_item TO <lv_fld>.
      WRITE <lv_fld> TO lv_text.                        "#EC WRITE_MOVE
      lr_col_02->add_text( text = lv_text ).

      lr_table->new_row( ).
    ENDLOOP.

    " And show data
    e_dyndoc_id->merge_document( ).
    e_dyndoc_id->display_document(
     reuse_control      = abap_true
     parent = mo_header_cont ).
  ENDMETHOD.

  METHOD on_hotspot_click.
    DATA:
      ls_fld_value      TYPE REF TO lcl_opt=>ts_fld_value,
      lo_table_comp_alv TYPE REF TO lcl_table_comp_alv,
      lo_table_alv      TYPE REF TO lcl_table_alv.

    " Current item
    READ TABLE lcl_opt=>mt_fld_value REFERENCE INTO ls_fld_value INDEX es_row_no-row_id.
    CHECK sy-subrc = 0.

    CASE e_column_id.
      WHEN 'CATALOG'.
        CHECK ls_fld_value->ui_type = zcl_aqo_helper=>mc_ui_table.
        lo_table_comp_alv = lcl_table_comp_alv=>get_instance( ).
        lo_table_comp_alv->call_screen( ls_fld_value ).

      WHEN 'VALUE_BUTTON'.
        " Only for tables
        CASE ls_fld_value->ui_type.
          WHEN zcl_aqo_helper=>mc_ui_table.
            lo_table_alv = lcl_table_alv=>get_instance( 1 ).
            lo_table_alv->call_screen( ls_fld_value ).

          WHEN zcl_aqo_helper=>mc_ui_string.
            go_string_memo = lcl_string_memo=>get_instance( 1 ).
            go_string_memo->call_screen( ls_fld_value ).
        ENDCASE.

      WHEN 'HISTORY_LOGS'.
        CHECK lines( ls_fld_value->value ) > 1.
        go_logs_alv = lcl_logs_alv=>get_instance( ).
        go_logs_alv->call_screen( ls_fld_value ).

    ENDCASE.
  ENDMETHOD.

  METHOD on_double_click.
    DATA:
      ls_fld_value TYPE REF TO lcl_opt=>ts_fld_value,
      lv_tab       TYPE dd02v-tabname,
      lv_fld       TYPE d021s-fnam.
    CHECK e_column = 'ROLLNAME'.

    " Read current item
    READ TABLE lcl_opt=>mt_fld_value REFERENCE INTO ls_fld_value INDEX e_row-index.
    CHECK sy-subrc = 0.

    " Check is table and field name
    CHECK ls_fld_value->rollname CP '*-*' AND zcl_aqo_helper=>create_type_descr( iv_rollname = ls_fld_value->rollname ) IS NOT INITIAL.

    " Drill down
    SPLIT ls_fld_value->rollname AT '-' INTO lv_tab lv_fld.
    CHECK sy-subrc = 0.

    CALL FUNCTION 'RS_DD_STRU_EDIT'
      EXPORTING
        objname   = lv_tab
        fname     = lv_fld
        edit_mode = 'S'
      EXCEPTIONS
        OTHERS    = 5.

    " Show as error
    CHECK sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDMETHOD.

  METHOD pai.
    DATA:
      lv_cmd          TYPE syucomm.

    " Save & clear
    lv_cmd = cv_cmd.
    CLEAR cv_cmd.

    " Write data back
    mo_grid->check_changed_data( ).

    " Custom checks
    CHECK data_check( ) = abap_true OR lv_cmd = 'VIEW'.

    CASE lv_cmd.
      WHEN 'SAVE' OR 'TRANSPORT' OR 'DEL_OPT'.
        lcl_opt=>pai(
         CHANGING
           cv_cmd = lv_cmd ).

      WHEN 'VIEW' OR 'CHANGE'.
        me->sel_screen_show( ).

      WHEN 'CLNT_COPY'.
        me->copy_2_client( ).

      WHEN 'FIND_REF'.
        me->find_ref( ).

    ENDCASE.
  ENDMETHOD.                    "pai

  METHOD on_data_changed.
    DATA:
      ls_fld_value TYPE REF TO lcl_opt=>ts_fld_value,
      lv_row       TYPE i,
      lt_unq       TYPE zcl_aqo_helper=>tt_unique_type,
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

      IF ls_fld_value->ui_type <> zcl_aqo_helper=>mc_ui_table AND
         ls_fld_value->ui_type <> zcl_aqo_helper=>mc_ui_string.

        lv_unq = ls_fld_value->rollname.
        INSERT lv_unq INTO TABLE lt_unq.

        " Have unique name
        IF sy-subrc <> 0 OR

           " Is table and field name
           ls_fld_value->rollname NP '*-*' OR

           " In dictionary
           zcl_aqo_helper=>create_type_descr( iv_rollname = ls_fld_value->rollname ) IS INITIAL.

          MESSAGE e002(zaqo_message) WITH ls_fld_value->name INTO sy-msgli.
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
      lr_data_changed TYPE REF TO cl_alv_changed_data_protocol,
      lr_msglist      TYPE REF TO if_reca_message_list,
      ls_message      TYPE REF TO lvc_s_msg1.

    " Create protocol
    CREATE OBJECT lr_data_changed
      EXPORTING
        i_calling_alv = mo_grid.

    " Use defined catalog
    IF mo_grid IS NOT INITIAL.
      mo_grid->get_backend_fieldcatalog(
       IMPORTING
         et_fieldcatalog = lr_data_changed->mt_fieldcatalog ).
    ENDIF.

    " Customs checks
    on_data_changed(
     sender          = mo_grid
     er_data_changed = lr_data_changed ).

    " Show without grid
    IF mo_grid IS INITIAL AND lr_data_changed->mt_protocol IS NOT INITIAL.
      " lr_data_changed->display_protocol( ).
      lr_msglist  = cf_reca_message_list=>create( ).
      LOOP AT lr_data_changed->mt_protocol REFERENCE INTO ls_message.
        lr_msglist->add( id_msgty = ls_message->msgty
                         id_msgid = ls_message->msgid
                         id_msgno = ls_message->msgno
                         id_msgv1 = ls_message->msgv1
                         id_msgv2 = ls_message->msgv2
                         id_msgv3 = ls_message->msgv3
                         id_msgv4 = ls_message->msgv4 ).
      ENDLOOP.

      CALL FUNCTION 'RECA_GUI_MSGLIST_POPUP'
        EXPORTING
          io_msglist = lr_msglist
          id_title   = 'Fix errors in DEV mandt first'(dis)
          if_popup   = abap_false.
    ENDIF.

    CHECK lr_data_changed->mt_protocol IS INITIAL.

    " Checks ok
    rv_ok = abap_true.
  ENDMETHOD.

  METHOD sel_screen_show.
    DATA:
      lt_event         TYPE STANDARD TABLE OF rsdsevents,
      ls_event         TYPE REF TO rsdsevents,
      ls_fld_value     TYPE REF TO lcl_opt=>ts_fld_value,
      lv_sel_id        TYPE rsdynsel-selid,
      lt_init          TYPE rsds_texpr,
      ls_restrict      TYPE sscr_restrict_ds,
      ls_list_tab      TYPE REF TO sscr_opt_list,
      lt_scr_fld       TYPE STANDARD TABLE OF rsdsfields,
      ls_scr_fld       TYPE rsdsfields,
      lt_scr_fld_txt   TYPE STANDARD TABLE OF rsdstexts,
      ls_scr_fld_txt   TYPE rsdstexts,
      ls_scr_fld_value TYPE sscr_ass_ds,
      lt_scr_fld_evt   TYPE STANDARD TABLE OF rsdsevflds,
      ls_scr_fld_evt   TYPE rsdsevflds,
      lt_range_ret     TYPE rsds_trange,
      lt_range         TYPE rsds_trange,
      ls_range         TYPE REF TO rsds_range,
      ls_range_sub1    TYPE REF TO rsds_frange,
      ls_range_sub2    TYPE REF TO rsdsselopt,
      lv_tabfld        TYPE string,
      lv_name          TYPE zcl_aqo_helper=>ts_field_value-rollname,
      lr_data          TYPE REF TO data,
      lt_unique_type   TYPE zcl_aqo_helper=>tt_unique_type,
      lr_unique_type   TYPE REF TO zcl_aqo_helper=>tt_unique_type.
    FIELD-SYMBOLS:
      <lt_val> TYPE STANDARD TABLE,
      <ls_val> TYPE any,
      <lv_val> TYPE any.

    " events
    APPEND INITIAL LINE TO lt_event REFERENCE INTO ls_event.
    ls_event->event = 'O'. " AT SELECTION-SCREEN OUTPUT
    ls_event->prog  = sy-cprog.
    ls_event->form  = 'SEL_SCREEN_PBO'.

    APPEND INITIAL LINE TO lt_event REFERENCE INTO ls_event.
    ls_event->event = 'A'. " AT SELECTION-SCREEN
    ls_event->prog  = sy-cprog.
    ls_event->form  = 'SEL_SCREEN_PAI'.

    APPEND INITIAL LINE TO ls_restrict-opt_list_tab REFERENCE INTO ls_list_tab.
    ls_list_tab->name       = 'JUST_EQ'.
    ls_list_tab->options-eq = abap_true.

    " Text and edit
    CLEAR lt_unique_type.
    GET REFERENCE OF lt_unique_type INTO lr_unique_type.

    LOOP AT lcl_opt=>mt_fld_value REFERENCE INTO ls_fld_value.
      CLEAR:
       ls_scr_fld,
       ls_scr_fld_txt,
       ls_scr_fld_value,
       ls_scr_fld_evt.

      " General options
      ls_scr_fld_value-kind = 'S'.
      ls_scr_fld_txt-text   = ls_fld_value->label.

      " Find for table
      IF ls_fld_value->ui_type = zcl_aqo_helper=>mc_ui_table OR ls_fld_value->ui_type = zcl_aqo_helper=>mc_ui_string.
        lv_tabfld = 'SYINDEX'.
        zcl_aqo_helper=>find_table_fieldname(
         EXPORTING
          iv_name        = ls_fld_value->name
          ir_unique_type = lr_unique_type
         CHANGING
          cv_rollname    = lv_tabfld ).

        " Option
        ls_scr_fld_value-sg_main       = '*'.
        ls_scr_fld_value-sg_addy       = ' '.
      ELSE.
        lv_tabfld = ls_fld_value->rollname.

        CASE ls_fld_value->ui_type.
          WHEN zcl_aqo_helper=>mc_ui_range.
            " Option
            ls_scr_fld_value-sg_main       = '*'.
            ls_scr_fld_value-sg_addy       = ' '.
            " ls_scr_fld_value-op_main       = JUST_EQ | NOINTERVLS | NOPATTERN

          WHEN zcl_aqo_helper=>mc_ui_table OR zcl_aqo_helper=>mc_ui_string.

          WHEN OTHERS. "zcl_aqo_helper=>mc_ui_parameter.
            " Parameter
            ls_scr_fld_value-sg_main       = 'I'.
            ls_scr_fld_value-sg_addy       = 'N'.
            ls_scr_fld_value-op_main       = 'JUST_EQ'.

        ENDCASE.
      ENDIF.

      " Fields
      IF lv_tabfld CP '*-*'.
        SPLIT lv_tabfld AT '-' INTO
          ls_scr_fld-tablename
          ls_scr_fld-fieldname.
        INSERT lv_tabfld INTO TABLE lt_unique_type.
      ELSE.
        MESSAGE s002(zaqo_message) WITH lv_tabfld DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      " Fields
      APPEND ls_scr_fld TO lt_scr_fld.

      " Text
      ls_scr_fld_txt-tablename  = ls_scr_fld-tablename.
      ls_scr_fld_txt-fieldname  = ls_scr_fld-fieldname.
      APPEND ls_scr_fld_txt TO lt_scr_fld_txt.

      " Option
      ls_scr_fld_value-tablename   = ls_scr_fld-tablename.
      ls_scr_fld_value-fieldname   = ls_scr_fld-fieldname.
      APPEND ls_scr_fld_value TO ls_restrict-ass_tab.

      " Event
      IF ls_scr_fld_evt IS NOT INITIAL.
        ls_scr_fld_evt-tablename   = ls_scr_fld-tablename.
        ls_scr_fld_evt-fieldname   = ls_scr_fld-fieldname.
        APPEND ls_scr_fld_evt TO lt_scr_fld_evt.
      ENDIF.

************************
      " Values
************************
      CHECK ls_fld_value->ui_type <> zcl_aqo_helper=>mc_ui_table AND
            ls_fld_value->ui_type <> zcl_aqo_helper=>mc_ui_string.

      APPEND INITIAL LINE TO lt_range REFERENCE INTO ls_range.
      ls_range->tablename = ls_scr_fld-tablename.

      APPEND INITIAL LINE TO ls_range->frange_t REFERENCE INTO ls_range_sub1.
      ls_range_sub1->fieldname = ls_scr_fld-fieldname.

      lr_data = ls_fld_value->cur_value.
      CASE ls_fld_value->ui_type.
        WHEN zcl_aqo_helper=>mc_ui_range.
          ASSIGN lr_data->* TO <lt_val>.

          LOOP AT <lt_val> ASSIGNING <ls_val>.
            APPEND INITIAL LINE TO ls_range_sub1->selopt_t REFERENCE INTO ls_range_sub2.
            ls_range_sub2->sign   = get_text_value( is_data = <ls_val> iv_field = 'SIGN' ).
            ls_range_sub2->option = get_text_value( is_data = <ls_val> iv_field = 'OPTION' ).
            ls_range_sub2->low    = get_text_value( is_data = <ls_val> iv_field = 'LOW' ).
            ls_range_sub2->high   = get_text_value( is_data = <ls_val> iv_field = 'HIGH' ).
          ENDLOOP.

        WHEN zcl_aqo_helper=>mc_ui_table OR zcl_aqo_helper=>mc_ui_string.

        WHEN OTHERS. " zcl_aqo_helper=>mc_ui_parameter
          ASSIGN lr_data->* TO <lv_val>.
          " No need if empty
          CHECK sy-subrc = 0 AND <lv_val> IS NOT INITIAL.
          APPEND INITIAL LINE TO ls_range_sub1->selopt_t REFERENCE INTO ls_range_sub2.
          ls_range_sub2->sign   = 'I'.
          ls_range_sub2->option = 'EQ'.
          ls_range_sub2->low    = get_text_value( is_data = <lv_val>  ).
      ENDCASE.
    ENDLOOP.

    " Transform to internal format
    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_EX'
      EXPORTING
        field_ranges = lt_range[]
      IMPORTING
        expressions  = lt_init[].

    " initialization
    CALL FUNCTION 'FREE_SELECTIONS_INIT'
      EXPORTING
        kind                     = 'F'
        expressions              = lt_init
        restriction              = ls_restrict
      IMPORTING
        selection_id             = lv_sel_id
      TABLES
        fields_tab               = lt_scr_fld
        field_texts              = lt_scr_fld_txt
        events                   = lt_event
        event_fields             = lt_scr_fld_evt
      EXCEPTIONS
        fields_incomplete        = 1
        fields_no_join           = 2
        field_not_found          = 3
        no_tables                = 4
        table_not_found          = 5
        expression_not_supported = 6
        incorrect_expression     = 7
        illegal_kind             = 8
        area_not_found           = 9
        inconsistent_area        = 10
        kind_f_no_fields_left    = 11
        kind_f_no_fields         = 12
        too_many_fields          = 13
        dup_field                = 14
        field_no_type            = 15
        field_ill_type           = 16
        dup_event_field          = 17
        node_not_in_ldb          = 18
        area_no_field            = 19
        OTHERS                   = 20.
    IF sy-subrc <> 0.
      MESSAGE s003(zaqo_message) DISPLAY LIKE 'E' WITH 'FREE_SELECTIONS_INIT' sy-subrc.
      RETURN.
    ENDIF.

    " call dialog.
    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id    = lv_sel_id
        title           = 'Maintenance parameters'(tit)
        frame_text      = 'Parameters'(opt)
        as_window       = abap_false
        start_col       = 30
        status          = 0
        just_display    = space
        tree_visible    = space
      IMPORTING
        field_ranges    = lt_range_ret
      TABLES
        fields_tab      = lt_scr_fld
      EXCEPTIONS
        internal_error  = 1
        no_action       = 2
        selid_not_found = 3
        illegal_status  = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
      IF sy-subrc <> 2.
        MESSAGE s003(zaqo_message) DISPLAY LIKE 'E' WITH 'FREE_SELECTIONS_DIALOG' sy-subrc.
      ENDIF.

      RETURN.
    ENDIF.

********************
    " Write back one by one
    LOOP AT lt_range_ret REFERENCE INTO ls_range.

      LOOP AT ls_range->frange_t REFERENCE INTO ls_range_sub1.
        CONCATENATE ls_range->tablename '-' ls_range_sub1->fieldname INTO lv_name.

        " Read by key
        READ TABLE lcl_opt=>mt_fld_value REFERENCE INTO ls_fld_value
         WITH KEY rollname = lv_name.
        CHECK sy-subrc = 0.

        lr_data = ls_fld_value->cur_value.
        CASE ls_fld_value->ui_type.
            " Write back a range
          WHEN zcl_aqo_helper=>mc_ui_range.
            ASSIGN lr_data->* TO <lt_val>.
            CHECK sy-subrc = 0.

            CLEAR <lt_val>.
            LOOP AT ls_range_sub1->selopt_t REFERENCE INTO ls_range_sub2.
              APPEND INITIAL LINE TO <lt_val> ASSIGNING <lv_val>.
              MOVE-CORRESPONDING ls_range_sub2->* TO <lv_val>.
            ENDLOOP.

          WHEN zcl_aqo_helper=>mc_ui_table OR zcl_aqo_helper=>mc_ui_string.

            " Write back a parameter
          WHEN OTHERS. "zcl_aqo_helper=>mc_ui_parameter.
            ASSIGN lr_data->* TO <lv_val>.
            CHECK sy-subrc = 0.

            " ls_range_sub1->selopt_t[] could be empty
            CLEAR <lv_val>.
            READ TABLE ls_range_sub1->selopt_t REFERENCE INTO ls_range_sub2 INDEX 1.
            CHECK sy-subrc = 0.

            <lv_val> = ls_range_sub2->low.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    " And save
    lcl_opt=>do_save( iv_mandt = sy-mandt ).
  ENDMETHOD.                    "sel_screen_show

  METHOD get_text_value.
    DATA:
      lv_kind  TYPE abap_typekind.
    FIELD-SYMBOLS:
      <lv_val> TYPE any.

    IF iv_field IS NOT INITIAL.
      ASSIGN COMPONENT iv_field OF STRUCTURE is_data TO <lv_val>.
    ELSE.
      ASSIGN is_data TO <lv_val>.
    ENDIF.

    DESCRIBE FIELD <lv_val> TYPE lv_kind.

    CASE lv_kind.
      WHEN  cl_abap_typedescr=>typekind_packed OR cl_abap_typedescr=>typekind_float OR
            cl_abap_typedescr=>typekind_num OR cl_abap_typedescr=>typekind_int OR
            cl_abap_typedescr=>typekind_int1 OR cl_abap_typedescr=>typekind_int2 OR
            cl_abap_typedescr=>typekind_numeric OR '/' OR 'a' OR 'e'. "DECFLOAT

        IF <lv_val> = 0.
          rv_text = '0'.
        ELSE.
          WRITE <lv_val> TO rv_text EXPONENT 0 NO-GROUPING LEFT-JUSTIFIED.
          REPLACE FIRST OCCURRENCE OF ',' IN rv_text WITH '.'.
        ENDIF.
      WHEN OTHERS.
        rv_text = <lv_val>.
    ENDCASE.
  ENDMETHOD.

  METHOD copy_2_client.
    DATA : lt_field TYPE STANDARD TABLE OF sval,
           ls_field TYPE REF TO sval,
           lv_rc    TYPE char1,
           lv_mandt TYPE sy-mandt.

    APPEND INITIAL LINE TO lt_field REFERENCE INTO ls_field.
    ls_field->fieldname = 'MANDT'.
    ls_field->tabname   = 'T001'.
    ls_field->value     = sy-mandt.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Specify the client number'(cln)
      IMPORTING
        returncode      = lv_rc
      TABLES
        fields          = lt_field
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0 OR lv_rc = 'A'.
      MESSAGE s118(ed) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    READ TABLE lt_field REFERENCE INTO ls_field INDEX 1.
    CHECK sy-subrc = 0.
    lv_mandt = ls_field->value.

    "check client
    CHECK lv_mandt <> sy-mandt.

**   check unsaved data exist
*    IF check_unsaved_data( ) EQ abap_true.
**     save data
*      data_save( ).
*    ENDIF.

    lcl_opt=>do_save( iv_mandt = lv_mandt ).
  ENDMETHOD.

  METHOD find_ref.
    DATA:
      lo_where_used TYPE REF TO lcl_where_used.
    lo_where_used = lcl_where_used=>get_instance( ).
    lo_where_used->call_screen( ).
  ENDMETHOD.

  METHOD export.
    DATA:
      lv_title     TYPE string,
      lv_file_name TYPE string,
      lv_path      TYPE string,
      lv_filename  TYPE string,
      lv_fullpath  TYPE string.

    lv_title     = 'Save option values'(sov).
    CONCATENATE p_pack `-` p_opt_id `-` sy-mandt `-` sy-datum `-` sy-uzeit `.aqob` INTO lv_file_name.
    cl_gui_frontend_services=>file_save_dialog(
     EXPORTING
       window_title      = lv_title
       default_file_name = lv_file_name
     CHANGING
       path         = lv_path
       filename     = lv_filename
       fullpath     = lv_fullpath
     EXCEPTIONS
       OTHERS       = 1 ).
    CHECK sy-subrc = 0 AND lv_fullpath IS NOT INITIAL.

    " Save to file
    zcl_aqo_helper=>download(
     iv_xcontent = lcl_opt=>mo_option->ms_db_item-fields
     iv_filename = lv_fullpath ).
  ENDMETHOD.

  METHOD import.
    DATA:
      lt_file  TYPE filetable,
      ls_file  TYPE REF TO file_table,
      lv_file  TYPE string,
      lv_rc    TYPE i,
      lt_data  TYPE solix_tab,
      lv_len   TYPE i,
      lv_xdata TYPE xstring.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        " window_title   =
        multiselection    = abap_false
        file_filter       = '*.aqob'
        default_extension = 'aqob'
      CHANGING
        file_table        = lt_file
        rc                = lv_rc ).
    CHECK lt_file[] IS NOT INITIAL.

    " 1 file only
    READ TABLE lt_file REFERENCE INTO ls_file INDEX 1.
    CHECK sy-subrc = 0.

    lv_file = ls_file->filename.
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename   = lv_file
        filetype   = 'BIN'
      IMPORTING
        filelength = lv_len
      TABLES
        data_tab   = lt_data
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    lv_xdata = zcl_aqo_helper=>binary_to_xstring(
     it_table  = lt_data
     iv_length = lv_len ).

    do_update(
     iv_set    = `FIELDS = IV_FIELDS`
     iv_fields = lv_xdata ).
  ENDMETHOD.

  METHOD change_description.
    " Previous values
    p_o_desc = lcl_opt=>mo_option->ms_db_item-description.
    p_o_prev = lcl_opt=>mo_option->ms_db_item-prev_value_cnt.

    " Show screen
    CALL SELECTION-SCREEN 1030 STARTING AT 5 1.
    CHECK p_o_desc IS NOT INITIAL AND p_o_prev IS NOT INITIAL.

    do_update(
     iv_set         = `DESCRIPTION = IV_DESCRIPTION PREV_VALUE_CNT = IV_PREV_COUNT`
     iv_description = p_o_desc
     iv_prev_count  = p_o_prev ).
  ENDMETHOD.

  METHOD pbo_1030.
    DATA:
      lt_exclude TYPE STANDARD TABLE OF syucomm.

    " Chaneg title
    SET TITLEBAR 'ST_MAIN' WITH 'Enter option description'(eod).

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = 'OK_CANCEL'
      TABLES
        p_exclude = lt_exclude.

    " Make like obligatory
    LOOP AT SCREEN.
      CHECK screen-group1 = 'OBL'.
      screen-required = '2'. " recommended
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD pai_1030.
    DATA:
      lv_exit TYPE abap_bool.

    CASE cv_cmd.
      WHEN 'OK'.
        IF p_o_desc IS INITIAL OR p_o_prev IS INITIAL.
          MESSAGE e055(00).
          RETURN.
        ENDIF.
        IF p_o_prev > 5 OR p_o_prev < 1.
          MESSAGE 'Previous values count have to be from 1 to 5'(f15) TYPE 'E'.
          RETURN.
        ENDIF.

        lv_exit = abap_true.

      WHEN 'CANCEL'.
        lv_exit = abap_true.
        CLEAR:
         p_o_desc,
         p_o_prev.
    ENDCASE.

    IF lv_exit = abap_true.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDMETHOD.

  METHOD do_update.
    UPDATE ztaqo_option
     SET (iv_set)
    WHERE package_id = p_pack
      AND option_id  = p_opt_id.

    IF sy-subrc = 0.
      MESSAGE 'Data updated'(upd) TYPE 'S'.
    ELSE.
      MESSAGE 'Error during updating!'(edu) TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  go_fld_value_alv = lcl_fld_value_alv=>get_instance( ).
  go_fld_value_alv->pbo( ).
ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  go_fld_value_alv = lcl_fld_value_alv=>get_instance( ).
  go_fld_value_alv->pai(
   CHANGING
     cv_cmd = gv_ok_code ).
ENDMODULE.
