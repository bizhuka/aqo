*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_tree DEFINITION FINAL FRIENDS zcl_eui_event_caller.
  PUBLIC SECTION.
    DATA:
      BEGIN OF ms_node,
        rec_opened   TYPE lvc_nkey,
        rec_created  TYPE lvc_nkey,
        struc_search TYPE lvc_nkey,
        free_search  TYPE lvc_nkey,
        new_option   TYPE lvc_nkey VALUE '~~~', " Can be hidden
        user_prefs   TYPE lvc_nkey,
      END OF ms_node.

    METHODS:
      constructor
        IMPORTING
          io_prefs TYPE REF TO lcl_user_prefs,

      make_gui,

      fill,

      delete_from
        IMPORTING
          iv_parent_node TYPE lvc_nkey
          is_db_key      TYPE ts_db_key OPTIONAL
          iv_from        TYPE i         OPTIONAL
          iv_refresh     TYPE abap_bool OPTIONAL,

      f4_free_search,

      add_opened
        IMPORTING
          is_db_key TYPE ts_db_key
          iv_insert TYPE abap_bool DEFAULT abap_true.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_tree_data,
        option_id TYPE ztaqo_option-option_id,
      END OF ts_tree_data,
      tt_tree_data TYPE STANDARD TABLE OF ts_tree_data WITH DEFAULT KEY,

      BEGIN OF ts_node_package,
        devclass    TYPE tdevc-devclass,
        parent_node TYPE lvc_nkey,
      END OF ts_node_package,
      tt_node_package TYPE SORTED TABLE OF ts_node_package WITH UNIQUE KEY devclass.

    DATA:
      mo_prefs     TYPE REF TO lcl_user_prefs,
      mt_tree_data TYPE REF TO tt_tree_data,
      mo_gui_tree  TYPE REF TO cl_gui_alv_tree.

    METHODS:
      _fill_tree_with_created_opt,

      _on_link_click FOR EVENT link_click OF cl_gui_alv_tree
        IMPORTING
          node_key,

      _show_f4
        RETURNING VALUE(rs_db_key) TYPE ts_db_key,

      _new_option
        RETURNING VALUE(rs_db_key) TYPE ts_db_key,
      _on_pai_new_option FOR EVENT pai_event OF zif_eui_manager
        IMPORTING
          sender
          iv_command
          cv_close,

      _fill_struc_search,
      _insert_parent_package
        IMPORTING
          iv_package TYPE tdevc-devclass
        EXPORTING
          ev_node    TYPE lvc_nkey
        CHANGING
          ct_package TYPE tt_node_package,

      _expand_invert_node
        IMPORTING
          iv_node  TYPE lvc_nkey
          iv_level TYPE i         DEFAULT 1,

      _add_node
        IMPORTING
                  i_node_text    TYPE csequence
                  iv_option_id   TYPE ztaqo_option-option_id  OPTIONAL
                  i_relat_node   TYPE lvc_nkey                OPTIONAL
                  i_relationship TYPE i                       DEFAULT cl_gui_column_tree=>relat_last_child
                  is_layout      TYPE lvc_s_layn              OPTIONAL
                  iv_item_icon   TYPE icon_d                  OPTIONAL
                  iv_item_class  TYPE i                       OPTIONAL
        RETURNING VALUE(rv_node) TYPE lvc_nkey,

      _add_parent_node
        IMPORTING
                  i_node_text    TYPE csequence
                  iv_item_icon   TYPE icon_d                  OPTIONAL
                  iv_layout_icon TYPE icon_d                  OPTIONAL
                  i_relat_node   TYPE lvc_nkey                OPTIONAL
        RETURNING VALUE(rv_node) TYPE lvc_nkey,

      _add_option_node
        IMPORTING
          is_db_key      TYPE ts_db_key
          iv_parent      TYPE lvc_nkey
          i_relationship TYPE i DEFAULT cl_gui_column_tree=>relat_last_child.
ENDCLASS.

CLASS lcl_tree IMPLEMENTATION.
  METHOD constructor.
    mo_prefs = io_prefs.
  ENDMETHOD.

  METHOD make_gui.
    " 1 time only
    CHECK mt_tree_data IS INITIAL.
    CREATE DATA mt_tree_data.

    DATA ls_header TYPE treev_hhdr.
    ls_header-heading = 'Options'(opt).
    ls_header-width   = 35.                              "#EC NUMBER_OK
    ls_header-t_image = icon_tree.

    DATA lo_eui_tree TYPE REF TO zcl_eui_tree.
    CREATE OBJECT lo_eui_tree
      EXPORTING
        ir_table       = mt_tree_data
        is_header      = ls_header
        no_toolbar     = abap_true
        no_html_header = abap_true.

    DATA lo_doc_container TYPE REF TO cl_gui_docking_container.
    CREATE OBJECT lo_doc_container
      EXPORTING
        dynnr     = sy-dynnr
        side      = cl_gui_docking_container=>dock_at_left
        extension = 330. "#EC NUMBER_OK
    lo_eui_tree->add_handler( me ).
    lo_eui_tree->pbo( io_container = lo_doc_container ).
    mo_gui_tree = lo_eui_tree->get_tree( ).
  ENDMETHOD.

  METHOD fill.
    ms_node-rec_opened = _add_parent_node(
      i_node_text  = 'Recently opened'(reo)
      iv_item_icon = icon_time ).
    DATA lr_db_key TYPE REF TO ts_db_key.
    LOOP AT mo_prefs->t_opened[] REFERENCE INTO lr_db_key.
      _add_option_node( is_db_key = lr_db_key->*
                        iv_parent = ms_node-rec_opened ).
    ENDLOOP.

    _fill_tree_with_created_opt( ).

    ms_node-struc_search = _add_parent_node(
      i_node_text    = 'Structure Search'(sts)
      iv_layout_icon = icon_tree "  iv_item_icon = icon_tree

     ).

    ms_node-free_search = _add_parent_node(
      i_node_text    = 'Free Search'(frs)
      iv_layout_icon = icon_search
    ).

    IF go_editor->mv_is_dev = abap_true.
      ms_node-new_option = _add_parent_node(
        i_node_text    = 'New option'(neo)
        iv_layout_icon = icon_create ).
    ENDIF.

    ms_node-user_prefs = _add_parent_node(
     i_node_text    = 'User preferences'(upf)
     iv_layout_icon = icon_activity ).

    " update gui
    _expand_invert_node( iv_node = ms_node-rec_opened ).
    _expand_invert_node( iv_node = ms_node-rec_created ).
    mo_gui_tree->frontend_update( ).
  ENDMETHOD.

  METHOD f4_free_search.
    _on_link_click( node_key = ms_node-free_search ).
  ENDMETHOD.

  METHOD _on_link_click.
    DATA ls_command TYPE ts_command.
    " By default
    ls_command-ucomm = mc_pai_cmd-open_option.

    CASE node_key.
      WHEN ms_node-free_search.
        ls_command-db_key = _show_f4( ).

      WHEN ms_node-struc_search.
        _fill_struc_search( ).

      WHEN ms_node-new_option.
        ls_command-db_key = _new_option( ).
        ls_command-ucomm  = mc_pai_cmd-new_option.

      WHEN ms_node-user_prefs.
        ls_command-ucomm      = mc_pai_cmd-show_user_prefs.
        ls_command-package_id = ls_command-option_id = '-'.

      WHEN OTHERS.
        DATA: ls_tree_data  TYPE ts_tree_data,
              lv_package_id TYPE lvc_value.
        mo_gui_tree->get_outtab_line( EXPORTING  i_node_key    = node_key
                                      IMPORTING  e_outtab_line = ls_tree_data
                                                 e_node_text   = lv_package_id
                                      EXCEPTIONS OTHERS        = 1 ).
        CHECK sy-subrc = 0.

        " collapse or expand
        IF ls_tree_data-option_id IS INITIAL.
          _expand_invert_node( node_key ).
          RETURN.
        ENDIF.

        ls_command-package_id = lv_package_id.
        ls_command-option_id  = ls_tree_data-option_id.
    ENDCASE.

    CHECK ls_command-db_key IS NOT INITIAL.
    zcl_aqo_helper=>exchange_command( is_command = ls_command ).
  ENDMETHOD.

  METHOD _show_f4.
    DATA lt_ret  TYPE STANDARD TABLE OF ddshretval.
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname    = ''             " No need returns all fields in SH exit   "#EC NOTEXT
        fieldname  = ''                                     "#EC NOTEXT
        searchhelp = 'ZHAQO_OPTION'                         "#EC NOTEXT
        dynpprog   = sy-repid
        dynpnr     = sy-dynnr
      TABLES
        return_tab = lt_ret
      EXCEPTIONS
        OTHERS     = 0.

    DATA ls_ret  TYPE REF TO ddshretval.
    LOOP AT lt_ret REFERENCE INTO ls_ret WHERE fieldname = 'PACKAGE_ID' OR fieldname = 'OPTION_ID'.
      CASE ls_ret->fieldname.
        WHEN 'PACKAGE_ID'.                                  "#EC NOTEXT
          rs_db_key-package_id = ls_ret->fieldval.
        WHEN 'OPTION_ID'.                                   "#EC NOTEXT
          rs_db_key-option_id  = ls_ret->fieldval.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD _new_option.
    DATA: lr_db_key TYPE REF TO ts_db_key,
          lo_screen TYPE REF TO zcl_eui_screen,
          lo_error  TYPE REF TO zcx_eui_exception.

    CREATE DATA lr_db_key.
    DATA lv_prog TYPE sycprog.
    CONCATENATE sy-cprog `NEW_OPTION` INTO lv_prog.
    TRY.
        CREATE OBJECT lo_screen
          EXPORTING
            ir_context = lr_db_key
            iv_cprog   = lv_prog
            iv_dynnr   = zcl_eui_screen=>mc_dynnr-dynamic.
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    lo_screen->customize( name = 'PACKAGE_ID' required = '1' ).
    lo_screen->customize( name = 'OPTION_ID'  required = '1' ).

    DATA ls_status TYPE zif_eui_manager=>ts_status.
    ls_status-title = 'Creating an option via ABAP code is much easier!'(ad2).
    lo_screen->set_status( ls_status ).

    lo_screen->popup( iv_col_end = 70 ).                 "#EC NUMBER_OK
    CHECK lo_screen->show(
      io_handler      = me
      iv_handlers_map = '_ON_PAI_NEW_OPTION' ) = 'OK'.
    rs_db_key = lr_db_key->*.
  ENDMETHOD.

  METHOD _on_pai_new_option.
    CHECK iv_command = 'OK'.
    DATA lo_screen TYPE REF TO zcl_eui_screen.
    lo_screen ?= sender.

    " Screen data
    DATA lr_db_key TYPE REF TO ts_db_key.
    lr_db_key ?= lo_screen->get_context( ).

    " Is the option exists?
    DATA ls_key TYPE ts_db_key.
    zcl_aqo_helper=>get_by_key( EXPORTING is_db_key  = lr_db_key->*
                                CHANGING  cs_db_item = ls_key ).
    IF ls_key IS NOT INITIAL.
      MESSAGE 'The option already exists'(toi) TYPE 'S' DISPLAY LIKE 'E'.
      cv_close->* = abap_false.
      RETURN.
    ENDIF.

    " Is the package exists?
    SELECT SINGLE devclass INTO ls_key-package_id
    FROM tdevc
    WHERE devclass = lr_db_key->package_id.
    IF sy-subrc <> 0.
      MESSAGE s020(zaqo_message) WITH lr_db_key->package_id DISPLAY LIKE 'E'.
      cv_close->* = abap_false.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD _fill_struc_search.
    DATA lt_children TYPE lvc_t_nkey.
    mo_gui_tree->get_children( EXPORTING  i_node_key  = ms_node-struc_search
                               IMPORTING  et_children = lt_children
                               EXCEPTIONS OTHERS      = 1 ).
    CHECK sy-subrc = 0.

    IF lt_children[] IS NOT INITIAL.
      _expand_invert_node( iv_node  = ms_node-struc_search
                           iv_level = 0 ).
      RETURN.
    ENDIF.

    DATA lt_db_key TYPE zcl_aqo_helper=>tt_db_key.
    lt_db_key = zcl_aqo_helper=>get_db_keys( ).

    DATA: lr_db_key       TYPE REF TO ts_db_key,
          lt_node_package TYPE tt_node_package.
    LOOP AT lt_db_key REFERENCE INTO lr_db_key.
      DATA lv_parent TYPE lvc_nkey.
      _insert_parent_package( EXPORTING iv_package = lr_db_key->package_id
                              IMPORTING ev_node    = lv_parent
                              CHANGING  ct_package = lt_node_package ).

      _add_option_node( is_db_key = lr_db_key->*
                        iv_parent = lv_parent ).
    ENDLOOP.

    _expand_invert_node( iv_node = ms_node-struc_search ).
  ENDMETHOD.

  METHOD _insert_parent_package.
    CLEAR ev_node.

    DATA: lv_parent_name TYPE tdevc-devclass,
          lv_rem         TYPE string.
    SPLIT iv_package AT '_' INTO lv_parent_name lv_rem.

    " Try to make virtual parent package name
    IF lv_rem IS INITIAL.
      CLEAR lv_parent_name.

      DATA: lv_len   TYPE i,
            lv_char  TYPE char1,
            lv_index TYPE i.
      lv_len = strlen( iv_package ).
      DO lv_len TIMES.
        lv_index = sy-index - 1.
        lv_char  = iv_package+lv_index(1).
        IF lv_char >= '0' AND lv_char <= '9'.
          EXIT.
        ENDIF.
        CONCATENATE lv_parent_name lv_char INTO lv_parent_name.
      ENDDO.
    ENDIF.

    DATA ls_package LIKE LINE OF ct_package.
    READ TABLE ct_package INTO ls_package
     WITH TABLE KEY devclass = lv_parent_name.
    IF sy-subrc <> 0.
      ls_package-devclass = lv_parent_name.
      ls_package-parent_node = _add_parent_node(
         i_node_text   = ls_package-devclass
         i_relat_node  = ms_node-struc_search ).
      INSERT ls_package INTO TABLE ct_package.
    ENDIF.

    ev_node = ls_package-parent_node.
  ENDMETHOD.

  METHOD _expand_invert_node.
    DATA lt_expanded TYPE lvc_t_nkey.
    mo_gui_tree->get_expanded_nodes( CHANGING   ct_expanded_nodes = lt_expanded
                                     EXCEPTIONS OTHERS            = 0 ).
    READ TABLE lt_expanded TRANSPORTING NO FIELDS
     WITH KEY table_line = iv_node.
    IF sy-subrc = 0.
      mo_gui_tree->collapse_subtree( EXPORTING  i_node_key = iv_node
                                     EXCEPTIONS OTHERS     = 0 ).
      RETURN.
    ENDIF.

    mo_gui_tree->expand_node( EXPORTING  i_node_key    = iv_node
                                         i_level_count = iv_level
                              EXCEPTIONS OTHERS        = 0 ).
  ENDMETHOD.

  METHOD _fill_tree_with_created_opt.
    DATA lt_db_key TYPE zcl_aqo_helper=>tt_db_key.
    lt_db_key = zcl_aqo_helper=>get_db_keys(
      iv_where    = 'CREATED_UNAME = IV_PARAM1'
      iv_param1   = sy-uname
      iv_count    = mo_prefs->s_opt-v_max_count
      iv_order_by = 'CREATED_DATE DESCENDING' ).

    " Is developer who creates options?
    CHECK lt_db_key[] IS NOT INITIAL.
    ms_node-rec_created = _add_parent_node(
      i_node_text  = 'Recently created'(rec)
      iv_item_icon = icon_date ).

    DATA lr_db_key TYPE REF TO ts_db_key.
    LOOP AT lt_db_key REFERENCE INTO lr_db_key.
      _add_option_node( is_db_key = lr_db_key->*
                        iv_parent = ms_node-rec_created ).
    ENDLOOP.
  ENDMETHOD.

  METHOD _add_parent_node.
    DATA ls_layout TYPE lvc_s_layn.
    ls_layout-isfolder = abap_true.
    ls_layout-exp_image = ls_layout-n_image = iv_layout_icon.

    rv_node = _add_node(
      i_node_text   = i_node_text
      i_relat_node  = i_relat_node
      is_layout     = ls_layout
      iv_item_icon  = iv_item_icon
      iv_item_class = cl_gui_column_tree=>item_class_link ).
  ENDMETHOD.

  METHOD delete_from.
    DATA lt_children TYPE lvc_t_nkey.
    mo_gui_tree->get_children( EXPORTING  i_node_key  = iv_parent_node
                               IMPORTING  et_children = lt_children
                               EXCEPTIONS OTHERS      = 0 ).
    DATA lv_child TYPE lvc_nkey.
    LOOP AT lt_children INTO lv_child FROM iv_from.
      IF is_db_key IS SUPPLIED.
        DATA: ls_tree_data  TYPE ts_tree_data,
              lv_package_id TYPE lvc_value.
        mo_gui_tree->get_outtab_line( EXPORTING  i_node_key    = lv_child
                                      IMPORTING  e_outtab_line = ls_tree_data
                                                 e_node_text   = lv_package_id
                                      EXCEPTIONS OTHERS        = 1 ).
        CHECK sy-subrc = 0
          AND is_db_key-package_id = lv_package_id
          AND is_db_key-option_id  = ls_tree_data-option_id.
      ENDIF.

      mo_gui_tree->delete_subtree( EXPORTING  i_node_key = lv_child
                                   EXCEPTIONS OTHERS     = 0 ).
    ENDLOOP.

    CHECK iv_refresh = abap_true.
    mo_gui_tree->frontend_update( ).
  ENDMETHOD.

  METHOD add_opened.
    " Delete same option
    delete_from( iv_parent_node = ms_node-rec_opened
                 is_db_key      = is_db_key ).
    IF iv_insert = abap_true.
      _add_option_node( is_db_key      = is_db_key
                        iv_parent      = ms_node-rec_opened
                        i_relationship = cl_gui_column_tree=>relat_first_child ).
    ENDIF.
    " Delete oversized
    delete_from( iv_parent_node = ms_node-rec_opened
                 iv_from        = mo_prefs->s_opt-v_max_count + 1
                 iv_refresh     = abap_true ).
  ENDMETHOD.

  METHOD _add_option_node.
    DATA ls_layout TYPE lvc_s_layn.
    ls_layout-n_image = icon_change_text.

    _add_node(
      i_node_text    = is_db_key-package_id
      iv_option_id   = is_db_key-option_id
      i_relat_node   = iv_parent
      is_layout      = ls_layout
      iv_item_class  = cl_gui_column_tree=>item_class_link
      i_relationship = i_relationship ).
  ENDMETHOD.

  METHOD _add_node.
    DATA lt_item_layout TYPE lvc_t_layi.
    DATA lr_item_layout TYPE REF TO lvc_s_layi.
    IF iv_item_icon IS NOT INITIAL OR iv_item_class IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_item_layout REFERENCE INTO lr_item_layout.
      lr_item_layout->fieldname = cl_alv_tree_base=>c_hierarchy_column_name.
      lr_item_layout->t_image   = iv_item_icon.
      lr_item_layout->class     = iv_item_class.
    ENDIF.

    DATA ls_line TYPE ts_tree_data.
    ls_line-option_id = iv_option_id.

    DATA l_node_text TYPE lvc_value.
    l_node_text = i_node_text.

    mo_gui_tree->add_node(
      EXPORTING  i_relat_node_key = i_relat_node
                 i_relationship   = i_relationship
                 is_outtab_line   = ls_line
                 i_node_text      = l_node_text
                 is_node_layout   = is_layout
                 it_item_layout   = lt_item_layout
      IMPORTING  e_new_node_key   = rv_node
      EXCEPTIONS OTHERS           = 1 ).
    CHECK sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDMETHOD.

ENDCLASS.
