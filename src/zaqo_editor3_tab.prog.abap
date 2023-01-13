*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_tab DEFINITION ABSTRACT.

  PUBLIC SECTION.
    METHODS:
      " For ALV tables based on TS_FLD_VALUE
      show
        IMPORTING iv_skip_alv_check   TYPE abap_bool OPTIONAL
        RETURNING VALUE(rv_close_cmd) TYPE syucomm,

      " For general ALV in tabs
      pbo.

  PROTECTED SECTION.
    DATA:
      mo_alv    TYPE REF TO zcl_eui_alv,
      ms_db_key TYPE ts_db_key,
      mr_table  TYPE REF TO data.

    METHODS:
      _fill_table,

      _create_alv
        IMPORTING
          iv_for_field TYPE abap_bool,

      _get_layout
        RETURNING VALUE(rs_layout) TYPE lvc_s_layo,

      _get_catalog
        RETURNING VALUE(rt_catalog) TYPE lvc_t_fcat,

      _get_toolbar
        RETURNING VALUE(rt_toolbar) TYPE ttb_button,

      _get_filter
        RETURNING VALUE(rt_filter) TYPE lvc_t_filt,

      _get_sort
        RETURNING VALUE(rt_sort) TYPE lvc_t_sort,

      _get_status
        RETURNING VALUE(rs_status) TYPE zif_eui_manager=>ts_status,

      _on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid      "#EC CALLED
        IMPORTING
          e_object
          e_interactive,

      _on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid "#EC CALLED
        IMPORTING
          e_row_id
          e_column_id,

      _on_user_command FOR EVENT user_command OF cl_gui_alv_grid "#EC CALLED
        IMPORTING
          sender
          e_ucomm,

      _on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid "#EC CALLED
        IMPORTING
          sender
          er_data_changed,

      _create_container
        RETURNING VALUE(ro_container) TYPE REF TO cl_gui_custom_container,

      _on_app_event FOR EVENT app_event OF lcl_editor
        IMPORTING
          iv_origin
          cv_ok,

      need_refresh
        RETURNING VALUE(rv_refresh) TYPE abap_bool,

      _refresh_now,

      _refresh_after.
ENDCLASS.

CLASS lcl_tab IMPLEMENTATION.
  METHOD show.
    _fill_table( ).

    _create_alv( iv_for_field = abap_true ).

    mo_alv->set_status( _get_status( ) ).

    mo_alv->popup( iv_row_end = 28 ).                    "#EC NUMBER_OK

    IF iv_skip_alv_check = abap_true.
      go_editor->skip_message( mo_alv ).
    ENDIF.

    rv_close_cmd = mo_alv->show( ).
  ENDMETHOD.

  METHOD pbo.
    IF mo_alv IS INITIAL.
      SET HANDLER _on_app_event FOR go_editor.
      _create_alv( iv_for_field = abap_false ).
    ENDIF.

    IF need_refresh( ) = abap_true.
      _fill_table( ).

      DATA ls_stable TYPE lvc_s_stbl.
      ls_stable-row = ls_stable-col = abap_true.
      mo_alv->get_grid( )->refresh_table_display( is_stable = ls_stable ).
    ENDIF.
  ENDMETHOD.

  METHOD _on_app_event.
    IF iv_origin = mc_event-open.
      _refresh_after( ).
    ENDIF.
  ENDMETHOD.

  METHOD need_refresh.
    DATA ls_new_db_key LIKE ms_db_key.
    MOVE-CORRESPONDING zsaqo3_general_info TO ls_new_db_key.

    IF ms_db_key <> ls_new_db_key.
      ms_db_key = ls_new_db_key.
      rv_refresh = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD _refresh_now.
    CLEAR ms_db_key.
    pbo( ).
  ENDMETHOD.

  METHOD _refresh_after.
    CLEAR ms_db_key.
  ENDMETHOD.

  METHOD _create_alv.
    CREATE OBJECT mo_alv
      EXPORTING
        ir_table       = mr_table
        is_layout      = _get_layout( )
        it_mod_catalog = _get_catalog( )
        it_filter      = _get_filter( )
        it_sort        = _get_sort( ).
    mo_alv->add_handler( me ).
    " Need container?
    CHECK iv_for_field <> abap_true.

    " TAB for general purpose
    mo_alv->pbo( io_container = _create_container( ) ).
  ENDMETHOD.

  METHOD _create_container.
    DATA lv_name TYPE text10 VALUE 'CUST_'.
    CONCATENATE lv_name sy-dynnr+1 INTO lv_name.
    CREATE OBJECT ro_container
      EXPORTING
        container_name = lv_name.
  ENDMETHOD.

  METHOD _get_layout.
    rs_layout-smalltitle = 'X'.
  ENDMETHOD.

  METHOD _on_toolbar.
    CLEAR e_object->mt_toolbar[].
    CHECK e_interactive <> abap_true.
    e_object->mt_toolbar = _get_toolbar( ).
  ENDMETHOD.

  METHOD _on_data_changed.                                  "#EC NEEDED
  ENDMETHOD.

  METHOD _fill_table.                                       "#EC NEEDED
  ENDMETHOD.

  METHOD _get_catalog.                                      "#EC NEEDED
  ENDMETHOD.

  METHOD _get_filter.                                       "#EC NEEDED
  ENDMETHOD.

  METHOD _get_sort.                                         "#EC NEEDED
  ENDMETHOD.

  METHOD _get_toolbar.                                      "#EC NEEDED
  ENDMETHOD.

  METHOD _on_hotspot_click.                                 "#EC NEEDED
  ENDMETHOD.

  METHOD _on_user_command.                                  "#EC NEEDED
  ENDMETHOD.

  METHOD _get_status.                                       "#EC NEEDED
  ENDMETHOD.
ENDCLASS.
