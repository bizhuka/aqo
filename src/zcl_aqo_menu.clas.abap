class ZCL_AQO_MENU definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_menu.
            INCLUDE TYPE stb_button.
    TYPES:
      " For menu
      par_function TYPE ui_func,
      ftype        TYPE cua_ftyp,
      hidden       TYPE cua_ftyp,
      accelerator  TYPE cua_path,
      top          TYPE flag,
      " Do not show
      hide         TYPE abap_bool,
      END OF ts_menu .
  types:
    tt_menu TYPE STANDARD TABLE OF ts_menu WITH DEFAULT KEY .

  constants MC_OAOR_NEW_FILE type STRING value 'NEW_FILE' ##NO_TEXT.
  constants MC_OAOR_NEW_VERSION type STRING value 'NEW_VERSION' ##NO_TEXT.
  constants MC_OAOR_UPDATE_VERSION type STRING value 'UPDATE_VERSION' ##NO_TEXT.

  methods CONSTRUCT_BUTTONS
    importing
      !IT_MENU type TT_MENU .
  class-methods GET_MENU
    importing
      !IV_PACKAGE_ID type CSEQUENCE
      !IV_OPTION_ID type CSEQUENCE
    returning
      value(RO_AQO_MENU) type ref to ZCL_AQO_MENU .
  methods SET_VISIBLE
    importing
      !IV_VISIBLE type ABAP_BOOL .
protected section.
private section.

  data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MO_CONTAINER type ref to CL_GUI_GOS_CONTAINER .

  methods ON_FUNCTION_SELECTED
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
ENDCLASS.



CLASS ZCL_AQO_MENU IMPLEMENTATION.


METHOD construct_buttons.
  DATA:
    lp_dialog_status TYPE char1,
    lp_gui           TYPE char1,
    lp_cat           TYPE char1,
    lt_stb_menu      TYPE SORTED TABLE OF stb_btnmnu WITH UNIQUE KEY function,
    ls_stb_menu      TYPE stb_btnmnu,
    lv_icon          TYPE icon-id,
    lv_width         TYPE i.
  FIELD-SYMBOLS:
    <ls_menu>     LIKE LINE OF it_menu,
    <ls_stb_menu> TYPE stb_btnmnu.

  " Do not work in standard transactions (except SE38, SE80 ...)
  CHECK sy-tcode CP 'Z*'
     OR sy-tcode CP 'Y*'
     OR sy-tcode CP 'SE*'.

  " if we are called by RFC in a dialogless BAPI or in update task or in batch suppress the starter
  GET PARAMETER ID 'FLAG_DIALOG_STATUS' FIELD lp_dialog_status.

  CALL FUNCTION 'CAT_IS_ACTIVE'
    IMPORTING
      active = lp_cat.

  CALL FUNCTION 'RFC_IS_GUI_ON'
    IMPORTING
      on = lp_gui.

  CHECK  " TODO sy-binpt IS INITIAL
         sy-batch IS INITIAL
     AND sy-oncom <> 'V'
     AND lp_dialog_status IS INITIAL
     AND lp_cat = space
     AND lp_gui = 'Y'.

  " Could be different buttons
  IF mo_toolbar IS NOT INITIAL.
    mo_toolbar->finalize( ).
    mo_toolbar->free( ).
    CLEAR mo_toolbar.
  ENDIF.

  IF mo_container IS NOT INITIAL.
    mo_container->finalize( ).
    mo_container->free( ).
    CLEAR mo_container.
  ENDIF.

  " Calc new width
  lv_width = 0.
  LOOP AT it_menu ASSIGNING <ls_menu> WHERE hide <> abap_true
                                        AND par_function IS INITIAL.
    CASE <ls_menu>-butn_type.
      WHEN cntb_btype_menu.
        ADD 51 TO lv_width.

      WHEN cntb_btype_sep.
        ADD 6 TO lv_width.

      WHEN OTHERS.
        ADD 41 TO lv_width.
    ENDCASE.
  ENDLOOP.

  " Create new menu
  CREATE OBJECT mo_container
    EXPORTING
      width                   = lv_width
      no_autodef_progid_dynnr = abap_true
    EXCEPTIONS
      OTHERS                  = 5.
  CHECK sy-subrc = 0.

  CREATE OBJECT mo_toolbar
    EXPORTING
      parent = mo_container.

  LOOP AT it_menu ASSIGNING <ls_menu> WHERE hide <> abap_true.
    " No search in ICON table
    lv_icon = <ls_menu>-icon.

    IF <ls_menu>-par_function IS INITIAL.
      mo_toolbar->add_button(
        fcode       = <ls_menu>-function
        icon        = <ls_menu>-icon
        is_disabled = <ls_menu>-disabled
        butn_type   = <ls_menu>-butn_type
        text        = <ls_menu>-text
        quickinfo   = <ls_menu>-quickinfo
        is_checked  = <ls_menu>-checked ).
      " CHECK ERRORS
    ELSE.
      " Instead of CL_GUI_TOOLBAR=>M_TABLE_CTXMENU[]
      READ TABLE lt_stb_menu ASSIGNING <ls_stb_menu>
         WITH TABLE KEY function = <ls_menu>-par_function.

      " Create new one
      IF sy-subrc <> 0.
        CREATE OBJECT ls_stb_menu-ctmenu.
        ls_stb_menu-function = <ls_menu>-par_function.
        INSERT ls_stb_menu INTO TABLE lt_stb_menu ASSIGNING <ls_stb_menu>.
      ENDIF.

      CASE <ls_menu>-butn_type.
        WHEN cntb_btype_sep.
          <ls_stb_menu>-ctmenu->add_separator( ).

        WHEN cntb_btype_menu.
          " Save for sub menu
          CREATE OBJECT ls_stb_menu-ctmenu.
          ls_stb_menu-function = <ls_menu>-function.
          INSERT ls_stb_menu INTO TABLE lt_stb_menu.

          " And add it
          <ls_stb_menu>-ctmenu->add_submenu(
            menu        = ls_stb_menu-ctmenu " <-- sub menu
            icon        = lv_icon
            disabled    = <ls_menu>-disabled
            text        = <ls_menu>-text
            hidden      = <ls_menu>-hidden
            accelerator = <ls_menu>-accelerator ).

        WHEN OTHERS.
          <ls_stb_menu>-ctmenu->add_function(
            fcode       = <ls_menu>-function
            icon        = lv_icon
            disabled    = <ls_menu>-disabled
            text        = <ls_menu>-text
            checked     = <ls_menu>-checked
            ftype       = <ls_menu>-ftype
            hidden      = <ls_menu>-hidden
            accelerator = <ls_menu>-accelerator
            insert_at_the_top = <ls_menu>-top ).
      ENDCASE.

      " Set menu or delete it
      mo_toolbar->set_static_ctxmenu(
       EXPORTING
        fcode     = <ls_menu>-par_function
        ctxmenu   = <ls_stb_menu>-ctmenu
       EXCEPTIONS
        OTHERS    = 1 ).
    ENDIF.
  ENDLOOP.
ENDMETHOD.


METHOD get_menu.
  DATA:
    ls_event TYPE REF TO cntl_simple_event,
    lt_event TYPE cntl_simple_events.
  ro_aqo_menu = lcl_aqo_option=>get_menu(
   iv_package_id = iv_package_id
   iv_option_id  = iv_option_id ).

  " And set the handler
  CHECK ro_aqo_menu->mo_toolbar IS NOT INITIAL.

  " 1 event only
  APPEND INITIAL LINE TO lt_event REFERENCE INTO ls_event.
  ls_event->appl_event = abap_true.
  ls_event->eventid = cl_gui_toolbar=>m_id_function_selected.
  ro_aqo_menu->mo_toolbar->set_registered_events( events = lt_event ).

  SET HANDLER:
    ro_aqo_menu->on_function_selected FOR ro_aqo_menu->mo_toolbar.
ENDMETHOD.


METHOD on_function_selected.
  DATA:
    ls_unq_menu TYPE REF TO lcl_aqo_option=>ts_unq_menu,
    lo_err      TYPE REF TO zcx_aqo_exception,
    lo_error    TYPE REF TO cx_root,
    lv_update   TYPE abap_bool,
    ls_button   TYPE REF TO stb_button.

  DEFINE read_option.
    IF   ls_unq_menu->option IS INITIAL
      OR ls_unq_menu->option->ms_db_item-package_id <> ls_unq_menu->package_id
      OR ls_unq_menu->option->ms_db_item-option_id  <> ls_unq_menu->option_id.

      TRY.
          ls_unq_menu->option = zcl_aqo_option=>create(
             iv_package_id  = ls_unq_menu->package_id
             iv_option_id   = ls_unq_menu->option_id ).
        CATCH zcx_aqo_exception INTO lo_err.
          MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
      ENDTRY.
    ENDIF.
  END-OF-DEFINITION.

  LOOP AT lcl_aqo_option=>mt_unq_menu REFERENCE INTO ls_unq_menu.
    CHECK ls_unq_menu->menu = me.

    " Set no error mode
    zcl_aqo_helper=>is_in_editor( iv_tcode = ls_unq_menu->tcode ).

    read_option.
    TRY.
        CALL METHOD lcl_aqo_option=>(fcode)
          EXPORTING
            is_unq_menu = ls_unq_menu
            io_option   = ls_unq_menu->option
          RECEIVING
            rv_update   = lv_update.

        " Option was changed
        IF lv_update = abap_true.
          CLEAR ls_unq_menu->option.
          read_option.
        ENDIF.
      CATCH cx_root INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    " All is OK
    RETURN.
  ENDLOOP.

  MESSAGE 'Menu not found in lcl_aqo_option=>mt_unq_menu[]'(nop) TYPE 'X'.
ENDMETHOD.


METHOD set_visible.
  CHECK mo_toolbar   IS NOT INITIAL
    AND mo_container IS NOT INITIAL.
  mo_container->set_visible( iv_visible ).
ENDMETHOD.
ENDCLASS.
