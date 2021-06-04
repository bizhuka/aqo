*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_online_docu IMPLEMENTATION.
  METHOD constructor.
    CHECK lcl_opt=>mv_is_dev = abap_true.

    CREATE OBJECT mo_menu
      EXPORTING
        io_handler = me.

    DATA lt_menu TYPE zcl_eui_menu=>tt_menu.
    DATA lr_menu TYPE REF TO zcl_eui_menu=>ts_menu.

    APPEND INITIAL LINE TO lt_menu REFERENCE INTO lr_menu.
    lr_menu->function = 'ONLINE_DOCU'.
    lr_menu->text     = 'Online documentation'(odo).
    lr_menu->icon     = icon_message_information_small.

    mo_menu->create_toolbar( it_menu  = lt_menu
                                  iv_width = 190 ).
  ENDMETHOD.

  METHOD _on_function_selected.
    CHECK fcode = 'ONLINE_DOCU'.

    DATA lv_url TYPE text255 VALUE 'https://bizhuka.github.io/aqo'.
    IF sy-langu = 'R'.
      CONCATENATE lv_url '/ru' INTO lv_url.
    ENDIF.

    " Show online documentation in browser
    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url    = lv_url
      EXCEPTIONS
        OTHERS = 0.
  ENDMETHOD.

  METHOD hide_button.
    CHECK mo_menu IS NOT INITIAL.

    DATA lo_container TYPE REF TO cl_gui_container.
    lo_container = mo_menu->get_container( ).
    IF lo_container IS NOT INITIAL.
      lo_container->set_visible( abap_false ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
