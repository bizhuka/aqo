*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zaqo_bsp_editor.

CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      start_of_selection.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD start_of_selection.
    DATA:
      lt_param      TYPE tihttpnvp,
      ls_param      TYPE REF TO ihttpnvp,
      lv_url        TYPE string,
      lv_urlc(4096) TYPE c.

    IF sy-tcode = 'ZAQO_BSP_VIEWER'.
      APPEND INITIAL LINE TO lt_param REFERENCE INTO ls_param.
      ls_param->name  = 'viewer'.
      ls_param->value = 'true'.
    ENDIF.

    cl_http_ext_webapp=>create_url_for_bsp_application(
     EXPORTING
       bsp_application      = 'sap/zbsp_aqo/index.html'
       bsp_start_page       = ''
       bsp_start_parameters = lt_param
     IMPORTING
       abs_url              = lv_url ).

    " Call the BSP Application in the default Browser
    lv_urlc = lv_url.
    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url         = lv_urlc
        window_name = 'BSP'
        new_window  = abap_true
      EXCEPTIONS
        OTHERS      = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_main=>start_of_selection( ).
