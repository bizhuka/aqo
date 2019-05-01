*&---------------------------------------------------------------------*
* For BSP MIME run BSP_UPDATE_MIMEREPOS
*&---------------------------------------------------------------------*
REPORT zaqo_loader.

TYPE-POOLS:
 abap.

PARAMETERS:
  p_path TYPE string OBLIGATORY DEFAULT 'd:\Users\MoldaB\Desktop\lib\AQOwww\',
  p_repo TYPE string            DEFAULT '/SAP/BC/BSP/SAP/ZBSP_AQO/',
  p_excl TYPE string            DEFAULT '*sapui5res*'.


TYPES:
  BEGIN OF ts_item,
    path      TYPE string,
    is_folder TYPE abap_bool,
  END OF ts_item,
  tt_item TYPE STANDARD TABLE OF ts_item WITH DEFAULT KEY.

CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-METHODS:
      start,

      fill_with_folders
        IMPORTING
          iv_path TYPE string
          iv_sep  TYPE char1
        CHANGING
          ct_item TYPE tt_item.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD start.
    DATA:
      lv_sep    TYPE char01,
      lt_item   TYPE tt_item,
      ls_item   TYPE REF TO ts_item,
      lt_w3mime TYPE w3mimetabtype,
      lv_len    TYPE i,
      lv_xstr   TYPE xstring,
      lo_mr_api TYPE REF TO if_mr_api.

    " / Or \
    cl_gui_frontend_services=>get_file_separator(
     CHANGING
       file_separator = lv_sep ).

    " Main file
    APPEND INITIAL LINE TO lt_item REFERENCE INTO ls_item.
    CONCATENATE p_path 'index.html' INTO ls_item->path.

    " just one folder
    APPEND INITIAL LINE TO lt_item REFERENCE INTO ls_item.
    ls_item->is_folder = abap_true.
    CONCATENATE p_path 'webapp' lv_sep INTO ls_item->path.

    fill_with_folders(
     EXPORTING
       iv_path = ls_item->path
       iv_sep  = lv_sep
     CHANGING
       ct_item = lt_item ).

    " old method of storage
    lo_mr_api = cl_mime_repository_api=>if_mr_api~get_api( ).

    " Just show folder first
    SORT lt_item STABLE BY is_folder DESCENDING.
    LOOP AT lt_item REFERENCE INTO ls_item.
      " Load file
      IF ls_item->is_folder <> abap_true.
        CLEAR lt_w3mime.
        cl_gui_frontend_services=>gui_upload(
           EXPORTING
             filename   = ls_item->path
             filetype   = 'BIN'       " File Type Binary
           IMPORTING
             filelength = lv_len
           CHANGING
             data_tab   = lt_w3mime
           EXCEPTIONS
             OTHERS     = 1 ).
        CHECK sy-subrc = 0.
      ENDIF.

      " Delete path
      REPLACE FIRST OCCURRENCE OF p_path IN ls_item->path WITH ''.

      " Create URL

      REPLACE ALL OCCURRENCES OF '\' IN ls_item->path WITH '/'.
      CONCATENATE p_repo ls_item->path INTO ls_item->path.

      " If it is folder
      IF ls_item->is_folder = abap_true.
        " Folder in MIME
        lo_mr_api->create_folder(
         EXPORTING
          i_url  = ls_item->path
         EXCEPTIONS
          OTHERS = 1 ).
        CHECK sy-subrc = 0.

        CONTINUE.
      ENDIF.

      lv_xstr = zcl_aqo_helper=>binary_to_xstring(
       it_table  = lt_w3mime
       iv_length = lv_len ).

      lo_mr_api->put(
       EXPORTING
        i_url     = ls_item->path
        i_content = lv_xstr
         EXCEPTIONS
          OTHERS = 1 ).
      IF sy-subrc = 0.
        WRITE : / ls_item->path COLOR COL_POSITIVE.
      ELSE.
        WRITE : / ls_item->path COLOR COL_NEGATIVE.
      ENDIF.
    ENDLOOP.

    MESSAGE 'Done'(tx1) TYPE 'S'.
  ENDMETHOD.

  METHOD fill_with_folders.
    DATA:
      lv_cnt    TYPE i,
      lt_folder TYPE STANDARD TABLE OF text1000,
      lv_folder TYPE REF TO text1000,
      ls_item   TYPE REF TO ts_item.

    cl_gui_frontend_services=>directory_list_files(
     EXPORTING
       directory        = iv_path
     CHANGING
       file_table       = lt_folder
       count            = lv_cnt
     EXCEPTIONS
       OTHERS           = 1 ).
    CHECK sy-subrc = 0 AND lv_cnt > 0.

    " Add one by one
    LOOP AT lt_folder REFERENCE INTO lv_folder WHERE table_line NP '.*'.
      IF p_excl IS NOT INITIAL.
        CHECK lv_folder->* NP p_excl.
      ENDIF.

      " New item
      APPEND INITIAL LINE TO ct_item REFERENCE INTO ls_item.
      CONCATENATE iv_path lv_folder->* INTO ls_item->path.

      IF cl_gui_frontend_services=>directory_exist( ls_item->path ) = abap_true.
        ls_item->is_folder = abap_true.

        CONCATENATE ls_item->path iv_sep INTO ls_item->path.
        " Next level
        fill_with_folders(
         EXPORTING
           iv_path   = ls_item->path
           iv_sep    = iv_sep
         CHANGING
           ct_item   = ct_item ).
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
  lcl_main=>start( ).
