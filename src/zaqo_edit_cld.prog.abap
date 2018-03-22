*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_gui_html_viewer DEFINITION INHERITING FROM cl_gui_html_viewer FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_parent TYPE REF TO cl_gui_container,

      " Execute script in browser
      run_js
        IMPORTING
          iv_function TYPE csequence
          iv_param    TYPE csequence OPTIONAL,

      on_sapevent FOR EVENT sapevent OF cl_gui_html_viewer
        IMPORTING action postdata. "#EC NEEDED frame query_table getdata.

    CLASS-METHODS:
      parse_fields
        IMPORTING
                  it_postdata      TYPE cnht_post_data_tab
        RETURNING VALUE(rt_fields) TYPE tihttpnvp.
ENDCLASS.

CLASS lcl_opt DEFINITION INHERITING FROM zcl_aqo FINAL.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_fav,
        uname     TYPE syuname,
        object    TYPE ztaqo_data-object,
        subobject TYPE ztaqo_data-subobject,
      END OF ts_fav,

      BEGIN OF ts_own_opt,
        fav TYPE SORTED TABLE OF ts_fav WITH UNIQUE KEY uname object subobject,
      END OF ts_own_opt.

    CLASS-DATA:
      mo_html_viewer TYPE REF TO lcl_gui_html_viewer,
      mo_opt         TYPE REF TO zcl_aqo.

    CLASS-METHODS:
      pbo,

      pai
        IMPORTING
          iv_cmd TYPE syucomm,

      get_options
        IMPORTING
          iv_guid TYPE csequence,

      show_option
        IMPORTING
          iv_object    TYPE csequence
          iv_subobject TYPE csequence
          iv_guid      TYPE csequence,

      load_data
        IMPORTING
                  iv_data       TYPE string OPTIONAL
                  iv_subtype    TYPE c
                  iv_url        TYPE w3url
        RETURNING VALUE(rv_url) TYPE w3url,

      value_request
        IMPORTING
          iv_datatype TYPE csequence
          iv_guid     TYPE csequence,

      range_request
        IMPORTING
          iv_title    TYPE sytitle
          iv_datatype TYPE csequence
          iv_ranges   TYPE string
          iv_guid     TYPE csequence,

      transport_option,

      delete_option
        IMPORTING
          iv_guid TYPE csequence,

      save_option
        IMPORTING
          iv_option TYPE string
          iv_mandt  TYPE symandt
          iv_guid   TYPE csequence,

      deep_scan
        IMPORTING
          iv_guid TYPE csequence,

      set_favorite
        IMPORTING
          iv_object    TYPE csequence
          iv_subobject TYPE csequence
          iv_favorite  TYPE csequence
          iv_guid      TYPE csequence,

      load_from_smw0
        IMPORTING
                  iv_objid       TYPE wwwdata-objid
        RETURNING VALUE(rv_data) TYPE string.
ENDCLASS.

DATA:
  gv_ok_code TYPE syucomm.
