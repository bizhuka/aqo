*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

DATA:
  gv_ok_code TYPE syucomm.

" compatibility mode
CLASS zcl_aqo_helper DEFINITION LOAD.

TYPES:
  " Description of search help field
  BEGIN OF ts_sh_field.
    INCLUDE TYPE zcl_aqo_helper=>ts_field_desc AS field_desc.
TYPES:
  " Runtime fields
  is_key        TYPE xsdboolean,
  is_searchable TYPE xsdboolean,
  " short_label TYPE scrtext_s,
  END OF ts_sh_field,
  tt_sh_field TYPE STANDARD TABLE OF ts_sh_field WITH DEFAULT KEY,

  " ! Additional fields for mo_opt->mt_field_value
  BEGIN OF ts_field_value_send.
    INCLUDE TYPE zcl_aqo_helper=>ts_field_value.
TYPES:
  cur_value   TYPE REF TO data,
  sub_columns TYPE REF TO data,
  END OF ts_field_value_send,
  tt_field_value_send TYPE STANDARD TABLE OF ts_field_value_send WITH DEFAULT KEY,

  BEGIN OF ts_param,
    key   TYPE string,
    value TYPE string,
  END OF ts_param,
  tt_param TYPE STANDARD TABLE OF ts_param WITH DEFAULT KEY,

  BEGIN OF ts_filter_param,
    sh_filter TYPE ddshselops,
    db_filter TYPE string,
  END OF ts_filter_param,

  BEGIN OF ts_sort_param,
    sh_sort TYPE abap_sortorder_tab,
    db_sort TYPE text255,
  END OF ts_sort_param,

  BEGIN OF ts_request,
    url        TYPE string,
    entity     TYPE string,
    params     TYPE tt_param,
    count_only TYPE xsdboolean,
  END OF ts_request,

  BEGIN OF ts_response,
    success TYPE xsdboolean,
    data    TYPE REF TO data,
    error   TYPE string,
    status  TYPE i,
    fields  TYPE tt_sh_field,
  END OF ts_response,

  BEGIN OF ts_back_info,
    kind      TYPE symsgty,
    info_text TYPE string,
  END OF ts_back_info.

CLASS lcl_gui_html_viewer DEFINITION INHERITING FROM cl_gui_html_viewer FINAL.
  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
                  io_parent     TYPE REF TO cl_gui_container
                  it_sinon_file TYPE string_table
        RAISING   zcx_aqo_exception,

      " Execute script in browser
      run_js
        IMPORTING
          iv_function TYPE csequence
          iv_param    TYPE csequence OPTIONAL,

      on_sapevent FOR EVENT sapevent OF cl_gui_html_viewer
        IMPORTING action postdata getdata.     "#EC NEEDED query_table.

    CLASS-METHODS:
      parse_fields
        IMPORTING
                  iv_get           TYPE csequence
                  it_post          TYPE cnht_post_data_tab
        RETURNING VALUE(rt_fields) TYPE tihttpnvp,

      call_by_name
        IMPORTING
                  it_param      TYPE tihttpnvp
                  iv_method     TYPE csequence
                  io_me         TYPE REF TO lcl_gui_html_viewer OPTIONAL
        RETURNING VALUE(rv_out) TYPE string.
ENDCLASS.

CLASS lcl_opt DEFINITION INHERITING FROM zcl_aqo_option ABSTRACT FINAL.
  PUBLIC SECTION.

    CONSTANTS:
      mc_base_repo   TYPE string VALUE '/SAP/BC/BSP/SAP/ZBSP_AQO/'.

    CLASS-DATA:
      mo_html_viewer TYPE REF TO lcl_gui_html_viewer,
      mt_sinon_file  TYPE string_table.

    CONSTANTS:
      mc_again TYPE string VALUE 'CALL_AGAIN'.

    CLASS-METHODS:
      pbo
        RAISING zcx_aqo_exception,

      pai
        CHANGING
          cv_cmd TYPE syucomm,

      get_entity_desc
        IMPORTING
                  iv_entity   TYPE csequence
        EXPORTING
                  ev_db       TYPE string
                  ev_shlp     TYPE shlpname
                  ev_db_fld   TYPE zdaqo_db_field
                  ev_dom      TYPE string

                  es_sh_desc  TYPE shlp_descr
                  er_table    TYPE REF TO data
                  et_sh_field TYPE tt_sh_field
        RAISING   zcx_aqo_exception,

      get_odata
        IMPORTING
                  iv_entity     TYPE csequence
                  it_params     TYPE tt_param
                  iv_count_only TYPE xsdboolean
        EXPORTING
                  es_response   TYPE ts_response
        RAISING   zcx_aqo_exception,

      get_sh_table
        IMPORTING
          iv_maxrows TYPE i
        CHANGING
          cs_sh_desc TYPE shlp_descr
          ct_table   TYPE STANDARD TABLE,

      get_send_fields
        IMPORTING
                  it_field        TYPE zcl_aqo_helper=>tt_field_value
                  io_option       TYPE REF TO zcl_aqo_option OPTIONAL
        RETURNING VALUE(rt_field) TYPE tt_field_value_send
        RAISING   zcx_aqo_exception,

      sap_init_app
        RETURNING VALUE(rv_out) TYPE string
        RAISING   zcx_aqo_exception,

      sap_odata_query
        IMPORTING
                  iv_requests   TYPE string
        RETURNING VALUE(rv_out) TYPE string
        RAISING   zcx_aqo_exception,

      sap_get_sh_fields
        IMPORTING
                  iv_entities   TYPE string
        RETURNING VALUE(rv_out) TYPE string
        RAISING   zcx_aqo_exception,

      sap_read_option
        IMPORTING
                  iv_package_id TYPE string
                  iv_option_id  TYPE string
        RETURNING VALUE(rv_out) TYPE string
        RAISING   zcx_aqo_exception,

      sap_save_option
        IMPORTING
                  iv_package_id   TYPE string
                  iv_option_id    TYPE string
                  iv_cur_fields   TYPE string
                  iv_cur_option   TYPE string
                  VALUE(iv_mandt) TYPE string
        RETURNING VALUE(rv_out)   TYPE string
        RAISING   zcx_aqo_exception,

      sap_get_field_desc
        IMPORTING
                  iv_name       TYPE string
                  iv_rollname   TYPE string
        RETURNING VALUE(rv_out) TYPE string
        RAISING   zcx_aqo_exception,

      sap_drill_down
        IMPORTING
                  iv_datatype   TYPE string
                  iv_again      TYPE csequence OPTIONAL
        RETURNING VALUE(rv_out) TYPE string,

      sap_navigate_to
        IMPORTING
                  " 1-st mode
                  iv_include    TYPE string OPTIONAL
                  iv_line       TYPE string OPTIONAL
                  " 2-d mode
                  iv_index      TYPE string OPTIONAL

                  iv_again      TYPE csequence OPTIONAL
        RETURNING VALUE(rv_out) TYPE string,

      sap_download_file
        IMPORTING
                  iv_file_name  TYPE string
                  iv_content    TYPE string
                  iv_charset    TYPE string OPTIONAL
                  iv_again      TYPE csequence OPTIONAL
        RETURNING VALUE(rv_out) TYPE string,

      sap_delete_or_transport
        IMPORTING
                  iv_package_id TYPE string
                  iv_option_id  TYPE string
                  iv_delete     TYPE string
                  iv_request    TYPE string
        RETURNING VALUE(rv_out) TYPE string
        RAISING   zcx_aqo_exception.

ENDCLASS.
