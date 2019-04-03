class ZCL_AQO_WEB_SERVICE definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AQO_WEB_SERVICE IMPLEMENTATION.


METHOD if_http_extension~handle_request.
  DATA:
    lo_request  TYPE REF TO if_http_request,
    lo_response TYPE REF TO	if_http_response,
    lt_field    TYPE tihttpnvp,
    ls_field    TYPE REF TO ihttpnvp,
    lv_tabix    TYPE sytabix,
    lv_tcode    TYPE sytcode,
    lv_action   TYPE string,
    lv_data     TYPE string.

  lo_request  = server->request.
  lo_response = server->response.

  lo_request->get_form_fields(
   CHANGING
     fields = lt_field ).

  lv_tcode = 'ZAQO_EDITOR'.
  LOOP AT lt_field REFERENCE INTO ls_field WHERE
     name = 'action' OR name = 'viewer'.
    lv_tabix = sy-tabix.

    CASE ls_field->name.
      WHEN 'action'.
        lv_action = ls_field->value.

      WHEN 'viewer'.
        lv_tcode = 'ZAQO_VIEWER'.
    ENDCASE.

    " Do not send
    DELETE lt_field INDEX lv_tabix.
  ENDLOOP.
  zcl_aqo_helper=>is_in_editor( iv_tcode = lv_tcode ).

  " allow other extensions to do something
  if_http_extension~flow_rc = if_http_extension=>co_flow_ok_others_opt.

  " create some response data
  lo_response->set_header_field(
    name  = 'Content-Type'                                  "#EC NOTEXT
    value = 'application/json' ).

  lo_response->set_header_field(
    name  = 'Access-Control-Allow-Origin'                   "#EC NOTEXT
    value = '*' ).

  " All in 1 place
  PERFORM call_by_name IN PROGRAM zaqo_editor
   USING
     lv_action
     lt_field
   CHANGING
     lv_data.

  lo_response->set_cdata( data = lv_data ).
ENDMETHOD.
ENDCLASS.
