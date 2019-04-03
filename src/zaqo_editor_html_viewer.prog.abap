*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_gui_html_viewer IMPLEMENTATION.
  METHOD constructor.
    DATA:
      lt_event   TYPE cntl_simple_events,
      ls_event   TYPE REF TO cntl_simple_event,
      lv_str     TYPE string,
      lv_xstr    TYPE xstring,
      lv_type    TYPE text100,
      lv_subtype TYPE text100,
      lt_w3mime  TYPE w3mimetabtype,
      lv_size    TYPE i.

    super->constructor(
     parent               = io_parent
     query_table_disabled = abap_true " param is 250 symbols only
    ).

    APPEND INITIAL LINE TO lt_event REFERENCE INTO ls_event.
    ls_event->eventid    = cl_gui_html_viewer=>m_id_sapevent.
    ls_event->appl_event = abap_true.

    set_registered_events( lt_event ).
    SET HANDLER:
      on_sapevent FOR me.


    " make texts available before run
    DATA:
      lv_file      TYPE string,
      lv_is_folder TYPE abap_bool,
      lv_url       TYPE text1000,
      lo_mr_api    TYPE REF TO if_mr_api.
    lo_mr_api = cl_mime_repository_api=>if_mr_api~get_api( ).

    " IE11 lags in SAP GUI
    LOOP AT it_sinon_file INTO lv_file WHERE table_line CP '*.js'
                                          OR table_line CP '*.css'.
      lv_url = lv_file.

      " Read whole file
      CLEAR lv_xstr.
      lo_mr_api->get(
       EXPORTING
         i_url       = lv_file
       IMPORTING
         e_is_folder = lv_is_folder
         e_content   = lv_xstr
       EXCEPTIONS
         OTHERS     = 5 ).

      " Skip folders
      CHECK lv_is_folder <> abap_true.

      CLEAR lt_w3mime.
      zcl_aqo_helper=>xstring_to_binary(
       EXPORTING
         iv_xstring = lv_xstr
       IMPORTING
         et_table   = lt_w3mime
         ev_length  = lv_size ).

      IF lv_file CP '*.js'.
        lv_type    = 'text'.
        lv_subtype = 'javascript'.
      ELSEIF lv_file CP '*.css'.
        lv_type    = 'text'.
        lv_subtype = 'css'.
      ELSEIF lv_file CP '*.json'.
        lv_type    = 'application'.
        lv_subtype = 'json'.
      ELSEIF lv_file CP '*.ttf'.
        lv_type    = 'application'.
        lv_subtype = 'x-font-ttf'.
      ELSEIF lv_file CP '*.woff'.
        lv_type    = 'application'.
        lv_subtype = 'font-woff'.
      ELSEIF lv_file CP '*.woff2'.
        lv_type    = 'application'.
        lv_subtype = 'font-woff2'.
      ELSEIF lv_file CP '*.properties'.
        lv_type    = 'text'.
        lv_subtype = 'x-java-properties'.
      ELSE.
        MESSAGE s024(zaqo_message) WITH 'constructor' lv_file.
        zcx_aqo_exception=>raise_sys_error( ).
      ENDIF.

      REPLACE FIRST OCCURRENCE OF lcl_opt=>mc_base_repo IN lv_url WITH ''.
      load_data(
        EXPORTING
          url        = lv_url
          type       = lv_type
          subtype	   = lv_subtype
          size       = lv_size
       "IMPORTING
       "   assigned_url = rv_url
        CHANGING
          data_table = lt_w3mime
        EXCEPTIONS
          OTHERS     = 2 ).
    ENDLOOP.
    cl_gui_cfw=>flush( ).

    " Try to read the document from archive
    CONCATENATE lcl_opt=>mc_base_repo 'index.html' INTO lv_url.
    lo_mr_api->get(
     EXPORTING
      i_url     = lv_url
     IMPORTING
      e_content = lv_xstr
     EXCEPTIONS
      OTHERS    = 5 ).

    " Do not use internal folder
    lv_str = zcl_aqo_helper=>xstring_to_string( lv_xstr ).
    REPLACE FIRST OCCURRENCE OF `src="webapp/sapui5res/sap-ui-core.js"` IN lv_str
     WITH `src="https://sapui5.hana.ondemand.com/1.62.0/resources/sap-ui-core.js"`.
    lv_xstr = zcl_aqo_helper=>string_to_xstring( lv_str ).

    CLEAR lt_w3mime.
    zcl_aqo_helper=>xstring_to_binary(
     EXPORTING
      iv_xstring = lv_xstr
     IMPORTING
      et_table   = lt_w3mime
      ev_length  = lv_size ).

    " Load only one file from archive
    load_data(
      EXPORTING
        url        = 'index.html'
        type       = 'text'
        subtype	   = 'html'
        size       = lv_size
      CHANGING
        data_table = lt_w3mime
      EXCEPTIONS
        OTHERS     = 2 ).
  ENDMETHOD.

  METHOD run_js.
    DATA:
      lv_script TYPE string,
      lt_script TYPE soli_tab.

    " Call fm in browser
    IF iv_param IS INITIAL.
      CONCATENATE iv_function `();` INTO lv_script.
    ELSE.
      CONCATENATE iv_function `(` iv_param  `);` INTO lv_script.
    ENDIF.

    " As table
    lt_script = cl_bcs_convert=>string_to_soli( lv_script ).

    set_script( script = lt_script ).

    execute_script(
     EXCEPTIONS
      OTHERS = 1 ).
  ENDMETHOD.

  METHOD on_sapevent.
    DATA:
      lt_param      TYPE tihttpnvp.
    lt_param = parse_fields(
     iv_get  = getdata
     it_post = postdata ).

    call_by_name(
     it_param  = lt_param
     iv_method = action
     io_me     = me ).
  ENDMETHOD.

  METHOD call_by_name.
    DATA:
      ls_param      TYPE REF TO ihttpnvp,
      lt_abap_param TYPE abap_parmbind_tab,
      ls_abap_param TYPE abap_parmbind,
      lv_guid       TYPE string,
      lo_error      TYPE REF TO zcx_aqo_exception,
      ls_back_info  TYPE ts_back_info,
      lv_param      TYPE string.

    " Fill all importing params
    LOOP AT it_param REFERENCE INTO ls_param.
      IF ls_param->name = 'GUID' OR ls_param->name = 'guid'.
        lv_guid = ls_param->value.

        " Always one RETURNING VALUE
        ls_abap_param-name = 'RV_OUT'.
        ls_abap_param-kind = cl_abap_objectdescr=>receiving.
        GET REFERENCE OF rv_out INTO ls_abap_param-value.
        INSERT ls_abap_param INTO TABLE lt_abap_param.
        CONTINUE.
      ENDIF.

      " IV_ prefix for all
      CONCATENATE `IV_` ls_param->name INTO ls_abap_param-name.
      TRANSLATE ls_abap_param-name TO UPPER CASE.

      ls_abap_param-kind = cl_abap_objectdescr=>exporting.
      GET REFERENCE OF ls_param->value INTO ls_abap_param-value.
      INSERT ls_abap_param INTO TABLE lt_abap_param.
    ENDLOOP.

    TRY.
        CALL METHOD lcl_opt=>(iv_method) PARAMETER-TABLE lt_abap_param.
      CATCH zcx_aqo_exception INTO lo_error.
        ls_back_info-info_text = lo_error->get_text( ).
        ls_back_info-kind         = 'E'.
        rv_out = zcl_aqo_helper=>to_json( ls_back_info ).
    ENDTRY.

    " Callback   lv_guid IS NOT INITIAL AND
    CHECK io_me IS NOT INITIAL.

    " How to call fm
    CONCATENATE `"` lv_guid `"` INTO lv_param.
    IF rv_out IS NOT INITIAL AND rv_out <> lcl_opt=>mc_again.
      CONCATENATE lv_param `, ` rv_out INTO lv_param.
    ENDIF.

    " Get result back
    io_me->run_js( iv_function = 'call_back'
                   iv_param    = lv_param ).

    " Call again
    IF rv_out = lcl_opt=>mc_again.
      ls_abap_param-name = 'IV_AGAIN'.
      ls_abap_param-kind = cl_abap_objectdescr=>exporting.
      GET REFERENCE OF lcl_opt=>mc_again INTO ls_abap_param-value.
      INSERT ls_abap_param INTO TABLE lt_abap_param.

      CALL METHOD lcl_opt=>(iv_method) PARAMETER-TABLE lt_abap_param.
    ENDIF.
  ENDMETHOD.

  METHOD parse_fields.
    DATA:
      lv_string     TYPE string,
      lt_substrings TYPE stringtab,
      lv_substring  TYPE string,
      ls_field      LIKE LINE OF rt_fields.

    DEFINE unescape.
      " do not use cl_http_utility as it does strange things with the encoding
      " todo, more to be added here
      REPLACE ALL OCCURRENCES OF:
        `%24` IN &1 WITH `$`,
        `%26` IN &1 WITH `&`,
        `%2B` IN &1 WITH `+`,
        `%2C` IN &1 WITH `,`,
        `%3A` IN &1 WITH `:`,
        `%3B` IN &1 WITH `;`,
        `%3D` IN &1 WITH `=`,
        `%3F` IN &1 WITH `?`,
        `%40` IN &1 WITH `@`.
    END-OF-DEFINITION.

    IF it_post IS NOT INITIAL.
      CONCATENATE LINES OF it_post INTO lv_string.
    ELSE.
      lv_string = iv_get.
    ENDIF.

    "lv_string = cl_http_utility=>unescape_url( lv_string ).

    SPLIT lv_string AT '&' INTO TABLE lt_substrings.

    LOOP AT lt_substrings INTO lv_substring.
      CLEAR: ls_field.

      SPLIT lv_substring AT '=' INTO ls_field-name ls_field-value.

      " Convert from url
      unescape:
       ls_field-name,
       ls_field-value.

      " Insert in upper case
      TRANSLATE ls_field-name TO UPPER CASE.
      INSERT ls_field INTO TABLE rt_fields.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
