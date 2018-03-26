*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_gui_html_viewer IMPLEMENTATION.
  METHOD constructor.
    DATA:
      lt_event TYPE cntl_simple_events,
      ls_event TYPE REF TO cntl_simple_event.

    super->constructor(
     parent               = io_parent
     query_table_disabled = abap_true ). " param is 250 symbols only

    APPEND INITIAL LINE TO lt_event REFERENCE INTO ls_event.
    ls_event->eventid    = cl_gui_html_viewer=>m_id_sapevent.
    ls_event->appl_event = abap_true.

    set_registered_events( lt_event ).
    SET HANDLER:
      on_sapevent FOR me.
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
      lv_name      TYPE string,
      lv_object    TYPE ztaqo_data-object,
      lv_subobject TYPE ztaqo_data-subobject,
      lv_datatype  TYPE zcl_aqo_util=>ts_field_opt-rollname,
      lv_title     TYPE sytitle,
      lv_ranges    TYPE string,
      lv_mandt     TYPE symandt,
      lt_param     TYPE tihttpnvp,
      lv_include   TYPE trdir-name,
      lv_line      TYPE i,
      lv_option    TYPE string,
      lv_guid      TYPE string,
      lv_favorite  TYPE string.
    FIELD-SYMBOLS:
      <ls_param> LIKE LINE OF lt_param,
      <lv_any>   TYPE ANY.

    lt_param = parse_fields( postdata ).

    " Copy values
    LOOP AT lt_param ASSIGNING <ls_param>.
      CONCATENATE 'lv_' <ls_param>-name INTO lv_name.

      UNASSIGN <lv_any>.
      ASSIGN (lv_name) TO <lv_any>.
      <lv_any> = <ls_param>-value.
    ENDLOOP.

    CASE action.
      WHEN 'GET_OPTIONS'.
        lcl_opt=>get_options( lv_guid ).

      WHEN 'SHOW_OPTION'.
        lcl_opt=>show_option(
         iv_object    = lv_object
         iv_subobject = lv_subobject
         iv_guid      = lv_guid ).

      WHEN 'CALL_OLD_UI'.
        lcl_opt=>call_old_ui(
         iv_object    = lv_object
         iv_subobject = lv_subobject ).

      WHEN 'DO_CLOSE'.
        LEAVE TO SCREEN 0.

      WHEN 'DRILL_DOWN'.
        zcl_aqo_util=>drill_down( lv_datatype ).

      WHEN 'VALUE_REQUEST'.
        lcl_opt=>value_request( iv_datatype = lv_datatype
                                iv_guid     = lv_guid ).

      WHEN 'RANGE_REQUEST'.
        lcl_opt=>range_request(
         iv_title    = lv_title
         iv_datatype = lv_datatype
         iv_ranges   = lv_ranges
         iv_guid     = lv_guid ).

      WHEN 'SAVE_OPTION'.
        IF lv_mandt IS INITIAL.
          lv_mandt = sy-mandt.
        ENDIF.

        lcl_opt=>save_option(
          iv_option = lv_option
          iv_mandt  = lv_mandt
          iv_guid   = lv_guid ).

      WHEN 'TRANSPORT_OPTION'.
        lcl_opt=>transport_option( ).

      WHEN 'DELETE_OPTION'.
        lcl_opt=>delete_option( lv_guid ).

      WHEN 'NAVIGATE_TO'.
        zcl_aqo_util=>navigate_to(
         iv_include  = lv_include
         iv_position = lv_line ).

      WHEN 'DEEP_SCAN'.
        lcl_opt=>deep_scan( lv_guid ).

      WHEN 'SET_FAVORITE'.
        lcl_opt=>set_favorite(
         iv_object    = lv_object
         iv_subobject = lv_subobject
         iv_favorite  = lv_favorite
         iv_guid      = lv_guid ).

    ENDCASE.
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
      replace all occurrences of:
        `%24` in &1 with `$`,
        `%26` in &1 with `&`,
        `%2B` in &1 with `+`,
        `%2C` in &1 with `,`,
        `%3A` in &1 with `:`,
        `%3B` in &1 with `;`,
        `%3D` in &1 with `=`,
        `%3F` in &1 with `?`,
        `%40` in &1 with `@`.
    END-OF-DEFINITION.

    CONCATENATE LINES OF it_postdata INTO lv_string.
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
