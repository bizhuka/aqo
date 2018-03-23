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
      lv_object    TYPE ztparams_cluster-object,
      lv_subobject TYPE ztparams_cluster-subobject,
      lv_datatype  TYPE zcl_aqo=>ts_field_opt-rollname,
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
      <lv_any>   TYPE any.

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

    CONCATENATE LINES OF it_postdata INTO lv_string.
    "lv_string = cl_http_utility=>unescape_url( lv_string ).

    SPLIT lv_string AT '&' INTO TABLE lt_substrings.

    LOOP AT lt_substrings INTO lv_substring.
      CLEAR: ls_field.

      ls_field-name = substring_before( val = lv_substring
                                        sub = '=' ).
      unescape ls_field-name.
      TRANSLATE ls_field-name TO UPPER CASE.

      ls_field-value = substring_after( val = lv_substring
                                        sub = '=' ).
      unescape ls_field-value.

      INSERT ls_field INTO TABLE rt_fields.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
