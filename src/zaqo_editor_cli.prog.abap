*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_opt IMPLEMENTATION.
  METHOD pbo.
    DATA:
      lo_cont   TYPE REF TO cl_gui_custom_container,
      lo_mr_api TYPE REF TO if_mr_api,
      lv_url    TYPE string.

    SET PF-STATUS 'ST_MAIN'.
    SET TITLEBAR 'ST_TITLE'.

    " One time only
    CHECK mo_html_viewer IS INITIAL.

    CREATE OBJECT lo_cont
      EXPORTING
        container_name = 'CUST_CONTROL'.

    " Load the whole folder
    lo_mr_api = cl_mime_repository_api=>if_mr_api~get_api( ).

    CONCATENATE mc_base_repo 'webapp/' INTO lv_url.
    lo_mr_api->file_list(
     EXPORTING
       i_url            = lv_url
       i_recursive_call = abap_true
     IMPORTING
       e_files          = mt_sinon_file
     EXCEPTIONS
       OTHERS           = 6 ).
    DELETE mt_sinon_file WHERE table_line CP '*webapp/sapui5res*'.

    " Pass screen and the archive
    CREATE OBJECT mo_html_viewer
      EXPORTING
        io_parent     = lo_cont
        it_sinon_file = mt_sinon_file.

    " And show
    mo_html_viewer->show_url( url = 'index.html'  ).
  ENDMETHOD.

  METHOD pai.
    CASE cv_cmd.
      WHEN 'ESCAPE' OR 'CANCEL'.
        IF zcl_aqo_helper=>confirm(
           iv_title    = 'Confirmation'(ext)
           iv_question = 'Close without saving?'(cnf) ) = abap_true.
          LEAVE TO SCREEN 0.
        ENDIF.

      WHEN OTHERS.
        RETURN.
    ENDCASE.

    CLEAR cv_cmd.
  ENDMETHOD.

  METHOD sap_init_app.
    TYPES:
      BEGIN OF ts_pair,
        name TYPE string,
        file TYPE string,
      END OF ts_pair,

      BEGIN OF ts_response,
        pairs  TYPE STANDARD TABLE OF ts_pair WITH DEFAULT KEY,
        is_dev TYPE xsdboolean,
      END OF ts_response.
    DATA:
      ls_response  TYPE ts_response,
      lv_xstr      TYPE xstring,
      lv_file      TYPE string,
      ls_pair      TYPE REF TO ts_pair,
      lo_mr_api    TYPE REF TO if_mr_api,
      lv_is_folder TYPE abap_bool,
      lv_url       TYPE string.

    " In prod just view mode
    ls_response-is_dev = zcl_aqo_helper=>is_dev_mandt( ).

    " If BSP send 1 file only
    IF mt_sinon_file IS INITIAL.
      CONCATENATE mc_base_repo 'webapp/localService/metadata.xml' INTO lv_url.
      APPEND lv_url TO mt_sinon_file.
    ENDIF.
    lo_mr_api = cl_mime_repository_api=>if_mr_api~get_api( ).

    LOOP AT mt_sinon_file INTO lv_file.
      " Read whole file
      CLEAR:
       lv_xstr,
       lv_is_folder.
      lo_mr_api->get(
       EXPORTING
         i_url       = lv_file
       IMPORTING
         e_is_folder = lv_is_folder
         e_content   = lv_xstr
       EXCEPTIONS
         OTHERS     = 5 ).
      IF sy-subrc <> 0.
        MESSAGE x000(zaqo_message) WITH 'Load from BSP mime error'(er1) lv_file INTO sy-msgli.
        " hides the error above zcx_aqo_exception=>raise_sys_error( ).
      ENDIF.

      " Skip folder
      CHECK lv_is_folder <> abap_true.
      APPEND INITIAL LINE TO ls_response-pairs REFERENCE INTO ls_pair.
      ls_pair->name = lv_file.

      " Relative path and file content
      REPLACE FIRST OCCURRENCE OF lcl_opt=>mc_base_repo IN ls_pair->name WITH ''.
      ls_pair->file = zcl_aqo_helper=>xstring_to_string( lv_xstr ).
    ENDLOOP.

    " Send back all files
    rv_out = zcl_aqo_helper=>to_json( ls_response ).
  ENDMETHOD.

  METHOD get_entity_desc.
    DATA:
      lv_table_name     TYPE dfies-tabname,
      lv_field_name     TYPE dfies-fieldname,
      ls_field          TYPE REF TO dfies,
      lt_shlp_descr_tab TYPE shlp_desct,
      lo_struc          TYPE REF TO cl_abap_structdescr,
      lo_table          TYPE REF TO cl_abap_tabledescr,
      lt_field_desc     TYPE zcl_aqo_helper=>tt_field_desc,
      ls_field_desc     TYPE zcl_aqo_helper=>ts_field_desc,
      ls_sh_field       TYPE REF TO ts_sh_field,
      lt_fielddescr     TYPE ddfields,
      ls_fld_prop       TYPE REF TO ddshfprop,
      lt_keys           TYPE SORTED TABLE OF fieldname WITH UNIQUE KEY table_line.

    " Based on entity name
    IF iv_entity CP 'DB_*'.
      ev_db     = iv_entity+3.
    ELSEIF iv_entity CP 'SHLP_*'.
      ev_shlp   = iv_entity+5.
    ELSEIF iv_entity CP 'FLD_*'.
      ev_db_fld = iv_entity+4.
    ELSEIF iv_entity CP 'DOM_*'.
      ev_dom    = iv_entity+4.
    ENDIF.

    " Field of table
    IF ev_db_fld IS NOT INITIAL.
      SPLIT ev_db_fld AT '-' INTO lv_table_name lv_field_name.

      " Get top SH
      CALL FUNCTION 'F4IF_DETERMINE_SEARCHHELP'
        EXPORTING
          tabname   = lv_table_name
          fieldname = lv_field_name
        IMPORTING
          shlp      = es_sh_desc
        EXCEPTIONS
          OTHERS    = 1.
      " No SH
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      " Get elementary SH
      CALL FUNCTION 'DD_SHLP_EXPAND_HELPMETHOD'
        EXPORTING
          shlp_top = es_sh_desc
        IMPORTING
          shlp_tab = lt_shlp_descr_tab
        EXCEPTIONS
          OTHERS   = 1.
      IF sy-subrc = 0.
        READ TABLE lt_shlp_descr_tab INDEX 1 INTO es_sh_desc.
      ENDIF.
    ENDIF.

    " Table or view
    IF ev_db IS NOT INITIAL.
      lo_struc ?= cl_abap_tabledescr=>describe_by_name( ev_db ).
      es_sh_desc-fielddescr = lo_struc->get_ddic_field_list( ).
    ENDIF.

    " Search help
    IF ev_shlp IS NOT INITIAL.
      CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
        EXPORTING
          shlpname = ev_shlp
        IMPORTING
          shlp     = es_sh_desc.
    ENDIF.

    " Work with copy
    lt_fielddescr[] = es_sh_desc-fielddescr[].

    " MANDT no need
    DO 1 TIMES.
      CHECK ev_db IS NOT INITIAL.

      READ TABLE lt_fielddescr REFERENCE INTO ls_field INDEX 1.
      CHECK sy-subrc = 0 AND ls_field->keyflag = abap_true AND ls_field->datatype = 'CLNT'.
      DELETE lt_fielddescr INDEX 1.
    ENDDO.

    " Fixed values (from domain)
    IF es_sh_desc-shlptype = 'FV'.
      LOOP AT lt_fielddescr REFERENCE INTO ls_field.
        CASE ls_field->fieldname.
          WHEN '_LOW'.
            ls_field->keyflag = abap_true.
          WHEN '_HIGH'.
            DELETE lt_fielddescr INDEX sy-tabix.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    IF er_table IS REQUESTED.
      IF lo_struc IS INITIAL.
        " Strucure fields
        LOOP AT lt_fielddescr REFERENCE INTO ls_field.
          ls_field_desc = zcl_aqo_helper=>get_field_desc( is_sh_field = ls_field->* ).
          INSERT ls_field_desc INTO TABLE lt_field_desc.
        ENDLOOP.

        " Output table
        lo_struc = zcl_aqo_helper=>create_structure( it_field_desc = lt_field_desc ).
      ENDIF.

      lo_table = cl_abap_tabledescr=>create( p_line_type = lo_struc ).
      CREATE DATA er_table TYPE HANDLE lo_table.
    ENDIF.

    IF et_sh_field IS REQUESTED.
      " Primary key
      LOOP AT lt_fielddescr REFERENCE INTO ls_field WHERE keyflag = abap_true.
        INSERT ls_field->fieldname INTO TABLE lt_keys.
      ENDLOOP.

      " For speed
      SORT es_sh_desc-fieldprop BY fieldname.

      LOOP AT lt_fielddescr REFERENCE INTO ls_field.
        APPEND INITIAL LINE TO et_sh_field REFERENCE INTO ls_sh_field.
        ls_sh_field->field_desc  = zcl_aqo_helper=>get_field_desc( is_sh_field = ls_field->* ).
        " ls_sh_field->short_label = ls_field->scrtext_s.

        READ TABLE lt_keys TRANSPORTING NO FIELDS
         WITH TABLE KEY table_line = ls_field->fieldname.
        IF sy-subrc = 0.
          ls_sh_field->is_key = abap_true.
        ENDIF.

        DO 1 TIMES.
          " SH fields properties
          READ TABLE es_sh_desc-fieldprop REFERENCE INTO ls_fld_prop BINARY SEARCH
           WITH KEY fieldname = ls_field->fieldname.
          CHECK sy-subrc = 0.

          IF ls_fld_prop->shlpinput = abap_true AND ls_fld_prop->shlpselpos > 0.
            ls_sh_field->is_searchable = abap_true.
          ENDIF.

          " No keys
          IF lt_keys[] IS INITIAL
            " Exporting & importing
             AND ls_fld_prop->shlpoutput = abap_true
             AND ls_fld_prop->shlpinput  = abap_true.
            ls_sh_field->is_key = abap_true.
          ENDIF.
        ENDDO.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD sap_get_sh_fields.
    TYPES:
      BEGIN OF ts_out,
        fields TYPE tt_sh_field,
        table  TYPE REF TO data, " Only for domains (combobox)
      END OF ts_out.
    DATA:
      lt_entities TYPE stringtab,
      lv_entity   TYPE string,
      ls_sh_desc  TYPE shlp_descr,
      ls_out      TYPE ts_out,
      lt_out      TYPE STANDARD TABLE OF ts_out WITH DEFAULT KEY.
    FIELD-SYMBOLS:
      <lt_table> TYPE STANDARD TABLE.

    zcl_aqo_helper=>from_json(
     EXPORTING
       iv_json = iv_entities
     IMPORTING
       ex_data = lt_entities ).

    LOOP AT lt_entities INTO lv_entity.
      " Clear from prev values
      CLEAR:
       ls_sh_desc,
       ls_out.

      " Get description
      get_entity_desc(
       EXPORTING
        iv_entity   = lv_entity
       IMPORTING
        es_sh_desc  = ls_sh_desc
        et_sh_field = ls_out-fields
        er_table    = ls_out-table ).

      " For combo
      IF ls_sh_desc-shlptype <> 'FV'.
        CREATE DATA ls_out-table TYPE string.
      ELSE.
        ASSIGN ls_out-table->* TO <lt_table>.

        get_sh_table(
         EXPORTING
          iv_maxrows = 0
         CHANGING
          cs_sh_desc = ls_sh_desc
          ct_table   = <lt_table> ).

        " _LOW & _TEXT
        SORT <lt_table> BY ('_LOW').
      ENDIF.

      " Add to results
      APPEND ls_out TO lt_out.
    ENDLOOP.

    " As one string
    rv_out = zcl_aqo_helper=>to_json( lt_out ).
  ENDMETHOD.

  METHOD get_sh_table.
    DATA:
      ls_fld_prop       TYPE REF TO ddshfprop,
      lt_shlp_return    TYPE STANDARD TABLE OF ddshretval,
      ls_shlp_return    TYPE REF TO ddshretval,
      lt_shlp_record    TYPE STANDARD TABLE OF seahlpres,
      ls_call_control   TYPE ddshf4ctrl,
      lt_shlp_descr_tab TYPE shlp_desct,
      lv_prev_pos       TYPE ddshretval-recordpos,
      ls_field          TYPE REF TO dfies.
    FIELD-SYMBOLS:
      <ls_row>   TYPE any,
      <lv_value> TYPE any.

    " Show all fields
    LOOP AT cs_sh_desc-fieldprop REFERENCE INTO ls_fld_prop.
      ls_fld_prop->shlpoutput = abap_true.
    ENDLOOP.

    " Call with SELECT event only (probably no texts)
    IF cs_sh_desc-intdescr-selmexit IS INITIAL.
      CALL FUNCTION 'F4IF_SELECT_VALUES'
        EXPORTING
          shlp           = cs_sh_desc
          maxrows        = iv_maxrows
          call_shlp_exit = abap_true " 'SELECT' only!
        TABLES
          return_tab     = lt_shlp_return.
    ELSE.
      " Get records first
      CALL FUNCTION 'F4IF_SELECT_VALUES'
        EXPORTING
          shlp           = cs_sh_desc
          maxrows        = iv_maxrows
          call_shlp_exit = abap_true
        TABLES
          record_tab     = lt_shlp_record.

      " Disp event
      ls_call_control-step       = 'DISP'.
      ls_call_control-maxrecords = iv_maxrows.

      APPEND cs_sh_desc TO lt_shlp_descr_tab.
      CALL FUNCTION cs_sh_desc-intdescr-selmexit
        TABLES
          shlp_tab    = lt_shlp_descr_tab
          record_tab  = lt_shlp_record
        CHANGING
          shlp        = cs_sh_desc
          callcontrol = ls_call_control.

      " To normal state -> lt_shlp_return
      CLEAR lt_shlp_return.
      PERFORM transform_outval IN PROGRAM saplsdsd
        TABLES lt_shlp_record lt_shlp_return
        USING ls_call_control cs_sh_desc.
    ENDIF.

    " Write data to table
    lv_prev_pos = 0.
    SORT cs_sh_desc-fielddescr BY fieldname.
    LOOP AT lt_shlp_return REFERENCE INTO ls_shlp_return.
      " New row ?
      IF lv_prev_pos <> ls_shlp_return->recordpos.
        APPEND INITIAL LINE TO ct_table ASSIGNING <ls_row>.
      ENDIF.
      lv_prev_pos = ls_shlp_return->recordpos.

      " value
      ASSIGN COMPONENT ls_shlp_return->fieldname OF STRUCTURE <ls_row> TO <lv_value>.
      CHECK sy-subrc = 0.

      " Copy field field
      READ TABLE cs_sh_desc-fielddescr REFERENCE INTO ls_field BINARY SEARCH
        WITH KEY fieldname = ls_shlp_return->fieldname.

      " Special case for certain types
      CASE ls_field->inttype.
        WHEN cl_abap_typedescr=>typekind_time.
          CONCATENATE ls_shlp_return->fieldval+0(2)
                      ls_shlp_return->fieldval+3(2)
                      ls_shlp_return->fieldval+6(2) INTO <lv_value>.

        WHEN cl_abap_typedescr=>typekind_date.
          CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
            EXPORTING
              date_external = ls_shlp_return->fieldval
            IMPORTING
              date_internal = <lv_value>
            EXCEPTIONS
              OTHERS        = 1.
          IF sy-subrc <> 0.
            CLEAR <lv_value>.
          ENDIF.

          " Integer, byte, short
        WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1  OR cl_abap_typedescr=>typekind_int2.
          REPLACE ALL OCCURRENCES OF '.' IN ls_shlp_return->fieldval WITH ''.
          <lv_value> = ls_shlp_return->fieldval.

        WHEN OTHERS.
          <lv_value> = ls_shlp_return->fieldval.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD sap_odata_query.
    DATA:
      lt_request  TYPE STANDARD TABLE OF ts_request,
      ls_request  TYPE REF TO ts_request,
      lt_response TYPE STANDARD TABLE OF ts_response,
      ls_response TYPE REF TO ts_response.

    zcl_aqo_helper=>from_json(
     EXPORTING
       iv_json = iv_requests
     IMPORTING
       ex_data = lt_request ).

    LOOP AT lt_request REFERENCE INTO ls_request.
      " New Response
      APPEND INITIAL LINE TO lt_response REFERENCE INTO ls_response.

      " Always
      ls_response->success = abap_true.
      ls_response->status  = 200.

      CASE ls_request->entity.
        WHEN 'AqoOptions'.
          get_odata(
           EXPORTING
             iv_entity     = 'SHLP_ZHAQO_OPTION'
             it_params     = ls_request->params
             iv_count_only = ls_request->count_only
           IMPORTING
             es_response   = ls_response->* ).

        WHEN OTHERS.
          get_odata(
           EXPORTING
             iv_entity     = ls_request->entity
             it_params     = ls_request->params
             iv_count_only = ls_request->count_only
           IMPORTING
             es_response   = ls_response->* ).
      ENDCASE.
    ENDLOOP.

    " Send back all files
    rv_out = zcl_aqo_helper=>to_json( lt_response ).
  ENDMETHOD.

  METHOD get_odata.
    DATA:
      lv_db           TYPE string,
      ls_sh_desc      TYPE shlp_descr,
      lr_table        TYPE REF TO data,
      lv_table_name   TYPE dfies-tabname,
      ls_param        TYPE REF TO ts_param,
      lv_skip         TYPE i,
      lv_top          TYPE i,
      lv_count        TYPE i,
      ls_sh_field     TYPE REF TO ts_sh_field,
      ls_sort         TYPE abap_sortorder,
      lv_where        TYPE string,
      ls_filter_param TYPE ts_filter_param,
      ls_sort_param   TYPE ts_sort_param.
    FIELD-SYMBOLS:
      <lv_count> TYPE int4,
      <lt_table> TYPE STANDARD TABLE.

    " Metadata
    get_entity_desc(
     EXPORTING
      iv_entity   = iv_entity
     IMPORTING
      ev_db       = lv_db
      "ev_shlp     = lv_shlp
      es_sh_desc  = ls_sh_desc
      er_table    = lr_table
      et_sh_field = es_response-fields ).

    " Output table
    ASSIGN lr_table->* TO <lt_table>.

    " Return count
    IF iv_count_only = abap_true.
      CREATE DATA es_response-data TYPE int4.
      ASSIGN es_response-data->* TO <lv_count>.
    ENDIF.

    LOOP AT it_params REFERENCE INTO ls_param.
      CASE ls_param->key.
        WHEN '$count'.
          " do nothing iv_count_only = abap_true

        WHEN '$skip'.
          lv_skip = ls_param->value.

        WHEN '$top'.
          lv_top = ls_param->value.

        WHEN '$orderby'.
          zcl_aqo_helper=>from_json(
           EXPORTING
             iv_json = ls_param->value
           IMPORTING
             ex_data = ls_sort_param ).

        WHEN '$filter'.
          zcl_aqo_helper=>from_json(
           EXPORTING
             iv_json = ls_param->value
           IMPORTING
             ex_data = ls_filter_param ).
          ls_sh_desc-selopt = ls_filter_param-sh_filter.
          lv_where = ls_filter_param-db_filter.

        WHEN OTHERS.
          MESSAGE s024(zaqo_message) WITH 'GET_ODATA' ls_param->key.
          zcx_aqo_exception=>raise_sys_error( ).

      ENDCASE.
    ENDLOOP.

    " How many items
    lv_count = lv_skip + lv_top.

    " Order by
    IF ls_sort_param IS INITIAL.
      ls_sort_param-db_sort = 'PRIMARY KEY'.

      " For safety delete $skip, $top
      LOOP AT es_response-fields REFERENCE INTO ls_sh_field WHERE is_key = abap_true.
        ls_sort-name = ls_sh_field->name.
        APPEND ls_sort TO ls_sort_param-sh_sort.
      ENDLOOP.
    ENDIF.

    " Use in DB select in a SH
    zcl_aqo_helper=>sh_sort_order( ls_sort_param-db_sort ).

    " Get table name
    lv_table_name = lv_db.
    IF ls_sh_desc IS NOT INITIAL AND lv_table_name IS INITIAL.
      lv_table_name = ls_sh_desc-intdescr-selmethod.
    ENDIF.

    " Count if no params ($count only)
    IF lv_table_name IS NOT INITIAL AND iv_count_only = abap_true AND lines( it_params ) = 1.
      TRY.
          " Database view?
          SELECT COUNT( * ) INTO <lv_count>
          FROM (lv_table_name)
          WHERE (lv_where).
          RETURN.
        CATCH cx_sy_dynamic_osql_semantics.
          " Based on internal table lines[]
          <lv_count> = 0.
      ENDTRY.
    ENDIF.

    IF lv_db IS NOT INITIAL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE <lt_table>
        UP TO lv_count ROWS
      FROM (lv_db)
      WHERE (lv_where)
      ORDER BY (ls_sort_param-db_sort).
    ENDIF.

    " SHLP_* & FLD_*
    IF ls_sh_desc-fieldprop IS NOT INITIAL.
      get_sh_table(
       EXPORTING
        iv_maxrows = lv_count
       CHANGING
        cs_sh_desc = ls_sh_desc
        ct_table   = <lt_table> ).
    ENDIF.

    " Delete before $skip (No need to delete after $top)
    SORT <lt_table> BY (ls_sort_param-sh_sort).

    IF lv_skip > 0.
      DELETE <lt_table> FROM 1 TO lv_skip.
    ENDIF.

    " If have params get $count from internal table
    IF iv_count_only = abap_true.
      <lv_count> = lines( <lt_table> ).
    ELSE.
      es_response-data = lr_table.
    ENDIF.

    " Just for test
    MESSAGE s025(zaqo_message) WITH lv_top lv_skip.
  ENDMETHOD.

  METHOD sap_drill_down.
    IF iv_again IS INITIAL.
      rv_out = mc_again.
      RETURN.
    ENDIF.

    " Block UI
    zcl_aqo_helper=>drill_down( iv_datatype ).
  ENDMETHOD.

  METHOD sap_navigate_to.
    DATA:
      lt_usage TYPE zcl_aqo_helper=>tt_usage,
      ls_usage TYPE zcl_aqo_helper=>ts_usage,
      ls_opt   TYPE ztaqo_option.

    IF iv_again IS INITIAL.
      rv_out = mc_again.
      RETURN.
    ENDIF.

    " Already set
    IF iv_include IS NOT INITIAL.
      ls_usage-include = iv_include.
      ls_usage-line    = iv_line.
    ELSE.
      lt_usage         = zcl_aqo_helper=>get_usage( ).
      ls_usage-index   = iv_index.
      " From usage list
      READ TABLE lt_usage INTO ls_usage
       WITH KEY index = ls_usage-index.
    ENDIF.

    " Block UI
    zcl_aqo_helper=>navigate_to(
     iv_include  = ls_usage-include
     iv_position = ls_usage-line ).
  ENDMETHOD.

  METHOD sap_download_file.
    DATA:
      lv_encoding TYPE abap_encoding,
      lv_content  TYPE string,
      lv_title    TYPE string,
      lv_path     TYPE string,
      lv_filename TYPE string,
      lv_fullpath TYPE string.

    IF iv_again IS INITIAL.
      rv_out = mc_again.
      RETURN.
    ENDIF.

    lv_title = 'Save option values'(sov).
    cl_gui_frontend_services=>file_save_dialog(
     EXPORTING
       window_title      = lv_title
       default_file_name = iv_file_name
     CHANGING
       path         = lv_path
       filename     = lv_filename
       fullpath     = lv_fullpath
     EXCEPTIONS
       OTHERS       = 1 ).
    CHECK sy-subrc = 0 AND lv_fullpath IS NOT INITIAL.

    " What code page
    lv_encoding = iv_charset.
    IF lv_encoding IS INITIAL.
      lv_encoding = zcl_aqo_helper=>mc_utf8.
    ENDIF.

    zcl_aqo_helper=>download(
     iv_filename = lv_fullpath
     iv_content  = iv_content
     iv_encoding = lv_encoding ).
  ENDMETHOD.

  METHOD get_send_fields.
    DATA:
      ls_field_value      TYPE REF TO zcl_aqo_helper=>ts_field_value,
      ls_field_value_send TYPE REF TO ts_field_value_send,
      lt_sub_columns      LIKE it_field.
    FIELD-SYMBOLS:
      <lt_sub_columns>    LIKE rt_field.

    LOOP AT it_field REFERENCE INTO ls_field_value.
      " Copy as is
      APPEND INITIAL LINE TO rt_field REFERENCE INTO ls_field_value_send.
      MOVE-CORRESPONDING ls_field_value->* TO ls_field_value_send->*.

      " Value for 1-st level only
      IF io_option IS NOT INITIAL.
        ls_field_value_send->cur_value = io_option->get_field_value( ls_field_value->name ).
      ELSE.
        CREATE DATA ls_field_value_send->cur_value TYPE string.
      ENDIF.

      " Sub columns ?
      IF ls_field_value->sub_fdesc IS INITIAL.
        CREATE DATA ls_field_value_send->sub_columns TYPE string.
        CONTINUE.
      ENDIF.

      " Create table on the fly
      CREATE DATA ls_field_value_send->sub_columns LIKE rt_field.
      ASSIGN ls_field_value_send->sub_columns->* TO <lt_sub_columns>.

      CLEAR lt_sub_columns.
      zcl_aqo_helper=>from_json(
       EXPORTING
        iv_json = ls_field_value->sub_fdesc
       IMPORTING
        ex_data = lt_sub_columns ).

      <lt_sub_columns> = get_send_fields(
        it_field      = lt_sub_columns ).
    ENDLOOP.
  ENDMETHOD.

  METHOD sap_read_option.
    TYPES:
      BEGIN OF ts_response,
        rows         TYPE tt_field_value_send,
        " Both affect edit mode (+is_editable) is_dev
        is_read_only TYPE xsdboolean,
        warning_text TYPE string,
      END OF ts_response,

      BEGIN OF ts_lock,
        package_id TYPE ztaqo_option-package_id,
        option_id  TYPE ztaqo_option-option_id,
      END OF ts_lock.

    DATA:
      lo_option   TYPE REF TO zcl_aqo_option,
      ls_response TYPE ts_response,
      lt_seqg3    TYPE STANDARD TABLE OF seqg3 WITH DEFAULT KEY,
      ls_seqg3    TYPE REF TO seqg3,
      ls_lock     TYPE ts_lock.

*    " Unlock previous option. How block for BSP ?
*    IF lo_option IS NOT INITIAL.
*      lo_option->lock( iv_unlock = abap_true ).
*    ENDIF.

    " Locked objects of current user
    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        gname  = 'ZTAQO_OPTION'
      TABLES
        enq    = lt_seqg3
      EXCEPTIONS
        OTHERS = 3.
    IF sy-subrc <> 0.
      CLEAR lt_seqg3.
    ENDIF.

    " Unlock all
    LOOP AT lt_seqg3 REFERENCE INTO ls_seqg3.
      ls_lock = ls_seqg3->garg.

      " Unlock 1 option
      CALL FUNCTION 'DEQUEUE_EZTAQO_OPTION'
        EXPORTING
          package_id = ls_lock-package_id
          option_id  = ls_lock-option_id
          _scope     = '1'
        EXCEPTIONS
          OTHERS     = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      ENDIF.
    ENDLOOP.


    lo_option = zcl_aqo_option=>create(
        iv_package_id = iv_package_id
        iv_option_id  = iv_option_id ).

    " All fields are gray
    IF zcl_aqo_helper=>is_in_editor( iv_is_viewer = abap_true ) = abap_true.
      ls_response-is_read_only = abap_true.
      ls_response-warning_text = 'View mode'(vwm).
    ELSEIF lo_option->lock( ) <> abap_true.
      ls_response-is_read_only = abap_true.

      " Blocked by someone
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_response-warning_text.
    ENDIF.

    " Return without any filter a table
    ls_response-rows = get_send_fields(
     it_field  = lo_option->mt_field_value
     io_option = lo_option ).

    " As one string
    rv_out = zcl_aqo_helper=>to_json( ls_response ).
  ENDMETHOD.

  METHOD sap_save_option.
    TYPES:
      " ! Add fields to lo_option->mt_field_value
      BEGIN OF ts_field_value_get.
        INCLUDE TYPE zcl_aqo_helper=>ts_field_value.
    TYPES:
      new_value TYPE string,
      END OF ts_field_value_get.

    DATA:
      lo_option          TYPE REF TO zcl_aqo_option,
      lt_field_value_get TYPE STANDARD TABLE OF ts_field_value_get WITH DEFAULT KEY,
      ls_field_value_get TYPE REF TO ts_field_value_get,
      ls_field_value     TYPE zcl_aqo_helper=>ts_field_value,
      lr_value           TYPE REF TO data,
      ls_db_item         TYPE ztaqo_option,
      ls_back_info       TYPE ts_back_info.
    FIELD-SYMBOLS:
      <lv_value> TYPE any.

    lo_option = zcl_aqo_option=>create(
        iv_package_id = iv_package_id
        iv_option_id  = iv_option_id ).

    " DB option
    zcl_aqo_helper=>from_json(
     EXPORTING
       iv_json = iv_cur_option
     IMPORTING
       ex_data = ls_db_item ).

    lo_option->ms_db_item-description    = ls_db_item-description.
    lo_option->ms_db_item-prev_value_cnt = ls_db_item-prev_value_cnt.

    " Fields
    zcl_aqo_helper=>from_json(
     EXPORTING
       iv_json = iv_cur_fields
     IMPORTING
       ex_data = lt_field_value_get ).

    " Copy field by field
    CLEAR lo_option->mt_field_value.
    LOOP AT lt_field_value_get REFERENCE INTO ls_field_value_get.
      " New field
      CLEAR ls_field_value.
      MOVE-CORRESPONDING ls_field_value_get->* TO ls_field_value.

      " Add new value
      lo_option->add_history_value(
       EXPORTING
         iv_value       = ls_field_value_get->new_value
       CHANGING
         cs_field_value = ls_field_value ).

      " And add
      INSERT ls_field_value INTO TABLE lo_option->mt_field_value.
    ENDLOOP.

    IF iv_mandt IS INITIAL.
      iv_mandt = sy-mandt.
    ENDIF.

    " Ok
    ls_back_info-kind      = 'I'.
    ls_back_info-info_text = lo_option->save( iv_mandt = iv_mandt ).

    rv_out = zcl_aqo_helper=>to_json( ls_back_info ).    "#EC CI_VALPAR
  ENDMETHOD.

  METHOD sap_get_field_desc.
    DATA:
      ls_field_value TYPE zcl_aqo_helper=>ts_field_value,
      lo_type        TYPE REF TO cl_abap_datadescr,
      lr_data        TYPE REF TO data.
    FIELD-SYMBOLS:
      <lv_data> TYPE any.

    " Create by text description
    lo_type = zcl_aqo_helper=>create_type_descr( iv_rollname = iv_rollname ).
    CREATE DATA lr_data TYPE HANDLE lo_type.

    " Get full description
    ASSIGN lr_data->* TO <lv_data>.
    ls_field_value-field_desc = zcl_aqo_helper=>get_field_desc(
     iv_field_name = iv_name
     iv_data       = <lv_data> ).

    " As one string
    rv_out = zcl_aqo_helper=>to_json( ls_field_value ).
  ENDMETHOD.

  METHOD sap_delete_or_transport.
    DATA:
      lo_option    TYPE REF TO zcl_aqo_option,
      ls_back_info TYPE ts_back_info.

    lo_option = zcl_aqo_option=>create(
        iv_package_id = iv_package_id
        iv_option_id  = iv_option_id ).

    " Type 'E' handled in try block
    ls_back_info-kind = 'I'.
    IF iv_delete = 'true'.
      ls_back_info-info_text = lo_option->delete( iv_request ).
    ELSE.
      ls_back_info-info_text = lo_option->transport( iv_request ).
    ENDIF.

    " Result
    rv_out = zcl_aqo_helper=>to_json( ls_back_info ).
  ENDMETHOD.

ENDCLASS.


*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pbo_100 OUTPUT.
  lcl_opt=>pbo( ).
ENDMODULE.

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_100 INPUT.
  lcl_opt=>pai(
   CHANGING
    cv_cmd = gv_ok_code ).
ENDMODULE.

**&---------------------------------------------------------------------*
**----------------------------------------------------------------------*
*MODULE pai_exit INPUT.
*  lcl_opt=>pai( 'ESCAPE' ).
*ENDMODULE.
