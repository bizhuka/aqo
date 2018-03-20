*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_opt IMPLEMENTATION.
  METHOD class_constructor.
  ENDMETHOD.

  METHOD pbo.
    DATA:
      lo_cont TYPE REF TO cl_gui_custom_container.

    SET PF-STATUS 'ST_MAIN'.
    SET TITLEBAR 'ST_TITLE'.

    " One time only
    CHECK mo_html_viewer IS INITIAL.

    CREATE OBJECT:
      lo_cont EXPORTING
        container_name = 'CUST_CONTROL',

      mo_html_viewer EXPORTING
        io_parent = lo_cont. " cl_gui_container=>screen0.

    load_data( iv_data    = load_from_smw0( 'Z_AQO_UTIL_JS' )
               iv_subtype = 'javascript'
               iv_url     = 'util.js' ).

    load_data( iv_data    = load_from_smw0( 'Z_AQO_START_DIALOG_JS' )
               iv_subtype = 'javascript'
               iv_url     = 'start_dialog.js' ).

    load_data( iv_data    = load_from_smw0( 'Z_AQO_USAGE_JS' )
               iv_subtype = 'javascript'
               iv_url     = 'usage.js' ).

    load_data( iv_data    = load_from_smw0( 'Z_AQO_INDEX_HTML' )
               iv_subtype = 'html'
               iv_url     = 'index.html' ).

    mo_html_viewer->show_url( url = 'index.html'  ).
  ENDMETHOD.

  METHOD get_options.
    TYPES:
      BEGIN OF ts_json,
        object    TYPE ztaqo_data-object,
        subobject TYPE ztaqo_data-subobject,
        uname     TYPE ztaqo_data-uname,
        udate     TYPE char8, "ZTAQO_DATA-udate, for search without delemeters
        utime     TYPE char6, "ZTAQO_DATA-utime, for search without delemeters
        fav       TYPE xsdboolean,
      END OF ts_json.
    DATA:
      lt_cluster TYPE STANDARD TABLE OF ztaqo_data,
      ls_cluster TYPE REF TO ztaqo_data,
      lt_json    TYPE STANDARD TABLE OF ts_json,
      ls_json    TYPE ts_json,
      lv_json    TYPE string,
      lv_objid   TYPE wwwdata-objid,
      lv_i18n    TYPE string,
      ls_own_opt TYPE REF TO ts_own_opt,
      lo_opt     TYPE REF TO zcl_aqo.

    " Saved options
    SELECT DISTINCT object subobject uname udate utime INTO CORRESPONDING FIELDS OF TABLE lt_cluster
    FROM ztaqo_data.

    CREATE:
      DATA   ls_own_opt,
      OBJECT lo_opt EXPORTING
        iv_object    = '$TMP'
        iv_subobject = 'ZAQO_EDIT'
        ir_data      = ls_own_opt.

    IF lo_opt->read( ) IS NOT INITIAL.
      lo_opt->save( iv_confirm = abap_false ).
    ENDIF.

    " Manual convert date and time
    LOOP AT lt_cluster REFERENCE INTO ls_cluster WHERE object    <> lo_opt->ms_cluster-object OR
                                                       subobject <> lo_opt->ms_cluster-subobject.
      CLEAR ls_json.
      MOVE-CORRESPONDING ls_cluster->* TO ls_json.

      " Check is
      READ TABLE ls_own_opt->fav TRANSPORTING NO FIELDS
       WITH TABLE KEY uname = sy-uname object = ls_cluster->object subobject = ls_cluster->subobject.
      IF sy-subrc = 0.
        ls_json-fav = abap_true.
      ENDIF.

      INSERT ls_json INTO TABLE lt_json.
    ENDLOOP.

    " Favorite first
    SORT lt_json BY fav DESCENDING object subobject.
    lv_json = to_json( im_data = lt_json ).

*    zcl_aok_util=>download(
*     im_content  = lv_json
*     im_path     = 'c:\Users\IBM_ADMIN\IdeaProjects\aqo\json\'
*     im_filename = 'data.json'
*     im_open     = abap_false ).
*    BREAK-POINT.

    " SMW0
    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          WRITE sy-langu TO lv_objid.
        WHEN 2.
          lv_objid = 'EN'.
      ENDCASE.

      CONCATENATE 'Z_AQO_I18N_' lv_objid '_JSON' INTO lv_objid.

      lv_i18n = load_from_smw0( iv_objid = lv_objid ).
      IF lv_i18n IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDDO.

    CONCATENATE `"` iv_guid `", ` lv_json `, ` lv_i18n INTO lv_json.
    mo_html_viewer->run_js( iv_function   = 'call_back'
                            iv_param      = lv_json ).
  ENDMETHOD.

  METHOD load_from_smw0.
    DATA: ls_key    TYPE wwwdatatab,
          lv_size_c TYPE wwwparams-value,
          lv_size   TYPE i,
          lt_w3mime TYPE w3mimetabtype.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_objid.

    " Get exact file size
    CALL FUNCTION 'WWWPARAMS_READ'
      EXPORTING
        relid            = ls_key-relid
        objid            = ls_key-objid
        name             = 'filesize'
      IMPORTING
        value            = lv_size_c
      EXCEPTIONS
        entry_not_exists = 1.
    CHECK sy-subrc = 0.

    " Get binary data
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.
    CHECK sy-subrc = 0.

    lv_size = lv_size_c.
    rv_data = binary_to_string( it_table  = lt_w3mime
                                iv_length = lv_size ).
  ENDMETHOD.

  METHOD load_data.
    DATA:
      lv_xstr  TYPE xstring,
      lv_size  TYPE int4,
      lt_xdata TYPE w3mimetabtype.

    " cl_bcs_convert ?
    lv_xstr = string_to_xstring( iv_data ).

    xstring_to_binary(
     EXPORTING
      iv_xstring = lv_xstr
     IMPORTING
      et_table   = lt_xdata
      ev_length  = lv_size ).

    mo_html_viewer->load_data(
      EXPORTING
        type         = 'text'
        subtype      = iv_subtype
        size         = lv_size
        url          = iv_url
      IMPORTING
        assigned_url = rv_url
      CHANGING
        data_table   = lt_xdata
      EXCEPTIONS
        OTHERS       = 1 ) .
  ENDMETHOD.

  METHOD pai.
    CASE iv_cmd.
      WHEN 'ESCAPE'.
        mo_html_viewer->run_js( iv_function = 'closeDialog' ).

    ENDCASE.
  ENDMETHOD.

  METHOD show_option.
    TYPES:
      BEGIN OF ts_t000,
        mandt TYPE t000-mandt,
        mtext TYPE t000-mtext,
      END OF ts_t000,

      BEGIN OF ts_json,
        tech_visible TYPE xsdboolean,
        edit_visible TYPE xsdboolean,

        read_only    TYPE xsdboolean,
        dev_mandt    TYPE xsdboolean,

        title        TYPE string,
        fld_opt      LIKE mo_opt->mt_field_opt,
        copy_to      TYPE STANDARD TABLE OF ts_t000 WITH DEFAULT KEY,

        last_call    TYPE abap_callstack_line,
      END OF ts_json.

    DATA:
      ls_json  TYPE ts_json,
      lv_param TYPE string.
    CREATE OBJECT mo_opt
      EXPORTING
        iv_object         = iv_object
        iv_subobject      = iv_subobject
        iv_save_last_call = abap_false.

    mo_opt->read( ).

    " In prod just edit text
    ls_json-dev_mandt = zcl_aqo=>is_dev_mandt( ).
    IF ls_json-dev_mandt = abap_true.
      ls_json-tech_visible = abap_true.
      ls_json-edit_visible = abap_false.
    ELSE.
      ls_json-tech_visible = abap_false.
      ls_json-edit_visible = abap_true.
    ENDIF.

    " All fields are gray
    IF mo_opt->lock( ) <> abap_true.
      ls_json-read_only = abap_true.

      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    ls_json-fld_opt      = mo_opt->mt_field_opt.
    CONCATENATE iv_object ` - ` iv_subobject INTO ls_json-title.

    " Copy to another MANDT
    SELECT mandt mtext INTO CORRESPONDING FIELDS OF TABLE ls_json-copy_to
    FROM t000
    WHERE mandt <> sy-mandt.

    " Where used
    ls_json-last_call = mo_opt->ms_last_call.

    lv_param = to_json( im_data = ls_json ).

*    zcl_aok_util=>download(
*     im_content  = lv_param
*     im_path     = 'c:\Users\IBM_ADMIN\IdeaProjects\aqo\json\'
*     im_filename = 'opt.json'
*     im_open     = abap_false ).
*    BREAK-POINT.

    CONCATENATE `"` iv_guid `", ` lv_param INTO lv_param.
    mo_html_viewer->run_js( iv_function   = 'call_back'
                            iv_param      = lv_param ). " As json object
  ENDMETHOD.

  METHOD split_type.
    DATA:
      lo_type TYPE REF TO cl_abap_datadescr.

    " Check is table and field name
    CHECK iv_datatype CP '*-*'.

    lo_type = mo_opt->create_type_descr( iv_rollname = iv_datatype ).
    CHECK lo_type IS NOT INITIAL.

    " Drill down
    SPLIT iv_datatype AT '-' INTO ev_table ev_field.
  ENDMETHOD.

  METHOD drill_down.
    DATA:
      lv_tab TYPE dd02v-tabname,
      lv_fld TYPE d021s-fnam.

    split_type(
     EXPORTING
       iv_datatype = iv_datatype
     IMPORTING
       ev_table    = lv_tab
       ev_field    = lv_fld ).
    CHECK lv_fld IS NOT INITIAL.

    CALL FUNCTION 'RS_DD_STRU_EDIT'
      EXPORTING
        objname   = lv_tab
        fname     = lv_fld
        edit_mode = 'S'
      EXCEPTIONS
        OTHERS    = 5.

    " Show as error
    CHECK sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDMETHOD.

  METHOD value_request.
    DATA:
      lt_return    TYPE STANDARD TABLE OF ddshretval,
      ls_return    TYPE REF TO ddshretval,
      lv_tabname   TYPE dfies-tabname,
      lv_fieldname TYPE dfies-fieldname,
      lv_param     TYPE string.

    split_type(
     EXPORTING
       iv_datatype = iv_datatype
     IMPORTING
       ev_table    = lv_tabname
       ev_field    = lv_fieldname ).
    CHECK lv_fieldname IS NOT INITIAL.

    " Show SH
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname    = lv_tabname
        fieldname  = lv_fieldname
      TABLES
        return_tab = lt_return
      EXCEPTIONS
        OTHERS     = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    " 1 value
    READ TABLE lt_return REFERENCE INTO ls_return INDEX 1.
    CHECK sy-subrc = 0.

    CONCATENATE `"` iv_guid `", "` ls_return->fieldval `"` INTO lv_param.
    mo_html_viewer->run_js( iv_function = 'call_back'
                            iv_param    = lv_param ).
  ENDMETHOD.

  METHOD range_request.
    DATA:
      ls_tabfld TYPE rstabfield,
      lo_type   TYPE REF TO cl_abap_datadescr,
      lr_range  TYPE REF TO data,
      lv_ok     TYPE abap_bool,
      lv_param  TYPE string.
    FIELD-SYMBOLS:
      <lt_range> TYPE STANDARD TABLE.

    split_type(
     EXPORTING
       iv_datatype = iv_datatype
     IMPORTING
       ev_table    = ls_tabfld-tablename
       ev_field    = ls_tabfld-fieldname ).
    CHECK ls_tabfld-fieldname IS NOT INITIAL.

    " 1 field
    lo_type = zcl_aqo=>create_type_descr( iv_rollname = iv_datatype ).
    IF sy-subrc <> 0.
      MESSAGE s016(zaqo_mes) WITH iv_datatype DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Table of range
    lo_type = cl_abap_tabledescr=>create(
      p_line_type = zcl_aqo=>create_structure( io_range = lo_type ) ).

    " Create table and assigned it
    CREATE DATA lr_range TYPE HANDLE lo_type.
    ASSIGN lr_range->* TO <lt_range>.

    " Try to convert
    zcl_aqo=>from_json(
     EXPORTING
       iv_json = iv_ranges
     IMPORTING
       ex_data = <lt_range>
       ev_ok   = lv_ok ).
    IF lv_ok <> abap_true.
      MESSAGE s017(zaqo_mes) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Show ranges
    CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
      EXPORTING
        title         = iv_title
        tab_and_field = ls_tabfld
        "just_display  = '' TODO
      TABLES
        range         = <lt_range>
      EXCEPTIONS
        OTHERS        = 1.
    CHECK sy-subrc = 0.

    lv_param = zcl_aqo=>to_json( im_data = <lt_range> ).
    CONCATENATE `"` iv_guid `", ` lv_param INTO lv_param.
    mo_html_viewer->run_js( iv_function  = 'call_back'
                            iv_param     = lv_param ).
  ENDMETHOD.

  METHOD save_option.
    DATA:
      lt_fld_opt     LIKE mo_opt->mt_field_opt,
      lv_ok          TYPE abap_bool,
      lt_empty_field TYPE stringtab,
      lv_field       TYPE string,
      lv_param       TYPE string.
    FIELD-SYMBOLS:
      <ls_fld_opt> LIKE LINE OF lt_fld_opt.

    " Try to convert
    zcl_aqo=>from_json(
     EXPORTING
       iv_json = iv_option
     IMPORTING
       ex_data = lt_fld_opt
       ev_ok   = lv_ok ).
    IF lv_ok <> abap_true.
      MESSAGE s017(zaqo_mes) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT lt_fld_opt ASSIGNING <ls_fld_opt>.
      IF <ls_fld_opt>-text IS INITIAL.
        MESSAGE e001(zaqo_mes) WITH <ls_fld_opt>-name.
      ENDIF.

      " Make the fields look the similar
      TRANSLATE <ls_fld_opt>-rollname TO UPPER CASE.

      " Check type
      IF ( <ls_fld_opt>-kind = mc_kind_parameter OR <ls_fld_opt>-kind = mc_kind_select_option ) AND
         zcl_aqo=>create_type_descr( iv_rollname = <ls_fld_opt>-rollname ) IS INITIAL.
        MESSAGE e007(zaqo_mes) WITH <ls_fld_opt>-name.
      ENDIF.
    ENDLOOP.

    " Set new value and check it
    mo_opt->mt_field_opt = lt_fld_opt.
    lt_empty_field = mo_opt->read( iv_safe_read = abap_true ).

    IF lt_empty_field IS INITIAL.
      lv_param = 'true'.
      mo_opt->save( iv_confirm     = abap_false
                    iv_use_me_data = abap_false
                    iv_mandt       = iv_mandt ).
    ELSE.
      lv_param = 'false'.
    ENDIF.
    CONCATENATE `"` iv_guid `", ` lv_param INTO lv_param.
    mo_html_viewer->run_js( iv_function = 'call_back' iv_param = lv_param ).

    " Show messages
    LOOP AT lt_empty_field INTO lv_field.
      READ TABLE lt_fld_opt ASSIGNING <ls_fld_opt>
       WITH TABLE KEY name COMPONENTS name = lv_field.
      CHECK sy-subrc = 0.

      IF <ls_fld_opt>-kind = mc_kind_table.
        MESSAGE e005(zaqo_mes) WITH <ls_fld_opt>-text.
      ELSE.
        MESSAGE e018(zaqo_mes) WITH <ls_fld_opt>-text.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD transport_option.
    mo_opt->transport( ).
  ENDMETHOD.

  METHOD delete_option.
    DATA:
      lv_param TYPE string.

    IF mo_opt->delete( iv_confirm	= abap_false ) = abap_true.
      lv_param = 'true'.
    ELSE.
      lv_param = 'false'.
    ENDIF.

    CONCATENATE `"` iv_guid `", ` lv_param INTO lv_param.
    mo_html_viewer->run_js( iv_function = 'call_back' iv_param = lv_param ).
  ENDMETHOD.

  METHOD navigate_to.
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation   = 'SHOW'
        object_name = iv_include
        object_type = 'REPS'
        position    = iv_position
      EXCEPTIONS
        OTHERS      = 3.
    CHECK sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDMETHOD.

  METHOD deep_scan.
    TYPES:
      BEGIN OF ts_usage.
        INCLUDE TYPE wbcrossgt.
    TYPES:
      meth  TYPE string,
      line  TYPE i,
      found TYPE abap_bool,
      END OF ts_usage.
    DATA:
      lt_usage      TYPE STANDARD TABLE OF ts_usage,
      ls_usage      TYPE REF TO ts_usage,
      lv_len        TYPE i,
      lv_class_name TYPE seoclskey,
      lv_rem        TYPE string,
      lo_clif       TYPE REF TO if_oo_clif_incl_naming,
      lo_cldesc     TYPE REF TO if_oo_class_incl_naming,
      lt_meth       TYPE seop_methods_w_include,
      ls_meth       TYPE REF TO seop_method_w_include,
      lv_param      TYPE string.

    " Index for Global Types - Where-Used List Workbench
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_usage
    FROM wbcrossgt
    WHERE otype = 'ME'
      AND name  = 'ZCL_AQO\ME:CONSTRUCTOR'.

    LOOP AT lt_usage REFERENCE INTO ls_usage.
      get_position(
       EXPORTING
         iv_include = ls_usage->include
       IMPORTING
         ev_line    = ls_usage->line
         ev_found   = ls_usage->found ).

      " Is class
      lv_len = strlen( ls_usage->include ).
      CHECK lv_len = 35.
      lv_class_name = ls_usage->include(30).
      SPLIT lv_class_name AT '=' INTO lv_class_name lv_rem.

      " Try to get methods
      cl_oo_include_naming=>get_instance_by_cifkey(
        EXPORTING
         cifkey = lv_class_name
        RECEIVING
         cifref = lo_clif
        EXCEPTIONS
          OTHERS = 1 ).
      CHECK sy-subrc = 0.
      lo_cldesc ?= lo_clif.

      " Find name
      lt_meth = lo_cldesc->get_all_method_includes( ).
      READ TABLE lt_meth REFERENCE INTO ls_meth
       WITH KEY incname = ls_usage->include.
      CHECK sy-subrc = 0.

      ls_usage->meth = ls_meth->cpdkey-cpdname.
    ENDLOOP.

    SORT lt_usage STABLE BY found DESCENDING.

    " Show result
    lv_param = to_json( lt_usage ).
    CONCATENATE `"` iv_guid `", ` lv_param INTO lv_param.
    mo_html_viewer->run_js( iv_function   = 'call_back'
                            iv_param      = lv_param ).
  ENDMETHOD.

  METHOD set_favorite.
    DATA:
      ls_own_opt TYPE REF TO ts_own_opt,
      lo_opt     TYPE REF TO zcl_aqo,
      ls_fav     TYPE ts_fav,
      lv_ok      TYPE string,
      lv_param   TYPE string.

    ls_fav-uname     = sy-uname.
    ls_fav-object    = iv_object.
    ls_fav-subobject = iv_subobject.

    CREATE:
      DATA   ls_own_opt,
      OBJECT lo_opt EXPORTING
        iv_object    = '$TMP'
        iv_subobject = 'ZAQO_EDIT'
        ir_data      = ls_own_opt.

    lv_ok = 'false'.
    DO 1 TIMES.
      CHECK lo_opt->read( ) IS INITIAL.

      CHECK lo_opt->lock( ) = abap_true.

      IF iv_favorite = 'true'.
        INSERT ls_fav INTO TABLE ls_own_opt->fav.
      ELSE.
        DELETE ls_own_opt->fav WHERE uname     = ls_fav-uname
                                 AND object    = ls_fav-object
                                 AND subobject = ls_fav-subobject.
      ENDIF.
      lo_opt->save( iv_confirm = abap_false iv_message = abap_false ).

      CHECK lo_opt->lock( iv_unlock = abap_true ) = abap_true.

      lv_ok = 'true'.
    ENDDO.

    " Show own messages
    IF lv_ok <> 'true'.
      MESSAGE TEXT-m01 TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      CASE iv_favorite.
        WHEN 'true'.
          MESSAGE TEXT-m02 TYPE 'S'.
        WHEN 'false'.
          MESSAGE TEXT-m03 TYPE 'S'.
      ENDCASE.
    ENDIF.

    CONCATENATE `"` iv_guid `", "` lv_ok `"` INTO lv_param.
    mo_html_viewer->run_js( iv_function = 'call_back'
                            iv_param    = lv_param ).
  ENDMETHOD.

  METHOD get_position.
    DATA:
      lt_report TYPE stringtab,
      lt_result TYPE match_result_tab,
      ls_result TYPE REF TO match_result,
      lv_string TYPE string,
      lv_from   TYPE i,
      lv_index  TYPE syindex.

    " Read whole text
    READ REPORT iv_include INTO lt_report.

    " First occurance
    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          lv_string = mo_opt->ms_key-object.
        WHEN 2.
          lv_string = 'IV_OBJECT'.
        WHEN OTHERS.
      ENDCASE.

      REPLACE ALL OCCURRENCES OF '$' IN lv_string WITH ''.
      CONCATENATE '\b' lv_string '\b' INTO lv_string.
      FIND FIRST OCCURRENCE OF REGEX lv_string IN TABLE lt_report IGNORING CASE RESULTS lt_result.

      " Found or not
      READ TABLE lt_result INDEX 1 REFERENCE INTO ls_result.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDDO.

    " Second one
    CHECK ls_result IS NOT INITIAL.
    lv_from = ls_result->line - 4.
    IF lv_from <= 0.
      lv_from = 1.
    ENDIF.

    DO 2 TIMES.
      lv_index = sy-index.
      CASE lv_index.
        WHEN 1.
          lv_string = mo_opt->ms_key-subobject.
        WHEN 2.
          lv_string = 'IV_SUBOBJECT'.
        WHEN OTHERS.
      ENDCASE.

      CONCATENATE '\b' lv_string '\b' INTO lv_string.
      FIND FIRST OCCURRENCE OF REGEX lv_string IN TABLE lt_report FROM lv_from IGNORING CASE.
      CHECK sy-subrc = 0.

      ev_line = ls_result->line - 1.
      IF lv_index = 1.
        ev_found = abap_true.
      ENDIF.
      EXIT.
    ENDDO.
  ENDMETHOD.

  METHOD binary_to_string.
    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        input_length = iv_length
        encoding     = '4110'
      IMPORTING
        text_buffer  = rv_string
      TABLES
        binary_tab   = it_table.
  ENDMETHOD.

  METHOD string_to_xstring.
    " rv_xstring = cl_bcs_convert=>string_to_xstring( iv_string = iv_string iv_codepage = mc_utf8 ).
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text     = iv_string
        encoding = '4110'
      IMPORTING
        buffer   = rv_xstring.
  ENDMETHOD.

  METHOD xstring_to_binary.
    " et_table = cl_bcs_convert=>xstring_to_solix( iv_xstring ).
    " ev_length = xstrlen( iv_xstring ).
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = iv_xstring
      IMPORTING
        output_length = ev_length
      TABLES
        binary_tab    = et_table.
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
  lcl_opt=>pai( gv_ok_code ).
ENDMODULE.

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_exit INPUT.
  lcl_opt=>pai( 'ESCAPE' ).
ENDMODULE.
