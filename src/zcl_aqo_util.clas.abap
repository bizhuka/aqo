class ZCL_AQO_UTIL definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_usage,
        index   TYPE syindex,
        include TYPE wbcrossgt-include,
        meth    TYPE seocpdname,
        uname   TYPE syuname, "wbcrossgt-uname,
        udate   TYPE sydatum, "wbcrossgt-udate,
        uzeit   TYPE syuzeit, "wbcrossgt-uzeit,
        line    TYPE i,
        found   TYPE xsdboolean,
      END OF ts_usage .
  types:
    tt_usage TYPE STANDARD TABLE OF ts_usage WITH DEFAULT KEY .

  constants MC_UTF8 type ABAP_ENCODING value '4110' ##NO_TEXT.

  class-methods CREATE_TYPE_DESCR
    importing
      !IV_ROLLNAME type CSEQUENCE optional
      !IS_COMP type ZCL_AQO=>TS_COMP optional
      value(IR_TYPE) type ref to DATA optional
    returning
      value(RO_TYPE) type ref to CL_ABAP_DATADESCR .
  class-methods CREATE_STRUCTURE
    importing
      !IO_RANGE type ref to CL_ABAP_DATADESCR optional
      !IV_COMPS type STRING optional
      !IT_FIELD_OPT type ZCL_AQO=>TT_FIELD_OPT optional
    returning
      value(RO_STRUCT) type ref to CL_ABAP_STRUCTDESCR
    exceptions
      UNKNOWN_TYPE .
  class-methods CREATE_FIELD_CATALOG
    importing
      value(IR_STRUC) type ref to DATA optional
      !IO_STRUC type ref to CL_ABAP_STRUCTDESCR optional
      !IV_SORT type ABAP_BOOL optional
    exporting
      !ET_FIELDCAT type LVC_T_FCAT
    changing
      !CT_TABLE type STANDARD TABLE optional .
  class-methods FIND_TABLE_FIELDNAME
    importing
      !IV_NAME type CSEQUENCE
    changing
      !CV_ROLLNAME type CSEQUENCE
      !CV_TEXT type CSEQUENCE optional
      !CT_UNIQUE type ZCL_AQO=>TT_UNIQUE .
  class-methods TO_JSON
    importing
      !IM_DATA type ANY
    returning
      value(RV_JSON) type STRING .
  class-methods FROM_JSON
    importing
      !IV_JSON type STRING
    exporting
      !EX_DATA type ANY
      !EV_OK type ABAP_BOOL .
  class-methods IS_DEV_MANDT
    returning
      value(RV_EDITABLE) type ABAP_BOOL .
  class-methods SPLIT_TYPE
    importing
      !IV_DATATYPE type CSEQUENCE
    exporting
      !EV_TABLE type CSEQUENCE
      !EV_FIELD type CSEQUENCE .
  class-methods DRILL_DOWN
    importing
      !IV_DATATYPE type CSEQUENCE
    exporting
      !EV_TABLE type CSEQUENCE
      !EV_FIELD type CSEQUENCE .
  class-methods NAVIGATE_TO
    importing
      !IV_INCLUDE type CSEQUENCE
      !IV_POSITION type I .
  class-methods GET_USAGE
    importing
      !IV_OBJECT type ZTAQO_DATA-OBJECT
      !IV_SUBOBJECT type ZTAQO_DATA-SUBOBJECT
    returning
      value(RT_USAGE) type TT_USAGE .
  class-methods BINARY_TO_STRING
    importing
      !IT_TABLE type STANDARD TABLE
      !IV_LENGTH type I
    returning
      value(RV_STRING) type STRING .
  class-methods STRING_TO_XSTRING
    importing
      !IV_STRING type STRING
    returning
      value(RV_XSTRING) type XSTRING .
  class-methods XSTRING_TO_BINARY
    importing
      !IV_XSTRING type XSTRING
    exporting
      !EV_LENGTH type I
      !ET_TABLE type W3MIMETABTYPE .
  class-methods XSTRING_TO_STRING
    importing
      !IV_XSTRING type XSTRING
    returning
      value(RV_STRING) type STRING .
protected section.
private section.

  class-methods GET_POSITION
    importing
      !IV_INCLUDE type CSEQUENCE
      !IV_OBJECT type ZTAQO_DATA-OBJECT
      !IV_SUBOBJECT type ZTAQO_DATA-SUBOBJECT
    exporting
      !EV_LINE type I
      !EV_FOUND type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_AQO_UTIL IMPLEMENTATION.


METHOD binary_to_string.
  CALL FUNCTION 'SCMS_BINARY_TO_STRING'
    EXPORTING
      input_length = iv_length
      encoding     = mc_utf8
    IMPORTING
      text_buffer  = rv_string
    TABLES
      binary_tab   = it_table.
ENDMETHOD.


METHOD create_field_catalog.
  DATA:
    lr_table        TYPE REF TO data,
    lr_salv         TYPE REF TO cl_salv_table,
    lr_columns      TYPE REF TO cl_salv_columns_table,
    lr_aggregations TYPE REF TO cl_salv_aggregations.
  FIELD-SYMBOLS:
    <ls_data>     TYPE any,
    <lt_table>    TYPE STANDARD TABLE,
    <ls_fieldcat> LIKE LINE OF et_fieldcat.

  " №1 - Based on structure description
  IF io_struc IS NOT INITIAL.
    CREATE DATA ir_struc TYPE HANDLE io_struc.
  ENDIF.

  " №2 - Based on standard table
  IF ct_table IS SUPPLIED.
    ASSIGN ct_table TO <lt_table>.
  ELSEIF ir_struc IS NOT INITIAL.
    " №3 - Based on structure reference
    ASSIGN ir_struc->* TO <ls_data>.

    " Create table
    CREATE DATA lr_table LIKE STANDARD TABLE OF <ls_data>.
    ASSIGN lr_table->* TO <lt_table>.
  ENDIF.

  " Fields catalog
  TRY.
      cl_salv_table=>factory(
       IMPORTING
         r_salv_table   = lr_salv
       CHANGING
         t_table        = <lt_table>  ).

      lr_columns      = lr_salv->get_columns( ).
      lr_aggregations = lr_salv->get_aggregations( ).
      et_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
        r_columns      = lr_columns
        r_aggregations = lr_aggregations ).

      IF iv_sort = abap_true.
        SORT et_fieldcat BY fieldname.
      ENDIF.

      LOOP AT et_fieldcat ASSIGNING <ls_fieldcat>.
        IF <ls_fieldcat>-coltext IS INITIAL.
          <ls_fieldcat>-coltext = <ls_fieldcat>-reptext.
        ENDIF.

        IF <ls_fieldcat>-coltext IS INITIAL.
          <ls_fieldcat>-coltext = <ls_fieldcat>-scrtext_l.
        ENDIF.
      ENDLOOP.
    CATCH cx_salv_error.                                "#EC NO_HANDLER
  ENDTRY.
ENDMETHOD.


METHOD create_structure.
  DATA:
    lv_field   TYPE REF TO fieldname,
    lo_type    TYPE REF TO cl_abap_datadescr,
    lt_comp    TYPE abap_component_tab,
    lt_subcomp TYPE STANDARD TABLE OF zcl_aqo=>ts_comp,
    lr_type    TYPE REF TO data,
    lv_ok      TYPE abap_bool.
  FIELD-SYMBOLS:
    <ls_field_opt> TYPE zcl_aqo=>ts_field_opt,
    <ls_subfield>  TYPE zcl_aqo=>ts_comp,
    <ls_comp>      LIKE LINE OF lt_comp.

  " №2 For select-options
  IF io_range IS NOT INITIAL.
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'SIGN'.
    <ls_comp>-type = cl_abap_elemdescr=>get_c( p_length = 1 ).

    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'OPTION'.
    <ls_comp>-type = cl_abap_elemdescr=>get_c( p_length = 2 ).

    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'LOW'.
    <ls_comp>-type = io_range.

    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'HIGH'.
    <ls_comp>-type = io_range.
  ENDIF.

  " №3 For table's strcuture
  DO 1 TIMES.
    CHECK iv_comps IS NOT INITIAL.
    from_json(
     EXPORTING
       iv_json = iv_comps
     IMPORTING
       ex_data = lt_subcomp
       ev_ok   = lv_ok ).
    CHECK lv_ok = abap_true.

    LOOP AT lt_subcomp ASSIGNING <ls_subfield>.
      APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
      <ls_comp>-name = <ls_subfield>-name.

      CLEAR lo_type.

      " For tables speed 2
      IF <ls_subfield>-kind = zcl_aqo=>mc_kind_parameter.
        lo_type = create_type_descr( iv_rollname = <ls_subfield>-rollname ).
      ENDIF.

      IF lo_type IS INITIAL.
        CASE <ls_subfield>-sys_type.
          WHEN cl_abap_typedescr=>typekind_char.
            lo_type = cl_abap_elemdescr=>get_c( p_length = <ls_subfield>-length ).
          WHEN cl_abap_typedescr=>typekind_date.
            lo_type = cl_abap_elemdescr=>get_d( ).
          WHEN cl_abap_typedescr=>typekind_int.
            lo_type = cl_abap_elemdescr=>get_i( ).
          WHEN cl_abap_typedescr=>typekind_float.
            lo_type = cl_abap_elemdescr=>get_f( ).
          WHEN cl_abap_typedescr=>typekind_num.
            lo_type = cl_abap_elemdescr=>get_n( p_length = <ls_subfield>-length ).
          WHEN cl_abap_typedescr=>typekind_packed.
            lo_type = cl_abap_elemdescr=>get_p( p_length = <ls_subfield>-length p_decimals = <ls_subfield>-decimals ).
          WHEN cl_abap_typedescr=>typekind_string.
            lo_type = cl_abap_elemdescr=>get_string( ).
          WHEN cl_abap_typedescr=>typekind_time.
            lo_type = cl_abap_elemdescr=>get_t( ).
          WHEN cl_abap_typedescr=>typekind_table.
            "create_structure( iv_comps = <ls_subfield>-subcomps )
            lo_type = create_type_descr( is_comp = <ls_subfield> ).

          WHEN OTHERS.
            MESSAGE e007(zaqo_mes) WITH <ls_comp>-name RAISING unknown_type.
        ENDCASE.
      ENDIF.

      <ls_comp>-type = lo_type.
    ENDLOOP.
  ENDDO.

  " №4 Called from constructor if have in DB cluster
  LOOP AT it_field_opt ASSIGNING <ls_field_opt>.
    " Create sub level
    CLEAR: lo_type.

    " № 1 - level
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = <ls_field_opt>-name.
    <ls_comp>-type = create_type_descr( is_comp = <ls_field_opt>-comp ).
  ENDLOOP.

  ro_struct = cl_abap_structdescr=>create( lt_comp ).
ENDMETHOD.


METHOD CREATE_TYPE_DESCR.
  " No type
  CLEAR ro_type.

  " №0
  DO 1 TIMES.
    CHECK is_comp IS SUPPLIED.

*    " structure for DB
*    IF mo_ui_ext IS NOT INITIAL.
*      TRY.
*          ir_type = mo_ui_ext->create_field_type(
*           iv_name = is_comp-name
*           iv_type = zif_prog_params_ui_ext=>mc_type_db ).
*        CATCH cx_sy_dyn_call_illegal_method.
*          CLEAR ir_type.
*      ENDTRY.
*      CHECK ir_type IS INITIAL.
*    ENDIF.

    " For tables speed 1
    ro_type = create_type_descr( iv_rollname = is_comp-rollname ).
    CASE is_comp-kind.
        " P
      WHEN zcl_aqo=>mc_kind_parameter.

        " S
      WHEN zcl_aqo=>mc_kind_select_option.
        " Call №2 recursion
        ro_type = cl_abap_tabledescr=>create( p_line_type = create_structure( io_range = ro_type ) ).

        " T
      WHEN zcl_aqo=>mc_kind_table.
        " Call №3 recursion
        IF ro_type IS INITIAL.
          ro_type = create_structure( iv_comps = is_comp-subcomps ).
        ENDIF.

        ro_type = cl_abap_tabledescr=>create(
          p_line_type   = ro_type
          p_table_kind  = is_comp-table_kind
          p_unique      = is_comp-unique
          p_key         = is_comp-key
          p_key_kind    = is_comp-key_defkind ).
    ENDCASE.
  ENDDO.

  CHECK ro_type IS INITIAL.

  TRY.
      " №1 - Create from text
      IF ir_type IS INITIAL AND iv_rollname IS NOT INITIAL.
        CREATE DATA ir_type TYPE (iv_rollname).
      ENDIF.
      CHECK ir_type IS NOT INITIAL.

      " №2 - Based on incoming reference
      ro_type ?= cl_abap_datadescr=>describe_by_data_ref( ir_type ).
    CATCH cx_root ##CATCH_ALL.
      CLEAR ro_type.
  ENDTRY.
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


METHOD find_table_fieldname.
  TYPES:
    BEGIN OF ts_dd03l,
      tabname    TYPE dd03l-tabname,
      fieldname  TYPE dd03l-fieldname,
      shlporigin TYPE dd03l-shlporigin,
      "ddtext     TYPE dd03t-ddtext,
      tab_len    TYPE i,
    END OF ts_dd03l.

  DATA:
    lv_rollname TYPE rollname,
    lt_dd03l    TYPE STANDARD TABLE OF ts_dd03l,
    ls_dd03l    TYPE REF TO ts_dd03l,
    lv_tabfld   TYPE string,
    ls_dd04t    TYPE dd04t,
    lo_type     TYPE REF TO cl_abap_datadescr.

  " Table Fields
  CHECK cv_rollname IS NOT INITIAL.
  lv_rollname = cv_rollname.

  SELECT d~tabname d~fieldname d~shlporigin INTO CORRESPONDING FIELDS OF TABLE lt_dd03l ##TOO_MANY_ITAB_FIELDS
  FROM dd03l AS d UP TO 100 ROWS
  WHERE d~rollname = lv_rollname AND d~as4local = 'A' AND d~tabname NOT LIKE '/%' AND d~depth = 0.

  " Find short table name
  LOOP AT lt_dd03l REFERENCE INTO ls_dd03l.
    ls_dd03l->tab_len = strlen( ls_dd03l->tabname ).

    " In the end
    IF ls_dd03l->shlporigin IS NOT INITIAL.
      ls_dd03l->tab_len = ls_dd03l->tab_len - 1000.
    ENDIF.
  ENDLOOP.
  SORT lt_dd03l BY tab_len ASCENDING.

  " Try to find
  LOOP AT lt_dd03l REFERENCE INTO ls_dd03l.
    CONCATENATE ls_dd03l->tabname '-' ls_dd03l->fieldname INTO lv_tabfld.

    " Type exist
    lo_type = create_type_descr( iv_rollname = lv_tabfld ).
    CHECK lo_type IS NOT INITIAL.

    " Get next item
*    IF ct_unique IS SUPPLIED.
      READ TABLE ct_unique TRANSPORTING NO FIELDS
       WITH TABLE KEY table_line = lv_tabfld.
      CHECK sy-subrc <> 0.
*    ENDIF.
    cv_rollname = lv_tabfld.

    DO 1 TIMES.
      " If have no text
      CHECK cv_text IS SUPPLIED AND cv_text IS INITIAL.

      " №2
      SELECT SINGLE * INTO ls_dd04t
      FROM dd04t
      WHERE rollname   = lv_rollname
        AND ddlanguage = sy-langu
        AND as4local   = 'A'
        AND as4vers    = 0.
      CHECK sy-subrc = 0.

      IF ls_dd04t-ddtext IS NOT INITIAL.
        cv_text = ls_dd04t-ddtext.
      ELSE.
        cv_text = ls_dd04t-reptext.
      ENDIF.
    ENDDO.

    RETURN.
  ENDLOOP.
ENDMETHOD.


METHOD FROM_JSON.
  DATA:
    lo_error TYPE REF TO cx_root. "cx_transformation_error.

  " No need
  IF iv_json IS INITIAL.
    ev_ok = abap_false. " !!!
    RETURN.
  ENDIF.

*  " Add text for converion
*  IF iv_pure = abap_true.
*    IF iv_kind = mc_kind_table OR iv_kind = mc_kind_select_option.
*      CONCATENATE `{"DATA":` iv_json  `}` INTO lv_json.
*    ELSE.
*      CONCATENATE `{"DATA":"` iv_json  `"}` INTO lv_json.
*    ENDIF.
*  ELSE.
*    lv_json = iv_json.
*  ENDIF.
  TRY.
      CALL TRANSFORMATION id SOURCE XML iv_json
                             RESULT data = ex_data.
      ev_ok = abap_true.
    CATCH cx_root INTO lo_error. " transformation_error
      ev_ok = abap_false.
  ENDTRY.
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
        lv_string = iv_object.
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
        lv_string = iv_subobject.
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


METHOD get_usage.
  DATA:
    ls_usage      TYPE REF TO ts_usage,
    lv_len        TYPE i,
    lv_class_name TYPE seoclskey,
    lv_rem        TYPE string,
    lo_clif       TYPE REF TO if_oo_clif_incl_naming,
    lo_cldesc     TYPE REF TO if_oo_class_incl_naming,
    lt_meth       TYPE seop_methods_w_include,
    ls_meth       TYPE REF TO seop_method_w_include.

  " Index for Global Types - Where-Used List Workbench
  SELECT * INTO CORRESPONDING FIELDS OF TABLE rt_usage
  FROM wbcrossgt
  WHERE otype = 'ME'
    AND name  = 'ZCL_AQO\ME:CONSTRUCTOR'.

  LOOP AT rt_usage REFERENCE INTO ls_usage.
    get_position(
     EXPORTING
       iv_include   = ls_usage->include
       iv_object    = iv_object
       iv_subobject = iv_subobject
     IMPORTING
       ev_line      = ls_usage->line
       ev_found     = ls_usage->found ).

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

  SORT rt_usage STABLE BY found DESCENDING.

  " Unique number
  LOOP AT RT_USAGE REFERENCE INTO ls_usage.
    ls_usage->index = sy-tabix.
  ENDLOOP.
ENDMETHOD.


METHOD IS_DEV_MANDT.
  DATA:
    lv_cccoractiv TYPE t000-cccoractiv.

  " Check client
  SELECT SINGLE cccoractiv INTO lv_cccoractiv
  FROM t000
  WHERE mandt = sy-mandt.
  IF lv_cccoractiv <> '2'.
    rv_editable = abap_true.
  ENDIF.
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


METHOD split_type.
  DATA:
    lo_type TYPE REF TO cl_abap_datadescr.

  " Check is table and field name
  CHECK iv_datatype CP '*-*'.

  lo_type = zcl_aqo_util=>create_type_descr( iv_rollname = iv_datatype ).
  CHECK lo_type IS NOT INITIAL.

  " Drill down
  SPLIT iv_datatype AT '-' INTO ev_table ev_field.
ENDMETHOD.


METHOD string_to_xstring.
  " rv_xstring = cl_bcs_convert=>string_to_xstring( iv_string = iv_string iv_codepage = mc_utf8 ).
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text     = iv_string
      encoding = mc_utf8
    IMPORTING
      buffer   = rv_xstring.
ENDMETHOD.


METHOD to_json.
  DATA:
    lo_writer TYPE REF TO cl_sxml_string_writer,
    lv_xtring TYPE xstring.

  " IF lo_writer IS INITIAL. ELSE lo_writer->initialize( ).
  lo_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

  CALL TRANSFORMATION id SOURCE data = im_data
                         RESULT XML lo_writer.

  lv_xtring = lo_writer->get_output( ).

  " downgrad ?
  rv_json = zcl_aqo_util=>xstring_to_string( lv_xtring ).

*  " delete surroundin DATA
*  IF iv_pure = abap_true.
*    lv_end = strlen( rv_json ).
*
*    IF rv_json(9) CP `{"DATA":"`.
*      lv_beg = 9.
*      lv_end = lv_end - 11.
*    ELSE.
*      lv_beg = 8.
*      lv_end = lv_end - 9.
*    ENDIF.
*
*    rv_json = rv_json+lv_beg(lv_end).
*  ENDIF.
*
*  CHECK iv_prefix IS SUPPLIED.
*  CONCATENATE iv_prefix rv_json INTO rv_json RESPECTING BLANKS.
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


METHOD xstring_to_string.
  DATA:
    lo_conv  TYPE REF TO cl_abap_conv_in_ce.

  lo_conv = cl_abap_conv_in_ce=>create(
   encoding = mc_utf8
   input    = iv_xstring ).

  lo_conv->read(
   IMPORTING
    data =  rv_string ).
ENDMETHOD.
ENDCLASS.
