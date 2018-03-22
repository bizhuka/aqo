class ZCL_AQO_UTIL definition
  public
  final
  create public .

public section.

  constants MC_UTF8 type CPCODEPAGE value '4110' ##NO_TEXT.

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
  class-methods FIND_TABLE_FIELDNAME
    importing
      !IV_NAME type CSEQUENCE
    changing
      !CV_ROLLNAME type CSEQUENCE
      !CV_TEXT type CSEQUENCE optional .
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
protected section.
private section.
ENDCLASS.



CLASS ZCL_AQO_UTIL IMPLEMENTATION.


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


METHOD FIND_TABLE_FIELDNAME.
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

*    CHECK is_ok_table_fieldname( iv_name = iv_name lv_rollname = lv_tabfld ) = abap_true.
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


METHOD TO_JSON.
  DATA:
    lo_writer TYPE REF TO cl_sxml_string_writer,
    lv_xtring TYPE xstring.

  " IF lo_writer IS INITIAL. ELSE lo_writer->initialize( ).
  lo_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

  CALL TRANSFORMATION id SOURCE data = im_data
                         RESULT XML lo_writer.

  lv_xtring = lo_writer->get_output( ).

  " downgrad ?
  rv_json = cl_bcs_convert=>xstring_to_string( iv_xstr = lv_xtring iv_cp = mc_utf8 ).

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
ENDCLASS.
