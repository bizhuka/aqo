class ZCL_AQO_UTIL definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  types:
    BEGIN OF ts_comp,
        name        TYPE fieldname,
        sys_type    TYPE abap_typekind, " SYSTEM
        kind        TYPE rsscr_kind,    " P S T
        ui_type     TYPE string,        " Only for KIND = P
        length      TYPE i,             " Only for KIND = P
        decimals    TYPE i,             " Only for KIND = P
        " For editing in ALV
        rollname    TYPE ZDAQO_TABLE_AND_FIELD,
        text        TYPE DESCR_40,
        " Table description
        table_kind  TYPE abap_tablekind,
        unique      TYPE abap_bool,
        key         TYPE abap_keydescr_tab,
        key_defkind TYPE abap_keydefkind,
        subcomps    TYPE string,
    END OF ts_comp .
  types:
    tt_comp TYPE STANDARD TABLE OF ts_comp WITH DEFAULT KEY .
  types:
    BEGIN OF ts_field_opt,
        value       TYPE string,
        is_old      TYPE XSDBOOLEAN,
        edit        TYPE ZDAQO_EDITABLE.
        INCLUDE TYPE ts_comp AS comp.
    TYPES:
    END OF ts_field_opt .
  types:
    tt_field_opt TYPE STANDARD TABLE OF ts_field_opt WITH DEFAULT KEY .
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
  types:
    tt_unique TYPE SORTED TABLE OF STRING WITH UNIQUE KEY TABLE_LINE .

  constants MC_UTF8 type ABAP_ENCODING value '4110' ##NO_TEXT.
  constants MC_KIND_PARAMETER type RSSCR_KIND value 'P' ##NO_TEXT.
  constants MC_KIND_SELECT_OPTION type RSSCR_KIND value 'S' ##NO_TEXT.
  constants MC_KIND_TABLE type RSSCR_KIND value 'T' ##NO_TEXT.
  constants MC_KIND_MEMO type RSSCR_KIND value 'M' ##NO_TEXT.

  class-methods CREATE_TYPE_DESCR
    importing
      !IV_ROLLNAME type CSEQUENCE optional
      !IS_COMP type TS_COMP optional
      value(IR_TYPE) type ref to DATA optional
    returning
      value(RO_TYPE) type ref to CL_ABAP_DATADESCR
    raising
      ZCX_AQO_ERROR .
  class-methods CREATE_STRUCTURE
    importing
      !IO_RANGE type ref to CL_ABAP_DATADESCR optional
      !IV_COMPS type STRING optional
      !IT_FIELD_OPT type TT_FIELD_OPT optional
    returning
      value(RO_STRUCT) type ref to CL_ABAP_STRUCTDESCR .
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
      !CT_UNIQUE type TT_UNIQUE .
  class-methods TO_JSON
    importing
      !IM_DATA type ANY
      !IV_PURE type ABAP_BOOL default ABAP_FALSE
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
  class-methods EDIT_TRANSACTION
    importing
      !IV_OBJECT type CSEQUENCE optional
      !IV_SUBOBJECT type CSEQUENCE optional
      !IV_NEW_UI type ABAP_BOOL optional
    exporting
      !EV_OBJECT type CSEQUENCE
      !EV_SUBOBJECT type CSEQUENCE
      !EV_NEW_UI type ABAP_BOOL .
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
  class-methods DOWNLOAD
    importing
      !IV_CONTENT type STRING
      !IV_FILENAME type STRING .
protected section.
private section.

  class-data MT_XSDBOOLEAN type ref to STRINGTAB .

  class-methods GET_POSITION
    importing
      !IV_INCLUDE type CSEQUENCE
      !IV_OBJECT type ZTAQO_DATA-OBJECT
      !IV_SUBOBJECT type ZTAQO_DATA-SUBOBJECT
    exporting
      !EV_LINE type I
      !EV_FOUND type ABAP_BOOL .
  class-methods ABAP_2_JSON
    importing
      !IM_DATA type DATA
      !IV_NAME type STRING optional
    returning
      value(RV_JSON) type STRING .
ENDCLASS.



CLASS ZCL_AQO_UTIL IMPLEMENTATION.


METHOD abap_2_json.
  CONSTANTS:
    c_comma TYPE c VALUE ',',
    c_colon TYPE c VALUE ':',
    c_quote TYPE c VALUE '"'.

  DATA:
    dont_quote     TYPE xfeld,
    json_fragments TYPE TABLE OF string,
    rec_rv_json    TYPE string,
    "s_type          TYPE c,
    "l_type          TYPE c,
    l_comps        TYPE i,
    l_lines        TYPE i,
    l_index        TYPE i,
    l_value        TYPE string,
    l_name         TYPE string,
    l_strudescr    TYPE REF TO cl_abap_structdescr,
    lo_type        TYPE REF TO cl_abap_typedescr,
    lo_subtype     TYPE REF TO cl_abap_typedescr,
    lv_type        TYPE REF TO string.

  FIELD-SYMBOLS:
    <abap_data>     TYPE any,
    <itab>          TYPE ANY TABLE,
    <stru>          TYPE ANY TABLE,
    <comp>          TYPE any,
    <abapcomp>      TYPE abap_compdescr,
    <lt_xsdboolean> TYPE stringtab.

  " Lazy initialization
  DO 1 TIMES.
    " Init one time only
    CHECK mt_xsdboolean IS INITIAL.
    CREATE DATA mt_xsdboolean.

    ASSIGN mt_xsdboolean->* TO <lt_xsdboolean>.

    " Get all boolean types
    SELECT rollname INTO TABLE <lt_xsdboolean>
    FROM dd04l
    WHERE domname = 'XSDBOOLEAN' AND as4local = 'A'.

    " Add text for speed
    LOOP AT <lt_xsdboolean> REFERENCE INTO lv_type.
      CONCATENATE '\TYPE=' lv_type->* INTO lv_type->*.
    ENDLOOP.

    " Is standard table
    SORT <lt_xsdboolean> BY table_line.
  ENDDO.
  ASSIGN mt_xsdboolean->* TO <lt_xsdboolean>.


  DEFINE get_scalar_value.
    " &1 : assigned var
    " &2 : abap data
    " &3 : abap type
    &1 = &2.
****************************************************
* Adapt some basic ABAP types (pending inclusion of all basic abap types?)
* Feel free to customize this for your needs
    CASE &3->type_kind.
*       1. ABAP numeric types
      WHEN 'I'. " Integer
        CONDENSE &1.
        IF sign( &1 ) < 0.
          SHIFT &1 BY 1 PLACES RIGHT CIRCULAR.
        ENDIF.
        dont_quote = 'X'.

      WHEN 'F'. " Float
        CONDENSE &1.
        dont_quote = 'X'.

      WHEN 'P'. " Packed number (used in quantities or currency, for example)
        CONDENSE &1.
        IF sign( &1 ) < 0.
          SHIFT &1 BY 1 PLACES RIGHT CIRCULAR.
        ENDIF.
        dont_quote = 'X'.

      WHEN 'X'. " Hexadecimal
        CONDENSE &1.
        CONCATENATE '0x' &1 INTO &1.
*        dont_quote = 'X'.
*        "Quote it, as JSON doesn't support Hex or Octal as native types.

*       2. ABAP char types
      WHEN 'D'. " Date type
        CONCATENATE &1(4) '-' &1+4(2) '-' &1+6(2) INTO &1.

      WHEN 'T'. " Time representation
        CONCATENATE &1(2) ':' &1+2(2) ':' &1+4(2) INTO &1.

      WHEN 'N'. " Numeric text field
*           condense &1.

      WHEN 'C' OR 'g'. " Char sequences and Strings
        READ TABLE <lt_xsdboolean> TRANSPORTING NO FIELDS BINARY SEARCH
         WITH KEY table_line = &3->absolute_name.
        IF sy-subrc = 0.
          dont_quote = 'X'.
          IF &2 = abap_true.
            &1 = 'true'.
          ELSE.
            &1 = 'false'.
          ENDIF.
        ELSE.
          " Put safe chars
          REPLACE ALL OCCURRENCES OF '\' IN &1 WITH '\\' .
          REPLACE ALL OCCURRENCES OF '"' IN &1 WITH '\"' .
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN &1 WITH '\r\n' .
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN &1 WITH '\n' .
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN &1 WITH '\t' .
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace IN &1 WITH '\b' .
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN &1 WITH '\f' .
        ENDIF.

      WHEN 'y'.  " XSTRING
* Put the XSTRING in Base64
        &1 = cl_http_utility=>encode_x_base64( &2 ).

      WHEN OTHERS.
* Don't hesitate to add and modify scalar abap types to suit your taste.

    ENDCASE.
** End of scalar data preparing.

* Enclose value in quotes (or not)
    IF dont_quote NE 'X'.
      CONCATENATE c_quote &1 c_quote INTO &1.
    ENDIF.

    CLEAR dont_quote.

  END-OF-DEFINITION.


***************************************************
*  Prepare field names, JSON does quote names!!   *
*  You must be strict in what you produce.        *
***************************************************
  IF iv_name IS NOT INITIAL.
    CONCATENATE c_quote iv_name c_quote c_colon INTO rec_rv_json.
    APPEND rec_rv_json TO json_fragments.
    CLEAR rec_rv_json.
  ENDIF.

**
* Get ABAP data type
  lo_type = cl_abap_typedescr=>describe_by_data( im_data ).
  " DESCRIBE FIELD im_data TYPE l_type COMPONENTS l_comps.

***************************************************
*  Get rid of data references
***************************************************
  IF lo_type->type_kind EQ cl_abap_typedescr=>typekind_dref.
    ASSIGN im_data->* TO <abap_data>.
    lo_type = cl_abap_typedescr=>describe_by_data( <abap_data> ).

    IF sy-subrc NE 0.
      APPEND '{}' TO json_fragments.
      CONCATENATE LINES OF json_fragments INTO rv_json.
      EXIT.
    ENDIF.
  ELSE.
    ASSIGN im_data TO <abap_data>.
  ENDIF.

* Get ABAP data type again and start
  " DESCRIBE FIELD <abap_data> TYPE l_type COMPONENTS l_comps.

  " A little fastaer than description of description
  TRY.
      l_strudescr ?= lo_type.
    CATCH cx_sy_move_cast_error.
      CLEAR l_strudescr.
  ENDTRY.

***************************************************
*  Tables
***************************************************
  IF lo_type->type_kind EQ cl_abap_typedescr=>typekind_table.
* '[' JSON table opening bracket
    APPEND '[' TO json_fragments.
    ASSIGN <abap_data> TO <itab>.
    l_lines = lines( <itab> ).
    LOOP AT <itab> ASSIGNING <comp>.
      ADD 1 TO l_index.
*> Recursive call for each table row:
      rec_rv_json = abap_2_json( im_data = <comp> ).
      APPEND rec_rv_json TO json_fragments.
      CLEAR rec_rv_json.
      IF l_index < l_lines.
        APPEND c_comma TO json_fragments.
      ENDIF.
    ENDLOOP.
    APPEND ']' TO json_fragments.
* ']' JSON table closing bracket


***************************************************
*  Structures
***************************************************
  ELSE.
    IF l_strudescr IS NOT INITIAL. " lo_class->absolute_name = '\CLASS=CL_ABAP_STRUCTDESCR'. " l_comps IS NOT INITIAL.
* '{' JSON object opening curly brace
      APPEND '{' TO json_fragments.
      " l_strudescr ?=  lo_type. " cl_abap_typedescr=>describe_by_data( <abap_data> ).
      l_comps = lines( l_strudescr->components ).
      LOOP AT l_strudescr->components ASSIGNING <abapcomp>.
        l_index = sy-tabix .
        ASSIGN COMPONENT <abapcomp>-name OF STRUCTURE <abap_data> TO <comp>.
        l_name = <abapcomp>-name.
** ABAP names are usually in caps, set upcase to avoid the conversion to lower case.
        " DESCRIBE FIELD <comp> TYPE s_type.
        lo_subtype = cl_abap_typedescr=>describe_by_data( <comp> ).
        IF lo_subtype->type_kind EQ cl_abap_typedescr=>typekind_table   OR lo_subtype->type_kind EQ cl_abap_typedescr=>typekind_dref OR
           lo_subtype->type_kind EQ cl_abap_typedescr=>typekind_struct1 OR lo_subtype->type_kind EQ cl_abap_typedescr=>typekind_struct2.
*> Recursive call for non-scalars:
          rec_rv_json = abap_2_json( im_data = <comp> iv_name = l_name ).
        ELSE.
          IF lo_subtype->type_kind EQ cl_abap_typedescr=>typekind_oref OR lo_subtype->type_kind EQ cl_abap_typedescr=>typekind_iref.
            rec_rv_json = '"REF UNSUPPORTED"'.
          ELSE.
            get_scalar_value rec_rv_json <comp> lo_subtype.
          ENDIF.
          CONCATENATE c_quote l_name c_quote c_colon rec_rv_json INTO rec_rv_json.
        ENDIF.
        APPEND rec_rv_json TO json_fragments.
        CLEAR rec_rv_json. CLEAR l_name.
        IF l_index < l_comps.
          APPEND c_comma TO json_fragments.
        ENDIF.
      ENDLOOP.
      APPEND '}' TO json_fragments.
* '}' JSON object closing curly brace

****************************************************
*                  - Scalars -                     *
****************************************************
    ELSE.
      get_scalar_value l_value <abap_data> lo_type.
      APPEND l_value TO json_fragments.

    ENDIF.
* End of structure/scalar IF block.
***********************************

  ENDIF.
* End of main IF block.
**********************

* Use a loop in older releases that don't support concatenate lines.
  CONCATENATE LINES OF json_fragments INTO rv_json.
ENDMETHOD.


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
        <ls_fieldcat>-col_pos = sy-tabix.
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
    lt_comp    TYPE abap_component_tab,
    lt_subcomp TYPE STANDARD TABLE OF ts_comp,
    lr_type    TYPE REF TO data,
    lv_ok      TYPE abap_bool.
  FIELD-SYMBOLS:
    <ls_field_opt> TYPE ts_field_opt,
    <ls_subfield>  TYPE ts_comp,
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
      <ls_comp>-type = create_type_descr( is_comp = <ls_subfield> ).
    ENDLOOP.
  ENDDO.

  " №4 Called from constructor if have in DB cluster
  LOOP AT it_field_opt ASSIGNING <ls_field_opt>.
    " Create sub levels
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = <ls_field_opt>-name.
    <ls_comp>-type = create_type_descr( is_comp = <ls_field_opt>-comp ).
  ENDLOOP.

  ro_struct = cl_abap_structdescr=>create( lt_comp ).
ENDMETHOD.


METHOD create_type_descr.
  DATA:
    lo_line  TYPE REF TO cl_abap_datadescr,
    lv_kind  TYPE rsscr_kind,
    lv_msgv1 TYPE symsgv.

  " No type
  CLEAR ro_type.

  " №0
  DO 1 TIMES.
    CHECK is_comp IS SUPPLIED.
    lv_kind = is_comp-kind.

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
    IF is_comp-rollname IS NOT INITIAL.
      ro_type = create_type_descr( iv_rollname = is_comp-rollname ).
    ENDIF.

    IF ro_type IS INITIAL AND lv_kind = zcl_aqo_util=>mc_kind_parameter.
      CASE is_comp-sys_type.
        WHEN cl_abap_typedescr=>typekind_char.
          ro_type = cl_abap_elemdescr=>get_c( p_length = is_comp-length ).
        WHEN cl_abap_typedescr=>typekind_date.
          ro_type = cl_abap_elemdescr=>get_d( ).
        WHEN cl_abap_typedescr=>typekind_int.
          ro_type = cl_abap_elemdescr=>get_i( ).
        WHEN cl_abap_typedescr=>typekind_float.
          ro_type = cl_abap_elemdescr=>get_f( ).
        WHEN cl_abap_typedescr=>typekind_num.
          ro_type = cl_abap_elemdescr=>get_n( p_length = is_comp-length ).
        WHEN cl_abap_typedescr=>typekind_packed.
          ro_type = cl_abap_elemdescr=>get_p( p_length = is_comp-length p_decimals = is_comp-decimals ).
        WHEN cl_abap_typedescr=>typekind_string.
          ro_type = cl_abap_elemdescr=>get_string( ).
        WHEN cl_abap_typedescr=>typekind_time.
          ro_type = cl_abap_elemdescr=>get_t( ).
        WHEN cl_abap_typedescr=>typekind_table.
          "create_structure( iv_comps = <ls_subfield>-subcomps )
          lv_kind = zcl_aqo_util=>mc_kind_table.

        WHEN OTHERS.
          lv_msgv1 = is_comp-name.
          RAISE EXCEPTION TYPE zcx_aqo_error
            EXPORTING
              textid = zcx_aqo_error=>unknown_type
              msgv1  = lv_msgv1.
      ENDCASE.
    ENDIF.

    CASE lv_kind.
        " P
      WHEN zcl_aqo_util=>mc_kind_parameter.

        " S
      WHEN zcl_aqo_util=>mc_kind_select_option.
        " Call №2 recursion
        lo_line = create_structure( io_range = ro_type ).
        ro_type = cl_abap_tabledescr=>create( p_line_type = lo_line ).

        " T
      WHEN zcl_aqo_util=>mc_kind_table.
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
    CATCH cx_root.
      CLEAR ro_type.
  ENDTRY.
ENDMETHOD.


METHOD download.
  DATA:
    lv_xstring TYPE xstring,
    lv_size    TYPE int4,
    lt_xdata   TYPE w3mimetabtype.

  lv_xstring = string_to_xstring( iv_content ).

  " Convert to table
  xstring_to_binary(
   EXPORTING
     iv_xstring = lv_xstring
   IMPORTING
     ev_length  = lv_size
     et_table   = lt_xdata ).

  " For small files
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename     = iv_filename
      filetype     = 'BIN'
      bin_filesize = lv_size
    TABLES
      data_tab     = lt_xdata
    EXCEPTIONS
      OTHERS       = 1.
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


METHOD edit_transaction.

  " Just return parameters
  IF ev_object IS REQUESTED.
    GET PARAMETER ID:
     'ZAQO_OBJECT'    FIELD ev_object,
     'ZAQO_SUBOBJECT' FIELD ev_subobject,
     'ZAQO_NEW_UI'    FIELD ev_new_ui.

    RETURN.
  ENDIF.

  " Set transaction
  SET PARAMETER ID:
   'ZAQO_OBJECT'    FIELD iv_object,
   'ZAQO_SUBOBJECT' FIELD iv_subobject,
   'ZAQO_NEW_UI'    FIELD iv_new_ui.

  " CALL TRANSACTION '' AND SKIP FIRST SCREEN.
  IF iv_new_ui = abap_true.
    SUBMIT zaqo_edit VIA SELECTION-SCREEN. " AND RETURN.
  ELSE.
    SUBMIT zaqo_edit_old VIA SELECTION-SCREEN. " AND RETURN.
  ENDIF.
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

  SELECT d~tabname d~fieldname d~shlporigin INTO CORRESPONDING FIELDS OF TABLE lt_dd03l
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


METHOD from_json.
  DATA:
    lo_error TYPE REF TO cx_transformation_error.

  " No need
  IF iv_json IS INITIAL.
    ev_ok = abap_false. " Always have {"DATA":}
    RETURN.
  ENDIF.

  " Work with JSON even in ABAP 7.00!
  " Regardless the fact that it do not have if_sxml=>co_xt_json
  TRY.
      CALL TRANSFORMATION id SOURCE XML iv_json
                             RESULT data = ex_data.
      ev_ok = abap_true.
    CATCH cx_transformation_error INTO lo_error.
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
********************************************
  " Preferable way
********************************************
  DATA:
    lo_writer TYPE REF TO cl_sxml_string_writer,
    lv_xtring TYPE xstring,
    lv_end    TYPE i,
    lv_beg    TYPE i.

  " If have error delete comments in 'Alternative way'
  " And comment the whole 'Preferable way'
  lo_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

  CALL TRANSFORMATION id SOURCE data = im_data
                         RESULT XML lo_writer.

  lv_xtring = lo_writer->get_output( ).

  " downgrad ?
  rv_json = zcl_aqo_util=>xstring_to_string( lv_xtring ).

********************************************
  " Alternative way
********************************************
**  rv_json = abap_2_json( im_data = im_data iv_name = 'DATA' ).
**  CONCATENATE `{` rv_json `}` INTO rv_json.

  " delete surroundin DATA
  IF iv_pure = abap_true.
    lv_end = strlen( rv_json ).

    IF rv_json(9) CP `{"DATA":"`.
      lv_beg = 9.
      lv_end = lv_end - 11.
    ELSE.
      lv_beg = 8.
      lv_end = lv_end - 9.
    ENDIF.

    rv_json = rv_json+lv_beg(lv_end).
  ENDIF.
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
