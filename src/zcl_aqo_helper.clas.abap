class ZCL_AQO_HELPER definition
  public
  final
  create private .

public section.
  type-pools ABAP .

  types:
    abap_attrname_tab TYPE HASHED TABLE OF abap_attrname WITH UNIQUE KEY table_line .
  types:
    BEGIN OF ts_field_desc,
        name        TYPE zdaqo_attr_name, " abap_attrname, " 61 char
        sys_type    TYPE abap_typekind, " SYSTEM
        " kind        TYPE rsscr_kind, TODO delete everywhere   " P S T
        ui_type     TYPE zdaqo_ui_type, " Only for KIND = P
        length      TYPE i,             " Only for KIND = P
        decimals    TYPE i,             " Only for KIND = P
        " For editing in ALV
        rollname    TYPE zdaqo_db_field,
        label       TYPE dfies-fieldtext,
        " Table description
        table_kind  TYPE abap_tablekind,
        unique      TYPE abap_bool,
        key         TYPE abap_keydescr_tab,
        key_defkind TYPE abap_keydefkind,
        sub_fdesc   TYPE string,
      END OF ts_field_desc .
  types:
    tt_field_desc TYPE HASHED TABLE OF ts_field_desc WITH UNIQUE KEY name .
  types:
    BEGIN OF ts_history_value,
        changed TYPE sydatum,
        login   TYPE syuname,
        h_value TYPE string,
      END OF ts_history_value .
  types:
    tt_history_value TYPE SORTED TABLE OF ts_history_value WITH UNIQUE KEY changed .
  types:
    BEGIN OF ts_field_value,
        value       TYPE tt_history_value,
        is_editable TYPE zdaqo_edit_in_prod.
        INCLUDE TYPE ts_field_desc AS field_desc.
    TYPES:
    END OF ts_field_value .
  types:
    tt_field_value TYPE HASHED TABLE OF ts_field_value WITH UNIQUE KEY name .
  types:
    BEGIN OF ts_usage,
        index      TYPE syindex,
        package_id TYPE devclass,
        option_id  TYPE zdaqo_option_id,
        include    TYPE wbcrossgt-include,
        line       TYPE i,
        meth       TYPE seocpdname,
        uname      TYPE syuname, "wbcrossgt-uname,
        udate      TYPE sydatum, "wbcrossgt-udate,
        uzeit      TYPE syuzeit, "wbcrossgt-uzeit,
        found      TYPE xsdboolean,
      END OF ts_usage .
  types:
    tt_usage TYPE STANDARD TABLE OF ts_usage WITH DEFAULT KEY .
  types:
    tt_unique_type TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line .
  types:
    tt_e071 TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY .
  types TS_OAOR_FILE type ZSAQO_OAOR_FILE .
  types:
    tt_oaor_file TYPE STANDARD TABLE OF ts_oaor_file WITH DEFAULT KEY .

  constants MC_UTF8 type ABAP_ENCODING value '4110' ##NO_TEXT.
  constants MC_UI_CHAR type STRING value 'char' ##NO_TEXT.
  constants MC_UI_NUMC type STRING value 'numc' ##NO_TEXT.
  constants MC_UI_STRING type STRING value 'string' ##NO_TEXT.
  constants MC_UI_NUMERIC type STRING value 'numeric' ##NO_TEXT.
  constants MC_UI_BOOLEAN type STRING value 'boolean' ##NO_TEXT.
  constants MC_UI_DATE type STRING value 'date' ##NO_TEXT.
  constants MC_UI_TIME type STRING value 'time' ##NO_TEXT.
  constants MC_UI_DATETIME type STRING value 'datetime' ##NO_TEXT.
  constants MC_UI_RANGE type STRING value 'range' ##NO_TEXT.
  constants MC_UI_TABLE type STRING value 'table' ##NO_TEXT.
  constants MC_OAOR_OTHER type BAPIBDS01-CLASSTYPE value 'OT' ##NO_TEXT.

  class-methods GET_FIELD_DESC
    importing
      !IV_FIELD_NAME type CSEQUENCE optional
      !IV_DATA type ANY optional
      !IS_SH_FIELD type DFIES optional
      !IR_UNIQUE_TYPE type ref to TT_UNIQUE_TYPE optional
    returning
      value(RS_FIELD_DESC) type TS_FIELD_DESC
    raising
      ZCX_AQO_EXCEPTION .
  class-methods CREATE_TYPE_DESCR
    importing
      !IV_ROLLNAME type CSEQUENCE optional
      !IS_FIELD_DESC type TS_FIELD_DESC optional
      value(IR_TYPE) type ref to DATA optional
    returning
      value(RO_TYPE) type ref to CL_ABAP_DATADESCR
    raising
      ZCX_AQO_EXCEPTION .
  class-methods CREATE_STRUCTURE
    importing
      !IO_RANGE type ref to CL_ABAP_DATADESCR optional
      !IV_SUB_FDESC type STRING optional
      !IT_FIELD_DESC type TT_FIELD_DESC optional
    returning
      value(RO_STRUCT) type ref to CL_ABAP_STRUCTDESCR
    raising
      ZCX_AQO_EXCEPTION .
  class-methods CREATE_FIELD_CATALOG
    importing
      value(IR_STRUC) type ref to DATA optional
      !IO_STRUC type ref to CL_ABAP_STRUCTDESCR optional
      !IV_SORT type ABAP_BOOL optional
    exporting
      !ET_FIELDCAT type LVC_T_FCAT
    changing
      !CT_TABLE type STANDARD TABLE optional .
  class-methods COMPARE_2_FIELDS
    importing
      !IS_NEW type TS_FIELD_DESC
      !IV_REPAIR type ABAP_BOOL
      !CS_OLD type ref to TS_FIELD_DESC
    changing
      !CV_CHANGED type ABAP_BOOL
    raising
      ZCX_AQO_EXCEPTION .
  class-methods FIND_TABLE_FIELDNAME
    importing
      !IV_NAME type CSEQUENCE
      !IR_UNIQUE_TYPE type ref to TT_UNIQUE_TYPE
    changing
      !CV_ROLLNAME type CSEQUENCE
      !CV_LABEL type CSEQUENCE optional .
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
  class-methods IS_IN_EDITOR
    importing
      value(IV_ANY) type ABAP_BOOL default ABAP_TRUE
      !IV_IS_VIEWER type ABAP_BOOL optional
      !IV_IS_SAPUI5 type ABAP_BOOL optional
      !IV_TCODE type SYTCODE optional
    returning
      value(RV_OK) type ABAP_BOOL .
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
      !IV_POSITION type I
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods GET_USAGE
    returning
      value(RT_USAGE) type TT_USAGE .
  class-methods LOAD_FROM_SMW0
    importing
      !IV_RELID type CSEQUENCE
      !IV_OBJID type CSEQUENCE
    exporting
      !ET_TABLE type W3MIMETABTYPE
      !EV_SIZE type I .
  class-methods EDIT_IN_POPUP
    importing
      !IV_TYPE type CSEQUENCE
      !IV_TITLE type CSEQUENCE
    changing
      !CV_OK type ABAP_BOOL optional
      !CV_VALUE type ANY .
  class-methods BINARY_TO_STRING
    importing
      !IT_TABLE type STANDARD TABLE
      !IV_LENGTH type I
      !IV_ENCODING type ABAP_ENCODING default MC_UTF8
    returning
      value(RV_STRING) type STRING .
  class-methods BINARY_TO_XSTRING
    importing
      !IT_TABLE type STANDARD TABLE
      !IV_LENGTH type I
    returning
      value(RV_XSTRING) type XSTRING .
  class-methods STRING_TO_XSTRING
    importing
      !IV_STRING type STRING
      !IV_ENCODING type ABAP_ENCODING default MC_UTF8
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
      !IV_ENCODING type ABAP_ENCODING default MC_UTF8
    returning
      value(RV_STRING) type STRING .
  class-methods GET_LAST_CALL_INFO
    importing
      !IS_LAST_CALL type ABAP_CALLSTACK_LINE
    exporting
      !EV_NAME type CSEQUENCE
      !EV_IS_CLASS type ABAP_BOOL .
  class-methods CONFIRM
    importing
      !IV_TITLE type CSEQUENCE
      !IV_QUESTION type CSEQUENCE
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods DOWNLOAD
    importing
      !IV_CONTENT type STRING optional
      !IV_XCONTENT type XSTRING optional
      !IV_ENCODING type ABAP_ENCODING default MC_UTF8
      !IV_FILENAME type STRING .
  class-methods SH_SORT_ORDER
    importing
      !IV_VALUE type CSEQUENCE optional
    exporting
      !EV_VALUE type CSEQUENCE .
  class-methods CONVERT_TO_TIMESTAMP
    importing
      !IV_DATE type CLIKE default '00000000'
      !IV_TIME type CLIKE default '000000'
      !IV_MSEC type NUM03 default 000
    returning
      value(RV_TIMESTAMP) type STRING .
  class-methods MESSAGE_WITH_FIELDS
    importing
      !IT_FIELD type ABAP_ATTRNAME_TAB
      !IV_NUMBER type SYMSGNO
    returning
      value(RV_INFO) type STRING .
  class-methods SPLIT_FILE_PATH
    importing
      !IV_FULLPATH type CSEQUENCE
    exporting
      !EV_PATH type CSEQUENCE
      !EV_FILENAME type CSEQUENCE
      !EV_FILENAME_NOEXT type CSEQUENCE
      !EV_EXTENSION type CSEQUENCE .
  class-methods CHECK_IN_REQUEST
    importing
      !IV_TABLE_NAME type CSEQUENCE optional
      !IV_KEY1 type CLIKE optional
      !IV_KEY2 type CLIKE optional
      !IV_KEY3 type CLIKE optional
      !IS_OAOR_FILE type TS_OAOR_FILE optional
    changing
      !CV_TASK type E070-TRKORR optional
    raising
      ZCX_AQO_EXCEPTION .
  class-methods OAOR_CHECK_EXISTS
    importing
      !IV_PACK_ID type ZTAQO_OPTION-PACKAGE_ID
      !IV_OPTION_ID type ZTAQO_OPTION-OPTION_ID
    exporting
      !EV_TASK type E070-TRKORR .
  class-methods OAOR_GET_FILES
    importing
      !IV_PACK_ID type ZTAQO_OPTION-PACKAGE_ID
      !IV_OPTION_ID type ZTAQO_OPTION-OPTION_ID
      !IV_FILENAME type CSEQUENCE optional
    exporting
      !ES_OAOR_LAST type TS_OAOR_FILE
      !ET_OAOR_FILE type TT_OAOR_FILE .
  class-methods OAOR_DELETE_FILE
    importing
      !IV_PACK_ID type ZTAQO_OPTION-PACKAGE_ID
      !IV_OPTION_ID type ZTAQO_OPTION-OPTION_ID
      !IS_OAOR_FILE type TS_OAOR_FILE
    changing
      !CV_TASK type E070-TRKORR .
protected section.
private section.

  class-data MT_XSDBOOLEAN type ref to STRINGTAB .
  class-data MV_CODE type SYTCODE .

  class-methods GET_POSITION
    importing
      !IV_INCLUDE type CSEQUENCE
      !IV_PACKAGE type ZTAQO_OPTION-PACKAGE_ID
      !IV_OPTION type ZTAQO_OPTION-OPTION_ID
    exporting
      !EV_LINE type I
      !EV_FOUND type ABAP_BOOL .
  class-methods ABAP_2_JSON
    importing
      !IM_DATA type DATA
      !IV_NAME type STRING optional
    returning
      value(RV_JSON) type STRING .
  class-methods JSON_2_ABAP
    importing
      !JSON_STRING type STRING optional
      !VAR_NAME type STRING optional
      !PROPERTY_PATH type STRING default 'json_obj'
    exporting
      value(PROPERTY_TABLE) type JS_PROPERTY_TAB
    changing
      !JS_OBJECT type ref to CL_JAVA_SCRIPT optional
      value(ABAP_DATA) type ANY optional
    raising
      ZCX_AQO_EXCEPTION .
ENDCLASS.



CLASS ZCL_AQO_HELPER IMPLEMENTATION.


METHOD ABAP_2_JSON.
  CONSTANTS:
    c_comma TYPE c VALUE ',',
    c_colon TYPE c VALUE ':',
    c_quote TYPE c VALUE '"'.

  DATA:
    dont_quote     TYPE xfeld,
    json_fragments TYPE TABLE OF string,
    rec_rv_json    TYPE string,
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
        " &1 = convert_to_timestamp( iv_date = &1 ). 'T00:00:00'

      WHEN 'T'. " Time representation
        CONCATENATE &1(2) ':' &1+2(2) ':' &1+4(2) INTO &1.
        " '0000-00-00T'  &1 = convert_to_timestamp( iv_time = &1 ).

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
        ELSEIF &3->absolute_name = '\TYPE=XSDDATETIME_LOCAL'." others ?

          IF strlen( &1 ) = 14.
            CONCATENATE &1(4) '-' &1+4(2) '-' &1+6(2)
               'T' &1+8(2) ':' &1+10(2) ':' &1+12(2) INTO &1.
*            lv_date = &1(8).
*            lv_time = &1+8.
*            &1 = convert_to_timestamp( iv_date = lv_date iv_time = lv_time ).
          ELSE.
            &1 = ''.
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
      encoding     = iv_encoding
    IMPORTING
      text_buffer  = rv_string
    TABLES
      binary_tab   = it_table.
ENDMETHOD.


METHOD BINARY_TO_XSTRING.
  " cl_bcs_convert=>solix_to_xstring( )
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = iv_length
    IMPORTING
      buffer       = rv_xstring
    TABLES
      binary_tab   = it_table.
ENDMETHOD.


METHOD check_in_request.
  DATA:
    lt_e071            TYPE STANDARD TABLE OF e071  WITH DEFAULT KEY,
    lt_e071k           TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY,
    ls_e071            TYPE e071,
    lr_e071            TYPE REF TO e071,
    ls_e071k           TYPE e071k,
    lv_index           TYPE char1,
    lv_name            TYPE string,
    lv_len             TYPE i,
    lv_off             TYPE i,
    lv_where           TYPE string.
  FIELD-SYMBOLS:
    <lv_key> TYPE clike.

  " Always the same
  ls_e071-pgmid = ls_e071k-pgmid = 'R3TR'.

  " Current user + Not released (trstatus<>R) + strkorr <> space Task or sub-request has parent item
  " ? e070~trfunction = 'S'
  lv_where = `e070~as4user = sy-uname AND ( e070~trstatus = 'D' OR e070~trstatus = 'L' ) AND e070~strkorr <> space`.

**********************************************************************
  " Table row
**********************************************************************
  DO 1 TIMES.
    CHECK iv_table_name IS SUPPLIED.

    " Create key for table
    CLEAR lv_off.
    DO 3 TIMES.
      " Create name
      lv_index = sy-index.
      CONCATENATE 'IV_KEY' lv_index INTO lv_name.

      " Is supplied
      ASSIGN (lv_name) TO <lv_key>.
      CHECK sy-subrc = 0 AND <lv_key> IS NOT INITIAL.

      " Create key
      DESCRIBE FIELD <lv_key> LENGTH lv_len IN CHARACTER MODE.
      ls_e071k-tabkey+lv_off(lv_len)  = <lv_key>.

      " Move to next
      ADD lv_len TO lv_off.
    ENDDO.

    " Struc 1
    ls_e071-object      = 'TABU'.
    ls_e071-obj_name    = iv_table_name.
    ls_e071-objfunc     = 'K'.

    " Struc 2
    ls_e071k-object     = ls_e071k-mastertype = 'TABU'.
    ls_e071k-objname    = ls_e071k-mastername = iv_table_name.

    APPEND:
     ls_e071  TO lt_e071,
     ls_e071k TO lt_e071k.

    " Check in DB
    CHECK cv_task IS INITIAL.

    " Try to find
    SELECT SINGLE e070~trkorr INTO cv_task " e070~strkorr cv_request
    FROM e070 INNER JOIN e071k ON e071k~trkorr     = e070~trkorr
                              AND e071k~pgmid      = ls_e071k-pgmid
                              AND e071k~object     = ls_e071k-object
                              AND e071k~objname    = ls_e071k-objname
                              AND e071k~mastertype = ls_e071k-mastertype
                              AND e071k~mastername = ls_e071k-mastername
                              AND e071k~tabkey     = ls_e071k-tabkey
    WHERE (lv_where).
  ENDDO.

**********************************************************************
  " OAOR file
**********************************************************************
  DO 1 TIMES.
    CHECK is_oaor_file IS SUPPLIED.

    " Logical information object for BDS
    ls_e071-object      = 'SBDL'.
    ls_e071-obj_name    = is_oaor_file-doc_id+10(32).
    APPEND ls_e071 TO lt_e071.

    " Physical information object
    ls_e071-object      = 'SBDP'.
    ls_e071-obj_name    = is_oaor_file-objid.
    APPEND ls_e071 TO lt_e071.

    " Check in DB
    CHECK cv_task IS INITIAL.

    " Try to find
    LOOP AT lt_e071 REFERENCE INTO lr_e071.
      SELECT SINGLE e070~trkorr INTO cv_task
      FROM e070 INNER JOIN e071 ON e071~trkorr = e070~trkorr
      WHERE e071~pgmid    = lr_e071->pgmid
        AND e071~object   = lr_e071->object
        AND e071~obj_name = lr_e071->obj_name
        AND (lv_where).

      CHECK cv_task IS NOT INITIAL.
      EXIT.
    ENDLOOP.
  ENDDO.

  " Show dialog if task is empty and in old editor
  IF cv_task IS INITIAL AND zcl_aqo_helper=>is_in_editor( iv_is_sapui5 = abap_true ) <> abap_true.
    " select request/task
    CALL FUNCTION 'TR_ORDER_CHOICE_CORRECTION'
      EXPORTING
        iv_category = 'SYST'                                "#EC NOTEXT
      IMPORTING
        " ev_order    = cv_request
        ev_task     = cv_task " Can be empty
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc <> 0.
      zcx_aqo_exception=>raise_sys_error( ).
    ENDIF.
  ENDIF.

  " include data to request
  CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
    EXPORTING
      wi_trkorr = cv_task
    TABLES
      wt_e071   = lt_e071
      wt_e071k  = lt_e071k
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc <> 0.
    zcx_aqo_exception=>raise_sys_error( ).
  ENDIF.

  " Ok 1
  IF iv_table_name IS SUPPLIED.
    MESSAGE s023(zaqo_message) WITH iv_key1 iv_key2 iv_key3 cv_task.
    RETURN.
  ENDIF.

  " Ok 2
  IF is_oaor_file IS SUPPLIED.
    MESSAGE s039(zaqo_message) WITH is_oaor_file-file_name is_oaor_file-doc_ver_no cv_task.
    RETURN.
  ENDIF.
ENDMETHOD.


METHOD compare_2_fields.
  DATA:
    lt_old      TYPE tt_field_desc,
    lv_old_ok   TYPE abap_bool,
    lt_new      TYPE tt_field_desc,
    lv_new_ok   TYPE abap_bool,
    lv_changed  LIKE cv_changed,
    ls_old      TYPE REF TO ts_field_desc,
    ls_new      TYPE REF TO ts_field_desc,
    lv_subfield TYPE abap_attrname.

  DEFINE raise_error.
    " Add sub field
    IF lv_subfield IS NOT INITIAL.
     CONCATENATE '-' lv_subfield INTO lv_subfield.
    ENDIF.

    MESSAGE s028(zaqo_message) WITH cs_old->name lv_subfield.
    zcx_aqo_exception=>raise_sys_error( ).
    CLEAR lv_subfield.
  END-OF-DEFINITION.

  " ERROR - Parameter has now become Table or Range ?
  IF is_new-sys_type <> cs_old->sys_type OR
     is_new-ui_type  <> cs_old->ui_type.
    raise_error.
  ENDIF.

  " Check sub fields
  IF cs_old->sub_fdesc IS NOT INITIAL.
    from_json(
     EXPORTING
       iv_json = cs_old->sub_fdesc
     IMPORTING
       ex_data = lt_old
       ev_ok   = lv_old_ok ).

    from_json(
     EXPORTING
       iv_json = is_new-sub_fdesc
     IMPORTING
       ex_data = lt_new
       ev_ok   = lv_new_ok ).

    " Json format error
    IF lv_old_ok <> abap_true OR lv_new_ok <> abap_true.
      raise_error.
    ENDIF.

    LOOP AT lt_old REFERENCE INTO ls_old.
      " Field deleted
      READ TABLE lt_new REFERENCE INTO ls_new
       WITH TABLE KEY name = ls_old->name.
      IF sy-subrc <> 0.
        IF iv_repair = abap_true.
          DELETE lt_old WHERE name = ls_old->name.
          lv_changed = abap_true.
        ELSE.
          lv_subfield = ls_old->name.
          raise_error.
        ENDIF.

        CONTINUE.
      ENDIF.

      " Recursive check
      compare_2_fields(
       EXPORTING
         is_new     = ls_new->*
         iv_repair  = iv_repair
         cs_old     = ls_old
       CHANGING
         cv_changed = lv_changed ).
    ENDLOOP.

    LOOP AT lt_new REFERENCE INTO ls_new.
      READ TABLE lt_old REFERENCE INTO ls_old
       WITH TABLE KEY name = ls_new->name.
      CHECK sy-subrc <> 0.

      IF iv_repair = abap_true.
        INSERT ls_new->* INTO TABLE lt_old.
        lv_changed = abap_true.
      ELSE.
        lv_subfield = ls_new->name.
        raise_error.
      ENDIF.
    ENDLOOP.
  ENDIF.

  " Repair option (pass as 'IV_REPAIR = abap_true')
  IF cs_old->length      <> is_new-length      OR
     cs_old->decimals    <> is_new-decimals    OR
     " cs_old->rollname    <> is_new-rollname    OR
     cs_old->table_kind  <> is_new-table_kind  OR
     cs_old->unique      <> is_new-unique      OR
     cs_old->key[]       <> is_new-key[]       OR
     cs_old->key_defkind <> is_new-key_defkind OR
     lv_changed         = abap_true.

    IF iv_repair <> abap_true.
      raise_error.
    ELSE.
      cv_changed         = abap_true.
      " Copy new elementary declarations
      cs_old->length      = is_new-length.
      cs_old->decimals    = is_new-decimals.
      " cs_old->rollname    = is_new-rollname.

      " Copy new table declarations
      cs_old->table_kind  = is_new-table_kind.
      cs_old->unique      = is_new-unique.
      cs_old->key         = is_new-key.
      cs_old->key_defkind = is_new-key_defkind.
      cs_old->sub_fdesc   = to_json( lt_old ).
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD confirm.
  DATA:
    lv_answer TYPE char1.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = iv_title
      text_question         = iv_question
      icon_button_1         = 'ICON_OKAY'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false
    IMPORTING
      answer                = lv_answer
    EXCEPTIONS
      OTHERS                = 1.
  CHECK sy-subrc = 0 AND lv_answer = '1'.

  rv_ok = abap_true.
ENDMETHOD.


METHOD CONVERT_TO_TIMESTAMP.
  DATA:
    lv_date           TYPE sy-datum,
    lv_date_from      TYPE sy-datum,
    lv_days_timestamp TYPE timestampl,
    lv_secs_timestamp TYPE timestampl,
    lv_days_i         TYPE i,
    lv_time           TYPE sy-uzeit,
    lv_sec_i          TYPE i,
    lv_timestamp      TYPE timestampl,
    lv_dummy          TYPE string.                          "#EC NEEDED

  CONSTANTS:
     lc_day_in_sec TYPE i VALUE 86400.

  " Null value
  rv_timestamp = ''.

  " Milliseconds for the days since January 1, 1970, 00:00:00 GMT
  " one day has 86400 seconds
  lv_date = iv_date.
  IF lv_date IS NOT INITIAL.
    lv_date_from       = '19700101'.
    lv_days_i          = lv_date - lv_date_from.
    " Timestamp for passed days until today in seconds
    lv_days_timestamp  = lv_days_i * lc_day_in_sec.
  ENDIF.

  lv_time  = iv_time.
  lv_sec_i = lv_time.
  " Timestamp for time at present day
  lv_secs_timestamp = lv_sec_i.
  IF lv_time IS NOT INITIAL.
    lv_secs_timestamp = lv_secs_timestamp - sy-tzone.
  ENDIF.

  " Just return ''
  lv_timestamp = ( lv_days_timestamp + lv_secs_timestamp ) * 1000.
  CHECK lv_timestamp IS NOT INITIAL.

  rv_timestamp = lv_timestamp.
  SPLIT rv_timestamp AT '.' INTO rv_timestamp lv_dummy.
  rv_timestamp = rv_timestamp + iv_msec.

  SHIFT rv_timestamp RIGHT DELETING TRAILING space.
  SHIFT rv_timestamp LEFT  DELETING LEADING space.
  " CONCATENATE '/Date(' RV_TIMESTAMP ')/' into RV_TIMESTAMP.
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
        <ls_fieldcat>-col_id = <ls_fieldcat>-col_pos = sy-tabix.

        IF <ls_fieldcat>-coltext IS INITIAL.
          <ls_fieldcat>-coltext = <ls_fieldcat>-reptext.
        ENDIF.

        IF <ls_fieldcat>-coltext IS INITIAL.
          <ls_fieldcat>-coltext = <ls_fieldcat>-scrtext_l.
        ENDIF.

        IF <ls_fieldcat>-domname = 'XSDBOOLEAN'.
          <ls_fieldcat>-checkbox = abap_true.
        ENDIF.
      ENDLOOP.
    CATCH cx_salv_error.                                "#EC NO_HANDLER
  ENDTRY.
ENDMETHOD.


METHOD create_structure.
  DATA:
    lt_comp      TYPE abap_component_tab,
    lt_sub_fdesc TYPE tt_field_desc,
    lv_ok        TYPE abap_bool.
  FIELD-SYMBOLS:
    <ls_field_desc> TYPE ts_field_desc,
    <ls_subfield>   TYPE ts_field_desc,
    <ls_comp>       LIKE LINE OF lt_comp.

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
    CHECK iv_sub_fdesc IS NOT INITIAL.
    from_json(
     EXPORTING
       iv_json = iv_sub_fdesc
     IMPORTING
       ex_data = lt_sub_fdesc
       ev_ok   = lv_ok ).
    CHECK lv_ok = abap_true.

    LOOP AT lt_sub_fdesc ASSIGNING <ls_subfield>.
      APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
      <ls_comp>-name = <ls_subfield>-name.
      <ls_comp>-type = create_type_descr( is_field_desc = <ls_subfield> ).
    ENDLOOP.
  ENDDO.

  " №4 Called from constructor if have in DB cluster
  LOOP AT it_field_desc ASSIGNING <ls_field_desc>.
    " Create sub levels
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = <ls_field_desc>-name.
    <ls_comp>-type = create_type_descr( is_field_desc = <ls_field_desc> ).
  ENDLOOP.

*  TRY.
  ro_struct = cl_abap_structdescr=>create( lt_comp ).
*    CATCH cx_sy_type_creation INTO lo_prev_error.
*      RAISE EXCEPTION TYPE zcx_aqo_exception EXPORTING previous = lo_prev_error.
*  ENDTRY.

ENDMETHOD.


METHOD create_type_descr.
  DATA:
    lo_line       TYPE REF TO cl_abap_datadescr,
    lo_prev_error TYPE REF TO cx_root,
    lo_type       TYPE REF TO cl_abap_typedescr.

  " No type
  CLEAR ro_type.

  TRY.
      " №0
      IF is_field_desc IS SUPPLIED.

*    " structure for DB
*    IF mo_ui_ext IS NOT INITIAL.
*      TRY.
*          ir_type = mo_ui_ext->create_field_type(
*           iv_name = is_field_desc-name
*           iv_type = zif_prog_params_ui_ext=>mc_type_db ).
*        CATCH cx_sy_dyn_call_illegal_method.
*          CLEAR ir_type.
*      ENDTRY.
*      CHECK ir_type IS INITIAL.
*    ENDIF.

        " For tables speed 1
        IF is_field_desc-rollname IS NOT INITIAL.
          " Try to create
          TRY.
              ro_type = create_type_descr(
                iv_rollname = is_field_desc-rollname ).
            CATCH zcx_aqo_exception.
              CLEAR ro_type.
          ENDTRY.
        ENDIF.

        IF ro_type IS INITIAL.
          CASE is_field_desc-sys_type.
            WHEN cl_abap_typedescr=>typekind_char.
              ro_type = cl_abap_elemdescr=>get_c( p_length = is_field_desc-length ).
            WHEN cl_abap_typedescr=>typekind_date.
              ro_type = cl_abap_elemdescr=>get_d( ).
            WHEN cl_abap_typedescr=>typekind_int.
              ro_type = cl_abap_elemdescr=>get_i( ).
            WHEN cl_abap_typedescr=>typekind_float.
              ro_type = cl_abap_elemdescr=>get_f( ).
            WHEN cl_abap_typedescr=>typekind_num.
              ro_type = cl_abap_elemdescr=>get_n( p_length = is_field_desc-length ).
            WHEN cl_abap_typedescr=>typekind_packed.
              ro_type = cl_abap_elemdescr=>get_p( p_length = is_field_desc-length p_decimals = is_field_desc-decimals ).
            WHEN cl_abap_typedescr=>typekind_string.
              ro_type = cl_abap_elemdescr=>get_string( ).
            WHEN cl_abap_typedescr=>typekind_time.
              ro_type = cl_abap_elemdescr=>get_t( ).
            WHEN cl_abap_typedescr=>typekind_table.
              " Below in code CASE is_field_desc-ui_type.

            WHEN OTHERS.
              MESSAGE s007(zaqo_message) WITH is_field_desc-name INTO sy-msgli.
              zcx_aqo_exception=>raise_sys_error( ).
          ENDCASE.
        ENDIF.

        CASE is_field_desc-ui_type.
          WHEN mc_ui_range.
            " Call №2 recursion
            lo_line = create_structure( io_range = ro_type ).
            ro_type = cl_abap_tabledescr=>create( p_line_type = lo_line ).

          WHEN mc_ui_table.
            " Call №3 recursion
            IF ro_type IS INITIAL.
              ro_type = create_structure( iv_sub_fdesc = is_field_desc-sub_fdesc ).
            ENDIF.

            ro_type = cl_abap_tabledescr=>create(
              p_line_type   = ro_type
              p_table_kind  = is_field_desc-table_kind
              p_unique      = is_field_desc-unique
              p_key         = is_field_desc-key
              p_key_kind    = is_field_desc-key_defkind ).
        ENDCASE.
      ENDIF.

      CHECK ro_type IS INITIAL.

      " №0
      IF iv_rollname NP '*-*'.
        cl_abap_datadescr=>describe_by_name(
         EXPORTING
          p_name         = iv_rollname
         RECEIVING
          p_descr_ref    = lo_type
         EXCEPTIONS
          type_not_found = 1 ).

        " Handle somewhere else
        IF sy-subrc <> 0.
          MESSAGE s021(zaqo_message) WITH iv_rollname INTO sy-msgli.
          zcx_aqo_exception=>raise_sys_error( ).
        ENDIF.
        ro_type ?= lo_type.
        RETURN.
      ENDIF.

      " №1 - Create from text
      IF ir_type IS INITIAL AND iv_rollname IS NOT INITIAL.
        CREATE DATA ir_type TYPE (iv_rollname).
      ENDIF.

      " №2 - Based on incoming reference
      cl_abap_datadescr=>describe_by_data_ref(
       EXPORTING
        p_data_ref           = ir_type
       RECEIVING
        p_descr_ref          = lo_type
       EXCEPTIONS
        reference_is_initial = 1 ).

      " Handle somewhere else
      IF sy-subrc <> 0.
        MESSAGE s021(zaqo_message) WITH iv_rollname INTO sy-msgli.
        zcx_aqo_exception=>raise_sys_error( ).
      ENDIF.
      ro_type ?= lo_type.
    CATCH cx_root INTO lo_prev_error.
      CLEAR ro_type.
      zcx_aqo_exception=>raise_sys_error( ).
  ENDTRY.
ENDMETHOD.


METHOD download.
  DATA:
    lv_xstring TYPE xstring,
    lv_size    TYPE int4,
    lt_xdata   TYPE w3mimetabtype,
    lv_bom     TYPE xstring.

  lv_xstring = iv_xcontent.

  " Convert from string
  if iv_content IS SUPPLIED.
    lv_xstring = string_to_xstring(
     iv_string   = iv_content
     iv_encoding = iv_encoding ).

    " Add bom
    CASE iv_encoding.
      WHEN '4102'. " UTF-16 BE
        lv_bom = cl_abap_char_utilities=>byte_order_mark_big.
      WHEN '4103'. " UTF-16 LE
        lv_bom = cl_abap_char_utilities=>byte_order_mark_little.
      WHEN '4110'. " UTF-8
        lv_bom = cl_abap_char_utilities=>byte_order_mark_utf8.
    ENDCASE.

    IF lv_bom IS NOT INITIAL.
      CONCATENATE lv_bom lv_xstring INTO lv_xstring IN BYTE MODE.
    ENDIF.
  ENDIF.

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
*     write_bom    = abap_true
*     codepage     = iv_encoding
    TABLES
      data_tab     = lt_xdata
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
     DISPLAY LIKE 'E'.
  ENDIF.
ENDMETHOD.


METHOD DRILL_DOWN.
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


METHOD edit_in_popup.
  " TODO use synamic screen instead (bug with P)
  DATA:
    lt_field TYPE STANDARD TABLE OF sval,
    ls_field TYPE REF TO sval,
    lv_rc    TYPE char1.

  CLEAR cv_ok.
  APPEND INITIAL LINE TO lt_field REFERENCE INTO ls_field.

  " For F4
  SPLIT iv_type AT '-' INTO ls_field->tabname ls_field->fieldname.
  CHECK sy-subrc = 0
   AND ls_field->tabname IS NOT INITIAL
   AND ls_field->fieldname IS NOT INITIAL.

  ls_field->value = cv_value.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      no_value_check  = abap_true
      popup_title     = iv_title
    IMPORTING
      returncode      = lv_rc
    TABLES
      fields          = lt_field
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF sy-subrc <> 0 OR lv_rc = 'A'.
    MESSAGE s118(ed) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  " First row
  READ TABLE lt_field REFERENCE INTO ls_field INDEX 1.
  CHECK sy-subrc = 0.

  " Set new value
  cv_value = ls_field->value.
  MESSAGE s004(zaqo_message).
  cv_ok = abap_true.
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
  FIELD-SYMBOLS:
    <lt_unique_type> TYPE tt_unique_type.

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
  ASSIGN ir_unique_type->* TO <lt_unique_type>.
  LOOP AT lt_dd03l REFERENCE INTO ls_dd03l.
    CONCATENATE ls_dd03l->tabname '-' ls_dd03l->fieldname INTO lv_tabfld.

    " if type exist
    TRY.
        lo_type = create_type_descr( iv_rollname = lv_tabfld ).
      CATCH zcx_aqo_exception.
        CLEAR lo_type.
    ENDTRY.
    CHECK lo_type IS NOT INITIAL.

    " Get next item
    IF ir_unique_type IS NOT INITIAL.
      READ TABLE <lt_unique_type> TRANSPORTING NO FIELDS
       WITH TABLE KEY table_line = lv_tabfld.
      CHECK sy-subrc <> 0.

      " Do not repeat types
      INSERT lv_tabfld INTO TABLE <lt_unique_type>.
    ENDIF.
    cv_rollname = lv_tabfld.

    DO 1 TIMES.
      " If have no text
      CHECK cv_label IS SUPPLIED AND cv_label IS INITIAL.

      " №2
      SELECT SINGLE * INTO ls_dd04t
      FROM dd04t
      WHERE rollname   = lv_rollname
        AND ddlanguage = sy-langu
        AND as4local   = 'A'
        AND as4vers    = 0.
      CHECK sy-subrc = 0.

      IF ls_dd04t-ddtext IS NOT INITIAL.
        cv_label = ls_dd04t-ddtext.
      ELSE.
        cv_label = ls_dd04t-reptext.
      ENDIF.
    ENDDO.

    RETURN.
  ENDLOOP.
ENDMETHOD.


METHOD from_json.
  DATA:
    lv_xstring TYPE xstring,
    lv_length  TYPE i,
    lt_table   TYPE w3mimetabtype.

  " No need
  IF iv_json IS INITIAL.
    ev_ok = abap_false. " Always have {"DATA":}
    RETURN.
  ENDIF.

  TRY.
      " Work with JSON even in ABAP 7.02! 000116
      " Regardless the fact that it do not have if_sxml=>co_xt_json
*      CALL TRANSFORMATION id SOURCE XML iv_json
*                             RESULT data = ex_data.

      " For version ABAP 7.02 patch level 006
      zcl_aqo_helper=>json_2_abap(
       EXPORTING
        json_string = iv_json
        var_name    = 'DATA'
       CHANGING
        abap_data   = ex_data ).

      ev_ok = abap_true.

*    CATCH cx_transformation_error.
    CATCH zcx_aqo_exception.
      ev_ok = abap_false.

      " Is not procced in caller!
      IF ev_ok IS NOT REQUESTED.
        MESSAGE 'Oops!' TYPE 'X'.                           "#EC NOTEXT

        " For debug
        lv_xstring = string_to_xstring( iv_json ).
        xstring_to_binary(
         EXPORTING
           iv_xstring = lv_xstring
         IMPORTING
           ev_length  = lv_length
           et_table   = lt_table ).

        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename     = 'c:\temp\dump.json'
            filetype     = 'BIN'
            bin_filesize = lv_length
          TABLES
            data_tab     = lt_table
          EXCEPTIONS
            OTHERS       = 1.
        CHECK sy-subrc = 0.
      ENDIF.
  ENDTRY.
ENDMETHOD.


METHOD get_field_desc.
  DATA:
    ls_header        TYPE x030l,
    lr_table_descr   TYPE REF TO cl_abap_tabledescr,
    lr_struct_descr  TYPE REF TO cl_abap_structdescr,
    lv_cnt           TYPE i,
    lr_row           TYPE REF TO data,
    lo_type          TYPE REF TO cl_abap_typedescr,
    lt_sub_fdesc     TYPE tt_field_desc,
    " lv_find_db_field TYPE abap_bool,
    ls_subfield      TYPE ts_field_desc.
  FIELD-SYMBOLS:
    <ls_comp_tab> TYPE abap_compdescr,
    <ls_row>      TYPE any,
    <lv_subvalue> TYPE any,
    <ls_subfield> LIKE ls_subfield.

  IF is_sh_field IS NOT INITIAL.
    rs_field_desc-name     = is_sh_field-fieldname.
    rs_field_desc-sys_type = is_sh_field-inttype.
    rs_field_desc-length   = is_sh_field-leng.
    rs_field_desc-decimals = is_sh_field-decimals.
    rs_field_desc-label    = is_sh_field-fieldtext.
    rs_field_desc-rollname = is_sh_field-rollname.
  ELSE.
    lo_type = cl_abap_typedescr=>describe_by_data( iv_data ).
    rs_field_desc-name     = iv_field_name.
    rs_field_desc-sys_type = lo_type->type_kind. "kind.
    rs_field_desc-length   = lo_type->length.
    rs_field_desc-decimals = lo_type->decimals.
    IF lo_type->is_ddic_type( ) = abap_true.
      rs_field_desc-rollname = lo_type->get_relative_name( ).
    ENDIF.
  ENDIF.

  CASE rs_field_desc-sys_type.

    WHEN cl_abap_typedescr=>typekind_char.
      " Also CHAR
      CASE rs_field_desc-rollname.
        WHEN 'XSDBOOLEAN'.
          rs_field_desc-ui_type = mc_ui_boolean.

        WHEN 'XSDDATETIME_Z' OR 'XSDDATETIME_LONG_Z' OR
             'XSDDATETIME_OFFSET' OR 'XSDDATETIME_LOCAL' OR 'XSDDATETIME_LOCAL_DT'.
          rs_field_desc-ui_type = mc_ui_datetime.

        WHEN OTHERS.
          rs_field_desc-ui_type  = mc_ui_char.
          " lv_find_db_field = abap_true.
      ENDCASE.

    WHEN cl_abap_typedescr=>typekind_num OR cl_abap_typedescr=>typekind_numeric.
      rs_field_desc-ui_type  = mc_ui_numc.
      " lv_find_db_field = abap_true.

      " Memo text
    WHEN cl_abap_typedescr=>typekind_string.
      rs_field_desc-ui_type  = mc_ui_string.
      rs_field_desc-rollname = 'STRINGVAL'.

    WHEN cl_abap_typedescr=>typekind_table.
      rs_field_desc-ui_type  = mc_ui_table.
      lr_table_descr ?= lo_type.

      rs_field_desc-table_kind   = lr_table_descr->table_kind.
      rs_field_desc-unique       = lr_table_descr->has_unique_key.
      rs_field_desc-key          = lr_table_descr->key.
      rs_field_desc-key_defkind  = lr_table_descr->key_defkind.

      " No need for standardc table
      IF rs_field_desc-table_kind = cl_abap_tabledescr=>tablekind_std.
        CLEAR rs_field_desc-key.
      ENDIF.

      " Only for structures
      TRY.
          lr_struct_descr ?= lr_table_descr->get_table_line_type( ).
        CATCH cx_sy_move_cast_error.
          MESSAGE s008(zaqo_message) WITH rs_field_desc-name INTO sy-msgli.
          zcx_aqo_exception=>raise_sys_error( ).
      ENDTRY.

      " For speed creation
      IF lr_struct_descr->is_ddic_type( ) = abap_true.
        ls_header = lr_struct_descr->get_ddic_header( ).
        rs_field_desc-rollname     = ls_header-tabname.
      ENDIF.

      " Create STANDARD table for field catalog!
      CREATE DATA lr_row TYPE HANDLE lr_struct_descr.
      ASSIGN lr_row->* TO <ls_row>.

      CLEAR:
       rs_field_desc-sub_fdesc,
       lt_sub_fdesc.
      LOOP AT lr_struct_descr->components ASSIGNING <ls_comp_tab>.
        ASSIGN COMPONENT <ls_comp_tab>-name OF STRUCTURE <ls_row> TO <lv_subvalue>.

        " Recursion
        ls_subfield = get_field_desc( iv_field_name  = <ls_comp_tab>-name
                                      iv_data        = <lv_subvalue>
                                      ir_unique_type = ir_unique_type ).

        INSERT ls_subfield INTO TABLE lt_sub_fdesc.
      ENDLOOP.

      rs_field_desc-sub_fdesc = to_json( im_data = lt_sub_fdesc ).

      " Select option ?
      DO 1 TIMES.
        lv_cnt = lines( lt_sub_fdesc ).

        CHECK lv_cnt = 4.
        " Check by name
        LOOP AT lt_sub_fdesc TRANSPORTING NO FIELDS WHERE
           name = 'SIGN' OR name = 'OPTION' OR name = 'LOW' OR name = 'HIGH'. "#EC CI_HASHSEQ
          lv_cnt = lv_cnt - 1.
        ENDLOOP.

        " Select-option
        CHECK lv_cnt = 0.
        rs_field_desc-ui_type  = mc_ui_range.

        " No need in components
        CLEAR rs_field_desc-sub_fdesc.

        " Where to find TABLE-FIELDNAME
        READ TABLE lt_sub_fdesc ASSIGNING <ls_subfield>
         WITH TABLE KEY name = 'LOW'.
        rs_field_desc-rollname = <ls_subfield>-rollname.
        " lv_find_db_field = abap_true.
      ENDDO.

      " Date
    WHEN cl_abap_typedescr=>typekind_date.
      rs_field_desc-ui_type  = mc_ui_date.

      " Time
    WHEN cl_abap_typedescr=>typekind_time.
      rs_field_desc-ui_type  = mc_ui_time.

      " Integer, byte, short
    WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1  OR cl_abap_typedescr=>typekind_int2.
      rs_field_desc-ui_type  = mc_ui_numeric.

      " Double
    WHEN cl_abap_typedescr=>typekind_packed OR cl_abap_typedescr=>typekind_float OR
         '/' OR 'a' OR 'e'. " cl_abap_typedescr=>typekind_decfloat  OR cl_abap_typedescr=>typekind_decfloat16 OR cl_abap_typedescr=>typekind_decfloat34.
      rs_field_desc-ui_type  = mc_ui_numeric.

    WHEN OTHERS.

  ENDCASE.

  " TABLE-FIELDNAME from search help
  IF is_sh_field-reffield IS NOT INITIAL.
    CONCATENATE is_sh_field-reftable '-' is_sh_field-reffield INTO rs_field_desc-rollname.
  ENDIF.

  " Try to find TABLE-FIELDNAME
  IF " lv_find_db_field = abap_true AND
    rs_field_desc-ui_type <> mc_ui_table  AND
    rs_field_desc-ui_type <> mc_ui_string AND
    rs_field_desc-rollname NP '*-*'.
    find_table_fieldname(
     EXPORTING
      iv_name        = rs_field_desc-name
      ir_unique_type = ir_unique_type
     CHANGING
      cv_rollname    = rs_field_desc-rollname
      cv_label       = rs_field_desc-label ).
  ENDIF.

  " Set default text
  IF rs_field_desc-label IS INITIAL.
    rs_field_desc-label = rs_field_desc-name.
  ENDIF.

*  IF rs_field_desc-kind <> mc_kind_table AND
*     rs_field_desc-rollname IS INITIAL.
*    MESSAGE e009(zaqo_message) WITH rs_field_desc-name RAISING cannot_detect_type.
*  ENDIF.
ENDMETHOD.


METHOD get_last_call_info.
  DATA:
    lv_len TYPE i.

  ev_name = is_last_call-mainprogram.
  " Is class
  IF is_last_call-blocktype = 'METHOD' AND is_last_call-include CP '*=CM*'.
    ev_is_class = abap_true.

    " Delete 'CP' at the end
    lv_len = strlen( ev_name ) - 2.
    ev_name = ev_name(lv_len).

    " Delete all '='
    REPLACE ALL OCCURRENCES OF `=` IN ev_name WITH ``.
  ENDIF.
ENDMETHOD.


METHOD get_position.
  DATA:
    lt_report TYPE stringtab,
    lt_result TYPE match_result_tab,
    ls_result TYPE REF TO match_result,
    lv_string TYPE string,
    lv_from   TYPE i,
    lv_index  TYPE syindex,
    lv_ok_cnt TYPE i.

  " Read whole text
  READ REPORT iv_include INTO lt_report.

  " First occurance
  DO 2 TIMES.
    CASE sy-index.
      WHEN 1.
        lv_string = iv_package.
      WHEN 2.
        lv_string = 'IV_PACKAGE_ID'.
      WHEN OTHERS.
    ENDCASE.

    REPLACE ALL OCCURRENCES OF '$' IN lv_string WITH ''.
    CONCATENATE '\b' lv_string '\b' INTO lv_string.
    FIND FIRST OCCURRENCE OF REGEX lv_string IN TABLE lt_report IGNORING CASE RESULTS lt_result.

    " Found or not
    READ TABLE lt_result INDEX 1 REFERENCE INTO ls_result.
    IF sy-subrc = 0.
      ADD 1 TO lv_ok_cnt.
    ENDIF.
  ENDDO.

  " Second one
  CHECK ls_result IS NOT INITIAL AND lv_ok_cnt = 2.
  lv_from = ls_result->line - 4.
  IF lv_from <= 0.
    lv_from = 1.
  ENDIF.

  DO 2 TIMES.
    lv_index = sy-index.
    CASE lv_index.
      WHEN 1.
        lv_string = iv_option.
      WHEN 2.
        lv_string = 'IV_OPTION_ID'.
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
    ls_opt        TYPE ztaqo_option,
    ls_usage      TYPE REF TO ts_usage,
    lv_len        TYPE i,
    lv_class_name TYPE seoclskey,
    lv_rem        TYPE string,
    lo_clif       TYPE REF TO if_oo_clif_incl_naming,
    lo_cldesc     TYPE REF TO if_oo_class_incl_naming,
    lt_meth       TYPE seop_methods_w_include,
    ls_meth       TYPE REF TO seop_method_w_include.

  " Get from memory
  GET PARAMETER ID:
   'ZAQO_PACKAGE_ID' FIELD ls_opt-package_id,
   'ZAQO_OPTION_ID'  FIELD ls_opt-option_id.

  " Index for Global Types - Where-Used List Workbench
  SELECT * INTO CORRESPONDING FIELDS OF TABLE rt_usage
  FROM wbcrossgt
  WHERE otype = 'ME'
    AND name  = 'ZCL_AQO_OPTION\ME:CREATE'.

  LOOP AT rt_usage REFERENCE INTO ls_usage.
    IF ls_opt-package_id IS NOT INITIAL AND ls_opt-option_id IS NOT INITIAL.
      get_position(
       EXPORTING
         iv_include   = ls_usage->include
         iv_package   = ls_opt-package_id
         iv_option    = ls_opt-option_id
       IMPORTING
         ev_line      = ls_usage->line
         ev_found     = ls_usage->found ).
      IF ls_usage->found = abap_true.
        ls_usage->package_id = ls_opt-package_id.
        ls_usage->option_id  = ls_opt-option_id.
      ENDIF.
    ENDIF.

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
  LOOP AT rt_usage REFERENCE INTO ls_usage.
    ls_usage->index = sy-tabix.
  ENDLOOP.
ENDMETHOD.


METHOD is_dev_mandt.
  DATA:
    lv_cccoractiv TYPE t000-cccoractiv.

  " Check client
  SELECT SINGLE cccoractiv INTO lv_cccoractiv
  FROM t000
  WHERE mandt = sy-mandt.
  CHECK lv_cccoractiv <> '2'.

  rv_editable = abap_true.
ENDMETHOD.


METHOD is_in_editor.
  DATA:
    lv_tcode TYPE sytcode.

  " for BSP
  IF iv_tcode IS NOT INITIAL.
    mv_code = iv_tcode.
    RETURN.
  ENDIF.

  " Use from memory
  lv_tcode = mv_code.
  IF lv_tcode IS INITIAL.
    lv_tcode = sy-tcode.
  ENDIF.

  " if any is supplied ignore
  IF iv_is_sapui5 = abap_true OR iv_is_viewer = abap_true.
    iv_any = abap_false.
  ENDIF.

  IF ( iv_is_sapui5 = abap_true OR iv_any = abap_true ) AND
     ( lv_tcode = 'ZAQO_VIEWER' OR lv_tcode = 'ZAQO_EDITOR'
      OR sy-xform = 'ZFM_POST_ACTION' ).
    rv_ok = abap_true.
    RETURN.
  ENDIF.

  IF ( iv_is_viewer = abap_true OR iv_any = abap_true ) AND
     ( lv_tcode = 'ZAQO_VIEWER' OR lv_tcode = 'ZAQO_VIEWER_OLD' ).
    rv_ok = abap_true.
    RETURN.
  ENDIF.

  " If also check old UI
  IF iv_any = abap_true AND
    ( lv_tcode = 'ZAQO_VIEWER_OLD' OR lv_tcode = 'ZAQO_EDITOR_OLD' ).
    rv_ok = abap_true.
  ENDIF.
ENDMETHOD.


METHOD json_2_abap.
*/************************************************/*
*/ Input any abap data and this method tries to   /*
*/ fill it with the data in the JSON string.      /*
*/  Thanks to Juan Diaz for helping here!!        /*
*/************************************************/*

  TYPE-POOLS: abap, js.

  DATA:
    js_script         TYPE string,
    js_started        TYPE i VALUE 0,
    l_json_string     TYPE string,
    js_property_table TYPE   js_property_tab,
    js_property       TYPE LINE OF js_property_tab,
    l_property_path   TYPE string,
    compname          TYPE string,
    item_path         TYPE string.

  DATA:
    l_type   TYPE c,
    l_value  TYPE string,
    linetype TYPE string,
    l_comp   TYPE LINE OF abap_compdescr_tab.

  DATA:
    datadesc  TYPE REF TO cl_abap_typedescr,
    drefdesc  TYPE REF TO cl_abap_typedescr,
    linedesc  TYPE REF TO cl_abap_typedescr,
    strudesc  TYPE REF TO cl_abap_structdescr,
    tabldesc  TYPE REF TO cl_abap_tabledescr,
    data_desc TYPE REF TO cl_abap_datadescr.

  DATA newline TYPE REF TO data.

  DATA:
    BEGIN OF ls_error_text,
      part1 TYPE symsgv,
      part2 TYPE symsgv,
      part3 TYPE symsgv,
      part4 TYPE symsgv,
    END OF ls_error_text.

  FIELD-SYMBOLS:
    <abap_data> TYPE any,
    <itab>      TYPE ANY TABLE,
    <comp>      TYPE any,
    <jsprop>    TYPE LINE OF js_property_tab,
    <abapcomp>  TYPE abap_compdescr.


  DEFINE assign_scalar_value.
    "   &1   <abap_data>
    "   &2   js_property-value
    DESCRIBE FIELD &1 TYPE l_type.
    l_value = &2.
* convert or adapt scalar values to ABAP.
    CASE l_type.
      WHEN 'D'. " date type
        IF l_value CS '-'.
          REPLACE ALL OCCURRENCES OF '-' IN l_value WITH space.
          CONDENSE l_value NO-GAPS.
        ENDIF.
      WHEN 'T'. " time type
        IF l_value CS ':'.
          REPLACE ALL OCCURRENCES OF ':' IN l_value WITH space.
          CONDENSE l_value NO-GAPS.
        ENDIF.
      WHEN OTHERS.
        " may be other conversions or checks could be implemented here.
    ENDCASE.
    &1 = l_value.
  END-OF-DEFINITION.


  IF js_object IS NOT BOUND.

    IF json_string IS INITIAL. EXIT. ENDIF. " exit method if there is nothing to parse

    l_json_string = json_string.
    " js_object = cl_java_script=>create( STACKSIZE = 16384 ).
    js_object = cl_java_script=>create( stacksize = 16384 heapsize = 960000 ).

***************************************************
*  Parse JSON using JavaScript                    *
***************************************************
    js_object->bind( EXPORTING name_obj = 'abap_data' name_prop = 'json_string'    CHANGING data = l_json_string ).
    js_object->bind( EXPORTING name_obj = 'abap_data' name_prop = 'script_started' CHANGING data = js_started ).

* We use the JavaScript engine included in ABAP to read the JSON string.
* We simply use the recommended way to eval a JSON string as specified
* in RFC 4627 (http://www.ietf.org/rfc/rfc4627.txt).
*
* Security considerations:
*
*   Generally there are security issues with scripting languages.  JSON
*   is a subset of JavaScript, but it is a safe subset that excludes
*   assignment and invocation.
*
*   A JSON text can be safely passed into JavaScript's eval() function
*   (which compiles and executes a string) if all the characters not
*   enclosed in strings are in the set of characters that form JSON
*   tokens.  This can be quickly determined in JavaScript with two
*   regular expressions and calls to the test and replace methods.
*
*      var my_JSON_object = !(/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test(
*             text.replace(/"(\\.|[^"\\])*"/g, ''))) &&
*         eval('(' + text + ')');

    CONCATENATE

         'var json_obj; '
         'var json_text; '

         'function start() { '
         '  if(abap_data.script_started) { return; } '
         '  json_text = abap_data.json_string;'
         '  json_obj = !(/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test( '
         '      json_text.replace(/"(\\.|[^"\\])*"/g, ''''))) && '
         '    eval(''('' + json_text + '')''); '
         '  abap_data.script_started = 1; '
         '} '

         'if(!abap_data.script_started) start(); '


       INTO js_script RESPECTING BLANKS SEPARATED BY cl_abap_char_utilities=>newline.

    js_object->compile( script_name = 'json_parser'     script = js_script ).
    js_object->execute( script_name = 'json_parser' ).

    IF js_object->last_error_message IS NOT INITIAL.
      ls_error_text = js_object->last_error_message.
      MESSAGE s000(zaqo_message) WITH ls_error_text-part1 ls_error_text-part2 ls_error_text-part3 ls_error_text-part4.
      zcx_aqo_exception=>raise_sys_error( ).
    ENDIF.

  ENDIF.
** End of JS processing.

**
  IF var_name IS NOT INITIAL.
    CONCATENATE property_path var_name INTO l_property_path SEPARATED BY '.'.
  ELSE.
    l_property_path = property_path.
  ENDIF.
**
**
  js_property_table = js_object->get_properties_scope_global( property_path = l_property_path ).
  property_table = js_property_table.

* Exit if abap_data is not supplied, normally when called
* from json_deserialize to get top level properties
  IF abap_data IS NOT SUPPLIED.
    EXIT.
  ENDIF. "***

*
* Get ABAP data type, dereference if necessary and start
  datadesc = cl_abap_typedescr=>describe_by_data( abap_data ).
  IF datadesc->kind EQ cl_abap_typedescr=>kind_ref.
    ASSIGN abap_data->* TO <abap_data>.
  ELSE.
    ASSIGN abap_data TO <abap_data>.
  ENDIF.
  datadesc = cl_abap_typedescr=>describe_by_data( <abap_data> ).


  CASE datadesc->kind.

    WHEN cl_abap_typedescr=>kind_elem.
* Scalar: process ABAP elements. Assume no type conversions for the moment.
      IF var_name IS INITIAL.
        ls_error_text = 'VAR_NAME is required for scalar values.'.
        MESSAGE s000(zaqo_message) WITH ls_error_text-part1 ls_error_text-part2 ls_error_text-part3 ls_error_text-part4.
        zcx_aqo_exception=>raise_sys_error( ).
      ENDIF.
      js_property_table = js_object->get_properties_scope_global( property_path = property_path ).
      READ TABLE js_property_table WITH KEY name = var_name INTO js_property.
      IF sy-subrc EQ 0.
        assign_scalar_value <abap_data> js_property-value.
      ENDIF.


    WHEN cl_abap_typedescr=>kind_struct.
* Process ABAP structures
      strudesc ?= datadesc.
      LOOP AT js_property_table ASSIGNING <jsprop>.
        compname = <jsprop>-name.
        TRANSLATE compname TO UPPER CASE.
        READ TABLE strudesc->components WITH KEY name = compname INTO l_comp.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT l_comp-name OF STRUCTURE <abap_data> TO <comp>.
          CASE l_comp-type_kind.
            WHEN    cl_abap_typedescr=>typekind_struct1  " 'v'
                 OR cl_abap_typedescr=>typekind_struct2  " 'u'
                 OR cl_abap_typedescr=>typekind_table.   " 'h' (may need a different treatment one day)
              CONCATENATE l_property_path <jsprop>-name INTO item_path SEPARATED BY '.'.
*> Recursive call here
              json_2_abap( EXPORTING property_path = item_path CHANGING abap_data = <comp> js_object = js_object ).

            WHEN OTHERS.
* Process scalars in structures (same as the kind_elem above)
              assign_scalar_value <comp> <jsprop>-value.

          ENDCASE.
        ENDIF.
      ENDLOOP.

    WHEN cl_abap_typedescr=>kind_table.
* Process ABAP tables
      IF js_property_table IS NOT INITIAL.
        tabldesc ?= datadesc.
        data_desc ?= tabldesc->get_table_line_type( ).

        ASSIGN <abap_data> TO <itab>.
        LOOP AT js_property_table INTO js_property WHERE name NE 'length'. " the JS object length

*          linetype = linedesc->get_relative_name( ).
          CREATE DATA newline TYPE HANDLE data_desc.
          ASSIGN newline->* TO <comp>.

          CASE js_property-kind.
            WHEN 'O'.
              CONCATENATE l_property_path js_property-name INTO item_path SEPARATED BY '.'.
              CONDENSE item_path.
*> Recursive call here
              json_2_abap( EXPORTING property_path = item_path CHANGING abap_data = newline js_object = js_object ).
            WHEN OTHERS. " Assume scalars, 'S', 'I', or other JS types
              " Process scalars in plain table components(same as the kind_elem above)
              assign_scalar_value <comp> js_property-value.
          ENDCASE.
          INSERT <comp> INTO TABLE <itab>.
          FREE newline.
        ENDLOOP.
      ENDIF.

    WHEN OTHERS. " kind_class, kind_intf
      " forget it.

  ENDCASE.
ENDMETHOD.


METHOD load_from_smw0.
  DATA:
    ls_key    TYPE wwwdatatab,
    lv_size_c TYPE wwwparams-value.

  " Load the whole archive
  ls_key-relid = iv_relid.
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
      mime              = et_table
    EXCEPTIONS
      wrong_object_type = 1
      import_error      = 2.
  CHECK sy-subrc = 0.

  ev_size = lv_size_c.
ENDMETHOD.


METHOD MESSAGE_WITH_FIELDS.
  DATA:
    BEGIN OF ls_string,
      part1 TYPE symsgv,
      part2 TYPE symsgv,
      part3 TYPE symsgv,
      part4 TYPE symsgv,
    END OF ls_string,

    lv_field TYPE string,
    lv_text  TYPE text255.

  " 1 long string
  LOOP AT it_field INTO lv_field.
    CONCATENATE ls_string ',' lv_field INTO ls_string.
  ENDLOOP.

  " Delete first comma
  IF sy-subrc = 0.
    ls_string = ls_string+1.
  ENDIF.

  MESSAGE ID 'ZAQO_MESSAGE' TYPE 'S' NUMBER iv_number WITH
   ls_string-part1
   ls_string-part2
   ls_string-part3
   ls_string-part4 INTO lv_text.
  rv_info = lv_text.
ENDMETHOD.


METHOD navigate_to.
  " No need
  CHECK iv_include IS NOT INITIAL.

  CALL FUNCTION 'RS_TOOL_ACCESS'
    EXPORTING
      operation   = 'SHOW'
      object_name = iv_include
      object_type = 'REPS'
      position    = iv_position
    EXCEPTIONS
      OTHERS      = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

  " All ok
  rv_ok = abap_true.
ENDMETHOD.


METHOD oaor_check_exists.
  DATA:
    ls_bds_locl TYPE bds_locl,
    lo_error    TYPE REF TO zcx_aqo_exception.

  " select request/task
  CLEAR ev_task.
  TRY.
      zcl_aqo_helper=>check_in_request(
       EXPORTING
         iv_table_name = 'BDS_LOCL'
         iv_key1       = iv_pack_id
         iv_key2       = mc_oaor_other
      CHANGING
         cv_task       = ev_task
         " cv_request    = rv_request
         ).
    CATCH zcx_aqo_exception INTO lo_error.
      CLEAR: "rv_request,
       ev_task.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

  " If ok
  CHECK ev_task IS NOT INITIAL.

  " exist ?
  SELECT SINGLE * INTO ls_bds_locl
  FROM bds_locl
  WHERE classname = iv_pack_id
    AND classtype = mc_oaor_other.
  CHECK sy-subrc <> 0.

  " Create new
  ls_bds_locl-classname = iv_pack_id.
  ls_bds_locl-classtype = mc_oaor_other.
  ls_bds_locl-lo_class  = 'BDS_LOC2'.
  ls_bds_locl-ph_class  = 'BDS_POC2'.
  ls_bds_locl-re_class  = 'BDS_REC2'.
  ls_bds_locl-tabname   = 'BDS_CONN05'.
  ls_bds_locl-log_level = 0. " Or 2 ?
  ls_bds_locl-crea_user = sy-uname.
  CONCATENATE sy-datum sy-uzeit INTO ls_bds_locl-crea_time.

  " Update DB
  INSERT bds_locl FROM ls_bds_locl.
ENDMETHOD.


METHOD oaor_delete_file.
  DATA:
    lv_key           TYPE sbdst_object_key,
    lt_bds_signature TYPE sbdst_signature,
    ls_bds_signature TYPE bapisignat,
    lo_error         TYPE REF TO zcx_aqo_exception.

  " Subfolder in OAOR (and classname = package_id)
  lv_key = iv_option_id.

  " Prepare signature
  MOVE-CORRESPONDING is_oaor_file TO ls_bds_signature.
  APPEND ls_bds_signature TO lt_bds_signature.

  cl_bds_document_set=>delete(
    EXPORTING
      classname      = iv_pack_id
      classtype      = mc_oaor_other
      object_key     = lv_key
      x_force_delete = abap_true
    CHANGING
      signature      = lt_bds_signature
    EXCEPTIONS
      OTHERS         = 7 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  " Delete old file
  TRY.
      check_in_request(
         EXPORTING
          is_oaor_file = is_oaor_file
         CHANGING
          cv_task      = cv_task ).
    CATCH zcx_aqo_exception INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.
ENDMETHOD.


METHOD oaor_get_files.
  DATA:
    lv_key               TYPE sbdst_object_key,
    lt_sbdst_signature   TYPE sbdst_signature,
    ls_sbdst_signature   TYPE REF TO bapisignat,
    lt_sbdst_components2 TYPE sbdst_components2,
    ls_sbdst_components2 TYPE REF TO bapicompo2,
*    lt_connect           TYPE srm_bdsconn_t,
*    ls_connect           TYPE REF TO bapiconnec,
    ls_oaor_file         TYPE REF TO ts_oaor_file,
    lt_doc_id            TYPE SORTED TABLE OF ts_oaor_file-doc_id WITH UNIQUE KEY table_line,
    lv_field             TYPE string.
  FIELD-SYMBOLS:
    <l_val>              TYPE any.

  CLEAR:
    es_oaor_last,
    et_oaor_file.

  " Subfolder in OAOR (and classname = package_id)
  lv_key = iv_option_id.

  " Finding the right documents
  cl_bds_document_set=>get_info(
   EXPORTING
    classname           = iv_pack_id
    classtype           = mc_oaor_other
    object_key          = lv_key
   IMPORTING
    extended_components = lt_sbdst_components2
    " connections         = lt_connect
   CHANGING
    signature           = lt_sbdst_signature
   EXCEPTIONS
    OTHERS              = 7 ).
  CHECK sy-subrc = 0.

  " lt_sbdst_signature structure is complex
  LOOP AT lt_sbdst_signature REFERENCE INTO ls_sbdst_signature.

    AT NEW doc_count.
      " Create new item
      APPEND INITIAL LINE TO et_oaor_file REFERENCE INTO ls_oaor_file.
      ls_oaor_file->doc_id     = ls_sbdst_signature->doc_id.
      ls_oaor_file->doc_ver_no = ls_sbdst_signature->doc_ver_no.
      ls_oaor_file->doc_var_id = ls_sbdst_signature->doc_var_id.
      ls_oaor_file->comp_count = ls_sbdst_signature->comp_count.

      " Concatenate 2 tables
      READ TABLE lt_sbdst_components2 REFERENCE INTO ls_sbdst_components2
       INDEX ls_sbdst_signature->doc_count.
      ls_oaor_file->objid     = ls_sbdst_components2->objid.
      ls_oaor_file->file_name = ls_sbdst_components2->file_name.
      ls_oaor_file->class     = ls_sbdst_components2->class.
    ENDAT.

    " Find field by name
    lv_field = ls_sbdst_signature->prop_name.
    IF lv_field = 'BDS_KEYWORD'.
      lv_field = ls_sbdst_signature->prop_value.
    ENDIF.

    ASSIGN COMPONENT lv_field OF STRUCTURE ls_oaor_file->* TO <l_val>.
    CHECK sy-subrc = 0.

    " Set flag
    IF ls_sbdst_signature->prop_name = 'BDS_KEYWORD'.
      <l_val> = abap_true.
    ELSE.
      <l_val> = ls_sbdst_signature->prop_value.
    ENDIF.
  ENDLOOP.

  " Apply filters
  IF iv_filename IS NOT INITIAL.
    DELETE et_oaor_file WHERE file_name <> iv_filename.
  ENDIF.

  " Max versions first
  SORT et_oaor_file BY doc_id doc_ver_no DESCENDING.

  " Fill other fields
  LOOP AT et_oaor_file REFERENCE INTO ls_oaor_file.
    " Mark current version
    READ TABLE lt_doc_id TRANSPORTING NO FIELDS
     WITH TABLE KEY table_line = ls_oaor_file->doc_id.
    IF sy-subrc <> 0.
      INSERT ls_oaor_file->doc_id INTO TABLE lt_doc_id.
      ls_oaor_file->last_version = abap_true.
    ENDIF.
  ENDLOOP.

  " Max versions last
  SORT et_oaor_file BY doc_id doc_ver_no ASCENDING.

  LOOP AT et_oaor_file REFERENCE INTO ls_oaor_file.
    ls_oaor_file->tabix = sy-tabix.

    " Create at
    ls_oaor_file->created_at_date      = ls_oaor_file->created_at(8).
    ls_oaor_file->created_at_time      = ls_oaor_file->created_at+8(6).

    " Changed at
    ls_oaor_file->last_changed_at_date = ls_oaor_file->last_changed_at(8).
    ls_oaor_file->last_changed_at_time = ls_oaor_file->last_changed_at+8(6).
  ENDLOOP.

  " Get last version for 1 file
  CHECK iv_filename IS NOT INITIAL
    AND es_oaor_last IS REQUESTED.

  READ TABLE et_oaor_file INTO es_oaor_last WITH KEY last_version = abap_true.
ENDMETHOD.


METHOD SH_SORT_ORDER.
  IF iv_value IS SUPPLIED.
    SET PARAMETER ID 'ZAQO_SH_SORT_ORDER' FIELD iv_value.
  ELSEIF ev_value IS REQUESTED.
    GET PARAMETER ID 'ZAQO_SH_SORT_ORDER' FIELD ev_value.
  ENDIF.
ENDMETHOD.


METHOD split_file_path.
  DATA:
    lv_ind     TYPE i,
    lv_cnt     TYPE i,
    lv_dot_pos TYPE i.

********************
  DEFINE set_if_requested.
    IF &1 IS REQUESTED.
      &1 = &2.
    ENDIF.
  END-OF-DEFINITION.
********************

  " What ?
  CHECK iv_fullpath IS NOT INITIAL.

  " Prepare vars
  "  set_if_requested ev_ev_tension ''.
  lv_ind = strlen( iv_fullpath ) - 1.
  TRY.
      WHILE lv_ind > 0 AND iv_fullpath+lv_ind(1) <> '\' AND iv_fullpath+lv_ind(1) <> '/'.
        IF iv_fullpath+lv_ind(1) = '.' AND lv_dot_pos IS INITIAL. " Only 1 time
          lv_dot_pos = lv_ind + 1.
          set_if_requested ev_extension iv_fullpath+lv_dot_pos.
        ENDIF.
        lv_ind = lv_ind - 1.
        lv_cnt = sy-index.
      ENDWHILE.
    CATCH cx_sy_range_out_of_bounds.
      EXIT.
  ENDTRY.

  " Fill ev_path, ev_filename, ev_filename_noext, ev_ev_tension
  IF lv_ind = 0.
    set_if_requested ev_filename iv_fullpath.
    set_if_requested ev_path     ''.

    IF lv_dot_pos IS INITIAL.
      set_if_requested ev_filename_noext iv_fullpath.
    ELSE.
      lv_cnt = lv_dot_pos - 1.
      set_if_requested ev_filename_noext iv_fullpath(lv_cnt).
    ENDIF.
  ELSE.
    lv_ind = lv_ind + 1.
    set_if_requested ev_filename iv_fullpath+lv_ind(lv_cnt).
    set_if_requested ev_path     iv_fullpath(lv_ind).

    IF lv_dot_pos IS INITIAL.
      set_if_requested ev_filename_noext iv_fullpath+lv_ind(lv_cnt).
    ELSE.
      lv_cnt = lv_dot_pos - lv_ind - 1.
      set_if_requested ev_filename_noext iv_fullpath+lv_ind(lv_cnt).
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD split_type.
  " Check is table and field name
  CHECK iv_datatype CP '*-*'.

  TRY.
      " Exist in data dictionary?
      create_type_descr( iv_rollname = iv_datatype ).

      " DB and field
      SPLIT iv_datatype AT '-' INTO ev_table ev_field.
    CATCH zcx_aqo_exception.
      CLEAR:
       ev_table,
       ev_field.
  ENDTRY.
ENDMETHOD.


METHOD string_to_xstring.
  " rv_xstring = cl_bcs_convert=>string_to_xstring( iv_string = iv_string iv_codepage = mc_utf8 ).
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text     = iv_string
      encoding = iv_encoding
    IMPORTING
      buffer   = rv_xstring.
ENDMETHOD.


METHOD TO_JSON.
  DATA:
    lv_end    TYPE i,
    lv_beg    TYPE i.

  rv_json = abap_2_json( im_data = im_data iv_name = 'DATA' ).
  CONCATENATE `{` rv_json `}` INTO rv_json.

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


METHOD XSTRING_TO_BINARY.
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
   encoding = iv_encoding
   input    = iv_xstring ).

  lo_conv->read(
   IMPORTING
    data =  rv_string ).
ENDMETHOD.
ENDCLASS.
