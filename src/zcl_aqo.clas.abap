class ZCL_AQO definition
  public
  create public .

public section.
  type-pools ABAP .

  types:
    BEGIN OF ts_clst_key,
        object    TYPE ZTAQO_DATA-object,
        subobject TYPE ZTAQO_DATA-subobject,
      END OF ts_clst_key .

  data MS_CLUSTER type ZTAQO_DATA read-only .
  data MS_KEY type TS_CLST_KEY read-only .

  methods CONSTRUCTOR
    importing
      !IV_OBJECT type ZTAQO_DATA-OBJECT
      !IV_SUBOBJECT type ZTAQO_DATA-SUBOBJECT
      !IR_DATA type ref to DATA optional
      !IO_DATA type ref to OBJECT optional
      !IV_SAVE_LAST_CALL type ABAP_BOOL default ABAP_TRUE
    raising
      ZCX_AQO_ERROR .
  methods DELETE
    importing
      !IV_CONFIRM type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_OK) type ABAP_BOOL .
  methods LOCK
    importing
      !IV_UNLOCK type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_OK) type ABAP_BOOL .
  methods READ
    importing
      !IV_SAFE_READ type ABAP_BOOL default ABAP_FALSE
    returning
      value(RT_EMPTY_FIELD) type STRINGTAB .
  methods SAVE
    importing
      !IV_MANDT type SYMANDT default SY-MANDT
      !IV_DELETE_OLD type ABAP_BOOL default ABAP_FALSE
      !IV_USE_ME_DATA type ABAP_BOOL default ABAP_TRUE
      !IV_CONFIRM type ABAP_BOOL default ABAP_TRUE
      !IV_MESSAGE type ABAP_BOOL default ABAP_TRUE .
  methods TRANSPORT
    returning
      value(RV_OK) type ABAP_BOOL .
  methods GET_FIELD_DATA
    importing
      !IV_NAME type CSEQUENCE
    returning
      value(RR_DATA) type ref to DATA .
protected section.

  data MT_FIELD_OPT type ZCL_AQO_UTIL=>TT_FIELD_OPT .
  data MS_LAST_CALL type ABAP_CALLSTACK_LINE .
private section.

  data MR_DATA type ref to DATA .
  data MO_DATA type ref to OBJECT .
  data MT_ALL_FIELD type TTFIELDNAME .
  data MT_UNQ_NAME type ZCL_AQO_UTIL=>TT_UNIQUE .

  methods CONSTRUCT_NEW_COMP
    importing
      !IV_FIELD_NAME type LVC_FNAME
      !IM_DATA type ANY
    returning
      value(RS_COMP) type ZCL_AQO_UTIL=>TS_COMP
    exceptions
      IS_NOT_STRUCTURE
      CANNOT_DETECT_TYPE .
ENDCLASS.



CLASS ZCL_AQO IMPLEMENTATION.


METHOD constructor.
  DATA:
    lv_xml       TYPE string,
    lo_struc     TYPE REF TO cl_abap_structdescr,
    lo_class     TYPE REF TO cl_abap_classdescr,
    ls_comp      TYPE REF TO abap_compdescr,
    ls_attr      TYPE REF TO abap_attrdescr,
    lv_is_stat   TYPE abap_bool,
    lt_friend    TYPE abap_frndtypes_tab,
    lt_callstack TYPE abap_callstack,
    ls_last_call TYPE REF TO abap_callstack_line,
    lv_name      TYPE string,
    lv_msgv1     TYPE symsgv,
    lv_msgv2     TYPE symsgv,
    lr_field_opt TYPE REF TO zcl_aqo_util=>ts_field_opt.

  " Key fields
  ms_key-object    = iv_object.
  ms_key-subobject = iv_subobject.

  " Load data
  IMPORT xml       = lv_xml
         "ui_ext    = mv_ui_ext_class
         last_call = ms_last_call FROM DATABASE ztaqo_data(00) TO ms_cluster ID ms_key.

  " First transform
  IF lv_xml IS NOT INITIAL.
    CALL TRANSFORMATION id
     SOURCE XML lv_xml
     RESULT field_opt = mt_field_opt.
  ENDIF.

  " Structure or object ?
  IF ir_data IS NOT INITIAL.
    mr_data = ir_data.
  ELSEIF io_data IS NOT INITIAL.
    mo_data = io_data.
  ENDIF.

**********************************************************************
  " №1 Based on class
  IF mo_data IS NOT INITIAL.
    lo_class ?= cl_abap_classdescr=>describe_by_object_ref( mo_data ).
    lv_name = lo_class->get_relative_name( ).

    " Check class
    lt_friend = lo_class->get_friend_types( ).
    READ TABLE lt_friend TRANSPORTING NO FIELDS
     WITH KEY table_line->absolute_name = '\CLASS=ZCL_AQO'.
    IF sy-subrc <> 0.
      lv_msgv1 = lv_name.
      RAISE EXCEPTION TYPE zcx_aqo_error
        EXPORTING
          textid = zcx_aqo_error=>no_friend
          msgv1  = lv_msgv1.
    ENDIF.

*    " Is ZIF_PROG_PARAMS_UI_EXT ?
*    DO 1 TIMES.
*      " Only for global classes
*      CONCATENATE '\CLASS=' lv_name INTO lv_name.
*      CHECK lo_class->absolute_name = lv_name. " is_ddic_type( ) = abap_true.
*
*      " Check interface
*      READ TABLE lo_class->interfaces TRANSPORTING NO FIELDS
*       WITH KEY name = 'ZIF_PROG_PARAMS_UI_EXT'.
*      CHECK sy-subrc = 0.
*
*      mv_ui_ext_class = lo_class->get_relative_name( ).
*    ENDDO.

    " name type_kind length decimals
    lv_is_stat = abap_undefined.
    LOOP AT lo_class->attributes REFERENCE INTO ls_attr
       WHERE visibility   = cl_abap_objectdescr=>public
         AND is_read_only = abap_true
         AND is_inherited = abap_false
         AND is_constant  = abap_false
         " AND is_class     = abap_false  Also initialize class data
         AND is_virtual   = abap_false.

      " Check instance or static
      IF lv_is_stat <> abap_undefined AND lv_is_stat <> ls_attr->is_class.
        lv_msgv1 = ls_attr->name.
        RAISE EXCEPTION TYPE zcx_aqo_error
          EXPORTING
            textid = zcx_aqo_error=>wrong_attr_order
            msgv1  = lv_msgv1.
      ENDIF.
      lv_is_stat = ls_attr->is_class.

      " And add to list
      APPEND ls_attr->name TO mt_all_field.
    ENDLOOP.
  ENDIF.

**********************************************************************
*  " UI extension
*  TRY.
*      CALL METHOD (mv_ui_ext_class)=>('ZIF_PROG_PARAMS_UI_EXT~GET_INSTANCE')
*        RECEIVING
*          ro_instance = mo_ui_ext.
*    CATCH cx_sy_dyn_call_error.
*      CLEAR:
*       mo_ui_ext,
*       mv_ui_ext_class.
*  ENDTRY.

**********************************************************************
  " Try to make stucture dynamically
  IF mr_data IS INITIAL AND mo_data IS INITIAL.
    " Just impossible to do something
    IF mt_field_opt IS INITIAL.
      lv_msgv1 = iv_object.
      lv_msgv2 = iv_subobject.
      RAISE EXCEPTION TYPE zcx_aqo_error
        EXPORTING
          textid = zcx_aqo_error=>no_option_exist
          msgv1  = lv_msgv1
          msgv2  = lv_msgv2.
    ENDIF.
    lo_struc = zcl_aqo_util=>create_structure( it_field_opt = mt_field_opt ).
    CREATE DATA mr_data TYPE HANDLE lo_struc.
  ENDIF.

**********************************************************************
  " №2 Based on structure
  IF mr_data IS NOT INITIAL.
    lo_struc ?= cl_abap_structdescr=>describe_by_data_ref( mr_data ).

    " name type_kind length decimals
    LOOP AT lo_struc->components REFERENCE INTO ls_comp.
      " And add to list
      APPEND ls_comp->name TO mt_all_field.
    ENDLOOP.
  ENDIF.
**********************************************************************

  " Make old again
  LOOP AT mt_field_opt REFERENCE INTO lr_field_opt WHERE is_old = abap_true.
    DELETE mt_all_field WHERE table_line = lr_field_opt->name.
  ENDLOOP.

**********************************************************************
  " Where-Used List
  CALL FUNCTION 'SYSTEM_CALLSTACK'
    EXPORTING
      max_level = 2
    IMPORTING
      callstack = lt_callstack.
  READ TABLE lt_callstack INDEX 2 REFERENCE INTO ls_last_call.
  IF sy-subrc = 0 AND iv_save_last_call = abap_true.
    ms_last_call = ls_last_call->*.
  ENDIF.

**********************************************************************
  DATA:
    lv_field     TYPE REF TO fieldname,
    lt_new_field TYPE STANDARD TABLE OF lvc_fname,
    lr_new_field TYPE REF TO lvc_fname,
    ls_field_opt TYPE zcl_aqo_util=>ts_field_opt,
    lr_data      TYPE REF TO data.
  " Field data
  FIELD-SYMBOLS:
    <lv_field>    TYPE any.

  " Try to find new options
  LOOP AT me->mt_all_field REFERENCE INTO lv_field.
    READ TABLE mt_field_opt TRANSPORTING NO FIELDS
     WITH KEY name = lv_field->*.
    CHECK sy-subrc <> 0.

    APPEND lv_field->* TO lt_new_field.
  ENDLOOP.

  " If have something new continue
  CHECK lt_new_field IS NOT INITIAL.

  " add description one by one
  LOOP AT lt_new_field REFERENCE INTO lr_new_field.
    lr_data = get_field_data( lr_new_field->* ).
    ASSIGN lr_data->* TO <lv_field>.

    ls_field_opt-comp = construct_new_comp( iv_field_name = lr_new_field->*
                                            im_data       = <lv_field> ).

    " And finally add new field option
    INSERT ls_field_opt INTO TABLE mt_field_opt.
  ENDLOOP.
ENDMETHOD.


METHOD construct_new_comp.
  DATA:
    lr_data         TYPE REF TO data,
    ls_header       TYPE x030l,
    lr_table_descr  TYPE REF TO cl_abap_tabledescr,
    lr_struct_descr TYPE REF TO cl_abap_structdescr,
    lv_cnt          TYPE i,
    lr_row          TYPE REF TO data,
    lo_type         TYPE REF TO cl_abap_typedescr,
    lt_subcomp      TYPE STANDARD TABLE OF zcl_aqo_util=>ts_comp,
    lv_name         TYPE string.
  FIELD-SYMBOLS:
    <ls_comp_tab> TYPE abap_compdescr,
    <ls_row>      TYPE ANY,
    <lv_subvalue> TYPE ANY,
    <ls_subfield> TYPE zcl_aqo_util=>ts_comp.

  lo_type = cl_abap_typedescr=>describe_by_data( im_data ).
  rs_comp-name     = iv_field_name.
  rs_comp-sys_type = lo_type->type_kind. "kind.
  rs_comp-length   = lo_type->length.
  rs_comp-decimals = lo_type->decimals.
  IF lo_type->is_ddic_type( ) = abap_true.
    rs_comp-rollname = lo_type->get_relative_name( ).
  ENDIF.

  " Is parameter
  IF rs_comp-sys_type <> cl_abap_typedescr=>typekind_table.
    rs_comp-kind = zcl_aqo_util=>mc_kind_parameter.
    CASE rs_comp-sys_type.
        " Memo text
      WHEN cl_abap_typedescr=>typekind_string.
        rs_comp-ui_type  = 'memo'.
        rs_comp-rollname = 'STRINGVAL'.
        rs_comp-kind     = zcl_aqo_util=>mc_kind_memo.

        " Date
      WHEN cl_abap_typedescr=>typekind_date.
        rs_comp-ui_type  = 'date'.

        " Time
      WHEN cl_abap_typedescr=>typekind_time.
        rs_comp-ui_type  = 'time'.

        " Integer, byte, short
      WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1  OR cl_abap_typedescr=>typekind_int2.
        rs_comp-ui_type  = 'number'.

      WHEN cl_abap_typedescr=>typekind_num OR cl_abap_typedescr=>typekind_numeric.
        rs_comp-ui_type  = 'number'.

        " Double
      WHEN cl_abap_typedescr=>typekind_packed OR cl_abap_typedescr=>typekind_float OR
           '/' OR 'a' OR 'e'. " cl_abap_typedescr=>typekind_decfloat  OR cl_abap_typedescr=>typekind_decfloat16 OR cl_abap_typedescr=>typekind_decfloat34.
        rs_comp-ui_type  = 'number'.

      WHEN OTHERS.
        CASE lo_type->absolute_name.
          WHEN '\TYPE=XSDBOOLEAN'.
            rs_comp-ui_type = 'boolean'.

          WHEN '\TYPE=XSDDATETIME_Z' OR '\TYPE=XSDDATETIME_LONG_Z' OR
               '\TYPE=XSDDATETIME_OFFSET' OR '\TYPE=XSDDATETIME_LOCAL' OR '\TYPE=XSDDATETIME_LOCAL_DT'.
            rs_comp-ui_type = 'datetime'.
        ENDCASE.
    ENDCASE.
  ELSE.
    " Is table
    rs_comp-kind         = zcl_aqo_util=>mc_kind_table.
    lr_table_descr ?= lo_type.

    rs_comp-table_kind   = lr_table_descr->table_kind.
    rs_comp-unique       = lr_table_descr->has_unique_key.
    rs_comp-key          = lr_table_descr->key.
    rs_comp-key_defkind  = lr_table_descr->key_defkind.

    " No need for standardc table
    IF rs_comp-table_kind = cl_abap_tabledescr=>tablekind_std.
      CLEAR rs_comp-key.
    ENDIF.

    TRY.
        lr_struct_descr ?= lr_table_descr->get_table_line_type( ).
      CATCH cx_sy_move_cast_error.
        " TODO ZCX_ERROR
        MESSAGE e008(zaqo_mes) WITH rs_comp-name RAISING is_not_structure.
    ENDTRY.

    " For speed creation
    IF lr_struct_descr->is_ddic_type( ) = abap_true.
      ls_header = lr_struct_descr->get_ddic_header( ).
      rs_comp-rollname     = ls_header-tabname.
    ENDIF.

    " Create STANDARD table for field catalog!
    CREATE DATA lr_row TYPE HANDLE lr_struct_descr.
    ASSIGN lr_row->* TO <ls_row>.

    CLEAR:
     rs_comp-subcomps,
     lt_subcomp.
    LOOP AT lr_struct_descr->components ASSIGNING <ls_comp_tab>.
      APPEND INITIAL LINE TO lt_subcomp ASSIGNING <ls_subfield>.
      ASSIGN COMPONENT <ls_comp_tab>-name OF STRUCTURE <ls_row> TO <lv_subvalue>.

      " Recursion
      <ls_subfield> = construct_new_comp( iv_field_name = <ls_comp_tab>-name
                                          im_data       = <lv_subvalue> ).
    ENDLOOP.

    rs_comp-subcomps = zcl_aqo_util=>to_json( im_data = lt_subcomp ).

    " Select option ?
    DO 1 TIMES.
      lv_cnt = LINES( lt_subcomp ).

      CHECK lv_cnt = 4.
      " TODO Check length ?
      LOOP AT lt_subcomp TRANSPORTING NO FIELDS WHERE
         name = 'SIGN' OR name = 'OPTION' OR name = 'LOW' OR name = 'HIGH'.
        lv_cnt = lv_cnt - 1.
      ENDLOOP.

      CHECK lv_cnt = 0.
      rs_comp-kind   = zcl_aqo_util=>mc_kind_select_option.

      " No need in components
      CLEAR rs_comp-subcomps.

      UNASSIGN <ls_subfield>.

      " Where to find TABLE-FIELDNAME
      READ TABLE lt_subcomp ASSIGNING <ls_subfield>
       WITH KEY name = 'LOW'.
      rs_comp-rollname = <ls_subfield>-rollname.
    ENDDO.
  ENDIF.

  " Try to find TABLE-FIELDNAME
  DO 1 TIMES.
    CHECK   rs_comp-kind = zcl_aqo_util=>mc_kind_select_option OR
            rs_comp-kind = zcl_aqo_util=>mc_kind_parameter.
    zcl_aqo_util=>find_table_fieldname(
     EXPORTING
      iv_name        = rs_comp-name
     CHANGING
      cv_rollname    = rs_comp-rollname
      cv_text        = rs_comp-text
      ct_unique      = mt_unq_name ).

    IF rs_comp-rollname CP '*-*'.
      lv_name = rs_comp-rollname.
      INSERT lv_name INTO TABLE mt_unq_name.
    ENDIF.
  ENDDO.

*  IF rs_comp-kind <> zcl_aqo_util=>mc_kind_table AND
*     rs_comp-rollname IS INITIAL.
*    MESSAGE e009(zaqo_mes) WITH rs_comp-name RAISING cannot_detect_type.
*  ENDIF.
ENDMETHOD.


METHOD delete.
  DATA:
    lv_answer TYPE char1.

  " Own dialogs
  IF iv_confirm = abap_true.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = TEXT-del
        text_question         = TEXT-irr
        icon_button_1         = 'ICON_OKAY'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        OTHERS                = 1.
    CHECK sy-subrc = 0.
    " Cancelled
    IF lv_answer <> '1'.
      MESSAGE s130(ed) WITH TEXT-del DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDIF.

  IF zcl_aqo_util=>is_dev_mandt( ) <> abap_true.
    MESSAGE s011(zaqo_mes) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF transport( ) = abap_true.
    DELETE
    FROM ztaqo_data
     WHERE relid     = '00'
       AND object    = ms_key-object
       AND subobject = ms_key-subobject.
    MESSAGE s010(zaqo_mes) WITH ms_key-object ms_key-subobject.

    rv_ok = abap_true.
  ENDIF.
ENDMETHOD.


METHOD GET_FIELD_DATA.
  DATA:
    lv_name   TYPE string.
  FIELD-SYMBOLS:
    <ls_data> TYPE any,
    <lv_any>  TYPE any.

  " Get data
  IF mo_data IS NOT INITIAL.
    CONCATENATE 'MO_DATA->' iv_name INTO lv_name.
    ASSIGN (lv_name) TO <lv_any>.
  ELSE.
    ASSIGN mr_data->* TO <ls_data>.
    ASSIGN COMPONENT iv_name OF STRUCTURE <ls_data> TO <lv_any>.
  ENDIF.

  " Return it
  GET REFERENCE OF <lv_any> INTO rr_data.
ENDMETHOD.


METHOD lock.

  " Locks
  IF iv_unlock = abap_true.
    CALL FUNCTION 'DEQUEUE_EZTAQO_DATA'
      EXPORTING
        relid          = '00'
        object         = ms_key-object
        subobject      = ms_key-subobject
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
  ELSE.
    CALL FUNCTION 'ENQUEUE_EZTAQO_DATA'
      EXPORTING
        relid          = '00'
        object         = ms_key-object
        subobject      = ms_key-subobject
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
  ENDIF.
  CHECK sy-subrc = 0.

  " Ok locked
  rv_ok = abap_true.
ENDMETHOD.


METHOD read.
  DATA:
    lv_field         TYPE REF TO fieldname,
    lr_data          TYPE REF TO data,
    lr_table         TYPE REF TO data,
    lo_type          TYPE REF TO cl_abap_datadescr,
    lv_ok            TYPE abap_bool,
    ls_new_field_opt TYPE zcl_aqo_util=>ts_field_opt,
    ls_comp          TYPE zcl_aqo_util=>ts_comp.
  FIELD-SYMBOLS:
    <lv_value>     TYPE ANY,
    <lt_value>     TYPE STANDARD TABLE,
    <lt_any_tab>   TYPE ANY TABLE,
    <ls_value>     TYPE ANY,
    <ls_low>       TYPE ANY,
    <ls_field_opt> LIKE LINE OF mt_field_opt.


  LOOP AT mt_all_field REFERENCE INTO lv_field.
    " Read option
    READ TABLE mt_field_opt ASSIGNING <ls_field_opt>
     WITH KEY name = lv_field->*.
    CHECK sy-subrc = 0.

    " Destination
    lr_data = get_field_data( lv_field->* ).
    ASSIGN lr_data->* TO <lv_value>.
    CHECK sy-subrc = 0.

    " For tables only
    IF iv_safe_read = abap_true AND <ls_field_opt>-kind = zcl_aqo_util=>mc_kind_table.
      ls_comp            = <ls_field_opt>-comp.

      " Create standard table
      ls_comp-table_kind = cl_abap_tabledescr=>tablekind_std.
      CLEAR ls_comp-unique.

      " Assign it
      lo_type = zcl_aqo_util=>create_type_descr( is_comp = ls_comp ).
      CREATE DATA lr_table TYPE HANDLE lo_type.
      ASSIGN:
        lr_data->*  TO <lt_any_tab>,
        lr_table->* TO <lt_value>,
        lr_table->* TO <lv_value>.
    ENDIF.

    " №1
    zcl_aqo_util=>from_json(
     EXPORTING
       iv_json = <ls_field_opt>-value
     IMPORTING
       ex_data = <lv_value>
       ev_ok   = lv_ok ).

    IF iv_safe_read = abap_true AND <ls_field_opt>-kind = zcl_aqo_util=>mc_kind_table.
      " Prev ref
      ASSIGN lr_data->* TO <lv_value>.

      CLEAR <lt_any_tab>.
      LOOP AT <lt_value> ASSIGNING <ls_value>.
        INSERT <ls_value> INTO TABLE <lt_any_tab>.
      ENDLOOP.

      IF LINES( <lt_value> ) <> LINES( <lt_any_tab> ).
        lv_ok = abap_false.
      ENDIF.
    ENDIF.

    " Cannot read options
    CHECK lv_ok <> abap_true.
    APPEND lv_field->* TO rt_empty_field.

    " New field
    ls_new_field_opt-comp = construct_new_comp( iv_field_name = lv_field->*
                                                im_data       = <lv_value> ).

    " Only if KIND was changed
    CHECK <ls_field_opt>-rollname = ls_new_field_opt-rollname.

    " P -> S
    IF <ls_field_opt>-kind = zcl_aqo_util=>mc_kind_parameter AND
     ls_new_field_opt-kind = zcl_aqo_util=>mc_kind_select_option.
      ASSIGN <lv_value> TO <lt_value>.
      APPEND INITIAL LINE TO <lt_value> ASSIGNING <ls_value>.
      <ls_value> = 'IEQ'.

      " Second attempt
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_value>  TO <ls_low>.

      " №2
      zcl_aqo_util=>from_json(
       EXPORTING
         iv_json = <ls_field_opt>-value
       IMPORTING
         ex_data = <ls_low>
         ev_ok   = lv_ok ).
      IF lv_ok <> abap_true.
        CLEAR <lt_value>.
      ENDIF.

      " S -> P
    ELSEIF <ls_field_opt>-kind = zcl_aqo_util=>mc_kind_select_option AND
         ls_new_field_opt-kind = zcl_aqo_util=>mc_kind_parameter.
      " Create OLD range table
      lo_type = zcl_aqo_util=>create_type_descr( is_comp = <ls_field_opt>-comp ).
      CREATE DATA lr_data TYPE HANDLE lo_type.
      ASSIGN lr_data->* TO <lt_value>.

      DO 1 TIMES.
        " №2
        zcl_aqo_util=>from_json(
         EXPORTING
           iv_json = <ls_field_opt>-value
         IMPORTING
           ex_data = <lt_value>
           ev_ok   = lv_ok ).
        CHECK lv_ok = abap_true.

        " First row of range
        READ TABLE <lt_value> ASSIGNING <ls_value> INDEX 1.
        CHECK sy-subrc = 0.

        " Only one value
        ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_value> TO <ls_low>.
        CHECK sy-subrc = 0.

        <lv_value> = <ls_low>.
      ENDDO.

    ENDIF.

    " New type
    <ls_field_opt>-kind = ls_new_field_opt-kind.
  ENDLOOP.
ENDMETHOD.


METHOD save.
  DATA:
    lv_field   TYPE REF TO fieldname,
    lr_data    TYPE REF TO data,
    lv_answer  TYPE char1,
    lv_text    TYPE text255,
    lv_xml     TYPE string,
    lv_tabix   TYPE sytabix.
  FIELD-SYMBOLS:
    <lv_field>     TYPE any,
    <ls_field_opt> LIKE LINE OF mt_field_opt.

  " Own dialogs
  IF iv_confirm = abap_true.
    " Overrite message
    MESSAGE s019(zaqo_mes) WITH ms_cluster-object ms_cluster-subobject INTO lv_text.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = TEXT-sav
        text_question         = lv_text
        icon_button_1         = 'ICON_OKAY'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        OTHERS                = 1.
    CHECK sy-subrc = 0.

    " Cancelled
    IF lv_answer <> '1'.
      MESSAGE s130(ed) WITH TEXT-sav DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDIF.

  " Mark or delete old items
  LOOP AT mt_field_opt ASSIGNING <ls_field_opt>.
    lv_tabix = sy-tabix.

    " Is ok
    <ls_field_opt>-is_old = abap_false.

    READ TABLE mt_all_field TRANSPORTING NO FIELDS
     WITH KEY table_line = <ls_field_opt>-name.
    CHECK sy-subrc <> 0.

    " Check
    IF iv_delete_old = abap_true.
      DELETE mt_field_opt INDEX lv_tabix.
    ELSE.
      " Obsolete
      <ls_field_opt>-is_old = abap_true.
    ENDIF.
  ENDLOOP.

  " Data already set
  IF iv_use_me_data = abap_true.
    LOOP AT mt_all_field REFERENCE INTO lv_field.
      " Read option
      READ TABLE mt_field_opt ASSIGNING <ls_field_opt>
       WITH KEY name = lv_field->*.
      CHECK sy-subrc = 0.

      " Source
      lr_data = get_field_data( lv_field->* ).
      ASSIGN lr_data->* TO <lv_field>.
      CHECK sy-subrc = 0.

      " Leave only text
      <ls_field_opt>-value = zcl_aqo_util=>to_json( im_data = <lv_field> ).
    ENDLOOP.
  ENDIF.

  " Technical info
  ms_cluster-udate = sy-datum.
  ms_cluster-utime = sy-uzeit.
  ms_cluster-uname = sy-uname.

  " First transform
  CALL TRANSFORMATION id
   SOURCE field_opt = mt_field_opt
   RESULT XML lv_xml.

  " Save data
  EXPORT xml       = lv_xml
         "ui_ext    = mv_ui_ext_class
         last_call = ms_last_call TO DATABASE ztaqo_data(00)
    FROM ms_cluster CLIENT iv_mandt ID ms_key COMPRESSION ON.

  " Show info
  IF iv_message = abap_true.
    CONCATENATE ms_cluster-object ` - ` ms_cluster-subobject ` MANDT = ` iv_mandt INTO lv_text.
    MESSAGE s516(ed) WITH lv_text.
  ENDIF.
ENDMETHOD.


METHOD TRANSPORT.
  DATA : lt_e071  TYPE STANDARD TABLE OF e071,
         lt_e071k TYPE STANDARD TABLE OF e071k,
         ls_e071  TYPE e071,
         ls_e071k TYPE e071k,
         lv_task  TYPE e070-trkorr.

  " select request/task
  CALL FUNCTION 'TR_ORDER_CHOICE_CORRECTION'
    EXPORTING
      iv_category = 'SYST'                                "#EC NOTEXT
    IMPORTING
      ev_task     = lv_task
    EXCEPTIONS
      OTHERS      = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
    RETURN.
  ENDIF.

**   check unsaved data exist
*  IF check_unsaved_data( ) EQ abap_true.
**     save data
*    data_save( ).
*  ENDIF.

  ls_e071-pgmid       = 'R3TR'.
  ls_e071-object      = 'TABU'.
  ls_e071-obj_name    = 'ZTAQO_DATA'.
  ls_e071-objfunc     = 'K'.
  APPEND ls_e071 TO lt_e071.

  ls_e071k-pgmid         = 'R3TR'.
  ls_e071k-object        = 'TABU'.
  ls_e071k-objname       = 'ZTAQO_DATA'.
  ls_e071k-mastertype    = 'TABU'.
  ls_e071k-mastername    = 'ZTAQO_DATA'.
  ls_e071k-tabkey+0(3)   = sy-mandt.
  ls_e071k-tabkey+3(2)   = '00'.
  ls_e071k-tabkey+5(40)  = ms_key-object.
  ls_e071k-tabkey+45(30) = ms_key-subobject.
  ls_e071k-tabkey+75(1)  = '*'.
  APPEND ls_e071k TO lt_e071k.

* include data to request
  CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
    EXPORTING
      wi_trkorr = lv_task
    TABLES
      wt_e071   = lt_e071
      wt_e071k  = lt_e071k
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc = 0.
    MESSAGE s530(ed) WITH ms_key-object ms_key-subobject.
    rv_ok = abap_true.
  ELSE.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
  ENDIF.
ENDMETHOD.
ENDCLASS.
