class ZCL_AQO_OPTION definition
  public
  create protected .

public section.
*"* public components of class ZCL_AQO_OPTION
*"* do not include other source files here!!!
  type-pools ABAP .

  data MS_DB_ITEM type ZTAQO_OPTION read-only .

  class-methods CREATE
    importing
      !IV_PACKAGE_ID type CSEQUENCE optional
      !IV_OPTION_ID type CSEQUENCE default ZCL_AQO_HELPER=>MC_PROG-DEFAULT_ID
      !IR_DATA type ref to DATA optional
      !IO_DATA type ref to OBJECT optional
    preferred parameter IO_DATA
    returning
      value(RO_OPT) type ref to ZCL_AQO_OPTION .
  methods GET_FIELD_VALUE
    importing
      !IV_NAME type CSEQUENCE
    returning
      value(RR_DATA) type ref to DATA .
protected section.

*"* protected components of class ZCL_AQO_OPTION
*"* do not include other source files here!!!
  data MT_FIELD_VALUE type ZCL_AQO_HELPER=>TT_FIELD_VALUE .

  methods DELETE
    returning
      value(RV_INFO) type STRING .
  methods LOCK
    importing
      !IV_UNLOCK type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_OK) type ABAP_BOOL .
  methods SAVE
    importing
      !IS_DB type ANY
      !IV_CONFIRM type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_INFO) type STRING .
  methods TRANSPORT
    returning
      value(RV_INFO) type STRING .
  methods ADD_HISTORY_VALUE
    importing
      !IV_VALUE type STRING
    changing
      !CS_FIELD_VALUE type ZCL_AQO_HELPER=>TS_FIELD_VALUE .
  methods UPDATE
    importing
      !IS_ITEM type ANY
    returning
      value(RV_OK) type ABAP_BOOL .
private section.

  types:
    BEGIN OF ts_change,
      icon  TYPE icon_d,
      field TYPE fieldname,
      desc  TYPE zdaqo_description,
    END OF ts_change .
  types:
    tt_change TYPE STANDARD TABLE OF ts_change WITH DEFAULT KEY .

  data MT_CHANGE type TT_CHANGE .

  methods _GET_ABAP_VALUE
    importing
      !IR_DATA type ref to DATA
      !IO_DATA type ref to OBJECT
      !IV_NAME type CSEQUENCE
    returning
      value(RR_DATA) type ref to DATA .
  methods _CHECK_ABAP_DECLARATION
    importing
      !IR_DATA type ref to DATA
      !IO_DATA type ref to OBJECT
    exporting
      !ET_DECLARED_FIELD type ZCL_AQO_HELPER=>ABAP_ATTRNAME_TAB .
  methods _COMPARE_FIELDS
    importing
      !IR_DATA type ref to DATA
      !IO_DATA type ref to OBJECT
    changing
      !CT_DECLARED type ZCL_AQO_HELPER=>ABAP_ATTRNAME_TAB .
  methods _COMPARE_FIELD
    importing
      !IV_PREFIX type STRING
      !IS_NEW type ZCL_EUI_TYPE=>TS_FIELD_DESC
      !IR_OLD type ref to ZCL_EUI_TYPE=>TS_FIELD_DESC
    changing
      !CV_CHANGED type ABAP_BOOL optional .
  methods _ADD_NEW_FIELDS
    importing
      !IR_DATA type ref to DATA
      !IO_DATA type ref to OBJECT
    changing
      !CT_DECLARED type ZCL_AQO_HELPER=>ABAP_ATTRNAME_TAB .
  methods _READ_VALUES
    importing
      !IR_DATA type ref to DATA
      !IO_DATA type ref to OBJECT .
  methods _ADD_CHANGE
    importing
      !IV_NAME type CSEQUENCE
      !IV_PREFIX type CSEQUENCE
      !IV_NEW type ABAP_BOOL .
  methods _CHECK_BEFORE_SAVE .
  methods _SET_DB_DEFAULTS
    importing
      !IS_DB type ANY
    changing
      !CS_DB_ITEM type ZTAQO_OPTION .
  methods _CONFIRM_POPUP
    returning
      value(RV_OK) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_AQO_OPTION IMPLEMENTATION.


METHOD add_history_value.
  DATA:
    ls_history_value TYPE zcl_aqo_helper=>ts_history_value,
    ls_history_prev  TYPE REF TO zcl_aqo_helper=>ts_history_value,
    lv_last_index    TYPE i.

  " Leave only text
  ls_history_value-h_value = iv_value.
  ls_history_value-changed = sy-datum.
  ls_history_value-login   = sy-uname.

  " Previous
  lv_last_index = lines( cs_field_value-value ).
  READ TABLE cs_field_value-value REFERENCE INTO ls_history_prev INDEX lv_last_index.

  " Compare with prev value
  IF sy-subrc <> 0 OR ls_history_value-h_value <> ls_history_prev->h_value.
    " Today ?
    IF sy-subrc = 0 AND ls_history_value-changed = ls_history_prev->changed.
      DELETE cs_field_value-value INDEX lv_last_index.
    ENDIF.

    " Insert new one
    INSERT ls_history_value INTO TABLE cs_field_value-value.
  ENDIF.

  " Delete obselete data
  lv_last_index = lines( cs_field_value-value ).
  CHECK lv_last_index > 1.
  lv_last_index = lv_last_index - ms_db_item-prev_value_cnt.
  DO lv_last_index TIMES.
    DELETE cs_field_value-value INDEX 1.
  ENDDO.
ENDMETHOD.


METHOD create.
  DATA lv_is_class TYPE abap_bool.
  DO 1 TIMES.
    CHECK io_data IS BOUND.

    DATA lo_class TYPE REF TO cl_abap_objectdescr.
    lo_class ?= cl_abap_objectdescr=>describe_by_object_ref( io_data ).

    CHECK lo_class->absolute_name cp '\CLASS*'.
    lv_is_class = abap_true.
  ENDDO.

  DATA ls_db_key    TYPE zcl_aqo_helper=>ts_db_key.
  ls_db_key-package_id = iv_package_id.
  ls_db_key-option_id  = iv_option_id.

  DATA ls_last_call TYPE abap_callstack_line.
  lcl_helper=>create(
   EXPORTING
    iv_is_class   = lv_is_class
   IMPORTING
    eo_opt        = ro_opt
    es_last_call  = ls_last_call
   CHANGING
    cs_db_key     = ls_db_key ).

  IF ro_opt->ms_db_item IS NOT INITIAL. " AND ms_db_item-fields IS NOT INITIAL.
    TRY.
        CALL TRANSFORMATION id
         SOURCE XML ro_opt->ms_db_item-fields
         RESULT field_opt = ro_opt->mt_field_value.

        DATA lo_transform TYPE REF TO cx_transformation_error.
      CATCH cx_xslt_runtime_error INTO lo_transform.
        zcx_aqo_exception=>raise_sys_error( io_error = lo_transform ).
    ENDTRY.
  ELSE.
    lcl_helper=>check_package_exist( ls_db_key-package_id ).
  ENDIF.

  " Key is always valid
  MOVE-CORRESPONDING ls_db_key TO ro_opt->ms_db_item.

**********************************************************************
  " read current values and check them
  DATA lt_declared_field TYPE zcl_aqo_helper=>abap_attrname_tab.
  ro_opt->_check_abap_declaration(
    EXPORTING io_data           = io_data
              ir_data           = ir_data
    IMPORTING et_declared_field = lt_declared_field ).

  ro_opt->_read_values( io_data = io_data
                        ir_data = ir_data ).

**********************************************************************
  " Call save for user
  CHECK ls_last_call-mainprogram <> zcl_aqo_helper=>mc_prog-editor.

  IF lt_declared_field IS INITIAL AND ro_opt->mt_change[] IS INITIAL.
    IF zcl_aqo_helper=>is_in_editor( ) <> abap_true.
      zcl_aqo_helper=>add_menu( ro_opt->ms_db_item ).
    ENDIF.

    IF ro_opt->ms_db_item-last_call <> ls_last_call.
      ro_opt->update( ls_last_call  ).
    ENDIF.

    RETURN.
  ENDIF.

  " Or something like that SY-SYSID <> 'DEV'
  IF zcl_aqo_helper=>is_dev_mandt( ) <> abap_true.
    MESSAGE s006(zaqo_message) WITH ls_db_key-package_id
                                    ls_db_key-option_id.
    zcx_aqo_exception=>raise_sys_error( ).
  ENDIF.

  DATA lv_message TYPE string.
  lv_message = ro_opt->save( is_db = ls_last_call ).
  IF lv_message IS NOT INITIAL.
    MESSAGE lv_message TYPE 'S'.
  ENDIF.
ENDMETHOD.


METHOD delete.
  IF zcl_aqo_helper=>is_dev_mandt( ) <> abap_true.
    MESSAGE s011(zaqo_message) INTO sy-msgli.
    zcx_aqo_exception=>raise_sys_error( ).
  ENDIF.

  " Put to request
  IF ms_db_item-package_id NP '$*'.
    rv_info = transport( ).
    " TODO delete UI5 version
    CHECK sy-msgid = 'ZAQO_MESSAGE' AND sy-msgno = 023.  "#EC NUMBER_OK
  ENDIF.

  DELETE
  FROM ztaqo_option
   WHERE package_id = ms_db_item-package_id
     AND option_id  = ms_db_item-option_id.

  " Show info
  DATA lv_text TYPE string.
  MESSAGE s010(zaqo_message) WITH ms_db_item-package_id ms_db_item-option_id INTO lv_text.
  CONCATENATE lv_text ` ` rv_info INTO rv_info.
ENDMETHOD.


METHOD get_field_value.
  DATA:
    lv_last_index   TYPE i,
    ls_history_prev TYPE REF TO zcl_aqo_helper=>ts_history_value,
    lo_type         TYPE REF TO cl_abap_datadescr,
    lr_value        TYPE REF TO data,
    lv_ok           TYPE abap_bool,
    lo_error        TYPE REF TO zcx_eui_exception.
  FIELD-SYMBOLS:
    <ls_field_value> LIKE LINE OF mt_field_value,
    <lv_value>       TYPE any.

  " Read current
  READ TABLE mt_field_value ASSIGNING <ls_field_value>
   WITH TABLE KEY name = iv_name.
  IF sy-subrc <> 0.
    MESSAGE s030(zaqo_message) WITH iv_name INTO sy-msgli.
    zcx_aqo_exception=>raise_sys_error( ).
  ENDIF.

  " Last one
  lv_last_index = lines( <ls_field_value>-value ).
  READ TABLE <ls_field_value>-value REFERENCE INTO ls_history_prev INDEX lv_last_index.
  IF sy-subrc <> 0.
    MESSAGE s031(zaqo_message) WITH iv_name INTO sy-msgli.
    zcx_aqo_exception=>raise_sys_error( ).
  ENDIF.

  " Create type
  TRY.
      lo_type = zcl_eui_type=>create_type_descr( is_field_desc = <ls_field_value>-field_desc ).
    CATCH zcx_eui_exception INTO lo_error.
      zcx_aqo_exception=>raise_sys_error( io_error = lo_error ).
  ENDTRY.
  CREATE DATA lr_value TYPE HANDLE lo_type.
  ASSIGN lr_value->* TO <lv_value>.

  " Prevent dump for tables
  DATA lv_mode TYPE string VALUE zcl_eui_conv=>mc_json_mode-standard.
  IF zcl_aqo_helper=>is_in_editor( ) = abap_true.
    lv_mode = zcl_eui_conv=>mc_json_mode-safe.
  ENDIF.

  " Convert from JSON
  zcl_eui_conv=>from_json(
   EXPORTING
     iv_json = ls_history_prev->h_value
     iv_mode = lv_mode
   IMPORTING
     ex_data = <lv_value>
     ev_ok   = lv_ok ).
  IF lv_ok <> abap_true.
    MESSAGE s033(zaqo_message) WITH iv_name INTO sy-msgli.
    zcx_aqo_exception=>raise_sys_error( ).
  ENDIF.

  " Return it
  GET REFERENCE OF <lv_value> INTO rr_data.
ENDMETHOD.


METHOD lock.
  " Locks
  IF iv_unlock = abap_true.
    CALL FUNCTION 'DEQUEUE_EZTAQO_OPTION'
      EXPORTING
        package_id = ms_db_item-package_id
        option_id  = ms_db_item-option_id
        _scope     = '1'
      EXCEPTIONS
        OTHERS     = 3.
  ELSE.
    CALL FUNCTION 'ENQUEUE_EZTAQO_OPTION'
      EXPORTING
        package_id     = ms_db_item-package_id
        option_id      = ms_db_item-option_id
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
  ENDIF.

  " Show message in caller
  CHECK sy-subrc = 0.

  " Ok locked
  rv_ok = abap_true.
ENDMETHOD.


METHOD save.
  " Is dev ?
  DATA lv_is_dev TYPE abap_bool.
  lv_is_dev = zcl_aqo_helper=>is_dev_mandt( ).

  " Own dialogs (iv_confirm = abap_true)
  DO 1 TIMES.
    CHECK zcl_aqo_helper=>is_in_editor( iv_is_sapui5 = abap_true ) <> abap_true
      AND iv_confirm = abap_true.

    CHECK _confirm_popup( ) <> abap_true.

    MESSAGE s130(ed) WITH 'Save'(sav) DISPLAY LIKE 'E'.
    zcx_aqo_exception=>raise_sys_error( ).
  ENDDO.

  _check_before_save( ).

  " Always put in request in DEV
  rv_info = transport( ).

  _set_db_defaults(
   EXPORTING is_db      = is_db
   CHANGING  cs_db_item = ms_db_item ).

  " First transform (Data already set to mt_field_value)
  CALL TRANSFORMATION id
   SOURCE field_opt = mt_field_value
   RESULT XML ms_db_item-fields.

  " Save data
  MODIFY ztaqo_option FROM ms_db_item.
  COMMIT WORK AND WAIT.

  " Add previous messages text
  IF zcl_aqo_helper=>is_in_editor( ) = abap_true AND lv_is_dev <> abap_true.
    CONCATENATE 'The option was saved. Please copy or export it to DEV system!'(ms1) rv_info INTO rv_info SEPARATED BY space.
  ELSE.
    DATA lv_text TYPE string.
    CONCATENATE ms_db_item-package_id ` - ` ms_db_item-option_id INTO lv_text.
    MESSAGE s516(ed) WITH lv_text INTO lv_text.
    CONCATENATE lv_text `! ` rv_info INTO rv_info.
  ENDIF.
ENDMETHOD.


METHOD transport.
  " No need to transport
  IF ms_db_item-package_id CP '$*'.
    rv_info = 'No need to transport temporary options'(ms2).
    RETURN.
  ENDIF.

  " Already supplied by UI5 interface ?
  DATA lv_task TYPE e070-trkorr.
*  lv_task = iv_task.

  zcl_aqo_helper=>find_request( EXPORTING iv_table_name = 'ZTAQO_OPTION'
                                          iv_key1       = ms_db_item-package_id
                                          iv_key2       = ms_db_item-option_id
                                CHANGING  cv_task       = lv_task
                                          cv_ok_message = rv_info ).
ENDMETHOD.


METHOD update.
  MOVE-CORRESPONDING is_item TO me->ms_db_item.
  DATA lo_struc TYPE REF TO cl_abap_structdescr.
  lo_struc ?= cl_abap_structdescr=>describe_by_data( is_item ).

  DATA lv_set TYPE string.
  FIELD-SYMBOLS <ls_comp> LIKE LINE OF lo_struc->components.
  LOOP AT lo_struc->components ASSIGNING <ls_comp>.
    CONCATENATE lv_set ` ` <ls_comp>-name ` = ms_db_item-` "#EC NOTEXT
                           <ls_comp>-name INTO lv_set.
  ENDLOOP.

  UPDATE ztaqo_option SET (lv_set)
  WHERE package_id = ms_db_item-package_id
    AND option_id  = ms_db_item-option_id.

  CHECK sy-subrc = 0.
  rv_ok = abap_true.
ENDMETHOD.


METHOD _add_change.
  FIELD-SYMBOLS <ls_change> LIKE LINE OF mt_change.

  APPEND INITIAL LINE TO mt_change ASSIGNING <ls_change>.
  IF iv_prefix IS INITIAL.
    <ls_change>-field = iv_name.
  ELSE.
    CONCATENATE iv_prefix '-' iv_name INTO <ls_change>-field.
  ENDIF.

  CASE iv_new.
    WHEN abap_true.
      <ls_change>-icon  = icon_insert_row.
      <ls_change>-desc  = '+ Inserted'(din).
    WHEN abap_false.
      <ls_change>-icon  = icon_delete_row.
      <ls_change>-desc  = '- Deleted'(dld).
    WHEN abap_undefined.
      <ls_change>-icon  = icon_alarm.
      <ls_change>-desc  = '# Declaradion is changed'(dch).
  ENDCASE.
ENDMETHOD.


METHOD _add_new_fields.
  " OK - add description one by one (have something new in ABAP code)
  " IF lines( mt_field_value ) < lines( lt_declared_field )
  DATA lr_unique_type TYPE REF TO zcl_eui_type=>tt_unique_type.
  CREATE DATA lr_unique_type.

  DATA lr_new_field TYPE REF TO abap_attrname.
  LOOP AT ct_declared REFERENCE INTO lr_new_field.
    _add_change( iv_name   = lr_new_field->*
                 iv_new    = abap_true
                 iv_prefix = '' ).

    " Get from declaration
    DATA lr_data TYPE REF TO data.
    lr_data = _get_abap_value(
       io_data = io_data
       ir_data = ir_data
       iv_name = lr_new_field->* ).

    FIELD-SYMBOLS <lv_value> TYPE any.
    ASSIGN lr_data->* TO <lv_value>.

    DATA lv_value TYPE string.
    lv_value = zcl_eui_conv=>to_json( <lv_value> ).

    " Field cescription
    TRY.
        DATA ls_field_value TYPE zcl_aqo_helper=>ts_field_value.
        ls_field_value-field_desc = zcl_eui_type=>get_field_desc(
          iv_field_name  = lr_new_field->*
          iv_data        = <lv_value>
          ir_unique_type = lr_unique_type ).

        DATA lo_error TYPE REF TO zcx_eui_exception.
      CATCH zcx_eui_exception INTO lo_error.
        zcx_aqo_exception=>raise_sys_error( io_error = lo_error ).
    ENDTRY.

    " Add to history
    add_history_value(
     EXPORTING
       iv_value       = lv_value
     CHANGING
       cs_field_value = ls_field_value ).

    " And finally add new field option
    INSERT ls_field_value INTO TABLE mt_field_value.
  ENDLOOP.
ENDMETHOD.


METHOD _check_abap_declaration.
  " No error in editor
  CLEAR et_declared_field.

  " №1 Based on class
  IF io_data IS NOT INITIAL.
    et_declared_field = lcl_helper=>get_class_fields( io_data ).
  ENDIF.

  " №2 Based on structure
  IF ir_data IS NOT INITIAL.
    et_declared_field = lcl_helper=>get_struc_fields( ir_data ).
  ENDIF.

  _compare_fields(
   EXPORTING ir_data     = ir_data
             io_data     = io_data
   CHANGING  ct_declared = et_declared_field ).

  _add_new_fields(
   EXPORTING ir_data     = ir_data
             io_data     = io_data
   CHANGING  ct_declared = et_declared_field ).
ENDMETHOD.


METHOD _check_before_save.
  " Class or program
  DATA lv_is_class   TYPE abap_bool.
  DATA lv_program    TYPE string.

  zcl_aqo_helper=>get_last_call_info(
   EXPORTING is_last_call = ms_db_item-last_call
   IMPORTING ev_name      = lv_program
             ev_is_class  = lv_is_class ).

  DATA lv_in_editor TYPE abap_bool.
  lv_in_editor = zcl_aqo_helper=>is_in_editor( ).

  DATA lv_error_text TYPE text255.
  TRY.
      IF lv_is_class <> abap_true.
        PERFORM before_option_save IN PROGRAM (lv_program) IF FOUND
         USING
           me             " IO_OPTION
           lv_in_editor   " IV_IN_EDITOR
         CHANGING
           lv_error_text. " CV_ERROR_TEXT.
      ELSE.
        CALL METHOD (lv_program)=>('ZIF_AQO_EXT~BEFORE_OPTION_SAVE')
          EXPORTING
            io_option     = me
            iv_in_editor  = lv_in_editor
          CHANGING
            cv_error_text = lv_error_text.
      ENDIF.
    CATCH cx_root.
      CLEAR lv_error_text.
  ENDTRY.

  " Show an error
  IF lv_error_text IS NOT INITIAL.
    zcx_aqo_exception=>raise_sys_error( iv_message = lv_error_text ).
  ENDIF.
ENDMETHOD.


METHOD _compare_field.
  DATA:
    lt_old     TYPE zcl_eui_type=>tt_field_desc,
    lv_old_ok  TYPE abap_bool,
    lt_new     TYPE zcl_eui_type=>tt_field_desc,
    lv_new_ok  TYPE abap_bool,
    ls_old     TYPE REF TO zcl_eui_type=>ts_field_desc,
    ls_new     TYPE REF TO zcl_eui_type=>ts_field_desc,
    lv_changed TYPE abap_bool.

  DATA lv_prefix TYPE string.
  IF iv_prefix IS INITIAL.
    lv_prefix = ir_old->name.
  ELSE.
    CONCATENATE iv_prefix `-` ir_old->name INTO lv_prefix.
  ENDIF.

  " ERROR - Parameter has now become Table or Range ?
  IF is_new-sys_type <> ir_old->sys_type OR
     is_new-ui_type  <> ir_old->ui_type.
    MESSAGE s028(zaqo_message) WITH lv_prefix.
    zcx_aqo_exception=>raise_sys_error( ).
  ENDIF.

  " Check sub fields
  IF ir_old->sub_fdesc IS NOT INITIAL.
    zcl_eui_conv=>from_json(
     EXPORTING
       iv_json = ir_old->sub_fdesc
     IMPORTING
       ex_data = lt_old
       ev_ok   = lv_old_ok ).

    zcl_eui_conv=>from_json(
     EXPORTING
       iv_json = is_new-sub_fdesc
     IMPORTING
       ex_data = lt_new
       ev_ok   = lv_new_ok ).

    " Json format error
    IF lv_old_ok <> abap_true OR lv_new_ok <> abap_true.
      MESSAGE s028(zaqo_message) WITH lv_prefix.
      zcx_aqo_exception=>raise_sys_error( ).
    ENDIF.

    LOOP AT lt_old REFERENCE INTO ls_old.
      " Field deleted
      READ TABLE lt_new REFERENCE INTO ls_new
       WITH TABLE KEY name = ls_old->name.
      IF sy-subrc <> 0.
        _add_change( iv_name   = ls_old->name
                     iv_new    = abap_false
                     iv_prefix = lv_prefix ).
        DELETE lt_old WHERE name = ls_old->name.
        lv_changed = abap_true.

        CONTINUE.
      ENDIF.

      " Recursive check
      _compare_field( EXPORTING iv_prefix  = lv_prefix
                                is_new     = ls_new->*
                                ir_old     = ls_old
                      CHANGING  cv_changed = lv_changed ).
    ENDLOOP.

    LOOP AT lt_new REFERENCE INTO ls_new.
      READ TABLE lt_old REFERENCE INTO ls_old
       WITH TABLE KEY name = ls_new->name.
      CHECK sy-subrc <> 0.

      INSERT ls_new->* INTO TABLE lt_old.
      _add_change( iv_name   = ls_new->name
                   iv_new    = abap_true
                   iv_prefix = lv_prefix ).
      lv_changed = abap_true.
    ENDLOOP.
  ENDIF.

  " Repair option (pass as 'IV_REPAIR = abap_true')
  IF ir_old->length      <> is_new-length      OR
     ir_old->decimals    <> is_new-decimals    OR
     " ir_old->rollname    <> is_new-rollname    OR
     ir_old->table_kind  <> is_new-table_kind  OR
     ir_old->unique      <> is_new-unique      OR
     ir_old->key[]       <> is_new-key[]       OR
     ir_old->key_defkind <> is_new-key_defkind OR
     lv_changed          =  abap_true.

    _add_change( iv_name   = ir_old->name
                 iv_new    = abap_undefined
                 iv_prefix = iv_prefix ).
    " Copy new elementary declarations
    ir_old->length      = is_new-length.
    ir_old->decimals    = is_new-decimals.
    " ir_old->rollname    = is_new-rollname.

    " Copy new table declarations
    ir_old->table_kind  = is_new-table_kind.
    ir_old->unique      = is_new-unique.
    ir_old->key         = is_new-key.
    ir_old->key_defkind = is_new-key_defkind.

    lv_changed = abap_true.
    IF lt_old IS INITIAL.
      ir_old->rollname  = is_new-rollname.
    ELSE.
      ir_old->sub_fdesc = zcl_eui_conv=>to_json( lt_old ). " TODO lt_old?
    ENDIF.
  ENDIF.

  IF lv_changed = abap_true.
    cv_changed = lv_changed.
  ENDIF.
ENDMETHOD.


METHOD _compare_fields.
  DATA lt_editor_field LIKE ct_declared.

  " Check declarations
  DATA lr_field_value  TYPE REF TO zcl_aqo_helper=>ts_field_value.
  LOOP AT mt_field_value REFERENCE INTO lr_field_value.
    " Is not declared in ABAP code
    DELETE ct_declared WHERE table_line = lr_field_value->name.
    IF sy-subrc <> 0.
      INSERT lr_field_value->name INTO TABLE lt_editor_field.
      CONTINUE.
    ENDIF.

    " Get from declaration
    DATA lr_data TYPE REF TO data.
    lr_data = _get_abap_value(
       io_data = io_data
       ir_data = ir_data
       iv_name = lr_field_value->name ).

    FIELD-SYMBOLS <lv_value> TYPE any.
    ASSIGN lr_data->* TO <lv_value>.

    " Check existing decalration with editor field
    TRY.
        DATA ls_field_value  TYPE zcl_aqo_helper=>ts_field_value.
        ls_field_value-field_desc = zcl_eui_type=>get_field_desc(
            iv_field_name = lr_field_value->name
            iv_data       = <lv_value> ).

        DATA lo_error TYPE REF TO zcx_eui_exception.
      CATCH zcx_eui_exception INTO lo_error.
        zcx_aqo_exception=>raise_sys_error( io_error = lo_error ).
    ENDTRY.

    " Compare each existing field
    DATA ls_old TYPE REF TO zcl_eui_type=>ts_field_desc.
    GET REFERENCE OF lr_field_value->field_desc INTO ls_old.
    _compare_field(
       ir_old     = ls_old
       is_new     = ls_field_value-field_desc " abap code declaration
       iv_prefix  = '' ).
  ENDLOOP.

  " ERROR - IF lines( mt_field_value ) > lines( lt_declared_field )
  CHECK zcl_aqo_helper=>is_in_editor( ) <> abap_true AND lt_editor_field IS NOT INITIAL.

  DATA lv_message TYPE string.
  lv_message =  'Declared in editor only:'(dio).

  DATA lv_field TYPE abap_attrname.
  DATA lv_sep   TYPE string VALUE ``.
  LOOP AT lt_editor_field INTO lv_field.
    CONCATENATE lv_message lv_field INTO lv_message SEPARATED BY lv_sep.
    lv_sep = ','.
  ENDLOOP.

  MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'E'.
  zcx_aqo_exception=>raise_sys_error( iv_message = lv_message ).
ENDMETHOD.


METHOD _confirm_popup.
  " Overrite message
  DATA lv_title TYPE text255.
  MESSAGE s019(zaqo_message) WITH ms_db_item-package_id ms_db_item-option_id INTO lv_title.

  IF mt_change[] IS INITIAL.
    rv_ok = zcl_eui_screen=>confirm(
         iv_title    = 'Save'(sav)
         iv_question = lv_title
         iv_icon_1   = 'ICON_SYSTEM_SAVE' ).
    RETURN.
  ENDIF.

  DATA lt_table TYPE REF TO data.
  GET REFERENCE OF mt_change INTO lt_table.

  DATA ls_layout TYPE lvc_s_layo.
  ls_layout-no_toolbar = ls_layout-no_rowmark = abap_true.

  DATA lt_catalog TYPE lvc_t_fcat.
  DATA lr_catalog TYPE REF TO lvc_s_fcat.
  APPEND INITIAL LINE TO lt_catalog REFERENCE INTO lr_catalog.
  lr_catalog->fieldname = 'ICON'.
  lr_catalog->coltext   = `-----`.

  " Add Ok button
  APPEND INITIAL LINE TO lt_catalog REFERENCE INTO lr_catalog.
  lr_catalog->fieldname = 'DUMMY'.
  lr_catalog->edit      = 'X'.

  DATA lo_popup TYPE REF TO zcl_eui_alv.
  CREATE OBJECT lo_popup
    EXPORTING
      ir_table       = lt_table
      is_layout      = ls_layout
      it_mod_catalog = lt_catalog.

  lo_popup->popup( iv_col_beg = 25                       "#EC NUMBER_OK
                   iv_col_end = 78                       "#EC NUMBER_OK
                   iv_row_beg = 6                        "#EC NUMBER_OK
                   iv_row_end = 12                       "#EC NUMBER_OK
                 ).

  DATA ls_status TYPE zcl_eui_manager=>ts_status.
  ls_status-title = lv_title. " 'Save new option declaration'(sno).
  lo_popup->set_status( ls_status ).

  CHECK lo_popup->show( ) = 'OK'.
  rv_ok = abap_true.
ENDMETHOD.


METHOD _get_abap_value.
  DATA:
    lv_name    TYPE string.
  FIELD-SYMBOLS:
    <ls_data>  TYPE any,
    <lv_value> TYPE any.

  " Get data
  IF io_data IS NOT INITIAL.
    CONCATENATE 'IO_DATA->' iv_name INTO lv_name.
    ASSIGN (lv_name) TO <lv_value>.
  ELSE.
    ASSIGN ir_data->* TO <ls_data>.
    ASSIGN COMPONENT iv_name OF STRUCTURE <ls_data> TO <lv_value>.
  ENDIF.

  " Return as reference
  CHECK <lv_value> IS ASSIGNED.
  GET REFERENCE OF <lv_value> INTO rr_data.
ENDMETHOD.


METHOD _read_values.
  FIELD-SYMBOLS <ls_field_value> LIKE LINE OF mt_field_value.
  LOOP AT mt_field_value ASSIGNING <ls_field_value>.
    " Destination
    DATA lr_data TYPE REF TO data.
    lr_data = _get_abap_value(
       io_data = io_data
       ir_data = ir_data
       iv_name = <ls_field_value>-name ).

    FIELD-SYMBOLS <lv_value> TYPE any.
    ASSIGN lr_data->* TO <lv_value>.
    CHECK sy-subrc = 0.

    DATA lv_last_index TYPE i.
    lv_last_index = lines( <ls_field_value>-value ).

    DATA lr_history_value TYPE REF TO zcl_aqo_helper=>ts_history_value.
    READ TABLE <ls_field_value>-value REFERENCE INTO lr_history_value INDEX lv_last_index.
    CHECK sy-subrc = 0.

    DATA lv_ok TYPE abap_bool.
    CLEAR lv_ok.
    DO 2 TIMES.
      DATA lv_index TYPE syindex.
      lv_index = sy-index.
      " For tables only
      IF lv_index = 2.
        DATA ls_field_desc TYPE zcl_eui_type=>ts_field_desc.
        ls_field_desc = <ls_field_value>-field_desc.

        " Create standard table
        ls_field_desc-table_kind = cl_abap_tabledescr=>tablekind_std.
        CLEAR ls_field_desc-unique.

        " Assign it
        DATA lo_error TYPE REF TO zcx_eui_exception.
        TRY.
            DATA lo_type TYPE REF TO cl_abap_datadescr.
            lo_type = zcl_eui_type=>create_type_descr( is_field_desc = ls_field_desc ).
          CATCH zcx_eui_exception INTO lo_error.
            zcx_aqo_exception=>raise_sys_error( io_error = lo_error ).
        ENDTRY.

        DATA lr_table TYPE REF TO data.
        CREATE DATA lr_table TYPE HANDLE lo_type.

        FIELD-SYMBOLS <lt_any_tab> TYPE ANY TABLE.
        FIELD-SYMBOLS <lt_value>   TYPE STANDARD TABLE.
        ASSIGN:
          lr_data->*  TO <lt_any_tab>,
          lr_table->* TO <lt_value>,
          lr_table->* TO <lv_value>.
      ENDIF.

      zcl_eui_conv=>from_json(
       EXPORTING
         iv_json = lr_history_value->h_value
         " iv_mode = TODO SAFE ?
       IMPORTING
         ev_ok   = lv_ok
         ex_data = <lv_value> ).

      IF lv_index = 2 AND lv_ok = abap_true.
        " Safe copy of existing data
        DATA lr_prev TYPE REF TO data.
        CREATE DATA lr_prev LIKE LINE OF <lt_any_tab>.

        FIELD-SYMBOLS <ls_prev> TYPE any.
        ASSIGN lr_prev->* TO <ls_prev>.

        " No errors for duplications
        CLEAR <lt_any_tab>.

        FIELD-SYMBOLS <ls_value> TYPE any.
        LOOP AT <lt_value> ASSIGNING <ls_value>.
          MOVE-CORRESPONDING <ls_value> TO <ls_prev>.
          INSERT <ls_prev> INTO TABLE <lt_any_tab>.
        ENDLOOP.

        IF lines( <lt_value> ) <> lines( <lt_any_tab> ).
          lv_ok = abap_false.
        ENDIF.
      ENDIF.

      " Cannot read the table
      IF lv_index = 1 AND lv_ok <> abap_true AND <ls_field_value>-sys_type = cl_abap_typedescr=>typekind_table.
        CONTINUE.
      ENDIF.
      EXIT.
    ENDDO.

    CHECK lv_ok <> abap_true.

    DATA lv_message TYPE string.
    CONCATENATE 'Cannot read the value'(crv) <ls_field_value>-name INTO lv_message SEPARATED BY space.
    zcx_aqo_exception=>raise_sys_error( iv_message = lv_message ).
  ENDLOOP.
ENDMETHOD.


METHOD _set_db_defaults.
  " General info
  IF is_db IS NOT INITIAL.
    MOVE-CORRESPONDING is_db TO cs_db_item.
  ENDIF.

  " Technical info
  IF cs_db_item-prev_value_cnt IS INITIAL.
    cs_db_item-prev_value_cnt = 5.
  ENDIF.

  IF cs_db_item-created_date IS INITIAL.
    cs_db_item-created_date = sy-datum.
  ENDIF.

  IF cs_db_item-created_uname IS INITIAL.
    cs_db_item-created_uname = sy-uname.

    " Login could be deletes afterwards
    SELECT SINGLE name_textc INTO cs_db_item-created_name_txt
    FROM user_addr
    WHERE bname = sy-uname " ##WARN_OK  backward compatibility
    .
  ENDIF.
ENDMETHOD.
ENDCLASS.
