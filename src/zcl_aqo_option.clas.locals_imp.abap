*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_helper IMPLEMENTATION.
  METHOD create.
    CREATE OBJECT eo_opt.
    CLEAR es_last_call.

    DATA lt_callstack TYPE abap_callstack.
    " Where-Used List
    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level = 3
      IMPORTING
        callstack = lt_callstack.
    READ TABLE lt_callstack INDEX 3 INTO es_last_call.
    " Use package of calling include?
    IF sy-subrc = 0 AND cs_db_key-package_id IS INITIAL AND es_last_call-mainprogram <> zcl_aqo_helper=>mc_prog-editor.
      cs_db_key-package_id = get_default_package(
           is_stack    = es_last_call
           iv_is_class = iv_is_class ).
    ENDIF.

    " Load data
    zcl_aqo_helper=>get_by_key( EXPORTING is_db_key  = cs_db_key
                                CHANGING  cs_db_item = eo_opt->ms_db_item ).
  ENDMETHOD.

  METHOD get_default_package.
    DATA lv_object   TYPE tadir-object.
    DATA lv_obj_name TYPE tadir-obj_name.

    IF iv_is_class = abap_true.
      SPLIT is_stack-mainprogram(30) AT '=' INTO lv_obj_name lv_object.
      lv_object   = 'CLAS'.
    ELSE.
      lv_obj_name = is_stack-mainprogram.
      lv_object   = 'PROG'.
    ENDIF.

    SELECT SINGLE devclass INTO rv_package
    FROM tadir
    WHERE pgmid    = 'R3TR'
      AND object   = lv_object
      AND obj_name = lv_obj_name.

    CHECK sy-subrc <> 0.
    zcx_aqo_exception=>raise_sys_error( iv_message = 'Cannot detect default package'(cdd) ).
  ENDMETHOD.

  METHOD check_package_exist.
    " Check for new packages
    DATA lv_devclass TYPE tdevc-devclass.
    SELECT SINGLE devclass INTO lv_devclass
    FROM tdevc
    WHERE devclass = iv_package_id.

    " Oops
    IF lv_devclass IS INITIAL.
      MESSAGE s020(zaqo_message) WITH iv_package_id INTO sy-msgli.
      zcx_aqo_exception=>raise_sys_error( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_class_fields.
    DATA lo_class TYPE REF TO cl_abap_classdescr.
    lo_class ?= cl_abap_classdescr=>describe_by_object_ref( io_data ).

    DATA lv_name TYPE string.
    lv_name = lo_class->get_relative_name( ).

    " Check class
    DATA lt_friend TYPE abap_frndtypes_tab.
    lt_friend = lo_class->get_friend_types( ).

    READ TABLE lt_friend TRANSPORTING NO FIELDS
     WITH KEY table_line->absolute_name = '\CLASS=ZCL_AQO_OPTION'.
    IF sy-subrc <> 0.
      MESSAGE s014(zaqo_message) WITH lv_name INTO sy-msgli.
      zcx_aqo_exception=>raise_sys_error( ).
    ENDIF.

    " name type_kind length decimals
    DATA lv_is_stat TYPE abap_bool.
    lv_is_stat = abap_undefined.

    DATA ls_attr TYPE REF TO abap_attrdescr.
    LOOP AT lo_class->attributes REFERENCE INTO ls_attr
       WHERE visibility   = cl_abap_objectdescr=>public
         AND is_read_only = abap_true " <--- Mark as READ-ONLY attributes
         AND is_constant  = abap_false
         AND is_virtual   = abap_false

         " initialize CLASS-DATA or DATA (but not both!)
         " AND is_class     = abap_false

         " super class & child class can have different options! (in that case use only STATIC attributes)
         " Example 1 - IO_DATA = NEW ZCL_SUPER( ), 2 - IO_DATA = NEW ZCL_CHILD( )
         " Or use IR_DATA = ... instead of IO_DATA
         AND is_inherited = abap_false.

      " Check instance or static
      IF lv_is_stat <> abap_undefined AND lv_is_stat <> ls_attr->is_class.
        MESSAGE s014(zaqo_message) WITH ls_attr->name INTO sy-msgli.
        zcx_aqo_exception=>raise_sys_error( ).
      ENDIF.
      lv_is_stat = ls_attr->is_class.

      " And add to list
      INSERT ls_attr->name INTO TABLE rt_declared_field.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_struc_fields.
    DATA lo_struc TYPE REF TO cl_abap_structdescr.
    lo_struc ?= cl_abap_structdescr=>describe_by_data_ref( ir_data ).

    " name type_kind length decimals
    DATA ls_comp TYPE REF TO abap_compdescr.
    LOOP AT lo_struc->components REFERENCE INTO ls_comp.
      " And add to list
      DATA lv_field_name TYPE abap_attrname.
      lv_field_name = ls_comp->name.
      INSERT lv_field_name INTO TABLE rt_declared_field.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
