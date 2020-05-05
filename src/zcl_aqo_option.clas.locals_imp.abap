*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_helper IMPLEMENTATION.

  METHOD check_abap_declaration.
    DATA:
      lv_in_editor      TYPE abap_bool,
      lo_struc          TYPE REF TO cl_abap_structdescr,
      lo_class          TYPE REF TO cl_abap_classdescr,
      ls_comp           TYPE REF TO abap_compdescr,
      ls_attr           TYPE REF TO abap_attrdescr,
      lv_name           TYPE string,
      lv_field_name     TYPE abap_attrname,
      lt_friend         TYPE abap_frndtypes_tab,
      lv_is_stat        TYPE abap_bool,
      lt_declared_field TYPE zcl_aqo_helper=>abap_attrname_tab,
      lr_unique_type    TYPE REF TO zcl_eui_type=>tt_unique_type,
      lo_error          TYPE REF TO zcx_eui_exception.

    " No error in editor
    lv_in_editor = zcl_aqo_helper=>is_in_editor( ).

**********************************************************************
    " №1 Based on class
    IF io_data IS NOT INITIAL.
      lo_class ?= cl_abap_classdescr=>describe_by_object_ref( io_data ).
      lv_name = lo_class->get_relative_name( ).

      " Check class
      lt_friend = lo_class->get_friend_types( ).
      READ TABLE lt_friend TRANSPORTING NO FIELDS
       WITH KEY table_line->absolute_name = '\CLASS=ZCL_AQO_OPTION'.
      IF sy-subrc <> 0.
        MESSAGE s014(zaqo_message) WITH lv_name INTO sy-msgli.
        zcx_aqo_exception=>raise_sys_error( ).
      ENDIF.

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
          MESSAGE s014(zaqo_message) WITH ls_attr->name INTO sy-msgli.
          zcx_aqo_exception=>raise_sys_error( ).
        ENDIF.
        lv_is_stat = ls_attr->is_class.

        " And add to list
        INSERT ls_attr->name INTO TABLE lt_declared_field.
      ENDLOOP.
    ENDIF.

**********************************************************************
    " №2 Based on structure
    IF ir_data IS NOT INITIAL.
      lo_struc ?= cl_abap_structdescr=>describe_by_data_ref( ir_data ).

      " name type_kind length decimals
      LOOP AT lo_struc->components REFERENCE INTO ls_comp.
        " And add to list
        lv_field_name = ls_comp->name.
        INSERT lv_field_name INTO TABLE lt_declared_field.
      ENDLOOP.
    ENDIF.

**********************************************************************
    " Check abap declaration
**********************************************************************
    DATA:
      lr_data         TYPE REF TO data,
      lr_new_field    TYPE REF TO abap_attrname,
      lt_editor_field LIKE lt_declared_field,
      ls_field_value  TYPE zcl_aqo_helper=>ts_field_value,
      lr_field_value  TYPE REF TO zcl_aqo_helper=>ts_field_value,
      ls_old          TYPE REF TO zcl_eui_type=>ts_field_desc,
      lv_value        TYPE string.
    FIELD-SYMBOLS:
      <lv_value> TYPE any.

    " Just show warning
    IF iv_repair = abap_true AND lv_in_editor <> abap_true.
      MESSAGE s029(zaqo_message) DISPLAY LIKE 'W'.
    ENDIF.

    " Check declarations
    LOOP AT ct_field_value REFERENCE INTO lr_field_value.
      " Is not declared in ABAP code
      DELETE lt_declared_field WHERE table_line = lr_field_value->name.
      IF sy-subrc <> 0.
        INSERT lr_field_value->name INTO TABLE lt_editor_field.
        CONTINUE.
      ENDIF.

      " Get from declaration
      lr_data = io_option->get_abap_value(
         io_data = io_data
         ir_data = ir_data
         iv_name = lr_field_value->name ).
      ASSIGN lr_data->* TO <lv_value>.

      " Check existing decalration with editor field
      TRY.
          ls_field_value-field_desc = zcl_eui_type=>get_field_desc(
              iv_field_name = lr_field_value->name
              iv_data       = <lv_value> ).
        CATCH zcx_eui_exception INTO lo_error.
          zcx_aqo_exception=>raise_sys_error( io_error = lo_error ).
      ENDTRY.

      " Compare each existing field
      GET REFERENCE OF lr_field_value->field_desc INTO ls_old.
      lcl_helper=>compare_2_fields(
       EXPORTING
         is_new     = ls_field_value-field_desc " abap code declaration
         iv_repair  = iv_repair
         cs_old     = ls_old
       CHANGING
         cv_changed = cv_changed ).
    ENDLOOP.

    " ERROR - IF lines( mt_field_value ) > lines( lt_declared_field )
    IF lv_in_editor <> abap_true AND lt_editor_field IS NOT INITIAL.
      " Dont't have fields in abap source code
      " ---> lt_editor_field[]
      IF 1 = 2.
        MESSAGE s027(zaqo_message) WITH '' '' '' ''.
      ENDIF.

      " Show error
      zcl_aqo_helper=>message_with_fields(
       it_field  = lt_editor_field[]
       iv_number = 027 ).
      zcx_aqo_exception=>raise_sys_error( ).
    ENDIF.

**********************************************************************
    " OK - add description one by one (have something new in ABAP code)
    " IF lines( mt_field_value ) < lines( lt_declared_field )
    CREATE DATA lr_unique_type.
    LOOP AT lt_declared_field REFERENCE INTO lr_new_field.
      cv_changed = abap_true.

      " Get from declaration
      lr_data = io_option->get_abap_value(
         io_data = io_data
         ir_data = ir_data
         iv_name = lr_new_field->* ).
      ASSIGN lr_data->* TO <lv_value>.
      lv_value = zcl_eui_conv=>to_json( <lv_value> ).

      " Field cescription
      TRY.
          ls_field_value-field_desc = zcl_eui_type=>get_field_desc(
            iv_field_name  = lr_new_field->*
            iv_data        = <lv_value>
            ir_unique_type = lr_unique_type ).
        CATCH zcx_eui_exception INTO lo_error.
          zcx_aqo_exception=>raise_sys_error( io_error = lo_error ).
      ENDTRY.

      " Add to history
      io_option->add_history_value(
       EXPORTING
         iv_value       = lv_value
       CHANGING
         cs_field_value = ls_field_value ).

      " And finally add new field option
      INSERT ls_field_value INTO TABLE ct_field_value.
    ENDLOOP.
  ENDMETHOD.

  METHOD compare_2_fields.
    DATA:
      lt_old      TYPE zcl_eui_type=>tt_field_desc,
      lv_old_ok   TYPE abap_bool,
      lt_new      TYPE zcl_eui_type=>tt_field_desc,
      lv_new_ok   TYPE abap_bool,
      lv_changed  LIKE cv_changed,
      ls_old      TYPE REF TO zcl_eui_type=>ts_field_desc,
      ls_new      TYPE REF TO zcl_eui_type=>ts_field_desc,
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
      zcl_eui_conv=>from_json(
       EXPORTING
         iv_json = cs_old->sub_fdesc
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
        cs_old->sub_fdesc   = zcl_eui_conv=>to_json( lt_old ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
