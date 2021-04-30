*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_helper IMPLEMENTATION.
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
