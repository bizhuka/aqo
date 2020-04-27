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

CLASS lcl_unq_menu IMPLEMENTATION.
  METHOD get_eui_menu.
    DATA:
      lv_in_editor TYPE abap_bool,
      ls_unq_menu  TYPE ts_unq_menu,
      lr_unq_menu  TYPE REF TO ts_unq_menu,
      lo_unq_menu  TYPE REF TO lcl_unq_menu.

    lv_in_editor = zcl_aqo_helper=>is_in_editor( ).
    IF lv_in_editor = abap_true.
      ls_unq_menu-unq_id = 'ZFM_AQO_MENU_INIT'.
    ELSE.
      CONCATENATE iv_package_id '-' iv_option_id INTO ls_unq_menu-unq_id.
    ENDIF.

    " Check menu existence
    READ TABLE mt_unq_menu REFERENCE INTO lr_unq_menu
     WITH TABLE KEY unq_id = ls_unq_menu-unq_id.

    " Create new GOS menu
    IF sy-subrc <> 0.
      CREATE OBJECT ls_unq_menu-unq_menu.
      INSERT ls_unq_menu INTO TABLE mt_unq_menu REFERENCE INTO lr_unq_menu.
    ENDIF.
    lo_unq_menu = lr_unq_menu->unq_menu.

    " If new option
    IF lo_unq_menu->package_id <> iv_package_id OR lo_unq_menu->option_id  <> iv_option_id.
      " Construct unq menu
      lo_unq_menu->package_id = iv_package_id.
      lo_unq_menu->option_id  = iv_option_id.

      " User interface class
      IF lo_unq_menu->eui_menu IS INITIAL.
        CREATE OBJECT lo_unq_menu->eui_menu
          EXPORTING
            io_handler = lo_unq_menu.
      ENDIF.

      " prepare buttons
      DATA lt_menu TYPE zcl_eui_menu=>tt_menu.
      lt_menu = lo_unq_menu->get_buttons( iv_in_editor = lv_in_editor ).

      lo_unq_menu->eui_menu->create_toolbar( it_menu = lt_menu ).
    ENDIF.

    " And return
    ro_eui_menu = lo_unq_menu->eui_menu.
  ENDMETHOD.

  METHOD get_buttons.
    " Delete prev menu & create new one
    DATA:
      lv_is_dev        TYPE abap_bool,

      lv_in_viewer     TYPE abap_bool,
      lv_lock_info     TYPE stb_button-quickinfo,
      lv_view_only     TYPE abap_bool,

      lv_option_exist  TYPE abap_bool VALUE abap_false,
      lv_package_exist TYPE abap_bool VALUE abap_false,

      " For separators only
      lv_prev_hide     TYPE abap_bool,

      lv_menu_mode     TYPE ztaqo_option-menu_mode,
      lv_root_func     TYPE ui_func,
      lv_devclass      TYPE tdevc-devclass.
    FIELD-SYMBOLS:
     <ls_menu> LIKE LINE OF rt_menu.

    lv_is_dev    = zcl_aqo_helper=>is_dev_mandt( ).
    lv_lock_info = read_locks( ).

    " Exist ?
    SELECT SINGLE menu_mode INTO lv_menu_mode
    FROM ztaqo_option
    WHERE package_id = me->package_id
      AND option_id  = me->option_id.
    IF sy-subrc = 0.
      lv_option_exist = abap_true.
    ELSE.
      SELECT SINGLE devclass INTO lv_devclass
      FROM tdevc
      WHERE devclass = me->package_id.
      IF sy-subrc = 0.
        lv_package_exist = abap_true.
      ENDIF.
    ENDIF.

    IF iv_in_editor = abap_true.
      lv_in_viewer = zcl_aqo_helper=>is_in_editor( iv_is_viewer = abap_true ).
      me->tcode    = sy-tcode.
    ELSEIF lv_option_exist = abap_true.

      CASE lv_menu_mode.
        WHEN mc_menu_mode-hide.
          RETURN.

        WHEN mc_menu_mode-view.
          " Change tooltip
          IF lv_lock_info IS INITIAL.
            MESSAGE s034(zaqo_message) WITH 'settings' INTO lv_lock_info.
          ENDIF.
          me->tcode = mc_prog-viewer_tcode.

        WHEN mc_menu_mode-edit.
          " Do nothing

        WHEN OTHERS.
          MESSAGE 'Unknown MENU_MODE' TYPE 'X'.
      ENDCASE.

      " If not locked by user or settings
      IF lv_lock_info IS INITIAL.
        me->tcode = mc_prog-editor_tcode.
      ENDIF.
    ENDIF.

**********************************************************************
    " INITIALIZE the menu
**********************************************************************
    IF iv_in_editor <> abap_true. " Is not in editor transaction
      APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.

      " Just tooltip
      CONCATENATE me->package_id ` - ` me->option_id INTO <ls_menu>-quickinfo.
      <ls_menu>-icon         = icon_tools.
      <ls_menu>-butn_type    = cntb_btype_menu.
      <ls_menu>-function     = lv_root_func = mc_action-base.
    ENDIF.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = 'New'(crt).
    <ls_menu>-icon         = icon_create.
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-new.
    <ls_menu>-par_function = lv_root_func.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = 'View'(viw).
    <ls_menu>-quickinfo    = lv_lock_info.
    <ls_menu>-icon         = icon_display.
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-view.
    <ls_menu>-par_function = lv_root_func.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = <ls_menu>-quickinfo    = 'Edit'(edt).
    <ls_menu>-icon         = icon_change.
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-change.
    <ls_menu>-par_function = lv_root_func.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-butn_type    = cntb_btype_sep.
    <ls_menu>-par_function = lv_root_func.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = 'Transport'(trn).
    <ls_menu>-quickinfo    = lv_lock_info.
    <ls_menu>-icon         = icon_transport.
    <ls_menu>-butn_type    = cntb_btype_menu.
    <ls_menu>-function     = mc_action-transport_root.
    <ls_menu>-par_function = lv_root_func.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-butn_type    = cntb_btype_sep.
    <ls_menu>-par_function = mc_action-transport_root.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = <ls_menu>-quickinfo    = 'Export'(exp).
    <ls_menu>-icon         = icon_export.
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-export.
    <ls_menu>-par_function = mc_action-transport_root.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = 'Import'(imp).
    <ls_menu>-quickinfo    = lv_lock_info.
    <ls_menu>-icon         = icon_import.
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-import.
    <ls_menu>-par_function = mc_action-transport_root.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-butn_type    = cntb_btype_sep.
    <ls_menu>-par_function = mc_action-transport_root.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = 'Delete'(del).
    <ls_menu>-quickinfo    = lv_lock_info.
    <ls_menu>-icon         = icon_delete.
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-delete.
    <ls_menu>-par_function = mc_action-transport_root.

**********************************************************************
    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = <ls_menu>-quickinfo    = 'Where-Used List'(wul).
    <ls_menu>-icon         = icon_reference_list.
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-last_code.
    <ls_menu>-par_function = lv_root_func.

**********************************************************************

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-butn_type    = cntb_btype_sep.
    <ls_menu>-par_function = lv_root_func.
    <ls_menu>-function     = mc_action-about_sep. " Always show


    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = <ls_menu>-quickinfo    = 'Attach'(att).
    <ls_menu>-icon         = icon_attachment.
    <ls_menu>-butn_type    = cntb_btype_menu.
    <ls_menu>-function     = mc_action-attach_root.
    <ls_menu>-par_function = lv_root_func.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = <ls_menu>-quickinfo    = 'Add attachment'(aat).
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-attach_import.
    <ls_menu>-par_function = mc_action-attach_root.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = <ls_menu>-quickinfo    = 'Show attachments'(sat).
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-attach_show.
    <ls_menu>-par_function = mc_action-attach_root.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = <ls_menu>-quickinfo    = 'Delete attachment'(dat).
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-attach_delete.
    <ls_menu>-par_function = mc_action-attach_root.

    APPEND INITIAL LINE TO rt_menu ASSIGNING <ls_menu>.
    <ls_menu>-text         = <ls_menu>-quickinfo    = 'About'(abo).
    <ls_menu>-icon         = icon_tools.
    <ls_menu>-butn_type    = cntb_btype_button.
    <ls_menu>-function     = mc_action-about.
    <ls_menu>-par_function = lv_root_func.

**********************************************************************
    " Change visibility
    IF lv_in_viewer = abap_true OR lv_lock_info IS NOT INITIAL.
      lv_view_only = abap_true.
    ENDIF.

    " 0 - lv_is_dev, 1 - iv_in_editor, 2 - lv_view_only, 3 - lv_option_exist
    LOOP AT rt_menu ASSIGNING <ls_menu>.
      CASE <ls_menu>-function.

          " Just skip
        WHEN mc_action-base.

        WHEN mc_action-attach_root OR mc_action-last_code.
          IF lv_option_exist <> abap_true OR lv_is_dev <> abap_true.
            <ls_menu>-hide = abap_true.
          ENDIF.

        WHEN mc_action-transport_root.
          IF lv_option_exist <> abap_true. " Show EXPORT OR lv_is_dev <> abap_true.
            <ls_menu>-hide = abap_true.
          ENDIF.

        WHEN mc_action-new.
          IF   lv_option_exist = abap_true
            OR lv_view_only = abap_true.
            <ls_menu>-hide = abap_true.
          ENDIF.

          IF lv_is_dev <> abap_true.
            <ls_menu>-disabled = abap_true.
            <ls_menu>-quickinfo = 'Only in DEV!'.
          ENDIF.

          " For info
          IF lv_option_exist <> abap_true AND lv_package_exist <> abap_true.
            <ls_menu>-disabled = abap_true.
            <ls_menu>-quickinfo = `The package don't exist`.
          ENDIF.

          IF <ls_menu>-quickinfo IS INITIAL.
            <ls_menu>-quickinfo = 'SE38->ZAQO_TESTER is simpler'.
          ENDIF.

        WHEN mc_action-change.
          IF    lv_option_exist <> abap_true
             OR iv_in_editor = abap_true
             OR lv_view_only = abap_true.
            <ls_menu>-hide = abap_true.
          ENDIF.

        WHEN mc_action-view.
          IF    lv_option_exist <> abap_true
             OR iv_in_editor = abap_true
             OR lv_view_only <> abap_true.
            <ls_menu>-hide = abap_true.
          ENDIF.

        WHEN mc_action-import OR mc_action-delete " OR mc_action-transport  OR mc_action-save_in
             OR mc_action-attach_import OR mc_action-attach_delete.
          IF lv_option_exist <> abap_true.
            <ls_menu>-hide = abap_true.
          ENDIF.

          IF lv_is_dev <> abap_true
             OR iv_in_editor <> abap_true
             OR lv_view_only = abap_true.
            <ls_menu>-disabled = abap_true.
            <ls_menu>-quickinfo = 'In DEV editor only!'.
          ENDIF.

        WHEN mc_action-export OR mc_action-attach_show.

        WHEN mc_action-about OR mc_action-about_sep.
          IF lv_option_exist <> abap_true.
            <ls_menu>-hide = abap_true.
          ENDIF.

        WHEN OTHERS.
          IF <ls_menu>-butn_type = cntb_btype_sep.
            <ls_menu>-hide = lv_prev_hide.
          ELSE.
            MESSAGE 'Unknouwn button' TYPE 'X'.
          ENDIF.
      ENDCASE.

      IF <ls_menu>-disabled = abap_true OR <ls_menu>-hide = abap_true.
        lv_prev_hide = abap_true.
      ELSE.
        lv_prev_hide = abap_false.
      ENDIF.

      CHECK <ls_menu>-par_function IS INITIAL.
      CLEAR <ls_menu>-text.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_locks.
    DATA:
      lv_garg TYPE seqg3-garg,
      lt_lock TYPE STANDARD TABLE OF seqg3 WITH DEFAULT KEY.
    FIELD-SYMBOLS:
     <ls_lock> LIKE LINE OF lt_lock.

    lv_garg+0(30)  = me->package_id.
    lv_garg+30(30) = me->option_id.
    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        gname                 = 'ZTAQO_OPTION'
        garg                  = lv_garg
        guname                = space " by all users
      TABLES
        enq                   = lt_lock
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.
    CHECK sy-subrc = 0.

    READ TABLE lt_lock ASSIGNING <ls_lock> INDEX 1.
    CHECK sy-subrc = 0.

    MESSAGE s034(zaqo_message) WITH <ls_lock>-guname INTO rv_locked_text.
  ENDMETHOD.

  METHOD on_function_selected.
    DATA lo_err    TYPE REF TO zcx_aqo_exception.
    DATA lo_error  TYPE REF TO cx_root.
    DATA lv_update TYPE abap_bool.

    DEFINE read_option.
      IF   me->option IS INITIAL
        OR me->option->ms_db_item-package_id <> me->package_id
        OR me->option->ms_db_item-option_id  <> me->option_id.

        TRY.
            me->option = zcl_aqo_option=>create(
               iv_package_id  = me->package_id
               iv_option_id   = me->option_id ).
          CATCH zcx_aqo_exception INTO lo_err.
            MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
        ENDTRY.
      ENDIF.
    END-OF-DEFINITION.

    " Set no error mode
    zcl_aqo_helper=>is_in_editor( iv_tcode = me->tcode ).

    read_option.
    TRY.
        CALL METHOD me->(fcode)
          RECEIVING
            rv_update = lv_update.

        " Option was changed
        IF lv_update = abap_true.
          CLEAR me->option.
          read_option.
        ENDIF.
      CATCH cx_root INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD _export.
    DATA:
      lv_title     TYPE string,
      lv_file_name TYPE string,
      lv_path      TYPE string,
      lv_filename  TYPE string,
      lv_fullpath  TYPE string.

    lv_title     = 'Save option values'(sov).
    CONCATENATE option->ms_db_item-package_id `-` option->ms_db_item-option_id `-` sy-mandt `-` sy-datum `-` sy-uzeit `.aqob` INTO lv_file_name.
    cl_gui_frontend_services=>file_save_dialog(
     EXPORTING
       window_title      = lv_title
       default_file_name = lv_file_name
     CHANGING
       path         = lv_path
       filename     = lv_filename
       fullpath     = lv_fullpath
     EXCEPTIONS
       OTHERS       = 1 ).
    CHECK sy-subrc = 0 AND lv_fullpath IS NOT INITIAL.

    " Save to file
    DATA lo_file TYPE REF TO zcl_eui_file.
    CREATE OBJECT lo_file
      EXPORTING
        iv_xstring = option->ms_db_item-fields.
    TRY.
        lo_file->download( iv_full_path = lv_fullpath ).
      CATCH zcx_eui_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD _about.
    " Show in screen PARAMETERS:
    DATA ls_dyn_scr  TYPE REF TO zsaqo_about_dialog.
    DATA lo_screen   TYPE REF TO zcl_eui_screen.
    DATA lo_err      TYPE REF TO zcx_eui_exception.
    DATA lv_input    TYPE screen-input.
    DATA lv_cmd      TYPE syucomm.

    " Where to store data
    CREATE DATA ls_dyn_scr.

    " Previous values
    ls_dyn_scr->p_2_pack = option->ms_db_item-package_id.
    ls_dyn_scr->p_2_opt  = option->ms_db_item-option_id.
    ls_dyn_scr->p_2_date = option->ms_db_item-created_date.
    ls_dyn_scr->p_2_ntxt = option->ms_db_item-created_name_txt.
    " Editable
    ls_dyn_scr->p_2_desc = option->ms_db_item-description.
    ls_dyn_scr->p_2_prev = option->ms_db_item-prev_value_cnt.
    ls_dyn_scr->p_2_menu = option->ms_db_item-menu_mode.

    " Create screen manager
    TRY.
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr        = '1020'
            iv_cprog        = mc_prog-editor
            ir_context      = ls_dyn_scr
            " Set pf-status & text
            iv_status_name  = 'ABOUT_STATUS'
            iv_status_prog  = mc_prog-editor
            iv_status_title = 'Enter option description'(eod).
      CATCH zcx_eui_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Exclude buttons
    IF me->tcode = mc_prog-editor_tcode AND zcl_aqo_helper=>is_dev_mandt( ) = abap_true.
      lv_input = '1'.
      APPEND 'USER_INFO'   TO lo_screen->ms_status-exclude.
    ELSE.
      lv_input = '0'.
      APPEND 'MODIFY'      TO lo_screen->ms_status-exclude.
      APPEND 'DEV_INFO'    TO lo_screen->ms_status-exclude.

      " Also hide
      IF has_visible_files( iv_pack = ls_dyn_scr->p_2_pack iv_opt = ls_dyn_scr->p_2_opt ) <> abap_true.
        APPEND 'USER_INFO' TO lo_screen->ms_status-exclude.
      ENDIF.
    ENDIF.

    " Static PF status no need on_pbo_event.
    lo_screen->customize( iv_fieldname = 'P_2_PACK'   input = '0' ).
    lo_screen->customize( iv_fieldname = 'P_2_OPT'    input = '0' ).
    lo_screen->customize( iv_fieldname = 'P_2_DATE'   input = '0' ).
    lo_screen->customize( iv_fieldname = 'P_2_NTXT'   input = '0' ).

    lo_screen->customize( iv_fieldname = 'P_2_DESC'   input = lv_input required = '1' ).
    lo_screen->customize( iv_fieldname = 'P_2_PREV'   input = lv_input required = '1' ).
    lo_screen->customize( iv_fieldname = 'P_2_MENU'   input = lv_input ).

    " As popup
    lo_screen->popup( ).

    " Process action
    lv_cmd = lo_screen->show( io_handler = me iv_handlers_map = 'ON_ABOUT_PAI' ).
    CHECK lv_cmd <> zif_eui_manager=>mc_cmd-cancel.
    rv_update = abap_true.
  ENDMETHOD.

  METHOD on_about_pai.
    DATA lo_screen   TYPE REF TO zcl_eui_screen.
    DATA ls_dyn_scr  TYPE REF TO zsaqo_about_dialog.
    DATA lo_err      TYPE REF TO zcx_aqo_exception.

    " Get data
    lo_screen ?= sender.
    ls_dyn_scr ?= lo_screen->get_context( ).

    CASE iv_command.
      WHEN 'MODIFY'.
        IF ls_dyn_scr->p_2_prev > 7 OR ls_dyn_scr->p_2_prev < 1.
          MESSAGE 'Previous values count have to be from 1 to 7'(nir) TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        do_update(
         iv_set         = `DESCRIPTION = IV_DESCRIPTION PREV_VALUE_CNT = IV_PREV_COUNT MENU_MODE = IV_MENU_MODE`
         iv_description = ls_dyn_scr->p_2_desc
         iv_prev_count  = ls_dyn_scr->p_2_prev
         iv_menu_mode   = ls_dyn_scr->p_2_menu ).

        cv_close->* = abap_true.

      WHEN 'DEV_INFO'.
        cv_close->* = abap_true.

        " Show online documentation in browser
        CALL FUNCTION 'CALL_BROWSER'
          EXPORTING
            url    = 'https://github.com/bizhuka/aqo/wiki'
          EXCEPTIONS
            OTHERS = 6.
        CHECK sy-subrc = 0.

      WHEN 'USER_INFO'.
        cv_close->* = abap_true.

        " Check in attachments
        TRY.
            _attach_show( iv_vis_only = abap_true ).
          CATCH zcx_aqo_exception INTO lo_err.
            MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.

        " Ok or cancel ?
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.

  METHOD has_visible_files.
    DATA:
      ls_visible   TYPE ts_visible,
      lr_visible   TYPE REF TO ts_visible,
      lt_oaor_file TYPE zcl_aqo_helper=>tt_oaor_file.

    READ TABLE mt_visible REFERENCE INTO lr_visible
     WITH TABLE KEY pack = iv_pack
                    opt  = iv_opt.

    " Serach for the first time
    IF sy-subrc <> 0.
      ls_visible-pack = iv_pack.
      ls_visible-opt  = iv_opt.

      " Get all files
      zcl_aqo_helper=>oaor_get_files(
       EXPORTING
         iv_pack_id   = ls_visible-pack
         iv_option_id = ls_visible-opt
       IMPORTING
         et_oaor_file	= lt_oaor_file ).

      " Only visible and new files
      DELETE lt_oaor_file WHERE visible <> abap_true OR last_version <> abap_true.
      IF lt_oaor_file IS NOT INITIAL.
        ls_visible-visible = abap_true.
      ENDIF.

      " Insert for speed
      INSERT ls_visible INTO TABLE mt_visible REFERENCE INTO lr_visible.
    ENDIF.

    " And return
    rv_visible = lr_visible->visible.
  ENDMETHOD.

  METHOD _import.
    DATA:
      lt_file  TYPE filetable,
      ls_file  TYPE REF TO file_table,
      lv_file  TYPE string,
      lv_rc    TYPE i,
      lt_data  TYPE solix_tab,
      lv_len   TYPE i,
      lv_xdata TYPE xstring.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        " window_title   =
        multiselection    = abap_false
        file_filter       = '*.aqob'
        default_extension = 'aqob'
      CHANGING
        file_table        = lt_file
        rc                = lv_rc ).
    CHECK lt_file[] IS NOT INITIAL.

    " 1 file only
    READ TABLE lt_file REFERENCE INTO ls_file INDEX 1.
    CHECK sy-subrc = 0.

    lv_file = ls_file->filename.
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename   = lv_file
        filetype   = 'BIN'
      IMPORTING
        filelength = lv_len
      TABLES
        data_tab   = lt_data
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    " TODO check data structure
    lv_xdata = zcl_eui_conv=>binary_to_xstring(
     it_table  = lt_data
     iv_length = lv_len ).

    rv_update = do_update(
     iv_set    = `FIELDS = IV_FIELDS`
     iv_fields = lv_xdata ).
  ENDMETHOD.

  METHOD do_update.
    UPDATE ztaqo_option
     SET (iv_set)
    WHERE package_id = option->ms_db_item-package_id
      AND option_id  = option->ms_db_item-option_id.

    IF sy-subrc = 0.
      MESSAGE 'Data updated'(upd) TYPE 'S'.
      rv_ok = abap_true.
    ELSE.
      MESSAGE 'Error during updating!'(edu) TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD _last_code.
    DATA:
      lv_ok TYPE abap_bool.
    IF option->ms_db_item-mainprogram IS INITIAL.
      MESSAGE 'No previous call was found'(ncl) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Try to launch by save first save of option
    lv_ok = zcl_aqo_helper=>navigate_to(
       iv_include  = option->ms_db_item-include
       iv_position = option->ms_db_item-line ).

    " if not OK
    CHECK lv_ok <> abap_true
      AND option->ms_db_item-package_id IS NOT INITIAL
      AND option->ms_db_item-option_id  IS NOT INITIAL.

    " Pass params
    SET PARAMETER ID:
      'ZAQO_PACKAGE_ID' FIELD option->ms_db_item-package_id,
      'ZAQO_OPTION_ID'  FIELD option->ms_db_item-option_id.

    " Second attempt by code scan
    PERFORM call_by_name IN PROGRAM zaqo_editor_old
      USING 'CODE_SCAN_F4'.
  ENDMETHOD.

  METHOD _delete.
    " $ & transport
    DATA lv_message TYPE string.
    lv_message = option->delete( ).
    IF lv_message IS NOT INITIAL.
      MESSAGE lv_message TYPE 'S'.
    ENDIF.

    rv_update = abap_true.
  ENDMETHOD.

  METHOD _view.
    _change( ).
    rv_update = abap_true.
  ENDMETHOD.

  METHOD _new.
    _change( iv_command  = '_NEW_OPTION' ).
    rv_update = abap_true.
  ENDMETHOD.

  METHOD _change.
    SET PARAMETER ID:
      'ZAQO_PACKAGE_ID' FIELD option->ms_db_item-package_id,
      'ZAQO_OPTION_ID'  FIELD option->ms_db_item-option_id,
      'ZAQO_COMMAND'    FIELD iv_command.

    CALL TRANSACTION me->tcode " WITH AUTHORITY-CHECK "#EC CI_CALLTA
      AND SKIP FIRST SCREEN.
  ENDMETHOD.

  METHOD _attach_import.
    DATA:
      lt_file           TYPE sbdst_files,
      ls_file           TYPE bapifiles,
      lt_oaor_file      TYPE zcl_aqo_helper=>tt_oaor_file,
      ls_oaor_file      TYPE zcl_aqo_helper=>ts_oaor_file,
      lt_property       TYPE STANDARD TABLE OF bapiproper WITH DEFAULT KEY,
      lr_property       TYPE REF TO bapiproper,
      lt_bds_signature  TYPE sbdst_signature,
      ls_bds_signature  TYPE bapisignat,
      lv_key            TYPE sbdst_object_key,
      lt_file_table     TYPE filetable,
      ls_file_table     TYPE REF TO file_table,
      lv_rc             TYPE i,
      lv_action         TYPE i,
      lv_ext            TYPE string,
      lv_new_doc_ver_no TYPE sbdst_doc_ver_no,
      lv_last_index     TYPE i,
      lv_ok             TYPE abap_bool,
      lv_ar_object      TYPE toadv-ar_object,
      lv_task           TYPE e070-trkorr,
      lv_message        TYPE string,
      lv_oaor_mode      TYPE string.

    " Subfolder in OAOR (and classname = package_id)
    lv_key = option->ms_db_item-option_id.

    " Get file info
    cl_gui_frontend_services=>file_open_dialog(
     EXPORTING
       multiselection = abap_false
     CHANGING
       file_table     = lt_file_table
       rc             = lv_rc
       user_action    = lv_action
     EXCEPTIONS
       OTHERS      = 1 ).
    CHECK sy-subrc = 0 AND lt_file_table[] IS NOT INITIAL.

    " Create or not
    zcl_aqo_helper=>oaor_check_exists(
     EXPORTING
       iv_pack_id    = option->ms_db_item-package_id
       iv_option_id  = option->ms_db_item-option_id
     IMPORTING
       ev_task       = lv_task
       ev_ok_message = lv_message ).
    IF lv_message IS NOT INITIAL.
      MESSAGE lv_message TYPE 'S'.
    ENDIF.

    CHECK lv_task IS NOT INITIAL.

    " First file
    READ TABLE lt_file_table REFERENCE INTO ls_file_table INDEX 1.
    CHECK sy-subrc = 0.

    " Extract info & add
    zcl_eui_file=>split_file_path(
     EXPORTING
       iv_fullpath  = ls_file_table->filename
     IMPORTING
       ev_path      = ls_file-directory
       ev_filename  = ls_file-filename
       ev_extension = lv_ext ).

    " Always 1 file
    ls_file-comp_count = ls_file-doc_count = ls_bds_signature-doc_count  = 1.
    " Always in UPPER CASE (ID)
    TRANSLATE ls_file-filename TO UPPER CASE.
    " And add
    APPEND ls_file TO lt_file.

    " Read previous
    zcl_aqo_helper=>oaor_get_files(
     EXPORTING
      iv_pack_id   = option->ms_db_item-package_id
      iv_option_id = option->ms_db_item-option_id
      iv_filename  = ls_file-filename
     IMPORTING
      es_oaor_last = ls_oaor_file
      et_oaor_file = lt_oaor_file ).

    " New or existeng item
    IF ls_oaor_file IS INITIAL.
      ls_oaor_file-description = ls_oaor_file-file_name = ls_file-filename.
      lv_oaor_mode = mc_oaor-new_file.
      MESSAGE s036(zaqo_message).

    ELSEIF sy-datum = ls_oaor_file-last_changed_at_date.
      lv_oaor_mode = mc_oaor-update_version.
      MESSAGE s038(zaqo_message) WITH ls_oaor_file-doc_ver_no.

    ELSE.
      lv_oaor_mode = mc_oaor-new_version.
      MESSAGE s037(zaqo_message) WITH ls_oaor_file-doc_ver_no.
    ENDIF.

    " #######
    show_new_version_diloag(
     IMPORTING
       ev_ok        = lv_ok
     CHANGING
       cs_oaor_file = ls_oaor_file ).
    CHECK lv_ok = abap_true.

    " always equal to file name (Case sensetive)
    APPEND INITIAL LINE TO lt_property REFERENCE INTO lr_property.
    lr_property->prop_name  = 'DESCRIPTION'.
    lr_property->prop_value = ls_oaor_file-description.

    APPEND INITIAL LINE TO lt_property REFERENCE INTO lr_property.
    lr_property->prop_name  = 'BDS_DOCUMENTCLASS'.
    lr_property->prop_value = lv_ext.

    APPEND INITIAL LINE TO lt_property REFERENCE INTO lr_property.
    lr_property->prop_name  = 'LANGUAGE'.
    lr_property->prop_value = sy-langu.

    " Add as keyword
    IF ls_oaor_file-visible = abap_true.
      APPEND INITIAL LINE TO lt_property REFERENCE INTO lr_property.
      lr_property->prop_name  = 'BDS_KEYWORD'.
      lr_property->prop_value = 'VISIBLE'.
    ENDIF.

**********************************************************************
    CASE lv_oaor_mode.

        " First version
      WHEN mc_oaor-new_file.

        " Detect folder  'BDS_ATTACH' is first
        SELECT SINGLE ar_object INTO lv_ar_object
        FROM toadv
        WHERE standard = abap_true.
        CHECK sy-subrc = 0.

        APPEND INITIAL LINE TO lt_property REFERENCE INTO lr_property.
        lr_property->prop_name  = 'BDS_DOCUMENTTYPE'.
        lr_property->prop_value = lv_ar_object.

        " Convert to another table
        LOOP AT lt_property REFERENCE INTO lr_property.
          MOVE-CORRESPONDING lr_property->* TO ls_bds_signature.
          APPEND ls_bds_signature TO lt_bds_signature.
        ENDLOOP.

        cl_bds_document_set=>create_with_files(
           EXPORTING
             classname       = option->ms_db_item-package_id
             classtype       = zcl_aqo_helper=>mc_oaor_other
           CHANGING
             object_key      = lv_key
             signature       = lt_bds_signature
             files           = lt_file
           EXCEPTIONS
             OTHERS          = 7 ).
        IF sy-subrc = 0.
          " new version
          zcl_aqo_helper=>oaor_get_files(
           EXPORTING
            iv_pack_id   = option->ms_db_item-package_id
            iv_option_id = option->ms_db_item-option_id
            iv_filename  = ls_file-filename
           IMPORTING
            es_oaor_last = ls_oaor_file ).
        ENDIF.

**********************************************************************
        " Set new version
      WHEN mc_oaor-new_version.
        cl_bds_document_set=>create_version_with_files(
           EXPORTING
             classname       = option->ms_db_item-package_id
             classtype       = zcl_aqo_helper=>mc_oaor_other
             object_key      = lv_key
             doc_id          = ls_oaor_file-doc_id
             doc_ver_no      = ls_oaor_file-doc_ver_no
             doc_var_id      = ls_oaor_file-doc_var_id
           IMPORTING
             new_doc_ver_no  = lv_new_doc_ver_no
           CHANGING
             files           = lt_file
             properties      = lt_property
           EXCEPTIONS
             OTHERS          = 7 ).
        IF sy-subrc = 0.
          ls_oaor_file-doc_ver_no = lv_new_doc_ver_no.
        ENDIF.

**********************************************************************
        " Update existing
      WHEN mc_oaor-update_version.
        cl_bds_document_set=>update_with_files(
         EXPORTING
          classname       = option->ms_db_item-package_id
          classtype       = zcl_aqo_helper=>mc_oaor_other
          object_key       = lv_key
          doc_id          = ls_oaor_file-doc_id
          doc_ver_no      = ls_oaor_file-doc_ver_no
          doc_var_id      = ls_oaor_file-doc_var_id
          x_force_update  = abap_true
         CHANGING
          files           = lt_file
          properties      = lt_property
         EXCEPTIONS
          OTHERS          = 7 ).

      WHEN OTHERS.
        MESSAGE 'Please check OAOR mode' TYPE 'X'.
    ENDCASE.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Put new file
    zcl_aqo_helper=>check_in_request(
     EXPORTING
       is_oaor_file  = ls_oaor_file
     CHANGING
       cv_task       = lv_task
       cv_ok_message = lv_message ).
    IF lv_message IS NOT INITIAL.
      MESSAGE lv_message TYPE 'S'.
    ENDIF.

    " Delete obselete data
    lv_last_index = lines( lt_oaor_file ) + 1.
    lv_last_index = lv_last_index - option->ms_db_item-prev_value_cnt.
    DO lv_last_index TIMES.
      READ TABLE lt_oaor_file INTO ls_oaor_file INDEX sy-index.
      CHECK sy-subrc = 0.

      zcl_aqo_helper=>oaor_delete_file(
       EXPORTING
        iv_pack_id   = option->ms_db_item-package_id
        iv_option_id = option->ms_db_item-option_id
        is_oaor_file = ls_oaor_file
       CHANGING
        cv_task      = lv_task ).
    ENDDO.
  ENDMETHOD.

  METHOD show_new_version_diloag.
    " Show in screen PARAMETERS:
    DATA ls_dyn_scr  TYPE REF TO zsaqo_oaor_dialog.
    DATA lo_screen   TYPE REF TO zcl_eui_screen.
    DATA lo_err      TYPE REF TO zcx_eui_exception.
    DATA lv_input    TYPE screen-input.
    DATA lv_cmd      TYPE syucomm.

    CLEAR ev_ok.

    " Fill scrren with values
    CREATE DATA ls_dyn_scr.
    ls_dyn_scr->p_3_pack   = package_id.
    ls_dyn_scr->p_3_opt    = option_id.
    ls_dyn_scr->p_3_file   = cs_oaor_file-file_name.
    ls_dyn_scr->p_3_vers   = cs_oaor_file-doc_ver_no.
    " Editable
    ls_dyn_scr->p_3_desc   = cs_oaor_file-description.
    ls_dyn_scr->p_3_vis    = cs_oaor_file-visible.

    " Create screen manager
    TRY.
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr        = '1030'
            iv_cprog        = mc_prog-editor
            ir_context      = ls_dyn_scr
            iv_status_title = 'New file info'(nfi). " Set pf-status & text
      CATCH zcx_eui_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Ok & Cancel
    lv_input = '1'.
    IF me->tcode <> mc_prog-editor_tcode OR zcl_aqo_helper=>is_dev_mandt( ) <> abap_true.
      APPEND 'OK' TO lo_screen->ms_status-exclude.
      lv_input = '0'.
    ENDIF.

    " Static PF status no need on_pbo_event.
    lo_screen->customize( iv_fieldname = 'P_3_PACK'   input = '0' ).
    lo_screen->customize( iv_fieldname = 'P_3_OPT'    input = '0' ).
    lo_screen->customize( iv_fieldname = 'P_3_FILE'   input = '0' ).
    lo_screen->customize( iv_fieldname = 'P_3_VERS'   input = '0' ).
    lo_screen->customize( iv_fieldname = 'P_3_DESC'   input = lv_input ).
    lo_screen->customize( iv_fieldname = 'P_3_VIS'    input = lv_input ).

    " As popup
    lo_screen->popup( ).

    " Process action
    lv_cmd = lo_screen->show( ).
    CHECK lv_cmd <> zif_eui_manager=>mc_cmd-cancel.

    cs_oaor_file-description = ls_dyn_scr->p_3_desc.
    cs_oaor_file-visible     = ls_dyn_scr->p_3_vis.
    ev_ok                    = abap_true.
  ENDMETHOD.

  METHOD _attach_show.
    DATA:
      lt_oaor_file TYPE zcl_aqo_helper=>tt_oaor_file,
      ls_oaor_file TYPE REF TO zcl_aqo_helper=>ts_oaor_file,
      lt_return    TYPE STANDARD TABLE OF ddshretval WITH DEFAULT KEY,
      ls_return    TYPE REF TO ddshretval,
      ls_object_id TYPE sdokobject,
      lt_info      TYPE STANDARD TABLE OF sdokfilaci,
      ls_info      TYPE REF TO sdokfilaci,
      lt_text      TYPE STANDARD TABLE OF sdokcntasc,
      lt_bin       TYPE STANDARD TABLE OF sdokcntbin,
      lv_filetype  TYPE char10,
      lv_file_size TYPE i,
      lv_path      TYPE string,
      lv_sep       TYPE char1,
      lv_filename  TYPE string,
      lv_ext       TYPE string,
      lv_len       TYPE i,
      lv_index     TYPE i,
      lv_message   TYPE string,
      lv_task      TYPE e070-trkorr.
    FIELD-SYMBOLS:
     <lt_table>   TYPE STANDARD TABLE.

    zcl_aqo_helper=>oaor_get_files(
     EXPORTING
       iv_pack_id   = option->ms_db_item-package_id
       iv_option_id = option->ms_db_item-option_id
     IMPORTING
       et_oaor_file = lt_oaor_file ).

    " Visible for end users
    DO 1 TIMES.
      CHECK iv_vis_only = abap_true.

      " Hide tech files
      DELETE lt_oaor_file WHERE visible <> abap_true OR last_version <> abap_true.
      lv_index = lines( lt_oaor_file ).

      " Oops!
      CHECK lv_index = 0.
      MESSAGE 'No user guide was found' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDDO.

    " Just read first item
    IF lv_index = 1.
      READ TABLE lt_oaor_file REFERENCE INTO ls_oaor_file INDEX 1.
    ELSE.
      " Show dialog
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          ddic_structure   = 'ZSAQO_OAOR_FILE_F4'
          retfield         = 'TABIX'
          callback_program = 'SAPLZFG_AQO_MENU'
          callback_form    = 'CALLBACK_OAOR_F4'
          value_org        = 'S'
        TABLES
          value_tab        = lt_oaor_file
          return_tab       = lt_return
        EXCEPTIONS
          OTHERS           = 3.
      CHECK sy-subrc = 0.

      " First item
      READ TABLE lt_return REFERENCE INTO ls_return INDEX 1.
      CHECK sy-subrc = 0.

      " Read by TABIX in table
      lv_index = ls_return->fieldval.
      READ TABLE lt_oaor_file REFERENCE INTO ls_oaor_file
        WITH KEY tabix = lv_index.
    ENDIF.

    " Is Ok
    CHECK sy-subrc = 0.

    " delete
    IF iv_delete = abap_true.
      " Request for deleting file
      zcl_aqo_helper=>oaor_check_exists(
       EXPORTING
         iv_pack_id    = option->ms_db_item-package_id
         iv_option_id  = option->ms_db_item-option_id
       IMPORTING
         ev_task       = lv_task
         ev_ok_message = lv_message ).
      IF lv_message IS NOT INITIAL.
        MESSAGE lv_message TYPE 'S'.
      ENDIF.

      IF lv_task IS NOT INITIAL.
        zcl_aqo_helper=>oaor_delete_file(
         EXPORTING
           iv_pack_id   = option->ms_db_item-package_id
           iv_option_id = option->ms_db_item-option_id
           is_oaor_file = ls_oaor_file->*
         CHANGING
           cv_task      = lv_task ).
      ENDIF.
      RETURN.
    ENDIF.

    MOVE-CORRESPONDING ls_oaor_file->* TO ls_object_id.
    CALL FUNCTION 'SDOK_PHIO_LOAD_CONTENT'
      EXPORTING
        object_id           = ls_object_id
        text_as_stream      = abap_true
      TABLES
        file_access_info    = lt_info
        file_content_ascii  = lt_text
        file_content_binary = lt_bin
      EXCEPTIONS
        OTHERS              = 5.
    CHECK sy-subrc = 0.

    READ TABLE lt_info REFERENCE INTO ls_info INDEX 1.
    CHECK sy-subrc = 0.

    " Text or binary
    IF lt_bin[] IS NOT INITIAL.
      ASSIGN lt_bin  TO <lt_table>.
      lv_filetype  = 'BIN'.
      lv_file_size = ls_info->file_size.
    ELSE.
      ASSIGN lt_text TO <lt_table>.
      lv_filetype  = 'ASC'. " <---  TODO use ZCL_EUI_FILE --->
    ENDIF.

    " No need to clean files (cl_gui_frontend_services=>file_delete). SAP gui cleans 'SAP GUI\tmp\' automatically
    cl_gui_frontend_services=>get_temp_directory( CHANGING temp_dir = lv_path EXCEPTIONS OTHERS = 1 ).
    CHECK sy-subrc = 0.

    " Add file separator
    cl_gui_frontend_services=>get_file_separator(
     CHANGING
       file_separator = lv_sep ).
    cl_gui_cfw=>flush( ).

    " Extract info & add
    zcl_eui_file=>split_file_path(
     EXPORTING
       iv_fullpath   = ls_oaor_file->file_name
     IMPORTING
       ev_file_noext = lv_filename
       ev_extension  = lv_ext ).

    " Whole path
    lv_len = strlen( lv_path ) - 1.
    IF lv_path+lv_len(1) <> lv_sep.
      CONCATENATE lv_path lv_sep INTO lv_path.
    ENDIF.
    CONCATENATE lv_path lv_filename ` ` sy-datum `-` sy-uzeit `.` lv_ext INTO lv_path.

    " Download
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename      = lv_path
        filetype      = lv_filetype
        bin_filesize  = lv_file_size
      CHANGING
        data_tab      = <lt_table>
      EXCEPTIONS
        OTHERS        = 24 ).
    CHECK sy-subrc = 0.

    cl_gui_cfw=>flush( ).
    cl_gui_frontend_services=>execute(
     EXPORTING
      document               = lv_path
      operation              = 'OPEN'
     EXCEPTIONS
      OTHERS                 = 1 ).
    CHECK sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
  ENDMETHOD.

  METHOD _attach_delete.
    CHECK zcl_eui_screen=>confirm(
        iv_title    = 'Confirmation'(cnf)
        iv_question = 'Deleting file is irreversible. Continue?'(def)
        iv_icon_1   = 'ICON_DELETE_TEMPLATE' ) = abap_true.

    " Delete in attachments
    _attach_show( iv_delete = abap_true ).
  ENDMETHOD.

ENDCLASS.
