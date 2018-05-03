*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_opt IMPLEMENTATION.
  METHOD initialization.
    DATA:
      lo_opt     TYPE REF TO zcl_aqo,
      ls_own_opt TYPE REF TO ts_own_opt,
      lv_new_ui  TYPE abap_bool,
      lv_index   TYPE sytabix.

    " If forced to use
    zcl_aqo_util=>edit_transaction(
     IMPORTING
      ev_object    = ms_srtat_param-object
      ev_subobject = ms_srtat_param-subobject
      ev_new_ui    = lv_new_ui ).

    " Read own options
    read_own_opt(
     IMPORTING
       eo_opt     = lo_opt
       es_own_opt = ls_own_opt ).

    " old ui ?
    READ TABLE ls_own_opt->old_ui TRANSPORTING NO FIELDS
     WITH TABLE KEY uname = sy-uname.
    CHECK sy-subrc = 0.
    lv_index = sy-tabix.

    IF lv_new_ui = abap_true.
      DELETE ls_own_opt->old_ui INDEX lv_index.
      save_own_opt( lo_opt ).
      RETURN.
    ENDIF.

    " Use old one
    SUBMIT zaqo_edit_old VIA SELECTION-SCREEN. " AND RETURN.
  ENDMETHOD.

  METHOD read_own_opt.

    CREATE:
      DATA   es_own_opt,
      OBJECT eo_opt EXPORTING
        iv_object    = mc_own_object
        iv_subobject = mc_own_subobject
        ir_data      = es_own_opt.

    " Saved only 1 time
    IF eo_opt->read( ) IS NOT INITIAL.
      eo_opt->save( iv_confirm = abap_false iv_message = abap_false ).
    ENDIF.
  ENDMETHOD.

  METHOD save_own_opt.
    " Lock
    CHECK io_opt->lock( ) = abap_true.

    " Save
    io_opt->save( iv_confirm = abap_false iv_message = abap_false ).

    " Unlock
    CHECK io_opt->lock( iv_unlock = abap_true ) = abap_true.

    " All ok
    rv_ok = abap_true.
  ENDMETHOD.

  METHOD pbo.
    DATA:
      lo_cont TYPE REF TO cl_gui_custom_container,
      lv_data TYPE string.

    SET PF-STATUS 'ST_MAIN'.
    SET TITLEBAR 'ST_TITLE'.

    " One time only
    CHECK mo_html_viewer IS INITIAL.

    CREATE OBJECT:
      lo_cont EXPORTING
        container_name = 'CUST_CONTROL',

      mo_html_viewer EXPORTING
        io_parent = lo_cont. " cl_gui_container=>screen0.

    load_data( iv_objid  = 'Z_AQO_UTIL_JS'
               iv_url    = 'z_aqo_util_js.w3mi.data.js' ).

    load_data( iv_objid  = 'Z_AQO_MAIN_CONTROLLER'
               iv_url    = 'z_aqo_main_controller.w3mi.data.js' ).

    load_data( iv_objid  = 'Z_AQO_MULTI_UI'
               iv_url    = 'z_aqo_multi_ui.w3mi.data.js' ).

    load_data( iv_objid  = 'Z_AQO_INDEX_HTML'
               iv_url    = 'index.html' ).

    mo_html_viewer->show_url( url = 'index.html'  ).
  ENDMETHOD.

  METHOD get_options.
    TYPES:
      BEGIN OF ts_smw0,
        id    TYPE string,
        objid TYPE wwwdata-objid,
      END OF ts_smw0,

      " DB
      BEGIN OF ts_cluster.
        INCLUDE TYPE ztaqo_data.
    TYPES:
      size TYPE i,
      END OF ts_cluster,

      " View
      BEGIN OF ts_model,
        object    TYPE ztaqo_data-object,
        subobject TYPE ztaqo_data-subobject,
        uname     TYPE ztaqo_data-uname,
        udate     TYPE char8, "ZTAQO_DATA-udate, for search without delimeters
        utime     TYPE char6, "ZTAQO_DATA-utime, for search without delimeters
        fav       TYPE xsdboolean,
        size      TYPE i,
      END OF ts_model.
    DATA:
      lt_cluster   TYPE STANDARD TABLE OF ts_cluster,
      ls_cluster   TYPE REF TO ts_cluster,
      lt_model     TYPE STANDARD TABLE OF ts_model,
      ls_model     TYPE ts_model,
      lv_model     TYPE string,
      lv_i18n      TYPE string,
      lv_objid     TYPE wwwdata-objid,
      ls_own_opt   TYPE REF TO ts_own_opt,
      lv_param     TYPE string,
      lt_smw0      TYPE STANDARD TABLE OF ts_smw0 WITH DEFAULT KEY,
      ls_smw0      TYPE REF TO ts_smw0,
      lt_comp      TYPE abap_component_tab,
      ls_component TYPE REF TO abap_compdescr,
      ls_comp      TYPE abap_componentdescr,
      lo_struct    TYPE REF TO cl_abap_structdescr,
      lr_struc     TYPE REF TO data.
    FIELD-SYMBOLS:
      <ls_struc> TYPE any,
      <lv_field> TYPE string.

    " Read own options
    read_own_opt(
     IMPORTING
       es_own_opt = ls_own_opt ).

    " Saved options
    SELECT object subobject uname udate utime COUNT(*) AS size INTO CORRESPONDING FIELDS OF TABLE lt_cluster
    FROM ztaqo_data
    GROUP BY object subobject uname udate utime.

    " Manual convert date and time
    LOOP AT lt_cluster REFERENCE INTO ls_cluster WHERE object    <> mc_own_object
                                                    OR subobject <> mc_own_subobject.
      CLEAR ls_model.
      MOVE-CORRESPONDING ls_cluster->* TO ls_model.

      " Size in kb (* width in bytes)
      ls_model-size = ls_model-size * 3103 / 1024.

      " Check is
      READ TABLE ls_own_opt->fav TRANSPORTING NO FIELDS
       WITH TABLE KEY uname = sy-uname object = ls_cluster->object subobject = ls_cluster->subobject.
      IF sy-subrc = 0.
        ls_model-fav = abap_true.
      ENDIF.

      INSERT ls_model INTO TABLE lt_model.
    ENDLOOP.

    " Favorite first
    SORT lt_model BY fav DESCENDING object subobject.
    lv_model = zcl_aqo_util=>to_json( im_data = lt_model ).

    " Save to file
    save_json( iv_json     = lv_model
               iv_filename = 'sel.json' ).

    " Translation texts
    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          WRITE sy-langu TO lv_objid.
        WHEN 2.
          lv_objid = 'EN'.
      ENDCASE.

      CONCATENATE 'Z_AQO_I18N_' lv_objid '_JSON' INTO lv_objid.

      lv_i18n = load_from_smw0( iv_objid = lv_objid ).
      IF lv_i18n IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDDO.

    " get smw0 ids
    zcl_aqo_util=>from_json(
     EXPORTING
       iv_json = iv_smw0
     IMPORTING
       ex_data = lt_smw0 ).

    " Get all fields
    lo_struct ?= cl_abap_structdescr=>describe_by_data( ms_srtat_param ).

    ls_comp-type = cl_abap_elemdescr=>get_string( ).
    LOOP AT lo_struct->components REFERENCE INTO ls_component.
      ls_comp-name = ls_component->name.
      APPEND ls_comp TO lt_comp.
    ENDLOOP.

    " Fields of structure
    LOOP AT lt_smw0 REFERENCE INTO ls_smw0.
      ls_comp-name = ls_smw0->id.
      APPEND ls_comp TO lt_comp.
    ENDLOOP.
    lo_struct = cl_abap_structdescr=>create( lt_comp ).

    " Create run-time structure
    CREATE DATA lr_struc TYPE HANDLE lo_struct.
    ASSIGN lr_struc->* TO <ls_struc>.
    MOVE-CORRESPONDING ms_srtat_param TO <ls_struc>.

    " Copy values
    LOOP AT lt_smw0 REFERENCE INTO ls_smw0.
      ASSIGN COMPONENT ls_smw0->id OF STRUCTURE <ls_struc> TO <lv_field>.
      <lv_field> = load_from_smw0( iv_objid = ls_smw0->objid ).
    ENDLOOP.

    " Pass initial values
    lv_param = zcl_aqo_util=>to_json( im_data = <ls_struc> iv_pure = abap_true ).

    " Whole command
    CONCATENATE `"` iv_guid `", ` lv_model `, ` lv_i18n `, ` lv_param INTO lv_param.
    mo_html_viewer->run_js( iv_function   = 'call_back'
                            iv_param      = lv_param ).
  ENDMETHOD.

  METHOD load_from_smw0.
    DATA: ls_key    TYPE wwwdatatab,
          lv_size_c TYPE wwwparams-value,
          lv_size   TYPE i,
          lt_w3mime TYPE w3mimetabtype.

    ls_key-relid = 'MI'.
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
        mime              = lt_w3mime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.
    CHECK sy-subrc = 0.

    lv_size = lv_size_c.
    rv_data = zcl_aqo_util=>binary_to_string( it_table  = lt_w3mime
                                iv_length = lv_size ).
  ENDMETHOD.

  METHOD load_data.
    mo_html_viewer->load_mime_object(
      EXPORTING
        object_id    = iv_objid
        object_url   = iv_url
      IMPORTING
        assigned_url = rv_url
      EXCEPTIONS
        OTHERS       = 1 ) .
  ENDMETHOD.

  METHOD pai.
    CASE iv_cmd.
      WHEN 'ESCAPE'.
        mo_html_viewer->run_js( iv_function = 'closeDialog' ).

    ENDCASE.
  ENDMETHOD.

  METHOD show_option.
    TYPES:
      BEGIN OF ts_t000,
        mandt TYPE t000-mandt,
        mtext TYPE t000-mtext,
      END OF ts_t000,

      BEGIN OF ts_json,
        read_only TYPE xsdboolean,
        dev_mandt TYPE xsdboolean,

        fld_opt   LIKE mo_opt->mt_field_opt,
        copy_to   TYPE STANDARD TABLE OF ts_t000 WITH DEFAULT KEY,

        last_call TYPE abap_callstack_line,
      END OF ts_json.

    DATA:
      ls_json     TYPE ts_json,
      lv_json     TYPE string,
      lv_filename TYPE string.
    CREATE OBJECT mo_opt
      EXPORTING
        iv_object         = iv_object
        iv_subobject      = iv_subobject
        iv_save_last_call = abap_false.

    mo_opt->read( ).

    " In prod just edit text
    ls_json-dev_mandt = zcl_aqo_util=>is_dev_mandt( ).

    " All fields are gray
    IF mo_opt->lock( ) <> abap_true.
      ls_json-read_only = abap_true.

      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " All fields options
    ls_json-fld_opt = mo_opt->mt_field_opt.

    " Copy to another MANDT
    SELECT mandt mtext INTO CORRESPONDING FIELDS OF TABLE ls_json-copy_to
    FROM t000
    WHERE mandt <> sy-mandt.

    " Where used
    ls_json-last_call = mo_opt->ms_last_call.

    lv_json = zcl_aqo_util=>to_json( im_data = ls_json ).

    " Save to file
    CONCATENATE iv_object '-' iv_subobject '.json' INTO lv_filename.
    save_json( iv_json     = lv_json
               iv_filename = lv_filename ).

    CONCATENATE `"` iv_guid `", ` lv_json INTO lv_json.
    mo_html_viewer->run_js( iv_function   = 'call_back'
                            iv_param      = lv_json ). " As json object
  ENDMETHOD.

  METHOD call_old_ui.
    DATA:
      ls_own_opt TYPE REF TO ts_own_opt,
      lo_opt     TYPE REF TO zcl_aqo,
      ls_uname   TYPE ts_uname.

    read_own_opt(
     IMPORTING
       eo_opt     = lo_opt
       es_own_opt = ls_own_opt ).

    " Switch to old UI
    ls_uname-uname = sy-uname.
    INSERT ls_uname INTO TABLE ls_own_opt->old_ui.

    " Save opt
    save_own_opt( lo_opt ).

    " And start another transaction
    zcl_aqo_util=>edit_transaction(
     iv_object    = iv_object
     iv_subobject = iv_subobject
     iv_new_ui    = abap_false ).
  ENDMETHOD.

  METHOD value_request.
    DATA:
      lt_return    TYPE STANDARD TABLE OF ddshretval,
      ls_return    TYPE REF TO ddshretval,
      lv_tabname   TYPE dfies-tabname,
      lv_fieldname TYPE dfies-fieldname,
      lv_param     TYPE string.

    zcl_aqo_util=>split_type(
     EXPORTING
       iv_datatype = iv_datatype
     IMPORTING
       ev_table    = lv_tabname
       ev_field    = lv_fieldname ).
    CHECK lv_fieldname IS NOT INITIAL.

    " Show SH
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname    = lv_tabname
        fieldname  = lv_fieldname
      TABLES
        return_tab = lt_return
      EXCEPTIONS
        OTHERS     = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    " 1 value
    READ TABLE lt_return REFERENCE INTO ls_return INDEX 1.
    CHECK sy-subrc = 0.

    CONCATENATE `"` iv_guid `", "` ls_return->fieldval `"` INTO lv_param.
    mo_html_viewer->run_js( iv_function = 'call_back'
                            iv_param    = lv_param ).
  ENDMETHOD.

  METHOD range_request.
    DATA:
      ls_tabfld TYPE rstabfield,
      lo_type   TYPE REF TO cl_abap_datadescr,
      lr_range  TYPE REF TO data,
      lv_ok     TYPE abap_bool,
      lv_param  TYPE string,
      lo_struc  TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS:
      <lt_range> TYPE STANDARD TABLE.

    zcl_aqo_util=>split_type(
     EXPORTING
       iv_datatype = iv_datatype
     IMPORTING
       ev_table    = ls_tabfld-tablename
       ev_field    = ls_tabfld-fieldname ).
    CHECK ls_tabfld-fieldname IS NOT INITIAL.

    " 1 field
    lo_type = zcl_aqo_util=>create_type_descr( iv_rollname = iv_datatype ).
    IF sy-subrc <> 0.
      MESSAGE s016(zaqo_mes) WITH iv_datatype DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Table of range
    lo_struc = zcl_aqo_util=>create_structure( io_range = lo_type ).
    lo_type = cl_abap_tabledescr=>create(
      p_line_type = lo_struc ).

    " Create table and assigned it
    CREATE DATA lr_range TYPE HANDLE lo_type.
    ASSIGN lr_range->* TO <lt_range>.

    " Try to convert
    zcl_aqo_util=>from_json(
     EXPORTING
       iv_json = iv_ranges
     IMPORTING
       ex_data = <lt_range>
       ev_ok   = lv_ok ).
    IF lv_ok <> abap_true.
      MESSAGE s017(zaqo_mes) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Show ranges
    CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
      EXPORTING
        title         = iv_title
        tab_and_field = ls_tabfld
        "just_display  = '' TODO
      TABLES
        range         = <lt_range>
      EXCEPTIONS
        OTHERS        = 1.
    CHECK sy-subrc = 0.

    lv_param = zcl_aqo_util=>to_json( im_data = <lt_range> ).
    CONCATENATE `"` iv_guid `", ` lv_param INTO lv_param.
    mo_html_viewer->run_js( iv_function  = 'call_back'
                            iv_param     = lv_param ).
  ENDMETHOD.

  METHOD save_option.
    DATA:
      lt_fld_opt     LIKE mo_opt->mt_field_opt,
      lv_ok          TYPE abap_bool,
      lt_empty_field TYPE stringtab,
      lv_field       TYPE string,
      lv_param       TYPE string.
    FIELD-SYMBOLS:
      <ls_fld_opt> LIKE LINE OF lt_fld_opt.

    " Try to convert
    zcl_aqo_util=>from_json(
     EXPORTING
       iv_json = iv_option
     IMPORTING
       ex_data = lt_fld_opt
       ev_ok   = lv_ok ).
    IF lv_ok <> abap_true.
      MESSAGE s017(zaqo_mes) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT lt_fld_opt ASSIGNING <ls_fld_opt>.
      IF <ls_fld_opt>-text IS INITIAL.
        MESSAGE e001(zaqo_mes) WITH <ls_fld_opt>-name.
      ENDIF.

      " Make the fields look the similar
      TRANSLATE <ls_fld_opt>-rollname TO UPPER CASE.

      " Check type
      IF ( <ls_fld_opt>-kind = zcl_aqo_util=>mc_kind_parameter
        OR <ls_fld_opt>-kind = zcl_aqo_util=>mc_kind_select_option ) AND
         zcl_aqo_util=>create_type_descr( iv_rollname = <ls_fld_opt>-rollname ) IS INITIAL.
        MESSAGE e007(zaqo_mes) WITH <ls_fld_opt>-name.
      ENDIF.
    ENDLOOP.

    " Set new value and check it
    mo_opt->mt_field_opt = lt_fld_opt.
    lt_empty_field = mo_opt->read( iv_safe_read = abap_true ).

    IF lt_empty_field IS INITIAL.
      lv_param = 'true'.
      mo_opt->save( iv_confirm     = abap_false
                    iv_use_me_data = abap_false
                    iv_mandt       = iv_mandt ).
    ELSE.
      lv_param = 'false'.
    ENDIF.
    CONCATENATE `"` iv_guid `", ` lv_param INTO lv_param.
    mo_html_viewer->run_js( iv_function = 'call_back' iv_param = lv_param ).

    " Show messages
    LOOP AT lt_empty_field INTO lv_field.
      READ TABLE lt_fld_opt ASSIGNING <ls_fld_opt>
       WITH KEY name = lv_field.
      CHECK sy-subrc = 0.

      IF <ls_fld_opt>-kind = zcl_aqo_util=>mc_kind_table.
        MESSAGE e005(zaqo_mes) WITH <ls_fld_opt>-text.
      ELSE.
        MESSAGE e018(zaqo_mes) WITH <ls_fld_opt>-text.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD transport_option.
    mo_opt->transport( ).
  ENDMETHOD.

  METHOD delete_option.
    DATA:
      lv_param TYPE string.

    IF mo_opt->delete( iv_confirm	= abap_false ) = abap_true.
      lv_param = 'true'.
    ELSE.
      lv_param = 'false'.
    ENDIF.

    CONCATENATE `"` iv_guid `", ` lv_param INTO lv_param.
    mo_html_viewer->run_js( iv_function = 'call_back' iv_param = lv_param ).
  ENDMETHOD.

  METHOD deep_scan.
    DATA:
      lt_usage TYPE zcl_aqo_util=>tt_usage,
      lv_json  TYPE string.

    " Get as table
    lt_usage = zcl_aqo_util=>get_usage(
     iv_object    = mo_opt->ms_key-object
     iv_subobject = mo_opt->ms_key-subobject ).

    " Convert to JSON
    lv_json = zcl_aqo_util=>to_json( lt_usage ).
    save_json( iv_json     = lv_json
               iv_filename = 'usage.json' ).

    " Show result
    CONCATENATE `"` iv_guid `", ` lv_json INTO lv_json.
    mo_html_viewer->run_js( iv_function   = 'call_back'
                            iv_param      = lv_json ).
  ENDMETHOD.

  METHOD set_favorite.
    DATA:
      ls_fav     TYPE ts_fav,
      lv_ok      TYPE string,
      lv_param   TYPE string,
      lo_opt     TYPE REF TO zcl_aqo,
      ls_own_opt TYPE REF TO ts_own_opt.

    " Read own options
    read_own_opt(
     IMPORTING
       eo_opt     = lo_opt
       es_own_opt = ls_own_opt ).

    ls_fav-uname     = sy-uname.
    ls_fav-object    = iv_object.
    ls_fav-subobject = iv_subobject.

    lv_ok = 'false'.
    DO 1 TIMES.
      IF iv_favorite = 'true'.
        INSERT ls_fav INTO TABLE ls_own_opt->fav.
      ELSE.
        DELETE ls_own_opt->fav WHERE uname     = ls_fav-uname
                                 AND object    = ls_fav-object
                                 AND subobject = ls_fav-subobject.
      ENDIF.

      " Write back
      CHECK save_own_opt( lo_opt ) = abap_true.
      lv_ok = 'true'.
    ENDDO.

    " Show own messages
    IF lv_ok <> 'true'.
      MESSAGE TEXT-m01 TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      CASE iv_favorite.
        WHEN 'true'.
          MESSAGE TEXT-m02 TYPE 'S'.
        WHEN 'false'.
          MESSAGE TEXT-m03 TYPE 'S'.
      ENDCASE.
    ENDIF.

    CONCATENATE `"` iv_guid `", "` lv_ok `"` INTO lv_param.
    mo_html_viewer->run_js( iv_function = 'call_back'
                            iv_param    = lv_param ).
  ENDMETHOD.

  METHOD save_json.
    DATA:
      ls_own_opt TYPE REF TO ts_own_opt.

    " No need
    IF mo_opt IS NOT INITIAL.
      CHECK mo_opt->ms_key-subobject <> mc_own_subobject.
    ENDIF.

    " Current option
    read_own_opt(
     IMPORTING
      es_own_opt = ls_own_opt ).

    " For test only
    CHECK ls_own_opt->debug = abap_true AND ls_own_opt->path IS NOT INITIAL.

    " For testing
    CONCATENATE ls_own_opt->path iv_filename INTO iv_filename.
    zcl_aqo_util=>download(
     iv_content  = iv_json
     iv_filename = iv_filename ).

    BREAK-POINT.
  ENDMETHOD.
ENDCLASS.


*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pbo_100 OUTPUT.
  lcl_opt=>pbo( ).
ENDMODULE.

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_100 INPUT.
  lcl_opt=>pai( gv_ok_code ).
ENDMODULE.

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_exit INPUT.
  lcl_opt=>pai( 'ESCAPE' ).
ENDMODULE.
