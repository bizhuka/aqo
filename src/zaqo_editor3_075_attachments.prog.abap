*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_attachment DEFINITION INHERITING FROM lcl_tab FINAL FRIENDS zcl_eui_event_caller.
  PUBLIC SECTION.
    METHODS:
      pbo REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      _fill_table       REDEFINITION,
      _get_layout       REDEFINITION,
      _get_catalog      REDEFINITION,
      _get_filter       REDEFINITION,
      _get_toolbar      REDEFINITION,
      _on_hotspot_click REDEFINITION,
      _on_user_command  REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF mc_button,
        import TYPE syucomm VALUE '_IMPORT',
        delete TYPE syucomm VALUE '_DELETE',
      END OF mc_button,

      BEGIN OF mc_oaor,
        new_file       TYPE string VALUE 'NEW_FILE',
        new_version    TYPE string VALUE 'NEW_VERSION',
        update_version TYPE string VALUE 'UPDATE_VERSION',
      END OF mc_oaor,

      BEGIN OF mc,
        oaor_other TYPE bapibds01-classtype VALUE 'OT',
      END OF mc.

    TYPES:
      ts_oaor_file TYPE zsaqo_oaor_file,
      tt_oaor_file TYPE STANDARD TABLE OF ts_oaor_file WITH DEFAULT KEY.

    DATA:
      mt_oaor_file TYPE tt_oaor_file.

    METHODS:
      _get_files
        IMPORTING
          iv_filename  TYPE csequence OPTIONAL
          iv_last_only TYPE abap_bool
        EXPORTING
          es_oaor_last TYPE ts_oaor_file
          et_oaor_file TYPE tt_oaor_file,

      _on_import,

      _on_delete
        IMPORTING
          io_grid TYPE REF TO cl_gui_alv_grid,

      _delete_previous
        IMPORTING
          iv_filename TYPE csequence,

      _import_get_version
        EXPORTING
          et_file      TYPE sbdst_files
          es_oaor_file TYPE ts_oaor_file
          ev_oaor_mode TYPE string
          ev_ext       TYPE string,

      _check_exists
        EXPORTING
          ev_task       TYPE e070-trkorr
          ev_ok_message TYPE csequence,

      _show_bds_locl_popup
        CHANGING
          cs_bds_locl TYPE bds_locl,

      _diloag_screen
        IMPORTING
          iv_doc_ver_no  TYPE ts_oaor_file-doc_ver_no
          it_listbox     TYPE vrm_values
          iv_title       TYPE csequence
        EXPORTING
          ev_ok          TYPE abap_bool
        CHANGING
          cv_file_name   TYPE ts_oaor_file-file_name
          cv_description TYPE ts_oaor_file-description,

      _change_file_name
        IMPORTING
          iv_file_name TYPE csequence
        CHANGING
          cs_file      TYPE bapifiles
        RAISING
          zcx_eui_exception,

      _find_request
        IMPORTING
          is_oaor_file  TYPE ts_oaor_file
        CHANGING
          cv_task       TYPE e070-trkorr
          cv_ok_message TYPE csequence,

      _delete_file
        IMPORTING
          is_oaor_file TYPE ts_oaor_file.
ENDCLASS.

CLASS lcl_attachment IMPLEMENTATION.
  METHOD pbo.
    GET REFERENCE OF mt_oaor_file INTO mr_table.
    super->pbo( ).
  ENDMETHOD.

  METHOD _fill_table.
    _get_files( EXPORTING iv_last_only = abap_false
                IMPORTING et_oaor_file = mt_oaor_file[] ).
  ENDMETHOD.

  METHOD _get_layout.
    rs_layout = super->_get_layout( ).
    "rs_layout-sel_mode   = 'A'.
    rs_layout-grid_title = 'OAOR attachments'(oao).
  ENDMETHOD.

  METHOD _get_catalog.
    DATA lr_catalog TYPE REF TO lvc_s_fcat.
    APPEND INITIAL LINE TO rt_catalog REFERENCE INTO lr_catalog.
    lr_catalog->fieldname = '*'.
    lr_catalog->tech      = 'X'.

    DATA ls_visible_fld TYPE ts_oaor_file-f4.
    DATA lo_struc TYPE REF TO cl_abap_structdescr.
    lo_struc ?= cl_abap_structdescr=>describe_by_data( ls_visible_fld ).

    DATA lr_comp TYPE REF TO abap_compdescr.
    LOOP AT lo_struc->components REFERENCE INTO lr_comp.
      APPEND INITIAL LINE TO rt_catalog REFERENCE INTO lr_catalog.
      lr_catalog->fieldname = lr_comp->name.
      lr_catalog->tech      = abap_undefined.

      CHECK lr_catalog->fieldname = 'FILE_NAME'.
      lr_catalog->hotspot = 'X'.
    ENDLOOP.
  ENDMETHOD.

  METHOD _get_filter.
    FIELD-SYMBOLS <ls_filter> LIKE LINE OF rt_filter.
    APPEND INITIAL LINE TO rt_filter ASSIGNING <ls_filter>.
    <ls_filter>-fieldname = 'LAST_VERSION'.
    <ls_filter>-sign      = 'I'.
    <ls_filter>-option    = 'EQ'.
    <ls_filter>-low       = abap_true.
  ENDMETHOD.

  METHOD _get_toolbar.
    FIELD-SYMBOLS <ls_button> LIKE LINE OF rt_toolbar.

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-text         = <ls_button>-quickinfo    = 'Add file'(adf).
    <ls_button>-icon         = icon_insert_row.
    <ls_button>-function     = mc_button-import.

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-text         = <ls_button>-quickinfo    = 'Delete file'(def).
    <ls_button>-icon         = icon_delete_row.
    <ls_button>-function     = mc_button-delete.

    CHECK go_editor->is_editable( ) <> abap_true.
    LOOP AT rt_toolbar ASSIGNING <ls_button>.
      <ls_button>-disabled = abap_true.
    ENDLOOP.
  ENDMETHOD.

  METHOD _on_hotspot_click.
    CHECK e_column_id-fieldname = 'FILE_NAME'.

    FIELD-SYMBOLS <ls_oaor_file> LIKE LINE OF mt_oaor_file.
    READ TABLE mt_oaor_file INDEX e_row_id-index ASSIGNING <ls_oaor_file>.
    CHECK sy-subrc = 0.

    DATA ls_object_id TYPE sdokobject.
    MOVE-CORRESPONDING <ls_oaor_file> TO ls_object_id.

    DATA lt_info      TYPE STANDARD TABLE OF sdokfilaci.
    DATA lt_text      TYPE STANDARD TABLE OF sdokcntasc.
    DATA lt_bin       TYPE STANDARD TABLE OF sdokcntbin.
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

    DATA ls_info TYPE REF TO sdokfilaci.
    READ TABLE lt_info REFERENCE INTO ls_info INDEX 1.
    CHECK sy-subrc = 0.

    " Text or binary
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    DATA lv_filetype  TYPE char10.
    DATA lv_file_size TYPE i.
    IF lt_bin[] IS NOT INITIAL.
      ASSIGN lt_bin  TO <lt_table>.
      lv_filetype  = 'BIN'.
      lv_file_size = ls_info->file_size.
    ELSE.
      ASSIGN lt_text TO <lt_table>.
      lv_filetype  = 'ASC'.
    ENDIF.

    " Download and open
    DATA lo_file  TYPE REF TO zcl_eui_file.
    DATA lo_error TYPE REF TO zcx_eui_exception.
    CREATE OBJECT lo_file.
    TRY.
        lo_file->import_from_binary(
         it_table  = <lt_table>
         iv_length = lv_file_size ).

        lo_file->download(
          iv_full_path   = <ls_oaor_file>-file_name
          iv_filetype    = lv_filetype
          iv_save_dialog = abap_true ).

        lo_file->open( ).
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD _on_user_command.
    DATA lo_error TYPE REF TO zcx_aqo_exception.
    TRY.
        CASE e_ucomm.
          WHEN mc_button-import.
            _on_import( ).

          WHEN mc_button-delete.
            _on_delete( sender ).
        ENDCASE.
      CATCH zcx_aqo_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD _get_files.
    DATA:
      lv_key               TYPE sbdst_object_key,
      lt_sbdst_signature   TYPE sbdst_signature,
      ls_sbdst_signature   TYPE REF TO bapisignat,
      lt_sbdst_components2 TYPE sbdst_components2,
      ls_sbdst_components2 TYPE REF TO bapicompo2,
      ls_oaor_file         TYPE REF TO ts_oaor_file,
      lt_doc_id            TYPE SORTED TABLE OF ts_oaor_file-doc_id WITH UNIQUE KEY table_line,
      lv_field             TYPE string.
    FIELD-SYMBOLS:
      <l_val>              TYPE any.

    CLEAR:
      es_oaor_last,
      et_oaor_file.

    DATA lv_ok TYPE bds_locl-classname.
    SELECT SINGLE classname INTO lv_ok
    FROM bds_locl
    WHERE classname EQ ms_db_key-package_id
      AND classtype EQ mc-oaor_other.
    CHECK lv_ok IS NOT INITIAL.

    " Subfolder in OAOR (and classname = package_id)
    lv_key = ms_db_key-option_id.

    " Finding the right documents
    cl_bds_document_set=>get_info(
     EXPORTING  classname           = ms_db_key-package_id
                classtype           = mc-oaor_other
                object_key          = lv_key
     IMPORTING  extended_components = lt_sbdst_components2
     CHANGING   signature           = lt_sbdst_signature
     EXCEPTIONS OTHERS              = 7 ).
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

    " Only last
    IF iv_last_only = abap_true.
      DELETE et_oaor_file WHERE last_version <> abap_true.
    ENDIF.

    " Max versions last
    SORT et_oaor_file BY doc_id doc_ver_no ASCENDING.

    LOOP AT et_oaor_file REFERENCE INTO ls_oaor_file.
      ls_oaor_file->tabix      = sy-tabix.
      ls_oaor_file->package_id = ms_db_key-package_id.
      ls_oaor_file->option_id	 = ms_db_key-option_id.

      DATA lv_timestamp TYPE tzntstmps.
      DEFINE conv_date_time.
        lv_timestamp = ls_oaor_file->&1.
        CONVERT TIME STAMP lv_timestamp TIME ZONE sy-zonlo
          INTO DATE ls_oaor_file->&1_date TIME ls_oaor_file->&1_time.
      END-OF-DEFINITION.

      conv_date_time: created_at,
                      last_changed_at.
    ENDLOOP.

    " Get last version for 1 file
    CHECK iv_filename IS NOT INITIAL
      AND es_oaor_last IS REQUESTED.

    READ TABLE et_oaor_file INTO es_oaor_last
     WITH KEY last_version = abap_true
              file_name    = iv_filename.
  ENDMETHOD.

  METHOD _on_import.
    DATA lt_file           TYPE sbdst_files.
    DATA lr_file           TYPE REF TO bapifiles.
    DATA ls_oaor_file      TYPE ts_oaor_file.
    DATA lv_oaor_mode      TYPE string.
    DATA lv_ext            TYPE string.
    DATA lt_property       TYPE STANDARD TABLE OF bapiproper WITH DEFAULT KEY.
    DATA lr_property       TYPE REF TO bapiproper.
    DATA lv_ar_object      TYPE toadv-ar_object.
    DATA lt_bds_signature  TYPE sbdst_signature.
    DATA ls_bds_signature  TYPE bapisignat.
    DATA lv_key            TYPE sbdst_object_key.
    DATA lv_new_doc_ver_no TYPE sbdst_doc_ver_no.

    " Get new file info
    _import_get_version(
     IMPORTING
       et_file      = lt_file
       es_oaor_file = ls_oaor_file " new file
       ev_oaor_mode = lv_oaor_mode
       ev_ext       = lv_ext ).
    CHECK ls_oaor_file IS NOT INITIAL.

    " Only 1 file
    READ TABLE lt_file REFERENCE INTO lr_file INDEX 1.
    ls_bds_signature-doc_count = lr_file->doc_count.

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

**********************************************************************
    " Subfolder in OAOR (and classname = package_id)
    lv_key = ms_db_key-option_id. " ms_db_key-option_id

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
             classname       = ms_db_key-package_id
             classtype       = mc-oaor_other
           CHANGING
             object_key      = lv_key
             signature       = lt_bds_signature
             files           = lt_file
           EXCEPTIONS
             OTHERS          = 7 ).
        IF sy-subrc = 0.
          " new version
          _get_files(
           EXPORTING
            iv_filename  = lr_file->filename
            iv_last_only = abap_true
           IMPORTING
            es_oaor_last = ls_oaor_file ).
        ELSE.
          zcx_aqo_exception=>raise_sys_error( ).
        ENDIF.

**********************************************************************
        " Set new version
      WHEN mc_oaor-new_version.
        cl_bds_document_set=>create_version_with_files(
           EXPORTING
             classname       = ms_db_key-package_id
             classtype       = mc-oaor_other
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
        ELSE.
          zcx_aqo_exception=>raise_sys_error( ).
        ENDIF.

**********************************************************************
        " Update existing
      WHEN mc_oaor-update_version.
        cl_bds_document_set=>update_with_files(
         EXPORTING
          classname       = ms_db_key-package_id
          classtype       = mc-oaor_other
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
        IF sy-subrc <> 0.
          zcx_aqo_exception=>raise_sys_error( ).
        ENDIF.

      WHEN OTHERS.
        zcx_aqo_exception=>raise_dump( iv_message = 'Please check OAOR mode'(poa) ).
    ENDCASE.

    _delete_previous( lr_file->filename ).

    " Put new file in transport
    DATA lv_message TYPE string.
    DATA lv_task    TYPE e070-trkorr.
    DATA lo_error   TYPE REF TO zcx_aqo_exception.
    TRY.
        _find_request( EXPORTING is_oaor_file  = ls_oaor_file
                           CHANGING  cv_task       = lv_task
                                     cv_ok_message = lv_message ).
        IF lv_message IS NOT INITIAL.
          MESSAGE lv_message TYPE 'S'.
        ENDIF.
      CATCH zcx_aqo_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    _refresh_now( ).
  ENDMETHOD.

  METHOD _delete_previous.
    " Read previous
    DATA lt_all_version TYPE tt_oaor_file.
    _get_files( EXPORTING iv_filename  = iv_filename
                          iv_last_only = abap_false
                IMPORTING et_oaor_file = lt_all_version ).

    " Delete obselete data
    DATA lv_last_index TYPE i.
    lv_last_index = lines( lt_all_version ).
    lv_last_index = lv_last_index - go_editor->mo_option->ms_db_item-prev_value_cnt.
    DO lv_last_index TIMES.
      DATA ls_oaor_file LIKE LINE OF lt_all_version.
      READ TABLE lt_all_version INTO ls_oaor_file INDEX sy-index.
      CHECK sy-subrc = 0.

      _delete_file( is_oaor_file = ls_oaor_file ).
    ENDDO.
  ENDMETHOD.

  METHOD _on_delete.
    DATA lt_row TYPE lvc_t_row.
    io_grid->get_selected_rows( IMPORTING et_index_rows = lt_row ).
    IF lt_row[] IS INITIAL.
      MESSAGE 'Select files for deletion'(sfd) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CHECK zcl_eui_screen=>confirm(
        iv_title    = 'Confirmation'(cnf)
        iv_question = 'Deleting file is irreversible. Continue?'(dfi)
        iv_icon_1   = 'ICON_DELETE_TEMPLATE' ) = abap_true.

    " Request for deleting file
    DATA: lv_task    TYPE e070-trkorr,
          lv_message TYPE string.
    _check_exists(
     IMPORTING
       ev_task       = lv_task
       ev_ok_message = lv_message ).
    IF lv_message IS NOT INITIAL.
      MESSAGE lv_message TYPE 'S'.
    ENDIF.

    CHECK lv_task IS NOT INITIAL.

    " Delete form the end
    SORT lt_row BY index DESCENDING.

    FIELD-SYMBOLS: <ls_row>       LIKE LINE OF lt_row,
                   <ls_oaor_file> LIKE LINE OF mt_oaor_file.
    LOOP AT lt_row[] ASSIGNING <ls_row>.
      READ TABLE mt_oaor_file ASSIGNING <ls_oaor_file> INDEX <ls_row>-index.
      CHECK sy-subrc = 0.

      _delete_file( is_oaor_file = <ls_oaor_file> ).
      DELETE mt_oaor_file[] INDEX <ls_row>-index.
    ENDLOOP.

    _refresh_now( ).
  ENDMETHOD.

  METHOD _change_file_name.
    DATA lo_file       TYPE REF TO zcl_eui_file.
    DATA lv_temp_path  TYPE string.
    DATA lv_full_path  TYPE string.
    DATA lv_sep        TYPE char1.
    DATA lv_len        TYPE i.

    " 1-st upload file
    CREATE OBJECT lo_file.
    CONCATENATE cs_file-directory cs_file-filename INTO lv_full_path.
    lo_file->import_from_file( iv_full_path = lv_full_path ).

    " Save to temp dir No need to clean files (cl_gui_frontend_services=>file_delete). SAP gui cleans 'SAP GUI\tmp\' automatically
    cl_gui_frontend_services=>get_temp_directory(
     CHANGING
       temp_dir = lv_temp_path
     EXCEPTIONS
       OTHERS = 1 ).
    IF sy-subrc <> 0.
      zcx_eui_exception=>raise_sys_error( ).
    ENDIF.

    " Add file separator
    cl_gui_frontend_services=>get_file_separator(
     CHANGING
       file_separator = lv_sep ).
    cl_gui_cfw=>flush( ).

    lv_len = strlen( lv_temp_path ) - 1.
    IF lv_temp_path+lv_len(1) <> lv_sep.
      CONCATENATE lv_temp_path lv_sep INTO lv_temp_path.
    ENDIF.

    " Save to sub folter of temp
    lv_full_path = zcl_eui_conv=>guid_create( ).
    CONCATENATE lv_temp_path lv_full_path lv_sep INTO lv_temp_path.

    " 2-nd download with new name
    CONCATENATE lv_temp_path iv_file_name INTO lv_full_path.
    lo_file->download(
     iv_full_path   = lv_full_path
     iv_save_dialog = abap_false ).

    " Change if all ok
    cs_file-directory = lv_temp_path.
    cs_file-filename  = iv_file_name.
  ENDMETHOD.

  METHOD _check_exists.
    DATA:
      ls_bds_locl TYPE bds_locl,
      lo_error    TYPE REF TO zcx_aqo_exception.

    " select request/task
    CLEAR ev_task.
    TRY.
        zcl_aqo_helper=>find_request( EXPORTING iv_table_name = 'BDS_LOCL'
                                                iv_key1       = ms_db_key-package_id
                                                iv_key2       = mc-oaor_other
                                      CHANGING  cv_task       = ev_task
                                                " cv_request    = rv_request
                                                cv_ok_message = ev_ok_message ).
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
    WHERE classname = ms_db_key-package_id
      AND classtype = mc-oaor_other.
    CHECK sy-subrc <> 0.

    " Create new
    ls_bds_locl-classname = ms_db_key-package_id.
    ls_bds_locl-classtype = mc-oaor_other.
    ls_bds_locl-lo_class  = 'BDS_LOC2'.
    ls_bds_locl-ph_class  = 'BDS_POC2'.
    ls_bds_locl-re_class  = 'BDS_REC2'.
    ls_bds_locl-tabname   = 'BDS_CONN05'.
    ls_bds_locl-crea_user = sy-uname.
    CONCATENATE sy-datum sy-uzeit INTO ls_bds_locl-crea_time.

    _show_bds_locl_popup( CHANGING cs_bds_locl = ls_bds_locl ).
    IF ls_bds_locl IS INITIAL.
      CLEAR ev_task.
      ev_ok_message = 'BDS Assignment saving is canceled'(bac).
      RETURN.
    ENDIF.

    " Update DB
    INSERT bds_locl FROM ls_bds_locl.
  ENDMETHOD.

  METHOD _show_bds_locl_popup.
    DATA lo_screen TYPE REF TO zcl_eui_screen.
    TRY.
        DATA lr_context TYPE REF TO data.
        GET REFERENCE OF cs_bds_locl INTO lr_context.

        CREATE OBJECT lo_screen
          EXPORTING
            ir_context = lr_context
            iv_cprog   = 'ZAQO_EDIT3_BDS_POPUP'
            iv_dynnr   = zcl_eui_screen=>mc_dynnr-dynamic.

        DATA lo_error TYPE REF TO zcx_eui_exception.
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    lo_screen->customize( name = 'CLASSNAME' input = '0' ).
    lo_screen->customize( name = 'CLASSTYPE' input = '0' ).
    lo_screen->customize( name = 'CREA_USER' input = '0' ).
    lo_screen->customize( name = 'CREA_TIME' input = '0' ).

    DATA lv_col_end TYPE i.
    lo_screen->get_dimension( IMPORTING ev_col_end = lv_col_end ).
    lo_screen->popup( iv_col_end = lv_col_end ).

    IF lo_screen->show( ) <> 'OK'.
      CLEAR cs_bds_locl.
    ENDIF.
  ENDMETHOD.

  METHOD _import_get_version.
    DATA ls_result_oaor    LIKE es_oaor_file.
    DATA lt_many_file      TYPE tt_oaor_file.
    DATA lv_task           TYPE e070-trkorr.
    DATA lv_message        TYPE string.
    DATA lr_file           TYPE REF TO bapifiles.
    DATA ls_screen_file    TYPE ts_oaor_file.
    DATA lt_file_table     TYPE filetable.
    DATA ls_file_table     TYPE REF TO file_table.
    DATA lv_rc             TYPE i.
    DATA lv_action         TYPE i.
    DATA lv_ok             TYPE abap_bool.
    DATA lt_listbox        TYPE vrm_values.
    DATA lv_listbox_ind    TYPE num2.
    DATA lo_error          TYPE REF TO zcx_eui_exception.
    DATA lv_title          TYPE lvc_s_layo-grid_title.

    FIELD-SYMBOLS:
      <ls_result_oaor> TYPE ts_oaor_file,
      <ls_listbox>     TYPE vrm_value.

    " All is empty
    CLEAR:
      et_file,
      es_oaor_file,
      ev_oaor_mode,
      ev_ext.

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

    " Get 1-st
    READ TABLE lt_file_table REFERENCE INTO ls_file_table INDEX 1.
    CHECK sy-subrc = 0.

    " Extract info
    APPEND INITIAL LINE TO et_file REFERENCE INTO lr_file.
    zcl_eui_file=>split_file_path(
     EXPORTING
       iv_fullpath  = ls_file_table->filename
     IMPORTING
       ev_path      = lr_file->directory
       ev_filename  = lr_file->filename
       ev_extension = ev_ext ).

    " Create or not
    _check_exists(
     IMPORTING
       ev_task       = lv_task
       ev_ok_message = lv_message ).
    IF lv_message IS NOT INITIAL.
      MESSAGE lv_message TYPE 'S'.
    ENDIF.

    " Nope
    CHECK lv_task IS NOT INITIAL.

    " Always 1 file
    lr_file->comp_count = lr_file->doc_count = 1.
    " Always in UPPER CASE (ID). TODO mac or linux ? (case sensitive)
    TRANSLATE lr_file->filename TO UPPER CASE.

    " Read many files. Only last
    _get_files(
     EXPORTING
      " All last files iv_filename  = ls_file-filename
      iv_last_only = abap_true
     IMPORTING
      et_oaor_file = lt_many_file ).

**********************************************************************
    " Prepare listbox
**********************************************************************

    " Always 1-st position
    APPEND INITIAL LINE TO lt_listbox ASSIGNING <ls_listbox>.
    <ls_listbox>-key  = '01'.
    <ls_listbox>-text = lr_file->filename.

    " Other files list
    LOOP AT lt_many_file ASSIGNING <ls_result_oaor> WHERE file_name <> lr_file->filename.
      " Add as pair
      APPEND INITIAL LINE TO lt_listbox ASSIGNING <ls_listbox>.
      lv_listbox_ind    = sy-tabix.
      <ls_listbox>-key  = lv_listbox_ind.
      <ls_listbox>-text = <ls_result_oaor>-file_name.
    ENDLOOP.

**********************************************************************
    " Detect MODE
**********************************************************************

    " New or existeng item
    READ TABLE lt_many_file INTO ls_result_oaor
     WITH KEY file_name = lr_file->filename.
    IF ls_result_oaor IS INITIAL.
      ls_result_oaor-description = ls_result_oaor-file_name = lr_file->filename.
      ev_oaor_mode = mc_oaor-new_file.
      MESSAGE s036(zaqo_message) INTO lv_title.

    ELSEIF sy-datum = ls_result_oaor-last_changed_at_date.
      ev_oaor_mode = mc_oaor-update_version.
      MESSAGE s038(zaqo_message) WITH ls_result_oaor-doc_ver_no INTO lv_title.

    ELSE.
      ev_oaor_mode = mc_oaor-new_version.
      MESSAGE s037(zaqo_message) WITH ls_result_oaor-doc_ver_no INTO lv_title.
    ENDIF.

    " Also show as message
    MESSAGE lv_title TYPE 'S'.
**********************************************************************

    " Only now SET index in LSITBOX
    ls_result_oaor-file_name = '01'.

    ls_screen_file = ls_result_oaor.
    _diloag_screen(
     EXPORTING
       iv_doc_ver_no  = ls_screen_file-doc_ver_no
       it_listbox     = lt_listbox[]
       iv_title       = lv_title
     IMPORTING
       ev_ok          = lv_ok
     CHANGING
       cv_file_name   = ls_screen_file-file_name
       cv_description = ls_screen_file-description ).
    CHECK lv_ok = abap_true.

    " get from index
    DO 1 TIMES.
      " Get new name
      lv_listbox_ind = ls_screen_file-file_name.
      READ TABLE lt_listbox ASSIGNING <ls_listbox>
       WITH KEY key = lv_listbox_ind.

      " Get name by index
      CHECK sy-subrc  = 0.
      ls_result_oaor-file_name = <ls_listbox>-text.

**********************************************************************
      " Choosed another file name
**********************************************************************
      CHECK lv_listbox_ind <> '01'.

      " Read again with a new name
      READ TABLE lt_many_file INTO ls_result_oaor
       WITH KEY file_name = <ls_listbox>-text.
      CHECK sy-subrc = 0.

      IF sy-datum = ls_result_oaor-last_changed_at_date.
        ev_oaor_mode = mc_oaor-update_version.
        MESSAGE s038(zaqo_message) WITH ls_result_oaor-doc_ver_no.

      ELSE.
        ev_oaor_mode = mc_oaor-new_version.
        MESSAGE s037(zaqo_message) WITH ls_result_oaor-doc_ver_no.
      ENDIF.

      " Rename file
      TRY.
          _change_file_name(
            EXPORTING
              iv_file_name = ls_result_oaor-file_name
            CHANGING
              cs_file      = lr_file->* ).
        CATCH zcx_eui_exception INTO lo_error.
          MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
      ENDTRY.
    ENDDO.

    " Get modified
    ls_result_oaor-description = ls_screen_file-description.
    " Return result in the end
    es_oaor_file               = ls_result_oaor.
  ENDMETHOD.

  METHOD _find_request.
    DATA ls_bds       TYPE bapibds01.
    DATA lt_entry     TYPE STANDARD TABLE OF bapicomfil WITH DEFAULT KEY.
    DATA lr_entry     TYPE REF TO bapicomfil.
    DATA lt_signature TYPE STANDARD TABLE OF bapisignat WITH DEFAULT KEY.
    DATA lr_signature TYPE REF TO bapisignat.

    ls_bds-classname = is_oaor_file-package_id.
    ls_bds-classtype = mc-oaor_other.
    ls_bds-client    = sy-mandt.
    ls_bds-objkey    = is_oaor_file-option_id.

    APPEND INITIAL LINE TO lt_signature REFERENCE INTO lr_signature.
    lr_signature->doc_count  = is_oaor_file-comp_count. " c_index.
    lr_signature->doc_id     = is_oaor_file-doc_id.
    lr_signature->doc_ver_no = is_oaor_file-doc_ver_no.
    lr_signature->doc_var_id = is_oaor_file-doc_var_id.
    lr_signature->doc_var_tg = 'OR'.

    CALL FUNCTION 'BDS_BUSINESSDOCUMENT_GET_TP'
      EXPORTING
        logical_system   = ls_bds-log_system
        classname        = ls_bds-classname
        classtype        = ls_bds-classtype
        client           = ls_bds-client
        object_key       = ls_bds-objkey
      TABLES
        signature        = lt_signature
        commfile_entries = lt_entry
      EXCEPTIONS
        OTHERS           = 7.
    IF sy-subrc <> 0.
      zcx_aqo_exception=>raise_sys_error( ).
    ENDIF.

    DATA lt_e071 TYPE zcl_aqo_helper=>tt_e071.
    DATA ls_e071 LIKE LINE OF lt_e071.
    LOOP AT lt_entry REFERENCE INTO lr_entry.
      MOVE-CORRESPONDING lr_entry->* TO ls_e071.
      APPEND ls_e071 TO lt_e071.
    ENDLOOP.

    " Find new task or this is OK?
    IF cv_task IS INITIAL.

      " Find first
      DATA lr_e071 TYPE REF TO e071.
      LOOP AT lt_e071 REFERENCE INTO lr_e071.
        SELECT SINGLE e070~trkorr INTO cv_task
        FROM e070 INNER JOIN e071 ON e071~trkorr = e070~trkorr
        WHERE e071~pgmid    = lr_e071->pgmid
          AND e071~object   = lr_e071->object
          AND e071~obj_name = lr_e071->obj_name
          " @see ZCL_AQO_HELPER=>FIND_REQUEST
          AND ( e070~trstatus = 'D' OR e070~trstatus = 'L' ) AND e070~strkorr <> space.

        CHECK cv_task IS NOT INITIAL.
        EXIT.
      ENDLOOP.
    ENDIF.

    DATA lv_request TYPE e070-trkorr.
    zcl_aqo_helper=>put_2_request( EXPORTING it_e071       = lt_e071
                                             " it_e071k      = lt_e071k
                                   IMPORTING ev_request    = lv_request
                                   CHANGING  cv_task       = cv_task
                                             cv_ok_message = cv_ok_message ).
    " Ok
    CHECK lv_request IS NOT INITIAL AND cv_task IS NOT INITIAL.
    MESSAGE s039(zaqo_message) WITH is_oaor_file-file_name is_oaor_file-doc_ver_no lv_request INTO cv_ok_message.
  ENDMETHOD.

  METHOD _delete_file.
    DATA:
      lv_key           TYPE sbdst_object_key,
      lt_bds_signature TYPE sbdst_signature,
      ls_bds_signature TYPE bapisignat.

    " Subfolder in OAOR (and classname = package_id)
    lv_key = ms_db_key-option_id.

    " Prepare signature
    MOVE-CORRESPONDING is_oaor_file TO ls_bds_signature.    "#EC ENHOK
    APPEND ls_bds_signature TO lt_bds_signature.

    cl_bds_document_set=>delete(
      EXPORTING
        classname      = ms_db_key-package_id
        classtype      = mc-oaor_other
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
  ENDMETHOD.

  METHOD _diloag_screen.
    " Show in screen PARAMETERS:
    DATA ls_dyn_scr  TYPE REF TO zsaqo_oaor_dialog.
    DATA lo_screen   TYPE REF TO zcl_eui_screen.
    DATA lo_err      TYPE REF TO zcx_eui_exception.
    DATA lv_input    TYPE screen-input.
    DATA lv_cmd      TYPE syucomm.

    CLEAR ev_ok.

    " Fill scrren with values
    CREATE DATA ls_dyn_scr.
    ls_dyn_scr->p_3_pack   = ms_db_key-package_id.
    ls_dyn_scr->p_3_opt    = ms_db_key-option_id.
    ls_dyn_scr->p_3_file   = cv_file_name.
    ls_dyn_scr->p_3_vers   = iv_doc_ver_no.
    ls_dyn_scr->p_3_desc   = cv_description.

    " Create screen manager
    TRY.
        DATA lv_prog TYPE sycprog.
        CONCATENATE zcl_aqo_helper=>mc_prog-editor `OAOR_SCR` INTO lv_prog.
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr   = zcl_eui_screen=>mc_dynnr-dynamic
            iv_cprog   = lv_prog
            ir_context = ls_dyn_scr.

        DATA ls_status TYPE zcl_eui_manager=>ts_status.
        ls_status-title = iv_title. " 'New file info'(nfi). " Set pf-status & text
        lo_screen->set_status( ls_status ).
      CATCH zcx_eui_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Ok & Cancel
    lv_input = '1'.
    IF zcl_aqo_helper=>is_dev_mandt( ) <> abap_true.
      APPEND 'OK' TO lo_screen->ms_status-exclude.
      lv_input = '0'.
    ENDIF.

    " Static PF status no need on_pbo_event.
    lo_screen->customize( name = 'P_3_PACK'   input = '0' ).
    lo_screen->customize( name = 'P_3_OPT'    input = '0' ).
    " Can load with different name
    lo_screen->customize( name = 'P_3_FILE'   required   = '1'
                                              it_listbox = it_listbox ).
    lo_screen->customize( name = 'P_3_VERS'   input = '0' ).
    lo_screen->customize( name = 'P_3_DESC'   input = lv_input ).
    lo_screen->customize( name = 'P_3_VIS'    input = lv_input ).

    " As popup
    DATA lv_col_end TYPE i.
    lo_screen->get_dimension( IMPORTING ev_col_end = lv_col_end ).
    lo_screen->popup( iv_col_end = lv_col_end ).

    " Process action
    lv_cmd = lo_screen->show( ).
    CHECK lv_cmd = zif_eui_manager=>mc_cmd-ok.

    cv_description = ls_dyn_scr->p_3_desc.
    " !now index in list box!
    cv_file_name   = ls_dyn_scr->p_3_file.
    ev_ok                    = abap_true.
  ENDMETHOD.
ENDCLASS.


*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

MODULE pbo_075 OUTPUT.
  DATA go_attachment TYPE REF TO lcl_attachment.        "#EC DECL_MODUL
  IF go_attachment IS INITIAL.
    CREATE OBJECT go_attachment.
  ENDIF.

  go_attachment->pbo( ).
ENDMODULE.

*MODULE pai_075 INPUT.
*  go_attachment->pai( ).
*ENDMODULE.
