*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_transport DEFINITION INHERITING FROM lcl_tab FINAL FRIENDS zcl_eui_event_caller.
  PUBLIC SECTION.
    METHODS:
      pbo REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      _fill_table       REDEFINITION,
      _get_layout       REDEFINITION,
      _get_catalog      REDEFINITION,
      _get_sort         REDEFINITION,
      _get_toolbar      REDEFINITION,
      _on_hotspot_click REDEFINITION,
      _on_user_command  REDEFINITION,
      _on_app_event     REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF mc_button,
        export TYPE syucomm VALUE '_EXPORT',
        import TYPE syucomm VALUE '_IMPORT',
        delete TYPE syucomm VALUE '_DELETE',
      END OF mc_button.

    TYPES:
      BEGIN OF ts_se10_info,
        strkorr TYPE e070-strkorr,
        trkorr  TYPE e071k-trkorr,
        as4user TYPE e070-as4user,
        as4date TYPE e070-as4date,
        as4time TYPE e070-as4time,
        "trstatus TYPE e070-trstatus,
        ddtext  TYPE dd07t-ddtext,
        as4text TYPE e07t-as4text,
      END OF ts_se10_info,
      tt_se10_info TYPE STANDARD TABLE OF ts_se10_info WITH DEFAULT KEY.

    DATA:
      mt_se10_info TYPE tt_se10_info.
ENDCLASS.

CLASS lcl_transport IMPLEMENTATION.
  METHOD pbo.
    GET REFERENCE OF mt_se10_info INTO mr_table.
    super->pbo( ).
  ENDMETHOD.

  METHOD _on_app_event.
    super->_on_app_event( iv_origin = iv_origin
                          cv_ok     = cv_ok ).
    IF iv_origin = mc_event-after_save.
      _refresh_after( ).
    ENDIF.
  ENDMETHOD.

  METHOD _get_layout.
    rs_layout = super->_get_layout( ).
    rs_layout-sel_mode   = 'A'.
    rs_layout-grid_title = 'Transport history'(tra).
  ENDMETHOD.

  METHOD _fill_table.
    CLEAR mt_se10_info[].

    DATA: ls_e071k TYPE e071k.
    zcl_aqo_helper=>get_request_info(
      EXPORTING iv_table_name = 'ZTAQO_OPTION'
                iv_key1       = ms_db_key-package_id
                iv_key2       = ms_db_key-option_id
      IMPORTING es_e071k      = ls_e071k ).

    SELECT DISTINCT h~strkorr t~trkorr
                    h~as4user h~as4date h~as4time dom~ddtext " h~trstatus
                    d~as4text UP TO 100 ROWS INTO TABLE mt_se10_info
    FROM              e071k AS t
      INNER JOIN      e070  AS h ON h~trkorr = t~trkorr
      LEFT OUTER JOIN e07t  AS d ON d~trkorr = h~trkorr
                                AND d~langu  = sy-langu
      LEFT OUTER JOIN dd07t AS dom ON dom~domname    = 'TRSTATUS' "#EC "#EC CI_BUFFJOIN
                                  AND dom~ddlanguage = sy-langu
                                  AND dom~as4local   = 'A'
                                  AND dom~domvalue_l = h~trstatus
                                  AND dom~as4vers    = 0000
    WHERE t~pgmid      EQ   ls_e071k-pgmid
      AND t~object     EQ   ls_e071k-object
      AND t~objname    EQ   ls_e071k-objname
      AND t~mastertype EQ   ls_e071k-mastertype
      AND t~mastername EQ   ls_e071k-mastername
      AND t~tabkey     EQ   ls_e071k-tabkey " OR t~tabkey LIKE '%' " Previously mandt specific
      AND h~strkorr    NE   space
    ORDER BY h~as4date DESCENDING
             h~as4time DESCENDING.
  ENDMETHOD.

  METHOD _get_catalog.
    DATA lr_catalog TYPE REF TO lvc_s_fcat.
    APPEND INITIAL LINE TO rt_catalog REFERENCE INTO lr_catalog.
    lr_catalog->fieldname = 'STRKORR'.
    lr_catalog->hotspot   = abap_true.

    APPEND INITIAL LINE TO rt_catalog REFERENCE INTO lr_catalog.
    lr_catalog->fieldname = 'AS4USER'.
    lr_catalog->hotspot   = abap_true.

    APPEND INITIAL LINE TO rt_catalog REFERENCE INTO lr_catalog.
    lr_catalog->fieldname = 'DDTEXT'.
    lr_catalog->outputlen = 14.                          "#EC NUMBER_OK
  ENDMETHOD.

  METHOD _get_sort.
    FIELD-SYMBOLS <ls_sort> LIKE LINE OF rt_sort.
    APPEND INITIAL LINE TO rt_sort ASSIGNING <ls_sort>.
    <ls_sort>-fieldname = 'AS4DATE'.
    <ls_sort>-down      = abap_true.

    APPEND INITIAL LINE TO rt_sort ASSIGNING <ls_sort>.
    <ls_sort>-fieldname = 'AS4TIME'.
    <ls_sort>-down      = abap_true.
  ENDMETHOD.

  METHOD _on_hotspot_click.
    DATA lr_line TYPE REF TO ts_se10_info.
    READ TABLE mt_se10_info REFERENCE INTO lr_line INDEX e_row_id-index.
    CHECK sy-subrc = 0.

    CASE e_column_id-fieldname.
      WHEN 'STRKORR'.
        CALL FUNCTION 'TR_PRESENT_REQUEST'
          EXPORTING
            iv_trkorr = lr_line->strkorr.

      WHEN 'AS4USER'.
        CALL FUNCTION 'TR_SHOW_USER'
          EXPORTING
            iv_username = lr_line->as4user.
    ENDCASE.
  ENDMETHOD.

  METHOD _get_toolbar.
    FIELD-SYMBOLS <ls_button> LIKE LINE OF rt_toolbar.

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-text         = <ls_button>-quickinfo    = 'Export option to file'(exo).
    <ls_button>-icon         = icon_export.
    <ls_button>-function     = mc_button-export.

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-text         = 'Import option from file'(imo).
    <ls_button>-icon         = icon_import.
    <ls_button>-function     = mc_button-import.

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-butn_type    = cntb_btype_sep.

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-text         = 'Delete option'(dlo).
    <ls_button>-icon         = icon_delete.
    <ls_button>-function     = mc_button-delete.

    CHECK go_editor->is_editable( ) <> abap_true.
    LOOP AT rt_toolbar ASSIGNING <ls_button> WHERE function = mc_button-import OR function = mc_button-delete.
      <ls_button>-disabled = abap_true.
    ENDLOOP.
  ENDMETHOD.

  METHOD _on_user_command.
    CASE e_ucomm.
      WHEN mc_button-export.
        go_editor->do_export( ).

      WHEN mc_button-import.
        go_editor->do_import( ms_db_key ).

      WHEN mc_button-delete.
        go_editor->do_delete( ms_db_key ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

MODULE pbo_073 OUTPUT.
  DATA go_transport TYPE REF TO lcl_transport.          "#EC DECL_MODUL
  IF go_transport IS INITIAL.
    CREATE OBJECT go_transport.
  ENDIF.

  go_transport->pbo( ).
ENDMODULE.

*MODULE pai_073 INPUT.
*  go_transport->pai( ).
*ENDMODULE.
