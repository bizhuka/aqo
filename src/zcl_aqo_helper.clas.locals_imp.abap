*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      on_function_selected FOR EVENT function_selected OF cl_gui_toolbar.
ENDCLASS.
CLASS zcl_aqo_helper DEFINITION LOCAL FRIENDS lcl_handler.

CLASS lcl_handler IMPLEMENTATION.
  METHOD on_function_selected.
    CHECK zcl_aqo_helper=>mt_menu_sh[] IS NOT INITIAL.

    IF lines( zcl_aqo_helper=>mt_menu_sh[] ) = 1.
      DATA ls_db_opt LIKE LINE OF zcl_aqo_helper=>mt_menu_sh[].
      READ TABLE zcl_aqo_helper=>mt_menu_sh[] INTO ls_db_opt INDEX 1.
    ELSE.
      DATA lt_return TYPE STANDARD TABLE OF ddshretval.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield   = 'TABIX'
          dynpprog   = sy-repid
          dynpnr     = sy-dynnr
          value_org  = 'S'
        TABLES
          value_tab  = zcl_aqo_helper=>mt_menu_sh[]
          return_tab = lt_return
        EXCEPTIONS
          OTHERS     = 3.
      CHECK sy-subrc = 0 AND lt_return[] IS NOT INITIAL.

      DATA lr_return TYPE REF TO ddshretval.
      READ TABLE lt_return REFERENCE INTO lr_return INDEX 1.
      CHECK sy-subrc = 0.

      READ TABLE zcl_aqo_helper=>mt_menu_sh[] INTO ls_db_opt INDEX lr_return->fieldval.
    ENDIF.
    CHECK ls_db_opt IS NOT INITIAL.

    PERFORM start_editor IN PROGRAM (zcl_aqo_helper=>mc_prog-editor)
     USING ls_db_opt.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_scanner DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_include TYPE include,

      get_position
        IMPORTING
          iv_text1 TYPE csequence
          iv_text2 TYPE csequence OPTIONAL
          iv_from  TYPE i         DEFAULT 1
        EXPORTING
          ev_line  TYPE i
          ev_count TYPE i,

      get_package
        RETURNING VALUE(rv_package) TYPE tadir-devclass,

      get_method_name
        RETURNING VALUE(rv_name) TYPE string.

    DATA:
      mv_include TYPE include,
      mv_class   TYPE seoclsname,
      mt_report  TYPE stringtab.
ENDCLASS.
CLASS zcl_aqo_helper DEFINITION LOCAL FRIENDS lcl_scanner.

CLASS lcl_scanner IMPLEMENTATION.
  METHOD constructor.
    mv_include = iv_include.
    READ REPORT mv_include INTO mt_report.
    mv_class = zcl_aqo_helper=>get_class_name( mv_include ).
  ENDMETHOD.

  METHOD get_position.
    CLEAR ev_line.
    CLEAR ev_count.

    DATA lt_text TYPE stringtab.
    APPEND iv_text1 TO lt_text.
    APPEND iv_text2 TO lt_text.

    DATA lv_text TYPE string.
    LOOP AT lt_text INTO lv_text WHERE table_line IS NOT INITIAL.
      DATA lv_tabix TYPE sytabix.
      lv_tabix = sy-tabix.

      REPLACE ALL OCCURRENCES OF '$' IN lv_text WITH ''.
      CONCATENATE '\b' lv_text '\b' INTO lv_text.

      DATA lt_result TYPE match_result_tab.
      IF lv_tabix = 1.
        FIND FIRST OCCURRENCE OF REGEX lv_text IN TABLE mt_report FROM iv_from RESULTS lt_result IGNORING CASE.
      ELSE.
        FIND FIRST OCCURRENCE OF REGEX lv_text IN TABLE mt_report FROM iv_from RESULTS lt_result RESPECTING CASE.
      ENDIF.
      CHECK sy-subrc = 0.

      DATA lr_result TYPE REF TO match_result.
      READ TABLE lt_result INDEX 1 REFERENCE INTO lr_result.
      CHECK sy-subrc = 0.

      " Not so far from first call
      IF iv_from <> 1.
        DATA lv_from LIKE iv_from.
        lv_from = iv_from + 10.
        CHECK lv_from > lr_result->line.
      ENDIF.

      ev_count = ev_count + 1.
      ev_line  = lr_result->line.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_package.
    DATA lv_obj_name TYPE tadir-obj_name.
    DATA lv_object   TYPE tadir-object VALUE 'PROG'.

    lv_obj_name = mv_include.
    IF mv_class IS NOT INITIAL.
      lv_obj_name = mv_class.
      lv_object   = 'CLAS'.
    ENDIF.

    SELECT SINGLE devclass INTO rv_package
    FROM tadir
    WHERE pgmid    = 'R3TR'
      AND object   = lv_object
      AND obj_name = lv_obj_name.
  ENDMETHOD.

  METHOD get_method_name.
    DATA ls_key TYPE seoclskey.
    ls_key-clsname = mv_class.

    " Try to get methods
    DATA lo_clif TYPE REF TO if_oo_clif_incl_naming.
    cl_oo_include_naming=>get_instance_by_cifkey(
      EXPORTING
       cifkey = ls_key
      RECEIVING
       cifref = lo_clif
      EXCEPTIONS
        OTHERS = 1 ).
    CHECK sy-subrc = 0.

    DATA lo_cldesc TYPE REF TO if_oo_class_incl_naming.
    lo_cldesc ?= lo_clif.

    " Find name
    DATA lt_meth TYPE seop_methods_w_include.
    DATA lr_meth TYPE REF TO seop_method_w_include.

    lt_meth = lo_cldesc->get_all_method_includes( ).
    READ TABLE lt_meth REFERENCE INTO lr_meth
     WITH KEY incname = mv_include.
    CHECK sy-subrc = 0.

    rv_name = lr_meth->cpdkey-cpdname.
  ENDMETHOD.

ENDCLASS.
