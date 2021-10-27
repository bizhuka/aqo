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
