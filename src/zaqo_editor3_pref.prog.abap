*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_user_prefs DEFINITION FINAL FRIENDS zcl_eui_event_caller.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_opt,
        v_max_count TYPE num2,
        v_show_help TYPE xsdboolean,
        v_skip_decl TYPE xsdboolean,
      END OF ts_opt.
    DATA:
      t_opened TYPE zcl_aqo_helper=>tt_db_key READ-ONLY,
      s_opt    TYPE ts_opt    READ-ONLY.

    METHODS:
      constructor,

      show_screen,

      add_opened
        IMPORTING
          is_db_key TYPE ts_db_key
          iv_insert TYPE abap_bool DEFAULT abap_true.

  PRIVATE SECTION.
    METHODS:
      _check_opened,

      _save_all,

      _on_pref_pai FOR EVENT pai_event OF zif_eui_manager
        IMPORTING
          sender
          iv_command
          cv_close.
ENDCLASS.

CLASS lcl_user_prefs IMPLEMENTATION.
  METHOD constructor.
    DATA lv_prefs TYPE xstring.
    SELECT SINGLE prefs INTO lv_prefs
    FROM ztaqo_prefs
    WHERE uname = sy-uname.

    TRY.
        CALL TRANSFORMATION id
         SOURCE XML lv_prefs
         RESULT t_opened = me->t_opened
                s_opt    = me->s_opt.
      CATCH cx_transformation_error.
        CLEAR: me->t_opened[],
               me->s_opt.
    ENDTRY.

    _check_opened( ).

    " Some error in prefs?
    CHECK s_opt-v_max_count IS INITIAL.
    s_opt-v_max_count = 7.
    s_opt-v_show_help = abap_true.
  ENDMETHOD.

  METHOD _check_opened.
    CHECK t_opened[] IS NOT INITIAL.

    DATA lt_exist TYPE zcl_aqo_helper=>tt_db_key.
    SELECT package_id option_id INTO TABLE lt_exist
    FROM ztaqo_option
    FOR ALL ENTRIES IN t_opened
    WHERE package_id = t_opened-package_id
      AND option_id  = t_opened-option_id
    ORDER BY PRIMARY KEY.

    FIELD-SYMBOLS <ls_opened> LIKE LINE OF t_opened.
    LOOP AT t_opened[] ASSIGNING <ls_opened>.
      DATA lv_tabix TYPE sytabix.
      lv_tabix = sy-tabix.

      READ TABLE lt_exist TRANSPORTING NO FIELDS BINARY SEARCH
       WITH KEY package_id = <ls_opened>-package_id
                option_id  = <ls_opened>-option_id.
      CHECK sy-subrc <> 0.

      DELETE t_opened[] INDEX lv_tabix.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_opened.
    " Delete same option
    DELETE t_opened WHERE package_id = is_db_key-package_id
                      AND option_id  = is_db_key-option_id.

    IF iv_insert = abap_true.
      INSERT is_db_key INTO t_opened[] INDEX 1.
    ENDIF.

    " Delete oversized
    DELETE t_opened FROM s_opt-v_max_count + 1.

    _save_all( ).
  ENDMETHOD.

  METHOD _save_all.
    DATA ls_db TYPE ztaqo_prefs.
    ls_db-uname = sy-uname.

    CALL TRANSFORMATION id
     SOURCE t_opened = me->t_opened
            s_opt    = me->s_opt
     RESULT XML ls_db-prefs.

    MODIFY ztaqo_prefs FROM ls_db.
  ENDMETHOD.

  METHOD show_screen.
    DATA lo_screen TYPE REF TO zcl_eui_screen.
    DATA lr_opt    TYPE REF TO ts_opt.
    DATA lo_error  TYPE REF TO zcx_eui_exception.

    CREATE DATA lr_opt.
    lr_opt->* = s_opt.

    DATA lv_prog TYPE sycprog.
    CONCATENATE sy-cprog `SHOW_PREFS` INTO lv_prog.
    TRY.
        CREATE OBJECT lo_screen
          EXPORTING
            ir_context = lr_opt
            iv_cprog   = lv_prog
            iv_dynnr   = zcl_eui_screen=>mc_dynnr-dynamic.
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    lo_screen->customize( name = 'V_MAX_COUNT' iv_label = 'Maximum node count in tree'(mnc)  required = '1' ).
    lo_screen->customize( name = 'V_SHOW_HELP' iv_label = 'Show online help on startup'(soh) ).
    lo_screen->customize( name = 'V_SKIP_DECL' iv_label = 'Do not check class declarations'(skd) ).

    lo_screen->popup( iv_col_end = 40 ).                 "#EC NUMBER_OK
    CHECK lo_screen->show(
      io_handler      = me
      iv_handlers_map = '_ON_PREF_PAI' ) = 'OK'.

    s_opt = lr_opt->*.
    _save_all( ).
  ENDMETHOD.

  METHOD _on_pref_pai.
    CHECK iv_command = 'OK'.
    DATA lo_screen TYPE REF TO zcl_eui_screen.
    lo_screen ?= sender.

    " Screen data
    DATA lr_opt TYPE REF TO ts_opt.
    lr_opt ?= lo_screen->get_context( ).

    IF lr_opt->v_max_count > 10 OR lr_opt->v_max_count < 1.
      MESSAGE 'Set maximum from 1 to 10'(m10) TYPE 'S' DISPLAY LIKE 'E'.
      cv_close->* = abap_false.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
