FUNCTION-POOL zfg_aqo_menu.                 "MESSAGE-ID ..

*INCLUDE lzfg_aqo_menu...                  " Local class definition

DATA:
  gv_ok_code TYPE syucomm.

**********************************************************************

SELECTION-SCREEN BEGIN OF SCREEN 1010.
SELECTION-SCREEN BEGIN OF BLOCK bl_1010.

PARAMETERS:
  p_1_pack TYPE ztaqo_option-package_id        MODIF ID gry,
  p_1_opt  TYPE ztaqo_option-option_id         MODIF ID gry,
  p_1_date TYPE ztaqo_option-created_date      MODIF ID gry,
*  p_1_name TYPE ztaqo_option-created_uname     MODIF ID gry,
  p_1_ntxt TYPE ztaqo_option-created_name_txt  MODIF ID gry,

  p_1_desc TYPE ztaqo_option-description       MODIF ID deo, " DEV + OBL
  p_1_prev TYPE ztaqo_option-prev_value_cnt    MODIF ID deo, " DEV + OBL
  p_1_menu TYPE ztaqo_option-menu_mode         MODIF ID dev.

SELECTION-SCREEN END OF BLOCK bl_1010.
SELECTION-SCREEN END OF SCREEN 1010.
**********************************************************************

SELECTION-SCREEN BEGIN OF SCREEN 1020.
SELECTION-SCREEN BEGIN OF BLOCK bl_1020.

PARAMETERS:
  p_2_pack TYPE ztaqo_option-package_id        MODIF ID gry,
  p_2_opt  TYPE ztaqo_option-option_id         MODIF ID gry,
  p_2_file TYPE zsaqo_oaor_file_f4-file_name   MODIF ID gry,
  p_2_vers TYPE zsaqo_oaor_file_f4-doc_ver_no  MODIF ID gry,

  p_2_desc TYPE zsaqo_oaor_file_f4-description MODIF ID dev,
  p_2_vis  TYPE zsaqo_oaor_file_f4-visible AS CHECKBOX MODIF ID dev.

SELECTION-SCREEN END OF BLOCK bl_1020.
SELECTION-SCREEN END OF SCREEN 1020.

**********************************************************************
* For OAOR F4
**********************************************************************

FORM callback_oaor_f4
                 TABLES   record_tab  STRUCTURE seahlpres
                 CHANGING shlp        TYPE      shlp_descr
                          callcontrol LIKE      ddshf4ctrl.
  DATA:
    ls_selopt TYPE ddshselopt.

  " Set the restriction for ZSAQO_OAOR_FILE_F4-LAST_VERSION field
  ls_selopt-shlpfield = 'LAST_VERSION'.
  ls_selopt-sign      = 'I'.
  ls_selopt-option    = 'EQ'.
  ls_selopt-low       = 'X'.
  APPEND ls_selopt TO shlp-selopt.
ENDFORM.

**********************************************************************
* For event handlers
**********************************************************************

CLASS lcl_menu_screen DEFINITION FINAL.
  PUBLIC SECTION.

    CONSTANTS:
     mc_editro_repid TYPE syrepid VALUE 'ZAQO_EDITOR_OLD'.

    TYPES:
      tt_exclude TYPE STANDARD TABLE OF syucomm WITH DEFAULT KEY,

      BEGIN OF ts_visible,
        pack    TYPE ztaqo_option-package_id,
        opt     TYPE ztaqo_option-option_id,
        visible TYPE abap_bool,
      END OF ts_visible,
      tt_visible TYPE SORTED TABLE OF ts_visible WITH UNIQUE KEY pack opt.

    CLASS-DATA:
     mt_visible TYPE tt_visible.

    CLASS-METHODS:
      pbo_1010,
      pai_1010
        CHANGING
          cv_cmd TYPE syucomm,

      pbo_1020,
      pai_1020
        CHANGING
          cv_cmd TYPE syucomm,

      pbo_general
        IMPORTING
          iv_title   TYPE csequence
          iv_status  TYPE sypfkey
          iv_repid   TYPE syrepid
          it_exclude TYPE tt_exclude OPTIONAL,

      has_visible
        IMPORTING
                  iv_pack           TYPE ztaqo_option-package_id
                  iv_opt            TYPE ztaqo_option-option_id
        RETURNING VALUE(rv_visible) TYPE abap_bool.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_menu_screen IMPLEMENTATION.
  METHOD pbo_1010.
    DATA:
      lt_exclude TYPE tt_exclude,
      lv_is_dev  TYPE abap_bool.

    lv_is_dev = zcl_aqo_helper=>is_dev_mandt( ).
    IF lv_is_dev = abap_true.
      APPEND 'USER_INFO' TO lt_exclude.
    ELSE.
      APPEND:
        'EDIT'      TO lt_exclude,
        'DEV_INFO'  TO lt_exclude.

      " Also hide
      IF has_visible( iv_pack = p_1_pack iv_opt = p_1_opt ) <> abap_true.
        APPEND 'USER_INFO' TO lt_exclude.
      ENDIF.
    ENDIF.

    pbo_general(
     iv_title   = 'Enter option description'(eod)
     iv_status  = 'OK_ABOUT'
     iv_repid   = sy-repid
     it_exclude = lt_exclude ).
  ENDMETHOD.

  METHOD has_visible.
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

  METHOD pai_1010.
    DATA:
      lv_exit TYPE abap_bool.

    gv_ok_code = cv_cmd.
    CLEAR cv_cmd.

    CASE gv_ok_code.

      WHEN 'OK'.
        lv_exit = abap_true.
        CLEAR:
         p_1_desc,
         p_1_prev,
         p_1_menu.

      WHEN 'DEV_INFO' OR 'USER_INFO'.
        lv_exit = abap_true.

      WHEN 'EDIT'.
        IF p_1_desc IS INITIAL OR p_1_prev IS INITIAL.
          MESSAGE e055(00).
          RETURN.
        ENDIF.
        IF p_1_prev > 7 OR p_1_prev < 1.
          MESSAGE 'Previous values count have to be from 1 to 7'(f15) TYPE 'E'.
          RETURN.
        ENDIF.

        lv_exit = abap_true.
    ENDCASE.

    CHECK lv_exit = abap_true.
    LEAVE TO SCREEN 0.
  ENDMETHOD.

  METHOD pbo_1020.
    pbo_general(
     iv_title  = 'About'(abo)
     iv_status = 'OK_CANCEL'
     iv_repid  = mc_editro_repid ).
  ENDMETHOD.

  METHOD pai_1020.
    DATA:
      lv_exit TYPE abap_bool.

    CASE cv_cmd.
      WHEN 'OK'.
        IF p_2_desc IS INITIAL.
          MESSAGE e055(00).
          RETURN.
        ENDIF.
        lv_exit = abap_true.

      WHEN 'CANCEL'.
        lv_exit = abap_true.
        CLEAR p_2_pack.
    ENDCASE.

    CHECK lv_exit = abap_true.
    LEAVE TO SCREEN 0.
  ENDMETHOD.

  METHOD pbo_general.
    DATA:
      lv_is_dev TYPE abap_bool,
      lt_group  TYPE STANDARD TABLE OF screen-group1 WITH DEFAULT KEY,
      lv_group  TYPE screen-group1.

    " Chaneg title
    SET TITLEBAR 'ST_MAIN' OF PROGRAM mc_editro_repid WITH 'Enter option description'(eod).

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = iv_status
        p_program = iv_repid
      TABLES
        p_exclude = it_exclude.

    lv_is_dev = zcl_aqo_helper=>is_dev_mandt( ).

    " Make like obligatory
    LOOP AT SCREEN.
      CLEAR lt_group.

      CASE screen-group1.
        WHEN 'OBL' OR 'GRY' OR 'DEV'.
          APPEND screen-group1 TO lt_group.

          " DEV + OBL
        WHEN 'DEO'.
          APPEND 'DEV' TO lt_group.
          APPEND 'OBL' TO lt_group.

        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      " And change
      LOOP AT lt_group INTO lv_group.
        CASE lv_group.
          WHEN 'OBL'.
            screen-required = '2'. " recommended

          WHEN 'GRY'.
            screen-input    = '0'. " Gray

          WHEN 'DEV'.
            IF lv_is_dev <> abap_true.
              screen-input = '0'. " Gray
            ENDIF.
        ENDCASE.
      ENDLOOP.

      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

**********************************************************************
* Event handlers
**********************************************************************
AT SELECTION-SCREEN OUTPUT.
  CASE sy-dynnr.

    WHEN 1010.
      lcl_menu_screen=>pbo_1010( ).

    WHEN 1020.
      lcl_menu_screen=>pbo_1020( ).

  ENDCASE.

AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN 1010.
      lcl_menu_screen=>pai_1010( CHANGING cv_cmd = sy-ucomm ).

    WHEN 1020.
      lcl_menu_screen=>pai_1020( CHANGING cv_cmd = sy-ucomm ).
  ENDCASE.


**********************************************************************
* SH exits
**********************************************************************
  DEFINE f4ut_parameter_value_get.
    CALL FUNCTION 'F4UT_PARAMETER_VALUE_GET'
      EXPORTING
        parameter   = &1
        fieldname   = &1
      TABLES
        shlp_tab    = shlp_tab
        record_tab  = record_tab
        results_tab = &2
      CHANGING
        shlp        = shlp
        callcontrol = callcontrol.
  END-OF-DEFINITION.

  DEFINE f4ut_parameter_results_put.
    CALL FUNCTION 'F4UT_PARAMETER_RESULTS_PUT'
      EXPORTING
        parameter   = &1
        fieldname   = &1
      TABLES
        shlp_tab    = shlp_tab
        record_tab  = record_tab
        source_tab  = &2
      CHANGING
        shlp        = shlp
        callcontrol = callcontrol.
  END-OF-DEFINITION.

  DEFINE f4ut_results_map.
    CALL FUNCTION 'F4UT_RESULTS_MAP'
*    EXPORTING
*      source_structure   = &1
*      apply_restrictions = 'X'
      TABLES
        shlp_tab           = shlp_tab
        record_tab         = record_tab
        source_tab         = &1
      CHANGING
        shlp               = shlp
        callcontrol        = callcontrol.
  END-OF-DEFINITION.
