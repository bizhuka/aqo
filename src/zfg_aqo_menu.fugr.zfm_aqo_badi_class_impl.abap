FUNCTION zfm_aqo_badi_class_impl.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------
  TYPES:
    BEGIN OF ts_cl_shlp,
      clsname  TYPE seoclsname,
      descript TYPE seodescr,
    END OF ts_cl_shlp.
  DATA:
    lt_class     TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY,
    lt_cl_shlp   TYPE STANDARD TABLE OF ts_cl_shlp WITH DEFAULT KEY.
  FIELD-SYMBOLS:
    <ls_option_key> TYPE zsaqo3_general_info,
    <lv_class>      LIKE LINE OF lt_class,
    <ls_cl_shlp>    LIKE LINE OF lt_cl_shlp.

  CHECK callcontrol-step = 'SELECT'.

  ASSIGN ('(ZAQO_EDITOR3)ZSAQO3_GENERAL_INFO') TO <ls_option_key>.
  IF sy-subrc <> 0 OR <ls_option_key>-option_id IS INITIAL.
    zcx_aqo_exception=>raise_sys_error( iv_message = 'Please use ZAQO transaction'(aqo) ).
  ENDIF.

  SELECT clsname INTO TABLE lt_class
  FROM seometarel
  WHERE refclsname = <ls_option_key>-option_id.

  " Class name & short text
  IF lt_class IS NOT INITIAL.
    SELECT clsname descript INTO TABLE lt_cl_shlp
    FROM seoclasstx
    FOR ALL ENTRIES IN lt_class
    WHERE clsname = lt_class-table_line
      AND langu   = sy-langu.
  ENDIF.

  SORT: lt_cl_shlp BY clsname,
        lt_class   BY table_line.

  LOOP AT lt_class ASSIGNING <lv_class>.
    READ TABLE lt_cl_shlp TRANSPORTING NO FIELDS BINARY SEARCH
     WITH KEY clsname = <lv_class>.
    CHECK sy-subrc <> 0.

    " Loged in another langaue
    INSERT INITIAL LINE INTO lt_cl_shlp INDEX sy-tabix ASSIGNING <ls_cl_shlp>.
    <ls_cl_shlp>-clsname  = <lv_class>.
    CONCATENATE 'No class discrption in'(ncd) sy-langu INTO <ls_cl_shlp>-descript SEPARATED BY space.
  ENDLOOP.

  " Write fields back
  f4ut_parameter_results_put: 'CLSNAME'  lt_cl_shlp,
                              'DESCRIPT' lt_cl_shlp.

  " Just display
  callcontrol-step = 'DISP'.
ENDFUNCTION.
