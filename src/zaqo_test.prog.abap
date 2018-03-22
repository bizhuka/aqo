REPORT zaqo_test.


CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_alt_land,
        land1    TYPE t005t-land1,
        land_txt TYPE t005t-landx50,
      END OF ts_alt_land,

**********************************************************************
      " Main settings of the program
      " Be very careful with editing
      BEGIN OF ts_main_opt,
        edit_mask     TYPE editmask,          " CHAR
        bukrs         TYPE bukrs,             " CHAR
        pack_blocked  TYPE xsdboolean,        " ABAP_BOOL     swd_blocks
        msg_count     TYPE syst-tabix,        " INT4
        due_date      TYPE syst-datum,        " D
        due_time      TYPE syst-uzeit,        " T
        datetime      TYPE xsddatetime_local, " datetime
        sum           TYPE bscurr,            " P with sign
        allowed_bukrs TYPE RANGE OF bukrs,    " Range
        blocked_bukrs TYPE RANGE OF bukrs,     " Range
        some_numbers  TYPE RANGE OF syst-tabix,
        alt_land_text TYPE SORTED TABLE OF ts_alt_land WITH UNIQUE KEY land1,
        t002_tab      TYPE STANDARD TABLE OF t002 WITH DEFAULT KEY,
        some_text     TYPE string,
*        one_field      TYPE STANDARD TABLE OF char30 WITH DEFAULT KEY,  " Table_line is not structure
      END OF ts_main_opt.
**********************************************************************


    CLASS-DATA:
      " Options
      mr_opt TYPE ts_main_opt,
      " Class for data maintenance
      mo_opt TYPE REF TO zcl_aqo.

    CLASS-METHODS:
      class_constructor,

      start_of_selection,

      init_options
        IMPORTING
          it_empty_field TYPE stringtab
        CHANGING
          cs_opt         TYPE ts_main_opt
        EXCEPTIONS
          unknown_field.

ENDCLASS.


CLASS lcl_main IMPLEMENTATION.
  METHOD class_constructor.
    DATA:
      lv_ref TYPE REF TO data.

    GET REFERENCE OF mr_opt INTO lv_ref.
    CREATE OBJECT mo_opt
      EXPORTING
        iv_object    = '$TMP'                   " Better use package or program
        iv_subobject = 'Main options'(opt)      " Any text < 30 symbols
        ir_data      = lv_ref.                  " REF #( mr_opt )
  ENDMETHOD.

  METHOD start_of_selection.
    DATA:
      lt_empty_field TYPE stringtab.

    DO 1 TIMES.
      " read options
      lt_empty_field = mo_opt->read( ).
      CHECK lt_empty_field IS NOT INITIAL.

      " Or something like theat SY-SYSID <> 'DEV'
      IF zcl_aqo_util=>is_dev_mandt( ) <> abap_true.
        MESSAGE 'Attention! No options are detected'(t01) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      " Prepare only for demonstration purpose!
      " The step is optional
      init_options(
       EXPORTING
        it_empty_field = lt_empty_field
       CHANGING
        cs_opt         = mr_opt ).

      " And save
      mo_opt->save( ). " iv_delete_prev = abap_true
    ENDDO.
  ENDMETHOD.

  METHOD init_options.
    DATA:
      lv_field    TYPE string.
    FIELD-SYMBOLS:
      <ls_bukrs>  LIKE LINE OF cs_opt-allowed_bukrs,
      <ls_number> LIKE LINE OF cs_opt-some_numbers.
    DEFINE add_num_range.
      APPEND INITIAL LINE TO cs_opt-some_numbers ASSIGNING <ls_number>.
      <ls_number>-sign   = &1.
      <ls_number>-option = &2.
      <ls_number>-low    = &3.
      <ls_number>-high   = &4.
    END-OF-DEFINITION.

    LOOP AT it_empty_field INTO lv_field.
      CASE lv_field.
        WHEN 'ALLOWED_BUKRS'.
          APPEND INITIAL LINE TO cs_opt-allowed_bukrs ASSIGNING <ls_bukrs>.
          <ls_bukrs>-sign   = 'I'.
          <ls_bukrs>-option = 'BT'.
          <ls_bukrs>-low    = '1000'.
          <ls_bukrs>-high   = '5000'.

        WHEN 'ALT_LAND_TEXT'.
          SELECT land1 landx50 AS land_txt INTO CORRESPONDING FIELDS OF TABLE cs_opt-alt_land_text
          FROM t005t
          WHERE spras = sy-langu.

        WHEN 'BLOCKED_BUKRS'.
          APPEND INITIAL LINE TO cs_opt-blocked_bukrs ASSIGNING <ls_bukrs>.
          <ls_bukrs>-sign   = 'E'.
          <ls_bukrs>-option = 'BT'.
          <ls_bukrs>-low    = '6000'.
          <ls_bukrs>-high   = '9000'.

        WHEN 'BUKRS'.
          cs_opt-bukrs = '1000'.

        WHEN 'DUE_DATE'.
          cs_opt-due_date     = sy-datum + 30.

        WHEN 'DUE_TIME'.
          cs_opt-due_time     = sy-uzeit.

        WHEN 'DATETIME'.
          CONCATENATE sy-datum sy-uzeit INTO cs_opt-datetime.

        WHEN 'SUM'.
          cs_opt-sum          = '-9999999999.99' ##literal.

        WHEN 'EDIT_MASK'.
          cs_opt-edit_mask    = '*MA"SK'.

        WHEN 'MSG_COUNT'.
          cs_opt-msg_count    = -2147483647.

        WHEN 'PACK_BLOCKED'.
          cs_opt-pack_blocked = abap_true.

        WHEN 'SOME_NUMBERS'.
          add_num_range 'I' 'BT' 1  10.
          add_num_range 'I' 'BT' 21 30.
          add_num_range 'I' 'BT' 41 50.
          add_num_range 'I' 'GE' 61 0.

        WHEN 'T002_TAB'.
          SELECT * INTO TABLE cs_opt-t002_tab
          FROM t002.

        WHEN 'SOME_TEXT'.
          cs_opt-some_text = `Very very very very " very very \" very very very very very very very very very long text` ##no_text.

        WHEN OTHERS.
          MESSAGE e012(zcl_aqo) WITH lv_field RAISING unknown_field.

      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_main=>start_of_selection( ).
