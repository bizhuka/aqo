REPORT zaqo_tester.

TYPE-POOLS:
 abap.

PARAMETERS:
 p_cl_opt AS CHECKBOX.

TYPES:
  " Table in table
  BEGIN OF ts_email,
    email    TYPE sza5_d0700-smtp_addr,
    fullname TYPE ad_namtext,
  END OF ts_email,

  BEGIN OF ts_alt_land,
    land1      TYPE t005t-land1,
    landx50    TYPE t005t-landx50,
    " Check dropdown
    bdc_mode   TYPE ettcd_mode,
    " Table in table
    emails     TYPE HASHED TABLE OF ts_email WITH UNIQUE KEY email,
    " Range in table
    bukrs      TYPE RANGE OF bukrs,
    _dom_range TYPE RANGE OF ettcd_mode,
    " and string
    memo       TYPE string,
    " !!! Uncomment for testing purpose
    " natio    TYPE t005t-natio,
  END OF ts_alt_land,

**********************************************************************
  " Main settings of the program
  " Be very careful with editing
  BEGIN OF ts_main_opt,
    meins         TYPE meins,                " Char + conversion exit
    edit_mask     TYPE text30,               " CHAR
    combo         TYPE laspez,               " Listbox based on domain
    pack_blocked  TYPE os_boolean,           " Checkbox based on domain XSDBOOLEAN or OS_BOOLEAN (in ABAP 7.00)
    msg_count     TYPE syst-tabix,           " Number
    due_date      TYPE syst-datum,           " D
    due_time      TYPE syst-uzeit,           " T
    " datetime      TYPE xsddatetime_local,    " datetime NO component in OLD UI
    allowed_bukrs TYPE RANGE OF bukrs,  " Range
    blocked_bukrs TYPE RANGE OF bukrs,  " Range 2
    some_numbers  TYPE RANGE OF syst-tabix,
    alt_land_text TYPE SORTED TABLE OF ts_alt_land WITH UNIQUE KEY land1,
    t002_tab      TYPE STANDARD TABLE OF t002 WITH DEFAULT KEY,
    some_text     TYPE string,
    sum           TYPE bf_rbetr,             " P with sign
    sum2          TYPE dmbtr,                " P without sign

*     one_field      TYPE STANDARD TABLE OF char30 WITH DEFAULT KEY,  " Table_line is not structure
  END OF ts_main_opt.
**********************************************************************

CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      " Options
      ms_opt TYPE ts_main_opt.

    CLASS-METHODS:
      start_of_selection,

      set_default_values
        CHANGING
          cs_opt TYPE ts_main_opt.
ENDCLASS.


CLASS lcl_main IMPLEMENTATION.
  METHOD start_of_selection.
    DATA:
      lv_ref         TYPE REF TO data,
      lt_empty_field TYPE stringtab,
      lv_message     TYPE string.

    " Initials values in editor
    " In real app is usal empty!
    set_default_values(
     CHANGING
       cs_opt = ms_opt  ).

    " Or use class attributes
    GET REFERENCE OF ms_opt INTO lv_ref.           " ! Ref to data

    " Read new values
    zcl_aqo_option=>create(
      iv_package_id = '$TMP'        " Optional
      iv_option_id  = 'STRUCT_OPT'  " Optional
      ir_data       = lv_ref        " REF #( ms_opt )  Optional preferred parameter IO_DATA
    ).

    " Also initilize class option
    IF p_cl_opt = abap_true.
      DATA lo_opt TYPE REF TO zcl_aqo_tester.
      CREATE OBJECT lo_opt.
    ENDIF.

    " After editing and saving data launch it again
    " Put BREAK-POINT on the message
    lv_message = 'Now edit option by menu or in tr ZAQO_EDITOR_OLD, ZAQO_EDITOR, ZAQO_BSP_EDITOR'(t02).
    MESSAGE lv_message TYPE 'S'.
    " Double click here --> ms_opt <-- data was changed
    WRITE lv_message COLOR 2.
  ENDMETHOD.

  METHOD set_default_values.
    DATA:
      lv_field TYPE string,
      ls_email TYPE ts_email,
      lv_tabix TYPE sytabix,
      lv_mod   TYPE i.
    FIELD-SYMBOLS:
      <ls_bukrs>    LIKE LINE OF cs_opt-allowed_bukrs,
      <ls_number>   LIKE LINE OF cs_opt-some_numbers,
      <ls_alt_land> TYPE ts_alt_land.

    DEFINE add_num_range.
      APPEND INITIAL LINE TO cs_opt-some_numbers ASSIGNING <ls_number>.
      <ls_number>-sign   = &1.
      <ls_number>-option = &2.
      <ls_number>-low    = &3.
      <ls_number>-high   = &4.
    END-OF-DEFINITION.

    APPEND INITIAL LINE TO cs_opt-allowed_bukrs ASSIGNING <ls_bukrs>.
    <ls_bukrs>-sign   = 'I'.
    <ls_bukrs>-option = 'BT'.
    <ls_bukrs>-low    = '1000'.
    <ls_bukrs>-high   = '5000'.

    " Alternative texts for reports
    SELECT * INTO CORRESPONDING FIELDS OF TABLE cs_opt-alt_land_text
    FROM t005t
    WHERE spras = sy-langu.

    " Just for test
    LOOP AT cs_opt-alt_land_text ASSIGNING <ls_alt_land>.
      lv_tabix = sy-tabix.

      lv_mod = lv_tabix MOD 2.
      IF lv_mod = 0.
        DO 2 TIMES.
          ls_email-email = sy-index.
          CONCATENATE `text ` <ls_alt_land>-landx50 INTO ls_email-fullname.
          CONDENSE ls_email-email.
          CONCATENATE ls_email-email '@gmail.com' INTO ls_email-email.
          INSERT ls_email INTO TABLE <ls_alt_land>-emails.
        ENDDO.
      ENDIF.

      lv_mod = lv_tabix MOD 3.
      IF lv_mod = 0.
        DO 2 TIMES.
          APPEND INITIAL LINE TO <ls_alt_land>-bukrs ASSIGNING <ls_bukrs>.
          <ls_bukrs>-sign   = 'I'.
          <ls_bukrs>-option = 'EQ'.
          <ls_bukrs>-low    = sy-index * 1000.           "#EC NUMBER_OK
        ENDDO.
      ENDIF.

      lv_mod = lv_tabix MOD 4.
      IF lv_mod = 0.
        <ls_alt_land>-memo = 'Press me to edit'(prs).
      ENDIF.
    ENDLOOP.

    APPEND INITIAL LINE TO cs_opt-blocked_bukrs ASSIGNING <ls_bukrs>.
    <ls_bukrs>-sign   = 'E'.
    <ls_bukrs>-option = 'BT'.
    <ls_bukrs>-low    = '6000'.
    <ls_bukrs>-high   = '9000'.

    cs_opt-meins = 'KG'.

    cs_opt-due_date     = sy-datum + 30.                 "#EC NUMBER_OK

    cs_opt-due_time     = sy-uzeit.

*    CONCATENATE sy-datum sy-uzeit INTO cs_opt-datetime.

    " @see before_option_save
    cs_opt-sum          = '123.00'. " '-9999999999.99' error for negative values

    cs_opt-edit_mask    = '*MA"SK'.

    cs_opt-msg_count    = -2147483647.                   "#EC NUMBER_OK

    cs_opt-pack_blocked = abap_true.

    add_num_range 'I' 'BT' 1  10.                        "#EC NUMBER_OK
    add_num_range 'I' 'BT' 21 30.                        "#EC NUMBER_OK
    add_num_range 'I' 'BT' 41 50.                        "#EC NUMBER_OK
    add_num_range 'I' 'GE' 61 0.                         "#EC NUMBER_OK

    SELECT * INTO TABLE cs_opt-t002_tab
    FROM t002.

    cs_opt-some_text = 'Very very very very " very very \" very very very very very very very very very long text'. "#EC NOTEXT

    cs_opt-combo = 'S'.                                     "#EC NOTEXT
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_main=>start_of_selection( ).

  " @see -> ZIF_AQO_EXT~BEFORE_OPTION_SAVE
FORM before_option_save
  USING
    io_option     TYPE REF TO zcl_aqo_option
    iv_in_editor  TYPE abap_bool
  CHANGING
    cv_error_text TYPE csequence.

  DATA:
    lr_data  TYPE REF TO data,
    lo_error TYPE REF TO zcx_aqo_exception.
  FIELD-SYMBOLS:
    <lv_sum>    TYPE ts_main_opt-sum.

  " Only in editor (do not check in abap source code)
  CHECK iv_in_editor = abap_true.

  TRY.
      lr_data = io_option->get_field_value( 'SUM' ).
    CATCH zcx_aqo_exception INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  ASSIGN lr_data->* TO  <lv_sum>.

  " Just show info in editor
  IF <lv_sum> < 0.
    cv_error_text = '"SUM" is less than zero'(er1).
  ENDIF.
ENDFORM.
