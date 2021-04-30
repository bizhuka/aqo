class ZCL_AQO_TESTER definition
  public
  final
  create public

  global friends ZCL_AQO_OPTION .

public section.
  type-pools ABAP .

  interfaces ZIF_AQO_EXT .

  types:
    T002_RAB type standard table of T002 .
  types:
    TR_BUKRS_RANGE type range of BUKRS .

  data BUKRS type BUKRS read-only value '1000' ##NO_TEXT.
  data BUKRS_RANGE type TR_BUKRS_RANGE read-only .
  data BUKRS_TEXT type STRING read-only value '123' ##NO_TEXT.
  data T002_TAB type T002_RAB read-only .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AQO_TESTER IMPLEMENTATION.


METHOD constructor.
  DATA:
   lo_error TYPE REF TO zcx_aqo_exception.
  FIELD-SYMBOLS:
    <ls_bukrs>  LIKE LINE OF bukrs_range.

  " Default values! For simple types use initilazation in declaration itself

  " Optional initialization
  APPEND INITIAL LINE TO me->bukrs_range ASSIGNING <ls_bukrs>.
  <ls_bukrs>-sign   = 'I'.
  <ls_bukrs>-option = 'BT'.
  <ls_bukrs>-low    = '1000'.
  <ls_bukrs>-high   = '3000'.

  SELECT * INTO TABLE me->t002_tab
  FROM t002.

  " Read new values
  TRY.
      zcl_aqo_option=>create(
        iv_package_id = '$TMP'                " Package    "#EC NOTEXT
        iv_option_id  = 'Class options'(op1)  " Any text < 30 symbols
        " Public read-only attributes is options!
        " CLASS-DATA or DATA (but not both)
        io_data       = me
        " iv_repair     = abap_true
      ).
    CATCH zcx_aqo_exception INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  MESSAGE 'Edit attributes in tr. ZAQO_BSP_EDITOR, ZAQO_EDITOR or ZAQO_EDITOR_OLD'(edt) TYPE 'S'.
ENDMETHOD.


METHOD zif_aqo_ext~before_option_save.
  DATA:
    lr_data         TYPE REF TO data.
  FIELD-SYMBOLS:
    <lv_bukrs_text> LIKE bukrs_text.

  " Only in editor (do not check in abap source code)
  CHECK iv_in_editor = abap_true.

  lr_data = io_option->get_field_value( 'BUKRS_TEXT' ). "#EC NOTEXT
  ASSIGN lr_data->* TO  <lv_bukrs_text>.

  " Just show info in editor
  IF <lv_bukrs_text> <> '123'.
    cv_error_text = 'Bukrs text not equal to "123"'(wrn).
  ENDIF.
ENDMETHOD.
ENDCLASS.
