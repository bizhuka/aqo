class ZCL_AQO_OPT_EXAMPLE definition
  public
  create private

  global friends ZCL_AQO .

public section.

  types:
    TR_BUKRS_RANGE type RANGE OF bukrs .

  data BUKRS type BUKRS read-only value '1000' ##NO_TEXT.
  data BUKRS_RANGE type TR_BUKRS_RANGE read-only .
  data BUKRS_TEXT type STRING read-only value 'BUKRS BUKRS BUKRS BUKRS BUKRS BUKRS BUKRS BUKRS BUKRS BUKRS BUKRS BUKRS BUKRS BUKRS BUKRS' ##NO_TEXT.
  data T002_TAB type TTAB_T002 read-only .

  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_AQO_OPT_EXAMPLE
    exceptions
      UNKNOWN_FIELD .
  PROTECTED SECTION.
private section.

  class-data MO_INSTANCE type ref to ZCL_AQO_OPT_EXAMPLE .
ENDCLASS.



CLASS ZCL_AQO_OPT_EXAMPLE IMPLEMENTATION.


METHOD GET_INSTANCE.
  DATA:
    lo_opt         TYPE REF TO zcl_aqo,
    lt_empty_field TYPE stringtab,
    lv_field       TYPE string.
  FIELD-SYMBOLS:
    <ls_bukrs>  LIKE LINE OF bukrs_range.

  " 1 instance only
  ro_instance = mo_instance.
  CHECK ro_instance IS INITIAL.

  " Create it and save in var
  CREATE OBJECT mo_instance.
  ro_instance = mo_instance.

  " For reading attributes
  CREATE OBJECT lo_opt
    EXPORTING
      iv_object    = '$TMP'
      iv_subobject = 'Class options'
      io_data      = ro_instance.    " Pass current instance

  lt_empty_field = lo_opt->read( ).
  CHECK lt_empty_field IS NOT INITIAL.

  " Or something like that SY-SYSID <> 'DEV'
  IF zcl_aqo_util=>is_dev_mandt( ) <> abap_true.
    MESSAGE 'Attention! No options are detected'(t01) TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  " Prepare (optional)
  LOOP AT lt_empty_field INTO lv_field.
    CASE lv_field.
      WHEN 'BUKRS' OR 'BUKRS_TEXT'.
        " Initialized in definition

      WHEN 'BUKRS_RANGE'.
        APPEND INITIAL LINE TO ro_instance->bukrs_range ASSIGNING <ls_bukrs>.
        <ls_bukrs>-sign   = 'I'.
        <ls_bukrs>-option = 'BT'.
        <ls_bukrs>-low    = '1000'.
        <ls_bukrs>-high   = '3000'.

      WHEN 'T002_TAB'.
        SELECT * INTO TABLE ro_instance->t002_tab
        FROM t002.

      WHEN OTHERS.
        MESSAGE e012(zsy_opt) WITH lv_field RAISING unknown_field.
    ENDCASE.
  ENDLOOP.

  " And save
  lo_opt->save( ).
ENDMETHOD.
ENDCLASS.
