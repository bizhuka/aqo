class ZCL_AQO_BADI_TEST_02 definition
  public
  final
  create public .

public section.

  interfaces ZIF_AQO_BADI_TEST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AQO_BADI_TEST_02 IMPLEMENTATION.


METHOD zif_aqo_badi_test~some_method.
  MESSAGE 'Class 02' TYPE 'I'.
ENDMETHOD.
ENDCLASS.
