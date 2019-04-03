class ZCX_AQO_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.
  type-pools ABAP .

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_AQO_EXCEPTION,
      msgid type symsgid value 'ZAQO_MESSAGE',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_AQO_EXCEPTION .
  data MSGV1 type MSGV1 .
  data MSGV2 type MSGV2 .
  data MSGV3 type MSGV3 .
  data MSGV4 type MSGV4 .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type MSGV1 optional
      !MSGV2 type MSGV2 optional
      !MSGV3 type MSGV3 optional
      !MSGV4 type MSGV4 optional .
  class-methods RAISE_SYS_ERROR
    raising
      ZCX_AQO_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCX_AQO_EXCEPTION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_AQO_EXCEPTION .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


METHOD raise_sys_error.
  DATA:
    BEGIN OF ls_string,
      part1 TYPE symsgv,
      part2 TYPE symsgv,
      part3 TYPE symsgv,
      part4 TYPE symsgv,
    END OF ls_string.

  " Any error
  MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_string.

  " Devided to blocks
  RAISE EXCEPTION TYPE zcx_aqo_exception
    EXPORTING
      textid = zcx_aqo_exception=>zcx_aqo_exception
      msgv1  = ls_string-part1
      msgv2  = ls_string-part2
      msgv3  = ls_string-part3
      msgv4  = ls_string-part4.
ENDMETHOD.
ENDCLASS.
