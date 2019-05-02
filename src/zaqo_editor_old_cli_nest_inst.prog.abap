*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_nested_instance IMPLEMENTATION.
  METHOD get_instance_by_level.
    DATA:
      ls_instance    LIKE LINE OF mt_instance.
    FIELD-SYMBOLS:
      <ls_instance>  LIKE LINE OF mt_instance.

    READ TABLE mt_instance ASSIGNING <ls_instance>
     WITH TABLE KEY cl_name = iv_cl_name
                    level   = iv_level.
    IF sy-subrc <> 0.
      ls_instance-cl_name = iv_cl_name.
      ls_instance-level   = iv_level.

      " Create new instance by name
      CREATE OBJECT ls_instance-instance TYPE (iv_cl_name).
      ls_instance-instance->mv_level = iv_level.

      INSERT ls_instance INTO TABLE mt_instance ASSIGNING <ls_instance>.
    ENDIF.

    ro_instance = <ls_instance>-instance.
  ENDMETHOD.
ENDCLASS.
