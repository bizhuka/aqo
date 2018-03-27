*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_grid IMPLEMENTATION.
  METHOD set_err_cells.
    DATA:
      ls_protocol TYPE REF TO lvc_s_msg1,
      lt_fcat     TYPE lvc_t_fcat,
      ls_fcat     TYPE REF TO lvc_s_fcat,
      lt_err_cell TYPE lvc_t_err,
      ls_err_cell TYPE REF TO lvc_s_err.

    CHECK io_protocol->mt_protocol IS NOT INITIAL.

    io_grid->get_frontend_fieldcatalog(
     IMPORTING
      et_fieldcatalog = lt_fcat ).

    LOOP AT io_protocol->mt_protocol REFERENCE INTO ls_protocol.
      " Find column number
      READ TABLE lt_fcat REFERENCE INTO ls_fcat
       WITH KEY fieldname = ls_protocol->fieldname.
      CHECK sy-subrc = 0.

      APPEND INITIAL LINE TO lt_err_cell REFERENCE INTO ls_err_cell.
      ls_err_cell->row_id = ls_protocol->row_id.
      ls_err_cell->col_id = ls_fcat->col_id.
    ENDLOOP.

    " Simple trick
    CALL FUNCTION 'DP_CONTROL_ASSIGN_TABLE'
      EXPORTING
        h_cntl       = io_grid->h_control
        medium       = cndp_medium_r3table
        propertyname = 'ErrorCells'
      TABLES
        data         = lt_err_cell
      EXCEPTIONS
        OTHERS       = 1.

    io_protocol->display_protocol( ).
  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
