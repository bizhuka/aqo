*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_grid IMPLEMENTATION.
  METHOD set_err_cells.
    DATA:
      ls_protocol TYPE REF TO lvc_s_msg1,
      ls_fcat     TYPE REF TO lvc_s_fcat,
      lt_err_cell TYPE lvc_t_err,
      ls_err_cell TYPE REF TO lvc_s_err.

    CHECK io_protocol->mt_protocol IS NOT INITIAL.

    LOOP AT io_protocol->mt_protocol REFERENCE INTO ls_protocol.
      " Find column number
      READ TABLE io_protocol->mt_fieldcatalog REFERENCE INTO ls_fcat
       WITH KEY fieldname = ls_protocol->fieldname.
      CHECK sy-subrc = 0.

      APPEND INITIAL LINE TO lt_err_cell REFERENCE INTO ls_err_cell.
      ls_err_cell->row_id = ls_protocol->row_id.
      ls_err_cell->col_id = ls_fcat->col_id.
    ENDLOOP.

    " Simple trick
    IF io_grid IS NOT INITIAL.
      CALL FUNCTION 'DP_CONTROL_ASSIGN_TABLE'
        EXPORTING
          h_cntl       = io_grid->h_control
          medium       = cndp_medium_r3table
          propertyname = 'ErrorCells'
        TABLES
          data         = lt_err_cell
        EXCEPTIONS
          OTHERS       = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
         DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.

    io_protocol->display_protocol( ).
  ENDMETHOD.
ENDCLASS.
