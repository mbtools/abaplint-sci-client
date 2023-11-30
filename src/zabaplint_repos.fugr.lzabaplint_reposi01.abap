*----------------------------------------------------------------------*
***INCLUDE LZABAPLINT_UII01.
*----------------------------------------------------------------------*

MODULE user_command_2000 INPUT.

  CASE gv_ok_code.
    WHEN 'SAVE'.
      PERFORM save.
      CLEAR gv_ok_code.
    WHEN 'ADD'.
      PERFORM add.
      CLEAR gv_ok_code.
    WHEN 'DELETE'.
      PERFORM delete.
      CLEAR gv_ok_code.
    WHEN 'UPDATE_DEF'.
      " PERFORM update_with_default_conf.
      CLEAR gv_ok_code.
    WHEN 'DIFF_DEF'.
      PERFORM show_diff.
      CLEAR gv_ok_code.
    WHEN 'BACK'.
      CLEAR gv_ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
