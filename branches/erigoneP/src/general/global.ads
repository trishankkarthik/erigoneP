-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
--
-- Compile-time types and constants
--
package Global is
  -- Byte is used for all indices and values of variables
  --   None used as null value
  type    Byte       is mod 256;
  for     Byte'Size  use 8;
  None:   constant   Byte := Byte'Last;

  -- Unconstrained Byte_Array_Base and constrained Byte_Array
  type        Byte_Array_Base is array(Byte range <>) of Byte;
  pragma      Pack(Byte_Array_Base);
  subtype     Byte_Array      is Byte_Array_Base(Byte);
  Zero_Bytes: constant Byte_Array := (others => 0);

  -- Process/variable names and source statements
  subtype Name           is String(1..64);
  Blanks: constant Name  := (others => ' ');

  -- Exceptions
  --   Compilation_Error is declared in Compile_Global
  File_Error:           exception;
  Internal_Error:       exception;
  Unexpected_Input:     exception;
  Termination_Error:    exception;
  Counterexample_Error: exception;
end Global;
