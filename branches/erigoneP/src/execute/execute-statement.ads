-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
--
--  Evaluate an expression
--
with Automata, Global, State_Vectors;
package Execute.Statement is
  -- Evaulate the byte Code in the Current state
  -- For a statement, it also updates the state vector
  --   so the parameter Current is of mode in-out
  procedure Evaluate(
    Current: in out State_Vectors.State_Vector; 
    Size:    Global.Byte;
    Code:    access Automata.Byte_Code_Array;
    Result:  out Global.Byte);
end Execute.Statement;
