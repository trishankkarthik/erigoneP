-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
--
-- State vectors
--
with Config;
with Global; use Global;
package State_Vectors is
  -- A state vector contains:
  --   the current location counter of each process,
  --   the current value of each variable,
  --   a flag if in an inner search for an acceptance cycle,
  --   a counter for the fairness copies.
  type State_Vector is
    record
      Process:  Byte_Array_Base(Config.Process_Index) :=
                  Zero_Bytes(Config.Process_Index);
      Variable: Byte_Array_Base(Config.Data_Index)  := 
                  Zero_Bytes(Config.Data_Index);
      Inner:    Byte       := 0;
      Fair:     Byte       := 0;
    end record;

  -- For zeroing a state vector
  Zero_Vector: constant State_Vector := 
    (Zero_Bytes(Config.Process_Index), 
     Zero_Bytes(Config.Data_Index),
     0, 0);

  -- Get initial values for state vector
  function Get_Initial_State_Vector return State_Vector;

  -- Return the value of the location counter of a Process from state vector S
  function Get_Process_Location_Value(
    S: State_Vector; Process: Byte) return Byte;

  -- Return the value of a Variable from state vector S
  --   Functions for single-byte values and for multiple-byte values
  function Get_Byte_Variable_Value(
    S: State_Vector; Address: Byte) return Byte;
  pragma Inline(Get_Byte_Variable_Value);

  function Get_Variable_Value(
    S:       State_Vector; 
    Address: Byte;
    Size:    Byte) return Byte_Array_Base;
  pragma Inline(Get_Variable_Value);

  -- Update the Value of a location counter in a Process in state vector S
  procedure Update_Process_Location(
    S:       in out State_Vector; 
    Process: in Byte; 
    Value:   in Byte);
  pragma Inline(Update_Process_Location);

  -- Update the Value of a Variable in the state vector S
  --   Procedures for single-byte values and for multiple-byte values
  procedure Update_Byte_Variable(
    S:        in out State_Vector;
    Address:  in Byte;
    Value:    in Byte);
  pragma Inline(Update_Byte_Variable);

  procedure Update_Variable(
    S:        in out State_Vector;
    Address:  in Byte;
    Value:    in Byte_Array_Base);
  pragma Inline(Update_Variable);

  -- Put a prefix string P, followed by the state vector S
  procedure Put_State_Vector(P: in String; S: in State_Vector);
end State_Vectors;
