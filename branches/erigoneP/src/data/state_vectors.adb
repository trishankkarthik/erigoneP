-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
with Ada.Text_IO, Ada.Strings.Fixed;
with Automata, Options, Symbol_Tables, Utilities;
package body State_Vectors is
  -- Get initial values for state vector
  function Get_Initial_State_Vector return State_Vector is
    S: State_Vector;
  begin
    -- The following two lines cause an error in the GPL 2008
    --   version of GNAT
    -- S.Process  := Automata.Get_Process_Initials(Config.Process_Index);
    -- S.Variable := Symbol_Tables.Get_Variable_Initials(Config.Data_Index);
    -- The temporary workaround is as follows:
    S.Process  :=
      Automata.Get_Process_Initials(
        Config.Process_Index'First..Config.Process_Index'Last);
    S.Variable :=
      Symbol_Tables.Get_Variable_Initials(
        Config.Data_Index'First..Config.Data_Index'Last);
    return S;
  end Get_Initial_State_Vector;

  -- Return the value of the location counter of a Process from state vector S
  function Get_Process_Location_Value(
    S: State_Vector; Process: Byte) return Byte is
  begin
    return S.Process(Process);
  end Get_Process_Location_Value;

  -- Return the value of a Variable from state vector S
  --   Functions for single-byte values and for multiple-byte values
  function Get_Byte_Variable_Value(
    S: State_Vector; Address: Byte) return Byte is
  begin
    return S.Variable(Address);
  end Get_Byte_Variable_Value;

  function Get_Variable_Value(
    S:       State_Vector;
    Address: Byte;
    Size:    Byte) return Byte_Array_Base is
  begin
    return S.Variable(Address .. Address + Size-1);
  end Get_Variable_Value;

  -- Update the Value of a location counter in a Process in state vector S
  --   Procedures for single-byte values and for multiple-byte values
  procedure Update_Process_Location(
    S:       in out State_Vector; 
    Process: in Byte; 
    Value:   in Byte) is
  begin
    S.Process(Process) := Value;
  end Update_Process_Location;

  -- Update the Value of a Variable in the state vector S
  procedure Update_Byte_Variable(
    S:        in out State_Vector;
    Address:  in Byte;
    Value:    in Byte) is
  begin
    S.Variable(Address) := Value;
  end Update_Byte_Variable;

  procedure Update_Variable(
    S:        in out State_Vector;
    Address:  in Byte;
    Value:    in Byte_Array_Base) is
  begin
    S.Variable(Address .. Address + Value'Length-1) := Value;
  end Update_Variable;

  -- Put a prefix string P, followed by the state vector S
  --   Put with named association so we have to get arrays
  --   of the process and variable names
  procedure Put_State_Vector(P: in String; S: in State_Vector) is
    use type Options.Execution_Type, Symbol_Tables.Symbol_Type;
    use Utilities, Ada.Text_IO;
    Sym:   Symbol_Tables.Symbol;
    Ch:    Symbol_Tables.Channel;
    Value: Byte;
  begin
    -- Put prefix string
    Put(P);

    -- Put location counters for each process
    for I in 0..Automata.Processes-1 loop
      Put(Trim(Automata.Get_Process_Name(I)) & "=", S.Process(I));
    end loop;

    -- Put values for each variable
    for I in 0..Symbol_Tables.Variables-1 loop
      Sym := Symbol_Tables.Get_Symbol(I);
      Put(Trim(Sym.Identifier));
      if Sym.Typ = Symbol_Tables.MType_Type then
        Put("=" & Symbol_Tables.Get_MType(S.Variable(Sym.Offset)) & ",");
      elsif Sym.Typ = Symbol_Tables.Array_Type then
        Put("={");
        exit when Sym.Length = 0;
        for L in 0..Sym.Length-1 loop
          Put("", S.Variable(Sym.Offset + L));
        end loop;
        Put("},");
      else
        Put("=", S.Variable(Sym.Offset));
      end if;
    end loop;

    -- Display channels if there are any and if requested
    if Symbol_Tables.Channels /= 0 and then Options.Display(Options.Channels) then
      for I in 1 .. Symbol_Tables.Channels loop
        Ch := Symbol_Tables.Get_Channel(I);
        -- Nothing to display for rendezvous channels
        if Ch.Buffer_Size /= 0 then
          Put("channel" & Trim(Byte'Image(I)) & "={");
          -- Nothing to display if channel is empty
          -- First byte in channel is the number of messages in it
          if S.Variable(Ch.Offset) /= 0 then
            Put("", S.Variable(Ch.Offset));
            -- For each message in the channel
            for C in 0 .. S.Variable(Ch.Offset)-1 loop
              Put("[");
              -- For each element in a message
              for E in 0 .. Ch.Elements-1 loop
                Value := S.Variable(Ch.Offset + 1 + C*Ch.Elements + E);
                if Ch.Element_Types(E) = Symbol_Tables.MType_Type then
                  Put(Symbol_Tables.Get_MType(Value) & ",");
                else
                  Put("", Value);
                end if;
              end loop;
              Put("],");
            end loop;
          end if;
          Put("},");
        end if;
      end loop;
    end if;

    -- Inner flag displayed if acceptance or fairness verification
    if Options.Execution_Mode = Options.Acceptance or
       Options.Execution_Mode = Options.Fairness   then
      Put("inner=", S.Inner);
    end if;

    -- Fairness counter displayed if fairness verification
    if Options.Execution_Mode = Options.Fairness   then
      Put("fair=", S.Fair);
    end if;
    New_Line;
  end Put_State_Vector;
end State_Vectors;
