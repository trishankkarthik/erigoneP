-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
with Ada.Strings.Fixed;
with Automata.Display, Config, Execute.Statement, Compile_Global, Options;
with State_Vectors, Utilities;
package body Symbol_Tables is
  -- The symbol table
  Symbol_Table: array(Config.Symbol_Index) of Symbol;
  -- The Last_Offset is used when new symbols must be allocated
  --   in the state vector for local variables of "active [N] proctype"
  Last_Offset:  Byte;

  -- The number table is a table of the constant numbers
  Number_Table: array(Byte) of Byte;
  Numbers: Byte;

  -- The string table is a table of the constant strings
  String_Table:  array(Byte) of Name;
  String_Length: array(Byte) of Byte;
  Strings: Byte;

  -- The MType table is a table of names of values
  MType_Table:  array(Byte) of Name;
  MType_Length: array(Byte) of Byte;
  MTypes: Byte;

  -- Table of fixes for offsets of local variables
  type Byte_Fix_Record is
    record
      Copy:       Byte;  -- In this Copy, replace
      Old_Offset: Byte;  --   the Old_Offset by
      New_Offset: Byte;  --   the New_Offset
    end record;
  Byte_Fix_Array: array(Config.Symbol_Index) of Byte_Fix_Record;
  Byte_Fixes:     Byte;

  -- The channel table
  -- NOTE: the index starts at 1 not 0 !!
  Channel_Table: array(Config.Channel_Index) of Channel;

  procedure Initialize is
  begin
    Variables   := 0;
    Numbers     := 0;
    Strings     := 0;
    MTypes      := 0;
    Channels    := 0;
    Byte_Fixes  := 0;
    Last_Offset := 0;
  end Initialize;

  -- Get and set numbers and strings in their tables
  function Get_Number(I: Byte) return Byte is
  begin
    return Number_Table(I);
  end Get_Number;

  function Get_String(I: Byte) return String is
  begin
    return String_Table(I)(1..Integer(String_Length(I)));
  end Get_String;

  function Get_MType(I: Byte) return String is
  begin
    return MType_Table(I)(1..Integer(MType_Length(I)));
  end Get_MType;

  procedure Set_Number(S: in String) is
  begin
    Number_Table(Utilities.Extract(S, "offset")) :=
                 Utilities.Extract(S, "value");
    Numbers := Numbers + 1;
  end Set_Number;

  procedure Set_String(S: in String) is
    use Utilities;
    S1: String := Extract_Paren(S, "value", """", """");
    I:  Byte   := Extract(S, "offset");
  begin
    String_Table(I)  := Pad(S1);
    String_Length(I) := S1'Length;
    Strings := Strings + 1;
  end Set_String;

  procedure Set_MType(S: in String) is
    use Utilities;
    S1: String := Trim(Extract(S, "name"));
    I:  Byte   := Extract(S, "value");
  begin
    MType_Table(I)  := Pad(S1);
    MType_Length(I) := S1'Length;
    MTypes := MTypes + 1;
  end Set_MType;

  -- Get a Symbol to display in the state vector
  function  Get_Symbol(V: Byte) return Symbol is
  begin
    return Symbol_Table(V);
  end Get_Symbol;

  -- Get a channel from the channel table
  -- NOTE: the index starts at 1 not 0 !!
  function  Get_Channel(C: Byte) return Channel is
  begin
    return Channel_Table(C);
  end Get_Channel;

  -- Get total size of data to check size of state vector
  function  Get_Data_Size return Byte is
    B: Byte := 0;
  begin
    -- Data needed for variables
    if Variables > 0 then
      for V in 0 .. Variables-1 loop
        B := B + Symbol_Table(V).Length * Symbol_Table(V).Size;
      end loop;
    end if;

    -- Data needed for channels
    for C in 1 .. Channels loop
      if Channel_Table(C).Buffer_Size /= 0 then
        B := B + Channel_Table(C).Length;
      end if;
    end loop;
    return B;
  end Get_Data_Size;

  -- Return an array of initial values of the variables
  --   by evaluating each initial expression.
  function  Get_Variable_Initials return Byte_Array_Base is
    Result: Byte;
    Scratch: State_Vectors.State_Vector;  -- Temp for initial values
  begin
    for V in 0 .. Variables-1 loop
      if Symbol_Table(V).Code_Size = 0 then
        -- If there is no initializing expression, the initial value is zero
        Scratch.Variable(Symbol_Table(V).Offset) := 0;
      else
        -- Evaluate the initializing expression into the Scratch vector
        Execute.Statement.Evaluate(
          Scratch, Symbol_Table(V).Code_Size, 
          new Automata.Byte_Code_Array'(Symbol_Table(V).Byte_Code), Result);

        -- For arrays, the initial value will be set in the first element
        --   Next, set all elements to the same initial value
        if Symbol_Table(V).Typ = Array_Type and then
           Symbol_Table(V).Length > 1 then
          declare
            Initial: Byte_Array_Base :=
              State_Vectors.Get_Variable_Value(
                Scratch,
                Symbol_Table(V).Offset,
                Symbol_Table(V).Size);
          begin
            for E in 1..Symbol_Table(V).Length-1 loop
              State_Vectors.Update_Variable(
                Scratch, 
                Symbol_Table(V).Offset + E * Symbol_Table(V).Size,
                Initial);
            end loop;
          end;
        end if;
      end if;
    end loop;
    return Scratch.Variable;
  end Get_Variable_Initials;

  -- Put one symbol
  procedure Put_Symbol(V: in Config.Symbol_Index) is
    S: Symbol renames Symbol_Table(V);
    I: Byte := 0;   -- For channel elements
    use Ada.Text_IO;
  begin
    Put("name=" & Utilities.Trim(S.Identifier)); 
    Put(",type="  & Utilities.To_Lower(Symbol_Type'Image(S.Typ)));

    if S.Typ = Array_Type then
      Put(",element_type="  & 
          Utilities.To_Lower(Symbol_Type'Image(S.Element_Typ)));
    end if;

    Utilities.Put(",offset=", S.Offset);
    Utilities.Put("length=", S.Length);
    Utilities.Put("size=",   S.Size);
    Put("scope=" & Utilities.To_Lower(Scope_Type'Image(S.Scope)) & ",");
    Automata.Display.Put_Byte_Code(S.Code_Size, S.Byte_Code);
  end Put_Symbol;

  -- Put a channel
  procedure Put_Channel(Ch: Channel) is
    use Ada.Text_IO, Utilities;
  begin
    Put("buffer_size=", Ch.Buffer_Size);
    if Ch.Buffer_Size /= 0 then
      Put("offset=", Ch.Offset);
      Put("length=", Ch.Length);
    end if;
    Put("elements=", Ch.Elements);
    Put("element type={");
    for E in 0 .. Ch.Elements-1 loop
      Put(To_Lower(Symbol_Type'Image(Ch.Element_Types(E))) & ",");
    end loop;
    Put("}");
    Put(",element size={");
    for E in 0 .. Ch.Elements-1 loop
      Put("", Ch.Element_Sizes(E));
    end loop;
    Put_Line("},");
  end Put_Channel;

  -- Display the symbol table, the number table and the string table
  procedure Put_Symbol_Table is
    use Ada.Text_IO, Utilities;
  begin
    if not Options.Display(Options.Program) then return; end if;

    -- Display the string table
    Put_Line("symbol table start=,");
    Put("variables=", Variables, New_Line => True);
    for V in 0 .. Variables-1 loop
      Put_Symbol(V);
    end loop;
    Put_Line("symbol table end=,");

    -- Display the number table
    if Numbers > 0 then
      Put_Line("number table start=,");
      Put("numbers=", Numbers, New_Line => True);
      for I in 0 .. Numbers-1 loop
        Put("offset=", I);
        Put("value=",  Number_Table(I), New_Line => True);
      end loop;
      Put_Line("number table end=,");
    end if;

    -- Display the string table
    if Strings > 0 then
      Put_Line("string table start=,");
      Put("strings=", Strings, New_Line => True);
      for I in 0 .. Strings-1 loop
        Put("offset=", I);
        Put_Line("value=""" & 
                 String_Table(I)(1..Integer(String_Length(I))) & """,");
      end loop;
      Put_Line("string table end=,");
    end if;

    -- Display the mtype table
    if MTypes > 0 then
      Put_Line("mtype table start=,");
      Put("mtypes=", MTypes, New_Line => True);
      for I in 1 .. MTypes loop
        Put("offset=", I);
        Put_Line("value=""" & 
                 MType_Table(I)(1..Integer(MType_Length(I))) & """,");
      end loop;
      Put_Line("mtype table end=,");
    end if;

    -- Display the channel table
    if Channels > 0 then
      Put_Line("channel table start=,");
      Put("channels=", Channels, New_Line => True);
      for I in 1 .. Channels loop
        Put("index=", I);
        Put_Channel(Channel_Table(I));
      end loop;
      Put_Line("channel table end=,");
    end if;

    Put("data size=", Get_Data_Size, New_Line => True);
  end Put_Symbol_Table;

  -- Extract channel declarations
  -- Each message can have a sequence of types/sizes
  procedure Set_Channel(S: in String) is
    use Utilities;
    Ch:     Channel;
    Types:  String := Extract_Paren(S, "element_type");
    Sizes:  String := Extract_Paren(S, "element_size");
    Start:  Positive;   -- For indexing individual elements
    Stop:   Natural;
    Count:  Byte := 0;  -- Count of types and sizes
    Channel_Index: Byte := Byte'Value(Extract(S, "name"));
  begin
    -- Extract from type1,type2,...
    Start := Types'First;
    loop
      Stop := Ada.Strings.Fixed.Index(Types, ",", Start);
      exit when Stop = 0;
      Ch.Element_Types(Count) := Symbol_Type'Value(Types(Start .. Stop-1));
      Start := Stop + 1;
      Count := Count + 1;
      if Count > Config.Message_Index'Last then
        raise Unexpected_Input with "too many elements in channel";
      end if;
    end loop;

    -- Extract from size1,size2,...
    Start := Sizes'First;
    Count := 0;
    loop
      Stop := Ada.Strings.Fixed.Index(Sizes, ",", Start);
      exit when Stop = 0;
      Ch.Element_Sizes(Count) := Byte'Value(Sizes(Start .. Stop-1));
      Start := Stop + 1;
      Count := Count + 1;
    end loop;

    Ch.Elements      := Count;
    Ch.Buffer_Size   := Extract(S, "buffer_size");
    
    -- Rendezvous channels (buffer_size=0) are not stored in state vector
    if Ch.Buffer_Size /= 0 then
      Ch.Offset      := Extract(S, "offset");
      Ch.Length      := Extract(S, "length");
    end if;

    -- Store channel (index starts at 1)
    Channels := Channels + 1;
    Channel_Table(Channel_Index) := Ch;
  end Set_Channel;

  -- Extract symbol state from string S and store in Symbol Table
  procedure Set_Symbol(S: in String) is
    Sym: Symbol;
    use Utilities;
  begin
    Sym.Identifier  := Extract(S, "name");
    Sym.Typ         := Symbol_Type'Value(Extract(S, "type"));
    Sym.Offset  := Extract(S, "offset");
    Sym.Length  := Extract(S, "length");
    Sym.Size    := Extract(S, "size");
    Sym.Scope   := Scope_Type'Value(Extract(S, "scope"));
    Automata.Extract_Byte_Code(
      Extract_Paren(S, "byte code"), Sym.Code_Size, Sym.Byte_Code);

    if Sym.Typ = Array_Type then
      Sym.Element_Typ := Symbol_Type'Value(Extract(S, "element_type"));
    end if;

    Symbol_Table(Variables) := Sym;
    Variables := Variables + 1;
  end Set_Symbol;

  -- Read the symbol table from the automata file
  procedure Read(Automata_File: in Ada.Text_IO.File_Type) is
    S:           String(1..1024);    -- String and length for Get_Line
    Length:      Natural;
    use Ada.Text_IO;
  begin
    loop
      Get_Line(Automata_File, S, Length);
      exit when Ada.Strings.Fixed.Index(S, "transitions start=") = 1;
      if    Ada.Strings.Fixed.Index(S, "symbol=")  = 1 then
        Set_Symbol(S);
      elsif Ada.Strings.Fixed.Index(S, "channel=") = 1 then
        Set_Channel(S);
      elsif Ada.Strings.Fixed.Index(S, "number=")  = 1 then
        Set_Number(S);
      elsif Ada.Strings.Fixed.Index(S, "string=")  = 1 then
        Set_String(S);
      elsif Ada.Strings.Fixed.Index(S, "mtype=")  = 1 then
        Set_MType(S);
      end if;
    end loop;
    -- Compute the last offset for adding new symbols
    Last_Offset := Symbol_Table(Variables-1).Offset +
                   Symbol_Table(Variables-1).Length *
                   Symbol_Table(Variables-1).Size;
  exception
    when others =>
      raise Unexpected_Input with S(1..Length);
  end Read;

  -- Modify byte code to use immediate data for constants
  --   "iconst i" means push the i'th constant;
  --   replace i by the value of the i'th constant
  procedure Fix_Load_Constant is
    use type Compile_Global.Opcode;
  begin
    -- Check each byte code in each variable initialization
    for V in 0 .. Variables-1 loop
      for B in 0 .. Symbol_Table(V).Code_Size-1 loop
        declare
          Code: Automata.Byte_Code renames Symbol_Table(V).Byte_Code(B);
        begin
          if Code.Operator = Compile_Global.iconst then
            Code.Operand1 := Get_Number(Code.Operand1);
          end if;
        end;
      end loop;
    end loop;
  end Fix_Load_Constant;

  -- For each load or store (of a local variable) in the byte code:
  --   In copy C of the process, replace the old offset by the new one
  procedure Fix_Addresses(
    C: in Byte; Code: in out Automata.Byte_Code_Array; Size: Byte) is
    use Compile_Global;
  begin
    for B in 0 .. Size loop
      case Code(B).Operator is
        -- These byte code instructions use offsets of variables
        when bit_load .. iastore | load_address | iinc | idec =>
          -- Search each byte code fix for Copy and Old_Offset
          for F in 0 .. Byte_Fixes-1 loop
            if C = Byte_Fix_Array(F).Copy and
               Code(B).Operand1 = Byte_Fix_Array(F).Old_Offset then
              -- Replace with New_Offset
              Code(B).Operand1 := Byte_Fix_Array(F).New_Offset;
            end if;
          end loop;
        when others => null;
      end case;
    end loop;
  end Fix_Addresses;

  -- Replace Number times the local symbols for a process P
  procedure Replicate_Local_Symbols(P: String; Number: Byte) is
    use Utilities;
    Period: Natural;  -- Index of period in variable name like "P.temp"
    Var:    Name;     -- Name of the variable itself with the period
  begin
    for V in 0 .. Variables-1 loop
      -- Find the period and check if P is the process prefix
      Period := Ada.Strings.Fixed.Index(Symbol_Table(V).Identifier, ".");
      if Symbol_Table(V).Identifier(1..Period-1) = P then
        -- Change name in first copy to "P_1"
        Var := Pad(Symbol_Table(V).Identifier(Period..Name'Last));
        Symbol_Table(V).Identifier := Pad(P & "_1" & Trim(Var));

        -- Copy the variable Number times
        for I in 2 .. Number loop
          -- Copy the symbol table entry
          Symbol_Table(Variables) := Symbol_Table(V);
          -- Change the variable name by adding _I
          Symbol_Table(Variables).Identifier :=
            Pad(P & "_" & Trim(Byte'Image(I)) & Trim(Var));
          -- Store in the byte fix table that: copy I changes old Offset
          --   to new Last_Offset
          Byte_Fix_Array(Byte_Fixes) :=
            (I, Symbol_Table(Variables).Offset, Last_Offset);
          Byte_Fixes := Byte_Fixes + 1;
          -- Change the offset
          Symbol_Table(Variables).Offset := Last_Offset;
          -- Fix up addresses in byte code
          Fix_Addresses(I,
            Symbol_Table(Variables).Byte_Code,
            Symbol_Table(Variables).Code_Size);
          -- Update Last_Offset by length*size
          Last_Offset :=
            Last_Offset + Symbol_Table(V).Length * Symbol_Table(V).Size;
          -- Increment variable count
          Variables := Variables + 1;
        end loop;
      end if;
    end loop;
  end Replicate_Local_Symbols;
end Symbol_Tables;
