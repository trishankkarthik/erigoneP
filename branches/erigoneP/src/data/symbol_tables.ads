-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
--
--  Symbol table
--
with Ada.Text_IO;
with Automata, Config;
with Global; use Global;
package Symbol_Tables is
  -- A symbol has an identifier, type, scope, size and
  --   offset as allocated in a state vector
  -- Element_Typ and Length is used for arrays
  -- The byte code is used for initialization of a variable

  type Symbol_Type is
    (None, Bit_Type, Byte_Type, Proc_Type, MType_Type, Label_Type, Array_Type);

  type Scope_Type  is (Global_Scope, Local_Scope, Never_Scope);
  
  type Symbol is
    record
      Identifier:   Name := Blanks;       -- Symbol identifier
      Typ:          Symbol_Type := None;  -- Symbol type
      Scope:        Scope_Type := Global_Scope;
                                          -- Symbol scope
      Offset:       Byte := 0;            -- Offset within state vector
      Element_Typ:  Symbol_Type := None;  -- Element of an array
      Size:         Byte := 0;            -- Size
      Length:       Byte := 0;            -- Length of data in bytes
      Code_Size:    Byte := 0;            -- Size of the byte code
      Byte_Code:    Automata.Byte_Code_Array := (others => <>);
                                          -- Byte code for initialization
    end record;

  -- Types and sizes of message elements
  type Symbol_Array_Type is array(Config.Message_Index) of Symbol_Type;
  type Size_Array_Type   is array(Config.Message_Index) of Byte;

  type Channel is
    record
      Buffer_Size:   Byte := 0;      -- Buffer size (0 for rendezvous)
      Offset:        Byte := 0;      -- Offset of channel within state vector
      Length:        Byte := 0;      -- Bytes for channel within state vector
      Elements:      Byte := 0;      -- Number of elements in a message
      Element_Types: Symbol_Array_Type := (others => None);
      Element_Sizes: Size_Array_Type   := (others => 0);
    end record;

  -- Number of variables in the program
  Variables: Byte;

  -- Number of channels in the program
  Channels: Byte;

  -- Initialize library-level variables
  procedure Initialize;

  -- Get and set numbers, strings and mtypes in their tables
  function  Get_Number(I: Byte) return Byte;
  function  Get_String(I: Byte) return String;
  function  Get_MType(I: Byte)  return String;
  procedure Set_Number(S: in String);
  procedure Set_String(S: in String);
  procedure Set_MType(S: in String);

  -- Get a channel from the channel table
  -- NOTE: the index starts at 1 not 0 !!
  function  Get_Channel(C: Byte) return Channel;

  -- Get a Symbol to display in the state vector
  function  Get_Symbol(V: Byte) return Symbol;

  -- Get the total size of the data to check size of state vector
  function  Get_Data_Size return Byte;

  -- Return an array of initial values of the variables
  --   by evaluating each initial expression.
  function  Get_Variable_Initials return Byte_Array_Base;

  -- Display the symbol table, the number table and the string table
  procedure Put_Symbol_Table;

  -- Read the symbol table from the automata file
  procedure Read(Automata_File: in Ada.Text_IO.File_Type);

  -- Modify byte code to use immediate data for constants
  procedure Fix_Load_Constant;

  -- Replace Number times the local symbols for a process P
  procedure Replicate_Local_Symbols(P: String; Number: Byte);

  -- For each load or store (of a local variable) in the byte code:
  --   In copy C of the process, replace the old offset by the new one
  procedure Fix_Addresses(
    C: in Byte; Code: in out Automata.Byte_Code_Array; Size: Byte);
end Symbol_Tables;
