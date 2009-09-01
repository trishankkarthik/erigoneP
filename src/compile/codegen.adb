-- Pomegranate: A SPIN-compatible compiler for the SPIN-compatible Erigone model checker. 
-- Copyright (C) 2008 Trishank Karthik.
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
--
-- COMMENTARY:
-- 1. Observe that there is much less defensive programming here.
-- The CodeGen package is meant to be used on the output of the much more paranoid AST.
-- Use it standalone and you must rely on an Ada runtime to handle errors.
-- 
-- TODO:
-- 1. High priority: when using Emit multiple times in a procedure, check that operands are cleared out.
-- 3. replace references to Globals with intermediate types in Types.CodeGen
-- 4. Undo atomic transitions for exiting atomic transitions and the last one

with Ada.Characters.Handling;
with Ada.Containers;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with AST;
with AST.Tools;
with Logger, Utility;

-- dependency on model checker
with Compile_Global;

package body CodeGen is

  -- internal procedures
  procedure Send_Args( A_Send_Args_Node : Types.AST.Send_Args_Node_Ptr );
  procedure Send( A_Send_Node : Types.AST.Send_Node_Ptr );

   procedure Open_Automaton_File( Filename : String ; Overwrite_File_Switch : Boolean ) is
   begin
    if Overwrite_File_Switch = True then
      Ada.Text_IO.Create( Automaton_File, Ada.Text_IO.Out_File, Filename );
    else
      Ada.Text_IO.Open( Automaton_File, Ada.Text_IO.Append_File, Filename );
    end if;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         Logger.Error( "File cannot be opened! Please try again. Abandoning code generation..." );
         raise File_Unopened;
      when others =>
         Logger.Error( "Unforeseen Ada IO exception! Abandoning code generation..." );
         raise;
   end Open_Automaton_File;

   procedure Close_Automaton_File is
   begin
      Ada.Text_IO.Close( Automaton_File );
   exception
      when others =>
         Logger.Error( "Unforeseen Ada IO exception! Abandoning code generation..." );
         raise;
   end Close_Automaton_File;

  -- TK: begin a process
  procedure Begin_New_Process is
    Identifier : Types.CodeGen.Name;
  begin
    Ada.Strings.Fixed.Move( Source  =>  Types.Lexer.Bound_String.To_String( Process_Name ),
                            Target  =>  Identifier,
                            Drop    => Ada.Strings.Right,
                            Justify => Ada.Strings.Left,
                            Pad     => Ada.Strings.Space  );
    Process_Identifier  := Identifier;
    State_Number        := 1;
  end Begin_New_Process;

  procedure Add_To_Incoming_Transitions(  Target            : Types.CodeGen.Byte                    ;
                                          Transition_Cursor : Types.CodeGen.Transitions_List.Cursor )
  is
    use type Types.CodeGen.Source_To_Transitions.Cursor;
    State_Cursor      : Types.CodeGen.Source_To_Transitions.Cursor;
    Transitions       : Types.CodeGen.Transitions_List_Cursors.List;
  begin
    State_Cursor := Incoming_Transitions_Map.Find( Target );
    if State_Cursor = Types.CodeGen.Source_To_Transitions.No_Element then
      Transitions.Append( Transition_Cursor );
      Incoming_Transitions_Map.Insert( Target, Transitions );
    else
      Transitions := Types.CodeGen.Source_To_Transitions.Element( State_Cursor );
      Transitions.Append( Transition_Cursor );
      Incoming_Transitions_Map.Replace( Target, Transitions );
    end if;
  end       Add_To_Incoming_Transitions;

  procedure Delete_From_Incoming_Transitions( Target            : Types.CodeGen.Byte                    ;
                                              Transition_Cursor : Types.CodeGen.Transitions_List.Cursor )
  is
    use type Types.CodeGen.Source_To_Transitions.Cursor;
    State_Cursor      : Types.CodeGen.Source_To_Transitions.Cursor;
    Transitions       : Types.CodeGen.Transitions_List_Cursors.List;
    Cursor_To_Delete  : Types.CodeGen.Transitions_List_Cursors.Cursor;
  begin
    State_Cursor := Incoming_Transitions_Map.Find( Target );
    if State_Cursor /= Types.CodeGen.Source_To_Transitions.No_Element then
      Transitions := Types.CodeGen.Source_To_Transitions.Element( State_Cursor );
      Cursor_To_Delete := Transitions.Find( Transition_Cursor );
      Transitions.Delete( Cursor_To_Delete );
      Incoming_Transitions_Map.Replace( Target, Transitions );
    end if;
  end      Delete_From_Incoming_Transitions;

  procedure Add_To_Outgoing_Transitions(  Source            : Types.CodeGen.Byte                    ;
                                          Transition_Cursor : Types.CodeGen.Transitions_List.Cursor )
  is
    use type Types.CodeGen.Source_To_Transitions.Cursor;
    State_Cursor      : Types.CodeGen.Source_To_Transitions.Cursor;
    Transitions       : Types.CodeGen.Transitions_List_Cursors.List;
  begin
    State_Cursor := Outgoing_Transitions_Map.Find( Source );
    if State_Cursor = Types.CodeGen.Source_To_Transitions.No_Element then
      Transitions.Append( Transition_Cursor );
      Outgoing_Transitions_Map.Insert( Source, Transitions );
    else
      Transitions := Types.CodeGen.Source_To_Transitions.Element( State_Cursor );
      Transitions.Append( Transition_Cursor );
      Outgoing_Transitions_Map.Replace( Source, Transitions );
    end if;
  end       Add_To_Outgoing_Transitions;

  procedure Delete_From_Outgoing_Transitions( Source            : Types.CodeGen.Byte                    ;
                                              Transition_Cursor : Types.CodeGen.Transitions_List.Cursor )
  is
    use type Types.CodeGen.Source_To_Transitions.Cursor;
    State_Cursor      : Types.CodeGen.Source_To_Transitions.Cursor;
    Transitions       : Types.CodeGen.Transitions_List_Cursors.List;
    Cursor_To_Delete  : Types.CodeGen.Transitions_List_Cursors.Cursor;
  begin
    State_Cursor := Outgoing_Transitions_Map.Find( Source );
    if State_Cursor /= Types.CodeGen.Source_To_Transitions.No_Element then
      Transitions := Types.CodeGen.Source_To_Transitions.Element( State_Cursor );
      Cursor_To_Delete := Transitions.Find( Transition_Cursor );
      Transitions.Delete( Cursor_To_Delete );
      Outgoing_Transitions_Map.Replace( Source, Transitions );
    end if;
  end       Delete_From_Outgoing_Transitions;

  -- TK: Write its transitions
  procedure Buffer_Transition(  Statement     : Types.CodeGen.Name                ;
                                Source        : Types.CodeGen.Byte                ;
                                Target        : Types.CodeGen.Byte                ;
                                Atomic        : Types.CodeGen.Byte                ;
                                End_Label     : Types.CodeGen.Byte                ;
                                Accept_Label  : Types.CodeGen.Byte                ;
                                Line_Number   : Types.CodeGen.Byte                ;
                                Byte_Code     : Types.CodeGen.Byte_Code_List.List ) is
    Transition        : Types.CodeGen.Transitions;
    Transition_Cursor : Types.CodeGen.Transitions_List.Cursor;
  begin

    Transition :=
    (
      Statement    => Statement,
      Source       => Source,
      Target       => Target,
      Atomic       => Atomic,
      End_Label    => End_Label,
      Accept_Label => Accept_Label,
      Line_Number  => Line_Number,
      Code_Size    => Types.CodeGen.Byte( Types.CodeGen.Byte_Code_List.Length( Byte_Code ) ),
      Byte_Code    => Byte_Code
    );
    Process_Transition_List.Append( Transition );
    Transition_Cursor := Process_Transition_List.Last;

    Add_To_Incoming_Transitions( Target, Transition_Cursor );
    Add_To_Outgoing_Transitions( Source, Transition_Cursor );

  end Buffer_Transition;

  -- TK: A straightforward noop elimination algorithm.
  procedure Eliminate_Noop is
    use type Ada.Containers.Count_Type;
    use type Types.CodeGen.Opcode;
    use type Types.CodeGen.Transitions_List.Cursor;
    use type Types.CodeGen.Transitions_List_Cursors.Cursor;
    use type Types.CodeGen.Source_To_Transitions.Cursor;
    Transitions_Cursor  : Types.CodeGen.Source_To_Transitions.Cursor;
    Incoming            : Types.CodeGen.Transitions_List_Cursors.List;
    Outgoing            : Types.CodeGen.Transitions_List_Cursors.List;
    Other_Transitions   : Types.CodeGen.Transitions_List_Cursors.List;
    Next_Noop           : Types.CodeGen.Transitions_List.Cursor;
    Next2               : Types.CodeGen.Transitions_List.Cursor;
    Next_Cursor         : Types.CodeGen.Transitions_List_Cursors.Cursor;
    Next_Cursor2        : Types.CodeGen.Transitions_List_Cursors.Cursor;
    Noop_Transition     : Types.CodeGen.Transitions;
    Transition2         : Types.CodeGen.Transitions;
    Old_Source          : Types.CodeGen.Byte;
  begin

    Next_Noop := Process_Transition_List.First;
    while Next_Noop /= Types.CodeGen.Transitions_List.No_Element loop
      Noop_Transition := Types.CodeGen.Transitions_List.Element( Next_Noop );
      if Noop_Transition.Code_Size = 1 and then Noop_Transition.Byte_Code.First_Element.Operator = Compile_Global.noop then

        Logger.Info( "Noop source: " & Types.CodeGen.Byte'Image( Noop_Transition.Source ) );
        Logger.Info( "Noop target: " & Types.CodeGen.Byte'Image( Noop_Transition.Target ) );
        Transitions_Cursor    := Incoming_Transitions_Map.Find( Noop_Transition.Source );
        if Transitions_Cursor /= Types.CodeGen.Source_To_Transitions.No_Element then
          Incoming            := Types.CodeGen.Source_To_Transitions.Element( Transitions_Cursor );
        end if;
        Transitions_Cursor    := Outgoing_Transitions_Map.Find( Noop_Transition.Target );
        if Transitions_Cursor /= Types.CodeGen.Source_To_Transitions.No_Element then
          Outgoing            := Types.CodeGen.Source_To_Transitions.Element( Transitions_Cursor );
        end if;

        if Incoming.Length = 0 then

          if Outgoing.Length /= 0 then
            Logger.Info( "Case 3" );
            Next_Cursor := Outgoing.First;
            while Next_Cursor /= Types.CodeGen.Transitions_List_Cursors.No_Element loop

              Next2               := Types.CodeGen.Transitions_List_Cursors.Element( Next_Cursor );
              Transition2         := Types.CodeGen.Transitions_List.Element( Next2 );
              Logger.Info( "Old Outgoing source: " & Types.CodeGen.Byte'Image( Transition2.Source ) );
              Logger.Info( "Outgoing target: " & Types.CodeGen.Byte'Image( Transition2.Target ) );
              -- update
              Old_Source          := Transition2.Source;
              Transition2.Source  := Noop_Transition.Source;
              Logger.Info( "New Outgoing source: " & Types.CodeGen.Byte'Image( Transition2.Source ) );
              Process_Transition_List.Replace_Element( Next2, Transition2 );
              Add_To_Outgoing_Transitions( Transition2.Source, Next2 );
              -- move on
              Types.CodeGen.Transitions_List_Cursors.Next( Next_Cursor );

              -- Update targets of transitions which used to refer to the old Transition2.Source
              Transitions_Cursor  := Incoming_Transitions_Map.Find( Old_Source );
              Other_Transitions   := Types.CodeGen.Source_To_Transitions.Element( Transitions_Cursor );
              Next_Cursor2        := Other_Transitions.First;
              while Next_Cursor2 /= Types.CodeGen.Transitions_List_Cursors.No_Element loop
                Next2             := Types.CodeGen.Transitions_List_Cursors.Element( Next_Cursor2 );
                Transition2       := Types.CodeGen.Transitions_List.Element( Next2 );
                -- update
                Transition2.Target:= Noop_Transition.Source;
                Process_Transition_List.Replace_Element( Next2, Transition2 );
                Add_To_Incoming_Transitions( Transition2.Target, Next2 );
                -- move on
                Types.CodeGen.Transitions_List_Cursors.Next( Next_Cursor2 );
              end loop;

            end loop;

            -- update Initial_State if noop eliminated is from Noop_Transition.Source to Initial_State
            if Noop_Transition.Target = Process_Initial_State then
              Process_Initial_State := Noop_Transition.Source;
            end if;
            Incoming_Transitions_Map.Delete( Old_Source );
            Outgoing_Transitions_Map.Delete( Old_Source );
            -- Don't forget to delete the noop from the map!
            Delete_From_Outgoing_Transitions( Noop_Transition.Source, Next_Noop );
          end if;

        else

          Logger.Info( "Cases 2 & 4" );
          Next_Cursor := Incoming.First;

          while Next_Cursor /= Types.CodeGen.Transitions_List_Cursors.No_Element loop
            Next2               := Types.CodeGen.Transitions_List_Cursors.Element( Next_Cursor );
            Transition2         := Types.CodeGen.Transitions_List.Element( Next2 );
            Logger.Info( "Incoming source: " & Types.CodeGen.Byte'Image( Transition2.Source ) );
            Logger.Info( "Old Incoming target: " & Types.CodeGen.Byte'Image( Transition2.Target ) );
            -- update
            Old_Source          := Transition2.Target;
            Transition2.Target  := Noop_Transition.Target;
            Logger.Info( "New Incoming target: " & Types.CodeGen.Byte'Image( Transition2.Target ) );
            Process_Transition_List.Replace_Element( Next2, Transition2 );
            Add_To_Incoming_Transitions( Transition2.Target, Next2 );
            -- move on
            Types.CodeGen.Transitions_List_Cursors.Next( Next_Cursor );

          end loop;
          
          -- update Initial_State if noop eliminated is from Initial_State to Noop_Transition.Target
          if Noop_Transition.Source = Process_Initial_State then
            Process_Initial_State := Noop_Transition.Target;
          end if;
          Incoming_Transitions_Map.Delete( Old_Source );
          Outgoing_Transitions_Map.Delete( Old_Source );
          -- Don't forget to delete the noop from the map!
          Delete_From_Incoming_Transitions( Noop_Transition.Target, Next_Noop );

        end if;

        Next2 := Types.CodeGen.Transitions_List.Next( Next_Noop );
        Process_Transition_List.Delete( Next_Noop );
        Next_Noop := Next2;
        Incoming.Clear;
        Outgoing.Clear;

      else
        Types.CodeGen.Transitions_List.Next( Next_Noop );
      end if;

    end loop;

  end Eliminate_Noop;

  -- TK: A slightly inefficient (only in finding visited transitions)
  -- but straightforward DFS algorithm.
  procedure Dive( Source : Types.CodeGen.Byte ) is
    use type Types.CodeGen.Source_To_Transitions.Cursor;
    use type Types.CodeGen.Transitions_List_Cursors.Cursor;
    Transitions_To_Visit_Cursor       : Types.CodeGen.Source_To_Transitions.Cursor;
    Transitions_To_Visit              : Types.CodeGen.Transitions_List_Cursors.List;
    Transition_To_Visit_Cursor        : Types.CodeGen.Transitions_List_Cursors.Cursor;
    Transition_Cursor                 : Types.CodeGen.Transitions_List.Cursor;
    Transition                        : Types.CodeGen.Transitions;
    Child_Transitions_To_Visit_Cursor : Types.CodeGen.Source_To_Transitions.Cursor;
    Child_Transitions_To_Visit        : Types.CodeGen.Transitions_List_Cursors.List;
    Child_Transition_To_Visit_Cursor  : Types.CodeGen.Transitions_List_Cursors.Cursor;
    Child_Transition_Cursor           : Types.CodeGen.Transitions_List.Cursor;
    Child_Transition                  : Types.CodeGen.Transitions;
    All_Children_Are_Nonatomic        : Boolean;
  begin

    Logger.Info( "Visiting source " & Types.CodeGen.Byte'Image( Source ) );
    Transitions_To_Visit_Cursor := Outgoing_Transitions_Map.Find( Source );
    if Transitions_To_Visit_Cursor /=  Types.CodeGen.Source_To_Transitions.No_Element then
      Transitions_To_Visit := Types.CodeGen.Source_To_Transitions.Element( Transitions_To_Visit_Cursor );
      Transition_To_Visit_Cursor := Transitions_To_Visit.First;
      -- For each transition T going out from the Source
      while Transition_To_Visit_Cursor /= Types.CodeGen.Transitions_List_Cursors.No_Element loop
        Transition_Cursor := Types.CodeGen.Transitions_List_Cursors.Element( Transition_To_Visit_Cursor );
        -- if outgoing T has not yet been visited
        if Visited_Transitions.Contains( Transition_Cursor ) = False then
          Transition := Types.CodeGen.Transitions_List.Element( Transition_Cursor );
          Logger.Info( "Visiting transition from " & Types.CodeGen.Byte'Image( Source ) & " to " & Types.CodeGen.Byte'Image( Transition.Target ) );
          
          if Transition.Atomic = 1 then
            Child_Transitions_To_Visit_Cursor := Outgoing_Transitions_Map.Find( Transition.Target );
            if Child_Transitions_To_Visit_Cursor /= Types.CodeGen.Source_To_Transitions.No_Element then
              All_Children_Are_Nonatomic := True;
              Child_Transitions_To_Visit := Types.CodeGen.Source_To_Transitions.Element( Child_Transitions_To_Visit_Cursor );
              Child_Transition_To_Visit_Cursor := Child_Transitions_To_Visit.First;
              -- iterate over all child transitions from this source transition
              while Child_Transition_To_Visit_Cursor /= Types.CodeGen.Transitions_List_Cursors.No_Element loop
                Child_Transition_Cursor := Types.CodeGen.Transitions_List_Cursors.Element( Child_Transition_To_Visit_Cursor );
                Child_Transition := Types.CodeGen.Transitions_List.Element( Child_Transition_Cursor );
                if Child_Transition.Atomic = 1 then
                  All_Children_Are_Nonatomic := False;
                  exit;
                end if;
                Types.CodeGen.Transitions_List_Cursors.Next( Child_Transition_To_Visit_Cursor );
              end loop;
              if All_Children_Are_Nonatomic = True then
                Logger.Info( "De-atomizing transition from " & Types.CodeGen.Byte'Image( Source ) & " to " & Types.CodeGen.Byte'Image( Transition.Target ) );
                Transition.Atomic := 0;
                Process_Transition_List.Replace_Element( Transition_Cursor, Transition );
              end if;
            else
              -- this transition is a last atomic transition, so we de-atomize it
              Logger.Info( "De-atomizing transition from " & Types.CodeGen.Byte'Image( Source ) & " to " & Types.CodeGen.Byte'Image( Transition.Target ) );
              Transition.Atomic := 0;
              Process_Transition_List.Replace_Element( Transition_Cursor, Transition );
            end if;
          end if;
          
          Visited_Transitions.Append( Transition_Cursor );
          -- DFS visit the transition target 
          Dive( Transition.Target );
        end if;
        Types.CodeGen.Transitions_List_Cursors.Next( Transition_To_Visit_Cursor );
      end loop;
    end if;

  end Dive;

  procedure Correct_Atomic_Transitions is
  begin

    Dive( Process_Initial_State );
    Visited_Transitions.Clear;

  end Correct_Atomic_Transitions;
  
  function  Add_To_String_Table( Format : Types.Lexer.Bounded_String_Hashed_Set.Cursor )
  return    Types.CodeGen.Operand is
    Index : Types.CodeGen.Operand;
  begin
    Index := Types.CodeGen.Add_To_String_Table( Format, Automaton_File );
    return Index;
  end       Add_To_String_Table;
  
  function  Add_To_Natural_Table( Number  : Types.Lexer.Natural_Hashed_Set.Cursor )
  return    Operand is
    Index : Types.CodeGen.Operand;
  begin
    Index := Types.CodeGen.Add_To_Natural_Table( Number, Automaton_File );
    return Index;
  end       Add_To_Natural_Table;

  -- MBA
  function  Trim(  Source   : in String; 
                  Side      : Ada.Strings.Trim_End := Ada.Strings.Both  )
            return String
            renames Ada.Strings.Fixed.Trim;

  -- MBA
  function  To_Lower( Item  : in String                                 )
            return String
            renames Ada.Characters.Handling.To_Lower;
  
  -- MBA: Put a byte code using the definitions in Types.Codegen
  procedure Put_Byte_Code( Code: in Types.CodeGen.Byte_Code_List.List ) is
    use type Ada.Containers.Count_Type;
    use type Types.CodeGen.Byte_Code_List.Cursor;
    Next  : Types.CodeGen.Byte_Code_List.Cursor;
    B     : Byte_Code;
  begin
    Ada.Text_IO.Put( Automaton_File, "byte code=" );
    if Code.Length = 0 then
        Ada.Text_IO.Put( Automaton_File, "," );
    else
      Ada.Text_IO.Put( Automaton_File, "{" );
      Next := Code.First;
      while Next /= Types.CodeGen.Byte_Code_List.No_Element loop
        B := Types.CodeGen.Byte_Code_List.Element( Next );
        Ada.Text_IO.Put( Automaton_File, 
          To_Lower( Types.CodeGen.Opcode'Image( B.Operator ) ) &
          Types.CodeGen.Operand'Image( B.Operand_1 ) &
          Types.CodeGen.Operand'Image( B.Operand_2 ) & ","
        );
        Types.CodeGen.Byte_Code_List.Next( Next );
      end loop;
      Ada.Text_IO.Put( Automaton_File, "}," );
    end if;
    Ada.Text_IO.New_Line( Automaton_File );
  end Put_Byte_Code;

  procedure Write_Mtype(  Name  : String              ;
                          Value : Types.CodeGen.Byte  )
  is
  begin
    Ada.Text_IO.Put_Line( Automaton_File,
                          "mtype=,name=" & To_Lower( Name ) & ",value=" & Trim( Types.CodeGen.Byte'Image( Value ) & "," )
    );
  end Write_Mtype;

  procedure Write_Symbol( Identifier  : Types.CodeGen.Name        ;
                          Typ         : Types.CodeGen.Symbol_Type ;
                          Index       : Types.CodeGen.Symbol_Table_Index  ;
                          Size        : Types.CodeGen.Byte        ;
                          Scope       : Types.CodeGen.Scope_Type  ) is
  begin
    Ada.Text_IO.Put(  Automaton_File,
      "symbol=,type=" & To_Lower( Symbol_Type'Image( Typ ) )
      & ",name="      & Trim( Identifier )
      & ",offset="    & Trim( Types.Codegen.Byte'Image( Index ) )
      & ",length=1,size="    & Trim( Types.Codegen.Byte'Image( Size ) )
      & ",scope="     & To_Lower( Scope_Type'Image( Scope ) ) & ","
    );
    Put_Byte_Code( Statement_Byte_Code );
    Flush;
    Logger.Info(  Types.CodeGen.Symbol_Type'Image( Typ )    & ", "
                  & Identifier                              & ", "
                  & Types.CodeGen.Scope_Type'Image( Scope ) & ", "
                  & Types.CodeGen.Symbol_Table_Index'Image( Index )
    );
  end Write_Symbol;

  procedure Write_Array(  Identifier  : Types.CodeGen.Name        ;
                          Typ         : Types.CodeGen.Symbol_Type ;
                          Index       : Types.CodeGen.Symbol_Table_Index  ;
                          Element_Size: Types.CodeGen.Byte        ;
                          Length      : Positive                  ;
                          Scope       : Types.CodeGen.Scope_Type  ) is
  begin
    Ada.Text_IO.Put(  Automaton_File,
      "symbol=,type=array_type,element_type="  & To_Lower( Symbol_Type'Image( Typ ) )
      & ",name="      & Trim( Identifier )
      & ",offset="    & Trim( Types.Codegen.Byte'Image( Index ) )
      & ",length="    & Trim( Positive'Image( Length ) )
      & ",size="      & Trim( Types.Codegen.Byte'Image( Element_Size ) )
      & ",scope="     & To_Lower( Scope_Type'Image( Scope ) )     & ","
    );
    Put_Byte_Code( Statement_Byte_Code );
    Flush;
    Logger.Info(  Types.CodeGen.Symbol_Type'Image( Typ )    & ", "
                  & Identifier                              & ", "
                  & Types.CodeGen.Scope_Type'Image( Scope ) & ", "
                  & Types.CodeGen.Symbol_Table_Index'Image( Index )
    );
  end Write_Array;

  -- no initialization instructions for a channel
  -- the convention is that the interpreter zero-es it out
  procedure Write_Channel(  A_Ch_Init_Node  : Types.AST.Ch_Init_Node_Ptr        ;
                            Index           : Types.CodeGen.Symbol_Table_Index  ;
                            Scope           : Types.CodeGen.Scope_Type          ) is
    use type Types.AST.Typename_Node_List.Cursor;
    use type Types.Lexer.Bound_String.Bounded_String;
    Next                : Types.AST.Typename_Node_List.Cursor;
    A_Typename_Node     : Types.AST.Typename_Node_Ptr;
    Built_In_Type       : Types.AST.Built_In_Data_Type_Ptr;
    Typ                 : Types.CodeGen.Symbol_Type;
    Element_Size_String : Types.Lexer.Bound_String.Bounded_String;
  begin

    Ada.Text_IO.Put( Automaton_File, "channel=," );
    Ada.Text_IO.Put( Automaton_File, "element_type={"             );

    Next := A_Ch_Init_Node.Typename_List.First;
    while Next /= Types.AST.Typename_Node_List.No_Element loop
      A_Typename_Node := Types.AST.Typename_Node_List.Element( Next );
      case A_Typename_Node.The_Type.Kind is

        when  Types.AST.Built_In_Type =>
          Built_In_Type := Types.AST.Built_In_Data_Type_Ptr( A_Typename_Node.The_Type );
          case Built_In_Type.Built_In_Kind is
            when  Types.AST.Bit   =>
              Typ := Types.CodeGen.Bit_Type;
            when  Types.AST.Byte  |
                  Types.AST.Chan  |
                  Types.AST.Mtype =>
              Typ := Types.CodeGen.Byte_Type;
            when  others          =>
              Raise_Unsupported_Construct( Types.AST.Built_In_Data_Type_Kind'Image( Built_In_Type.Built_In_Kind ) );
          end case;

        when  Types.AST.Array_Type    =>
          Code_Generation_Assert( False, "Internal inconsistency! Typename alone cannot possibly distinguish an array." );

        when  Types.AST.User_Type     =>
          Raise_Unsupported_Construct( "user-defined types for channel fields" );

      end case;
      Ada.Text_IO.Put( Automaton_File, To_Lower( Symbol_Type'Image( Typ ) ) & "," );
      Element_Size_String :=  Element_Size_String & Trim( Positive'Image( Built_In_Type.Size_In_Bytes ) ) & ",";
      Types.AST.Typename_Node_List.Next( Next );
    end loop;

    Ada.Text_IO.Put( Automaton_File, "}" );
    -- NOTE: A name is a handle is a name is a handle.
    Ada.Text_IO.Put( Automaton_File, ",name=" & Trim( Types.CodeGen.Byte'Image( Channel_Handle ) ) );
    Ada.Text_IO.Put( Automaton_File, ",buffer_size=" & Trim( Types.Lexer.Promela_Natural'Image( A_Ch_Init_Node.Buffer_Size ) ) );
    Ada.Text_IO.Put( Automaton_File, ",element_size={" & Types.Lexer.Bound_String.To_String( Element_Size_String ) & "}" );

    if A_Ch_Init_Node.Buffer_Size > 0 then
      Ada.Text_IO.Put( Automaton_File, ",length=" & Trim( Positive'Image( A_Ch_Init_Node.Channel_Size ) ) );
      Ada.Text_IO.Put( Automaton_File, ",offset=" & Trim( Types.CodeGen.Byte'Image( Index ) ) );
    end if;

    Ada.Text_IO.Put_Line( Automaton_File, ",scope="  & To_Lower( Scope_Type'Image( Scope ) ) & "," );

  end Write_Channel;

  procedure Write_Process( An_Active_Node : Types.AST.Active_Node_Ptr ) is
    use type Types.CodeGen.Transitions_List.Cursor;
    Count           : Types.CodeGen.Byte := Types.CodeGen.Byte( Process_Transition_List.Length );
    Next_Transition : Types.CodeGen.Transitions_List.Cursor;
    T               : Types.CodeGen.Transitions;
    T_Index         : Types.CodeGen.Byte := 0;
    Active_Number   : Types.Lexer.Promela_Natural;
  begin

    if Processes = 0 then
      Ada.Text_IO.Put_Line( Automaton_File, "transitions start=," );
    end if;

    if An_Active_Node /= null then
      Logger.Info( "Walking an Active..." );
      if An_Active_Node.Number /= null then
        Active_Number := Types.Lexer.Element_In_Table( An_Active_Node.Number.Number );
      else
        Active_Number := 1;
      end if;
    else
      Active_Number := 0;
    end if;

    Ada.Text_IO.Put( Automaton_File,
      "process="
      & Trim( Process_Identifier )
      & ",active="  & Trim( Types.Lexer.Promela_Natural'Image( Active_Number ) )
      & ",initial=" & Trim( Types.CodeGen.Byte'Image( Process_Initial_State ) )
    );
    Ada.Text_IO.Put_Line( Automaton_File, ",transitions=" & Trim( Types.CodeGen.Byte'Image( Count ) ) & "," );

    Next_Transition := Process_Transition_List.First;
    while Next_Transition /= Types.CodeGen.Transitions_List.No_Element loop
      T := Types.CodeGen.Transitions_List.Element( Next_Transition );

      Ada.Text_IO.Put( Automaton_File,
        "number="
        & Trim( Types.CodeGen.Byte'Image( T_Index ) )
        & ",source=" & Trim( Types.CodeGen.Byte'Image( T.Source ) )
        & ",target=" & Trim( Types.CodeGen.Byte'Image( T.Target ) )
        & ",atomic=" & Trim( Types.CodeGen.Byte'Image( T.Atomic ) )
        & ",end="    & Trim( Types.CodeGen.Byte'Image( T.End_Label ) )
        & ",accept=" & Trim( Types.CodeGen.Byte'Image( T.Accept_Label ) )
        & ",line="   & Trim( Types.CodeGen.Byte'Image( T.Line_Number ) )
        & ",statement={" & Trim( T.Statement ) & "},"
      );
      Put_Byte_Code( T.Byte_Code );
      T_Index := T_Index + 1;

      Next_Transition := Types.CodeGen.Transitions_List.Next( Next_Transition );
    end loop;

  end Write_Process;

  -- TK: end it
  procedure End_New_Process( An_Active_Node : Types.AST.Active_Node_Ptr ) is
  begin
    -- Unknown GOTO targets must have been handled by Sequence before the end of the process
    Code_Generation_Assert( Outstanding_Goto_Targets.Is_Empty = True,
                            "There is at least one unknown GOTO target in this process!"  );
    Eliminate_Noop;
    Correct_Atomic_Transitions;
    Write_Process( An_Active_Node );
    -- Clear process transition maps
    Process_Transition_List.Clear;
    Incoming_Transitions_Map.Clear;
    Outgoing_Transitions_Map.Clear;
    -- Again, in case of "overflow," there is a n-1 mismatch
    Processes := Processes + 1;
  end End_New_Process;

  function Scope_Put( Name : Types.Lexer.Bound_String.Bounded_String ; Definition : Types.AST.Node_Ptr )
  return Boolean
  is
    Inserted : Boolean;
    Position : Bounded_String_To_Node_Ptr_Hashed_Map.Cursor;
  begin
    Top.Hash_Table.Insert( Name, Definition, Position, Inserted );
    return Inserted;
  end Scope_Put;

  function  Scope_Get( Name : Types.Lexer.Bound_String.Bounded_String )
  return    Types.AST.Node_Ptr is
    use type Types.AST.Bounded_String_To_Node_Ptr_Hashed_Map.Cursor;
    Definition : Types.AST.Bounded_String_To_Node_Ptr_Hashed_Map.Cursor;
    S : Types.AST.Scope_Ptr := Top;
  begin
    -- NOTE: May return null
    while S /= null loop
      Definition := S.Hash_Table.Find( Name );
      if Definition /= Types.AST.Bounded_String_To_Node_Ptr_Hashed_Map.No_Element then
        return Types.AST.Bounded_String_To_Node_Ptr_Hashed_Map.Element( Definition );
      end if;
      S := S.Up;
    end loop;
    return null;
  end Scope_Get;

  -- If I am not mistaken, the following two procedures should be general.
  procedure Scope_Submerge is
  begin
    Logger.Info( "SCOPE: Submerging into deeper scope" );
    Top := Top.Children.First_Element;
  end Scope_Submerge;

  procedure Scope_Surface is
    S : Types.AST.Scope_Ptr;
  begin
    Top := Top.Up;
    -- The great wheel of life
    S := Top.Children.First_Element;
    Top.Children.Delete_First;
    Top.Children.Append( S );
    Logger.Info( "SCOPE: Surfacing into old scope" );
  end Scope_Surface;

  procedure Emit( Code : in out Types.CodeGen.Byte_Code )  is
  begin
    Logger.Info(  "CODE: "
                  & Types.CodeGen.Opcode'Image( Code.Operator )
                  & Integer'Image( Code.Operand_1 )
                  & Integer'Image( Code.Operand_2 )           );
    Statement_Byte_Code.Append( Code );
    -- clear code so that we don't accidentally leave anything
    Code.Operator   := Compile_Global.noop;
    Code.Operand_1  := 0;
    Code.Operand_2  := 0;
  end Emit;

  procedure Flush is
  begin
    Logger.Info( "Flushing accumulated byte code out of existence" );
    Statement_Byte_Code.Clear;
  end Flush;

  procedure Flush( A_Stmt_Node : Types.AST.Stmt_Node_Ptr ) is
  begin
    Logger.Info( "Flushing accumulated byte code into this statement" );
    A_Stmt_Node.Byte_Code := Statement_Byte_Code;
    Statement_Byte_Code.Clear;
  end Flush;
  
  function Generate_Any_Expr( An_Any_Expr_Node: Types.AST.Any_Expr_Node_Ptr ) return Types.CodeGen.Byte_Code_List.List is
    Code : Types.CodeGen.Byte_Code_List.List;
  begin
    Logger.Open_Log_File( "CodeGen.log" );
    Logger.Info( "LTL formulae translation" );
    Any_Expr( An_Any_Expr_Node );
    Logger.Info( "Flushing expression byte code" );
    Code := Statement_Byte_Code;
    Statement_Byte_Code.Clear;
    Logger.Close_Log_File;
    return Code;
  end Generate_Any_Expr;
  
  procedure Write_Transition( Source_Stmt : Types.AST.Stmt_Node_Ptr ; Target_Stmt : Types.AST.Stmt_Node_Ptr ) is
    Text                : Types.CodeGen.Name := Types.CodeGen.Blanks;
  begin

    -- TODO: end, accept labels

    Logger.Info(  Types.CodeGen.State_Number'Image( Source_Stmt.State_Number )
                  & ", " & Positive'Image( Source_Stmt.Line_Number )
                  &" -> " & Types.CodeGen.State_Number'Image( Target_Stmt.State_Number )
                  & ", " & Positive'Image( Target_Stmt.Line_Number )  );
    Ada.Strings.Fixed.Move( Source  =>  Ada.Strings.Unbounded.To_String( Target_Stmt.Text ),
                            Target  =>  Text,
                            Drop    =>  Ada.Strings.Right,
                            Justify =>  Ada.Strings.Left,
                            Pad     =>  Ada.Strings.Space  );
    Buffer_Transition(  Text,
                        Types.CodeGen.Byte( Source_Stmt.State_Number ),
                        Types.CodeGen.Byte( Target_Stmt.State_Number ),
                        Types.CodeGen.Byte( In_Atomic ),
                        0, 0,
                        Types.CodeGen.Byte( Target_Stmt.Line_Number ),
                        Target_Stmt.Byte_Code );

  end Write_Transition;

  -- FIXME: A temporary kludge to fix the end statement problem.
  -- Must find a general solution later, something that takes care of end label as well.
  -- TODO: why would this be generally correct? must find proof of correctness or otherwise.
  -- Well, it would be if we always had a final target Stmt, Noop_Stmt or otherwise.
  procedure Write_End_Transition( A_Sequence_Node : Types.AST.Sequence_Node_Ptr ) is
    Last_Stmt           : Types.AST.Stmt_Node_Ptr;
    Target              : Types.CodeGen.Byte;
    Last_Stmt_Byte_Code : Types.CodeGen.Byte_Code_List.List;
    Halt_Byte_Code      : Types.CodeGen.Byte_Code;
    Name                : Types.CodeGen.Name := Types.CodeGen.Blanks;
  begin
    Last_Stmt := Utility.Get_Last_Statement_In_Sequence( A_Sequence_Node );
    -- If the last Statement in the A_Proctype_Node.Sequence has any incoming transition
    Target := Types.CodeGen.Byte( Last_Stmt.State_Number );
    if Incoming_Transitions_Map.Contains( Target ) = True then
      Logger.Info(  Types.CodeGen.State_Number'Image( Last_Stmt.State_Number )
                    & ", " & Positive'Image( Last_Stmt.Line_Number )
                    &" -> " & Types.CodeGen.State_Number'Image( 0 )
                    & ", " & Positive'Image( 0 )  );
      Halt_Byte_Code.Operator := Compile_Global.halt;
      Last_Stmt_Byte_Code.Append( Halt_Byte_Code );
      Buffer_Transition(  Name,
                          Types.CodeGen.Byte( Last_Stmt.State_Number ),
                          0,        -- dummy 'end' state
                          0, 1, 0,  -- mark end transition
                          0,        -- dummy 'line' number,
                          Last_Stmt_Byte_Code );
    end if;
  end Write_End_Transition;

  procedure Raise_Unsupported_Construct( Name : String ) is
    Message : String := "SDS-Compiler does not, as yet, support " & Name & "!";
  begin
    Logger.Error( Message );
    raise Unsupported_Construct with Message;
  end Raise_Unsupported_Construct;  

  -- FIXME: Really, these should be no semantic checks here. Move to AST.
  procedure Code_Generation_Assert( Assertion : Boolean ; Message : String ) is
  begin
    if Assertion = False then
      Logger.Error( Message );
      raise Generic_Code_Generation_Error with Message;
    end if;
  end Code_Generation_Assert;

  procedure Spec( A_Spec_Node                 : Types.AST.Spec_Node_Ptr ;
                  Process_Declarations_Switch : Boolean                 ;
                  Automaton_Filename          : String                  )
  is
    use type Types.AST.Module_Node_List.Cursor;
    Next : Types.AST.Module_Node_List.Cursor;
  begin
    Process_Declarations_First := Process_Declarations_Switch;
    Logger.Open_Log_File( "CodeGen.log", Append => not Process_Declarations_First );
    Logger.Set_Level_Threshold( Logger.Info_Level );
    Open_Automaton_File( Automaton_Filename, Overwrite_File_Switch => Process_Declarations_Switch );
    Logger.Info( "Walking a Spec Node..." );

    Next := A_Spec_Node.Module_List.First;
    -- NOTE: Get global scope from Types.AST 
    Top := AST.Tools.Get_Root_Scope;
    while Next /= Types.AST.Module_Node_List.No_Element loop
      Module( Types.AST.Module_Node_List.Element( Next ) );
      Next := Types.AST.Module_Node_List.Next( Next );
    end loop;

    -- NOTE: we do not need delete the global scope because
    -- an LTL expression translation may need it later.
    --Logger.Info( "SCOPE: Out of global scope" );
    --Scope_Surface;
    Close_Automaton_File;
    Logger.Info( "SUCCESS! Code generated." );
    Logger.Close_Log_File;
  exception
    when others =>
      Close_Automaton_File;
      Logger.Info("FAILURE! Code generation failed.");
      Logger.Close_Log_File;
      raise;
  end Spec;

  -- Now Mtype bytes are in the Mtype_Node itself!
  procedure Mtype( A_Mtype_Node : Types.AST.Mtype_Node_Ptr ) is
    use type Types.AST.Module_Node_Ptr;
    use type Types.AST.Mtype_Node_Ptr;
    use type Types.AST.Bounded_String_To_Mtype_Byte_Hashed_Map.Cursor;
    Next  : Types.AST.Bounded_String_To_Mtype_Byte_Hashed_Map.Cursor;
    Name  : Types.Lexer.Bound_String.Bounded_String;
    Value : Types.CodeGen.Byte;

  begin

    if Process_Declarations_First = True then
      Logger.Info( "Walking a Mtype.. " );
      Next := A_Mtype_Node.Names_Map.First;
      while Next /= Types.AST.Bounded_String_To_Mtype_Byte_Hashed_Map.No_Element loop

        Name  := Types.AST.Bounded_String_To_Mtype_Byte_Hashed_Map.Key( Next );
        Value := Types.CodeGen.Byte( Types.AST.Bounded_String_To_Mtype_Byte_Hashed_Map.Element( Next ) );
        Write_Mtype( Types.Lexer.Bound_String.To_String( Name ), Value ); 
        Types.AST.Bounded_String_To_Mtype_Byte_Hashed_Map.Next( Next );

      end loop;
    end if;

  end Mtype;

  procedure Module( A_Module_Node : Types.AST.Module_Node_Ptr ) is
    A_Decl_Lst_Module_Node  : Types.AST.Decl_Lst_Module_Node_Ptr;
    An_Init_Module_Node     : Types.AST.Init_Module_Node_Ptr;
    A_Mtype_Module_Node     : Types.AST.Mtype_Module_Node_Ptr;
    A_Proctype_Module_Node  : Types.AST.Proctype_Module_Node_Ptr;
    An_Utype_Module_Node    : Types.AST.Utype_Module_Node_Ptr;
  begin
    Logger.Info( "Walking a Module..." );
    case A_Module_Node.Tag is

      when Types.AST.Node_Decl_Lst_Module =>
        Logger.Info( "Decl_Lst Module" );
        A_Decl_Lst_Module_Node := Types.AST.Decl_Lst_Module_Node_Ptr( A_Module_Node );
        Decl_Lst( A_Decl_Lst_Module_Node.Decl_Lst );
      when Types.AST.Node_Init_Module     =>
        Logger.Info( "Init Module" );
        An_Init_Module_Node := Types.AST.Init_Module_Node_Ptr( A_Module_Node );
        Proctype( An_Init_Module_Node.Pinit );
      when Types.AST.Node_Mtype_Module    =>
        Logger.Info( "Mtype Module" );
        A_Mtype_Module_Node := Types.AST.Mtype_Module_Node_Ptr( A_Module_Node );
        Mtype( A_Mtype_Module_Node.Mtype );
      when Types.AST.Node_Never_Module    =>
        Logger.Info( "Never Module" );
      when Types.AST.Node_Proctype_Module =>
        Logger.Info( "Proctype Module" );
        A_Proctype_Module_Node := Types.AST.Proctype_Module_Node_Ptr( A_Module_Node );
        Proctype( A_Proctype_Module_Node.Proctype );
      when Types.AST.Node_Trace_Module    =>
        Logger.Info( "Trace Module" );
      when Types.AST.Node_Utype_Module    =>
        Logger.Info( "Utype Module" );
        An_Utype_Module_Node := Types.AST.Utype_Module_Node_Ptr( A_Module_Node );
        Utype( An_Utype_Module_Node.Utype );
      when others                         =>
        Code_Generation_Assert( False, "Unrecognized A_Module_Node!" );

    end case;

  end Module;
  
  procedure Utype            ( An_Utype_Node   : Types.AST.Utype_Node_Ptr            ) is
    Name    : Types.Lexer.Bound_String.Bounded_String;
    A_Node  : Types.AST.Node_Ptr;
  begin
    if Process_Declarations_First = True then
      Logger.Info( "Walking an Utype.. " );
      Name := Types.Lexer.Element_In_Table( An_Utype_Node.Name );
      Logger.Info( "Name: " & Types.Lexer.Bound_String.To_String( Name ) );
      -- NOTE: Get utype name from current (global) scope 
      Logger.Info( "SCOPE: Getting '" & Types.Lexer.Bound_String.To_String( Name ) & "' from the current scope" );
      A_Node := Scope_Get( Name );
      Code_Generation_Assert( A_Node /= null,
                              "Missing utype '" & Types.Lexer.Bound_String.To_String( Name ) & "' in this scope!" );
      Logger.Info( "Size in bytes: " & Positive'Image( An_Utype_Node.Decl_Lst.Size_In_Bytes ) );
    end if;
  end Utype;

  procedure Decl_Lst( A_Decl_Lst_Node : Types.AST.Decl_Lst_Node_Ptr ) is
    use type Types.AST.One_Decl_Node_List.Cursor;
    Next      : Types.AST.One_Decl_Node_List.Cursor;
    Next_Ivar : Types.AST.Ivar_Node_List.Cursor;
  begin
    if Process_Declarations_First = True then
    Logger.Info( "Walking a Decl_Lst..." );
      Next := A_Decl_Lst_Node.One_Decl_List.First;
      while Next /= Types.AST.One_Decl_Node_List.No_Element loop
        One_Decl( Types.AST.One_Decl_Node_List.Element( Next ) );
        Next := Types.AST.One_Decl_Node_List.Next( Next );
      end loop;
    end if;
  end Decl_Lst;

  procedure One_Decl( A_One_Decl_Node : Types.AST.One_Decl_Node_Ptr ) is
    use type Types.AST.Ivar_Node_List.Cursor;
    Next                : Types.AST.Ivar_Node_List.Cursor;
  begin
    Logger.Info( "Walking a One_Decl..." );
    Logger.Info( "Visibility: " & Types.Lexer.Token_Type'Image( A_One_Decl_Node.Visibility ) );
    Next := A_One_Decl_Node.Variable_List.First;
    while Next /= Types.AST.Ivar_Node_List.No_Element loop
      Ivar( Types.AST.Ivar_Node_List.Element( Next ) );
      Next := Types.AST.Ivar_Node_List.Next( Next );
    end loop;
  end One_Decl;

  procedure Ch_Init(  A_Ch_Init_Ivar_Node : Types.AST.Ch_Init_Ivar_Node_Ptr ;
                      Scope               : Types.CodeGen.Scope_Type        ) is
    A_Ch_Init_Node  : Types.AST.Ch_Init_Node_Ptr;
    Offset_In_Byte  : Types.CodeGen.Byte;
  begin

    A_Ch_Init_Node      := A_Ch_Init_Ivar_Node.Assignment;
    Logger.Info( "Walking a Ch_Init..." );

    for I in Positive range 1 .. A_Ch_Init_Node.Array_Size loop

      -- We are using the latest, correct assignment to Symbol_Table_Index
      Channel_Handle    := Channel_Handle + 1;
      Write_Channel( A_Ch_Init_Node, Global_Index, Scope );

      -- only allocate state space for buffered channels
      if A_Ch_Init_Node.Buffer_Size > 0 then
        -- Offset
        Offset_In_Byte  := Types.CodeGen.Byte( A_Ch_Init_Node.Channel_Size );
        Logger.Info( "Byte Offset = " & Types.CodeGen.Byte'Image( Offset_In_Byte ) );
        Global_Index    := Global_Index + Offset_In_Byte;
      end if;

    end loop;

  end Ch_Init;

  procedure Ivar( An_Ivar_Node    : Types.AST.Ivar_Node_Ptr     ) is
    use type Types.Lexer.Bound_String.Bounded_String;
    A_Node                      : Types.AST.Node_Ptr;
    An_Any_Expr_Ivar_Node       : Types.AST.Any_Expr_Ivar_Node_Ptr;
    A_Ch_Init_Ivar_Node         : Types.AST.Ch_Init_Ivar_Node_Ptr;
    Built_In_Type               : Types.AST.Built_In_Data_Type_Ptr;
    An_Array                    : Types.AST.Array_Data_Type_Ptr;
    Name                        : Types.Lexer.Bound_String.Bounded_String;
    Identifier                  : Types.CodeGen.Name;
    Typ                         : Types.CodeGen.Symbol_Type;
    Offset_In_Byte              : Types.CodeGen.Byte;
    Scope                       : Types.CodeGen.Scope_Type;
    Variable_Or_Parameter       : Boolean;
    Code                        : Types.CodeGen.Byte_Code;
  begin
    Logger.Info( "Walking an Ivar..." );
    Name    := Types.Lexer.Element_In_Table( An_Ivar_Node.Name );
    Logger.Info( "Name: " & Types.Lexer.Bound_String.To_String( Name ) );
    Logger.Info( "SCOPE: Getting '" & Types.Lexer.Bound_String.To_String( Name ) & "' from the current scope" );
    -- Just a sanity check
    A_Node  := Scope_Get( Name );
    Code_Generation_Assert( A_Node /= null,
                            "Missing variable '" & Types.Lexer.Bound_String.To_String( Name ) & "' in this scope!" );

    case An_Ivar_Node.The_Type.Kind is

      when  Types.AST.Built_In_Type |
            Types.AST.Array_Type    =>

        if  An_Ivar_Node.The_Type.Kind = Types.AST.Built_In_Type  then
          Built_In_Type := Types.AST.Built_In_Data_Type_Ptr( An_Ivar_Node.The_Type );
        else
          An_Array      := Types.AST.Array_Data_Type_Ptr( An_Ivar_Node.The_Type );
          Built_In_Type := Types.AST.Built_In_Data_Type_Ptr( An_Array.Base_Type );
        end if;

        -- NOTE: At the moment, Erigone supports the subset { bit/bool, byte, chan, mtype }
        case Built_In_Type.Built_In_Kind is
          when  Types.AST.Bit   =>
            Typ := Types.CodeGen.Bit_Type;
          when  Types.AST.Byte  |
                Types.AST.Chan  |
                Types.AST.Mtype =>
            Typ := Types.CodeGen.Byte_Type;
          when  others          =>
            Raise_Unsupported_Construct( Types.AST.Built_In_Data_Type_Kind'Image( Built_In_Type.Built_In_Kind ) );
        end case;

        if    Process_Name /= Types.Lexer.Bound_String.Null_Bounded_String then
          -- NOTE: Variable_Or_Parameter not used for Symbol.Variable at the moment, but setting it anyway
          if In_Parameter_List = True then
            Variable_Or_Parameter := False;
          else
            Variable_Or_Parameter := True;
          end if;
          -- set Identifier to Process_Name.Name
          Ada.Strings.Fixed.Move( Source  =>  Types.Lexer.Bound_String.To_String( Process_Name )
                                              & "." & Types.Lexer.Bound_String.To_String( Name ),
                                  Target  =>  Identifier,
                                  Drop    => Ada.Strings.Right,
                                  Justify => Ada.Strings.Left,
                                  Pad     => Ada.Strings.Space  );
          Scope := Types.CodeGen.Local_Scope;
        else
          Ada.Strings.Fixed.Move( Source  =>  Types.Lexer.Bound_String.To_String( Name ),
                                  Target  =>  Identifier,
                                  Drop    => Ada.Strings.Right,
                                  Justify => Ada.Strings.Left,
                                  Pad     => Ada.Strings.Space  );
          Scope := Types.CodeGen.Global_Scope;
          Variable_Or_Parameter := True;
        end if;

        -- Offset
        Offset_In_Byte := Types.CodeGen.Byte( An_Ivar_Node.The_Type.Size_In_Bytes );
        Logger.Info( "Byte Offset = " & Types.CodeGen.Byte'Image( Offset_In_Byte ) );

        -- We are using the latest, correct assignment to Symbol_Table_Index
        An_Ivar_Node.Index  := Global_Index;
        Global_Index        := Global_Index + Offset_In_Byte;

        -- Either generate code to compute initial value or allocate state space for channels
        case An_Ivar_Node.Tag is

          when Types.AST.Node_Any_Expr_Ivar =>
            Code_Generation_Assert( Built_In_Type.Built_In_Kind /= Types.AST.Chan, "Channel variables cannot be initialized with expressions!" );
            An_Any_Expr_Ivar_Node := Types.AST.Any_Expr_Ivar_Node_Ptr( An_Ivar_Node );
            Any_Expr( An_Any_Expr_Ivar_Node.Assignment );

          when Types.AST.Node_Ch_Init_Ivar  =>
            Code_Generation_Assert( Built_In_Type.Built_In_Kind = Types.AST.Chan, "Non-channel variables cannot be initialized with channels!" );
            A_Ch_Init_Ivar_Node   := Types.AST.Ch_Init_Ivar_Node_Ptr( An_Ivar_Node );
            Ch_Init( A_Ch_Init_Ivar_Node, Scope );

          when Types.AST.Node_Ivar          =>
            -- zero it out
            Code.Operator         := Compile_Global.bipush;
            Emit( Code );

          when others                       =>
            Code_Generation_Assert( False, "Unrecognized An_Ivar_Node!" );

        end case;
        
        -- we can store into the variable now that we have its index 
        Store_Ivar( An_Ivar_Node );

        if An_Array = null then
          Write_Symbol( Identifier  => Identifier,
                        Typ         => Typ,
                        Index       => An_Ivar_Node.Index,
                        Size        => Offset_In_Byte,
                        Scope       => Scope  );
        else
          -- one design considered a special instruction to init arrays
          -- but this design of convention was adopted
          Write_Array(  Identifier    => Identifier,
                        Typ           => Typ,
                        Index         => An_Ivar_Node.Index,
                        Element_Size  => Types.CodeGen.Byte( An_Array.Base_Type.Size_In_Bytes ),
                        Length        => An_Array.Number_Of_Elements,
                        Scope         => Scope  );
        end if;

    when  Types.AST.User_Type =>
      Raise_Unsupported_Construct( "user-defined types" );

    end case;

  exception
    when Constraint_Error =>
      Code_Generation_Assert( False, "Size of a variable is beyond our present scope for the model checker's memory!" );

  end Ivar;

  -- TODO: walk Xr_Xs_Step_List
  procedure Sequence         ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr         ) is
    use type Types.AST.Step_Node_List.Cursor;
    Next              : Types.AST.Step_Node_List.Cursor;
    A_Step_Node       : Types.AST.Step_Node_Ptr;
    A_Stmt_Step_Node  : Types.AST.Stmt_Step_Node_Ptr;
    Prev_Stmt_In_Step : Types.AST.Stmt_Node_Ptr;
    This_Stmt_In_Step : Types.AST.Stmt_Node_Ptr;
  begin
    Logger.Info( "Walking a Sequence" );
    
    -- We prepend and append the Proctype Sequence with a noop.
    if Process_Declarations_First = True then
      -- NOTE: Loop to walk Decl_Lst_Step_List
      -- Not necessary to protect it with this IF clause
      Next := A_Sequence_Node.Decl_Lst_Step_List.First;
      while Next /= Types.AST.Step_Node_List.No_Element loop
        A_Step_Node := Types.AST.Step_Node_List.Element( Next );
        Step( A_Step_Node );
        Types.AST.Step_Node_List.Next( Next );
      end loop;
    else
      Prepend_And_Append_Sequence_With_Noop( A_Sequence_Node );
    end if;
   
    -- NOTE: Loop to walk Stmt_Step_List
    -- Do the usual stuff, number _sequential_ Stmt_Step-s
    Next := A_Sequence_Node.Stmt_Step_List.First;
    while Next /= Types.AST.Step_Node_List.No_Element loop
    
      A_Step_Node := Types.AST.Step_Node_List.Element( Next );

      -- Get a Stmt number, if applicable...
      if  Process_Declarations_First = False then

        A_Stmt_Step_Node                := Types.AST.Stmt_Step_Node_Ptr( A_Step_Node );
        This_Stmt_In_Step               := A_Stmt_Step_Node.Statement;
        This_Stmt_In_Step.State_Number  := State_Number;
        State_Number                    := State_Number + 1;

      end if;

      -- ...then Step for bytecode generation... 
      Step( A_Step_Node );
      
      -- ...then link the transition states.
      if  Process_Declarations_First = False then

        if Prev_Stmt_In_Step = null then
          -- FIXED: I hope this is a more readable boolean condition
          -- TODO: is this exception list complete? what about other kinds of statements?
          if  not ( This_Stmt_In_Step.Tag = Types.AST.Node_Do_Stmt       or else 
                    This_Stmt_In_Step.Tag = Types.AST.Node_If_Stmt       or else
                    This_Stmt_In_Step.Tag = Types.AST.Node_Sequence_Stmt or else
                    This_Stmt_In_Step.Tag = Types.AST.Node_Atomic_Stmt   or else
                    This_Stmt_In_Step.Tag = Types.AST.Node_D_Step_Stmt   or else
                    This_Stmt_In_Step.Tag = Types.AST.Node_Goto_Stmt     or else
                    This_Stmt_In_Step.Tag = Types.AST.Node_Name_Stmt     )  then
            Prev_Stmt_In_Step := This_Stmt_In_Step;
          else
            -- do NOT link TO next stmt FROM this DO, If, {}, Atomic {}, D_Step {}, Goto or Label_Name : Stmt
            Prev_Stmt_In_Step := null;
          end if;
        else
          Write_Transition( Prev_Stmt_In_Step, This_Stmt_In_Step );
          if  not ( This_Stmt_In_Step.Tag = Types.AST.Node_Do_Stmt       or else 
                    This_Stmt_In_Step.Tag = Types.AST.Node_If_Stmt       or else
                    This_Stmt_In_Step.Tag = Types.AST.Node_Sequence_Stmt or else
                    This_Stmt_In_Step.Tag = Types.AST.Node_Atomic_Stmt   or else
                    This_Stmt_In_Step.Tag = Types.AST.Node_D_Step_Stmt   or else
                    This_Stmt_In_Step.Tag = Types.AST.Node_Goto_Stmt     or else
                    This_Stmt_In_Step.Tag = Types.AST.Node_Name_Stmt     )  then
            Prev_Stmt_In_Step := This_Stmt_In_Step;
          else
            -- do NOT link TO next stmt FROM this DO, If, {}, Atomic {}, D_Step {}, Goto or Label_Name : Stmt
            Prev_Stmt_In_Step := null;
          end if;
        end if;

      end if;
      --

      Types.AST.Step_Node_List.Next( Next );

    end loop;
    
    -- Here we link up various constructs and sequences properly. Correctness > optimum.
    if Process_Declarations_First = False then
      Process_Do_Statements( A_Sequence_Node );
      Process_If_Statements( A_Sequence_Node );
      Process_Nested_Atomic_D_Step_Statements( A_Sequence_Node );
      Process_Goto_Statements( A_Sequence_Node );
    end if;
  end Sequence;

  procedure Prepend_And_Append_Sequence_With_Noop(  A_Sequence_Node : Types.AST.Sequence_Node_Ptr )
  is
    Next                : Types.AST.Step_Node_List.Cursor;
    A_Stmt_Step_Node    : Types.AST.Stmt_Step_Node_Ptr;
    A_Step_Node         : Types.AST.Step_Node_Ptr;
    Last_Stmt_Node      : Types.AST.Stmt_Node_Ptr;
  begin

    Logger.Info( "Appending and prepending a Sequence with noop" );

    -- We wish to insert a noop as the first Stmt.
    if In_Options = False then
      A_Stmt_Step_Node                        := new Types.AST.Stmt_Step_Node;
      A_Stmt_Step_Node.Tag                    := Types.AST.Node_Stmt_Step;
      A_Stmt_Step_Node.Statement              := new Types.AST.Noop_Stmt_Node;
      A_Stmt_Step_Node.Statement.Tag          := Types.AST.Node_Noop_Stmt;
      A_Stmt_Step_Node.Statement.Line_Number  := 1;
      A_Step_Node                             := Types.AST.Step_Node_Ptr( A_Stmt_Step_Node );
      A_Sequence_Node.Stmt_Step_List.Prepend( A_Step_Node );
    end if;

    -- NOTE: Only append a noop following a
    -- DO-OD, IF-FI, {}, Atomic {}, D_Step {} or Label_Name : Stmt
    Last_Stmt_Node  := Utility.Get_Last_Statement_In_Sequence( A_Sequence_Node );
    if  Last_Stmt_Node.Tag = Types.AST.Node_Do_Stmt        or else
        Last_Stmt_Node.Tag = Types.AST.Node_If_Stmt        or else
        Last_Stmt_Node.Tag = Types.AST.Node_Sequence_Stmt  or else
        Last_Stmt_Node.Tag = Types.AST.Node_Atomic_Stmt    or else
        Last_Stmt_Node.Tag = Types.AST.Node_D_Step_Stmt    or else
        Last_Stmt_Node.Tag = Types.AST.Node_Name_Stmt      then
      --- Lesson of the day: when using a single pointer to point to
      -- 2 distinct objects, make sure you do not forget to do so.
      A_Stmt_Step_Node                        := new Types.AST.Stmt_Step_Node;
      A_Stmt_Step_Node.Tag                    := Types.AST.Node_Stmt_Step;
      A_Stmt_Step_Node.Statement              := new Types.AST.Noop_Stmt_Node;
      A_Stmt_Step_Node.Statement.Tag          := Types.AST.Node_Noop_Stmt;
      A_Stmt_Step_Node.Statement.Line_Number  := 1;
      A_Step_Node                             := Types.AST.Step_Node_Ptr( A_Stmt_Step_Node );
      A_Sequence_Node.Stmt_Step_List.Append( A_Step_Node );
    end if;

  end Prepend_And_Append_Sequence_With_Noop;

  procedure Insert_Outstanding_Goto_Target( Name              : Types.Lexer.Bound_String.Bounded_String ;
                                            A_Goto_Stmt_Node  : Types.AST.Goto_Stmt_Node_Ptr            )
  is
    use type Types.AST.Bounded_String_To_Stmt_Node_List_Hashed_Map.Cursor;
    A_Stmt_Node_List  : Types.AST.Stmt_Node_List.List;
    A_Stmt_Node       : Types.AST.Stmt_Node_Ptr;
    Cursor            : Types.AST.Bounded_String_To_Stmt_Node_List_Hashed_Map.Cursor;
  begin
    Cursor      := Outstanding_Goto_Targets.Find( Name );
    A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_Goto_Stmt_Node );
    if Cursor /= Types.AST.Bounded_String_To_Stmt_Node_List_Hashed_Map.No_Element then
      A_Stmt_Node_List := Types.AST.Bounded_String_To_Stmt_Node_List_Hashed_Map.Element( Cursor );
    end if;
    A_Stmt_Node_List.Append( A_Stmt_Node );
    if Cursor /= Types.AST.Bounded_String_To_Stmt_Node_List_Hashed_Map.No_Element then
      Outstanding_Goto_Targets.Replace( Name, A_Stmt_Node_List );
    else
      Outstanding_Goto_Targets.Insert( Name, A_Stmt_Node_List );
    end if;
  end       Insert_Outstanding_Goto_Target;

  procedure Write_Outstanding_Goto_Targets( A_Name_Stmt_Node : Types.AST.Name_Stmt_Node_Ptr )
  is
    use type Types.AST.Stmt_Node_List.Cursor;
    use type Types.AST.Bounded_String_To_Stmt_Node_List_Hashed_Map.Cursor;
    Name                    : Types.Lexer.Bound_String.Bounded_String;
    A_Stmt_Node_List        : Types.AST.Stmt_Node_List.List;
    A_Stmt_Node             : Types.AST.Stmt_Node_Ptr;
    A_Stmt_Node_List_Cursor : Types.AST.Bounded_String_To_Stmt_Node_List_Hashed_Map.Cursor;
    A_Stmt_Node_Cursor      : Types.AST.Stmt_Node_List.Cursor;
  begin
    Name                    := Types.Lexer.Element_In_Table( A_Name_Stmt_Node.Name );
    A_Stmt_Node_List_Cursor := Outstanding_Goto_Targets.Find( Name );
    if A_Stmt_Node_List_Cursor /= Types.AST.Bounded_String_To_Stmt_Node_List_Hashed_Map.No_Element then

      A_Stmt_Node_List      := Types.AST.Bounded_String_To_Stmt_Node_List_Hashed_Map.Element( A_Stmt_Node_List_Cursor );
      A_Stmt_Node_Cursor    := A_Stmt_Node_List.First;

      while A_Stmt_Node_Cursor /= Types.AST.Stmt_Node_List.No_Element loop

        A_Stmt_Node         := Types.AST.Stmt_Node_List.Element( A_Stmt_Node_Cursor );
        Write_Transition(   A_Stmt_Node,
                            Types.AST.Stmt_Node_Ptr( A_Name_Stmt_Node ) );
        A_Stmt_Node_Cursor  := Types.AST.Stmt_Node_List.Next( A_Stmt_Node_Cursor );

      end loop;

      Outstanding_Goto_Targets.Delete( A_Stmt_Node_List_Cursor );

    end if;
  end       Write_Outstanding_Goto_Targets;

  procedure Process_Do_Statements         ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr         ) is
    use type Types.AST.Sequence_Node_List.Cursor;
    use type Types.AST.Step_Node_List.Cursor;
    Next                    : Types.AST.Step_Node_List.Cursor;
    A_Do_Stmt_Node          : Types.AST.Do_Stmt_Node_Ptr;
    After_A_Do_Stmt_Node    : Types.AST.Stmt_Node_Ptr;
    A_Sequence_List_Cursor  : Types.AST.Sequence_Node_List.Cursor;
    Another_Sequence_Node   : Types.AST.Sequence_Node_Ptr;
    A_Stmt_Node             : Types.AST.Stmt_Node_Ptr;
  begin
    Logger.Info( "START Processing DO-OD Statement-s only" );
    Next := A_Sequence_Node.Stmt_Step_List.First;
    while Next /= Types.AST.Step_Node_List.No_Element loop
      Utility.Get_Next_Do_Statement( Next, A_Do_Stmt_Node );
      if A_Do_Stmt_Node /= null then
        Utility.Get_Next_Statement_In_Steps( Next, After_A_Do_Stmt_Node );
        Code_Generation_Assert( After_A_Do_Stmt_Node /= null, "A Do_Stmt_Node is not followed by another Stmt!" );
        -- Sequences of options in A_Do_Stmt_Node.Options
        A_Sequence_List_Cursor := A_Do_Stmt_Node.Options.Sequence_List.First;
        while A_Sequence_List_Cursor /= Types.AST.Sequence_Node_List.No_Element loop
          -- A sequence
          Another_Sequence_Node := Types.AST.Sequence_Node_List.Element( A_Sequence_List_Cursor );
          -- CASE A: Link A_Do_Stmt_Node to first stmt in option
          A_Stmt_Node := Utility.Get_First_Statement_In_Sequence( Another_Sequence_Node );
          Code_Generation_Assert( A_Stmt_Node /= null, "No first Step in a Sequence in an Option is a Stmt_Step!" );
          Write_Transition( Types.AST.Stmt_Node_Ptr( A_Do_Stmt_Node ), A_Stmt_Node );
          -- CASE B: Link last stmt in option, depending on 2 possible subcases
          A_Stmt_Node := Utility.Get_Last_Statement_In_Sequence( Another_Sequence_Node );
          Code_Generation_Assert( A_Stmt_Node /= null, "No last Step in a Sequence in an Option is a Stmt_Step!" );
          if A_Stmt_Node.Tag = Types.AST.Node_Break_Stmt then
            -- CASE B-1: Link out of Do_Stmt, after it, to the next Stmt
            Write_Transition( A_Stmt_Node, After_A_Do_Stmt_Node );
          else
            -- CASE B-2: Link back to Do_Stmt
            Write_Transition( A_Stmt_Node, Types.AST.Stmt_Node_Ptr( A_Do_Stmt_Node ) );
          end if;
          -- Next Sequence in Options
          A_Sequence_List_Cursor := Types.AST.Sequence_Node_List.Next( A_Sequence_List_Cursor );
        end loop;
      end if;
    end loop;
    Logger.Info( "STOP Processing DO-OD Statement-s only" );
  end Process_Do_Statements;

  procedure Process_If_Statements         ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr         ) is
    use type Types.AST.Step_Node_List.Cursor;
    use type Types.AST.Sequence_Node_List.Cursor;
    Next                    : Types.AST.Step_Node_List.Cursor;
    An_If_Stmt_Node         : Types.AST.If_Stmt_Node_Ptr;
    After_An_If_Stmt_Node   : Types.AST.Stmt_Node_Ptr;
    A_Sequence_List_Cursor  : Types.AST.Sequence_Node_List.Cursor;
    Another_Sequence_Node   : Types.AST.Sequence_Node_Ptr;
    A_Stmt_Node             : Types.AST.Stmt_Node_Ptr;
    -- NOTE: Good example of an unused variable not detected by GNAT GPL 2008.
    --Name                    : Types.CodeGen.Name := Types.CodeGen.Blanks;
  begin
    Logger.Info( "START Processing IF-FI Statement-s only" );
    Next := A_Sequence_Node.Stmt_Step_List.First;
    while Next /= Types.AST.Step_Node_List.No_Element loop
      Utility.Get_Next_If_Statement( Next, An_If_Stmt_Node );
      if An_If_Stmt_Node /= null then
        Utility.Get_Next_Statement_In_Steps( Next, After_An_If_Stmt_Node );
        Code_Generation_Assert( After_An_If_Stmt_Node /= null, "An If_Stmt_Node is not followed by another Stmt!" );
        -- Sequences of Options in An_If_Stmt_Node.Options
        A_Sequence_List_Cursor := An_If_Stmt_Node.Options.Sequence_List.First;
        while A_Sequence_List_Cursor /= Types.AST.Sequence_Node_List.No_Element loop
          -- A sequence 
          Another_Sequence_Node := Types.AST.Sequence_Node_List.Element( A_Sequence_List_Cursor );
          -- CASE A: Link An_If_Stmt_Node to the first stmt in this sequence
          A_Stmt_Node := Utility.Get_First_Statement_In_Sequence( Another_Sequence_Node );
          Code_Generation_Assert( A_Stmt_Node /= null, "No first Step in a Sequence in an Option is a Stmt_Step!" );
          Write_Transition( Types.AST.Stmt_Node_Ptr( An_If_Stmt_Node ), A_Stmt_Node );
          -- CASE B: Link last stmt in this sequence to the stmt after this if stmt
          A_Stmt_Node := Utility.Get_Last_Statement_In_Sequence( Another_Sequence_Node );
          Code_Generation_Assert( A_Stmt_Node /= null, "No last Step in a Sequence in an Option is a Stmt_Step!" );
          -- Link out of If_Stmt, after it, to the next Stmt...
          -- NOTE: ...but only IF the source is NOT a GOTO statement
          -- TODO: Must enforce in AST that in other cases, GOTO must be last statement in sequence.
          if A_Stmt_Node.Tag /= Types.AST.Node_Goto_Stmt then
            Write_Transition( A_Stmt_Node, After_An_If_Stmt_Node );
          end if;
          -- Next Sequence in Options
          A_Sequence_List_Cursor := Types.AST.Sequence_Node_List.Next( A_Sequence_List_Cursor );
        end loop;
      end if;
    end loop;
    Logger.Info( "STOP Processing IF-FI Statement-s only" );
  end Process_If_Statements;

  -- TODO: further refactor code
  procedure Process_Goto_Statements       ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr         ) is
    use type Types.AST.Step_Node_List.Cursor;
    Next                    : Types.AST.Step_Node_List.Cursor;
    A_Step_Node             : Types.AST.Step_Node_Ptr;
    A_Stmt_Step_Node        : Types.AST.Stmt_Step_Node_Ptr;
    A_Stmt_Node             : Types.AST.Stmt_Node_Ptr;
    A_Goto_Stmt_Node        : Types.AST.Goto_Stmt_Node_Ptr;
    A_Name_Stmt_Node        : Types.AST.Name_Stmt_Node_Ptr;
    After_A_Name_Stmt_Node  : Types.AST.Stmt_Node_Ptr;
    Name                    : Types.Lexer.Bound_String.Bounded_String;
    Target                  : Types.AST.Node_Ptr;
  begin

    Logger.Info( "START Processing GOTO Statement-s only" );
    Next := A_Sequence_Node.Stmt_Step_List.First;

    while Next /= Types.AST.Step_Node_List.No_Element loop
      A_Step_Node := Types.AST.Step_Node_List.Element( Next );

      if A_Step_Node.Tag = Types.AST.Node_Stmt_Step then
        A_Stmt_Step_Node := Types.AST.Stmt_Step_Node_Ptr( A_Step_Node );
        A_Stmt_Node := A_Stmt_Step_Node.Statement;

        case A_Stmt_Node.Tag is

          when Types.AST.Node_Goto_Stmt =>

            A_Goto_Stmt_Node := Types.AST.Goto_Stmt_Node_Ptr( A_Stmt_Node );
            -- Find GOTO target
            Name := Types.Lexer.Element_In_Table( A_Goto_Stmt_Node.Name );
            Target := Scope_Get( Name );
            if Target /= null then
              Code_Generation_Assert( Target.Tag = Types.AST.Node_Name_Stmt,
                                      "GOTO target name has not been reserved for a NAMED statement!"  );
              A_Name_Stmt_Node := Types.AST.Name_Stmt_Node_Ptr( Target );
              Logger.Info( "GOTO-ing to named statement : " & Types.Lexer.Bound_String.To_String( Name ) );
              Write_Transition( Types.AST.Stmt_Node_Ptr( A_Goto_Stmt_Node ),
                                Types.AST.Stmt_Node_Ptr( A_Name_Stmt_Node ) );
            else
              -- to be determined (TBD) later in this process
              Insert_Outstanding_Goto_Target( Name, A_Goto_Stmt_Node );
            end if;
            Types.AST.Step_Node_List.Next( Next );

          when Types.AST.Node_Name_Stmt =>

            A_Name_Stmt_Node := Types.AST.Name_Stmt_Node_Ptr( A_Stmt_Node );

            -- NOTE: wire transition FROM Label_Name : Stmt directly TO the first Stmt in its Sequence child
            A_Stmt_Node := Utility.Get_First_Statement_In_Sequence( A_Name_Stmt_Node.Sequence );
            Code_Generation_Assert( A_Stmt_Node /= null, "No first Statement found in Sequence!" );
            Logger.Info( "FROM Label_NAME : Stmt TO first Stmt in Sequence" );
            Write_Transition( Types.AST.Stmt_Node_Ptr( A_Name_Stmt_Node ), A_Stmt_Node );

            -- NOTE: write transition FROM the last Stmt in its Sequence child to Stmt after Label_Name : Stmt
            -- guaranteed by Prepend_And_Append_Sequence_With_Noop to have Stmt after this

            -- fast forward till the next Stmt_Step in A_Sequence_Node
            Utility.Get_Next_Statement_In_Steps( Next, After_A_Name_Stmt_Node );
            Code_Generation_Assert( After_A_Name_Stmt_Node /= null, "After_A_Name_Stmt_Node is null!" );
            
            A_Stmt_Node := Utility.Get_Last_Statement_In_Sequence( A_Name_Stmt_Node.Sequence );
            Code_Generation_Assert( A_Stmt_Node /= null, "No last Statement found in Sequence!" );
            Logger.Info( "FROM last Stmt in Sequence TO After Label_Name : Stmt" );
            Write_Transition( A_Stmt_Node, Types.AST.Stmt_Node_Ptr( After_A_Name_Stmt_Node ) );

            -- NOTE: write Outstanding_Goto_Targets here
            Write_Outstanding_Goto_Targets( A_Name_Stmt_Node );

          when others =>
            Types.AST.Step_Node_List.Next( Next );

        end case;

      else
        Types.AST.Step_Node_List.Next( Next );
      end if;

    end loop;

    Logger.Info( "STOP Processing GOTO Statement-s only" );

  end Process_Goto_Statements;

  procedure Process_Nested_Atomic_D_Step_Statements( A_Sequence_Node : Types.AST.Sequence_Node_Ptr ) is
    use type Types.AST.Step_Node_List.Cursor;
    Next                  : Types.AST.Step_Node_List.Cursor;
    A_Stmt_Node           : Types.AST.Stmt_Node_Ptr;
    Another_Sequence_Node : Types.AST.Sequence_Node_Ptr;
    After_Stmt_Node       : Types.AST.Stmt_Node_Ptr;
    Target_Stmt_Node      : Types.AST.Stmt_Node_Ptr;
    Previous_In_Atomic    : Types.CodeGen.Byte range 0 .. 1;
  begin
    Logger.Info( "START Processing Nested, Atomic or D_Step Sequences-s only" );
    Next := A_Sequence_Node.Stmt_Step_List.First;
    while Next /= Types.AST.Step_Node_List.No_Element loop
      Previous_In_Atomic  := In_Atomic;
      Utility.Get_Next_Nested_Atomic_D_Step_Statement( Next, A_Stmt_Node, Another_Sequence_Node );
      if A_Stmt_Node /= null then

        -- In_Atomic
        if A_Stmt_Node.Tag = Types.AST.Node_Atomic_Stmt then
          In_Atomic := 1;
        end if;
        Utility.Get_Next_Statement_In_Steps( Next, After_Stmt_Node );
        Code_Generation_Assert( After_Stmt_Node /= null, "No Stmt follows nested Sequence!" );
        -- Link A_Sequence_Stmt_Node.State_Number to the first Stmt inside
        Target_Stmt_Node := Utility.Get_First_Statement_In_Sequence( Another_Sequence_Node );
        Code_Generation_Assert( Target_Stmt_Node /= null, "No target Stmt to link to inside nested Sequence!" );
        Write_Transition( A_Stmt_Node, Target_Stmt_Node );
        -- Do not wrongly mark the next transition
        In_Atomic := Previous_In_Atomic;
        -- Link the last Stmt inside to the next Stmt outside
        Target_Stmt_Node := Utility.Get_Last_Statement_In_Sequence( Another_Sequence_Node );
        Code_Generation_Assert( Target_Stmt_Node /= null, "No target Stmt to link to inside nested Sequence!" );
        Write_Transition( Target_Stmt_Node, After_Stmt_Node );

      else
        In_Atomic := Previous_In_Atomic;
      end if;
    end loop;
    Logger.Info( "STOP Processing Nested, Atomic or D_Step Sequence-s only" );
  end Process_Nested_Atomic_D_Step_Statements;

  procedure Step             ( A_Step_Node     : Types.AST.Step_Node_Ptr             ) is
    A_Stmt_Step_Node      : Types.AST.Stmt_Step_Node_Ptr;
    A_Decl_Lst_Step_Node  : Types.AST.Decl_Lst_Step_Node_Ptr;
    A_Xr_Xs_Step_Node     : Types.AST.Xr_Xs_Step_Node_Ptr;
  begin
    Logger.Info( "Walking a Step..." );
    case A_Step_Node.Tag is

      when Types.AST.Node_Decl_Lst_Step =>
        if Process_Declarations_First = True then
          Logger.Info( "Walking a Decl_Lst_Step..." );
          A_Decl_Lst_Step_Node := Types.AST.Decl_Lst_Step_Node_Ptr( A_Step_Node );
          -- NOTE: do not push/pop scope here; see the Proctype procedure
          Decl_Lst( A_Decl_Lst_Step_Node.Decl_Lst );
        end if;
      when Types.AST.Node_Stmt_Step     =>
        if Process_Declarations_First = False then
          Logger.Info( "Walking a Stmt_Step..." );
          A_Stmt_Step_Node := Types.AST.Stmt_Step_Node_Ptr( A_Step_Node );
          -- TODO: walk Decl_Lst-s in Sequence-s and Options in Stmt-s
          -- heck, should Decl_Lst-s even be allowed in there?
          Stmt( A_Stmt_Step_Node.Statement );
          if A_Stmt_Step_Node.Unless /= null then
            --TODO: UNLESS statements
            Raise_Unsupported_Construct( "UNLESS statements" );
          end if;
        end if;
      when Types.AST.Node_Xr_Xs_Step    =>
        if Process_Declarations_First = False then
          Logger.Info( "Walking a Xr_Xs_Step" );
          A_Xr_Xs_Step_Node := Types.AST.Xr_Xs_Step_Node_Ptr( A_Step_Node );
        end if;
      when others                       =>
        Code_Generation_Assert( False, "Unrecognized A_Step_Node!" );

    end case;
  end Step;

  procedure Const( A_Const_Node : Types.AST.Const_Node_Ptr ) is
    Code  : Types.CodeGen.Byte_Code;
  begin

    Logger.Info( "Walking a Const" );
    Code.Operator   := Compile_Global.iconst;
    Code.Operand_1  := Add_To_Natural_Table( A_Const_Node.Number );
    Logger.Info(  "constant = "
                  & Types.Lexer.Promela_Natural'Image( Types.CodeGen.Get_From_Natural_Table( Code.Operand_1 ) ) );
    Emit( Code );

  end Const;

  procedure Send_Args( A_Send_Args_Node : Types.AST.Send_Args_Node_Ptr ) is
  begin

    Logger.Info( "Walking a Send_Args" );
    -- Grammar guarantees arguments won't be empty
    Arg_Lst( A_Send_Args_Node.Arguments );

  end Send_Args;

  procedure Send( A_Send_Node : Types.AST.Send_Node_Ptr ) is
    Code  : Types.CodeGen.Byte_Code;
  begin

    -- no checks here, assuming it has all been done in AST
    Logger.Info( "Walking a Send" );

    -- First, Walk send arguments in reverse
    Send_Args( A_Send_Node.Arguments );

    case A_Send_Node.Tag is
      when  Types.AST.Node_Fifo_Send    =>
        Code.Operator := Compile_Global.fifo_send;
      when  Types.AST.Node_Sorted_Send  =>
        Code.Operator := Compile_Global.sorted_send;
      when others                       =>
        AST.Tools.Semantic_Assert( False, "Unrecognized A_Send_Node!" );
    end case;

    -- next, load the channel variable onto the stack
    Load_Varref( A_Send_Node.Source );
    -- number of arguments
    Code.Operand_1 := Types.CodeGen.Operand( A_Send_Node.Arguments.Arguments.Any_Expr_List.Length );
    -- finally, emit code for send instruction
    Emit( Code );

  end Send;

  -- A near-duplicate of Store_Varref.
  -- The only difference is that this uses the operand-stack-side-effect-free
  -- set of X_store_receive instructions. In fact, the interpreter must complain
  -- should it not see these instructions followed by a channel receive instruction.
  -- This procedure could probably be refactored to share common code.
  -- It is just mentally easier to separate them right now.
  procedure Load_Address( A_Varref_Node : Types.AST.Varref_Node_Ptr ) is
    An_Ivar_Node          : Types.AST.Ivar_Node_Ptr;
    Built_In              : Types.AST.Built_In_Data_Type_Ptr;
    An_Array              : Types.AST.Array_Data_Type_Ptr;
    Offset_In_Byte        : Types.CodeGen.Byte;
    Name                  : Types.Lexer.Bound_String.Bounded_String;
    Code                  : Types.CodeGen.Byte_Code;
  begin
    Logger.Info( "Computing Varref address for a receive argument..." );

    case  A_Varref_Node.Declaration.Tag is

      when  Types.AST.Node_Ivar           |
            Types.AST.Node_Any_Expr_Ivar  =>

        An_Ivar_Node := Types.AST.Ivar_Node_Ptr( A_Varref_Node.Declaration );

        case An_Ivar_Node.The_Type.Kind is

          when Types.AST.Built_In_Type  =>

            Built_In := Types.AST.Built_In_Data_Type_Ptr( An_Ivar_Node.The_Type );
            if A_Varref_Node.Array_Offset /= null then
              -- useless indexing, simply check that 0 <= index < 1.
              -- the first implementation was to assert index == 0.
              -- this implementation, which uses instructions reserved
              -- for arrays, was requested to distinguish between assert
              -- and other runtime errors.
              -- first, compute the index and push it onto stack
              Any_Expr( A_Varref_Node.Array_Offset );
              -- check 0 <= index < 1
              Code.Operator   := Compile_Global.check_index;
              Code.Operand_1  := 1;
              Emit( Code );
            end if;

            -- load address: simply push address for Varref[0] onto stack
            Code.Operator   := Compile_Global.load_address;
            Code.Operand_1  := Types.CodeGen.Operand( An_Ivar_Node.Index );
            -- size of built-in type
            Offset_In_Byte := Types.CodeGen.Byte( Built_In.Size_In_Bytes );
            Logger.Info( "Byte Offset = " & Types.CodeGen.Byte'Image( Offset_In_Byte ) );
            Code.Operand_2 := Types.CodeGen.Operand( Offset_In_Byte );
            Emit( Code );

          when Types.AST.Array_Type =>

            An_Array := Types.AST.Array_Data_Type_Ptr( An_Ivar_Node.The_Type );
            if A_Varref_Node.Array_Offset /= null then
              -- one design considered one instruction to do
              -- both index checking and storing/loading
              -- but this design was adopted
              -- compute the index and push it onto stack
              Any_Expr( A_Varref_Node.Array_Offset );
              -- check 0 <= index < length 
              Code.Operator   := Compile_Global.check_index;
              Code.Operand_1  := An_Array.Number_Of_Elements;
              Emit( Code );
              -- because there are 2 instructions for array store/load
              -- the convention is that they are to be read and executed
              -- consecutively; we will not push the index again
              -- (computation may be expensive) so the interpreter
              -- should remember the index with the check_index instruction
            else
              -- the varref[ 0 ] case
              -- push 0 as index
              Code.Operator   := Compile_Global.bipush;
              Code.Operand_1  := 0;
              Emit( Code );
              -- check 0 <= index < length 
              Code.Operator   := Compile_Global.check_index;
              Code.Operand_1  := An_Array.Number_Of_Elements;
              Emit( Code );
            end if;

            -- load address: push address for Varref[X], with X memorized from check_index, onto stack
            Code.Operator := Compile_Global.load_address;
            -- offset
            Code.Operand_1 := Types.CodeGen.Operand( An_Ivar_Node.Index );
            -- size of array base type
            Offset_In_Byte := Types.CodeGen.Byte( An_Array.Base_Type.Size_In_Bytes );
            Logger.Info( "Byte Offset = " & Types.CodeGen.Byte'Image( Offset_In_Byte ) );
            Code.Operand_2 := Types.CodeGen.Operand( Offset_In_Byte );
            Emit( Code );

          when  Types.AST.User_Type =>
            Raise_Unsupported_Construct( "storing into user-defined types" );

        end case;

      when Types.AST.Node_Mtype =>
        Code_Generation_Assert( False, "Cannot store into mtype constants!" );

      when  others  =>
        Code_Generation_Assert( False, "Unrecognized variable declaration!" );

    end case;

  end Load_Address;

  procedure Recv_Arg( A_Recv_Arg_Node : Types.AST.Recv_Arg_Node_Ptr ) is
    A_Varref_Recv_Arg_Node  : Types.AST.Varref_Recv_Arg_Node_Ptr;
    An_Eval_Recv_Arg_Node   : Types.AST.Eval_Recv_Arg_Node_Ptr;
    A_Const_Recv_Arg_Node   : Types.AST.Const_Recv_Arg_Node_Ptr;
    Code                    : Types.CodeGen.Byte_Code;
  begin

    Logger.Info( "Walking a Recv_Arg" );

    case A_Recv_Arg_Node.Tag is

      when Types.AST.Node_Varref_Recv_Arg =>
        A_Varref_Recv_Arg_Node := Types.AST.Varref_Recv_Arg_Node_Ptr( A_Recv_Arg_Node );
        Load_Address( A_Varref_Recv_Arg_Node.Variable );

      when Types.AST.Node_Eval_Recv_Arg   =>
        An_Eval_Recv_Arg_Node := Types.AST.Eval_Recv_Arg_Node_Ptr( A_Recv_Arg_Node );
        Load_Varref( An_Eval_Recv_Arg_Node.Argument );

      when Types.AST.Node_Const_Recv_Arg  =>
        A_Const_Recv_Arg_Node := Types.AST.Const_Recv_Arg_Node_Ptr( A_Recv_Arg_Node );
        Const( A_Const_Recv_Arg_Node.A_Constant );
        if A_Const_Recv_Arg_Node.Negate = True then
          Code.Operator := Compile_Global.ineg;
          Emit( Code );
        end if;

      when others                         =>
        AST.Tools.Semantic_Assert( False, "Unrecognized A_Recv_Arg_Node!" );

    end case;

  end Recv_Arg;

  procedure Recv_Args( A_Recv_Args_Node : Types.AST.Recv_Args_Node_Ptr ) is
    use type Types.AST.Recv_Arg_Node_List.Cursor;
    Previous        : Types.AST.Recv_Arg_Node_List.Cursor;
    A_Recv_Arg_Node : Types.AST.Recv_Arg_Node_Ptr;
  begin

    Logger.Info( "Walking a Recv_Args" );
    -- Grammar guarantees arguments won't be empty;
    -- Walk receive arguments in reverse
    Previous := A_Recv_Args_Node.Arguments.Last;
    while Previous /= Types.AST.Recv_Arg_Node_List.No_Element loop
      A_Recv_Arg_Node := Types.AST.Recv_Arg_Node_List.Element( Previous );
      Recv_Arg( A_Recv_Arg_Node );
      Types.AST.Recv_Arg_Node_List.Previous( Previous );
    end loop;

  end Recv_Args;

  procedure Recv( A_Recv_Node : Types.AST.Recv_Node_Ptr ) is
    Code  : Types.CodeGen.Byte_Code;
  begin

    Logger.Info( "Walking a Recv" );

    -- first, generate code for receive arguments.
    -- it is important that we do this now because the Erigone
    -- interpreter cares about the last instruction in this statement.
    -- as for the tricky store-into-a-variable receive argument,
    -- we solve the problem by using the load_address instruction; 
    -- see Load_Address.
    -- The interpreter must, of course, be careful in how it handles receive arguments.
    Recv_Args( A_Recv_Node.Arguments );

    case A_Recv_Node.Tag is
      when  Types.AST.Node_Move_Fifo_Recv   =>
        Code.Operator := Compile_Global.move_fifo_receive;
      when  Types.AST.Node_Copy_Fifo_Recv   =>
        Code.Operator := Compile_Global.copy_fifo_receive;
      when  Types.AST.Node_Move_Random_Recv =>
        Code.Operator := Compile_Global.move_random_receive;
      when  Types.AST.Node_Copy_Random_Recv =>
        Code.Operator := Compile_Global.copy_random_receive;
      when others                       =>
        AST.Tools.Semantic_Assert( False, "Unrecognized A_Recv_Node!" );
    end case;

    -- next, load the channel variable onto the stack
    Load_Varref( A_Recv_Node.Target );
    -- number of arguments
    Code.Operand_1 := Types.CodeGen.Operand( A_Recv_Node.Arguments.Arguments.Length );
    -- finally, emit code for receive instruction
    Emit( Code );

  end Recv;
 
  procedure Stmt( A_Stmt_Node     : Types.AST.Stmt_Node_Ptr             ) is
    An_Any_Expr_Stmt_Node : Types.AST.Any_Expr_Stmt_Node_Ptr;
    An_Assert_Stmt_Node   : Types.AST.Assert_Stmt_Node_Ptr;
    An_Assign_Stmt_Node   : Types.AST.Assign_Stmt_Node_Ptr;
    An_Atomic_Stmt_Node   : Types.AST.Atomic_Stmt_Node_Ptr;
    A_Do_Stmt_Node        : Types.AST.Do_Stmt_Node_Ptr;
    A_D_Step_Stmt_Node    : Types.AST.D_Step_Stmt_Node_Ptr;
    An_If_Stmt_Node       : Types.AST.If_Stmt_Node_Ptr;
    A_Name_Stmt_Node      : Types.AST.Name_Stmt_Node_Ptr;
    A_Printf_Stmt_Node    : Types.AST.Printf_Stmt_Node_Ptr;
    A_Printm_Stmt_Node    : Types.AST.Printm_Stmt_Node_Ptr;
    A_Recv_Stmt_Node      : Types.AST.Recv_Stmt_Node_Ptr;
    A_Send_Stmt_Node      : Types.AST.Send_Stmt_Node_Ptr;
    A_Sequence_Stmt_Node  : Types.AST.Sequence_Stmt_Node_Ptr;
    A_Mtype_Node          : Types.AST.Mtype_Node_Ptr;
    Name                  : Types.Lexer.Bound_String.Bounded_String;
    An_Ivar_Node          : Types.AST.Ivar_Node_Ptr;
    Built_In              : Types.AST.Built_In_Data_Type_Ptr;
    Previous_In_Atomic    : Types.CodeGen.Byte range 0 .. 1 := 0;
    Code                  : Types.CodeGen.Byte_Code;
  begin
    Logger.Info( "Walking a Stmt..." );
    Logger.Info( "State number : "  & Positive'Image( A_Stmt_Node.State_Number  ) );
    Logger.Info( "Line number : "   & Positive'Image( A_Stmt_Node.Line_Number   ) );

    case A_Stmt_Node.Tag is

      when Types.AST.Node_Any_Expr_Stmt =>
        An_Any_Expr_Stmt_Node := Types.AST.Any_Expr_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking an Any_Expr_Stmt_Node" );
        Any_Expr( An_Any_Expr_Stmt_Node.Expression );

      when Types.AST.Node_Assign_Stmt =>
        An_Assign_Stmt_Node := Types.AST.Assign_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking an Assign_Stmt_Node" );
        Assign( An_Assign_Stmt_Node.Assignment );

      when Types.AST.Node_Assert_Stmt =>
        An_Assert_Stmt_Node := Types.AST.Assert_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking an Assert_Stmt_Node" );
        Any_Expr( An_Assert_Stmt_Node.Expression );
        Code.Operator := Compile_Global.assert;
        Emit( Code );

      when Types.AST.Node_Atomic_Stmt =>
        An_Atomic_Stmt_Node := Types.AST.Atomic_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking an Atomic_Stmt_Node" );
        Previous_In_Atomic := In_Atomic;
        In_Atomic := 1;
        Sequence( An_Atomic_Stmt_Node.Sequence );
        In_Atomic := Previous_In_Atomic;
        -- Noop for an Atomic
        Emit( Code );

      when Types.AST.Node_Break_Stmt  =>
        Logger.Info( "Walking a Break_Stmt_Node" );
        -- Noop for a Break
        Emit( Code );

      when Types.AST.Node_D_Step_Stmt =>
        A_D_Step_Stmt_Node := Types.AST.D_Step_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking a D_Step_Stmt_Node" );
        Sequence( A_D_Step_Stmt_Node.Sequence );

      when Types.AST.Node_Do_Stmt =>
        A_Do_Stmt_Node := Types.AST.Do_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking a Do_Stmt_Node" );
        Options( A_Do_Stmt_Node.Options );
        -- Noop for a DO
        Emit( Code );

      when Types.AST.Node_Else_Stmt =>
        Logger.Info( "Walking an Else_Stmt_Node" );
        Code.Operator := Compile_Global.logic_else;
        Emit( Code );

      when Types.AST.Node_Goto_Stmt =>
        Logger.Info( "Walking a Goto_Stmt_Node" );
        -- Noop for a GOTO
        Emit( Code );

      when Types.AST.Node_If_Stmt =>
        An_If_Stmt_Node := Types.AST.If_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking an If_Stmt_Node" );
        Options( An_If_Stmt_Node.Options );
        -- Noop for an IF
        Emit( Code );

      when Types.AST.Node_Name_Stmt =>
        A_Name_Stmt_Node := Types.AST.Name_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking a Name_Stmt_Node" );
        -- Add a GOTO target in this scope
        Name := Types.Lexer.Element_In_Table( A_Name_Stmt_Node.Name );
        Logger.Info( "Adding NAMED statement : " & Types.Lexer.Bound_String.To_String( Name ) );
        Code_Generation_Assert( Scope_Put( Name, Types.AST.Node_Ptr( A_Stmt_Node ) ) = True,
                                "Redefining existing identifier '"
                                & Types.Lexer.Bound_String.To_String( Name )
                                & "' in this scope!"  );
        -- TODO: special handling of ACCEPT and END labels
        -- TODO: does Erigone handle PROGRESS label yet?
        -- Process Stmt as Sequence instead, for proper transition wiring
        -- Before that, emit noop for this labeled statement
        Emit( Code );
        Flush( A_Stmt_Node );
        Sequence( A_Name_Stmt_Node.Sequence );
        -- emit noop *again* for this labeled statement because the common Flush
        -- call at the end of the procedure *reassigns* byte code to this A_Stmt_Node
        Emit( Code );
        -- After this, we go out of this Stmt, go back into Sequence,
        -- where we process Goto_Stmt-s, which would then have the number to this Stmt.

      when Types.AST.Node_Printf_Stmt =>
        A_Printf_Stmt_Node := Types.AST.Printf_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking a Printf_Stmt_Node" );
        Arg_Lst( A_Printf_Stmt_Node.Arguments );
        Code.Operator := Compile_Global.printf;
        Code.Operand_1 := Add_To_String_Table( A_Printf_Stmt_Node.String );
        if A_Printf_Stmt_Node.Arguments /= null then
          Code.Operand_2 := Types.CodeGen.Operand( A_Printf_Stmt_Node.Arguments.Any_Expr_List.Length );
        -- else 0 by default
        end if;
        Emit( Code );

      when Types.AST.Node_Printm_Stmt =>
        A_Printm_Stmt_Node := Types.AST.Printm_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking a Printm_Stmt_Node" );
        case A_Printm_Stmt_Node.Declaration.Tag is
          when  Types.AST.Node_Mtype          =>
            -- bipush
            A_Mtype_Node := Types.AST.Mtype_Node_Ptr( A_Printm_Stmt_Node.Declaration );
            Name := Types.Lexer.Element_In_Table( A_Printm_Stmt_Node.Name );
            Code.Operator := Compile_Global.bipush;
            Code.Operand_1 := Types.CodeGen.Operand( A_Mtype_Node.Names_Map.Element( Name ) );
          when  Types.AST.Node_Ivar           |
                Types.AST.Node_Any_Expr_Ivar  =>
            -- byte_load; TODO: not-so-silent failure (i.e. not mtype, bla bla)
            An_Ivar_Node := Types.AST.Ivar_Node_Ptr( A_Printm_Stmt_Node.Declaration );
            if An_Ivar_Node.The_Type.all in Types.AST.Built_In_Data_Type then
              Built_In := Types.AST.Built_In_Data_Type_Ptr( An_Ivar_Node.The_Type );
              if Built_In.Built_In_Kind = Types.AST.Mtype then
                Code.Operator := Compile_Global.byte_load;
                Code.Operand_1 := Types.CodeGen.Operand( An_Ivar_Node.Index );
              end if;
            end if;
          when others                         =>
            Code_Generation_Assert( False, "Unrecognized A_Printf_Stmt_Node.Declaration!" );
        end case;
        Emit( Code );
        -- printm
        Code.Operator := Compile_Global.printm;
        Emit( Code );

      when Types.AST.Node_Noop_Stmt =>
        Logger.Info( "Walking a Noop_Stmt_Node" );
        Emit( Code );

      when Types.AST.Node_Recv_Stmt =>
        A_Recv_Stmt_Node := Types.AST.Recv_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking a Recv_Stmt_Node" );
        -- don't emit code, Recv will take care of it
        Recv( A_Recv_Stmt_Node.Receive );

      when Types.AST.Node_Sequence_Stmt =>
        A_Sequence_Stmt_Node := Types.AST.Sequence_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking a Sequence_Stmt_Node" );
        Sequence( A_Sequence_Stmt_Node.Sequence );
        -- Noop for a { Sequence }
        Emit( Code );

      when Types.AST.Node_Send_Stmt =>
        A_Send_Stmt_Node := Types.AST.Send_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking a Send_Stmt_Node" );
        -- don't emit code, Send will take care of it
        Send( A_Send_Stmt_Node.Send );

      when others =>
        Code_Generation_Assert( False, "Unrecognized A_Stmt_Node!" );

    end case;
    Flush( A_Stmt_Node );

  end Stmt;
  
  procedure Assign           ( An_Assign_Node  : Types.AST.Assign_Node_Ptr           ) is
    An_Any_Expr_Assign_Node : Types.AST.Any_Expr_Assign_Node_Ptr;
    An_Ivar_Node            : Types.AST.Ivar_Node_Ptr;
    Code                    : Types.CodeGen.Byte_Code;
  begin
    Logger.Info( "Walking an Assign" );
    -- TODO: not-so-silent failure (not Ivar_Node, bla bla)
    if  An_Assign_Node.L_Value.Declaration.Tag = Types.AST.Node_Ivar           or else
        An_Assign_Node.L_Value.Declaration.Tag = Types.AST.Node_Any_Expr_Ivar  then
      case An_Assign_Node.Tag is
        when Types.AST.Node_Any_Expr_Assign   =>
          An_Any_Expr_Assign_Node := Types.AST.Any_Expr_Assign_Node_Ptr( An_Assign_Node );
          Any_Expr( An_Any_Expr_Assign_Node.R_Value );
          Store_Varref( An_Any_Expr_Assign_Node.L_Value );
        when Types.AST.Node_Decrement_Assign  =>
          An_Ivar_Node    := Types.AST.Ivar_Node_Ptr( An_Assign_Node.L_Value.Declaration );
          Code.Operator   := Compile_Global.idec;
          Code.Operand_1  := Types.CodeGen.Operand( An_Ivar_Node.Index );
          Code.Operand_2  := 1;
          Emit( Code );
        when Types.AST.Node_Increment_Assign  =>
          An_Ivar_Node    := Types.AST.Ivar_Node_Ptr( An_Assign_Node.L_Value.Declaration );
          Code.Operator   := Compile_Global.iinc;
          Code.Operand_1  := Types.CodeGen.Operand( An_Ivar_Node.Index );
          Code.Operand_2  := 1;
          Emit( Code );
        when others                           =>
          Code_Generation_Assert( False, "Unrecognized An_Assign_Node!" );
      end case;
    end if;
  end Assign;
  
  -- TODO: consider whether it is general enough for send_args, not just printf and run
  procedure Arg_Lst          ( An_Arg_Lst_Node : Types.AST.Arg_Lst_Node_Ptr          ) is
    use type Types.AST.Any_Expr_Node_List.Cursor;
    Previous  : Types.AST.Any_Expr_Node_List.Cursor;
  begin
    Logger.Info( "Walking an Arg_Lst in reverse..." );
    if An_Arg_Lst_Node /= null then
      -- we walk the argument list in reverse, so that they are placed properly on the stack
      Previous := An_Arg_Lst_Node.Any_Expr_List.Last;
      while Previous /= Types.AST.Any_Expr_Node_List.No_Element loop
        Any_Expr( Types.AST.Any_Expr_Node_List.Element( Previous ) );
        Previous := Types.AST.Any_Expr_Node_List.Previous( Previous );
      end loop;
    end if;
  end Arg_Lst;
  
  procedure Options          ( An_Options_Node : Types.AST.Options_Node_Ptr          ) is
    use type Types.AST.Sequence_Node_List.Cursor;
    Next                          : Types.AST.Sequence_Node_List.Cursor;
  begin
    Logger.Info( "Walking an Options..." );
    Next          := An_Options_Node.Sequence_List.First;
    while Next /= Types.AST.Sequence_Node_List.No_Element loop
      In_Options  := True;
      Sequence( Types.AST.Sequence_Node_List.Element( Next ) );
      In_Options  := False;
      Next        := Types.AST.Sequence_Node_List.Next( Next );
    end loop;
  end Options;
  
  procedure Proctype         ( A_Proctype_Node : Types.AST.Proctype_Node_Ptr         ) is
    A_Node                  : Types.AST.Node_Ptr;
  begin

    Logger.Info( "" );
    Logger.Info( "Walking a Proctype..." );
    -- COMMENTARY: Not an elegant solution.
    Process_Name := Types.Lexer.Element_In_Table( A_Proctype_Node.Name );
    if Process_Declarations_First = False then
      Begin_New_Process;
    end if;

    Logger.Info( "SCOPE: Getting '" & Types.Lexer.Bound_String.To_String( Process_Name ) & "' from the current scope" );
    A_Node := Scope_Get( Process_Name );
    Code_Generation_Assert( A_Node /= null,
                            "Missing proctype '"                                &
                            Types.Lexer.Bound_String.To_String( Process_Name )  &
                            "' in this scope!"                                  );

    Scope_Submerge;
    if A_Proctype_Node.Parameters /= null then
      In_Parameter_List := True;
      Decl_Lst( A_Proctype_Node.Parameters );
      In_Parameter_List := False;
    end if;
    Sequence( A_Proctype_Node.Sequence );
    Scope_Surface;

    if Process_Declarations_First = False then
      Write_End_Transition( A_Proctype_Node.Sequence );
      End_New_Process( A_Proctype_Node.Active );
    end if;
    Process_Name := Types.Lexer.Bound_String.Null_Bounded_String;

  end Proctype;

  procedure EmitBinaryOp( Operator : Types.Lexer.Token_Type ) is
    Code : Types.CodeGen.Byte_Code;
  begin
    case Operator is
      when Types.Lexer.Token_Add          =>
        Code.Operator := Compile_Global.iadd;
      when Types.Lexer.Token_And_Bitwise  =>
        Code.Operator := Compile_Global.iand;
      when Types.Lexer.Token_And_Logical  =>
        Code.Operator := Compile_Global.logic_and;
      when Types.Lexer.Token_Divide       =>
        Code.Operator := Compile_Global.idiv;
      when Types.Lexer.Token_Equals       =>
        Code.Operator := Compile_Global.icmpeq;
      when Types.Lexer.Token_Greater_Than =>
        Code.Operator := Compile_Global.icmpgt;
      when Types.Lexer.Token_Greater_Than_Or_Equal_To
                                          =>
        Code.Operator := Compile_Global.icmpge;
      when Types.Lexer.Token_Left_Shift   =>
        Code.Operator := Compile_Global.ishl;
      when Types.Lexer.Token_Less_Than    =>
        Code.Operator := Compile_Global.icmplt;
      when Types.Lexer.Token_Less_Than_Or_Equal_To
                                          =>
        Code.Operator := Compile_Global.icmple;
      when Types.Lexer.Token_Modulus      =>
        Code.Operator := Compile_Global.irem;
      when Types.Lexer.Token_Multiply     =>
        Code.Operator := Compile_Global.imul;
      when Types.Lexer.Token_Not_Equals   =>
        Code.Operator := Compile_Global.icmpne;
      when Types.Lexer.Token_Or_Bitwise   =>
        Code.Operator := Compile_Global.ior;
      when Types.Lexer.Token_Or_Logical   =>
        Code.Operator := Compile_Global.logic_or;
      when Types.Lexer.Token_Right_Shift  =>
        -- NOTE: ARITHMETIC right shift, as opposed to iushr
        Code.Operator := Compile_Global.ishr;
      when Types.Lexer.Token_Subtract     =>
        Code.Operator := Compile_Global.isub;
      when Types.Lexer.Token_Xor_Bitwise  =>
        Code.Operator := Compile_Global.ixor;
      when others =>
        Logger.Error( "Found invalid operator '" & Types.Lexer.Token_Type'Image( Operator ) & "'!" );
        raise Invalid_Operator;
    end case;
    Emit( Code );
  end EmitBinaryOp;

  procedure EmitUnaryOp( Operator : Types.Lexer.Token_Type ) is
    Code : Types.CodeGen.Byte_Code;
  begin
    case Operator is
      when Types.Lexer.Token_Ones_Complement  =>
        Code.Operator := Compile_Global.inot;
      when Types.Lexer.Token_Not_Or_Send_1    =>
        Code.Operator := Compile_Global.logic_not;
      when Types.Lexer.Token_Subtract         =>
        Code.Operator := Compile_Global.ineg;
      when others =>
        Logger.Error( "Found invalid operator '" & Types.Lexer.Token_Type'Image( Operator ) & "'!" );
        raise Invalid_Operator;
    end case;
    Emit( Code );
  end EmitUnaryOp;

  procedure Poll( A_Poll_Node : Types.AST.Poll_Node_Ptr ) is
    Code  : Types.CodeGen.Byte_Code;
  begin

    Logger.Info( "Walking a Poll" );

    -- first, generate code for receive arguments.
    -- it is important that we do this now because the Erigone
    -- interpreter cares about the last instruction in this statement.
    -- as for the tricky store-into-a-variable receive argument,
    -- we solve the problem by using the load_address instruction; 
    -- see Load_Address.
    -- The interpreter must, of course, be careful in how it handles receive arguments.
    Recv_Args( A_Poll_Node.Arguments );

    case A_Poll_Node.Tag is
      when  Types.AST.Node_Fifo_Poll    =>
        Code.Operator := Compile_Global.fifo_poll;
      when  Types.AST.Node_Random_Poll  =>
        Code.Operator := Compile_Global.random_poll;
      when others                       =>
        AST.Tools.Semantic_Assert( False, "Unrecognized A_Poll_Node!" );
    end case;

    -- next, load the channel variable onto the stack
    Load_Varref( A_Poll_Node.Target );
    -- number of arguments
    Code.Operand_1 := Types.CodeGen.Operand( A_Poll_Node.Arguments.Arguments.Length );
    -- finally, emit code for poll instruction
    Emit( Code );

  end Poll;
  
  procedure Any_Expr ( An_Any_Expr_Node : Types.AST.Any_Expr_Node_Ptr ) is
    A_Higher_Precedence_Implies_Any_Expr_Node
                                        : Types.AST.Higher_Precedence_Implies_Any_Expr_Node_Ptr;
    An_Unary_Any_Expr_Node              : Types.AST.Unary_Any_Expr_Node_Ptr;
    A_Channel_Poll_Any_Expr_Node        : Types.AST.Channel_Poll_Any_Expr_Node_Ptr;
    A_Const_Any_Expr_Node               : Types.AST.Const_Any_Expr_Node_Ptr;
    A_Len_Any_Expr_Node                 : Types.AST.Len_Any_Expr_Node_Ptr;
    A_Poll_Any_Expr_Node                : Types.AST.Poll_Any_Expr_Node_Ptr;
    A_Varref_Any_Expr_Node              : Types.AST.Varref_Any_Expr_Node_Ptr;
    A_Binary_Any_Expr_Node              : Types.AST.Binary_Any_Expr_Node_Ptr;
    Code                                : Types.CodeGen.Byte_Code;
  begin
    Logger.Info( "Walking an Any_Expr..." );

    case An_Any_Expr_Node.Tag is

      when Types.AST.Node_Binary_Any_Expr =>
        A_Binary_Any_Expr_Node := Types.AST.Binary_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Code_Generation_Assert( A_Binary_Any_Expr_Node.Left_Leaf /= null xor A_Binary_Any_Expr_Node.Left_Expression /= null,
                               "A_Binary_Any_Expr_Node has both a left leaf and a left expression!" );
        Code_Generation_Assert( A_Binary_Any_Expr_Node.Right_Leaf /= null xor A_Binary_Any_Expr_Node.Right_Expression /= null, 
                               "A_Binary_Any_Expr_Node has both a right leaf and a right expression!" );
        if A_Binary_Any_Expr_Node.Left_Leaf /= null then
          Any_Expr( A_Binary_Any_Expr_Node.Left_Leaf );
        else
          Any_Expr( Types.AST.Any_Expr_Node_Ptr( A_Binary_Any_Expr_Node.Left_Expression ) );
        end if;
        if A_Binary_Any_Expr_Node.Right_Leaf /= null then
          Any_Expr( A_Binary_Any_Expr_Node.Right_Leaf );
        else
          Any_Expr( Types.AST.Any_Expr_Node_Ptr( A_Binary_Any_Expr_Node.Right_Expression ) );
        end if;
        EmitBinaryOp( A_Binary_Any_Expr_Node.Operator );

      when Types.AST.Node_Const_Any_Expr  =>
        A_Const_Any_Expr_Node := Types.AST.Const_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        -- Meaningless check given Promela semantics, really,
        -- but may be good to keep it around for stricter type checking in future
        if A_Const_Any_Expr_Node.A_Constant.The_Type = Types.AST.Int_Data_Type then
          Const( A_Const_Any_Expr_Node.A_Constant );
        end if;

      when Types.AST.Node_Empty_Channel_Poll_Any_Expr =>
        Logger.Info( "Walking a Empty_Channel_Poll_Any_Expr_Node..." );
        A_Channel_Poll_Any_Expr_Node  := Types.AST.Channel_Poll_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Code.Operator                 := Compile_Global.channel_empty;
        -- next, load the channel variable onto the stack
        Load_Varref( A_Channel_Poll_Any_Expr_Node.Channel_Name );
        -- finally, emit code for send instruction
        Emit( Code );

      when Types.AST.Node_Full_Channel_Poll_Any_Expr =>
        Logger.Info( "Walking a Full_Channel_Poll_Any_Expr_Node..." );
        A_Channel_Poll_Any_Expr_Node  := Types.AST.Channel_Poll_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Code.Operator                 := Compile_Global.channel_full;
        -- next, load the channel variable onto the stack
        Load_Varref( A_Channel_Poll_Any_Expr_Node.Channel_Name );
        -- finally, emit code for send instruction
        Emit( Code );

      when Types.AST.Node_Higher_Precedence_Implies_Any_Expr  =>
        -- TODO: support tertiary/implies condition
        A_Higher_Precedence_Implies_Any_Expr_Node :=
          Types.AST.Higher_Precedence_Implies_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Any_Expr( A_Higher_Precedence_Implies_Any_Expr_Node.Generic_Any_Expr );

      when Types.AST.Node_Len_Any_Expr =>
        Logger.Info( "Walking a Len_Any_Expr_Node..." );
        A_Len_Any_Expr_Node := Types.AST.Len_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Code.Operator       := Compile_Global.channel_len;
        -- next, load the channel variable onto the stack
        Load_Varref( A_Len_Any_Expr_Node.Channel_Name );
        -- finally, emit code for send instruction
        Emit( Code );

      when Types.AST.Node_Nempty_Channel_Poll_Any_Expr =>
        Logger.Info( "Walking a Nempty_Channel_Poll_Any_Expr_Node..." );
        A_Channel_Poll_Any_Expr_Node  := Types.AST.Channel_Poll_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Code.Operator                 := Compile_Global.channel_nempty;
        -- next, load the channel variable onto the stack
        Load_Varref( A_Channel_Poll_Any_Expr_Node.Channel_Name );
        -- finally, emit code for send instruction
        Emit( Code );

      when Types.AST.Node_Nfull_Channel_Poll_Any_Expr =>
        Logger.Info( "Walking a Nfull_Channel_Poll_Any_Expr_Node..." );
        A_Channel_Poll_Any_Expr_Node  := Types.AST.Channel_Poll_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Code.Operator                 := Compile_Global.channel_nfull;
        -- next, load the channel variable onto the stack
        Load_Varref( A_Channel_Poll_Any_Expr_Node.Channel_Name );
        -- finally, emit code for send instruction
        Emit( Code );

      when Types.AST.Node_Poll_Any_Expr =>
        Logger.Info( "Walking a Poll_Any_Expr_Node..." );
        A_Poll_Any_Expr_Node := Types.AST.Poll_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Poll( A_Poll_Any_Expr_Node.Poll );

      when Types.AST.Node_Unary_Any_Expr  =>
        An_Unary_Any_Expr_Node := Types.AST.Unary_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Any_Expr( An_Unary_Any_Expr_Node.Generic_Any_Expr );
        EmitUnaryOp( An_Unary_Any_Expr_Node.Operator );

      when Types.AST.Node_Varref_Any_Expr =>
        A_Varref_Any_Expr_Node := Types.AST.Varref_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Load_Varref( A_Varref_Any_Expr_Node.Variable );

      when others =>
        Raise_Unsupported_Construct( "other expressions" );

    end case;

  end Any_Expr;
 
  -- This procedure could probably be refactored to share common code.
  -- It is just mentally easier to separate them right now.
  procedure Load_Varref      ( A_Varref_Node   : Types.AST.Varref_Node_Ptr           ) is
    An_Ivar_Node          : Types.AST.Ivar_Node_Ptr;
    Built_In              : Types.AST.Built_In_Data_Type_Ptr;
    An_Array              : Types.AST.Array_Data_Type_Ptr;
    Offset_In_Byte        : Types.CodeGen.Byte;
    A_Mtype_Node          : Types.AST.Mtype_Node_Ptr;
    Name                  : Types.Lexer.Bound_String.Bounded_String;
    Code                  : Types.CodeGen.Byte_Code;
  begin
    Logger.Info( "Loading a Varref..." );

    case A_Varref_Node.Declaration.Tag is

      when  Types.AST.Node_Ivar           |
            Types.AST.Node_Any_Expr_Ivar  |
            Types.AST.Node_Ch_Init_Ivar   =>

        An_Ivar_Node := Types.AST.Ivar_Node_Ptr( A_Varref_Node.Declaration );

        case  An_Ivar_Node.The_Type.Kind is

          when  Types.AST.Built_In_Type =>

            Built_In := Types.AST.Built_In_Data_Type_Ptr( An_Ivar_Node.The_Type );
            if A_Varref_Node.Array_Offset /= null then
              -- useless indexing, simply check that 0 <= index < 1.
              -- the first implementation was to assert index == 0.
              -- this implementation, which uses instructions reserved
              -- for arrays, was requested to distinguish between assert
              -- and other runtime errors.
              -- first, compute the index and push it onto stack
              Any_Expr( A_Varref_Node.Array_Offset );
              -- check 0 <= index < 1
              Code.Operator   := Compile_Global.check_index;
              Code.Operand_1  := 1;
              Emit( Code );
              case Built_In.Built_In_Kind is
                when  Types.AST.Bit       =>
                  Code.Operator := Compile_Global.bit_aload;
                when  Types.AST.Byte      |
                      Types.AST.Chan      |
                      Types.AST.Mtype     =>
                  Code.Operator := Compile_Global.byte_aload;
                when  Types.AST.Short     =>
                  Code.Operator := Compile_Global.short_aload;
                -- NOTE: I am reasonably assuming that pid is represented as an unsigned
                when  Types.AST.Pid       |
                      Types.AST.Unsigned  =>
                  Code.Operator := Compile_Global.unsigned_aload;
                when  Types.AST.Int       =>
                  Code.Operator := Compile_Global.iaload;
              end case;
              -- offset
              Code.Operand_1 := Types.CodeGen.Operand( An_Ivar_Node.Index );
              -- size...but really, except for pedantic correctness, it makes no difference
              -- here because a zero is a zero is a zero
              Offset_In_Byte := Types.CodeGen.Byte( An_Ivar_Node.The_Type.Size_In_Bytes );
              Logger.Info( "Byte Offset = " & Types.CodeGen.Byte'Image( Offset_In_Byte ) );
              Code.Operand_2 := Types.CodeGen.Operand( Offset_In_Byte );
              Emit( Code );

            else

              case Built_In.Built_In_Kind is
                when  Types.AST.Bit       =>
                  Code.Operator := Compile_Global.bit_load;
                when  Types.AST.Byte      |
                      Types.AST.Chan      |
                      Types.AST.Mtype     =>
                  Code.Operator := Compile_Global.byte_load;
                when  Types.AST.Short     =>
                  Code.Operator := Compile_Global.short_load;
                -- NOTE: I am reasonably assuming that a pid is represented as an unsigned
                when  Types.AST.Pid       |
                      Types.AST.Unsigned  =>
                  Code.Operator := Compile_Global.unsigned_load;
                when  Types.AST.Int       =>
                  Code.Operator := Compile_Global.iload;
              end case;
              Code.Operand_1 := Types.CodeGen.Operand( An_Ivar_Node.Index );
              Emit( Code );

            end if;

          when  Types.AST.Array_Type  =>

            An_Array := Types.AST.Array_Data_Type_Ptr( An_Ivar_Node.The_Type );
            if A_Varref_Node.Array_Offset /= null then
              -- one design considered one instruction to do
              -- both index checking and storing/loading
              -- but this design was adopted
              -- compute the index and push it onto stack
              Any_Expr( A_Varref_Node.Array_Offset );
              -- check 0 <= index < length 
              Code.Operator   := Compile_Global.check_index;
              Code.Operand_1  := An_Array.Number_Of_Elements;
              Emit( Code );
              -- because there are 2 instructions for array store/load
              -- the convention is that they are to be read and executed
              -- consecutively; we will not push the index again
              -- (computation may be expensive) so the interpreter
              -- should remember the index with the check_index instruction
            else
              -- the varref[ 0 ] case
              -- push 0 as index
              Code.Operator   := Compile_Global.bipush;
              Code.Operand_1  := 0;
              Emit( Code );
              -- check 0 <= index < length 
              Code.Operator   := Compile_Global.check_index;
              Code.Operand_1  := An_Array.Number_Of_Elements;
              Emit( Code );
            end if;

            Built_In := Types.AST.Built_In_Data_Type_Ptr( An_Array.Base_Type );
            case Built_In.Built_In_Kind is
              when  Types.AST.Bit       =>
                Code.Operator := Compile_Global.bit_aload;
              when  Types.AST.Byte      |
                    Types.AST.Chan      |
                    Types.AST.Mtype     =>
                Code.Operator := Compile_Global.byte_aload;
              when  Types.AST.Short     =>
                Code.Operator := Compile_Global.short_aload;
              -- NOTE: I am reasonably assuming that pid is represented as an unsigned
              when  Types.AST.Pid       |
                    Types.AST.Unsigned  =>
                Code.Operator := Compile_Global.unsigned_aload;
              when  Types.AST.Int       =>
                Code.Operator := Compile_Global.iaload;
            end case;
            -- offset
            Code.Operand_1 := Types.CodeGen.Operand( An_Ivar_Node.Index );
            -- size of array base type
            Offset_In_Byte := Types.CodeGen.Byte( An_Array.Base_Type.Size_In_Bytes );
            Logger.Info( "Byte Offset = " & Types.CodeGen.Byte'Image( Offset_In_Byte ) );
            Code.Operand_2 := Types.CodeGen.Operand( Offset_In_Byte );
            Emit( Code );

          when  Types.AST.User_Type =>
            Raise_Unsupported_Construct( "loading from user-defined types" );

        end case;

      when Types.AST.Node_Mtype =>

        A_Mtype_Node    := Types.AST.Mtype_Node_Ptr( A_Varref_Node.Declaration );
        Name            := Types.Lexer.Element_In_Table( A_Varref_Node.Name );
        Code.Operator   := Compile_Global.bipush;
        Code.Operand_1  := Types.CodeGen.Operand( A_Mtype_Node.Names_Map.Element( Name ) );
        Emit( Code );

      when others =>
        Code_Generation_Assert( False, "Unrecognized variable declaration!" );

    end case;

  end Load_Varref;

  -- This procedure could probably be refactored to share common code.
  -- It is just mentally easier to separate them right now.
  procedure Store_Varref     ( A_Varref_Node   : Types.AST.Varref_Node_Ptr           ) is
    An_Ivar_Node          : Types.AST.Ivar_Node_Ptr;
    Built_In              : Types.AST.Built_In_Data_Type_Ptr;
    An_Array              : Types.AST.Array_Data_Type_Ptr;
    Offset_In_Byte        : Types.CodeGen.Byte;
    Name                  : Types.Lexer.Bound_String.Bounded_String;
    Code                  : Types.CodeGen.Byte_Code;
  begin
    Logger.Info( "Storing into a Varref..." );

    case  A_Varref_Node.Declaration.Tag is

      when  Types.AST.Node_Ivar           |
            Types.AST.Node_Any_Expr_Ivar  =>

        An_Ivar_Node := Types.AST.Ivar_Node_Ptr( A_Varref_Node.Declaration );

        case An_Ivar_Node.The_Type.Kind is

          when Types.AST.Built_In_Type  =>

            Built_In := Types.AST.Built_In_Data_Type_Ptr( An_Ivar_Node.The_Type );
            if A_Varref_Node.Array_Offset /= null then
              -- useless indexing, simply check that 0 <= index < 1.
              -- the first implementation was to assert index == 0.
              -- this implementation, which uses instructions reserved
              -- for arrays, was requested to distinguish between assert
              -- and other runtime errors.
              -- first, compute the index and push it onto stack
              Any_Expr( A_Varref_Node.Array_Offset );
              -- check 0 <= index < 1
              Code.Operator   := Compile_Global.check_index;
              Code.Operand_1  := 1;
              Emit( Code );
              case Built_In.Built_In_Kind is
                when  Types.AST.Bit       =>
                  Code.Operator := Compile_Global.bit_astore;
                when  Types.AST.Byte      |
                      Types.AST.Mtype     =>
                  Code.Operator := Compile_Global.byte_astore;
                when  Types.AST.Short     =>
                  Code.Operator := Compile_Global.short_astore;
                -- NOTE: I am reasonably assuming that pid is represented as an unsigned
                when  Types.AST.Pid       |
                      Types.AST.Unsigned  =>
                  Code.Operator := Compile_Global.unsigned_astore;
                when  Types.AST.Int       =>
                  Code.Operator := Compile_Global.iastore;
                when  Types.AST.Chan      =>
                  -- TODO: chan
                  null;
              end case;
              -- offset
              Code.Operand_1 := Types.CodeGen.Operand( An_Ivar_Node.Index );
              -- size...but really, except for pedantic correctness, it makes no difference
              -- here because a zero is a zero is a zero
              Offset_In_Byte := Types.CodeGen.Byte( An_Ivar_Node.The_Type.Size_In_Bytes );
              Logger.Info( "Byte Offset = " & Types.CodeGen.Byte'Image( Offset_In_Byte ) );
              Code.Operand_2 := Types.CodeGen.Operand( Offset_In_Byte );
              Emit( Code );

            else

              case Built_In.Built_In_Kind is
                when  Types.AST.Bit       =>
                  Code.Operator := Compile_Global.bit_store;
                when  Types.AST.Byte      |
                      Types.AST.Mtype     =>
                  Code.Operator := Compile_Global.byte_store;
                when  Types.AST.Short     =>
                  Code.Operator := Compile_Global.short_store;
                -- NOTE: I am reasonably assuming that pid is represented as an unsigned
                when  Types.AST.Pid       |
                      Types.AST.Unsigned  =>
                  Code.Operator := Compile_Global.unsigned_store;
                when  Types.AST.Int       =>
                  Code.Operator := Compile_Global.istore;
                when  Types.AST.Chan      =>
                  -- TODO: chan
                  null;
              end case;
              Code.Operand_1 := Types.CodeGen.Operand( An_Ivar_Node.Index );
              Emit( Code );

            end if;

          when Types.AST.Array_Type =>

            An_Array := Types.AST.Array_Data_Type_Ptr( An_Ivar_Node.The_Type );
            if A_Varref_Node.Array_Offset /= null then
              -- one design considered one instruction to do
              -- both index checking and storing/loading
              -- but this design was adopted
              -- compute the index and push it onto stack
              Any_Expr( A_Varref_Node.Array_Offset );
              -- check 0 <= index < length 
              Code.Operator   := Compile_Global.check_index;
              Code.Operand_1  := An_Array.Number_Of_Elements;
              Emit( Code );
              -- because there are 2 instructions for array store/load
              -- the convention is that they are to be read and executed
              -- consecutively; we will not push the index again
              -- (computation may be expensive) so the interpreter
              -- should remember the index with the check_index instruction
            else
              -- the varref[ 0 ] case
              -- push 0 as index
              Code.Operator   := Compile_Global.bipush;
              Code.Operand_1  := 0;
              Emit( Code );
              -- check 0 <= index < length 
              Code.Operator   := Compile_Global.check_index;
              Code.Operand_1  := An_Array.Number_Of_Elements;
              Emit( Code );
            end if;

            Built_In := Types.AST.Built_In_Data_Type_Ptr( An_Array.Base_Type );
            case Built_In.Built_In_Kind is
              when  Types.AST.Bit       =>
                Code.Operator := Compile_Global.bit_astore;
              when  Types.AST.Byte      |
                    Types.AST.Mtype     =>
                Code.Operator := Compile_Global.byte_astore;
              when  Types.AST.Short     =>
                Code.Operator := Compile_Global.short_astore;
              -- NOTE: I am reasonably assuming that pid is represented as an unsigned
              when  Types.AST.Pid       |
                    Types.AST.Unsigned  =>
                Code.Operator := Compile_Global.unsigned_astore;
              when  Types.AST.Int       =>
                Code.Operator := Compile_Global.iastore;
              when  Types.AST.Chan      =>
                -- TODO: chan
                null;
            end case;
            -- offset
            Code.Operand_1 := Types.CodeGen.Operand( An_Ivar_Node.Index );
            -- size of array base type
            Offset_In_Byte := Types.CodeGen.Byte( An_Array.Base_Type.Size_In_Bytes );
            Logger.Info( "Byte Offset = " & Types.CodeGen.Byte'Image( Offset_In_Byte ) );
            Code.Operand_2 := Types.CodeGen.Operand( Offset_In_Byte );
            Emit( Code );

          when  Types.AST.User_Type =>
            Raise_Unsupported_Construct( "storing into user-defined types" );

        end case;

      when Types.AST.Node_Mtype =>
        Code_Generation_Assert( False, "Cannot store into mtype constants!" );

      when  others  =>
        Code_Generation_Assert( False, "Unrecognized variable declaration!" );

    end case;

  end Store_Varref;

  procedure Store_Ivar       ( An_Ivar_Node    : Types.AST.Ivar_Node_Ptr             ) is
    Built_In                  : Types.AST.Built_In_Data_Type_Ptr;
    An_Array                  : Types.AST.Array_Data_Type_Ptr;
    Code                      : Types.CodeGen.Byte_Code;
    Array_Offset              : Types.CodeGen.Operand := 0;
    Number_Of_Elements        : Natural;
    Backtrack_Channel_Handle  : Types.CodeGen.Byte := 0;
  begin

    Logger.Info( "Storing into an Ivar..." );
    case An_Ivar_Node.The_Type.Kind is
      when  Types.AST.Built_In_Type |
            Types.AST.Array_Type    =>

        if An_Ivar_Node.The_Type.Kind = Types.AST.Built_In_Type then
          Built_In := Types.AST.Built_In_Data_Type_Ptr( An_Ivar_Node.The_Type );
        else
          An_Array := Types.AST.Array_Data_Type_Ptr( An_Ivar_Node.The_Type );
          Code_Generation_Assert( An_Array.Base_Type.Kind = Types.AST.Built_In_Type, "Array base type is not a built-in type!" );
          Built_In := Types.AST.Built_In_Data_Type_Ptr( An_Array.Base_Type );
        end if;

        case Built_In.Built_In_Kind is
          when  Types.AST.Bit       =>
            Code.Operator := Compile_Global.bit_store;
          when  Types.AST.Byte      |
                Types.AST.Mtype     =>
            Code.Operator := Compile_Global.byte_store;
          when  Types.AST.Short     =>
            Code.Operator := Compile_Global.short_store;
          -- NOTE: I am reasonably assuming that pid is represented as an unsigned
          when  Types.AST.Pid       |
                Types.AST.Unsigned  =>
            Code.Operator := Compile_Global.unsigned_store;
          when  Types.AST.Int       =>
            Code.Operator := Compile_Global.istore;
          -- NOTE: Special processing for channels
          when  Types.AST.Chan      =>
            case An_Ivar_Node.Tag is

            when  Types.AST.Node_Ch_Init_Ivar   =>
              if An_Array = null then
                Number_Of_Elements := 0;
              else
                Number_Of_Elements := An_Array.Number_Of_Elements - 1;
              end if;
              Backtrack_Channel_Handle := Channel_Handle;
              for I in reverse Natural range 0 .. Number_Of_Elements loop
                Code.Operator   := Compile_Global.bipush;
                Code.Operand_1  := Types.CodeGen.Operand( Backtrack_Channel_Handle );
                Emit( Code );
                Code.Operator   := Compile_Global.byte_store;
                if An_Array /= null then
                  Array_Offset  := An_Array.Base_Type.Size_In_Bytes * I;
                end if;
                Code.Operand_1  := Types.CodeGen.Operand( An_Ivar_Node.Index ) + Array_Offset;
                Emit( Code );
                Backtrack_Channel_Handle := Backtrack_Channel_Handle - 1;
              end loop;
              -- Get out in order to avoid doing the wrong thing!
              return;

            when  Types.AST.Node_Any_Expr_Ivar  =>
              Code_Generation_Assert( False, "Channel variables cannot be initialized with an expression!" );

            when  Types.AST.Node_Ivar           =>
              -- zero it out in Ivar
              Code.Operator := Compile_Global.byte_store;

            when  others                        =>
              Code_Generation_Assert( False, "Unrecognized An_Ivar_Node!" );

            end case;
        end case;

        Code.Operand_1 := Types.CodeGen.Operand( An_Ivar_Node.Index );

      when Types.AST.User_Type  =>
        Raise_Unsupported_Construct( "storing into user-defined types" );

    end case;

    Emit( Code );

  end Store_Ivar;

  
end CodeGen;
