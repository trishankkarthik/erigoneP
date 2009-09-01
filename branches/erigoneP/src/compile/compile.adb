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

with Ada.Exceptions, Ada.Strings.Unbounded, Ada.Characters.Handling;

with Compile_Global;

with Logger, Lexer, Parser, AST, Codegen;
with Types.AST, Types.CodeGen;

package body Compile is

  Root_Node : Types.AST.Spec_Node_Ptr;

  procedure Write_Symbol_Table( Automata_File_Name : String ) is
  begin
    CodeGen.Spec( Root_Node, Process_Declarations_Switch => True, Automaton_Filename => Automata_File_Name );
  exception
    when others =>
      Logger.Close_Log_File;
      Logger.Info( "FAILURE (in writing symbol table)! See CodeGen.log." );
      raise;
  end Write_Symbol_Table;
  
  procedure Write_Automata( Automata_File_Name : String ) is
  begin
    CodeGen.Spec( Root_Node, Process_Declarations_Switch => False, Automaton_Filename => Automata_File_Name );
  exception
    when others =>
      Logger.Close_Log_File;
      Logger.Info( "FAILURE (in writing automata table)! See CodeGen.log." );
      raise;
  end Write_Automata;

  procedure Parse( Filename : in String;  Automata_File_Name: in String; Logs: in Boolean) is
    use Ada.Exceptions;
  begin
    Logger.Logger_Output_Switch := Logs;
    Root_Node := Parser.Parse( Source_Filename => Filename );
    AST.Spec( Root_Node );
    Write_Symbol_Table( Automata_File_Name );
    Write_Automata( Automata_File_Name );
  exception
    when E: others =>
      Logger.Info( "FAILURE (in compilation)! See (Parser|AST|CodeGen).log." );
      Logger.Close_Log_File;
      raise Compile_Global.Compilation_Error
        with Exception_Name(E) & ":" & Exception_Message(E);
  end Parse;

  -- MBA
  function To_Lower(Item: in String) return String
    renames Ada.Characters.Handling.To_Lower;

  -- MBA
  function Byte_Code_To_String(
      Code: in Types.CodeGen.Byte_Code_List.List) return String is
    use Types.CodeGen;
    Size: Byte := Byte(Byte_Code_List.Length(Code));
    B: Byte_Code;
    use Ada.Strings.Unbounded;
    S: Unbounded_String := To_Unbounded_String("{");
    Position: Byte_Code_List.Cursor := Byte_Code_List.First(Code);
  begin
    if Size = 0 then return ","; end if;
    for C in 0 .. Size - 1 loop 
      B := Byte_Code_List.Element(Position);
      S := S &
        (To_Lower(Types.CodeGen.Opcode'Image(B.Operator)) &
        Operand'Image(B.Operand_1) & Operand'Image(B.Operand_2) & ",");
      Position := Byte_Code_List.Next(Position);
    end loop;
    S := S & "},";
    return To_String(S);
  end Byte_Code_To_String;

  function Translate_LTL_Expression( F : String ) return String is
    use Ada.Exceptions;
    T     : Types.AST.Any_Expr_Node_Ptr;
    Code  : Types.CodeGen.Byte_Code_List.List;
  begin
    Lexer.Fill_Buffer_With_String( F );
    T := Parser.Parse_Any_Expr;
    AST.Analyze_Any_Expr( T );
    Code := CodeGen.Generate_Any_Expr( T );
    return Byte_Code_To_String(Code);
  exception
    when E: others =>
      Logger.Close_Log_File;
      Logger.Info( "FAILURE (in generation of expression byte code)! See (Parser|AST|CodeGen).log." );
      raise Compile_Global.Compilation_Error
        with Exception_Name(E) & ":" & Exception_Message(E);
  end Translate_LTL_Expression;

end Compile;
