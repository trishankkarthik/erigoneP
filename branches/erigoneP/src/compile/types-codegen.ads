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
-- TYPES-CODEGEN.ADS: The child types package for the code generator package.
--
-- TODO:
-- 1. We don't check whether the number of variables outnumber
-- the maximum range of Symbol_Table_Index, although you would
-- probably get a runtime constraint error.
-- 2. A type for state numbers. In general, more accommodation
-- for Byte (e.g. state numbers, line numbers, etc.).

with Ada.Text_IO;

with Types.Lexer;

-- dependency on model checker
with Compile_Global;

use type Types.Lexer.Bounded_String_Hashed_Set.Cursor;
use type Types.Lexer.Natural_Hashed_Set.Cursor;

package Types.CodeGen is

  -- An enumeration of the complete set of instructions
  -- or opcodes for the SDS virtual machine. See "The Pomegranate
  -- Virtual Machine Specification." (promela_spec.pdf)
  -- Originally defined here, it has now been moved to the model checker
  -- due to a design decision.
  subtype Opcode is Compile_Global.Opcode;

  -- The type of a byte code operand; also the
  -- Index_Type of String_Vector, Natural_Vector
  subtype Operand is Natural;
  -- A byte code record
  type Byte_Code is
    record
      Operator      : Opcode  := Compile_Global.noop;
      Operand_1     : Operand := 0;
      Operand_2     : Operand := 0;
    end record;
  
  -- START: SDS data structures

  -- Byte is used for all indices and values of variables.
  -- I am defining it as a range instead of a mod for safety
  -- because that is all I need.
  type    Byte       is range 0 .. 255;
  -- None used as null value
  None:   constant   Byte := Byte'Last;
  
  -- Process/variable names and source statements
  subtype Name           is String( 1..32 );
  Blanks: constant Name  := (others => ' ');
 
  -- Symbol table data structures
  type Symbol_Type is
  (
    Bit_Type,
    Byte_Type,
    Proc_Type,
    Label_Type
  );
  type Scope_Type is 
  (
    Global_Scope,
    Local_Scope,
    Never_Scope
  );
  -- Number of byte codes per statement
  subtype Byte_Code_Index is Byte range 0..31;
  
  -- STOP: SDS data structures

  -- The type of the index into the SDS symbol table;
  -- used for referring to variables; we obtain the type
  -- directly from SDS
  subtype Symbol_Table_Index is Byte;
  -- The range of an integer in Promela, its widest primitive type
  type Promela_Integer is range ( -(2**31) - 1 ) .. ( (2**31) - 1 );
  -- Statement state number type
  subtype State_Number is Natural;
  
  -- A list of byte codes
  package Byte_Code_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Byte_Code
  );

  -- START: SDS data structures

  -- Transitions
  type Transitions is
    record
      Statement:    Name;
      Source:       Byte;
      Target:       Byte;
      Atomic:       Byte;
      End_Label:    Byte;
      Accept_Label: Byte;
      Line_Number:  Byte;
      Code_Size:    Byte;
      Byte_Code:    Byte_Code_List.List;
    end record;
  
  -- STOP: SDS data structures

  -- TK: A doubly linked list of Transitions
  package Transitions_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Transitions
  );
  
  -- TK: A doubly linked list of Transitions_List cursors
  use type Transitions_List.Cursor;
  package Transitions_List_Cursors is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Transitions_List.Cursor
  );

  -- TK: A map from Global.Byte to Transitions_List_Cursors
  use type Transitions_List_Cursors.List;
  function Byte_Hash( N : Byte ) return Ada.Containers.Hash_Type;
  package Source_To_Transitions is new Ada.Containers.Hashed_Maps(
    Key_Type        =>  Byte,
    Element_Type    =>  Transitions_List_Cursors.List,
    Hash            =>  Byte_Hash,
    Equivalent_Keys =>  "="
  );
 
  -- Vectors for Promela strings and naturals. What are we trying to do here?
  -- We are mapping an index from a byte code operand to a cursor,
  -- which in turn indexes to a value in a table in Types.Lexer.
  -- e.g. Byte_Code.Operand_1 -> Types.Lexer.Natural_Hashed_Set.Cursor
  -- TODO: I cannot think of a cleverer way of mapping indices to cursors right now.
  -- A compile-time only O(n) constraint, to be sure, but smarter will be better.
  package String_Vector is new Ada.Containers.Vectors
  (
    Index_Type    => Operand,
    Element_Type  => Types.Lexer.Bounded_String_Hashed_Set.Cursor
  );
  package Natural_Vector is new Ada.Containers.Vectors
  (
    Index_Type    => Operand,
    Element_Type  => Types.Lexer.Natural_Hashed_Set.Cursor
  );

  -- Add a Types.Lexer.Bounded_String_Hashed_Set.Cursor, if it is not already present,
  -- and get an index to it
  function  Add_To_String_Table   ( Format  : Types.Lexer.Bounded_String_Hashed_Set.Cursor  ;
                                    Automaton_File  : Ada.Text_IO.File_Type                 )
            return Operand;
  -- Get the corresponding Types.Lexer.Bound_String.Bounded_String of an index
  function  Get_From_String_Table ( Index   : Operand                                       )
            return Types.Lexer.Bound_String.Bounded_String;
  -- Get String_Table length
  function  Get_String_Table_Length
            return Integer;
  -- Add a Types.Lexer.Natural_Hashed_Set.Cursor, if it is not already present,
  -- and get an index to it
  function  Add_To_Natural_Table  ( Number  : Types.Lexer.Natural_Hashed_Set.Cursor         ;
                                    Automaton_File  : Ada.Text_IO.File_Type                 )
            return Operand;
  -- Get the corresponding Types.Lexer.Promela_Natural of an index
  function  Get_From_Natural_Table( Index   : Operand                                       )
            return Types.Lexer.Promela_Natural;
  -- Get Natural_Table length
  function  Get_Natural_Table_Length
            return Integer;

  private

    -- the Strings Table for the interpreter, mapping indices to cursors
    String_Table  : String_Vector.Vector;
    -- the Naturals Table for the interpreter, mappping indices to cursors
    Natural_Table : Natural_Vector.Vector;

end Types.CodeGen;
