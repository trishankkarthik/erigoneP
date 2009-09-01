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
package body Types.Lexer is

   function Natural_Hash (N : Promela_Natural) return Ada.Containers.Hash_Type is
      A : constant := Promela_Natural'Last + 1;
   begin
      -- is this good enough?
      return Ada.Containers.Hash_Type'Mod(N);
      -- TODO: is this really knuth's multiplicative method?
      --return Ada.Containers.Hash_Type (N) * A;
   end Natural_Hash;
   
   function Element_In_Table
     (a_cursor : Types.Lexer.Bounded_String_Hashed_Set.Cursor)
      return     Types.Lexer.Bound_String.Bounded_String
   is
   begin
      return Types.Lexer.Bounded_String_Hashed_Set.Element (a_cursor);
   end Element_In_Table;

   function Element_In_Table
     (a_cursor : Types.Lexer.Natural_Hashed_Set.Cursor)
      return     Types.Lexer.Promela_Natural
   is
   begin
      return Types.Lexer.Natural_Hashed_Set.Element (a_cursor);
   end Element_In_Table;
   
   function Add_To_Name_Table( lexeme : Types.Lexer.Bound_String.Bounded_String )
      return     Types.Lexer.Bounded_String_Hashed_Set.Cursor is
      hashed_set_table_cursor : Types.Lexer.Bounded_String_Hashed_Set.Cursor;
      Inserted                : Boolean;
   begin
      Name_Table.Insert
      (lexeme,
       hashed_set_table_cursor,
       inserted);
      return hashed_set_table_cursor;
   end Add_To_Name_Table;
   
   function Add_To_String_Table( lexeme : Types.Lexer.Bound_String.Bounded_String )
      return     Types.Lexer.Bounded_String_Hashed_Set.Cursor is
      hashed_set_table_cursor : Types.Lexer.Bounded_String_Hashed_Set.Cursor;
      Inserted                : Boolean;
   begin
      String_Table.Insert
      (lexeme,
       hashed_set_table_cursor,
       inserted);
      return hashed_set_table_cursor;
   end Add_To_String_Table;
   
   function Add_To_Natural_Table( N : Types.Lexer.Promela_Natural )
   return   Types.Lexer.Natural_Hashed_Set.Cursor is
      Natural_Cursor        : Types.Lexer.Natural_Hashed_Set.Cursor;
      Inserted              : Boolean;
   begin
     Number_Table.Insert
        (N,
         natural_cursor,
         inserted);
      return Natural_Cursor;
   end Add_To_Natural_Table;

end Types.Lexer;
