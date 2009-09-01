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
-- SCANNER.ADB: A simple program to test the lexer.

with Lexer;                   use type Lexer.Token_Type;
with Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Scanner is
   Lexeme              : Lexer.Bound_String.Bounded_String;
   Value               : Natural;
   Last_Line_Number    : Positive;
   Current_Line_Number : Positive;
begin

   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put ("Usage: scanner <Source_Filename>");
   else
      Lexer.Set_Source_Filename (Ada.Command_Line.Argument (1));

      while Lexer.Current_Token.Kind /= Lexer.Token_EOF loop
         Last_Line_Number := Lexer.Get_Line_Number;
         Lexer.Scan;
         Current_Line_Number := Lexer.Get_Line_Number;
         -- to keep track of lines in comments
         if Current_Line_Number - Last_Line_Number > 1 then
            Ada.Text_IO.New_Line
              (Ada.Text_IO.Count (Current_Line_Number - Last_Line_Number - 1));
         end if;

         if Lexer.Current_Token.Kind = Lexer.Token_Newline then
            Ada.Text_IO.New_Line;
         -- I am not testing for Token_Null here, so that I can see whether
         --the lexer returns spurious Token_Null-s for valid Promela source
         elsif Lexer.Current_Token.Kind /= Lexer.Token_EOF then
            -- a really handy function
            Ada.Text_IO.Put
              (Ada.Characters.Handling.To_Lower
                  (Lexer.Token_Type'Image (Lexer.Current_Token.Kind)));
            if Lexer.Current_Token.Kind = Lexer.Token_Name then
               Ada.Text_IO.Put ("=");
               Lexeme :=
                  Lexer.Element_In_Table (Lexer.Current_Token.Lexeme_Cursor);
               -- Ada.Text_IO.Unbounded_IO.Put does the same thing anyway
               Ada.Text_IO.Put (Lexer.Bound_String.To_String (Lexeme));
            elsif Lexer.Current_Token.Kind = Lexer.Token_Number then
               Ada.Text_IO.Put ("=");
               Value :=
                  Lexer.Element_In_Table (Lexer.Current_Token.Value_Cursor);
               Ada.Integer_Text_IO.Put (Value, 1);
            elsif Lexer.Current_Token.Kind = Lexer.Token_String then
               Ada.Text_IO.Put ("=");
               Lexeme :=
                  Lexer.Element_In_Table (Lexer.Current_Token.Lexeme_Cursor);
               Ada.Text_IO.Put
                 ('"' & Lexer.Bound_String.To_String (Lexeme) & '"');
            end if;
            Ada.Text_IO.Put (", ");
         end if;
      end loop;

   end if;

end Scanner;
