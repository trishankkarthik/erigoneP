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
-- LEXER.ADS: This is the lexer package specification.
-- Basically, the lexer is a simple deterministic finite state machine.
-- It is also a complete lexer for Promela as of November 2008.
-- It has been extensively documented elsewhere; see
-- "A lexical analyzer in Ada 2005 for Promela of SPIN 5."
-- (promela_lexer.pdf)

with Ada.Containers;             use type Ada.Containers.Hash_Type;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

with Logger;
with Types.Lexer;

package Lexer is

   -- The most recently seen token is detailed here
   Current_Token          : Types.Lexer.Token;
   -- A generic lexical exception
   Generic_Lexical_Error  : exception;

   -- Tell the lexer the source filename
   procedure Set_Source_Filename (filename : String);
   -- Get a token
   procedure Scan;
   -- For LTL formulae translation
   procedure Fill_Buffer_With_String( S : String );
   -- Reset token to default values
   procedure Reset;

private
   -- The reference to the source file
   Source_File    : Ada.Text_IO.File_Type;
   -- The line number at the source the lexer is currently at
   Line_Number    : Positive  := 1;
   -- What is the next character?
   Peek           : Character := ' ';
   -- A flag to control the lexer from reading the next character
   Retract        : Boolean   := False;
   -- for DFA, dragon-style
   State : Positive range 1 .. 20 := 1;
   -- a hashed map to look up keywords and get token types
   Keyword_Table  :Types.Lexer.Bounded_String_Hashed_Map.Map;
   -- number of lines to read in; there probably is a typical distribution of
   --number of lines of source code
   Buffer_Lines   : Positive := 128;
   -- keep track of size of variable buffer
   Buffer_Size    : Positive := 1;
   -- used to keep track of buffer status
   Buffer_Counter : Positive := Buffer_Size + 1;
   -- a buffer to read n characters in 1 system call
   -- instead of making n system calls
   Buffer         : Ada.Strings.Unbounded.Unbounded_String :=
                    Ada.Strings.Unbounded.Null_Unbounded_String;

   -- Tell the lexer what Promela keywords are
   procedure Set_Reserved_Keywords;
   -- The the lexer about global variable names
   procedure Set_Special_Names;
   -- Read the next character off the source
   procedure Read_Character;
   -- Clean up resources
   procedure Cleanup;

end Lexer;
