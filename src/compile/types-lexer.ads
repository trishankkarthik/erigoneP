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
--
-- TYPES-LEXER.ADS: The child types package for the lexer package.

with Ada.Strings.Bounded;
with Ada.Strings.Bounded.Hash;

package Types.Lexer is

  -- A complete list of tokens for a Promela lexer
  type Token_Type is (
    Token_Name,
    Token_Number,
    Token_String,

  -- Operators, delimiters
    Token_Left_Curly_Bracket,       -- {
    Token_Right_Curly_Bracket,      -- }
    Token_Left_Square_Bracket,      -- [
    Token_Right_Square_Bracket,     -- ]
    Token_Left_Parentheses,         -- (
    Token_Right_Parentheses,        -- )
    Token_Semicolon,                -- ;
    Token_Comma,                    -- ,
    Token_Dot,                      -- .
    Token_Alias,                    -- @
    Token_Ones_Complement,          -- ~
    Token_Guard,                    -- ::
    Token_Colon,                    -- :
    Token_Implies,                  -- ->
    Token_Assignment,               -- =
    Token_Decrement,                -- --
    Token_Increment,                -- ++
    Token_Not_Or_Send_1,            -- !
    Token_Receive_1,                -- ?
    Token_Receive_2,                -- ??
    Token_Send_2,                   -- !!
    -- Aligned with the planets to accomodate easy operator
    -- precedence indexing; see Operator_Index type in ast.adb
    Token_Or_Logical,               -- ||
    Token_And_Logical,              -- &&
    Token_Or_Bitwise,               -- |
    Token_Xor_Bitwise,              -- ^
    Token_And_Bitwise,              -- &
    Token_Equals,                   -- ==
    Token_Not_Equals,               -- !=
    Token_Less_Than,                -- <
    Token_Less_Than_Or_Equal_To,    -- <=
    Token_Greater_Than,             -- >
    Token_Greater_Than_Or_Equal_To, -- >=
    Token_Left_Shift,               -- <<
    Token_Right_Shift,              -- >>
    Token_Add,                      -- +
    Token_Subtract,                 -- -
    Token_Divide,                   -- /
    Token_Multiply,                 -- *
    Token_Modulus,                  -- %

  -- Reserved keywords
    Token_Active,
    Token_Assert,
    Token_Atomic,
    -- Built in types
    Token_Bit,
    Token_Bool,
    Token_Byte,
    Token_Chan,
    Token_Mtype,
    Token_Short,
    Token_Int,
    Token_Pid,
    Token_Unsigned,
    -- Built in types
    Token_Break,
    Token_C_Code,
    Token_C_Decl,
    Token_C_Expr,
    Token_C_State,
    Token_C_Track,
    Token_D_Proctype,
    Token_D_Step,
    Token_Do,
    Token_Else,
    Token_Empty,
    Token_Enabled,
    Token_Eval,
    Token_False,
    Token_Fi,
    Token_Full,
    Token_Goto,
    Token_Hidden,
    Token_If,
    Token_Init,
    Token_Len,
    Token_Local,
    Token_Nempty,
    Token_Never,
    Token_Nfull,
    Token_Notrace,
    Token_Np_Underscore,
    Token_Od,
    Token_Of,
    Token_Pc_Value,
    Token_Print,
    Token_Printf,
    Token_Printm,
    Token_Priority,
    Token_Proctype,
    Token_Provided,
    Token_Run,
    Token_Show,
    Token_Skip,
    Token_Timeout,
    Token_Trace,
    Token_True,
    Token_Typedef,
    Token_Unless,
    Token_Xr,
    Token_Xs,

  -- NOTE: the default State, users should watch out for this
    Token_Null,
  -- this is solely to support printing new lines, outside of comments anyway
    Token_Newline,
  -- a signal to stop scanning
    Token_EOF);

  type Promela_Natural is range 0 .. ( 2**31 - 1 );

  -- this is the bounded string package we use everywhere to keep track of names
  -- and strings; performance is not bad, if implementation advice is taken to be
  -- accurate
  package Bound_String is new Ada.Strings.Bounded.Generic_Bounded_Length (
    64);
  use type Bound_String.Bounded_String;
  function Bounded_String_Hash is new Ada.Strings.Bounded.Hash (
    Bounded => Bound_String);
  function Natural_Hash (N : Promela_Natural) return Ada.Containers.Hash_Type;
  package Bounded_String_Hashed_Map is new Ada.Containers.Hashed_Maps (
    Key_Type => Bound_String.Bounded_String,
    Element_Type => Token_Type,
    Hash => Bounded_String_Hash,
    Equivalent_Keys => "=");
  -- needed for /=
  use type Bounded_String_Hashed_Map.Cursor;
  package Bounded_String_Hashed_Set is new Ada.Containers.Hashed_Sets (
    Element_Type => Bound_String.Bounded_String,
    Hash => Bounded_String_Hash,
    Equivalent_Elements => "=");
  package Natural_Hashed_Set is new Ada.Containers.Hashed_Sets (
    Element_Type => Promela_Natural,
    Hash => Natural_Hash,
    Equivalent_Elements => "=");
   
  -- the idea is to use a single all-unifying generic Token
  type Token is record
    -- Everything
    Kind : Token_Type := Token_Null;
    -- Token_Name, Token_String (we multiplex this)
    Lexeme_Cursor : Bounded_String_Hashed_Set.Cursor :=
      Bounded_String_Hashed_Set.No_Element;
    -- Token_Number (TODO: is cursor really the best thing to index?)
    Value_Cursor : Natural_Hashed_Set.Cursor :=
      Natural_Hashed_Set.No_Element;
    -- NOTE: Line number for CodeGen!
    Line_Number : Positive;
  end record;
   
   -- Given a cursor and knowledge of what it is (string or natural),
   -- use these functions to get the corresponding value
   function Element_In_Table
     (a_cursor : Types.Lexer.Bounded_String_Hashed_Set.Cursor)
      return     Types.Lexer.Bound_String.Bounded_String;
   function Element_In_Table
     (a_cursor : Types.Lexer.Natural_Hashed_Set.Cursor)
      return     Types.Lexer.Promela_Natural;
   -- Do not use these functions unless absolutely necessary. They
   -- are used internally by the lexer and the parser in the Const
   -- procedure, where some constants (true, false) are entered as numbers.
   function Add_To_Name_Table( lexeme : Types.Lexer.Bound_String.Bounded_String )
      return     Types.Lexer.Bounded_String_Hashed_Set.Cursor;
   function Add_To_String_Table( lexeme : Types.Lexer.Bound_String.Bounded_String )
      return     Types.Lexer.Bounded_String_Hashed_Set.Cursor;
   function Add_To_Natural_Table( N : Types.Lexer.Promela_Natural )
      return     Types.Lexer.Natural_Hashed_Set.Cursor;

   private

    -- keep track of names
    Name_Table     : Types.Lexer.Bounded_String_Hashed_Set.Set;
    -- keep track of strings
    String_Table   : Types.Lexer.Bounded_String_Hashed_Set.Set;
    -- keep track of numbers
    Number_Table   : Types.Lexer.Natural_Hashed_Set.Set;

end Types.Lexer;
