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
-- TYPES-PARSER.ADS: The child types package for the parser package.

with Types.Lexer;

use type Types.Lexer.Token;
use type Types.Lexer.Token_Type;
use type Types.Lexer.Bound_String.Bounded_String;

package Types.Parser is

  -- A procedure to init various global variables (e.g. keyword table). 
  procedure Init_Globals;

  -- A type for holding Types.Lexer.Token-s in a list;
  -- for, say, a buffer
  package Token_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Types.Lexer.Token
  );

  -- A map from Types.Lexer.Token_Type to Types.Lexer.Bound_String.Bounded String
  function  Token_Type_Hash( T : Types.Lexer.Token_Type )
            return Ada.Containers.Hash_Type;
  -- an inverse of Types.Lexer.Bounded_String_Hashed_Map
  package Token_Type_To_Bounded_String_Hashed_Map is new Ada.Containers.Hashed_Maps (
    Key_Type        => Types.Lexer.Token_Type,
    Element_Type    => Types.Lexer.Bound_String.Bounded_String,
    Hash            => Token_Type_Hash,
    Equivalent_Keys => "=");

  -- Return printable String of a Types.Lexer.Token_Type
  function  To_String( T : Types.Lexer.Token_Type )
            return String;

  private
  
    -- a map to translate from Types.Lexer.Token_Type
    -- to a printable Types.Lexer.Bound_String.Bounded_String
    Keyword_Table : Token_Type_To_Bounded_String_Hashed_Map.Map;

end Types.Parser;
