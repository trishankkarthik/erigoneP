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
-- TYPES.ADS: The Types package is a collection of child packages,
-- each of which is a package of types, procedures and
-- functions that serve a major package of the Pomegranate
-- compiler. Correspondingly, there is a types child
-- package for Lexer (Types.Lexer), Parser (Types.Parser),
-- AST (Types.AST) and CodeGen (Types.CodeGen).
-- As you can see, we make good use of the standard
-- Ada 2005 Containers packages.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

package Types is

  -- A dummy procedure to satisfy compilation.
  procedure Dummy;

end Types;
