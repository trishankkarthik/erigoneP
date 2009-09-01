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
-- AST.ADS: The semantic analyzer package specification.
-- The semantic analyzer is reponsible for performing some compile-time
-- checks on the abstract syntax tree of the input and report an error
-- if any is found. It also decorates the AST with additional data
-- for such purposes as type checking and code generation; this is
-- one of many things inspired by GNAT.

with Types.AST;

package AST is

  -- For LTL formulae translation
  procedure Analyze_Any_Expr  ( An_Any_Expr_Node: in out Types.AST.Any_Expr_Node_Ptr );

  -- One needs to only call this procedure, passing in the root
  -- node of the AST, to perform semantic analysis and tree decoration
  procedure Spec             ( A_Spec_Node     : Types.AST.Spec_Node_Ptr             );

end AST;
