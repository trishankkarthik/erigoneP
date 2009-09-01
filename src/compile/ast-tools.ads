-- Pomegranate: A SPIN-compatible compiler for the SPIN-compatible Erigone model checker. 
-- Copyright (C) 2009 Trishank Karthik.
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
-- AST-TOOLS.ADS: Utilities for semantic analysis.

with Types.AST;
with Types.Lexer;

package AST.Tools is

  -- Raise_Unsupported_Construct uses this to tell the user that we do
  -- not support a construct
  Unsupported_Construct   : exception;
  -- Semantic_Assert uses this to tell the user about any semantic error
  Generic_Semantic_Error  : exception;

  -- Use this to tell the user about an unsupported construct.
  procedure Raise_Unsupported_Construct( Name : String );
  -- Use this to assert a semantic check, culminating in immediate
  -- run-time termination and a message in case of failure.

  procedure Semantic_Assert( Assertion : Boolean ; Message : String );

  -- Why do we have this procedure? Originally, before there was any
  -- code generation, I used to push and pop the scope here. However,
  -- for code maintenance and modularity, code generation is performed
  -- in a separate package (see CodeGen). Consequently, we need a way to
  -- pass scopes between packages. This function returns the "root" scope.
  function  Get_Root_Scope
  return    Types.AST.Scope_Ptr;

  -- For this package to work as intended, this procedure must be called to
  -- initialize built-in datatypes and the operator precedence map.
  procedure Init_Globals;

  -- Link, in the current scope, this name to this node; return True if
  -- there is not another entry in this scope with the same name, False
  -- otherwise.
  function  Scope_Put( Name       : Types.Lexer.Bound_String.Bounded_String      ;
                       Definition : Types.AST.Node_Ptr                           )
  return    Boolean;

  -- Get, from the current scope or its parents, the node that is mapped
  -- to this name; returns null if there is no entry for this name.
  function  Scope_Get( Name       : Types.Lexer.Bound_String.Bounded_String      )
  return    Types.AST.Node_Ptr;

  -- Find, in the parent of all scopes, an entry with this name;
  -- return True if successful, False otherwise.
  function  Is_There_Global( Name : Types.Lexer.Bound_String.Bounded_String      )
  return    Boolean;

  -- Push the current scope unto the tree so that it becomes a parent
  -- for children scopes.
  procedure Scope_Push;

  -- Do not delete the child scope, but restore the current scope to
  -- that of its parent.
  procedure Scope_Pop;

  -- Return the least upper bound for two primitive, non-array types.
  -- It is reserved for future type checking on Promela.
  function  LUB( Type1 : Types.AST.Data_Type_Ptr ; Type2 : Types.AST.Data_Type_Ptr )
  return    Types.AST.Data_Type_Ptr;

  -- Every Types.AST.Any_Expr_Node that comes out of the parser with a binary operator
  -- is not properly structured because the grammar is designed for easier parsing
  -- rather than easier AST construction. Thus, we use this recursive function
  -- to restructure the subtree to more properly reflect a binary expression.
  function  Binary_Tree_Restructure( An_Any_Expr_Node : Types.AST.Any_Expr_Node_Ptr )
  return    Types.AST.Binary_Any_Expr_Node_Ptr;

  -- After we have restructured subtrees to resemble binary expressions, we then
  -- correct for operator precedence recursively.
  procedure Binary_Tree_Precedence_Correction(  A_Binary_Any_Expr_Node  : in out Types.AST.Binary_Any_Expr_Node_Ptr ;
                                                Binary_Tree_Changed     : in out Boolean                            );

  function  Is_A_Channel_Variable( A_Varref_Node : Types.AST.Varref_Node_Ptr )
  return    Boolean;

end AST.Tools;
