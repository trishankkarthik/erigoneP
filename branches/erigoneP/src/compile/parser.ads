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
-- PARSER.ADS: The top-down, recursive-descent parser package specification.
-- It is a LL(1) predictive parser, except for a few cases where
-- looking ahead at more than 1 token is necessary for disambiguation.
-- Every function or procedure here, except for the few utility ones,
-- correspond directly to the left-factored, left-recursion-eliminated
-- Promela grammar; see "A left-factored, left-recursion-eliminated version
-- of the Promela grammar for SPIN 5" (promela_grammar.pdf).
-- The FIRST and FOLLOW sets were computed by a Python program I wrote
-- in Allan Gottlieb's Compiler Construction class (see computeset.py).
-- It was thought to be complete, until I saw that the official Promela
-- grammar does not specify everything that its manual pages do
-- (e.g. number of bits must be specified for unsigned types);
-- however it is mostly complete except for these undocumented constructs.
-- It should be easy to extend it to be complete.
-- 
-- TODO:
-- 1. A Promela preprocessor (inline, #define, #include,
-- remove comments and unnecessary whitespace)

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

with Lexer;
with Types.AST;     use Types.AST;
with Types.Lexer;   use Types.Lexer;
with Types.Parser;  use Types.Parser;

package Parser is

  -- A user need only call this function, passing in the filename of the
  -- input source file, which returns the root node of a corresponding
  -- abstract syntax tree given a successful parse.
  function Parse(Source_Filename : String)  return Types.AST.Spec_Node_Ptr;
  -- For LTL formulae translation
  function Parse_Any_Expr                   return Types.AST.Any_Expr_Node_Ptr;

  -- The lexer should never return Types.Lexer.Token_Null in a valid source file.
  Found_Token_Null    : exception;
  -- Expected one particular token according to the grammar, found another instead.
  Mismatched_Token    : exception;
  -- A list of self-describing exceptions.
  Syntax_Error        : exception;
  Unsupported_Syntax  : exception;

  private

    -- The next token in the stream, similar to the Peek character in the lexer.
    Look_Ahead        : Types.Lexer.Token;
    -- A buffer to hold tokens for cases of disambiguation, where we read ahead
    Buffer            : Types.Parser.Token_List.List;
    -- String to accumulate tokens for Stmt-s
    Statement_Text    : Ada.Strings.Unbounded.Unbounded_String :=
                          Ada.Strings.Unbounded.Null_Unbounded_String;
    -- Switch to accumulate tokens for Stmt-s
    Accumulate_Tokens : Boolean := False;

    -- Ensure that the next token, Look_Ahead, matches the expected Token type
    procedure Match(Terminal        : Types.Lexer.Token_Type);
    -- Get the next token, from the lexer by default or the buffer if it is not
    -- empty, and put it in Look_Ahead
    procedure Get_Token;
    -- A procedure to prepend a token in the buffer
    procedure Prepend_Token(A_Token : Types.Lexer.Token);
    -- Output an error message and raise Syntax_Error
    procedure Syntax_Error_Output(S : String);

    -- Every function or procedure henceforth reflects a production in the
    -- our Promela grammar. Every function or procedure parses the input
    -- by expecting certain tokens according to the FIRST and FOLLOW sets for
    -- the corresponding production while building an abstract syntax tree;
    -- see ComputeSet.log for the FIRST and FOLLOW sets for every production.
    -- All functions other than Spec return roots to sub-trees,
    -- whereas Spec returns the root node itself.
    function  Spec              return Types.AST.Spec_Node_Ptr;
    procedure Spec_Prime        ( Root_Node       : Types.AST.Spec_Node_Ptr     );
    function  Module            return Types.AST.Module_Node_Ptr;
    function  Init              return Types.AST.Proctype_Node_Ptr;
    function  Init_Prime        return Types.AST.Priority_Node_Ptr;
    function  Priority          return Types.AST.Priority_Node_Ptr;
    function  Never             return Types.AST.Never_Node_Ptr;
    function  Trace             return Types.AST.Trace_Node_Ptr;
    function  Trace_Prime       return Types.Lexer.Token_Type;
    function  Utype             return Types.AST.Utype_Node_Ptr;
    function  Mtype             return Types.AST.Mtype_Node_Ptr;
    procedure Mtype_Prime1;
    procedure Mtype_Prime2      ( A_Mtype_Node    : Types.AST.Mtype_Node_Ptr    );
    function  Sequence          return Types.AST.Sequence_Node_Ptr;
    procedure Sequence_Prime    ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr );
    procedure Sequence_Prime_Prime
                                ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr );
    function  Step              return Types.AST.Step_Node_Ptr;
    -- COMMENTARY: Since Step_Prime1 is used nowhere else, it does not warrant a node type of its own.
    function  Step_Prime1       return Types.AST.Stmt_Node_Ptr;
    procedure Step_Prime2       ( A_Xr_Xs_Step_Node : Types.AST.Xr_Xs_Step_Node_Ptr );
    function  Decl_Lst          return Types.AST.Decl_Lst_Node_Ptr;
    procedure Decl_Lst_Prime    ( A_Decl_Lst_Node : Types.AST.Decl_Lst_Node_Ptr );
    function  One_Decl          return Types.AST.One_Decl_Node_Ptr;
    function  One_Decl_Prime1   return Types.Lexer.Token_Type;
    procedure One_Decl_Prime2   ( A_One_Decl_Node : Types.AST.One_Decl_Node_Ptr );
    function  Visible           return Types.Lexer.Token_Type;
    function  Typename          return Types.AST.Typename_Node_Ptr;
    function  Ivar              return Types.AST.Ivar_Node_Ptr;
    function  Ivar_Prime1       return Types.AST.Const_Node_Ptr;
    function  Ivar_Prime2       ( Name : Types.Lexer.Bounded_String_Hashed_Set.Cursor ; Array_Length : Types.AST.Const_Node_Ptr )
                                return Types.AST.Ivar_Node_Ptr;
    function  Ivar_Prime3       ( Name : Types.Lexer.Bounded_String_Hashed_Set.Cursor ; Array_Length : Types.AST.Const_Node_Ptr )
                                return Types.AST.Ivar_Node_Ptr;
    function  Ch_Init           return Types.AST.Ch_Init_Node_Ptr;
    procedure Ch_Init_Prime     ( A_Ch_Init_Node  : Types.AST.Ch_Init_Node_Ptr  );
    function  Proctype          return Types.AST.Proctype_Node_Ptr;
    function  Proctype_Prime1   return Types.AST.Active_Node_Ptr;
    function  Proctype_Prime2   return Types.AST.Decl_Lst_Node_Ptr;
    function  Proctype_Prime3   return Types.AST.Priority_Node_Ptr;
    function  Proctype_Prime4   return Types.AST.Enabler_Node_Ptr;
    function  Proctype_Prime5   return Types.Lexer.Token_Type;
    function  Active            return Types.AST.Active_Node_Ptr;
    -- COMMENTARY: Since Active_Prime is used nowhere else, it does not warrant a node type of its own.
    function  Active_Prime      return Types.AST.Const_Node_Ptr;
    function  Enabler           return Types.AST.Enabler_Node_Ptr;
    function  Assign_Prime      ( A_Varref_Node   : Types.AST.Varref_Node_Ptr   )
                                return Types.AST.Assign_Node_Ptr;
    function  Varref            return Types.AST.Varref_Node_Ptr;
    function  Varref_Prime1     return Types.AST.Any_Expr_Node_Ptr;
    function  Varref_Prime2     return Types.AST.Varref_Node_Ptr;
    function  Send_Prime        ( A_Varref_Node   : Types.AST.Varref_Node_Ptr   )
                                return Types.AST.Send_Node_Ptr;
    function  Receive_Prime     ( A_Varref_Node   : Types.AST.Varref_Node_Ptr   )
                                return Types.AST.Recv_Node_Ptr;
    function  Receive_Prime_Prime
                                ( Is_Fifo : Boolean ; A_Varref_Node : Types.AST.Varref_Node_Ptr )
                                return Types.AST.Recv_Node_Ptr;
    function  Poll_Prime        ( A_Varref_Node   : Types.AST.Varref_Node_Ptr   )
                                return Types.AST.Poll_Node_Ptr;
    function  Send_Args         return Types.AST.Send_Args_Node_Ptr;
    function  Send_Args_Prime   ( An_Any_Expr_Node: Types.AST.Any_Expr_Node_Ptr )
                                return Types.AST.Send_Args_Node_Ptr;
    function  Arg_Lst           return Types.AST.Arg_Lst_Node_Ptr;
    procedure Arg_Lst_Prime     ( An_Arg_Lst_Node : Types.AST.Arg_Lst_Node_Ptr  );
    function  Recv_Args         return Types.AST.Recv_Args_Node_Ptr;
    function  Recv_Args_Prime   ( A_Recv_Arg_Node : Types.AST.Recv_Arg_Node_Ptr )
                                return Types.AST.Recv_Args_Node_Ptr;
    function  Recv_Args_Prime_Prime
                                ( A_Recv_Arg_Node : Types.AST.Recv_Arg_Node_Ptr )
                                return Types.AST.Recv_Args_Node_Ptr;
    procedure Recv_Args_Prime_Prime
                                ( A_Recv_Args_Node: Types.AST.Recv_Args_Node_Ptr );
    function  Recv_Arg          return Types.AST.Recv_Arg_Node_Ptr;
    function  Recv_Arg_Prime    return Boolean;
    function  Stmt              return Types.AST.Stmt_Node_Ptr;
    function  Stmt_Prime1       return Types.AST.Arg_Lst_Node_Ptr;
    procedure Stmt_Prime2;
    function  Stmt_Prime3       ( A_Varref_Node   : Types.AST.Varref_Node_Ptr   )
                                return Types.AST.Stmt_Node_Ptr;
    function  Options           return Types.AST.Options_Node_Ptr;
    procedure Options_Prime     ( An_Options_Node : Types.AST.Options_Node_Ptr  );
    function  Chanpoll          return Types.AST.Channel_Poll_Any_Expr_Node_Ptr;
    function  Any_Expr          return Types.AST.Any_Expr_Node_Ptr;
    function  Any_Expr_Prime1   return Types.AST.Arg_Lst_Node_Ptr;
    function  Any_Expr_Prime2   return Types.AST.Priority_Node_Ptr;
    function  Any_Expr_Prime3   return Types.AST.Binar_Op_Node_Ptr;
    function  Any_Expr_Prime4   return Types.AST.Implies_Node_Ptr;
    function  Any_Expr_Prime5   ( A_Varref_Node   : Types.AST.Varref_Node_Ptr   )
                                return Types.AST.Any_Expr_Node_Ptr;
    function  Binarop           return Types.Lexer.Token_Type;
    function  Andor             return Types.Lexer.Token_Type;
    function  Unarop            return Types.Lexer.Token_Type;
    function  Uname             return Types.AST.Uname_Node_Ptr;
    function  Const             return Types.AST.Const_Node_Ptr;

end Parser;
