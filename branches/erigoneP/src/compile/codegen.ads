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
-- CODEGEN.ADS: The stack machine code generator package specification.
--
-- TODO:
-- 1. Complete code generation for missing constructs.
-- 3. Byte code optimization.

with Ada.Text_IO;

with Types.AST;     use Types.AST;
with Types.CodeGen; use Types.CodeGen;
with Types.Lexer;   use Types.Lexer;

package CodeGen is

  -- We raise this exception when an invalid binary
  -- or unary operator is seen.
  Invalid_Operator              : exception;
  -- As is the same in AST, we raise this exception when
  -- we do not, as yet, support a Promela construct.
  Unsupported_Construct         : exception;
  -- We raise this when the automaton text file cannot be opened for writing.
  File_Unopened                 : exception;
  -- We can throw this when code generation fails for any reason.
  Generic_Code_Generation_Error : exception;
  -- A user needs to only pass the root node of an AST to this
  -- procedure to generate code. The user should only do so after
  -- the entire AST has been subjected to semantic analysis in the
  -- AST package.
  procedure Spec               ( A_Spec_Node                  : Types.AST.Spec_Node_Ptr;
                                 Process_Declarations_Switch  : Boolean                ;
                                 Automaton_Filename           : String                 );
  -- For LTL formulae translation
  function Generate_Any_Expr  ( An_Any_Expr_Node: Types.AST.Any_Expr_Node_Ptr          )
           return Types.CodeGen.Byte_Code_List.List;


  private

    -- The automaton text file that we will be writing to.
    Automaton_File          : Ada.Text_IO.File_Type;
    -- The pointer to the current scope in a tree of scopes.
    -- Why a tree and not a linked list? See the type Scope in Types.AST.
    Top                     : Types.AST.Scope_Ptr;
    -- A boolean switch which, when true, is used to generate code
    -- for variable declarations and to generate code for statements
    -- otherwise.
    Process_Declarations_First  : Boolean;
    -- Process_Name is set to the name of the proctype we are currently
    -- visiting; otherwise it is Null_Bounded_String.
    Process_Name            : Types.Lexer.Bound_String.Bounded_String
                                        := Types.Lexer.Bound_String.Null_Bounded_String;
    -- In or out of an options sequence?
    In_Options : Boolean;
    -- 1 if we are visiting an Atomic sequence; 0 otherwise
    -- Used directly in Write_Transition to set an atomic transition
    In_Atomic               : Types.CodeGen.Byte range 0 .. 1 := 0;
    -- True if the variable being visited is a parameter in some proctype,
    -- false otherwise.
    In_Parameter_List       : Boolean;
    -- The globally incremented counter used to label every
    -- Types.AST.Stmt_Node uniquely.
    State_Number            : Types.CodeGen.State_Number;
    -- The list used to store byte code instructions to be flushed
    -- into a Types.AST.Stmt_Node.
    Statement_Byte_Code     : Types.CodeGen.Byte_Code_List.List;
    -- To handle as yet unnknown GOTO targets
    Outstanding_Goto_Targets: Types.AST.Bounded_String_To_Stmt_Node_List_Hashed_Map.Map;
    -- Channel handles
    Channel_Handle          : Types.CodeGen.Byte := 0;

    -- START: Transplant
    Global_Index              : Types.CodeGen.Symbol_Table_Index  := 0;
    Processes                 : Types.CodeGen.Byte                := 0;
    Process_Identifier        : Types.CodeGen.Name;
    Process_Initial_State     : Types.CodeGen.Byte                := 1;
    Process_Transition_List   : Types.CodeGen.Transitions_List.List;
    Incoming_Transitions_Map  : Types.CodeGen.Source_To_Transitions.Map;
    Outgoing_Transitions_Map  : Types.CodeGen.Source_To_Transitions.Map;
    Visited_Transitions       : Types.CodeGen.Transitions_List_Cursors.List;

    -- Open, close automaton file with filename
    procedure Open_Automaton_File( Filename : String ; Overwrite_File_Switch : Boolean );
    procedure Close_Automaton_File;
    -- begin a process, write its transitions, end it
    procedure Begin_New_Process;
    procedure Buffer_Transition(  Statement     : Types.CodeGen.Name  ;
                                  Source        : Types.CodeGen.Byte  ;
                                  Target        : Types.CodeGen.Byte  ;
                                  Atomic        : Types.CodeGen.Byte  ;
                                  End_Label     : Types.CodeGen.Byte  ;
                                  Accept_Label  : Types.CodeGen.Byte  ;
                                  Line_Number   : Types.CodeGen.Byte  ;
                                  Byte_Code     : Types.CodeGen.Byte_Code_List.List );
    procedure End_New_Process( An_Active_Node : Types.AST.Active_Node_Ptr );
    -- STOP: Transplant

    -- Link, in the current scope, this name to this node; return True if
    -- there is not another entry in this scope with the same name, False
    -- otherwise.
    function  Scope_Put( Name       : Types.Lexer.Bound_String.Bounded_String      ;
                         Definition : Types.AST.Node_Ptr                           )
              return Boolean;
    -- Get, from the current scope or its parents, the node that is mapped
    -- to this name; returns null if there is no entry for this name.
    function  Scope_Get        ( Name            : Types.Lexer.Bound_String.Bounded_String )
              return Types.AST.Node_Ptr;
    -- Change the scope to the first child of the current, parent scope.
    procedure Scope_Submerge;
    -- Change the scope to the parent of the current scope,
    -- deleting the first child scope of this parent from which
    -- we just "submerged."
    procedure Scope_Surface;
    -- Append this intruction to the global list of instructions.
    procedure Emit             ( Code            : in out Types.CodeGen.Byte_Code      );
    -- Flush the global list of instructions; used only while generating
    -- code for variable declarations, where the code is sent directly
    -- to the symbol table.
    procedure Flush;
    -- "Flush" the global list of instructions into a Types.AST.Stmt_Node.
    procedure Flush            ( A_Stmt_Node     : Types.AST.Stmt_Node_Ptr             );
    -- A procedure to unify writing transitions
    procedure Write_Transition( Source_Stmt      : Types.AST.Stmt_Node_Ptr             ;
                                Target_Stmt      : Types.AST.Stmt_Node_Ptr             );
    -- Use this to tell the user about an unsupported construct.
    procedure Raise_Unsupported_Construct
                               ( Name : String                                         );
    -- Use this to assert a code generation sanity check, culminating in
    -- immediate run-time termination and a message in case of failure.
    procedure Code_Generation_Assert
                               ( Assertion       : Boolean                             ;
                                 Message         : String                              );

    -- CodeGen visitors: these procedures are used in generating code
    -- for declarations and statements, as well as in linking
    -- states (or statements) appropriately to produce a finite state
    -- machine.
    procedure Module           ( A_Module_Node   : Types.AST.Module_Node_Ptr           );
    procedure Utype            ( An_Utype_Node   : Types.AST.Utype_Node_Ptr            );
    procedure Decl_Lst         ( A_Decl_Lst_Node : Types.AST.Decl_Lst_Node_Ptr         );
    procedure One_Decl         ( A_One_Decl_Node : Types.AST.One_Decl_Node_Ptr         );
    procedure Ivar             ( An_Ivar_Node    : Types.AST.Ivar_Node_Ptr             );
    procedure Sequence         ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr         );

    -- Every Sequence_Node, besides that inside an Options_Node, is
    -- both prepended and appended with a noop (no operation) Stmt_Node,
    -- Types.AST.Noop_Stmt_Node in certain conditions. A Sequence_Node
    -- inside an Options_Node is only appended with a noop Stmt_Node.
    -- Why do we do this? This is to guarantee that certain sequences
    -- will always have an entry or an exit.
    procedure Prepend_And_Append_Sequence_With_Noop
                               ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr         );
    -- Remember GOTO targets with unknown targets
    procedure Insert_Outstanding_Goto_Target
                               ( Name            : Types.Lexer.Bound_String.Bounded_String ;
                                 A_Goto_Stmt_Node: Types.AST.Goto_Stmt_Node_Ptr            );
    -- When a previously unknown GOTO target is seen, write all outstanding transitions
    procedure Write_Outstanding_Goto_Targets
                              ( A_Name_Stmt_Node : Types.AST.Name_Stmt_Node_Ptr        );

    -- Process DO-OD statements to produce the correct transitions
    procedure Process_Do_Statements
                               ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr         );
    -- Process IF-FI statements to produce the correct transitions
    procedure Process_If_Statements
                               ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr         );
    -- Process GOTO statements to produce the correct transitions
    procedure Process_Goto_Statements
                               ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr         );
    -- Process nested, atomic or d_step statements to
    -- produce the correct transitions
    procedure Process_Nested_Atomic_D_Step_Statements
                               ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr         );

    procedure Step             ( A_Step_Node     : Types.AST.Step_Node_Ptr             );
    procedure Stmt             ( A_Stmt_Node     : Types.AST.Stmt_Node_Ptr             );
    procedure Assign           ( An_Assign_Node  : Types.AST.Assign_Node_Ptr           );
    procedure Arg_Lst          ( An_Arg_Lst_Node : Types.AST.Arg_Lst_Node_Ptr          );
    procedure Options          ( An_Options_Node : Types.AST.Options_Node_Ptr          );
    procedure Proctype         ( A_Proctype_Node : Types.AST.Proctype_Node_Ptr         );
    -- Emit code for a binary operator
    procedure EmitBinaryOp     ( Operator        : Types.Lexer.Token_Type              );
    -- Emit code for a unary operator
    procedure EmitUnaryOp      ( Operator        : Types.Lexer.Token_Type              );
    procedure Any_Expr         ( An_Any_Expr_Node: Types.AST.Any_Expr_Node_Ptr         );
    -- Generate code for loading from a variable
    procedure Load_Varref      ( A_Varref_Node   : Types.AST.Varref_Node_Ptr           );
    -- Generate code for storing into a variable
    procedure Store_Varref     ( A_Varref_Node   : Types.AST.Varref_Node_Ptr           );
    -- Generate code for storing into a variable
    procedure Store_Ivar       ( An_Ivar_Node    : Types.AST.Ivar_Node_Ptr             );
    
end CodeGen;
