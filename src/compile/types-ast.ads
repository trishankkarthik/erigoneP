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
-- TYPES-AST.ADS: This is, by far, the most complicated child types package.
-- Most of the types are devoted to fulfilling a niche as an AST node, so
-- they correspond directly to our Promela grammar. Most of their attributes
-- are there to serve their roles as pure AST nodes, except for a few which
-- are for semantic analysis (e.g. types) and code generation (e.g. indices).
--
-- TODO:
-- 1. Constrain Lexer.Token_Type types..OR...use Boolean instead of Token_Type-s
-- 2. It would be nice to optimize space by creating and linking to only
-- one Stmt_Node per constant (i.e. number or string), but every Stmt_Node
-- has a unique line and state number.

with Ada.Strings.Unbounded;

with Types.Lexer;
with Types.CodeGen;

use type Types.Lexer.Bounded_String_Hashed_Set.Cursor;
use type Types.Lexer.Bound_String.Bounded_String;

package Types.AST is

  -------------------------------- SYMBOL TABLE ----------------------------------------------

  -- For Promela data types
  type Data_Type_Kind is ( Built_In_Type, Array_Type, User_Type );
  type Data_Type is abstract tagged
    record
      Kind                : Data_Type_Kind;
      -- TODO: Positive or Types.CodeGen.Byte?
      Size_In_Bytes       : Positive;
    end record;
  type Data_Type_Ptr          is access all Data_Type'Class;

  -- TODO: may as well merge this with Types.CodeGen.Symbol_Type
  type Built_In_Data_Type_Kind is ( Bit, Byte, Mtype, Chan, Pid, Short, Int, Unsigned );
  type Built_In_Data_Type is new Data_Type with
    record
      Size_In_Bits        : Positive;
      Min                 : Integer;
      Max                 : Integer;
      Built_In_Kind       : Built_In_Data_Type_Kind;
    end record;
  type Built_In_Data_Type_Ptr is access constant Built_In_Data_Type;
  
  -- NOTE: null by default, use Init_Globals for initialization
  Bit_Data_Type   : Data_Type_Ptr;
  Byte_Data_Type  : Data_Type_Ptr;
  Mtype_Data_Type : Data_Type_Ptr;
  Chan_Data_Type  : Data_Type_Ptr;
  Pid_Data_Type   : Data_Type_Ptr;
  Short_Data_Type : Data_Type_Ptr;
  Int_Data_Type   : Data_Type_Ptr;

  procedure Init_Globals;

  -- For mtypes
  use type Types.CodeGen.Byte;
  subtype Mtype_Byte        is Types.CodeGen.Byte range 1 .. 255;
  type    Mtype_Array       is array ( Mtype_Byte ) of Lexer.Bounded_String_Hashed_Set.Cursor;
  
  Mtype_Full_Exception      : exception;
  Mtype_Redundant_Exception : exception;
 
  subtype Operator_Index is Lexer.Token_Type range Lexer.Token_Or_Logical .. Lexer.Token_Modulus;
  type Operator_Precedence is array ( Operator_Index ) of Positive range 3 .. 12;

  -------------------------------- SYMBOL TABLE ----------------------------------------------


  -------------------------------- ABSTRACT SYNTAX TREE --------------------------------------
 
  -- See the diagram in "ast.png" for a visualization of the AST hierarchy.
  -- the ultimate ancestor of all AST nodes
  type Node;
  type Node_Ptr                   is access all Node'Class;
  
  -- forward declarations
  type Spec_Node;
  type Spec_Node_Ptr              is access Spec_Node;
  type Module_Node;
  type Module_Node_Ptr            is access all Module_Node'Class;
  type Proctype_Module_Node;
  type Proctype_Module_Node_Ptr   is access all Proctype_Module_Node;
  type Init_Module_Node;
  type Init_Module_Node_Ptr       is access all Init_Module_Node;
  type Never_Module_Node;
  type Never_Module_Node_Ptr      is access all Never_Module_Node;
  type Trace_Module_Node;
  type Trace_Module_Node_Ptr      is access all Trace_Module_Node;
  type Utype_Module_Node;
  type Utype_Module_Node_Ptr      is access all Utype_Module_Node;
  type Mtype_Module_Node;
  type Mtype_Module_Node_Ptr      is access all Mtype_Module_Node;
  type Decl_Lst_Module_Node;
  type Decl_Lst_Module_Node_Ptr   is access all Decl_Lst_Module_Node;
  type Never_Node;
  type Never_Node_Ptr             is access Never_Node;
  type Trace_Node;
  type Trace_Node_Ptr             is access Trace_Node;
  type Utype_Node;
  type Utype_Node_Ptr             is access all Utype_Node'Class;
  type Mtype_Node;
  type Mtype_Node_Ptr             is access all Mtype_Node'Class;

  type Sequence_Node;
  type Sequence_Node_Ptr          is access Sequence_Node;
  type Step_Node;
  type Step_Node_Ptr              is access all Step_Node'Class;
  type Stmt_Step_Node;
  type Stmt_Step_Node_Ptr         is access all Stmt_Step_Node;
  type Decl_Lst_Step_Node;
  type Decl_Lst_Step_Node_Ptr     is access all Decl_Lst_Step_Node;
  type Xr_Xs_Step_Node;
  type Xr_Xs_Step_Node_Ptr        is access all Xr_Xs_Step_Node;
  
  type Decl_Lst_Node;
  type Decl_Lst_Node_Ptr          is access Decl_Lst_Node;
  type One_Decl_Node;
  type One_Decl_Node_Ptr          is access One_Decl_Node;
  type Typename_Node;
  type Typename_Node_Ptr          is access all Typename_Node'Class;
  type Predefined_Typename_Node;
  type Predefined_Typename_Node_Ptr
                                  is access all Predefined_Typename_Node;
  type Uname_Typename_Node;
  type Uname_Typename_Node_Ptr    is access all Uname_Typename_Node;
  type Priority_Node;
  type Priority_Node_Ptr          is access Priority_Node;
  type Ivar_Node;
  type Ivar_Node_Ptr              is access all Ivar_Node'Class;
  type Any_Expr_Ivar_Node;
  type Any_Expr_Ivar_Node_Ptr     is access all Any_Expr_Ivar_Node;
  type Ch_Init_Node;
  type Ch_Init_Node_Ptr           is access Ch_Init_Node;
  type Ch_Init_Ivar_Node;
  type Ch_Init_Ivar_Node_Ptr      is access all Ch_Init_Ivar_Node;

  type Active_Node;
  type Active_Node_Ptr            is access Active_Node;
  type Enabler_Node;
  type Enabler_Node_Ptr           is access Enabler_Node;
  type Proctype_Node;
  type Proctype_Node_Ptr          is access Proctype_Node'Class;

  type Varref_Node;
  type Varref_Node_Ptr            is access Varref_Node;
  type Send_Args_Node;
  type Send_Args_Node_Ptr         is access Send_Args_Node;
  type Arg_Lst_Node;
  type Arg_Lst_Node_Ptr           is access Arg_Lst_Node;
  type Recv_Args_Node;
  type Recv_Args_Node_Ptr         is access Recv_Args_Node;
  type Recv_Arg_Node;
  type Recv_Arg_Node_Ptr          is access all Recv_Arg_Node'Class;
  type Varref_Recv_Arg_Node;
  type Varref_Recv_Arg_Node_Ptr   is access all Varref_Recv_Arg_Node;
  type Eval_Recv_Arg_Node;
  type Eval_Recv_Arg_Node_Ptr     is access all Eval_Recv_Arg_Node;
  type Const_Recv_Arg_Node;
  type Const_Recv_Arg_Node_Ptr    is access all Const_Recv_Arg_Node;

  type Send_Node;
  type Send_Node_Ptr              is access all Send_Node'Class;
  type Fifo_Send_Node;
  type Fifo_Send_Node_Ptr         is access all Fifo_Send_Node;
  type Sorted_Send_Node;
  type Sorted_Send_Node_Ptr       is access all Sorted_Send_Node;
  type Recv_Node;
  type Recv_Node_Ptr              is access all Recv_Node'Class;
  type Move_Fifo_Recv_Node;
  type Move_Fifo_Recv_Node_Ptr    is access all Move_Fifo_Recv_Node;
  type Copy_Fifo_Recv_Node;
  type Copy_Fifo_Recv_Node_Ptr    is access all Copy_Fifo_Recv_Node;
  type Move_Random_Recv_Node;
  type Move_Random_Recv_Node_Ptr  is access all Move_Random_Recv_Node;
  type Copy_Random_Recv_Node;
  type Copy_Random_Recv_Node_Ptr  is access all Copy_Random_Recv_Node;
  type Assign_Node;
  type Assign_Node_Ptr            is access all Assign_Node'Class;
  type Any_Expr_Assign_Node;
  type Any_Expr_Assign_Node_Ptr   is access all Any_Expr_Assign_Node;
  type Decrement_Assign_Node;
  type Decrement_Assign_Node_Ptr  is access all Decrement_Assign_Node;
  type Increment_Assign_Node;
  type Increment_Assign_Node_Ptr  is access all Increment_Assign_Node;
  
  type Stmt_Node;
  type Stmt_Node_Ptr              is access all Stmt_Node'Class;

  type Send_Stmt_Node;
  type Send_Stmt_Node_Ptr         is access all Send_Stmt_Node;
  type Recv_Stmt_Node;
  type Recv_Stmt_Node_Ptr         is access all Recv_Stmt_Node;
  type Assign_Stmt_Node;
  type Assign_Stmt_Node_Ptr       is access all Assign_Stmt_Node;  
  type Goto_Stmt_Node;
  type Goto_Stmt_Node_Ptr         is access all Goto_Stmt_Node;
  type If_Stmt_Node;
  type If_Stmt_Node_Ptr           is access all If_Stmt_Node;
  type Do_Stmt_Node;
  type Do_Stmt_Node_Ptr           is access all Do_Stmt_Node;
  type Atomic_Stmt_Node;
  type Atomic_Stmt_Node_Ptr       is access all Atomic_Stmt_Node;
  type D_Step_Stmt_Node;
  type D_Step_Stmt_Node_Ptr       is access all D_Step_Stmt_Node;
  type Sequence_Stmt_Node;
  type Sequence_Stmt_Node_Ptr     is access all Sequence_Stmt_Node;
  type Break_Stmt_Node;
  type Break_Stmt_Node_Ptr        is access all Break_Stmt_Node;
  type Else_Stmt_Node;
  type Else_Stmt_Node_Ptr         is access all Else_Stmt_Node;
  type Name_Stmt_Node;
  type Name_Stmt_Node_Ptr         is access all Name_Stmt_Node;
  type Printf_Stmt_Node;
  type Printf_Stmt_Node_Ptr       is access all Printf_Stmt_Node;
  type Printm_Stmt_Node;
  type Printm_Stmt_Node_Ptr       is access all Printm_Stmt_Node;
  type Assert_Stmt_Node;
  type Assert_Stmt_Node_Ptr       is access all Assert_Stmt_Node;
  type Any_Expr_Stmt_Node;
  type Any_Expr_Stmt_Node_Ptr     is access all Any_Expr_Stmt_Node;
  type C_Code_Stmt_Node;
  type C_Code_Stmt_Node_Ptr       is access all C_Code_Stmt_Node;
  type C_Expr_Stmt_Node;
  type C_Expr_Stmt_Node_Ptr       is access all C_Expr_Stmt_Node;
  type C_Decl_Stmt_Node;
  type C_Decl_Stmt_Node_Ptr       is access all C_Decl_Stmt_Node;
  type C_Track_Stmt_Node;
  type C_Track_Stmt_Node_Ptr      is access all C_Track_Stmt_Node;
  type C_State_Stmt_Node;
  type C_State_Stmt_Node_Ptr      is access all C_State_Stmt_Node;

  type Options_Node;
  type Options_Node_Ptr           is access Options_Node;
  
  type Any_Expr_Node;
  type Any_Expr_Node_Ptr          is access all Any_Expr_Node'Class;
  type Implies_Node;
  type Implies_Node_Ptr           is access Implies_Node;
  type Binar_Op_Node;
  type Binar_Op_Node_Ptr          is access Binar_Op_Node;
  type Poll_Node;
  type Poll_Node_Ptr              is access all Poll_Node'Class;
  type Fifo_Poll_Node;
  type Fifo_Poll_Node_Ptr         is access all Fifo_Poll_Node;
  type Random_Poll_Node;
  type Random_Poll_Node_Ptr       is access all Random_Poll_Node;

  type Binary_Any_Expr_Node;
  type Binary_Any_Expr_Node_Ptr   is access all Binary_Any_Expr_Node;
  
  type Higher_Precedence_Implies_Any_Expr_Node;
  type Higher_Precedence_Implies_Any_Expr_Node_Ptr
                                  is access all Higher_Precedence_Implies_Any_Expr_Node;
  type Unary_Any_Expr_Node;
  type Unary_Any_Expr_Node_Ptr    is access all Unary_Any_Expr_Node;
  type Len_Any_Expr_Node;
  type Len_Any_Expr_Node_Ptr      is access all Len_Any_Expr_Node;
  type Const_Any_Expr_Node;
  type Const_Any_Expr_Node_Ptr    is access all Const_Any_Expr_Node;

  type Varref_Any_Expr_Node;
  type Varref_Any_Expr_Node_Ptr   is access all Varref_Any_Expr_Node;
  type Poll_Any_Expr_Node;
  type Poll_Any_Expr_Node_Ptr     is access all Poll_Any_Expr_Node;
  type Remoteref_Any_Expr_Node;
  type Remoteref_Any_Expr_Node_Ptr
                                  is access all Remoteref_Any_Expr_Node;
  type Timeout_Any_Expr_Node;
  type Timeout_Any_Expr_Node_Ptr  is access all Timeout_Any_Expr_Node;
  type Non_Progress_Any_Expr_Node;
  type Non_Progress_Any_Expr_Node_Ptr
                                  is access all Non_Progress_Any_Expr_Node;
  type Enabled_Any_Expr_Node;
  type Enabled_Any_Expr_Node_Ptr  is access all Enabled_Any_Expr_Node;
  type Pc_Value_Any_Expr_Node;
  type Pc_Value_Any_Expr_Node_Ptr is access all Pc_Value_Any_Expr_Node;
  type Run_Any_Expr_Node;
  type Run_Any_Expr_Node_Ptr      is access all Run_Any_Expr_Node;
  type Channel_Poll_Any_Expr_Node;
  type Channel_Poll_Any_Expr_Node_Ptr
                                  is access all Channel_Poll_Any_Expr_Node'Class;
  type Empty_Channel_Poll_Any_Expr_Node;
  type Empty_Channel_Poll_Any_Expr_Node_Ptr
                                  is access all Empty_Channel_Poll_Any_Expr_Node;
  type Nempty_Channel_Poll_Any_Expr_Node;
  type Nempty_Channel_Poll_Any_Expr_Node_Ptr
                                  is access all Nempty_Channel_Poll_Any_Expr_Node;
  type Full_Channel_Poll_Any_Expr_Node;
  type Full_Channel_Poll_Any_Expr_Node_Ptr
                                  is access all Full_Channel_Poll_Any_Expr_Node;
  type Nfull_Channel_Poll_Any_Expr_Node;
  type Nfull_Channel_Poll_Any_Expr_Node_Ptr
                                  is access all Nfull_Channel_Poll_Any_Expr_Node;

  -- There probably is a reason (i.e. reserved use) why it is Uname instead of NAME.
  type Uname_Node;
  type Uname_Node_Ptr             is access Uname_Node;
  type Const_Node;
  type Const_Node_Ptr             is access Const_Node;

  -- A much needed set of enumerated constants to let us discriminate between nodes
  -- TODO: remove enumerations which are not used in abstract types
  type Node_Tag is
  (
    -- Generic tag
    Node_Null,
    Node_Spec,
    Node_Module,
    Node_Proctype_Module,
    Node_Init_Module,
    Node_Never_Module,
    Node_Trace_Module,
    Node_Utype_Module,
    Node_Mtype_Module,
    Node_Decl_Lst_Module,
    Node_Never,
    Node_Trace,
    Node_Utype,
    Node_Mtype,
    Node_Sequence,
    Node_Step,
    Node_Stmt_Step,
    Node_Decl_Lst_Step,
    Node_Xr_Xs_Step,
    Node_Decl_Lst,
    Node_One_Decl,
    Node_Typename,
    Node_Predefined_Typename,
    Node_Uname_Typename,
    Node_Priority,
    Node_Ivar,
    Node_Any_Expr_Ivar,
    Node_Ch_Init,
    Node_Ch_Init_Ivar,
    Node_Active,
    Node_Enabler,
    Node_Proctype,
    Node_Varref,
    Node_Send_Args,
    Node_Arg_Lst,
    Node_Recv_Args,
    Node_Recv_Arg,
    Node_Varref_Recv_Arg,
    Node_Eval_Recv_Arg,
    Node_Const_Recv_Arg,
    Node_Send,
    Node_Fifo_Send,
    Node_Sorted_Send,
    Node_Recv,
    Node_Move_Fifo_Recv,
    Node_Copy_Fifo_Recv,
    Node_Move_Random_Recv,
    Node_Copy_Random_Recv,
    Node_Assign,
    Node_Any_Expr_Assign,
    Node_Decrement_Assign,
    Node_Increment_Assign,
    Node_Stmt,
    Node_Send_Stmt,
    Node_Recv_Stmt,
    Node_Assign_Stmt,
    Node_Goto_Stmt,
    Node_If_Stmt,
    Node_Do_Stmt,
    Node_Atomic_Stmt,
    Node_D_Step_Stmt,
    Node_Sequence_Stmt,
    Node_Break_Stmt,
    Node_Else_Stmt,
    Node_Name_Stmt,
    Node_Printf_Stmt,
    Node_Printm_Stmt,
    Node_Assert_Stmt,
    Node_Any_Expr_Stmt,
    Node_C_Code_Stmt,
    Node_C_Expr_Stmt,
    Node_C_Decl_Stmt,
    Node_C_Track_Stmt,
    Node_C_State_Stmt,
    Node_Noop_Stmt,
    Node_Options,
    Node_Any_Expr,
    Node_Implies,
    Node_Binar_Op,
    Node_Poll,
    Node_Fifo_Poll,
    Node_Random_Poll,
    Node_Binary_Any_Expr,
    Node_Higher_Precedence_Implies_Any_Expr,
    Node_Unary_Any_Expr,
    Node_Len_Any_Expr,
    Node_Const_Any_Expr,
    Node_Varref_Any_Expr,
    Node_Poll_Any_Expr,
    Node_Remoteref_Any_Expr,
    Node_Timeout_Any_Expr,
    Node_Non_Progress_Any_Expr,
    Node_Enabled_Any_Expr,
    Node_Pc_Value_Any_Expr,
    Node_Run_Any_Expr,
    Node_Channel_Poll_Any_Expr,
    Node_Empty_Channel_Poll_Any_Expr,
    Node_Nempty_Channel_Poll_Any_Expr,
    Node_Full_Channel_Poll_Any_Expr,
    Node_Nfull_Channel_Poll_Any_Expr,
    Node_Uname,
    Node_Const
  );

  -- Some useful lists

  -- Useful for Mtype_Node
  package Bounded_String_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Lexer.Bound_String.Bounded_String
  );

  package Module_Node_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Module_Node_Ptr
  );

  -- especially for Mtype_Node
  package Bounded_String_To_Mtype_Byte_Hashed_Map is new Ada.Containers.Hashed_Maps(
    Key_Type        => Lexer.Bound_String.Bounded_String,
    Element_Type    => Types.AST.Mtype_Byte,
    Hash            => Lexer.Bounded_String_Hash,
    Equivalent_Keys => "=");

  package One_Decl_Node_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => One_Decl_Node_Ptr
  );

  package Ivar_Node_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Ivar_Node_Ptr
  );
  
  package Typename_Node_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Typename_Node_Ptr
  );
  
  package Any_Expr_Node_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Any_Expr_Node_Ptr
  );

  package Recv_Arg_Node_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Recv_Arg_Node_Ptr
  );

  package Sequence_Node_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Sequence_Node_Ptr
  );
  
  package Step_Node_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Step_Node_Ptr
  );

  package Stmt_Node_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Stmt_Node_Ptr
  );

  -- A hashed map of identifier -> Stmt_Node-s
  -- Goto `symbol table'; see Outstanding_Goto_Targets in CodeGen
  use type Stmt_Node_List.List;
  package Bounded_String_To_Stmt_Node_List_Hashed_Map is new Ada.Containers.Hashed_Maps(
    Key_Type        => Lexer.Bound_String.Bounded_String,
    -- TODO: might wanna change this to a container Identifier_Node
    Element_Type    => Stmt_Node_List.List,
    Hash            => Lexer.Bounded_String_Hash,
    Equivalent_Keys => "=");
  
  package Varref_Node_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Varref_Node_Ptr
  );

  -- Node definitions

  type Node is abstract
    tagged record
      Tag : Node_Tag  :=  Node_Null;
    end record;
  
  type Spec_Node is new Node with
    record
      Module_List : Module_Node_List.List;
    end record;
 
  -- Why do all the derivates have children with their own node types? Robustness.
  type Module_Node          is abstract new Node          with null record;
  
  type Proctype_Module_Node is          new Module_Node   with
    record
      Proctype          : Proctype_Node_Ptr;
    end record;
  
  type Init_Module_Node     is          new Module_Node   with
    record
      Pinit             : Proctype_Node_Ptr;
    end record;

  type Never_Module_Node    is          new Module_Node   with
    record
      Never             : Never_Node_Ptr;
    end record;

  type Trace_Module_Node    is          new Module_Node   with
    record
      Trace             : Trace_Node_Ptr;
    end record;

  type Utype_Module_Node    is          new Module_Node   with
    record
      Utype             : Utype_Node_Ptr;
    end record;

  type Mtype_Module_Node    is          new Module_Node   with
    record
      Mtype             : Mtype_Node_Ptr;
    end record;

  type Never_Node is new Node with
    record
      Sequence          : Sequence_Node_Ptr;
    end record;

  type Trace_Node is new Node with
    record
      Version           : Lexer.Token_Type;
      Sequence          : Sequence_Node_Ptr;
    end record;

  type Utype_Node is new Node with
    record
      Name              : Lexer.Bounded_String_Hashed_Set.Cursor  := Lexer.Bounded_String_Hashed_Set.No_Element;
      Decl_Lst          : Decl_Lst_Node_Ptr;
    end record;

  type Mtype_Node is new Node with
    record
      Names_List        : Bounded_String_List.List;
      Names_Map         : Bounded_String_To_Mtype_Byte_Hashed_Map.Map;
    end record;

  type Sequence_Node is new Node with
    record
      Decl_Lst_Step_List: Step_Node_List.List;
      Stmt_Step_List    : Step_Node_List.List;
      Xr_Xs_Step_List   : Step_Node_List.List;
    end record;

  type Step_Node is abstract new Node with null record;

  type Stmt_Step_Node is new Step_Node with
    record
      Statement         : Stmt_Node_Ptr;
      Unless            : Stmt_Node_Ptr;
    end record;

  type Decl_Lst_Step_Node is new Step_Node with
    record
      Decl_Lst          : Decl_Lst_Node_Ptr;
    end record;

  type Xr_Xs_Step_Node is new Step_Node with
    record
      Channel_Assertion : Lexer.Token_Type range Lexer.Token_Xr .. Lexer.Token_Xs;
      Channel_Names_List: Varref_Node_List.List;
    end record;

  type Decl_Lst_Node is new Node with
    record
      One_Decl_List     : One_Decl_Node_List.List;
      -- semantic analysis
      Total_Variable_List
                        : Ivar_Node_List.List;
      Size_In_Bytes     : Positive;
    end record;

  type Decl_Lst_Module_Node is new Module_Node  with
    record
      Decl_Lst          : Decl_Lst_Node_Ptr;
    end record;

  type Typename_Node is abstract new Node with
    record
      -- Point to data type; to be percolated later to Ivar_Node
      The_Type          : Data_Type_Ptr;
    end record;
  
  type Predefined_Typename_Node is new Typename_Node with
    record
      Name              : Lexer.Token_Type range Lexer.Token_Bit  .. Lexer.Token_Unsigned;
    end record;

  type Uname_Typename_Node is new Typename_Node with
    record
      UName             : Uname_Node_Ptr;
    end record;
 
  type Ivar_Node is new Node with
    record
      Name              : Lexer.Bounded_String_Hashed_Set.Cursor  := Lexer.Bounded_String_Hashed_Set.No_Element;
      -- NOTE: Making Array_Length a Const_Node_Ptr instead of Lexer.Natural_Hashed_Set.Cursor,
      -- so that debugging is easier in unexpected cases.
      Array_Length      : Const_Node_Ptr;
      -- semantic analysis
      The_Type          : Data_Type_Ptr;
      -- code generation
      Index             : Types.CodeGen.Symbol_Table_Index := 0;
    end record;

  type Any_Expr_Ivar_Node is new Ivar_Node with
    record
      Assignment        : Any_Expr_Node_Ptr;
    end record;

  type Ch_Init_Node is new Node with
    record
      Size              : Const_Node_Ptr;
      Typename_List     : Typename_Node_List.List;
      -- Semantic analysis
      Array_Size        : Positive;
      Buffer_Size       : Types.Lexer.Promela_Natural := 0;
      Types_Size        : Positive;
      -- Buffer_Size * Types_Size
      Channel_Size      : Positive;
      Size_In_Bytes     : Positive;
    end record;

  type Ch_Init_Ivar_Node is new Ivar_Node with
    record
      Assignment        : Ch_Init_Node_Ptr;
    end record;

  -- COMMENTARY: Now, you may wonder why I give trivial things like these their own nodes.
  -- One reason is for consistency.
  -- The other is for robustness, so that a change in the Priority production can be more readily accommodated.
  type Priority_Node is new Node with
    record
      Number            : Const_Node_Ptr;
    end record;
  
  type Active_Node is new Node with
    record
      Number            : Const_Node_Ptr;
    end record;
  
  type Enabler_Node is new Node with
    record
      Condition         : Any_Expr_Node_Ptr;
    end record;
  
  type Proctype_Node is new Node with
    record
      Active            : Active_Node_Ptr;
      -- FIXME: I suppose I could make this boolean.
      Version           : Lexer.Token_Type;
      Name              : Lexer.Bounded_String_Hashed_Set.Cursor  := Lexer.Bounded_String_Hashed_Set.No_Element;
      Parameters        : Decl_Lst_Node_Ptr;
      Priority          : Priority_Node_Ptr;
      Enabler           : Enabler_Node_Ptr;
      Sequence          : Sequence_Node_Ptr;
    end record;

  type One_Decl_Node is new Node with
    record
      Visibility        : Lexer.Token_Type                        := Lexer.Token_Null;
      Typename          : Typename_Node_Ptr;
      Variable_List     : Ivar_Node_List.List;
      -- semantic analysis
      Size_In_Bytes     : Positive;
    end record;

  type Varref_Node is new Node with
    record
      Name              : Lexer.Bounded_String_Hashed_Set.Cursor  := Lexer.Bounded_String_Hashed_Set.No_Element;
      Array_Offset      : Any_Expr_Node_Ptr;
      Struct_Offset     : Varref_Node_Ptr;
      -- Semantic Analysis: point to node that declared this name (see AST.Varref)
      -- TODO: actually, why not directly point to the declaration type?
      -- Even though this way seems efficient,
      -- pointing directly to the type may result in cleaner code.
      Declaration       : Node_Ptr;
    end record;

  type Send_Args_Node is new Node with
    record
      Arguments         : Arg_Lst_Node_Ptr;
    end record;

  type Arg_Lst_Node is new Node with
    record
      Any_Expr_List     : Any_Expr_Node_List.List;
    end record;

  type Recv_Args_Node is new Node with
    record
      Arguments         : Recv_Arg_Node_List.List;
    end record;

  type Recv_Arg_Node is abstract new Node with null record;

  type Varref_Recv_Arg_Node is new Recv_Arg_Node with
    record
      Variable          : Varref_Node_Ptr;
    end record;

  type Eval_Recv_Arg_Node is new Recv_Arg_Node with
    record
      Argument          : Varref_Node_Ptr;
    end record;

  type Const_Recv_Arg_Node is new Recv_Arg_Node with
    record
      Negate            : Boolean;
      A_Constant        : Const_Node_Ptr;
    end record;

  type Send_Node is abstract new Node with
    record
      Source            : Varref_Node_Ptr;
      Arguments         : Send_Args_Node_Ptr;
    end record;

  type Fifo_Send_Node is new Send_Node with null record;
  type Sorted_Send_Node is new Send_Node with null record;

  type Recv_Node is abstract new Node with
    record
      Target            : Varref_Node_Ptr;
      Arguments         : Recv_Args_Node_Ptr;
    end record;

  type Move_Fifo_Recv_Node is new Recv_Node with null record;
  type Copy_Fifo_Recv_Node is new Recv_Node with null record;
  type Move_Random_Recv_Node is new Recv_Node with null record;
  type Copy_Random_Recv_Node is new Recv_Node with null record;

  type Assign_Node is abstract new Node with
    record
      L_Value           : Varref_Node_Ptr;
    end record;

  type Any_Expr_Assign_Node is new Assign_Node with
    record
      R_Value           : Any_Expr_Node_Ptr;
    end record;

  type Decrement_Assign_Node is new Assign_Node with null record;
  type Increment_Assign_Node is new Assign_Node with null record;

  type Stmt_Node is abstract new Node with
    record
      -- CodeGen
      Line_Number           : Positive := 1;
      State_Number          : Positive := 1;
      Byte_Code             : Types.CodeGen.Byte_Code_List.List;
      Text                  : Ada.Strings.Unbounded.Unbounded_String :=
                                Ada.Strings.Unbounded.Null_Unbounded_String;
    end record;

  type Assign_Stmt_Node is new Stmt_Node with
    record
      Assignment        : Assign_Node_Ptr;
    end record;
 
  type Send_Stmt_Node is new Stmt_Node with
    record
      Send              : Send_Node_Ptr;
    end record;

  type Recv_Stmt_Node is new Stmt_Node with
    record
      Receive           : Recv_Node_Ptr;
    end record;
  
  type Goto_Stmt_Node is new Stmt_Node with
    record
      Name              : Lexer.Bounded_String_Hashed_Set.Cursor  := Lexer.Bounded_String_Hashed_Set.No_Element;
      -- TODO: fill this in; see AST.Name_Stmt_Node.Statement
      Goto_Target       : AST.Stmt_Node_Ptr;
    end record;
  
  type If_Stmt_Node is new Stmt_Node with
    record
      Options           : AST.Options_Node_Ptr;
    end record;

  type Do_Stmt_Node is new Stmt_Node with
    record
      Options           : AST.Options_Node_Ptr;
    end record;

  type Atomic_Stmt_Node is new Stmt_Node with
    record
      Sequence          : AST.Sequence_Node_Ptr;
    end record;

  type D_Step_Stmt_Node is new Stmt_Node with
    record
      Sequence          : AST.Sequence_Node_Ptr;
    end record;

  type Sequence_Stmt_Node is new Stmt_Node with
    record
      Sequence          : AST.Sequence_Node_Ptr;
    end record;

  type Break_Stmt_Node is new Stmt_Node with null record;
  type Else_Stmt_Node is new Stmt_Node with null record;

  type Name_Stmt_Node is new Stmt_Node with
    record
      Name              : Lexer.Bounded_String_Hashed_Set.Cursor  := Lexer.Bounded_String_Hashed_Set.No_Element;
      Statement         : Stmt_Node_Ptr;
      -- SEMANTIC ANALYSIS + CODE GENERATION
      Sequence          : Sequence_Node_Ptr;
    end record;

  type Printf_Stmt_Node is new Stmt_Node with
    record
      String            : Lexer.Bounded_String_Hashed_Set.Cursor  := Lexer.Bounded_String_Hashed_Set.No_Element;
      Arguments         : Arg_Lst_Node_Ptr;
    end record;

  type Printm_Stmt_Node is new Stmt_Node with
    record
      Name              : Lexer.Bounded_String_Hashed_Set.Cursor  := Lexer.Bounded_String_Hashed_Set.No_Element;
      -- AST: point to Mtype_Node or mtype Ivar_Node
      Declaration       : Node_Ptr;
    end record;
  
  type Assert_Stmt_Node is new Stmt_Node with
    record
      Expression : Any_Expr_Node_Ptr;
    end record;

  type Any_Expr_Stmt_Node is new Stmt_Node with
    record
      Expression : Any_Expr_Node_Ptr;
    end record;

  -- NOTE: Not that we recognize C code now!
  type C_Code_Stmt_Node   is new Stmt_Node with null record;
  type C_Expr_Stmt_Node   is new Stmt_Node with null record;
  type C_Decl_Stmt_Node   is new Stmt_Node with null record;
  type C_Track_Stmt_Node  is new Stmt_Node with null record;
  type C_State_Stmt_Node  is new Stmt_Node with null record;
  
  -- CodeGen
  type Noop_Stmt_Node is new Stmt_Node with null record;

  type Options_Node is new Node with
    record
      Sequence_List     : Sequence_Node_List.List;
    end record;

  type Any_Expr_Node is abstract new Node with
    record
      BinaryOp          : Binar_Op_Node_Ptr;
      -- Semantic Analysis
      --LUB_Type          : Data_Type_Ptr;
    end record;

  -- START: auxillary nodes for Any_Expr_Node-s
  type Implies_Node is new Node with
    record
      IfTrue            : Any_Expr_Node_Ptr;
      IfFalse           : Any_Expr_Node_Ptr;
    end record;

  type Poll_Node is abstract new Node with
    record
      Target            : Varref_Node_Ptr;
      Arguments         : Recv_Args_Node_Ptr;
    end record;

  type Fifo_Poll_Node is new Poll_Node with null record;
  type Random_Poll_Node is new Poll_Node with null record;

  type Binar_Op_Node is new Node with
    record
      Operator          : Lexer.Token_Type;
      Generic_Any_Expr  : Any_Expr_Node_Ptr;
    end record;
  -- STOP: auxillary nodes for Any_Expr_Node-s
  
  -- START: Tree rewriting
  -- NOTE: Yes, the Binary_Any_Expr_Node is not the most efficient data structure,
  -- but it is one that allows for the easiest tree rewriting.
  type Binary_Any_Expr_Node is new Any_Expr_Node with
    record
      Operator          : Lexer.Token_Type          := Lexer.Token_Null;
      Left_Leaf         : Any_Expr_Node_Ptr;
      Right_Leaf        : Any_Expr_Node_Ptr;
      Left_Expression   : Binary_Any_Expr_Node_Ptr;
      Right_Expression  : Binary_Any_Expr_Node_Ptr;
    end record;
  -- STOP:  Tree rewriting

  type Higher_Precedence_Implies_Any_Expr_Node is new Any_Expr_Node with
    record
      Generic_Any_Expr  : Any_Expr_Node_Ptr;
      Implies           : Implies_Node_Ptr;
    end record;

  type Unary_Any_Expr_Node is new Any_Expr_Node with
    record
      Operator          : Lexer.Token_Type;
      Generic_Any_Expr  : Any_Expr_Node_Ptr;
    end record;

  type Len_Any_Expr_Node is new Any_Expr_Node with
    record
      Channel_Name      : Varref_Node_Ptr;
    end record;

  type Varref_Any_Expr_Node is new Any_Expr_Node with
    record
      Variable          : Varref_Node_Ptr;
    end record;

  type Poll_Any_Expr_Node is new Any_Expr_Node with
    record
      Poll              : Poll_Node_Ptr;
    end record;

  type Remoteref_Any_Expr_Node is new Any_Expr_Node with
    record
      Version           : Lexer.Token_Type;
      Process           : Varref_Node_Ptr;
      Process_Target    : Lexer.Bounded_String_Hashed_Set.Cursor  := Lexer.Bounded_String_Hashed_Set.No_Element;
    end record;

  type Const_Any_Expr_Node is new Any_Expr_Node with
    record
      A_Constant        : Const_Node_Ptr;
    end record;

  type Timeout_Any_Expr_Node is new Any_Expr_Node with null record;

  type Non_Progress_Any_Expr_Node is new Any_Expr_Node with null record;

  type Enabled_Any_Expr_Node is new Any_Expr_Node with
    record
      Generic_Any_Expr  : Any_Expr_Node_Ptr;
    end record;

  type Pc_Value_Any_Expr_Node is new Any_Expr_Node with
    record
      Generic_Any_Expr  : Any_Expr_Node_Ptr;
    end record;

  type Run_Any_Expr_Node is new Any_Expr_Node with
    record
      Name              : Lexer.Bounded_String_Hashed_Set.Cursor  := Lexer.Bounded_String_Hashed_Set.No_Element;
      Arguments         : Arg_Lst_Node_Ptr;
      Priority          : Priority_Node_Ptr;
    end record;

  type Channel_Poll_Any_Expr_Node is abstract new Any_Expr_Node with
    record
      Channel_Name      : Varref_Node_Ptr;
    end record;

  type Empty_Channel_Poll_Any_Expr_Node is new Channel_Poll_Any_Expr_Node with null record;
  type Nempty_Channel_Poll_Any_Expr_Node is new Channel_Poll_Any_Expr_Node with null record;
  type Full_Channel_Poll_Any_Expr_Node is new Channel_Poll_Any_Expr_Node with null record;
  type Nfull_Channel_Poll_Any_Expr_Node is new Channel_Poll_Any_Expr_Node with null record;

  type Uname_Node is new Node with
    record
      Name              : Lexer.Bounded_String_Hashed_Set.Cursor  := Lexer.Bounded_String_Hashed_Set.No_Element;
    end record;

  type Const_Node is new Node with
    record
      Number            : Lexer.Natural_Hashed_Set.Cursor         := Lexer.Natural_Hashed_Set.No_Element;
      -- Semantic Analysis
      The_Type          : Data_Type_Ptr;
    end record;
  
  -------------------------------- ABSTRACT SYNTAX TREE --------------------------------------
  

  -------------------------------- SYMBOL TABLE ----------------------------------------------

  -- Add an mtype to the global table and decorate it with its number
  procedure Add_Mtype(  A_Mtype_Node  : Mtype_Node_Ptr                                  ;
                        Name          : Types.Lexer.Bound_String.Bounded_String         );

  
  -- Node "symbol table" (a hashed map of identifier -> node_ptr)
  package Bounded_String_To_Node_Ptr_Hashed_Map is new Ada.Containers.Hashed_Maps(
    Key_Type        => Lexer.Bound_String.Bounded_String,
    -- TODO: might wanna change this to a container Identifier_Node
    Element_Type    => AST.Node_Ptr,
    Hash            => Lexer.Bounded_String_Hash,
    Equivalent_Keys => "=");

  -- I promised an explanation for why Scope is a tree of Scope-s rather
  -- than a linked list. Well, we need to pass the entire chain of Scope-s
  -- between AST and CodeGen without destroying any one of them, and if you
  -- think about it or draw it out, you will see that they form a tree.
  type Scope;
  type Scope_Ptr is access Scope;
  package Scope_List is new Ada.Containers.Doubly_Linked_Lists(
      Element_Type  => Scope_Ptr
  );
  type Scope is
    record
      Hash_Table  : Bounded_String_To_Node_Ptr_Hashed_Map.Map;
      Up          : Scope_Ptr;
      -- This is for the benefit of CodeGen
      Children    : Scope_List.List;
    end record;

  -- START: TODO in the future
  type Array_Data_Type is new Data_Type with
    record
      Base_Type           : Data_Type_Ptr;
      Number_Of_Elements  : Positive;
    end record;
  type Array_Data_Type_Ptr is access all Array_Data_Type;

  type User_Data_Type is new Data_Type with
    record
      Variables           : Ivar_Node_List.List;
    end record;
  type User_Data_Type_Ptr is access all User_Data_Type;

  -- STOP: TODO in the future

  -------------------------------- SYMBOL TABLE ----------------------------------------------

  private
    
    Mtype_Map           : Bounded_String_To_Mtype_Byte_Hashed_Map.Map;
    Mtype_Table_Counter : Types.AST.Mtype_Byte  := Types.AST.Mtype_Byte'First;
  
    -- Built-in types; see the procedure Init_Datatypes
    Bit_Type      : aliased Types.AST.Built_In_Data_Type := ( Types.AST.Built_In_Type,  1,  1,  0,      1,        Types.AST.Bit       );
    Byte_Type     : aliased Types.AST.Built_In_Data_Type := ( Types.AST.Built_In_Type,  1,  8,  0,      255,      Types.AST.Byte      );
    Mtype_Type    : aliased Types.AST.Built_In_Data_Type := ( Types.AST.Built_In_Type,  1,  8,  0,      255,      Types.AST.Mtype     );
    Chan_Type     : aliased Types.AST.Built_In_Data_Type := ( Types.AST.Built_In_Type,  1,  8,  0,      255,      Types.AST.Chan      );
    Short_Type    : aliased Types.AST.Built_In_Data_Type := ( Types.AST.Built_In_Type,  2,  16, -2**15, 2**15-1,  Types.AST.Short     );
    -- NOTE: Reasonably assuming that pid is ultimately represented as an unsigned
    Pid_Type      : aliased Types.AST.Built_In_Data_Type := ( Types.AST.Built_In_Type,  4,  31, 0,      2**31-1,  Types.AST.Pid       );
    Int_Type      : aliased Types.AST.Built_In_Data_Type := ( Types.AST.Built_In_Type,  4,  32, -2**31, 2**31-1,  Types.AST.Int       );

end Types.AST;
