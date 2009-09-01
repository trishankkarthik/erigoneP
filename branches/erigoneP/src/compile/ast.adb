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
-- TODO:
-- 1. More, more semantic checks.

with AST.Tools;
with Logger, Utility;
with Lexer;
with Types.AST;
with Types.CodeGen;
with Types.Lexer;

package body AST is

  -- AST visitors: Every procedure henceforth is used to visit the corresponding
  -- AST node, where certain semantic checks are performed (e.g. pointer is not
  -- null, there are no conflicting variables, etc.) and the node may be decorated
  -- with some data (e.g. type, mtype "constant folding", etc.).
  procedure Module           ( A_Module_Node   : Types.AST.Module_Node_Ptr           );
  procedure Mtype            ( A_Mtype_Node    : Types.AST.Mtype_Node_Ptr            );
  procedure Utype            ( An_Utype_Node   : Types.AST.Utype_Node_Ptr            );
  procedure Decl_Lst         ( A_Decl_Lst_Node : Types.AST.Decl_Lst_Node_Ptr         );
  procedure One_Decl         ( A_One_Decl_Node : Types.AST.One_Decl_Node_Ptr         );
  procedure Typename         ( A_Typename_Node : Types.AST.Typename_Node_Ptr         );
  procedure Ivar             ( An_Ivar_Node    : Types.AST.Ivar_Node_Ptr             ;
                               A_Typename_Node : Types.AST.Typename_Node_Ptr         );
  procedure Sequence         ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr         );
  procedure Step             ( A_Step_Node     : Types.AST.Step_Node_Ptr             );
  procedure Stmt             ( A_Stmt_Node     : Types.AST.Stmt_Node_Ptr             );
  procedure Send_Args        ( A_Send_Args_Node: Types.AST.Send_Args_Node_Ptr        );
  procedure Send             ( A_Send_Node     : Types.AST.Send_Node_Ptr             );
  procedure Recv_Arg         ( A_Recv_Arg_Node : Types.AST.Recv_Arg_Node_Ptr         );
  procedure Recv_Args        ( A_Recv_Args_Node: Types.AST.Recv_Args_Node_Ptr        );
  procedure Recv             ( A_Recv_Node     : Types.AST.Recv_Node_Ptr             );
  procedure Assign           ( An_Assign_Node  : Types.AST.Assign_Node_Ptr           );
  procedure Options          ( An_Options_Node : Types.AST.Options_Node_Ptr          );
  procedure Ch_Init          ( A_Ch_Init_Ivar_Node
                                               : Types.AST.Ch_Init_Ivar_Node_Ptr     );
  procedure Poll             ( A_Poll_Node     : Types.AST.Poll_Node_Ptr             );
  procedure Proctype         ( A_Proctype_Node : Types.AST.Proctype_Node_Ptr         );
  procedure Active           ( An_Active_Node  : Types.AST.Active_Node_Ptr           );
  procedure Priority         ( A_Priority_Node : Types.AST.Priority_Node_Ptr         );
  procedure Enabler          ( An_Enabler_Node : Types.AST.Enabler_Node_Ptr          );
  procedure Any_Expr         ( An_Any_Expr_Node: in out Types.AST.Any_Expr_Node_Ptr  );
  procedure Implies          ( An_Implies_Node : Types.AST.Implies_Node_Ptr          );
  procedure Varref           ( A_Varref_Node   : Types.AST.Varref_Node_Ptr           );
  procedure Arg_Lst          ( An_Arg_Lst_Node : Types.AST.Arg_Lst_Node_Ptr          );
  procedure Const            ( A_Const_Node    : Types.AST.Const_Node_Ptr            );

  -- (MANLY) GLOBAL VARIABLES
  -- Ch_Init
  Number_Of_Channels          : Natural := 0;
  Number_Of_Process_Instances : Natural := 0;

  procedure Analyze_Any_Expr  ( An_Any_Expr_Node: in out Types.AST.Any_Expr_Node_Ptr ) is
  begin
    Logger.Open_Log_File( "AST.log" );
    Logger.Info( "LTL Formulae translation" );
    Any_Expr( An_Any_Expr_Node );
    Logger.Close_Log_File;
  end Analyze_Any_Expr;

  procedure Spec( A_Spec_Node : Types.AST.Spec_Node_Ptr ) is
    use type Types.AST.Module_Node_List.Cursor;
    use type Types.AST.Spec_Node_Ptr;
    Next : Types.AST.Module_Node_List.Cursor;
    A_Module_Node : Types.AST.Module_Node_Ptr;
  begin
    Logger.Open_Log_File("AST.log");
    Logger.Set_Level_Threshold( Logger.Info_Level );
    AST.Tools.Semantic_Assert( A_Spec_Node /= null, "A_Spec_Node is null!" );
    AST.Tools.Init_Globals;
    Logger.Info( "Walking a Spec Node..." );
    Next := A_Spec_Node.Module_List.First;
    AST.Tools.Scope_Push;
    while Next /= Types.AST.Module_Node_List.No_Element loop
      A_Module_Node := Types.AST.Module_Node_List.Element( Next );
      Module( A_Module_Node );
      Types.AST.Module_Node_List.Next( Next );
    end loop;
    --AST.Tools.Scope_Pop;
    Logger.Info("SUCCESS! Semantically analyzed.");
    Logger.Close_Log_File;
  exception
    when others =>
      Logger.Info("FAILURE! Tripped on a semantic error.");
      Logger.Close_Log_File;
      raise;
  end Spec;

  procedure Module( A_Module_Node : Types.AST.Module_Node_Ptr ) is
    use type Types.AST.Module_Node_Ptr;
  begin
    AST.Tools.Semantic_Assert(  A_Module_Node /= null, "A_Module_Node is null!" );
    Logger.Info( "Walking a Module..." );

    case A_Module_Node.Tag is

      when Types.AST.Node_Decl_Lst_Module =>
        Logger.Info( "Decl_Lst Module" );
        Decl_Lst( Types.AST.Decl_Lst_Module_Node_Ptr(A_Module_Node).Decl_Lst );
      when Types.AST.Node_Init_Module     =>
        Logger.Info( "Init Module" );
        Proctype( Types.AST.Init_Module_Node_Ptr(A_Module_Node).Pinit );
      when Types.AST.Node_Mtype_Module    =>
        Logger.Info( "Mtype Module" );
        Mtype( Types.AST.Mtype_Module_Node_Ptr(A_Module_Node).Mtype );
      when Types.AST.Node_Never_Module    =>
        Logger.Info( "Never Module" );
        AST.Tools.Raise_Unsupported_Construct( "never modules" );
      when Types.AST.Node_Trace_Module    =>
        Logger.Info( "Trace Module" );
        AST.Tools.Raise_Unsupported_Construct( "trace modules" );
      when Types.AST.Node_Proctype_Module =>
        Logger.Info( "Proctype Module" );
        Proctype( Types.AST.Proctype_Module_Node_Ptr(A_Module_Node).Proctype );
      when Types.AST.Node_Utype_Module    =>
        -- everything you need to know about a user type is in its Utype_Node
        Logger.Info( "Utype Module" );
        AST.Tools.Raise_Unsupported_Construct( "user-defined types" );
        Utype( Types.AST.Utype_Module_Node_Ptr(A_Module_Node).Utype );
      when others                         =>
        AST.Tools.Semantic_Assert( False, "Unrecognized A_Module_Node!" );

    end case;

  end Module;

  -- Now Mtype bytes are in the Mtype_Node itself!
  procedure Mtype( A_Mtype_Node : Types.AST.Mtype_Node_Ptr ) is
    use type Types.AST.Module_Node_Ptr;
    use type Types.AST.Mtype_Node_Ptr;
    use type Types.AST.Bounded_String_List.Cursor;
    Previous  : Types.AST.Bounded_String_List.Cursor;
    Name      : Types.Lexer.Bound_String.Bounded_String;
  begin

    AST.Tools.Semantic_Assert( A_Mtype_Node /= null, "A_Mtype_Node is null!" );
    Logger.Info( "Walking a Mtype.. " );
    -- add mtype name one by one to Mtype_Table
    Previous := A_Mtype_Node.Names_List.Last;
    while Previous /= Types.AST.Bounded_String_List.No_Element loop
      Name := Types.AST.Bounded_String_List.Element( Previous );
      Logger.Info(  "Adding "
                    & Types.Lexer.Bound_String.To_String( Name )
                    & " to Mtype_Map." );
      -- Link name to its byte in A_Mtype_Node.Names_Map
      Types.AST.Add_Mtype( A_Mtype_Node, Name );
      -- Link name to Mtype_Node in current (global) scope 
      Logger.Info(  "SCOPE: Putting '"
                    & Types.Lexer.Bound_String.To_String( Name )
                    & "' into the current scope" );
      AST.Tools.Semantic_Assert(  AST.Tools.Scope_Put( Name, Types.AST.Node_Ptr(A_Mtype_Node) ) = True,
                                  "Redefining existing variable '"
                                  & Types.Lexer.Bound_String.To_String( Name )
                                  & "' in this scope!"  );
      Types.AST.Bounded_String_List.Previous( Previous );
    end loop;

  exception

    when  Types.AST.Mtype_Full_Exception      |
          Types.AST.Mtype_Redundant_Exception =>
      Logger.Error( "Ran into a problem with adding a Mtype constant! See exception." );
      raise;

  end Mtype;
  
  procedure Utype            ( An_Utype_Node   : Types.AST.Utype_Node_Ptr            ) is
    use type Types.AST.Utype_Node_Ptr;
    Name : Types.Lexer.Bound_String.Bounded_String;
  begin
    AST.Tools.Semantic_Assert( An_Utype_Node /= null, "An_Utype_Node is null!" );
    Logger.Info( "Walking an Utype.. " );
    Name := Types.Lexer.Element_In_Table( An_Utype_Node.Name );
    Logger.Info( "Name: " & Types.Lexer.Bound_String.To_String( Name ) );
    -- NOTE: Put utype name into current (global) scope 
    Logger.Info( "SCOPE: Putting '" & Types.Lexer.Bound_String.To_String( Name ) & "' into the current scope" );
    AST.Tools.Semantic_Assert( AST.Tools.Scope_Put( Name, Types.AST.Node_Ptr(An_Utype_Node) ) = True,
                           "Redefining existing variable '" & Types.Lexer.Bound_String.To_String( Name ) & "' in this scope!" );
    AST.Tools.Scope_Push;
    Decl_Lst( An_Utype_Node.Decl_Lst );
    AST.Tools.Scope_Pop;
    -- aha, let Decl_Lst figure out the damned size, variables
    Logger.Info( "Size in bytes: " & Positive'Image( An_Utype_Node.Decl_Lst.Size_In_Bytes ) );
  end Utype;

  procedure Decl_Lst( A_Decl_Lst_Node : Types.AST.Decl_Lst_Node_Ptr ) is
    use type Types.AST.Ivar_Node_List.Cursor;
    use type Types.AST.One_Decl_Node_List.Cursor;
    use type Types.AST.Decl_Lst_Node_Ptr;
    Next            : Types.AST.One_Decl_Node_List.Cursor;
    Next_Ivar       : Types.AST.Ivar_Node_List.Cursor;
    A_One_Decl_Node : Types.AST.One_Decl_Node_Ptr;
    An_Ivar_Node    : Types.AST.Ivar_Node_Ptr;
  begin
    AST.Tools.Semantic_Assert( A_Decl_Lst_Node /= null, "A_Decl_Lst_Node is null!" );
    Logger.Info( "Walking a Decl_Lst..." );
    Next := A_Decl_Lst_Node.One_Decl_List.First;
    while Next /= Types.AST.One_Decl_Node_List.No_Element loop
      A_One_Decl_Node := Types.AST.One_Decl_Node_List.Element( Next );
      One_Decl( A_One_Decl_Node );
      -- now collect all variables in order from one_decl for decl_lst
      -- yes, we're walking one_decl again.
      Next_Ivar := A_One_Decl_Node.Variable_List.First;
      while Next_Ivar /= Types.AST.Ivar_Node_List.No_Element loop
        An_Ivar_Node := Types.AST.Ivar_Node_List.Element(Next_Ivar);
        A_Decl_Lst_Node.Total_Variable_List.Append( An_Ivar_Node );
        Types.AST.Ivar_Node_List.Next( Next_Ivar );
      end loop;
      A_Decl_Lst_Node.Size_In_Bytes := A_Decl_Lst_Node.Size_In_Bytes + A_One_Decl_Node.Size_In_Bytes;
      Types.AST.One_Decl_Node_List.Next( Next );
    end loop;
  end Decl_Lst;

  procedure One_Decl( A_One_Decl_Node : Types.AST.One_Decl_Node_Ptr ) is
    use type Types.AST.Ivar_Node_List.Cursor;
    use type Types.AST.One_Decl_Node_Ptr;
    Next                : Types.AST.Ivar_Node_List.Cursor;
    An_Ivar_Node        : Types.AST.Ivar_Node_Ptr;
  begin
    AST.Tools.Semantic_Assert( A_One_Decl_Node /= null, "A_One_Decl_Node is null!" );
    Logger.Info( "Walking a One_Decl..." );
    Logger.Info( "Visibility: " & Types.Lexer.Token_Type'Image( A_One_Decl_Node.Visibility ) );
    Typename( A_One_Decl_Node.Typename );
    Next := A_One_Decl_Node.Variable_List.First;
    while Next /= Types.AST.Ivar_Node_List.No_Element loop
      An_Ivar_Node := Types.AST.Ivar_Node_List.Element( Next );
      Ivar( An_Ivar_Node, A_One_Decl_Node.Typename );
      A_One_Decl_Node.Size_In_Bytes := A_One_Decl_Node.Size_In_Bytes + An_Ivar_Node.The_Type.Size_In_Bytes;
      Types.AST.Ivar_Node_List.Next( Next );
    end loop;
  end One_Decl;

  procedure Typename( A_Typename_Node : Types.AST.Typename_Node_Ptr ) is
    use type Types.AST.Typename_Node_Ptr;
    A_Predefined_Typename_Node  : Types.AST.Predefined_Typename_Node_Ptr;
    Name                        : Types.Lexer.Bound_String.Bounded_String;
  begin
    AST.Tools.Semantic_Assert(  A_Typename_Node /= null, "A_Typename_Node is null!" );
    Logger.Info( "Walking a Typename..." );

    case A_Typename_Node.Tag is

      when Types.AST.Node_Predefined_Typename =>
        A_Predefined_Typename_Node := Types.AST.Predefined_Typename_Node_Ptr( A_Typename_Node );
        Logger.Info( "Name: " & Types.Lexer.Token_Type'Image( A_Predefined_Typename_Node.Name ) );
        Logger.Info( "Pointing to a built-in Data_Type" );
        case A_Predefined_Typename_Node.Name is

          when Types.Lexer.Token_Bit      =>
            A_Predefined_Typename_Node.The_Type := Types.AST.Bit_Data_Type;
          when Types.Lexer.Token_Bool     =>
            A_Predefined_Typename_Node.The_Type := Types.AST.Bit_Data_Type;
          when Types.Lexer.Token_Byte     =>
            A_Predefined_Typename_Node.The_Type := Types.AST.Byte_Data_Type;
          when Types.Lexer.Token_Chan     =>
            A_Predefined_Typename_Node.The_Type := Types.AST.Chan_Data_Type;
          when Types.Lexer.Token_Mtype    =>
            A_Predefined_Typename_Node.The_Type := Types.AST.Mtype_Data_Type;
          when Types.Lexer.Token_Pid      =>
            AST.Tools.Raise_Unsupported_Construct( "pid data type" );
            --A_Predefined_Typename_Node.The_Type := Types.AST.Pid_Data_Type;
          when Types.Lexer.Token_Short    =>
            A_Predefined_Typename_Node.The_Type := Types.AST.Short_Data_Type;
          when Types.Lexer.Token_Int      =>
            A_Predefined_Typename_Node.The_Type := Types.AST.Int_Data_Type;
          when Types.Lexer.Token_Unsigned =>
            AST.Tools.Raise_Unsupported_Construct( "unsigned variables" );
          when others                     =>
            AST.Tools.Semantic_Assert( False, "Unrecognized primitive data type in A_Typename_Node!" );

        end case;
      
      when Types.AST.Node_Uname_Typename      =>

        AST.Tools.Raise_Unsupported_Construct( "user-defined types" );
        --An_Uname_Typename_Node := Uname_Types.AST.Typename_Node_Ptr( A_Typename_Node );
        --AST.Tools.Semantic_Assert( An_Uname_Typename_Node.Uname.Name /= Types.Lexer.Bounded_String_Hashed_Set.No_Element, "A_Uname_Typename_Node.Uname is No_Element!" );
        --Name := Types.Types.Lexer.Element_In_Table( An_Uname_Typename_Node.Uname.Name );
        --Logger.Info( "Creating a new user data type for Uname: " & Types.Lexer.Bound_String.To_String( Name ) );
        --A_User_Data_Type := new User_Data_Type;
        --A_Node := AST.Tools.Scope_Get( Name );
        --AST.Tools.Semantic_Assert(  A_Node /= null and then A_Node.all in Utype_Node,
        --                       "Utype '" &
        --                       Types.Lexer.Bound_String.To_String( Name )
        --                       & "' not found in this scope!" );
        --Logger.Info( "Found Utype declaration" );
        --An_Utype_Node := Types.AST.Utype_Node_Ptr( A_Node );
        --A_User_Data_Type.Variables := An_Utype_Node.Decl_Lst.Total_Variable_List;
        -- The idea is that composite Utypes can refer to this component Utype's size
        --A_User_Data_Type.Size_In_Bits := An_Utype_Node.Decl_Lst.Size_In_Bits;
        --An_Uname_Typename_Node.The_Type := Types.AST.Data_Type_Ptr( A_User_Data_Type );

      when others                             =>
        AST.Tools.Semantic_Assert( False, "Unrecognized A_Typename_Node!" );

    end case;

  end Typename;

  -- TODO: take care of utypes, pid, unsigned 
  procedure Ivar( An_Ivar_Node    : Types.AST.Ivar_Node_Ptr     ;
                  A_Typename_Node : Types.AST.Typename_Node_Ptr ) is
    use type Types.Lexer.Promela_Natural;
    use type Types.AST.Node_Tag;
    use type Types.AST.Typename_Node_Ptr;
    use type Types.AST.Ivar_Node_Ptr;
    use type Types.AST.Const_Node_Ptr;
    use type Types.AST.Data_Type_Ptr;
    An_Any_Expr_Ivar_Node : Types.AST.Any_Expr_Ivar_Node_Ptr;
    A_Ch_Init_Ivar_Node   : Types.AST.Ch_Init_Ivar_Node_Ptr;
    Name                  : Types.Lexer.Bound_String.Bounded_String;
    Number                : Types.Lexer.Promela_Natural;
    An_Array              : Types.AST.Array_Data_Type_Ptr;
  begin

    AST.Tools.Semantic_Assert(  An_Ivar_Node /= null, "An_Ivar_Node is null!" );
    AST.Tools.Semantic_Assert(  A_Typename_Node /= null, "A_Typename_Node is null!" );
    Logger.Info( "Walking an Ivar..." );
    Name := Types.Lexer.Element_In_Table( An_Ivar_Node.Name );
    Logger.Info( "Name: " & Types.Lexer.Bound_String.To_String( Name ) );
    -- Put Ivar into current scope 
    AST.Tools.Semantic_Assert(  AST.Tools.Is_There_Global( Name ) = False,
                      "There is already a global variable with the name '" &
                      Types.Lexer.Bound_String.To_String(Name) & "'!" );
    Logger.Info( "SCOPE: Putting '" & Types.Lexer.Bound_String.To_String( Name ) & "' into the current scope" );
    AST.Tools.Semantic_Assert(  AST.Tools.Scope_Put( Name, Types.AST.Node_Ptr(An_Ivar_Node) ) = True,
                      "Redefining existing variable '"
                      & Types.Lexer.Bound_String.To_String( Name )
                      & "' in this scope!" );

    -- Pointing Ivar_Node to its datatype
    -- Take care of anything but utypes
    case A_Typename_Node.Tag is

      when Types.AST.Node_Predefined_Typename =>

        if An_Ivar_Node.Array_Length /= null then

          Logger.Info( "Setting built-in array data type with information from A_Typename_Node" );
          An_Array                    := new Types.AST.Array_Data_Type;
          An_Array.Kind               := Types.AST.Array_Type;
          -- This is fine because we have ensured that the base type is a predefined type
          An_Array.Base_Type          := A_Typename_Node.The_Type;
          -- get array length
          Const( An_Ivar_Node.Array_Length );
          Number                      := Types.Lexer.Element_In_Table( An_Ivar_Node.Array_Length.Number );
          AST.Tools.Semantic_Assert( Number > 0, "Array length is zero!" );
          -- arbitrary check, really, considering the miniscule size of Types.CodeGen.Byte
          AST.Tools.Semantic_Assert(  Number <= ( Types.Lexer.Promela_Natural( Types.CodeGen.Byte'Last ) ),
                            "Array length exceeds our present scope for the model checker's memory!" );
          An_Array.Number_Of_Elements := Positive( Number );
          -- array size in bytes = num of elements * size in byte of each element
          Logger.Info( "Byte Offset = " & Positive'Image( An_Array.Base_Type.Size_In_Bytes ) );
          An_Array.Size_In_Bytes      := An_Array.Number_Of_Elements * An_Array.Base_Type.Size_In_Bytes;
          An_Ivar_Node.The_Type       := Types.AST.Data_Type_Ptr( An_Array );

        else
          Logger.Info( "Setting built-in data type with information from A_Typename_Node" );
          An_Ivar_Node.The_Type       := A_Typename_Node.The_Type;
        end if;

        case An_Ivar_Node.Tag is

          when  Types.AST.Node_Any_Expr_Ivar  =>
            Logger.Info( "Walking into Ivar = Any_Expr..." );
            An_Any_Expr_Ivar_Node       := Types.AST.Any_Expr_Ivar_Node_Ptr( An_Ivar_Node );
            Any_Expr( An_Any_Expr_Ivar_Node.Assignment );

          when  Types.AST.Node_Ch_Init_Ivar   =>
            Logger.Info( "Walking into Ivar = Ch_Init..." );
            A_Ch_Init_Ivar_Node := Types.AST.Ch_Init_Ivar_Node_Ptr( An_Ivar_Node );
            Ch_Init( A_Ch_Init_Ivar_Node );

          when  Types.AST.Node_Ivar           =>
            null;

          when  others                        =>
            AST.Tools.Semantic_Assert( False, "Unrecognized An_Ivar_Node!" );

        end case;

      when Types.AST.Node_Uname_Typename      =>
        AST.Tools.Semantic_Assert( An_Ivar_Node.Tag /= Types.AST.Node_Any_Expr_Ivar,  "Utype variable cannot have an initial expression!" );
        AST.Tools.Semantic_Assert( An_Ivar_Node.Tag /= Types.AST.Node_Ch_Init_Ivar,   "Utype variable cannot have a channel initializer!" );
        AST.Tools.Raise_Unsupported_Construct( "user-defined types" );

      when others                             =>
        AST.Tools.Semantic_Assert( False, "Unrecognized A_Typename_Node!" );

    end case;

    AST.Tools.Semantic_Assert( An_Ivar_Node.The_Type /= null, "An_Ivar_Node.The_Type is null!" );
    -- Size in bytes
    Logger.Info( "Size in bytes: " & Positive'Image( An_Ivar_Node.The_Type.Size_In_Bytes ) );

  end Ivar;

  -- COMMENTARY: My god, SPIN does take channel buffer size seriously...
  -- ...a large enough size really spins the computer...try it!
  procedure Ch_Init  ( A_Ch_Init_Ivar_Node   : Types.AST.Ch_Init_Ivar_Node_Ptr       ) is
    use type Types.Lexer.Promela_Natural;
    use type Types.AST.Node_Tag;
    use type Types.Lexer.Token_Type;
    use type Types.AST.Typename_Node_List.Cursor;
    use type Types.AST.Ch_Init_Ivar_Node_Ptr;
    use type Types.AST.Const_Node_Ptr;
    A_Ch_Init_Node              : Types.AST.Ch_Init_Node_Ptr;
    Next                        : Types.AST.Typename_Node_List.Cursor;
    A_Typename_Node             : Types.AST.Typename_Node_Ptr;
    A_Predefined_Typename_Node  : Types.AST.Predefined_Typename_Node_Ptr;
    An_Array                    : Types.AST.Array_Data_Type_Ptr;
    Sum                         : Natural := 0;
  begin

    AST.Tools.Semantic_Assert( A_Ch_Init_Ivar_Node /= null, "A_Ch_Init_Ivar_Node is null!" );
    A_Ch_Init_Node              := A_Ch_Init_Ivar_Node.Assignment;
    Logger.Info( "Walking a Ch_Init..." );
    AST.Tools.Semantic_Assert( A_Ch_Init_Node.Size /= null, "A_Ch_Init_Node.Size is null!" );
    A_Ch_Init_Node.Buffer_Size  := Types.Lexer.Element_In_Table( A_Ch_Init_Node.Size.Number );
    Logger.Info( "Size: " & Types.Lexer.Promela_Natural'Image( A_Ch_Init_Node.Buffer_Size ) );
    AST.Tools.Semantic_Assert( A_Ch_Init_Node.Typename_List.Is_Empty = False, "A_Ch_Init_Node.Typename_List is empty!" );
    Logger.Info( "Walking into Typename_List..." );

    Next := A_Ch_Init_Node.Typename_List.First;
    while Next /= Types.AST.Typename_Node_List.No_Element loop
      A_Typename_Node := Types.AST.Typename_Node_List.Element( Next );
      -- let Typename take care of fields, sizes 
      Typename( A_Typename_Node );
      -- TODO: A suboptimal way to do this until we produce an unsigned data type for faster recognition
      if A_Typename_Node.Tag = Types.AST.Node_Predefined_Typename then
        A_Predefined_Typename_Node := Types.AST.Predefined_Typename_Node_Ptr( A_Typename_Node );
        AST.Tools.Semantic_Assert(  A_Predefined_Typename_Node.Name /= Types.Lexer.Token_Unsigned,
                          "Unsigned type is not allowed inside channel message fields!" );
      end if;
      Sum := Sum + A_Typename_Node.The_Type.Size_In_Bytes;
      Types.AST.Typename_Node_List.Next( Next );
    end loop;

    -- Array size consideration, which is the only reason why the input of this procedure is atypical
    -- Assume type checking (i.e. only channel variables) has been done before Ch_Init is called
    case A_Ch_Init_Ivar_Node.The_Type.Kind is

     when   Types.AST.Array_Type    => 
        An_Array                  := Types.AST.Array_Data_Type_Ptr( A_Ch_Init_Ivar_Node.The_Type );
        A_Ch_Init_Node.Array_Size := An_Array.Number_Of_Elements;

      when  Types.AST.Built_In_Type =>
        A_Ch_Init_Node.Array_Size := 1;

      when  Types.AST.User_Type     =>
        AST.Tools.Semantic_Assert( False, "Internal inconsistency! User-defined typed variables cannot have channel initializers!" );

    end case;

    if A_Ch_Init_Node.Buffer_Size > 0 then
      Number_Of_Channels := Number_Of_Channels + A_Ch_Init_Node.Array_Size;
      -- Section 7.1, PSMC, MBA
      AST.Tools.Semantic_Assert( Number_Of_Channels < 256, "SPIN does not allow more than 255 channels!" );
    end if;

    A_Ch_Init_Node.Types_Size   := Sum;
    -- The rest of the sizes have no meaning if Buffer_Size = 0
    if A_Ch_Init_Node.Buffer_Size > 0 then
      A_Ch_Init_Node.Channel_Size   := Natural( A_Ch_Init_Node.Buffer_Size ) * Sum;
      -- The Erigone interpreter wishes 1 extra byte in the state vector
      -- in order to store the number of data in the channel.
      A_Ch_Init_Node.Channel_Size   := A_Ch_Init_Node.Channel_Size + 1;
      A_Ch_Init_Node.Size_In_Bytes  := A_Ch_Init_Node.Channel_Size * A_Ch_Init_Node.Array_Size;
      AST.Tools.Semantic_Assert(  A_Ch_Init_Node.Size_In_Bytes <= 256,
                                  "Size of a channel is beyond our present scope for the model checker's memory!" );
    end if;

  end Ch_Init;

  procedure Walk_Step_Node_List( A_Step_Node_List : Types.AST.Step_Node_List.List )  is
    use type Types.AST.Step_Node_List.Cursor;
    Next        : Types.AST.Step_Node_List.Cursor;
    A_Step_Node : Types.AST.Step_Node_Ptr;
  begin
    Logger.Info( "Walking a Step_Node_List" );
    Next := A_Step_Node_List.First;
    while Next /= Types.AST.Step_Node_List.No_Element loop
      A_Step_Node := Types.AST.Step_Node_List.Element( Next );
      Step( A_Step_Node );
      Types.AST.Step_Node_List.Next( Next );
    end loop;
  end Walk_Step_Node_List;

  procedure Sequence         ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr         ) is
    use type Types.AST.Sequence_Node_Ptr;
  begin
    AST.Tools.Semantic_Assert( A_Sequence_Node /= null, "A_Sequence_Node is null!" );
    Logger.Info( "Walking a Sequence" );
    Logger.Info( "Walking a Sequence.Decl_Lst_Step_List" );
    Walk_Step_Node_List( A_Sequence_Node.Decl_Lst_Step_List );
    Logger.Info( "Walking a Sequence.Xr_Xs_Step_List" );
    Walk_Step_Node_List( A_Sequence_Node.Xr_Xs_Step_List );
    Logger.Info( "Walking a Sequence.Stmt_Step_List" );
    Walk_Step_Node_List( A_Sequence_Node.Stmt_Step_List );
  end Sequence;

  procedure Step             ( A_Step_Node     : Types.AST.Step_Node_Ptr             ) is
    use type Types.AST.Stmt_Node_Ptr;
    use type Types.AST.Step_Node_Ptr;
    A_Stmt_Step_Node      : Types.AST.Stmt_Step_Node_Ptr;
    A_Decl_Lst_Step_Node  : Types.AST.Decl_Lst_Step_Node_Ptr;
    A_Xr_Xs_Step_Node     : Types.AST.Xr_Xs_Step_Node_Ptr;
  begin
    AST.Tools.Semantic_Assert(  A_Step_Node /= null, "A_Step_Node is null!" );
    Logger.Info( "Walking a Step..." );

    case A_Step_Node.Tag is

      when Types.AST.Node_Stmt_Step     =>
        Logger.Info( "Walking a Stmt_Step..." );
        A_Stmt_Step_Node := Types.AST.Stmt_Step_Node_Ptr( A_Step_Node );
        Stmt( A_Stmt_Step_Node.Statement );
        if A_Stmt_Step_Node.Unless /= null then
          Logger.Info( "Walking an UNLESS Statement..." );
          AST.Tools.Raise_Unsupported_Construct( "unless statements" );
          Stmt( A_Stmt_Step_Node.Unless );
        end if;
      when Types.AST.Node_Decl_Lst_Step =>
        Logger.Info( "Walking a Decl_Lst_Step..." );
        A_Decl_Lst_Step_Node := Types.AST.Decl_Lst_Step_Node_Ptr( A_Step_Node );
        -- NOTE: do not push/pop scope here; see the Proctype procedure
        Decl_Lst( A_Decl_Lst_Step_Node.Decl_Lst );
      when Types.AST.Node_Xr_Xs_Step    =>
        Logger.Info( "Walking a Xr_Xs_Step" );
        AST.Tools.Raise_Unsupported_Construct( "channel assertions" );
        A_Xr_Xs_Step_Node := Types.AST.Xr_Xs_Step_Node_Ptr( A_Step_Node );
      when others                       =>
        AST.Tools.Semantic_Assert( False, "Unrecognized A_Step_Node!" );

    end case;

  end Step;

  -- TODO: need analysis for Sequence-s with GOTO Statement-s
  procedure Stmt             ( A_Stmt_Node     : Types.AST.Stmt_Node_Ptr             ) is
    use type Types.AST.Data_Type_Kind;
    use type Types.AST.Built_In_Data_Type_Kind;
    use type Types.AST.Node_Ptr;
    use type Types.AST.Stmt_Node_Ptr;
    An_Any_Expr_Stmt_Node : Types.AST.Any_Expr_Stmt_Node_Ptr;
    An_Assert_Stmt_Node   : Types.AST.Assert_Stmt_Node_Ptr;
    An_Assign_Stmt_Node   : Types.AST.Assign_Stmt_Node_Ptr;
    An_Atomic_Stmt_Node   : Types.AST.Atomic_Stmt_Node_Ptr;
    A_Do_Stmt_Node        : Types.AST.Do_Stmt_Node_Ptr;
    A_D_Step_Stmt_Node    : Types.AST.D_Step_Stmt_Node_Ptr;
    An_If_Stmt_Node       : Types.AST.If_Stmt_Node_Ptr;
    A_Name_Stmt_Node      : Types.AST.Name_Stmt_Node_Ptr;
    A_Printf_Stmt_Node    : Types.AST.Printf_Stmt_Node_Ptr;
    A_Printm_Stmt_Node    : Types.AST.Printm_Stmt_Node_Ptr;
    A_Recv_Stmt_Node      : Types.AST.Recv_Stmt_Node_Ptr;
    A_Send_Stmt_Node      : Types.AST.Send_Stmt_Node_Ptr;
    A_Sequence_Stmt_Node  : Types.AST.Sequence_Stmt_Node_Ptr;
    Declaration           : Types.AST.Node_Ptr;
    An_Ivar_Node          : Types.AST.Ivar_Node_Ptr;
    Built_In              : Types.AST.Built_In_Data_Type_Ptr;
    -- For labeled, printm statements
    A_Stmt_Step_Node      : Types.AST.Stmt_Step_Node_Ptr;
    A_Step_Node           : Types.AST.Step_Node_Ptr;
    Is_Mtype_Constant_Or_Variable
                          : Boolean := False;
    Name                  : Types.Lexer.Bound_String.Bounded_String;
  begin
    AST.Tools.Semantic_Assert(  A_Stmt_Node /= null, "A_Stmt_Node is null!" );
    Logger.Info( "Walking a Stmt..." );

    case A_Stmt_Node.Tag is

      when Types.AST.Node_Any_Expr_Stmt   =>
        An_Any_Expr_Stmt_Node := Types.AST.Any_Expr_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking an Any_Expr_Stmt_Node" );
        Any_Expr( An_Any_Expr_Stmt_Node.Expression );

      when Types.AST.Node_Assign_Stmt     =>
        An_Assign_Stmt_Node := Types.AST.Assign_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking an Assign_Stmt_Node" );
        Assign( An_Assign_Stmt_Node.Assignment );

      when Types.AST.Node_Assert_Stmt     =>
        An_Assert_Stmt_Node := Types.AST.Assert_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking an Assert_Stmt_Node" );
        Any_Expr( An_Assert_Stmt_Node.Expression );

      when Types.AST.Node_Atomic_Stmt     =>
        An_Atomic_Stmt_Node := Types.AST.Atomic_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking an Atomic_Stmt_Node" );
        Sequence( An_Atomic_Stmt_Node.Sequence );

      when Types.AST.Node_Break_Stmt       =>
        Logger.Info( "Walking a Break_Stmt_Node" );

      when  Types.AST.Node_C_Code_Stmt    |
            Types.AST.Node_C_Expr_Stmt    |
            Types.AST.Node_C_Decl_Stmt    |
            Types.AST.Node_C_Track_Stmt   |
            Types.AST.Node_C_State_Stmt   =>
        AST.Tools.Raise_Unsupported_Construct( "C code" );

      when Types.AST.Node_Do_Stmt         =>
        A_Do_Stmt_Node := Types.AST.Do_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking a Do_Stmt_Node" );
        Options( A_Do_Stmt_Node.Options );

      when Types.AST.Node_D_Step_Stmt     =>
        A_D_Step_Stmt_Node := Types.AST.D_Step_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking a D_Step_Stmt_Node" );
        Sequence( A_D_Step_Stmt_Node.Sequence );

      when Types.AST.Node_Else_Stmt       =>
        Logger.Info( "Walking an Else_Stmt_Node" );

      when Types.AST.Node_Goto_Stmt       =>
        Logger.Info( "Walking a Goto_Stmt_Node" );

      when Types.AST.Node_If_Stmt         =>
        An_If_Stmt_Node := Types.AST.If_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking an If_Stmt_Node" );
        Options( An_If_Stmt_Node.Options );

      when Types.AST.Node_Name_Stmt       =>
        A_Name_Stmt_Node := Types.AST.Name_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking an Name_Stmt_Node" );
        -- NOTE: disguise Stmt as Sequence so that we may process it properly
        -- without code duplication in CodeGen
        A_Name_Stmt_Node.Sequence     := new Types.AST.Sequence_Node;
        A_Name_Stmt_Node.Sequence.Tag := Types.AST.Node_Sequence;
        A_Stmt_Step_Node              := new Types.AST.Stmt_Step_Node;
        A_Stmt_Step_Node.Tag          := Types.AST.Node_Stmt_Step;
        A_Stmt_Step_Node.Statement    := A_Name_Stmt_Node.Statement;
        --Stmt( A_Name_Stmt_Node.Statement );
        A_Step_Node                   := Types.AST.Step_Node_Ptr( A_Stmt_Step_Node );
        A_Name_Stmt_Node.Sequence.Stmt_Step_List.Append( A_Step_Node );
        Sequence( A_Name_Stmt_Node.Sequence );

      when Types.AST.Node_Printf_Stmt     =>
        A_Printf_Stmt_Node := Types.AST.Printf_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking a Printf_Stmt_Node" );
        A_Printf_Stmt_Node := Types.AST.Printf_Stmt_Node_Ptr( A_Stmt_Node );
        -- TODO: compile-time parameter checking
        Arg_Lst( A_Printf_Stmt_Node.Arguments );

      when Types.AST.Node_Printm_Stmt     =>
        A_Printm_Stmt_Node := Types.AST.Printm_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking a Printm_Stmt_Node" );
        Name := Types.Lexer.Element_In_Table( A_Printm_Stmt_Node.Name );

        Declaration := AST.Tools.Scope_Get( Types.Lexer.Element_In_Table( A_Printm_Stmt_Node.Name ) );
        AST.Tools.Semantic_Assert( Declaration /= null, "Mtype constant or variable '" & Types.Lexer.Bound_String.To_String( Name ) & "' not found in this scope!" );
        case Declaration.Tag is

          when  Types.AST.Node_Ivar   =>
            An_Ivar_Node := Types.AST.Ivar_Node_Ptr( Declaration );
            if An_Ivar_Node.The_Type.Kind = Types.AST.Built_In_Type then
              Built_In := Types.AST.Built_In_Data_Type_Ptr( An_Ivar_Node.The_Type );
              if Built_In.Built_In_Kind = Types.AST.Mtype then
                Is_Mtype_Constant_Or_Variable := True;
              end if;
            end if;

          when  Types.AST.Node_Mtype  =>
            Is_Mtype_Constant_Or_Variable := True;

          when  others                =>
            -- no harm
            Is_Mtype_Constant_Or_Variable := False;

        end case;
        AST.Tools.Semantic_Assert( Is_Mtype_Constant_Or_Variable = True, "Mtype constant or variable '" & Types.Lexer.Bound_String.To_String( Name ) & "' not found in this scope!" );

        Logger.Info( "Found type declaration" );
        A_Printm_Stmt_Node.Declaration := Declaration;

      when Types.AST.Node_Recv_Stmt       =>
        Logger.Info( "Walking a Recv_Stmt_Node" );
        A_Recv_Stmt_Node  := Types.AST.Recv_Stmt_Node_Ptr( A_Stmt_Node );
        Recv( A_Recv_Stmt_Node.Receive );

      when Types.AST.Node_Send_Stmt       =>
        Logger.Info( "Walking a Send_Stmt_Node" );
        A_Send_Stmt_Node  := Types.AST.Send_Stmt_Node_Ptr( A_Stmt_Node );
        Send( A_Send_Stmt_Node.Send );

      when Types.AST.Node_Sequence_Stmt   =>
        A_Sequence_Stmt_Node := Types.AST.Sequence_Stmt_Node_Ptr( A_Stmt_Node );
        Logger.Info( "Walking a Sequence_Stmt_Node" );
        -- TODO: spin complains when a Stmt => { Sequence } has nothing but Decl_Lst-s
        Sequence( A_Sequence_Stmt_Node.Sequence );

      when others                         =>
        AST.Tools.Semantic_Assert( False, "Unrecognized A_Stmt_Node!" );

    end case;

  end Stmt;

  procedure Send_Args( A_Send_Args_Node : Types.AST.Send_Args_Node_Ptr ) is
    use type Types.AST.Send_Args_Node_Ptr;
  begin

    AST.Tools.Semantic_Assert( A_Send_Args_Node /= null, "A_Send_Args_Node is null!" );
    Logger.Info( "Walking a Send_Args" );
    -- Grammar guarantees arguments won't be empty
    Arg_Lst( A_Send_Args_Node.Arguments );

  end Send_Args;

  procedure Send( A_Send_Node : Types.AST.Send_Node_Ptr ) is
    use type Types.AST.Send_Node_Ptr;
  begin

    AST.Tools.Semantic_Assert( A_Send_Node /= null, "A_Send_Node is null!" );
    Logger.Info( "Walking a Send" );

    Varref( A_Send_Node.Source );
    AST.Tools.Semantic_Assert( AST.Tools.Is_A_Channel_Variable( A_Send_Node.Source ) = True, "Source of a send statement is not a channel variable!" );
    Send_Args( A_Send_Node.Arguments );

    case A_Send_Node.Tag is
      when  Types.AST.Node_Fifo_Send    |
            Types.AST.Node_Sorted_Send  =>
        null;
      when others                       =>
        AST.Tools.Semantic_Assert( False, "Unrecognized A_Send_Node!" );
    end case;

  end Send;

  procedure Recv_Arg ( A_Recv_Arg_Node : Types.AST.Recv_Arg_Node_Ptr ) is
    use type Types.AST.Recv_Arg_Node_Ptr;
    A_Varref_Recv_Arg_Node  : Types.AST.Varref_Recv_Arg_Node_Ptr;
    An_Eval_Recv_Arg_Node   : Types.AST.Eval_Recv_Arg_Node_Ptr;
    A_Const_Recv_Arg_Node   : Types.AST.Const_Recv_Arg_Node_Ptr;
  begin

    AST.Tools.Semantic_Assert( A_Recv_Arg_Node /= null, "A_Recv_Arg_Node is null!" );
    Logger.Info( "Walking a Recv_Arg" );

    case A_Recv_Arg_Node.Tag is

      when Types.AST.Node_Varref_Recv_Arg =>
        A_Varref_Recv_Arg_Node := Types.AST.Varref_Recv_Arg_Node_Ptr( A_Recv_Arg_Node );
        Varref( A_Varref_Recv_Arg_Node.Variable );

      when Types.AST.Node_Eval_Recv_Arg   =>
        An_Eval_Recv_Arg_Node := Types.AST.Eval_Recv_Arg_Node_Ptr( A_Recv_Arg_Node );
        Varref( An_Eval_Recv_Arg_Node.Argument );

      when Types.AST.Node_Const_Recv_Arg  =>
        A_Const_Recv_Arg_Node := Types.AST.Const_Recv_Arg_Node_Ptr( A_Recv_Arg_Node );
        Const( A_Const_Recv_Arg_Node.A_Constant );

      when others                         =>
        AST.Tools.Semantic_Assert( False, "Unrecognized A_Recv_Arg_Node!" );
    end case;

  end Recv_Arg;

  procedure Recv_Args( A_Recv_Args_Node : Types.AST.Recv_Args_Node_Ptr ) is
    use type Types.AST.Recv_Args_Node_Ptr;
    use type Types.AST.Recv_Arg_Node_List.Cursor;
    Next  : Types.AST.Recv_Arg_Node_List.Cursor;
    A_Recv_Arg_Node : Types.AST.Recv_Arg_Node_Ptr;
  begin

    AST.Tools.Semantic_Assert( A_Recv_Args_Node /= null, "A_Recv_Args_Node is null!" );
    Logger.Info( "Walking a Recv_Args" );
    -- Grammar guarantees arguments won't be empty
    Next := A_Recv_Args_Node.Arguments.First;
    while Next /= Types.AST.Recv_Arg_Node_List.No_Element loop
      A_Recv_Arg_Node := Types.AST.Recv_Arg_Node_List.Element( Next );
      Recv_Arg( A_Recv_Arg_Node );
      Types.AST.Recv_Arg_Node_List.Next( Next );
    end loop;

  end Recv_Args;

  procedure Recv( A_Recv_Node : Types.AST.Recv_Node_Ptr ) is
    use type Types.AST.Recv_Node_Ptr;
  begin

    AST.Tools.Semantic_Assert( A_Recv_Node /= null, "A_Recv_Node is null!" );
    Logger.Info( "Walking a Recv" );

    Varref( A_Recv_Node.Target );
    AST.Tools.Semantic_Assert( AST.Tools.Is_A_Channel_Variable( A_Recv_Node.Target ) = True, "Target of a recv statement is not a channel variable!" );
    Recv_Args( A_Recv_Node.Arguments );

    case A_Recv_Node.Tag is
      when  Types.AST.Node_Move_Fifo_Recv   |
            Types.AST.Node_Copy_Fifo_Recv   |
            Types.AST.Node_Move_Random_Recv |
            Types.AST.Node_Copy_Random_Recv =>
        null;
      when others                       =>
        AST.Tools.Semantic_Assert( False, "Unrecognized A_Recv_Node!" );
    end case;

  end Recv;
 
  procedure Assign           ( An_Assign_Node  : Types.AST.Assign_Node_Ptr           ) is
    use type Types.AST.Node_Tag;
    use type Types.AST.Assign_Node_Ptr;
    An_Any_Expr_Assign_Node : Types.AST.Any_Expr_Assign_Node_Ptr;
    --Widen_Type              : Types.AST.Data_Type_Ptr;
  begin

    AST.Tools.Semantic_Assert(  An_Assign_Node /= null, "An_Assign_Node is null!" );
    Logger.Info( "Walking an Assign" );
    Varref( An_Assign_Node.L_Value );
    --Some no-no-s
    AST.Tools.Semantic_Assert( An_Assign_Node.L_Value.Declaration.Tag /= Types.AST.Node_Mtype, "Cannot write to an mtype constant!" );
    if AST.Tools.Is_A_Channel_Variable( An_Assign_Node.L_Value ) = True then
      AST.Tools.Raise_Unsupported_Construct( "assignments to channel variables" );
    end if;

    case An_Assign_Node.Tag is

      when Types.AST.Node_Any_Expr_Assign   =>
        An_Any_Expr_Assign_Node := Types.AST.Any_Expr_Assign_Node_Ptr( An_Assign_Node );
        Any_Expr( An_Any_Expr_Assign_Node.R_Value );
        -- After visiting child
        --Widen_Type := Widen( An_Any_Expr_Assign_Node.R_Value.LUB_Type,
        --                     An_Any_Expr_Assign_Node.L_Value.Declaration.The_Type );
        -- TODO: allow meaningful truncation
        --AST.Tools.Semantic_Assert( Widen_Type /= null,
        --                       "Target is narrower than LUB of source expression!" );

      when  Types.AST.Node_Increment_Assign  |
            Types.AST.Node_Decrement_Assign  =>
        null;

      when others                           =>
        AST.Tools.Semantic_Assert( False, "unrecognized An_Assign_Node!" );

    end case;

  end Assign;

  procedure Options          ( An_Options_Node : Types.AST.Options_Node_Ptr          ) is
    use type Types.AST.Sequence_Node_List.Cursor;
    use type Types.AST.Options_Node_Ptr;
    Next            : Types.AST.Sequence_Node_List.Cursor;
    A_Sequence_Node : Types.AST.Sequence_Node_Ptr;
  begin
    AST.Tools.Semantic_Assert( An_Options_Node /= null, "An_Options_Node is null!" );
    Logger.Info( "Walking an Options..." );
    Next := An_Options_Node.Sequence_List.First;
    while Next /= Types.AST.Sequence_Node_List.No_Element loop
      A_Sequence_Node := Types.AST.Sequence_Node_List.Element( Next );
      Sequence( A_Sequence_Node );
      Types.AST.Sequence_Node_List.Next( Next );
    end loop;
  end Options;

  -- SPIN does not complain about 'undeclared' variables
  -- should one be actually referred to before it is defined, but we do!
  procedure Proctype         ( A_Proctype_Node : Types.AST.Proctype_Node_Ptr         ) is
    use type Types.Lexer.Token_Type;
    use type Types.AST.Proctype_Node_Ptr;
    use type Types.AST.Decl_Lst_Node_Ptr;
    Name  : Types.Lexer.Bound_String.Bounded_String;
  begin

    AST.Tools.Semantic_Assert( A_Proctype_Node /= null, "A_Proctype_Node is null!" );
    Logger.Info( "Walking a Proctype..." );
    Name := Types.Lexer.Element_In_Table( A_Proctype_Node.Name );
    Logger.Info( "Name: " & Types.Lexer.Bound_String.To_String( Name ) );
    -- Put proctype name into current (global) scope 
    Logger.Info( "SCOPE: Putting '" & Types.Lexer.Bound_String.To_String( Name ) & "' into the current scope" );
    AST.Tools.Semantic_Assert( AST.Tools.Scope_Put( Name, Types.AST.Node_Ptr(A_Proctype_Node) ) = True,
                           "Redefining existing variable '" & Types.Lexer.Bound_String.To_String( Name ) & "' in this scope!" );

    -- Create a new giant scope to rule them all and push the old one
    AST.Tools.Scope_Push;

    -- TODO: generate code differently depending on PROCTYPE or D_PROCTYPE
    Active( A_Proctype_Node.Active );
    -- Local parameters are no different from local variables
    if A_Proctype_Node.Parameters /= null then
      Decl_Lst( A_Proctype_Node.Parameters );
    end if;
    Priority( A_Proctype_Node.Priority );
    Enabler(  A_Proctype_Node.Enabler  );
    Sequence( A_Proctype_Node.Sequence );

    -- Pop the old scope
    AST.Tools.Scope_Pop;

  end Proctype;

  procedure Active           ( An_Active_Node  : Types.AST.Active_Node_Ptr           ) is
    use type Types.Lexer.Token_Type;
    use type Types.Lexer.Promela_Natural;
    use type Types.AST.Active_Node_Ptr;
    use type Types.AST.Const_Node_Ptr;
    Number  : Types.Lexer.Promela_Natural;
  begin
    if An_Active_Node /= null then
      Logger.Info( "Walking an Active..." );
      if An_Active_Node.Number /= null then
        -- TODO: validate range of number of processes
        -- SPIN does not seem to enforce this.
        Number                      := Types.Lexer.Element_In_Table( An_Active_Node.Number.Number );
        AST.Tools.Semantic_Assert(  Number > 1,
                                    "Less than 2 active processes declared!"  );
        Number_Of_Process_Instances := Number_Of_Process_Instances + Natural( Number );
        AST.Tools.Semantic_Assert(  Number_Of_Process_Instances <= 255,
                                    "More than 255 active processes, in total, declared!"  );
        Logger.Info("Active number: " & Types.Lexer.Promela_Natural'Image( Number ) );
      end if;
    end if;
  end Active;

  procedure Priority ( A_Priority_Node : Types.AST.Priority_Node_Ptr ) is
    use type Types.AST.Priority_Node_Ptr;
    Number              : Types.Lexer.Promela_Natural;
  begin
    -- Default priority for a process is 1
    if A_Priority_Node /= null then
      Logger.Info( "Walking a Priority" );
      AST.Tools.Raise_Unsupported_Construct( "process priority" );
      -- TODO: validate priority range
      Number := Types.Lexer.Element_In_Table( A_Priority_Node.Number.Number );
      Logger.Info("Priority number: " & Types.Lexer.Promela_Natural'Image( Number ) );
    end if;
  end Priority;

  procedure Enabler          ( An_Enabler_Node : Types.AST.Enabler_Node_Ptr          ) is
    use type Types.AST.Enabler_Node_Ptr;
  begin
    if An_Enabler_Node /= null then
      Logger.Info( "Walking an Enabler (yeah, man...enable the process! power to the process!)" );
      AST.Tools.Raise_Unsupported_Construct( "process enabler" );
      Logger.Info( "Walking an Enabler condition..." );
      -- TODO: make sure Any_Expr( Condition ) returns bit/bool type
      Any_Expr( An_Enabler_Node.Condition );
    end if;
  end Enabler;

  procedure Poll( A_Poll_Node : Types.AST.Poll_Node_Ptr ) is
    use type Types.AST.Poll_Node_Ptr;
  begin

    AST.Tools.Semantic_Assert( A_Poll_Node /= null, "A_Poll_Node is null!" );
    Logger.Info( "Walking a Poll..." );

    Varref( A_Poll_Node.Target );
    -- TODO: check that channel variable does not refer to a rendezvous channel
    AST.Tools.Semantic_Assert( AST.Tools.Is_A_Channel_Variable( A_Poll_Node.Target ) = True, "Target of a poll expression is not a channel variable!" );
    Recv_Args( A_Poll_Node.Arguments );

    case A_Poll_Node.Tag is
      when  Types.AST.Node_Fifo_Poll   |
            Types.AST.Node_Random_Poll =>
        null;
      when others                       =>
        AST.Tools.Semantic_Assert( False, "Unrecognized A_Poll_Node!" );
    end case;

  end Poll;

  -- TODO: more semantic checks!
  procedure Any_Expr ( An_Any_Expr_Node : in out Types.AST.Any_Expr_Node_Ptr ) is
    use type Types.Lexer.Token_Type;
    use type Types.AST.Any_Expr_Node_Ptr;
    use type Types.AST.Binar_Op_Node_Ptr;
    use type Types.AST.Binary_Any_Expr_Node_Ptr;
    use type Types.AST.Varref_Node_Ptr;
    A_Higher_Precedence_Implies_Any_Expr_Node
                                      : Types.AST.Higher_Precedence_Implies_Any_Expr_Node_Ptr;
    An_Unary_Any_Expr_Node            : Types.AST.Unary_Any_Expr_Node_Ptr;
    A_Len_Any_Expr_Node               : Types.AST.Len_Any_Expr_Node_Ptr;
    A_Const_Any_Expr_Node             : Types.AST.Const_Any_Expr_Node_Ptr;
    A_Varref_Any_Expr_Node            : Types.AST.Varref_Any_Expr_Node_Ptr;
    A_Poll_Any_Expr_Node              : Types.AST.Poll_Any_Expr_Node_Ptr;
    A_Remoteref_Any_Expr_Node         : Types.AST.Remoteref_Any_Expr_Node_Ptr;
    An_Enabled_Any_Expr_Node          : Types.AST.Enabled_Any_Expr_Node_Ptr;
    A_Pc_Value_Any_Expr_Node          : Types.AST.Pc_Value_Any_Expr_Node_Ptr;
    A_Run_Any_Expr_Node               : Types.AST.Run_Any_Expr_Node_Ptr;
    A_Channel_Poll_Any_Expr_Node      : Types.AST.Channel_Poll_Any_Expr_Node_Ptr;
    -- Tree rewrite
    A_Binary_Any_Expr_Node            : Types.AST.Binary_Any_Expr_Node_Ptr;
    Binary_Tree_Changed               : Boolean := False;
  begin

    AST.Tools.Semantic_Assert(  An_Any_Expr_Node /= null, "An_Any_Expr_Node is null!" );
    -- Do you have a Binary_Op != NULL ?
    if An_Any_Expr_Node.BinaryOp /= null then
      -- Then we will rewrite you using Binary_Tree_Rewrite( Any_Expr_Node ) !
      Logger.Info( "Rewriting Any_Expr with binary operations to conform to a correct binary tree..." );
      A_Binary_Any_Expr_Node := AST.Tools.Binary_Tree_Restructure( An_Any_Expr_Node );
      Logger.Info( "Rewrote Any_Expr with binary operations to conform to a correct binary tree." );
      -- now rewrite tree again and again according to precedence rules until we can change no more (a big small idea from SICP)
      loop
        AST.Tools.Binary_Tree_Precedence_Correction( A_Binary_Any_Expr_Node, Binary_Tree_Changed );
        exit when Binary_Tree_Changed = False;
        Logger.Info( "Tree might need one more round of operator precedence correction..." );
        Binary_Tree_Changed := False;
      end loop;
      -- Now you are a Binary_Any_Expr_Node
      An_Any_Expr_Node := Types.AST.Any_Expr_Node_Ptr( A_Binary_Any_Expr_Node );
    end if;

    Logger.Info( "Walking an Any_Expr..." );
    case An_Any_Expr_Node.Tag is

      when Types.AST.Node_Binary_Any_Expr =>
        Logger.Info( "Walking a Binary_Any_Expr_Node..." );
        A_Binary_Any_Expr_Node := Types.AST.Binary_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        AST.Tools.Semantic_Assert( A_Binary_Any_Expr_Node.Operator /= Types.Lexer.Token_Null, "A_Binary_Any_Expr_Node is missing an operator!" ); 
        AST.Tools.Semantic_Assert( A_Binary_Any_Expr_Node.Left_Leaf /= null xor A_Binary_Any_Expr_Node.Left_Expression /= null,
                               "A_Binary_Any_Expr_Node has both a left leaf and a left expression!" );
        AST.Tools.Semantic_Assert( A_Binary_Any_Expr_Node.Right_Leaf /= null xor A_Binary_Any_Expr_Node.Right_Expression /= null,
                               "A_Binary_Any_Expr_Node has both a right leaf and a right expression!" );
        Logger.Info( "Binary operator = " & Types.Lexer.Token_Type'Image( A_Binary_Any_Expr_Node.Operator ) );
        if A_Binary_Any_Expr_Node.Left_Leaf /= null then
          Logger.Info( "Walking left leaf..." );
          Any_Expr( A_Binary_Any_Expr_Node.Left_Leaf );
        end if;
        if A_Binary_Any_Expr_Node.Left_Expression /= null then
          Logger.Info( "Walking left expression..." );
          Any_Expr( Types.AST.Any_Expr_Node_Ptr( A_Binary_Any_Expr_Node.Left_Expression ) );
        end if;
        if A_Binary_Any_Expr_Node.Right_Leaf /= null then
          Logger.Info( "Walking right leaf..." );
          Any_Expr( A_Binary_Any_Expr_Node.Right_Leaf );
        end if;
        if A_Binary_Any_Expr_Node.Right_Expression /= null then
          Logger.Info( "Walking right expression..." );
          Any_Expr( Types.AST.Any_Expr_Node_Ptr( A_Binary_Any_Expr_Node.Right_Expression ) );
        end if;

      when  Types.AST.Node_Empty_Channel_Poll_Any_Expr  |
            Types.AST.Node_Nempty_Channel_Poll_Any_Expr |
            Types.AST.Node_Full_Channel_Poll_Any_Expr   |
            Types.AST.Node_Nfull_Channel_Poll_Any_Expr  =>
        Logger.Info( "Walking a Channel_Poll_Any_Expr_Node..." );
        A_Channel_Poll_Any_Expr_Node := Types.AST.Channel_Poll_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Varref( A_Channel_Poll_Any_Expr_Node.Channel_Name );
        -- TODO: check that channel variable does not refer to a rendezvous channel
        AST.Tools.Semantic_Assert( AST.Tools.Is_A_Channel_Variable( A_Channel_Poll_Any_Expr_Node.Channel_Name ) = True, "Target of a chanpoll expression is not a channel variable!" );

      when Types.AST.Node_Const_Any_Expr  =>
        Logger.Info( "Walking a Const_Any_Expr_Node..." );
        A_Const_Any_Expr_Node := Types.AST.Const_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Const( A_Const_Any_Expr_Node.A_Constant );

      when Types.AST.Node_Enabled_Any_Expr  =>
        Logger.Info( "Walking an Enabled_Any_Expr_Node..." );
        An_Enabled_Any_Expr_Node := Types.AST.Enabled_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Any_Expr( An_Enabled_Any_Expr_Node.Generic_Any_Expr );
        AST.Tools.Raise_Unsupported_Construct( "enabled" );

      when Types.AST.Node_Higher_Precedence_Implies_Any_Expr  =>
        Logger.Info( "Walking a Higher_Precedence_Implies_Any_Expr_Node..." );
        A_Higher_Precedence_Implies_Any_Expr_Node := Types.AST.Higher_Precedence_Implies_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Any_Expr( A_Higher_Precedence_Implies_Any_Expr_Node.Generic_Any_Expr );
        Implies( A_Higher_Precedence_Implies_Any_Expr_Node.Implies );

      when Types.AST.Node_Len_Any_Expr =>
        Logger.Info( "Walking a Len_Any_Expr_Node..." );
        A_Len_Any_Expr_Node := Types.AST.Len_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Varref( A_Len_Any_Expr_Node.Channel_Name );
        -- TODO: check that channel variable does not refer to a rendezvous channel
        AST.Tools.Semantic_Assert( AST.Tools.Is_A_Channel_Variable( A_Len_Any_Expr_Node.Channel_Name ) = True, "Target of a len expression is not a channel variable!" );

      when Types.AST.Node_Non_Progress_Any_Expr =>
        Logger.Info( "Walking a Non_Progress_Any_Expr_Node..." );
        AST.Tools.Raise_Unsupported_Construct( "np_" );

      when Types.AST.Node_Pc_Value_Any_Expr =>
        Logger.Info( "Walking a Pc_Value_Any_Expr_Node..." );
        A_Pc_Value_Any_Expr_Node := Types.AST.Pc_Value_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Any_Expr( A_Pc_Value_Any_Expr_Node.Generic_Any_Expr );
        AST.Tools.Raise_Unsupported_Construct( "pc_value" );

      when Types.AST.Node_Poll_Any_Expr =>
        Logger.Info( "Walking a Poll_Any_Expr_Node..." );
        A_Poll_Any_Expr_Node := Types.AST.Poll_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Poll( A_Poll_Any_Expr_Node.Poll );

      when Types.AST.Node_Remoteref_Any_Expr  =>
        Logger.Info( "Walking a Remoteref_Any_Expr_Node..." );
        A_Remoteref_Any_Expr_Node := Types.AST.Remoteref_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Logger.Info( "Version: " & Types.Lexer.Token_Type'Image( A_Remoteref_Any_Expr_Node.Version ) );
        AST.Tools.Semantic_Assert( A_Remoteref_Any_Expr_Node.Process /= null, "A_Remoteref_Any_Expr_Node.Process = null!" );
        Varref( A_Remoteref_Any_Expr_Node.Process );
        Logger.Info( "Process Target: " & Types.Lexer.Bound_String.To_String( Types.Lexer.Element_In_Table( A_Remoteref_Any_Expr_Node.Process_Target ) ) );
        AST.Tools.Raise_Unsupported_Construct( "remote references" );

      when Types.AST.Node_Run_Any_Expr  =>
        Logger.Info( "Walking a Run_Any_Expr_Node..." );
        A_Run_Any_Expr_Node := Types.AST.Run_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Logger.Info( "Name: " & Types.Lexer.Bound_String.To_String( Types.Lexer.Element_In_Table( A_Run_Any_Expr_Node.Name ) ) );
        Arg_Lst( A_Run_Any_Expr_Node.Arguments );
        Priority( A_Run_Any_Expr_Node.Priority );
        AST.Tools.Raise_Unsupported_Construct( "run expressions" );

      when Types.AST.Node_Timeout_Any_Expr  =>
        Logger.Info( "Walking a Timeout_Any_Expr_Node..." );
        AST.Tools.Raise_Unsupported_Construct( "timeout" );

      when Types.AST.Node_Unary_Any_Expr  =>
        Logger.Info( "Walking a Unary_Any_Expr_Node..." );
        An_Unary_Any_Expr_Node := Types.AST.Unary_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Logger.Info( "Operator: " & Types.Lexer.Token_Type'Image( An_Unary_Any_Expr_Node.Operator ) );
        AST.Tools.Semantic_Assert(  An_Unary_Any_Expr_Node.Operator = Types.Lexer.Token_Ones_Complement or else
                          An_Unary_Any_Expr_Node.Operator = Types.Lexer.Token_Not_Or_Send_1   or else
                          An_Unary_Any_Expr_Node.Operator = Types.Lexer.Token_Subtract,
                          "An_Unary_Any_Expr_Node.Operator is invalid!" );
        -- unary expression tree correction here; rewrite tree by rearranging stuff
        if An_Unary_Any_Expr_Node.Generic_Any_Expr.BinaryOp /= null then
          An_Unary_Any_Expr_Node.BinaryOp                   := An_Unary_Any_Expr_Node.Generic_Any_Expr.BinaryOp;
          An_Unary_Any_Expr_Node.Generic_Any_Expr.BinaryOp  := null;
          Any_Expr( An_Any_Expr_Node );
        else
          Any_Expr( An_Unary_Any_Expr_Node.Generic_Any_Expr );
        end if;

      when Types.AST.Node_Varref_Any_Expr =>
        Logger.Info( "Walking a Varref_Any_Expr_Node..." );
        A_Varref_Any_Expr_Node := Types.AST.Varref_Any_Expr_Node_Ptr( An_Any_Expr_Node );
        Varref( A_Varref_Any_Expr_Node.Variable );

      when others =>
        AST.Tools.Semantic_Assert( False, "Unrecognized An_Any_Expr_Node!" );
    
    end case;

    --if An_Any_Expr_Node.BinaryOp /= null then
    --  Binar_Op( An_Any_Expr_Node.BinaryOp );
    --  Logger.Info( "Visited children via Binar_OP" );
    --   NOTE: update LUB type here, after having visited children, possibly leaves
    --  if    A_Varref_Any_Expr_Node /= null  then
    --    AST.Tools.Semantic_Assert( A_Varref_Any_Expr_Node.BinaryOp.Generic_Any_Expr.LUB_Type /= null,
    --                           "A_Varref_Any_Expr_Node.BinaryOp.Generic_Any_Expr.LUB_Type = null!" );
    --    A_Varref_Any_Expr_Node.LUB_Type := LUB( A_Varref_Any_Expr_Node.Variable.Declaration.The_Type,
    --                                            A_Varref_Any_Expr_Node.BinaryOp.Generic_Any_Expr.LUB_Type );
    --  elsif A_Const_Any_Expr_Node /= null   then
    --    if A_Const_Any_Expr_Node.A_Constant.all in Number_Const_Node then
    --      A_Number_Const_Node := Number_Types.AST.Const_Node_Ptr( A_Const_Any_Expr_Node.A_Constant );
    --      AST.Tools.Semantic_Assert( A_Const_Any_Expr_Node.BinaryOp.Generic_Any_Expr.LUB_Type /= null,
    --                            "A_Const_Any_Expr_Node.BinaryOp.Generic_Any_Expr.LUB_Type = null!" );
    --      A_Const_Any_Expr_Node.LUB_Type := LUB( A_Number_Const_Node.The_Type,
    --                                             A_Const_Any_Expr_Node.BinaryOp.Generic_Any_Expr.LUB_Type );
    --    end if;
    --  end if;
    --  Logger.Info( "Calculated LUB Type");
    --else
    --  -- You are a leaf
    --  if A_Varref_Any_Expr_Node /= null then
    --    A_Varref_Any_Expr_Node.LUB_Type := A_Varref_Any_Expr_Node.Variable.Declaration.The_Type;
    --  elsif A_Const_Any_Expr_Node /= null then
    --    A_Number_Const_Node := Number_Types.AST.Const_Node_Ptr( A_Const_Any_Expr_Node.A_Constant );
    --    A_Const_Any_Expr_Node.LUB_Type := A_Number_Const_Node.The_Type;
    --    AST.Tools.Semantic_Assert( A_Const_Any_Expr_Node.LUB_Type /= null,
    --                           "A_Const_Any_Expr_Node.LUB_Type = null!" );
    --  end if;
    --end if;

  end Any_Expr;

  procedure Implies ( An_Implies_Node : Types.AST.Implies_Node_Ptr ) is
    use type Types.AST.Implies_Node_Ptr;
  begin
    Logger.Info( "Walking an Implies" );
    if An_Implies_Node /= null then
      AST.Tools.Raise_Unsupported_Construct( "conditional expressions" );
    end if;
  end Implies;

  procedure Varref ( A_Varref_Node : Types.AST.Varref_Node_Ptr ) is
    use type Types.AST.Node_Ptr;
    use type Types.AST.Varref_Node_Ptr;
    use type Types.AST.Any_Expr_Node_Ptr;
    use type Types.AST.Node_Tag;
    Name          : Types.Lexer.Bound_String.Bounded_String;
    Declaration   : Types.AST.Node_Ptr;
    An_Ivar_Node  : Types.AST.Ivar_Node_Ptr;
    An_Array      : Types.AST.Array_Data_Type_Ptr;
  begin
    AST.Tools.Semantic_Assert( A_Varref_Node /= null, "A_Varref_Node is null!" );
    Name        := Types.Lexer.Element_In_Table( A_Varref_Node.Name ); 
    Logger.Info( "Walking a Varref = '" & Types.Lexer.Bound_String.To_String( Name ) & "'" );
    Declaration := AST.Tools.Scope_Get( Name );
    AST.Tools.Semantic_Assert(  Declaration /= null, "Variable declaration not found in this scope!" );
    AST.Tools.Semantic_Assert(  Declaration.Tag = Types.AST.Node_Ivar           or else
                                Declaration.Tag = Types.AST.Node_Any_Expr_Ivar  or else
                                Declaration.Tag = Types.AST.Node_Ch_Init_Ivar   or else
                                Declaration.Tag = Types.AST.Node_Mtype,
                                "Variable declaration not found in this scope!" );
    Logger.Info( "Found variable type declaration" );
    A_Varref_Node.Declaration := Declaration;

    -- could be a scalar[0] or an array. let it do its thing. runtime type checking will take care of imprudence.
    if A_Varref_Node.Array_Offset /= null then
      AST.Tools.Semantic_Assert( Declaration.Tag /= Types.AST.Node_Mtype, "Mtype constants cannot be indexed!" );
      Any_Expr( A_Varref_Node.Array_Offset );
    end if;

    -- Struct_Offset
    case Declaration.Tag is

      when  Types.AST.Node_Ivar           |
            Types.AST.Node_Any_Expr_Ivar  |
            Types.AST.Node_Ch_Init_Ivar   =>
        An_Ivar_Node := Types.AST.Ivar_Node_Ptr( Declaration );
        case    An_Ivar_Node.The_Type.Kind is

          when  Types.AST.Built_In_Type =>
            AST.Tools.Semantic_Assert( A_Varref_Node.Struct_Offset = null, "Predefined types do not have a user-defined structure!" );

          when  Types.AST.Array_Type    =>
            An_Array  := Types.AST.Array_Data_Type_Ptr( An_Ivar_Node.The_Type );
            case An_Array.Base_Type.Kind is

              when Types.AST.Built_In_Type  =>
                AST.Tools.Semantic_Assert( A_Varref_Node.Struct_Offset = null, "Arrays of predefined types do not have a user-defined structure!" );

              when Types.AST.Array_Type     =>
                AST.Tools.Semantic_Assert( False, "Internal inconsistency! There are no arrays of arrays in Promela!" );

              when Types.AST.User_Type      =>
                Varref( A_Varref_Node.Struct_Offset );

            end case;

          when  Types.AST.User_Type     =>
            Varref( A_Varref_Node.Struct_Offset );

        end case;

      when  Types.AST.Node_Mtype          =>
        AST.Tools.Semantic_Assert( A_Varref_Node.Struct_Offset = null, "Mtype constants do not have a user-defined structure!" );

      when  others                        =>
        AST.Tools.Semantic_Assert( False, "Unrecognized variable declaration!" );

    end case;

  end Varref;

  procedure Arg_Lst ( An_Arg_Lst_Node : Types.AST.Arg_Lst_Node_Ptr ) is
    use type Types.AST.Arg_Lst_Node_Ptr;
    use type Types.AST.Any_Expr_Node_List.Cursor;
    Previous          : Types.AST.Any_Expr_Node_List.Cursor;
    An_Any_Expr_Node  : Types.AST.Any_Expr_Node_Ptr;
  begin
    Logger.Info( "Walking an Arg_Lst in reverse..." );
    if An_Arg_Lst_Node /= null then
      -- we walk the argument list in reverse, so that they are placed properly on the stack
      Previous := An_Arg_Lst_Node.Any_Expr_List.Last;
      while Previous /= Types.AST.Any_Expr_Node_List.No_Element loop
        An_Any_Expr_Node := Types.AST.Any_Expr_Node_List.Element( Previous );
        Any_Expr( An_Any_Expr_Node );
        Previous := Types.AST.Any_Expr_Node_List.Previous( Previous );
      end loop;
    end if;
  end Arg_Lst;

  procedure Const ( A_Const_Node    : Types.AST.Const_Node_Ptr ) is
    use type Types.AST.Const_Node_Ptr;
  begin
    AST.Tools.Semantic_Assert( A_Const_Node /= null, "A_Const_Node is null!" );
    Logger.Info( "Walking a Const..." );
    Logger.Info( "Number = " & Types.Lexer.Promela_Natural'Image( Types.Lexer.Element_In_Table( A_Const_Node.Number ) ) );
    -- We do this possibly for more rigorous type-checking someday,
    -- to better specify Promela's semantics.
    A_Const_Node.The_Type := Types.AST.Int_Data_Type;
  end Const;

end AST;
