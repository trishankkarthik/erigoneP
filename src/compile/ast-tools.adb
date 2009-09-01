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
-- AST-TOOLS.ADB: Utilities for semantic analysis.

with Ada.Unchecked_Deallocation;

with Logger;

package body AST.Tools is

  -- The pointer to the current scope in a tree of scopes.
  -- Why a tree and not a linked list? See the type Scope in Types.AST.
  Top                       : Types.AST.Scope_Ptr   := null;
  -- An operator precedence array used in restructuring
  -- a binary expression tree.
  Operator_Precedence_Array : Types.AST.Operator_Precedence;

  procedure Free_Binar_Op_Node is new Ada.Unchecked_Deallocation( Types.AST.Binar_Op_Node, Types.AST.Binar_Op_Node_Ptr );

  procedure Init_Globals is
  begin
    -- init built-in data type references
    Types.AST.Init_Globals;
    -- init operator precedence map; Ben-Ari(2008), pp. 7
    Operator_Precedence_Array( Types.Lexer.Token_Or_Logical                ) := 3; 
    Operator_Precedence_Array( Types.Lexer.Token_And_Logical               ) := 4;
    Operator_Precedence_Array( Types.Lexer.Token_Or_Bitwise                ) := 5;
    Operator_Precedence_Array( Types.Lexer.Token_Xor_Bitwise               ) := 6;
    Operator_Precedence_Array( Types.Lexer.Token_And_Bitwise               ) := 7; 
    Operator_Precedence_Array( Types.Lexer.Token_Equals                    ) := 8;
    Operator_Precedence_Array( Types.Lexer.Token_Not_Equals                ) := 8;
    Operator_Precedence_Array( Types.Lexer.Token_Less_Than                 ) := 9;
    Operator_Precedence_Array( Types.Lexer.Token_Less_Than_Or_Equal_To     ) := 9;
    Operator_Precedence_Array( Types.Lexer.Token_Greater_Than              ) := 9;
    Operator_Precedence_Array( Types.Lexer.Token_Greater_Than_Or_Equal_To  ) := 9;
    Operator_Precedence_Array( Types.Lexer.Token_Left_Shift                ) := 10;
    Operator_Precedence_Array( Types.Lexer.Token_Right_Shift               ) := 10;
    Operator_Precedence_Array( Types.Lexer.Token_Add                       ) := 11;
    Operator_Precedence_Array( Types.Lexer.Token_Subtract                  ) := 11;
    Operator_Precedence_Array( Types.Lexer.Token_Divide                    ) := 12;
    Operator_Precedence_Array( Types.Lexer.Token_Multiply                  ) := 12;
    Operator_Precedence_Array( Types.Lexer.Token_Modulus                   ) := 12;
  end Init_Globals;

  procedure Raise_Unsupported_Construct( Name : String ) is
    Message : String := "Erigone/Pomegranate does not, as yet, support " & Name & "!";
  begin
    Logger.Error( Message );
    raise Unsupported_Construct with Message;
  end Raise_Unsupported_Construct;

  procedure Semantic_Assert( Assertion : Boolean ; Message : String ) is
  begin
    if Assertion = False then
      Logger.Error( Message );
      raise Generic_Semantic_Error with Message; 
    end if;
  end Semantic_Assert;

  function Scope_Put( Name        : Types.Lexer.Bound_String.Bounded_String ;
                      Definition  : Types.AST.Node_Ptr                      )
  return    Boolean is
    Inserted : Boolean;
    Position : Types.AST.Bounded_String_To_Node_Ptr_Hashed_Map.Cursor;
  begin
    Top.Hash_Table.Insert( Name, Definition, Position, Inserted );
    return Inserted;
  end Scope_Put;

  -- TODO: Why don't I return an exception instead of null when Name is not found?
  -- If so, AST can boost from a huge code cleanup.
  function  Scope_Get( Name : Types.Lexer.Bound_String.Bounded_String )
  return    Types.AST.Node_Ptr is
    use type Types.AST.Bounded_String_To_Node_Ptr_Hashed_Map.Cursor;
    use type Types.AST.Scope_Ptr;
    Definition  : Types.AST.Bounded_String_To_Node_Ptr_Hashed_Map.Cursor;
    S           : Types.AST.Scope_Ptr := Top;
  begin
    -- NOTE: May return null
    while S /= null loop
      Definition := S.Hash_Table.Find( Name );
      if Definition /= Types.AST.Bounded_String_To_Node_Ptr_Hashed_Map.No_Element then
        return Types.AST.Bounded_String_To_Node_Ptr_Hashed_Map.Element( Definition );
      end if;
      S := S.Up;
    end loop;
    return null;
  end Scope_Get;
  
  function  Is_There_Global( Name : Types.Lexer.Bound_String.Bounded_String )
  return    Boolean is
    use type Types.AST.Bounded_String_To_Node_Ptr_Hashed_Map.Cursor;
    use type Types.AST.Scope_Ptr;
    Definition  : Types.AST.Bounded_String_To_Node_Ptr_Hashed_Map.Cursor;
    S           : Types.AST.Scope_Ptr := Top;
  begin
    -- Get to global scope  
    while S.Up /= null loop
      S := S.Up;
    end loop;
    Definition := S.Hash_Table.Find( Name );
    if Definition /= Types.AST.Bounded_String_To_Node_Ptr_Hashed_Map.No_Element then
        return True;
    else
      return False;
    end if;
  end Is_There_Global;
  
  procedure Scope_Push is
    use type Types.AST.Scope_Ptr;
    S : Types.AST.Scope_Ptr;
  begin
    Logger.Info( "SCOPE: Pushing old scope unto a new one" );
    S := new Types.AST.Scope;
    S.Up := Top;
    Top := S;
    if Top.Up /= null then
      -- append to the children of the parent of Top=S
      Logger.Info( "Appending scope to parent" );
      Top.Up.Children.Append( Top );
    end if;
  end Scope_Push;

  procedure Scope_Pop is
  begin
    Top := Top.Up;
    Logger.Info( "SCOPE: Popping back the old scope" );
  end Scope_Pop;

  -- Why, it must be the opposite of Scope_Get!
  function Get_Root_Scope return Types.AST.Scope_Ptr is
  begin
    return Top;
  end Get_Root_Scope;


  function  LUB( Type1 : Types.AST.Data_Type_Ptr; Type2 : Types.AST.Data_Type_Ptr )
  return    Types.AST.Data_Type_Ptr is
    use type Types.AST.Data_Type_Ptr;
  begin
    Semantic_Assert(       Type1 = Types.AST.Bit_Data_Type    or else
                           Type1 = Types.AST.Byte_Data_Type   or else
                           Type1 = Types.AST.Mtype_Data_Type  or else
                           Type1 = Types.AST.Pid_Data_Type    or else
                           Type1 = Types.AST.Short_Data_Type  or else
                           Type1 = Types.AST.Int_Data_Type,
                           "Type1 is an invalid (pointer/array/channel) datatype!" );
    Semantic_Assert(       Type2 = Types.AST.Bit_Data_Type    or else
                           Type2 = Types.AST.Byte_Data_Type   or else
                           Type2 = Types.AST.Mtype_Data_Type  or else
                           Type2 = Types.AST.Pid_Data_Type    or else
                           Type2 = Types.AST.Short_Data_Type  or else
                           Type2 = Types.AST.Int_Data_Type,
                           "Type2 is an invalid (pointer/array/channel) datatype!" );
    if      Type1 = Type2                         then
      return Type1;
    elsif   Type1 = Types.AST.Bit_Data_Type       then
      return Type2;
    elsif   Type1 = Types.AST.Byte_Data_Type      then
      if    Type2 = Types.AST.Bit_Data_Type       or else
            Type2 = Types.AST.Mtype_Data_Type     or else
            Type2 = Types.AST.Pid_Data_Type       then
        return Type1;
      else
        return Type2;
      end if;
    elsif   Type1 = Types.AST.Mtype_Data_Type     then
      if    Type2 = Types.AST.Bit_Data_Type       or else
            Type2 = Types.AST.Byte_Data_Type      or else
            Type2 = Types.AST.Pid_Data_Type       then
        return Types.AST.Byte_Data_Type;
      else
        return Type2;
      end if;
    elsif   Type1 = Types.AST.Pid_Data_Type       then
      if    Type2 = Types.AST.Bit_Data_Type       or else
            Type2 = Types.AST.Byte_Data_Type      or else
            Type2 = Types.AST.Mtype_Data_Type     then
        return Types.AST.Byte_Data_Type;
      else
        return Type2;
      end if;
    elsif   Type1 = Types.AST.Short_Data_Type     then
      if    Type2 = Types.AST.Int_Data_Type       then
        return Type2;
      else
        return Type1;
      end if;
    else
      return Type1;
    end if;
  end LUB;
  
  function  Binary_Tree_Restructure ( An_Any_Expr_Node : Types.AST.Any_Expr_Node_Ptr )
  return    Types.AST.Binary_Any_Expr_Node_Ptr is
    use type Types.AST.Binar_Op_Node_Ptr;
    A_Binary_Any_Expr_Node : Types.AST.Binary_Any_Expr_Node_Ptr;
  begin
    A_Binary_Any_Expr_Node            := new Types.AST.Binary_Any_Expr_Node;
    A_Binary_Any_Expr_Node.Tag        := Types.AST.Node_Binary_Any_Expr;
    A_Binary_Any_Expr_Node.Operator   := An_Any_Expr_Node.BinaryOp.Operator;
    A_Binary_Any_Expr_Node.Left_Leaf  := An_Any_Expr_Node;
    if An_Any_Expr_Node.BinaryOp.Generic_Any_Expr.BinaryOp = null then
      -- base case
      A_Binary_Any_Expr_Node.Right_Leaf       := An_Any_Expr_Node.BinaryOp.Generic_Any_Expr;
    else
      -- inductive case
      A_Binary_Any_Expr_Node.Right_Expression := Binary_Tree_Restructure( An_Any_Expr_Node.BinaryOp.Generic_Any_Expr );
    end if;
    Free_Binar_Op_Node( An_Any_Expr_Node.BinaryOp );
    return A_Binary_Any_Expr_Node;
  end Binary_Tree_Restructure;

  -- NOTE: Yes, A_Binary_Any_Expr_Node may be restructured to correctly reflect operator precedence.
  -- I think it is complete. Must prove that it is correct. Will annotate this algorithm with
  -- diagrams in the future for easier understanding.
  procedure Binary_Tree_Precedence_Correction( A_Binary_Any_Expr_Node : in out Types.AST.Binary_Any_Expr_Node_Ptr ;
                                               Binary_Tree_Changed    : in out Boolean                  ) is
    use type Types.AST.Binary_Any_Expr_Node_Ptr;
    use type Types.AST.Any_Expr_Node_Ptr;
    Left_Expression_Ptr   : Types.AST.Binary_Any_Expr_Node_Ptr;
    Right_Expression_Ptr  : Types.AST.Binary_Any_Expr_Node_Ptr;
  begin

    if A_Binary_Any_Expr_Node.Left_Expression /= null then
      Semantic_Assert( A_Binary_Any_Expr_Node.Left_Leaf = null, "A_Binary_Any_Expr_Node.Left_Leaf /= null!" );
      Left_Expression_Ptr                         := A_Binary_Any_Expr_Node.Left_Expression;
      if    Operator_Precedence_Array( A_Binary_Any_Expr_Node.Operator )                  >
            Operator_Precedence_Array( Left_Expression_Ptr.Operator )                     then
        if    A_Binary_Any_Expr_Node.Left_Expression.Right_Leaf       /= null then
          Semantic_Assert( A_Binary_Any_Expr_Node.Left_Expression.Right_Expression = null,
                                 "A_Binary_Any_Expr_Node.Left_Expression.Right_Expression /= null!" );
          A_Binary_Any_Expr_Node.Left_Leaf        := Left_Expression_Ptr.Right_Leaf;
          A_Binary_Any_Expr_Node.Left_Expression  := null;
          Left_Expression_Ptr.Right_Leaf          := null;
          Left_Expression_Ptr.Right_Expression    := A_Binary_Any_Expr_Node;
        elsif A_Binary_Any_Expr_Node.Left_Expression.Right_Expression /= null then
          Semantic_Assert( A_Binary_Any_Expr_Node.Left_Expression.Right_Leaf = null,
                                 "A_Binary_Any_Expr_Node.Left_Expression.Right_Leaf /= null! " );
          A_Binary_Any_Expr_Node.Left_Expression  := Left_Expression_Ptr.Right_Expression;
          Left_Expression_Ptr.Right_Expression    := A_Binary_Any_Expr_Node;
        end if;
        if Left_Expression_Ptr.Left_Expression /= null or else Left_Expression_Ptr.Right_Expression /= null then
          -- restart correction with the new root
          Binary_Tree_Changed := True or Binary_Tree_Changed;
          Binary_Tree_Precedence_Correction( Left_Expression_Ptr, Binary_Tree_Changed );
        -- else: right leaf is already there, so no matter
        end if;
        -- the new root
        A_Binary_Any_Expr_Node := Left_Expression_Ptr;
      else
        -- same root
        Binary_Tree_Changed := False or Binary_Tree_Changed;
        Binary_Tree_Precedence_Correction( A_Binary_Any_Expr_Node.Left_Expression, Binary_Tree_Changed );
      end if;
    end if;

    if A_Binary_Any_Expr_Node.Right_Expression /= null then
      Semantic_Assert( A_Binary_Any_Expr_Node.Right_Leaf = null, "A_Binary_Any_Expr_Node.Right_Leaf /= null!" );
      Right_Expression_Ptr                        := A_Binary_Any_Expr_Node.Right_Expression;
      if    Operator_Precedence_Array( A_Binary_Any_Expr_Node.Operator )                  >=
            Operator_Precedence_Array( Right_Expression_Ptr.Operator )                    then
        if    A_Binary_Any_Expr_Node.Right_Expression.Left_Leaf       /= null then
          Semantic_Assert( A_Binary_Any_Expr_Node.Right_Leaf = null, "A_Binary_Any_Expr_Node.Right_Leaf /= null!" );
          A_Binary_Any_Expr_Node.Right_Leaf       := Right_Expression_Ptr.Left_Leaf;
          A_Binary_Any_Expr_Node.Right_Expression := null;
          Right_Expression_Ptr.Left_Leaf := null;
          Semantic_Assert( Right_Expression_Ptr.Left_Expression = null, "Right_Expression_Ptr.Left_Expression /= null!" );
          Right_Expression_Ptr.Left_Expression    := A_Binary_Any_Expr_Node;
        elsif A_Binary_Any_Expr_Node.Right_Expression.Left_Expression /= null then
          Semantic_Assert( A_Binary_Any_Expr_Node.Right_Expression.Left_Leaf = null, "A_Binary_Any_Expr_Node.Right_Expression.Left_Leaf /= null!" );
          A_Binary_Any_Expr_Node.Right_Expression := Right_Expression_Ptr.Left_Expression;
          Right_Expression_Ptr.Left_Expression    := A_Binary_Any_Expr_Node;
        end if;
        if Right_Expression_Ptr.Left_Expression /= null or else Right_Expression_Ptr.Right_Expression /= null then
          -- restart correction with the new root
          Binary_Tree_Changed := True or Binary_Tree_Changed;
          Binary_Tree_Precedence_Correction( Right_Expression_Ptr, Binary_Tree_Changed );
        -- else: right leaf is already there, so no matter
        end if;
        -- the new root
        A_Binary_Any_Expr_Node := Right_Expression_Ptr;
      else
        -- same root
        Binary_Tree_Changed := False or Binary_Tree_Changed;
        Binary_Tree_Precedence_Correction( A_Binary_Any_Expr_Node.Right_Expression, Binary_Tree_Changed );
      end if;
    end if;

  end Binary_Tree_Precedence_Correction;

  function  Is_A_Channel_Variable( A_Varref_Node : Types.AST.Varref_Node_Ptr )
  return    Boolean is
    use type Types.AST.Node_Ptr;
    use type Types.AST.Built_In_Data_Type_Kind;
    Answer        : Boolean := False;
    Declaration   : Types.AST.Node_Ptr;
    An_Ivar_Node  : Types.AST.Ivar_Node_Ptr;
    Built_In      : Types.AST.Built_In_Data_Type_Ptr;
    An_Array      : Types.AST.Array_Data_Type_Ptr;
  begin

    if A_Varref_Node.Declaration = null then
      --AST.Varref( A_Varref_Node );
      null;
    end if;
    Declaration := A_Varref_Node.Declaration;
    Semantic_Assert( Declaration /= null, "Declaration = null!" );
    case Declaration.Tag is

      when  Types.AST.Node_Ivar           |
            Types.AST.Node_Any_Expr_Ivar  |
            Types.AST.Node_Ch_Init_Ivar   =>
        An_Ivar_Node := Types.AST.Ivar_Node_Ptr( Declaration );
        case    An_Ivar_Node.The_Type.Kind is

          when  Types.AST.Built_In_Type =>
            Built_In := Types.AST.Built_In_Data_Type_Ptr( An_Ivar_Node.The_Type );
            if Built_In.Built_In_Kind = Types.AST.Chan then
              Answer := True;
            end if;

          when  Types.AST.Array_Type    =>
            An_Array  := Types.AST.Array_Data_Type_Ptr( An_Ivar_Node.The_Type );
            case An_Array.Base_Type.Kind is

              when Types.AST.Built_In_Type  =>
                Built_In := Types.AST.Built_In_Data_Type_Ptr( An_Array.Base_Type );
                if Built_In.Built_In_Kind = Types.AST.Chan then
                  Answer := True;
                end if;

              when Types.AST.Array_Type     =>
                Semantic_Assert( False, "Internal inconsistency! There are no arrays of arrays in Promela!" );

              when Types.AST.User_Type      =>
                Answer := Is_A_Channel_Variable( A_Varref_Node.Struct_Offset );

            end case;

          when  Types.AST.User_Type     =>
            Answer := Is_A_Channel_Variable( A_Varref_Node.Struct_Offset );

        end case;

      when  Types.AST.Node_Mtype          =>
        null;

      when  others                        =>
        Semantic_Assert( False, "Unrecognized variable declaration!" );

    end case;
    return Answer;

  end       Is_A_Channel_Variable;

end AST.Tools;
