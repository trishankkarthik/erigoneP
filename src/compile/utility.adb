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
-- UTILITY.ADB: A utility package.

package body Utility is
  
  -- NOTE: may set A_Stmt_Node to NULL
  procedure  Get_Next_Statement_In_Steps( Next        : in out Types.AST.Step_Node_List.Cursor  ;
                                          A_Stmt_Node : in out Types.AST.Stmt_Node_Ptr          )
  is
    use type Types.AST.Step_Node_List.Cursor;
    A_Step_Node       : Types.AST.Step_Node_Ptr;
    A_Stmt_Step_Node  : Types.AST.Stmt_Step_Node_Ptr;
  begin
    A_Stmt_Node         := null;
    Types.AST.Step_Node_List.Next( Next );
    if Next /= Types.AST.Step_Node_List.No_Element then
      A_Step_Node       := Types.AST.Step_Node_List.Element( Next );
      A_Stmt_Step_Node  := Types.AST.Stmt_Step_Node_Ptr( A_Step_Node );
      A_Stmt_Node       := A_Stmt_Step_Node.Statement;
    end if;
  end       Get_Next_Statement_In_Steps;
  
  -- NOTE: may return NULL
  function  Get_First_Statement_In_Sequence( A_Sequence_Node : Types.AST.Sequence_Node_Ptr )
  return    Types.AST.Stmt_Node_Ptr
  is
    A_Step_Node             : Types.AST.Step_Node_Ptr;
    A_Stmt_Step_Node        : Types.AST.Stmt_Step_Node_Ptr;
    A_Stmt_Node             : Types.AST.Stmt_Node_Ptr;
  begin
    
    if A_Sequence_Node.Stmt_Step_List.Is_Empty = False then
      A_Step_Node       := A_Sequence_Node.Stmt_Step_List.First_Element;
      A_Stmt_Step_Node  := Types.AST.Stmt_Step_Node_Ptr( A_Step_Node );
      A_Stmt_Node       := A_Stmt_Step_Node.Statement;
    end if;

    return A_Stmt_Node;

  end Get_First_Statement_In_Sequence;

  -- NOTE: may return NULL
  function Get_Last_Statement_In_Sequence( A_Sequence_Node : Types.AST.Sequence_Node_Ptr )
  return    Types.AST.Stmt_Node_Ptr
  is
    A_Step_Node             : Types.AST.Step_Node_Ptr;
    A_Stmt_Step_Node        : Types.AST.Stmt_Step_Node_Ptr;
    A_Stmt_Node             : Types.AST.Stmt_Node_Ptr;
  begin

    if A_Sequence_Node.Stmt_Step_List.Is_Empty = False then
      A_Step_Node       := A_Sequence_Node.Stmt_Step_List.Last_Element;
      A_Stmt_Step_Node  := Types.AST.Stmt_Step_Node_Ptr( A_Step_Node );
      A_Stmt_Node       := A_Stmt_Step_Node.Statement;
    end if;

    return A_Stmt_Node;

  end Get_Last_Statement_In_Sequence;

  -- NOTE: may set NULL
  procedure Get_Next_Do_Statement( Next           : in out Types.AST.Step_Node_List.Cursor  ;
                                   A_Do_Stmt_Node : in out Types.AST.Do_Stmt_Node_Ptr       )
  is
    use type Types.AST.Step_Node_List.Cursor;
    use type Types.AST.Node_Tag;
    A_Step_Node             : Types.AST.Step_Node_Ptr;
    A_Stmt_Step_Node        : Types.AST.Stmt_Step_Node_Ptr;
    A_Stmt_Node             : Types.AST.Stmt_Node_Ptr;
  begin
    A_Do_Stmt_Node      := null;
    while Next /= Types.AST.Step_Node_List.No_Element loop
      A_Step_Node       := Types.AST.Step_Node_List.Element( Next );
      A_Stmt_Step_Node  := Types.AST.Stmt_Step_Node_Ptr( A_Step_Node );
      A_Stmt_Node       := A_Stmt_Step_Node.Statement;
      if A_Stmt_Node.Tag = Types.AST.Node_Do_Stmt then
        A_Do_Stmt_Node  := Types.AST.Do_Stmt_Node_Ptr( A_Stmt_Step_Node.Statement );
        exit;
      else
        Types.AST.Step_Node_List.Next( Next );
      end if;
    end loop;
  end       Get_Next_Do_Statement;

  -- NOTE: may set NULL
  procedure Get_Next_If_Statement( Next             : in out Types.AST.Step_Node_List.Cursor  ;
                                   An_If_Stmt_Node  : in out Types.AST.If_Stmt_Node_Ptr       )
  is
    use type Types.AST.Step_Node_List.Cursor;
    use type Types.AST.Node_Tag;
    A_Step_Node             : Types.AST.Step_Node_Ptr;
    A_Stmt_Step_Node        : Types.AST.Stmt_Step_Node_Ptr;
    A_Stmt_Node             : Types.AST.Stmt_Node_Ptr;
  begin
    An_If_Stmt_Node     := null;
    while Next /= Types.AST.Step_Node_List.No_Element loop
      A_Step_Node       := Types.AST.Step_Node_List.Element( Next );
      A_Stmt_Step_Node  := Types.AST.Stmt_Step_Node_Ptr( A_Step_Node );
      A_Stmt_Node       := A_Stmt_Step_Node.Statement;
      if A_Stmt_Node.Tag = Types.AST.Node_If_Stmt then
        An_If_Stmt_Node := Types.AST.If_Stmt_Node_Ptr( A_Stmt_Step_Node.Statement );
        exit;
      else
        Types.AST.Step_Node_List.Next( Next );
      end if;
    end loop;
  end       Get_Next_If_Statement;

  -- NOTE: may set NULL and/or In_Atomic
  procedure Get_Next_Nested_Atomic_D_Step_Statement(  Next            : in out Types.AST.Step_Node_List.Cursor  ;
                                                      A_Stmt_Node     : in out Types.AST.Stmt_Node_Ptr          ;
                                                      A_Sequence_Node : in out Types.AST.Sequence_Node_Ptr      )
  is
    use type Types.AST.Step_Node_List.Cursor;
    A_Step_Node             : Types.AST.Step_Node_Ptr;
    A_Stmt_Step_Node        : Types.AST.Stmt_Step_Node_Ptr;
    A_Sequence_Stmt_Node    : Types.AST.Sequence_Stmt_Node_Ptr;
    An_Atomic_Stmt_Node     : Types.AST.Atomic_Stmt_Node_Ptr;
    A_D_Step_Stmt_Node      : Types.AST.D_Step_Stmt_Node_Ptr;
  begin
    A_Stmt_Node         := null;
    A_Sequence_Node     := null;
    while Next /= Types.AST.Step_Node_List.No_Element loop
      A_Step_Node       := Types.AST.Step_Node_List.Element( Next );
      A_Stmt_Step_Node  := Types.AST.Stmt_Step_Node_Ptr( A_Step_Node );
      A_Stmt_Node       := A_Stmt_Step_Node.Statement;
      case A_Stmt_Node.Tag is
        when Types.AST.Node_Sequence_Stmt =>
          A_Sequence_Stmt_Node  := Types.AST.Sequence_Stmt_Node_Ptr( A_Stmt_Node );
          A_Sequence_Node       := A_Sequence_Stmt_Node.Sequence;
          exit;
        when Types.AST.Node_Atomic_Stmt   =>
          An_Atomic_Stmt_Node   := Types.AST.Atomic_Stmt_Node_Ptr( A_Stmt_Node );
          A_Sequence_Node       := An_Atomic_Stmt_Node.Sequence;
          exit;
        when Types.AST.Node_D_Step_Stmt   =>
          A_D_Step_Stmt_Node    := Types.AST.D_Step_Stmt_Node_Ptr( A_Stmt_Node );
          A_Sequence_Node       := A_D_Step_Stmt_Node.Sequence;
          exit;
        when others                       =>
          A_Stmt_Node           := null;
          Types.AST.Step_Node_List.Next( Next );
      end case;
    end loop;
  end       Get_Next_Nested_Atomic_D_Step_Statement;

end Utility;
