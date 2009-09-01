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
-- UTILITY.ADS: A utility package.

with Types.AST;

package Utility is

  -- Fast forward to the next Stmt in a Step_List
  procedure Get_Next_Statement_In_Steps
                             ( Next            : in out Types.AST.Step_Node_List.Cursor ;
                               A_Stmt_Node     : in out Types.AST.Stmt_Node_Ptr         );

  -- Get the first statement in a sequence
  function  Get_First_Statement_In_Sequence
                             ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr         )
            return  Types.AST.Stmt_Node_Ptr;

  -- Get the last statement in a sequence
  function  Get_Last_Statement_In_Sequence
                             ( A_Sequence_Node : Types.AST.Sequence_Node_Ptr         )
            return  Types.AST.Stmt_Node_Ptr;

  -- Get next DO statement
  procedure Get_Next_Do_Statement( Next           : in out Types.AST.Step_Node_List.Cursor  ;
                                   A_Do_Stmt_Node : in out Types.AST.Do_Stmt_Node_Ptr       );

  -- Get next IF statement
  procedure Get_Next_If_Statement( Next             : in out Types.AST.Step_Node_List.Cursor  ;
                                   An_If_Stmt_Node  : in out Types.AST.If_Stmt_Node_Ptr       );


  -- Get next nested, atomic or d_step statement
  procedure Get_Next_Nested_Atomic_D_Step_Statement(  Next            : in out Types.AST.Step_Node_List.Cursor  ;
                                                      A_Stmt_Node     : in out Types.AST.Stmt_Node_Ptr          ;
                                                      A_Sequence_Node : in out Types.AST.Sequence_Node_Ptr      );

end Utility;
