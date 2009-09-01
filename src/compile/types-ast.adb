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

package body Types.AST is

  procedure Init_Globals is
  begin
    -- Point to built-in data type definitions
    Bit_Data_Type   := Bit_Type'Access;
    Byte_Data_Type  := Byte_Type'Access;
    Mtype_Data_Type := Mtype_Type'Access;
    Chan_Data_Type  := Chan_Type'Access;
    Pid_Data_Type   := Pid_Type'Access;
    Short_Data_Type := Short_Type'Access;
    Int_Data_Type   := Int_Type'Access;
  end Init_Globals;

  -- I am not sticking to SPIN's conventions of setting the numbers,
  -- which I am guessing are arbitrary. The exact values really shouldn't matter to the users,
  -- which is the whole point of using constant names to refer to the values, whatever they are.
  procedure Add_Mtype(  A_Mtype_Node  : Mtype_Node_Ptr                                  ;
                        Name          : Types.Lexer.Bound_String.Bounded_String         )
  is
  begin
    if Mtype_Map.Contains( Name ) = True then
      raise Mtype_Redundant_Exception;
    else
      -- Simply to globally keep track of things
      Mtype_Map.Insert( Name, Mtype_Table_Counter );
      -- Map in the Mtype_Node the name to its byte
      A_Mtype_Node.Names_Map.Replace( Name, Mtype_Table_Counter );
      -- Increment global mtype counter
      Mtype_Table_Counter := Mtype_Table_Counter + 1;
    end if;
  exception
    when Constraint_Error =>
      raise Mtype_Full_Exception;
    when others           =>
      raise;
  end Add_Mtype;

end Types.AST;
