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

with Ada.Strings.Fixed;

package body Types.CodeGen is
 
  function Byte_Hash( N : Byte ) return Ada.Containers.Hash_Type is
  begin
    return Ada.Containers.Hash_Type'Mod( N );
  end Byte_Hash;
  
  -- MBA
  function  Trim(  Source   : in String; 
                  Side      : Ada.Strings.Trim_End := Ada.Strings.Both  )
  return String
  renames Ada.Strings.Fixed.Trim;

  function  Add_To_String_Table( Format : Types.Lexer.Bounded_String_Hashed_Set.Cursor  ;
                                 Automaton_File : Ada.Text_IO.File_Type                 )
  return Operand is
    Index   : String_Vector.Extended_Index;
  begin
    Index := String_Table.Find_Index( Format );
    if Index = String_Vector.No_Index then
      String_Table.Append( Format );
      Index := String_Table.Last_Index;
      Ada.Text_IO.Put_Line( Automaton_File,
        "string=,offset=" & Trim( Integer'Image( Index ) )
        & ",value="""     & Types.Lexer.Bound_String.To_String( Get_From_String_Table( Index ) )
        & ""","
      );
    end if;
    return Operand( Index );
  end       Add_To_String_Table;

  function  Get_From_String_Table( Index : Operand )
  return    Types.Lexer.Bound_String.Bounded_String is
    Vector_Index  : String_Vector.Extended_Index := String_Vector.Extended_Index( Index );
  begin
    return Types.Lexer.Element_In_Table( String_Table.Element( Vector_Index ) );
  end       Get_From_String_Table;
  
  function  Get_String_Table_Length
  return    Integer is
  begin
    return Integer( String_Table.Length );
  end       Get_String_Table_Length;

  function  Add_To_Natural_Table( Number  : Types.Lexer.Natural_Hashed_Set.Cursor ;
                                  Automaton_File  : Ada.Text_IO.File_Type         )
  return Operand is
    Index   : Natural_Vector.Extended_Index;
  begin
    Index := Natural_Table.Find_Index( Number );
    if Index = Natural_Vector.No_Index then
      Natural_Table.Append( Number );
      Index := Natural_Table.Last_Index;
      Ada.Text_IO.Put_Line( Automaton_File,
        "number=,offset=" & Trim( Integer'Image( Index ) )
        & ",value="       & Trim( Types.Lexer.Promela_Natural'Image( Get_From_Natural_Table( Index ) ) )
        & ","
      );
    end if;
    return Operand( Index );
  end       Add_To_Natural_Table;

  function  Get_From_Natural_Table( Index : Operand )
  return    Types.Lexer.Promela_Natural is
    Vector_Index  : Natural_Vector.Extended_Index := Natural_Vector.Extended_Index( Index );
  begin
    return Types.Lexer.Element_In_Table( Natural_Table.Element( Vector_Index ) );
  end       Get_From_Natural_Table;
  
  function  Get_Natural_Table_Length
  return    Integer is
  begin
    return Integer( Natural_Table.Length );
  end       Get_Natural_Table_Length;

end Types.CodeGen;
