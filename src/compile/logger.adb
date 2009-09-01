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

with Ada.Directories;

package body Logger is
  
  procedure Set_Level_Threshold(L : Level) is
  begin
    Logger_Level := L;
  end Set_Level_Threshold;

  procedure Open_Log_File(Filename : String ; Append : Boolean := False) is
    File_Mode : Ada.Text_IO.File_Mode := Ada.Text_IO.Out_File;
  begin
    if Logger_Output_Switch = True and then Ada.Text_IO.Is_Open(Log_File) = False then
      if Ada.Directories.Exists( Filename ) then
        if Append = True then
          File_Mode := Ada.Text_IO.Append_File;
        end if;
        Ada.Text_IO.Open(File => Log_File, Mode => File_Mode, Name => Filename);
      else
        Ada.Text_IO.Create(File => Log_File, Mode => File_Mode, Name => Filename);
      end if;
      Ada.Text_IO.Set_Output( Log_File );
    end if;
  end Open_Log_File;
  
  procedure Close_Log_File is
  begin
    if Logger_Output_Switch = True and then Ada.Text_IO.Is_Open(Log_File) = True then
      Ada.Text_IO.Close(Log_File);
      Ada.Text_IO.Set_Output( Ada.Text_IO.Standard_Output );
    end if;
  end Close_Log_File;
  
  procedure Output(S : String ; L : Level) is
  begin
    if Logger_Output_Switch = True and then Logger_Level <= L then
      Ada.Text_IO.Put_Line( S );
    end if;
  end Output;

  procedure New_Line is
  begin
    if Logger_Output_Switch = True and then Ada.Text_IO.Is_Open(Log_File) = True then
      Ada.Text_IO.New_Line( Log_File );
    else
      Ada.Text_IO.New_Line;
    end if;
  end New_Line;

  procedure Debug(S : String) is
  begin
    Output("(DEBUG) " & S, Debug_Level);
  end Debug;
  
  procedure Info(S : String) is
  begin
    Output("(INFO) " & S, Info_Level);
  end Info;
  
  procedure Warning(S : String) is
  begin
    Output("(WARNING) " & S, Warning_Level);
  end Warning;
  
  procedure Error(S : String) is
  begin
    Output("(ERROR) " & S, Error_Level);
  end Error;
  
  procedure Critical(S : String) is
  begin
    Output("(CRITICAL) " & S, Critical_Level);
  end Critical;

end Logger;
