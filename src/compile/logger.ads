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
-- LOGGER.ADS: A simple home-brewed logger package specification.
-- Roughly modelled after the Python 2.5 'logging' library.
-- http://www.python.org/doc/2.5.2/lib/module-logging.html
--
-- TODO:
-- 2. Automatic log file closing

with Ada.Text_IO;

package Logger is

  -- There are 5 levels of logging:
  -- Debug, Info, Warning, Error, Critical
  type Level is ( Debug_Level, Info_Level, Warning_Level, Error_Level, Critical_Level );
  
  -- Set the level threshold beyond which the logger will output;
  -- 1 by default, so this means that all levels are sent to output
  procedure Set_Level_Threshold (L        : Level   );
  -- Redirect all logging to this file name
  procedure Open_Log_File       (Filename : String  ; Append : Boolean := False  );
  -- Close the log file
  procedure Close_Log_File;

  -- Output a new line
  procedure New_Line;
  -- Level 1
  procedure Debug     (S : String);
  -- Level 2
  procedure Info      (S : String);
  -- Level 3
  procedure Warning   (S : String);
  -- Level 4
  procedure Error     (S : String);
  -- Level 5
  procedure Critical  (S : String);

  -- Control logging output
  -- Someone could abuse this to produce erratic logging behaviour,
  -- but that is perhaps beyond the duty of the logger.
  Logger_Output_Switch  : Boolean :=  False;

  private

    -- This is how we keep track of the logger level
    Logger_Level      : Level := Debug_Level;
    -- The reference to the log file
    Log_File          : Ada.Text_IO.File_Type;
    -- The actual procedure used to output to log file
    procedure Output  (S : String ; L : Level);

end Logger;
