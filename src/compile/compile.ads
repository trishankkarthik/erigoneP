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
-- POMEGRANATE.ADS: This is the interface between the main program
-- of Erigone (sds.adb) and the Pomegranate compiler.
-- The lexer, parser, semantic analyzer and
-- code generator are called at various points here.

package Compile is

  -- Lexical scan, parsing and semantic analysis
  -- of the input source file
  procedure Parse(
    Filename : in String; 
    Automata_File_Name: in String;
    Logs: in Boolean);

  -- LTL formulae translation
  function Translate_LTL_Expression( F : String ) return String;

end Compile;
