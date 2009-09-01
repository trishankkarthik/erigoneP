-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
with Compile;  -- Only with clause for the compiler
package body Compile_Global is
  -- Compile a source file and write the automata file
  procedure Compile_File(
    Source_File_Name: in String;
    Automata_File_Name: in String;
    Logs: in Boolean) renames Compile.Parse;

  -- Compile an expression (from an LTL formula)
  --   Return a string with the byte codes
  function Compile_Expression(Expression: String) return String
    renames Compile.Translate_LTL_Expression;
end Compile_Global;
