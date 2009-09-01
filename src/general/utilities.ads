-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
--
--  Utilities for string processing
--
with Ada.Strings.Fixed, Ada.Characters.Handling, Ada.Text_IO;
with Global; use Global;
package Utilities is
  -- Pad string S to a Name
  function Pad(
    S:     in String;
    Count: in Natural   := Name'Length;
    Pad:   in Character := Ada.Strings.Space)
     return Name renames Ada.Strings.Fixed.Head;

  -- Trim string Source with default Side Both
  function Trim(
    Source: in String; 
    Side:   Ada.Strings.Trim_End := Ada.Strings.Both) 
      return String renames Ada.Strings.Fixed.Trim;

  -- Rename To_Lower to avoid with'ing Ada.Characters.Handling 
  function To_Lower(Item: in String) return String
    renames Ada.Characters.Handling.To_Lower;

  -- Within S, find "Parm=Value," and return Value as String or Byte
  function Extract(S: String; Parm: String) return String;
  function Extract(S: String; Parm: String) return Byte;

  -- Within S, find "Parm={Value}" and extract parenthesized Value
  function Extract_Paren(
    S: String; Parm: String; Open: String := "{"; Close: String := "}")
      return String;

  -- Construct string with line number L and statement name S
  function Line_Statement(L: Byte; S: Name) return String;

  -- Put for Integer and Byte with default Width => 0 followed by ","
  -- Optional new line
  procedure Put(Item: in Integer; New_Line: in Boolean := False);
  procedure Put(Item: in Byte;    New_Line: in Boolean := False);

  -- Calls above Put after printing "S=" for named association
  procedure Put(S:    in String; 
                Item: in Integer; New_Line: in Boolean := False);
  procedure Put(S:    in String;
                Item: in Byte;    New_Line: in Boolean := False);

  -- Put S=Item with Boolean value Item in lower case
  procedure Put(S:    in String;
                Item: in Boolean; New_Line: in Boolean := False);
end Utilities;
