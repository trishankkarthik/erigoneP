-- Copyright 2009 by  Mordechai (Moti) Ben-Ari. See version.ads
with Ada.Command_Line, Ada.Text_IO, Ada.Strings.Bounded;
with Compile_Global;
procedure Compiler is
  use  Ada.Command_Line;
  Logs:        Boolean  := False;
  Count:       Natural  := 1;
  Usage_Error: exception;

  package Names is new Ada.Strings.Bounded.Generic_Bounded_Length(128);
  use Names;
  Promela_File, Automata_File, Root: Bounded_String;
  Extension:    Natural;

  procedure Error is
  begin
    Ada.Text_IO.Put_Line(
      "Usage: compiler [-o] Promela-file [Automata-file]");
    raise Usage_Error;
  end Error;

begin
  if Argument_Count < 1 then Error; end if;
  if Argument(Count)(1) = '-' then
    Logs := Argument(1) = "-o";
    if not Logs then Error; end if;
    Count := 2;
  end if;
  if Argument_Count < Count then Error; end if;
  Promela_File := To_Bounded_String(Argument(Count));
  Extension := Index(Promela_File, ".");
  if Extension = 0 then
    Root := Promela_File;
    Promela_File := Promela_File & ".pml";
  else
    Root := Bounded_Slice(Promela_File, 1, Extension-1);
  end if;
  Count := Count + 1;
  if    Argument_Count < Count then
    Automata_File := Root & ".aut";
  elsif Argument_Count = Count then
    Automata_File := To_Bounded_String(Argument(Count));
    if Index(Automata_File, ".") = 0 then
      Automata_File := Automata_file & ".aut";
    end if;
  else
    Error;
  end if;
--  Ada.Text_IO.Put_Line(To_String(Root));
--  Ada.Text_IO.Put_Line(To_String(Promela_File));
--  Ada.Text_IO.Put_Line(To_String(Automata_File));
  Compile_Global.Compile_File(
    To_String(Promela_File), To_String(Automata_File), Logs);
end Compiler;
