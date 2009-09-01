-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
--
-- Run-time options
--
with Ada.Text_IO;
with Compile_Global, Utilities;
package body Options is
  -- Put the options
  procedure Put_Options is
    use Utilities;
  begin
    Ada.Text_IO.Put(
      "execution mode=" &
      To_Lower(Execution_Type'Image(Execution_Mode)));
    if Execution_Mode = Compile_Only or Execution_Mode = LTL_Only then
      Ada.Text_IO.Put_Line(",");
      return;
    elsif Execution_Mode = Simulation then
      Ada.Text_IO.Put(
          ",simulation mode=" &
          To_Lower(Simulation_Type'Image(Simulation_Mode)));
      Put(",seed=", Seed);
      Put("trail number=",   Trail_Number);
    else
      Put(",error number=",  Error_Number);
      Put("hash slots=",    Hash_Slots);
      Put("state stack=",    State_Stack_Max / 1_000);
      Put("location stack=", Location_Stack_Max / 1_000);
      Put("progress steps=", Progress_Steps / 1_000);
    end if;
    Put("total steps=",      Total_Steps / 1_000, New_Line => True);
  end Put_Options;

  -- Set default options before reading arguments
  procedure Set_Defaults is
  begin
    Error_Number       := 1;
    Location_Stack_Max := 3_000;
    Progress_Steps     := 1_000;
    Seed               := -1;
    State_Stack_Max    := 2_000;
    Total_Steps        := 10_000;
    Trail_Number       := 0;
    Hash_Slots         := 22;

    Execution_Mode     := Simulation;
    LTL_Mode           := None;
    Simulation_Mode    := Random;

    Display            := (others => False);

    Compiler_Logs := False;
  end Set_Defaults;
end Options;
