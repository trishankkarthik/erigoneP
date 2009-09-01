-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
with Ada.Command_Line, Ada.Strings.Fixed, Ada.Text_IO;
with Files, Utilities, Version;
with Options; use Options;
package body Arguments is
  -- Internal exception raised by Error, handled by Get_Arguments
  Command_Line_Error: exception;

  -- Display usage
  procedure Usage is
    use Ada.Text_IO;

    -- Sizes are in thousands, display without 000
    function To_String(N: Positive) return String is
    begin
      return " (" & Utilities.Trim(Integer'Image(N/1000)) & ")";
    end To_String;

  begin -- Usage
    Put_Line(Version.Copyright);
    New_Line;
    Put_Line("Usage: erigone [arguments] filename (default extension .pml)");
    New_Line;
    Put_Line("  -X  execution mode, X is one of:");
    Put_Line("        [a]cceptance verification");
    Put_Line("        ltl2[b]a translation only");
    Put_Line("        [c]ompilation only");
    Put_Line("        [f]airness verification");
    Put_Line("        [g]uided simulation");
    Put_Line("        gN - guided simulation with trail N");
    Put_Line("        [i]nteractive simulation");
    Put_Line("        [r]andom simulation (default)");
    Put_Line("        [s]safety verification");
    New_Line;
    Put_Line("  -lXN limits in thousands except hash, (defaults):");
    Put_Line("    h   [h]ash slots     "  & To_String(Hash_Slots*1000) &
                                           " 2**N slots, 16<=N<=32");
    Put_Line("    l   [l]ocation stack "  & To_String(Location_Stack_Max));
    Put_Line("    p   [p]rogress steps "  & To_String(Progress_Steps));
    Put_Line("    s   [s]tate stack    "  & To_String(State_Stack_Max));
    Put_Line("    t   [t]otal steps    "  & To_String(Total_Steps));
    New_Line;
    Put_Line("  -h    [h]elp screen");
    Put_Line("  -mN   stop after N=[m]'th error; 0=report all errors");
    Put_Line("  -nN   ra[n]dom seed (default from clock)");
    Put_Line("  -o    write compiler l[o]gs");
    Put_Line("  -t[filename] linear [t]emporal logic formula");
    New_Line;
    Put_Line("  -d  display everything");
    Put_Line("  -dX display X, where X is a string of:");
    Put_Line("        [a]ll transitions");
    Put_Line("        [b]uchi automata");
    Put_Line("        [c]hosen transitions");
    Put_Line("        [e]xecutable transitions");
    Put_Line("        pro[g]ress notification");
    Put_Line("        [h]ash table");
    Put_Line("        [l]ocation stack");
    Put_Line("        si[m]ulation states");
    Put_Line("        [n]odes of LTL tableau");
    Put_Line("        [p]rogram symbols and transitions");
    Put_Line("        [r]untime statistics");
    Put_Line("        [s]tate stack");
    Put_Line("        [t]rail");
    Put_Line("        b[u]ffered channel contents");
    Put_Line("        [v]ersion and copyright message");
    Put_Line("        b[y]te code");
  end Usage;

  -- If argument error, display usage
  --   S is the unrecognized argument
  procedure Error(S: in String) is
  begin
    Ada.Text_IO.Put_Line("*** Invalid argument: " & S & " ***");
    Ada.Text_IO.New_Line;
    Usage;
    raise Command_Line_Error;
  end Error;

  -- Set flags for -d display argument
  procedure Set_Display(C: in Character) is
    D: Display_Type;
  begin
    case C is
      when 'a' => D := All_T;
      when 'b' => D := LTL;
      when 'c' => D := Chosen;
      when 'e' => D := Executable;
      when 'g' => D := Progress;
      when 'h' => D := Hash;
      when 'l' => D := Location_Stack;
      when 'm' => D := Simulation_States;
      when 'n' => D := Nodes;
      when 'p' => D := Program;
      when 'r' => D := Run;
      when 's' => D := State_Stack;
      when 't' => D := Trail;
      when 'u' => D := Channels;
      when 'v' => D := Options.Version;
      when 'y' => D := Byte_Codes;
      when others => Error("-d" & C);
    end case;
    Display(D) := True;
  end Set_Display;

  -- Set option from argument -S
  procedure Set_Option(S: in String) is
    I: Positive := S'First+1;  -- Argument is after hyphen

    -- Sizes must be multiplied by 1_000
    function Get_Thousands return Positive is
    begin
      return Positive'Value(S(I+2..S'Last)) * 1_000;
    end Get_Thousands;

  begin
    case S(I) is
      when 'a' => Execution_Mode  := Acceptance;
      when 'b' => Execution_Mode  := LTL_Only;
                  LTL_Mode        := File;
      when 'c' => Execution_Mode  := Compile_Only;
      when 'd' =>
        if S'Length = 2 then   -- Set all flags true if "-d"
          Display := (others => True);
        else -- For each character set the relevant flag
          for J in S'First+2..S'Last loop
            Set_Display(S(J));
          end loop;
        end if;
      when 'f' => Execution_Mode  := Fairness;
      when 'g' => Simulation_Mode := Guided;
                  if S'Length > 2 then
                    Trail_Number := Natural'Value(S(I+1..S'Last));
                  end if;
      when 'h' => Usage;
                  raise Command_Line_Error;
      when 'i' => Simulation_Mode := Interactive;
      when 'l' => -- Set limits -lXN
        case S(I+1) is
          when 'h' => Hash_Slots := Positive'Value(S(I+2..S'Last));
          when 'l' => Location_Stack_Max := Get_Thousands;
          when 'p' => Progress_Steps     := Get_Thousands;
          when 's' => State_Stack_Max    := Get_Thousands;
          when 't' => Total_Steps        := Get_Thousands;
          when others => Error(S);
        end case;
      when 'm' => Error_Number    := Natural'Value(S(I+1..S'Last));
      when 'n' => Seed            := Natural'Value(S(I+1..S'Last));
      when 'o' => Compiler_Logs   := True;
      when 'r' => Simulation_Mode := Random;
      when 's' => Execution_Mode  := Safety;
      when 't' => LTL_Mode        := File;
                  if S /= "-t" then
                    Files.LTL_File_Name := new String'(S(S'First+2..S'Last));
                  end if;
      when others => Error(S);
    end case;
    exception
      -- Size parameter may not be numeric
      when Constraint_Error => Error(S);
  end Set_Option;

  -- Set the file names from the source file name
  procedure Set_File_Names is
    use Ada.Command_Line;
    Index: Natural;
  begin
    -- Check whether the file name has an extension
    Index := Ada.Strings.Fixed.Index(
               Argument(Argument_Count), ".", Ada.Strings.Backward);
    if  Index = 0 then
      -- If not, use the default
      Files.Root_File_Name   :=
        new String'(Argument(Argument_Count));
      Files.Source_File_Name :=
        new String'(Files.Root_File_Name.all & Files.Source_Extension);
    else
      -- If so, compute the root for other files
      Files.Root_File_Name   := 
        new String'(Argument(Argument_Count)(1..Index-1));
      Files.Source_File_Name :=
        new String'(Argument(Argument_Count));
    end if;

    -- Add extensions to get other file names
    Files.LTL_File_Name      :=
          new String'(Files.Root_File_Name.all & Files.LTL_Extension);
    Files.Trail_File_Name    :=
          new String'(Files.Root_File_Name.all & Files.Trail_Extension);
    Files.Automata_File_Name :=
          new String'(Files.Root_File_Name.all & Files.Automata_Extension);
  end Set_File_Names;

  -- Put message concerning an argument
  procedure Message(S: in String) is
    use Ada.Command_Line, Ada.Text_IO;
  begin
    New_Line; Put_Line("*** " & S & " ***"); New_Line;
  end Message;

  -- Get and set arguments
  function Get_Arguments return Boolean is
    use Ada.Command_Line, Ada.Text_IO;
  begin
    if Argument_Count < 1 then         -- No arguments
      Usage;
      raise Command_Line_Error;
    elsif Argument_Count = 1 then      -- One argument must be:
      if Argument(1) = "-dv" then      --   -dv
        Put_Line(Version.Copyright);
        return False;
      elsif Argument(1) = "-h" then    --   or -h
        Usage;
        raise Command_Line_Error;
      elsif Argument(1)(1) = '-' then  --   or file name
        Message("Missing file name");
        return False;
      end if;
    end if;

    Set_File_Names;

    -- Process each argument
    for Count in 1 .. Argument_Count-1 loop
      if Argument(Count)(1) /= '-' then
        Error(Argument(Count));
      else
        Set_Option(Argument(Count));
      end if;
    end loop;

    -- Warning messages on the use of the -t argument
    if LTL_Mode /= File and 
         (Execution_Mode = Acceptance or
          Execution_Mode = Fairness) then
      Message("Warning: -t should be set for acceptance or fairness");
    elsif LTL_Mode = File and Execution_Mode = Simulation then
      Message("Warning: -t ignored for simulation");
    end if;

    -- Optionally display the version and the options
    if Display(Options.Version) then 
      Put_Line(Version.Copyright);
    end if;

    if Display(Run) then
      Put_Options;
    end if;
    return True;
  exception
    when Command_Line_Error => return False;
  end Get_Arguments;
end Arguments;
