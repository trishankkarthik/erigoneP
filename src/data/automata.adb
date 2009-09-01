-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
with Ada.Text_IO, Ada.Strings.Fixed, Ada.Exceptions;
with Automata.Display, Compile_Global, LTL, Config, Files;
with Options, Symbol_Tables, Utilities;
with Ada.Containers.Generic_Array_Sort;
package body Automata is
  procedure Initialize is
    use Options;
  begin
    Processes     := 0;
    Never         := Global.None;
    Accept_Count  := 0;
  end Initialize;

  -- < is the lexicographic order used to sort transitions
  --   They are sorted by Source state and then by Target state
  --     except that transitions for "else" are placed last
  function "<"(Left, Right: Transitions) return Boolean is
    use type Compile_Global.Opcode;
  begin
    return (Left.Source < Right.Source) or

           (Left.Source = Right.Source  and
            Left.Byte_Code(0).Operator  /= Compile_Global.logic_else and
            Right.Byte_Code(0).Operator  = Compile_Global.logic_else) or

            (Left.Source = Right.Source  and
            Left.Target < Right.Target);
  end "<";

  -- Sort transitions
  procedure Sort_Transitions is new
    Ada.Containers.Generic_Array_Sort(
      Byte, Transitions, Transition_Array);

  procedure Sort(T: in out Transition_Array) renames Sort_Transitions;

  -- Get a transition for location L
  function Get_Transition(L: Location_Type) return Transitions is
  begin
    return Program(L.Process).Transition_List(L.Transition);
  end Get_Transition;

  -- Check if the transition for location L is labeled "end"
  --   This is an optimization
  function Is_End(L: Location_Type) return Boolean is
  begin
    return Program(L.Process).Transition_List(L.Transition).End_Label = 1;
  end Is_End;

  -- Get the final Operator of the transition for location L
  --   This is an optimization
  function Get_Final_Operator(L: Location_Type)
      return Compile_Global.Opcode is
    T: Transitions renames Program(L.Process).Transition_List(L.Transition);
  begin
    return T.Byte_Code(T.Code_Size-1).Operator;
  end Get_Final_Operator;

  -- Is the state S of process P accepting?
  function Is_Accept(P: Byte; S: Byte) return Boolean is
  begin
    if Accept_Count = 0 then return False; end if;
    for A in 0 .. Accept_Count-1 loop
      if P = Accept_Table(A).Process and S = Accept_Table(A).State then
        return True;
      end if;
    end loop;
    return False;
  end Is_Accept;

  -- Get process name for displaying a state
  function  Get_Process_Name(P: Byte) return Name is
  begin
    return Program(P).Identifier;
  end Get_Process_Name;

  -- Return an array of the initial states of each process
  function Get_Process_Initials return Byte_Array_Base is
    P: Byte_Array;
  begin
    for I in Byte range 0..Processes-1 loop
      P(I) := Program(I).Initial_State;
    end loop;
    return P;
  end Get_Process_Initials;

  -- Return all transitions whose source state
  --   is a current state of a process in the state vector S
  function Get_All_Locations(S: State_Vectors.State_Vector)
      return Location_Record is
    N: Byte := 0;      -- Number of transitions satisfying the property
    C: Byte;           -- Count of transitions in the process
    L: Location_Record := ((others => <>), 0, 0);
    use type Options.Execution_Type;
  begin
    for P in 0..Processes-1 loop
      C := Program(P).Count-1;

      -- For fairness, remove the null transition from consideration
      if Options.Execution_Mode = Options.Fairness and then
         P /= Never then
        C := C-1;
      end if;

      -- Copy all transitions with Source equal to the current
      --   state for this process in the state vector
      for T in 0 .. C loop
        if Program(P).Transition_List(T).Source = S.Process(P) then
          if N > Config.Location_Index'Last then
            raise Internal_Error
              with "too many transitions from state";
          end if;
          L.Location_Array(N) := (P, T);
          N := N + 1;
        end if;
      end loop;
    end loop;
    L.Count := N;
    return L;
  end Get_All_Locations;

  -- Remove transition I from location record L
  procedure Remove_Transition(L: in out Location_Record; I: Byte) is
  begin
    if L.Count <= 1 then
      L.Count := 0;
    else
      -- Decrement Never_Index if removing non-never transition
      if  Automata.Never /= None and then
          L.Never_Index /= None and then
          L.Location_Array(I).Process /= Automata.Never then
        L.Never_Index := L.Never_Index - 1;
      end if;

      -- Move all the subsequent transitions
      L.Location_Array(I..L.Count-2) := L.Location_Array(I+1..L.Count-1);
      L.Count := L.Count - 1;

      -- Check if no more never transitions
      if L.Never_Index /= None and then L.Never_Index = L.Count then
        L.Never_Index := None;
      end if;
    end if;
  end Remove_Transition;

  -- Set the Source state as a null transition and return its index
  --   Used when unfolding state space for transition
  -- The null transition is the last one in the Transition_List
  function Set_Null_Transition(P: Byte; Source: Byte) return Byte is
  begin
    Program(P).Transition_List(Program(P).Count-1).Source := Source;
    Program(P).Transition_List(Program(P).Count-1).Target := Source;
    return Program(P).Count-1;
  end Set_Null_Transition;

  -- Extract a Byte_Code array of Size from a string S,
  --   which is the sequence of byte codes without the braces
  procedure Extract_Byte_Code(
      S: in String; Size: out Byte; Byte_Code: out Byte_Code_Array) is
    Start: Positive := S'First;  -- Start index of current search
    Stop:  Positive;             -- Stop index in the current search             
    use Ada.Strings.Fixed;
  begin
    Size := 0;
    while Start < S'Last loop
      if Size > Config.Byte_Code_Index'Last then
        raise Unexpected_Input with "too many byte codes";
      end if;

      -- The opcode ends with a blank
      Stop := Index(S, " ", Start);
      Byte_Code(Size).Operator  := 
        Compile_Global.Opcode'Value(S(Start .. Stop-1));

      -- The first operand ends with a blank
      Start := Stop + 1;
      Stop := Index(S, " ", Start);
      Byte_Code(Size).Operand1 := Byte'Value(S(Start .. Stop-1));

      -- The second operand ends with a comma
      Start := Stop + 1;
      Stop := Index(S, ",", Start);
      Byte_Code(Size).Operand2 := Byte'Value(S(Start .. Stop-1));
      Start := Stop + 1;
      Size := Size + 1;
    end loop;
  end Extract_Byte_Code;

  -- Create a transition by extracting data from string S
  function Set_Transition(S: in String) return Transitions is
    T: Transitions;
    B: Byte_Code_Array;
    use Utilities;
  begin
    T.Source       := Extract(S, "source");
    T.Target       := Extract(S, "target");
    T.Atomic       := Extract(S, "atomic");
    T.End_Label    := Extract(S, "end");
    T.Accept_Label := Extract(S, "accept");
    T.Line_Number  := Extract(S, "line");
    T.Statement    := new String'(Pad(Extract_Paren(S, "statement")));
    Extract_Byte_Code(
      Extract_Paren(S, "byte code"), T.Code_Size, B);
    T.Byte_Code    := new Byte_Code_Array'(B);
    return T;
  end Set_Transition;

  -- For "active [N] proctype", replicate transitions and local variables
  procedure Active_Proctypes(Active: Byte) is
    use Utilities;
    -- Name of proctype
    P_Name: String := Trim(Program(Processes-1).Identifier);
  begin
    -- Replicate local variables
    Symbol_Tables.Replicate_Local_Symbols(P_Name, Active);
    -- Add "_1" to first process name
    Program(Processes-1).Identifier := Pad(P_Name & "_1");

    -- Create Active-1 copies of last process read
    for I in 2 .. Active loop
      declare
        -- Rename entry in process table for new process
        P: Process_Type renames Program(Processes+I-2);
      begin
        -- Copy process data
        P := Program(Processes-1);
        -- Add "_I" to process name
        P.Identifier := Pad(P_Name & "_" & Trim(Byte'Image(I)));
        -- Fix addresses of load and store of local variables
        for T in 0 .. P.Count-1 loop
          -- First, clone the byte code array because it is allocated
          P.Transition_List(T).Byte_Code :=
            new Byte_Code_Array'(P.Transition_List(T).Byte_Code.all);
          Symbol_Tables.Fix_Addresses(I,
            P.Transition_List(T).Byte_Code.all,
            P.Transition_List(T).Code_Size);
        end loop;
      end;
    end loop;

    Processes := Processes + Active - 1;
  end Active_Proctypes;

  -- Read the automata from the automata file
  procedure Read(Automata_File: in Ada.Text_IO.File_Type) is
    S:           String(1..1024);    -- String and length for Get_Line
    Length:      Natural;
    Active:      Byte;               -- For active[N] proctype
    use Ada.Text_IO, Options, Utilities;
  begin
    while not End_Of_File(Automata_File) loop
      Get_Line(Automata_File, S, Length);

      -- List of transitions for a process
      if Ada.Strings.Fixed.Index(S, "process=") = 1 then
        Active := Extract(S, "active");
        declare
          P: Process_Type renames Program(Processes);
        begin
          P.Identifier    := Pad(Extract(S, "process"));
          P.Initial_State := Extract(S, "initial");
          P.Count         := Extract(S, "transitions");
          if P.Count > Config.Transition_Index'Last+1 then
              raise Unexpected_Input
                with "too many transitions in a process";
          end if;

          -- Read all the transitions
          for I in 0 .. P.Count-1 loop
            Get_Line(Automata_File, S, Length);
            P.Transition_List(I) := Set_Transition(S);
          end loop;

          Sort(P.Transition_List(0..P.Count-1));

          -- Create dummy transition transition where a null
          --  transition will be built for blocked processes
          --  when verifiying with fairness; see SMC, p. 183
          if Options.Execution_Mode = Options.Fairness then
            P.Transition_List(Byte(P.Count)) :=
              (Statement => new String'(Utilities.Pad("null")),
               Code_Size => 1,
               Byte_Code => new Byte_Code_Array'((Compile_Global.noop, 0, 0), others => <>),
               others => 0);
             P.Count := P.Count + 1;
          end if;
        end;

        Processes := Processes + 1;

        -- Replicate transitions for multiple active processes
        if Active > 1 then
          Active_Proctypes(Active);
        end if;

      -- There can be numbers and strings within the transitions
      elsif Ada.Strings.Fixed.Index(S, "number=") = 1 then
        Symbol_Tables.Set_Number(S);
      elsif Ada.Strings.Fixed.Index(S, "string=") = 1 then
        Symbol_Tables.Set_String(S);
      end if;
    end loop;
  exception
    when E:others =>
      raise Unexpected_Input with "automata file error: " &
            Ada.Exceptions.Exception_Name(E) & ":" &
            Ada.Exceptions.Exception_Message(E);
  end Read;

  -- Modify the byte code to use immediate data for constants
  --   "iconst i" means push the i'th constant;
  --   replace i by the value of the i'th constant
  procedure Fix_Load_Constant is
    use type Compile_Global.Opcode;
  begin
    -- For each byte code B in each transition T in each process P
    for P in 0 .. Processes-1 loop
      for T in 0 .. Program(P).Count-1 loop
        if Program(P).Transition_List(T).Code_Size /= 0 then
          for B in 0 .. Program(P).Transition_List(T).Code_Size-1 loop
            declare
              Code: Byte_Code renames 
                      Program(P).Transition_List(T).Byte_Code(B);
            begin
              if Code.Operator = Compile_Global.iconst then
                Code.Operand1 := Symbol_Tables.Get_Number(Code.Operand1);
              end if;
            end;
          end loop;
        end if;
      end loop;
    end loop;
  end Fix_Load_Constant;

  -- Cache process/state pairs that are accepting for efficiency
  procedure Set_Accept_Transitions is
    P: Byte := Processes - 1;   -- Index of current process
    Exists: Boolean;            -- Flag if state already exists
  begin
    for T in 0..Program(P).Count-1 loop
       if Program(P).Transition_List(T).Accept_Label = 1 then
         Exists := False;
         for A in 0 .. Accept_Count-1 loop
           if Accept_Table(A) =
             (P, Program(P).Transition_List(T).Source) then
             Exists := True;
             exit;
           end if;
         end loop;
         if not Exists then
           Accept_Table(Accept_Count) :=
             (P, Program(P).Transition_List(T).Source);
           Accept_Count := Accept_Count + 1;
         end if;
       end if;
    end loop;
  end Set_Accept_Transitions;

  -- Translate LTL formula into transitions
  --   Read the LTL formula from a file
  procedure Translate_LTL is
    LTL_Transitions: Transition_Array := 
      LTL.LTL_To_Automaton(Files.LTL_File_Name.all);
    Count: Byte := LTL_Transitions'Length;
  begin
    if Count > Config.Transition_Index'Last then
      raise Internal_Error with "too many transitions for LTL formula";
    -- The LTL formula will add a process; check if there is room
    elsif Processes > Config.Process_Index'Last then
      raise Internal_Error with "too many processes";
    end if;

    -- Store the transitions in the Program table as an addition process
    Program(Processes) := (
      Identifier        => Utilities.Pad(":never:"),
      Transition_List   =>
        LTL_Transitions &
        Transition_Array'(Count..Config.Transition_Index'Last => <>),
        Initial_State   => 0,
        Count           => Count);

    -- This process is the never process
    Never := Processes;
    Processes := Processes + 1;
    Set_Accept_Transitions;
  end Translate_LTL;
end Automata;
