-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
with Ada.Text_IO, Ada.Exceptions;
with Automata.Display, Execute.Verify.Safety, Execute.Verify.Acceptance;
with Hash_Tables, Location_Stack, Options;
with State_Stack, Symbol_Tables, Utilities; 
package body Execute.Verify is
  use Global;

  -- In order to find more than one error, execute the
  --   verification from a separate function
  -- Set Continue to true if more errors should be searched for
  procedure One_Verification(Continue: out Boolean) is
    use type Options.Execution_Type;
    use Ada.Exceptions;
  begin
    Continue := False;
    if Options.Execution_Mode = Options.Safety then
      Safety.Search;
    elsif Options.Execution_Mode = Options.Acceptance or
          Options.Execution_Mode = Options.Fairness then
      Acceptance.Search;
    end if;
    Ada.Text_IO.Put_Line("verification terminated=successfully,");
  exception
    when E: others =>
      Ada.Text_IO.Put_Line(
        "verification terminated=" &
        Exception_Message(E) & "," &
        Utilities.Line_Statement(
          Exception_Transition.Line_Number, 
          Exception_Transition.Statement.all) & ",");

      --  Verification terminated with Counterexample_Error
      if Exception_Identity(E) = Counterexample_Error'Identity then
        Error_Count := Error_Count + 1;
        -- Put the trail and continue if Error_Number = 0 (find all errors)
        if Options.Error_Number = 0 then
          Location_Stack.Put_Trail(Error_Count, Start_Of_Acceptance_Cycle);
          Continue := True;
        -- If reached the required Error_Number, put the trail and stop
        elsif Error_Count = Options.Error_Number then
          Location_Stack.Put_Trail(Error_Count, Start_Of_Acceptance_Cycle);
        -- Continue to find next error
        else
          Continue := True;
        end if;
      end if;
  end One_Verification;

  procedure Verify is
    Inserted: Boolean;          -- For hash table (not used)
    Continue: Boolean := True;  -- For finding m'th or all errors
  begin
    Error_Count := 0;
    Start_Of_Acceptance_Cycle := -1;

    -- Initialize the data structures
    -- Initializing the state stack also checks the size of the state vector
    Location_Stack.Initialize;
    Hash_Tables.Initialize;
    State_Stack.Initialize(
      Automata.Processes, Symbol_Tables.Get_Data_Size);

    -- Initialize the verification
    Current := State_Vectors.Get_Initial_State_Vector;
    Hash_Tables.Put_State(Current, Inserted);
    State_Stack.Push(Current);
    Get_And_Push_Transitions;

    -- Verify until the m'th error or all errors
    while Continue loop
      One_Verification(Continue);
    end loop;
  end Verify;

  -- Put the sizes of the stacks and the hash table
  procedure Put_Sizes is
  begin
    State_Stack.Put_Stack;
    Location_Stack.Put_Stack;
    Hash_Tables.Put_Hash;
  end Put_Sizes;

  -- A state is accepting if any the transitions from state S
  --   has an accept label
  -- Check in reverse order because currently only never claims
  --   have accept labels
  function Is_Accept_State(S: State_Vectors.State_Vector)
    return Boolean is
  begin
    for P in reverse 0 .. Automata.Processes-1 loop
      if Automata.Is_Accept(P, S.Process(P)) then
        return True;
      end if;
    end loop;
    return False;
  end Is_Accept_State;

  -- If in an invalid end state or a completed never claim, raise an exception
  procedure Check_End_State is
  begin
    if Options.Display(Options.Executable) then
      Ada.Text_IO.Put_Line("end state=" & 
        Utilities.To_Lower(End_Type'Image(End_State)) & ",");
    end if;

    if End_State = Valid then
      return;
    elsif End_State = Invalid then
      raise Counterexample_Error with "invalid end state";
    elsif End_State = Never then
      raise Counterexample_Error with "never claim complete";
    end if;
  end Check_End_State;

  -- Get the transitions from the current state and push on stack
  procedure Get_And_Push_Transitions is
    use type Options.Execution_Type;

    -- Push all the executable transitions in reverse order
    procedure Push_All_Transitions is
    begin
      if Automata.Never = None then
        for I in reverse 0..L_Rec.Count-1 loop
          Location_Stack.Push(
            Location_Stack.Location_Item'(
              L       => L_Rec.Location_Array(I),
              Never   => <>,
              Visited => <>,
              Last    => I=L_Rec.Count-1));
        end loop;
      else
        -- If there is a never claim, create the product of the
        --   transitions up to Never_Index and at or after Never_Index
        for I in reverse 0..L_Rec.Never_Index-1 loop
          for N in reverse L_Rec.Never_Index .. L_Rec.Count-1 loop
            Location_Stack.Push(
              Location_Stack.Location_Item'(
                L       => L_Rec.Location_Array(I),
                Never   => L_Rec.Location_Array(N).Transition,
                Visited => <>,
                Last    => I=L_Rec.Never_Index-1 and N=L_Rec.Count-1));
          end loop;
        end loop;
      end if;
    end Push_All_Transitions;

    -- When checking fairness, if all transitions for the i'th process are
    --   blocked in the i'th copy, a null transition must be added
    -- See page 183 of The Spin Model Checker
    procedure Add_Null_Transition is
      I: Byte := Current.Fair - 1;       -- The i'th copy
    begin
      -- Null transitions are not needed in the 0th or the last copy
      if Current.Fair = 0 or Current.Fair = Unfolded then
        return;
      end if;
  
      -- Look for transitions for i'th process
      for P in 0 .. L_Rec.Never_Index - 1 loop
        -- If one is found, return
        if L_Rec.Location_Array(P).Process = I then
          return;
        end if;
      end loop;
  
      -- Make room for the null transition before the never transitions
      L_Rec.Location_Array(L_Rec.Never_Index+1 .. L_Rec.Count+1) :=
        L_Rec.Location_Array(L_Rec.Never_Index .. L_Rec.Count);
  
      -- Set the source/target state in the null transition
      --   and put it in the executable transitions
      L_Rec.Location_Array(L_Rec.Never_Index) :=
        (I, Automata.Set_Null_Transition(
              I, State_Vectors.Get_Process_Location_Value(Current, I)));
      L_Rec.Never_Index := L_Rec.Never_Index + 1;
      L_Rec.Count := L_Rec.Count + 1;

      if Options.Display(Options.Executable) then
        Ada.Text_IO.Put("executable transitions (with null transition)=");
        Automata.Display.Put_All_Locations(L_Rec, Atomic);
      end if;
    end Add_Null_Transition;

  begin  -- Get_And_Push_Transitions
    Get_Executable_Transitions;

    -- Error if the never claim is terminated
    if End_State = Never then
      raise Counterexample_Error with "never claim terminated";
    end if;

    -- Pop the state stack if there are:
    --   no transitions or
    --     there is a never claim and there are transitions
    --       only from never claim or
    --       no transitions from never claim
    if L_Rec.Count = 0 or
        (Automata.Never /= None and then 
        (L_Rec.Never_Index = 0 or L_Rec.Never_Index = None)) then
      if Options.Execution_Mode = Options.Safety then
        Check_End_State;
      end if;
      State_Stack.Pop(State_Stack.No_Transitions_Available);
    else
      -- Add null transitions for fairness
      if Options.Execution_Mode = Options.Fairness then
        Add_Null_Transition;
      end if;
      -- Otherwise push all the transitions
      Push_All_Transitions;
    end if;
  end Get_And_Push_Transitions;
end Execute.Verify;
