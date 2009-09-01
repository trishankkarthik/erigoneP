-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
with Ada.Text_IO, Ada.Strings.Fixed;
with Compile_Global, Config, Options, Symbol_Tables, Utilities;
package body Execute.Statement is
  use Global, State_Vectors, Compile_Global;

  -- Store values from send statements:
  --   For rendezvous channels, store until taken by receive
  --   For sorted send, the values are used for match with channel
  Message_Buffer: Byte_Array_Base(Config.Message_Index);

  -- For receive statements, store which arguments
  --   are variables (1) and which values (0)
  -- Assumes load_address instruction is used for variables
  Is_Variable:    Byte_Array_Base(Config.Message_Index);
  Arg_Count:      Byte;

  -- A small local stack is used for interpretation of the byte code
  Stack: array(Config.Interpret_Index) of Global.Byte;
  Top:   Config.Interpret_Index;

  -- Evaulate the byte Code in the Current state
  -- For a statement, it also updates the state vector
  --   so the parameter Current is of mode in-out
  procedure Evaluate(
      Current:    in out State_Vectors.State_Vector; 
      Size:       Global.Byte;
      Code:       access Automata.Byte_Code_Array;
      Result:     out Global.Byte) is

    B:     Automata.Byte_Code;   -- Byte code being interpreted
    Op, Op1, Op2: Global.Byte;   -- Temporary variables for operands
    
    procedure Push(N: Global.Byte) is
    begin
      Stack(Top) := N;
      Top := Top + 1;
    end Push;
    
    function Pop return Global.Byte is
    begin
      Top := Top - 1;
      return Stack(Top);
    end Pop;
    
    function Get_Top return Global.Byte is
    begin
      return Stack(Top-1);
    end Get_Top;

    pragma Inline(Push, Pop, Get_Top);

    -- Print data for debugging the interpreter
    procedure Debug is
    begin
      Ada.Text_IO.Put("interpreting=" & 
        Utilities.To_Lower(Opcode'Image(B.Operator)) & 
        Byte'Image(B.Operand1) & Byte'Image(B.Operand2) & ",");
      Ada.Text_IO.Put("stack=");
      for I in 1..Top-1 loop
        Utilities.Put(Stack(I));
      end loop;
      Ada.Text_IO.New_Line;
    end Debug;

    -- Interpret printf("format specifier string", arguments ...)
    -- The first operand is the index into the string table
    --   for the format specifier string
    -- The second operand is the count of the arguments
    --   which have been pushed onto the stack
    procedure Interpret_Printf is
      use Options, Ada.Text_IO, Ada.Strings.Fixed;
      -- Get the format specifier string
      S:         String := Symbol_Tables.Get_String(B.Operand1);
      Specifier: Natural;              -- Index of a specifier in S
      Start:     Positive := S'First;  -- Index into S
    begin
      -- Do not print during verification
      if   Execution_Mode = Acceptance or
           Execution_Mode = Fairness   or
           Execution_Mode = Safety then
         return;
      end if;
      for I in 1 .. B.Operand2 loop   -- For each argument
        Specifier := Index(S(Start..S'Last), "%");
        if Specifier = 0 then
          raise Unexpected_Input with "not enough format specifiers";
        end if;
        -- Put the string up to the specifier
        Put(S(Start..Specifier-1));
        -- Put the data according to the specifier
        if S(Specifier+1) = 'd' then
          Put(Trim(Global.Byte'Image(Pop), Side => Ada.Strings.Both));
        elsif S(Specifier+1) = 'c' then
          Put(Character'Val(Pop));
        elsif S(Specifier+1) = 'e' then
          Put(Symbol_Tables.Get_MType(Pop));
        else
          raise Unexpected_Input with "unrecognized format specifier";
        end if;
        Start := Specifier + 2;
      end loop;

      -- Print the remainder of the specifier string,
      --   and put a new line if there is \n at the end
      if S(S'Last-1..S'Last) = "\n" then
        Put_Line(S(Start..S'Last-2));
      else
        Put(S(Start..S'Last));
      end if;
    end Interpret_Printf;

    -- Interpret the channel instructions
    procedure Channel_Instructions is
      use Symbol_Tables;
      -- The index of the channel for the instruction
      Channel_Index:       Byte := Pop;
      -- The size of the channel buffer
      Buffer_Size:         Byte := Get_Channel(Channel_Index).Buffer_Size;
      -- The offset of the channel buffer in the state vector
      Offset_Of_Channel:   Byte := Get_Channel(Channel_Index).Offset;
      -- The number of fields in a message
      Fields_In_Message: Byte := Get_Channel(Channel_Index).Elements;
      -- The number of messages in the channel buffer
      Elements_In_Channel: Byte := Get_Byte_Variable_Value(
                                     Current, Offset_Of_Channel);
      -- For receive:
      Elements_To_Search:  Byte;     -- Fields in message or 1 for FIFO
      Random_Index:        Byte;     -- Index where random found
      Success:             Boolean;  -- Success during random search
    begin
      --------------- Rendezvous channel -------------
      if Buffer_Size = 0 then

        ------------- Send -------------
        --  Set Handshake to channel index and
        --    pop values to Message_Buffer to wait for receive
        if B.Operator = fifo_send then
            Handshake := Channel_Index;
            for I in 0 .. Fields_In_Message-1 loop
              Message_Buffer(I) := Pop;
            end loop;

        ------------- Receive -------------
        -- Reset Handshake to release rendezvous
        -- Move elements from stack to variables in state vector
        -- The addresses within the state vector are on the stack
        -- The values have been saved by the send instruction
        elsif B.Operator = move_fifo_receive then
            Handshake := 0;
            for I in 0 .. B.Operand1-1 loop
              Update_Byte_Variable(Current, Pop, Message_Buffer(I));
            end loop;
        end if;

      ------------- Buffered channel -------------
      else
        --------------- Channel expressions -------------
        case B.Operator is
          when channel_len .. channel_nfull =>
            Op  := 0;
            case B.Operator is
              when channel_len    =>
                Op := Elements_In_Channel;
              when channel_empty  =>
                if Elements_In_Channel =  0 then Op := 1; end if;
              when channel_nempty =>
                if Elements_In_Channel /= 0 then Op := 1; end if;
              when channel_full   =>
                if Elements_In_Channel =  Buffer_Size then Op := 1; end if;
              when channel_nfull  =>
                if Elements_In_Channel /= Buffer_Size then Op := 1; end if;
              when others => null;
            end case;
            Push(Op);
            return;
          when others => null;
        end case;

        ------------- Send, receive and poll instructions -------------
        case B.Operator is

          ------------- FIFO Send -------------
          when fifo_send =>
            -- Move all elements from stack to channel in state vector
            --   Add 1 to offset to skip count of elements in channel
            --   and add Fields_In_Message*Elements_In_Channel
            --   to skip current contents of channel (for FIFO)
            for I in 0..B.Operand1-1 loop
              Update_Byte_Variable(
                Current, 
                Offset_Of_Channel+1 +
                  Fields_In_Message*Elements_In_Channel + I,
                Pop);
            end loop;

            -- Increment the number of elements in the channel
            Update_Byte_Variable(
              Current, Offset_Of_Channel, Elements_In_Channel+1);

          ------------- Sorted Send -------------
          when sorted_send =>
            -- Get all elements of the message from the stack
            for I in 0 .. Fields_In_Message-1 loop
              Message_Buffer(I) := Pop;
            end loop;

            -- Set a sentinel (255's) after the last message in the channel,
            --   so that the search will always succeed
            Update_Variable(
              Current, 
              Offset_Of_Channel+1 + 
                Fields_In_Message*Elements_In_Channel,
              (0..Fields_In_Message-1 => 255));

            -- Find the first message that is greater then the new one
            for I in 0 .. Elements_In_Channel loop
              if Message_Buffer(0..Fields_In_Message-1) <=
                 Get_Variable_Value(
                   Current,
                   Offset_Of_Channel+1 + 
                     Fields_In_Message*I,
                   Fields_In_Message) then

                 -- Move the rest of the messages to make room
                for J in reverse I .. Elements_In_Channel loop
                  Update_Variable(
                    Current, 
                    Offset_Of_Channel+1 + Fields_In_Message*J,
                    Get_Variable_Value(
                      Current, 
                      Offset_Of_Channel+1 + Fields_In_Message*(J-1),
                    Fields_In_Message));
                end loop;

                -- Move the new message into place
                Update_Variable(
                  Current,
                  Offset_Of_Channel+1 + 
                    Fields_In_Message*I,
                  Message_Buffer(0..Fields_In_Message-1));
                exit;
              end if;
            end loop;

            -- Increment the number of elements in the channel
            Update_Byte_Variable(
              Current, Offset_Of_Channel, Elements_In_Channel+1);

          ------------- Receive -------------
          when move_fifo_receive .. random_poll =>
            if B.Operator = move_fifo_receive or
               B.Operator = copy_fifo_receive or
               B.Operator = fifo_poll         then
              -- FIFO receive: check only the first element in the channel
              Elements_To_Search := 1;
              Random_Index       := 1;
            else
              -- Random receive: check all elements in the channel
              Elements_To_Search := Elements_In_Channel;
            end if;

            -- For random receive, we will need the arguments
            --   more than once, so pop and save them
            for F in 0..B.Operand1-1 loop
              Message_Buffer(F) := Pop;
            end loop;

            Channel_Element_Loop:
            for E in 0..Elements_To_Search-1 loop
              Success := True;

              Message_Field_Loop:
              for I in 0 .. B.Operand1-1 loop
                -- If the argument is a variable:
                if Is_Variable(B.Operand1-1-I) = 1 then
                  -- Copy elements from the channel
                  --   to variables in the state vector,
                  --   but not if poll instructions (side-effect free)
                  -- The offsets of the variables were popped to Message_Buffer
                  -- Add 1 to offset to skip count of elements in channel
                  if B.Operator /= fifo_poll and B.Operator /= random_poll then
                    Update_Byte_Variable(
                      Current,
                      Message_Buffer(I),
                      Get_Byte_Variable_Value(
                        Current,
                        Offset_Of_Channel+1 + E*Fields_In_Message+ I));
                  end if;

                -- The arugment is a value (constant or eval)
                else
                  -- Check if the value on the stack equals the value
                  --   in the channel; if not, set Result = 0
                  if Message_Buffer(I) /=
                     Get_Byte_Variable_Value(
                       Current,
                       Offset_Of_Channel+1 + E*Fields_In_Message+ I) then
                    Success := False;
                    exit Message_Field_Loop;
                  end if;
                end if;
              end loop Message_Field_Loop;

              Random_Index := E+1;  -- Loop index for use outside loop
              exit Channel_Element_Loop when Success;
            end loop Channel_Element_Loop;

            -- If move - not copy or poll - update channel in state vector
            if Success and
                 (B.Operator = move_fifo_receive or
                  B.Operator = move_random_receive) then
              -- Close up channel elements
              --   Move Random_Index   .. Elements_In_Channel elements
              --     to elements at Random_Index-1 .. Elements_In_Channel-1
              if Elements_In_Channel > 1 then
                for I in Random_Index .. Elements_In_Channel-1 loop
                  Update_Variable(
                    Current, 
                    Offset_Of_Channel+1 + (I-1)*Fields_In_Message,
                    Get_Variable_Value(
                      Current, 
                      Offset_Of_Channel+1 + I*Fields_In_Message,
                      Fields_In_Message));
                end loop;
              end if;

              -- Put zeros in place of deleted message for comparing
              --   equivalent state vectors in hash table
              Update_Variable(
                Current,
                Offset_Of_Channel+1 + (Elements_In_Channel-1)*Fields_In_Message,
                (0..Fields_In_Message-1 => 0));

              -- Decrement the number of elements in the channel
              Update_Byte_Variable(
                Current, Offset_Of_Channel, Elements_In_Channel-1);
            end if;  -- Move - not copy

          when others => null;
        end case;
      end if;  -- Rendezvous or buffered channel
      Push(Byte(Boolean'Pos(Success)));
    end Channel_Instructions;

  begin -- Evaluate
    -- Initialization for receive statements
    Arg_Count   := 0;
    Is_Variable := (others => 0);

    -- Zero element is not used so that Top can be decremented
    Top := 1;
    for IP in 0..Size-1 loop
      B := Code.all(IP);
      -- Debug;
      case B.Operator is
        when noop   => null;
        when assert => null;
        when halt   => raise Termination_Error with "halt executed";
        when printf => Interpret_Printf;

        -- Load immediate operands
        --   For receive statement, skip setting Is_Variable
        when iconst | bipush =>
          Push(B.Operand1);
          Arg_Count := Arg_Count + 1;

        -- Load address used for variables in receive statements
        --   Set the Is_Variable flag for this argument
        when load_address =>
          Push(B.Operand1);
          Is_Variable(Arg_Count) := 1;
          Arg_Count := Arg_Count + 1;

        when bit_load  | byte_load  =>
          Push(Get_Byte_Variable_Value(Current, B.Operand1));
          Arg_Count := Arg_Count + 1;

        when bit_store | byte_store =>
          Update_Byte_Variable(Current, B.Operand1, Pop);

        -- Array load/store:
        --   the index is on the top of stack
        --   the array offset is operand 1
        --   the size of and element is operand 2
        when bit_aload  | byte_aload  =>
          Push(Get_Byte_Variable_Value(
            Current, B.Operand1 + Pop*B.Operand2));
          Arg_Count := Arg_Count + 1;

        when bit_astore | byte_astore =>
          Op1 := B.Operand1 + Pop*B.Operand2;   -- Compute address
          Update_Byte_Variable(Current, Op1, Pop);

        -- Check array bound:
        --   the index is on the top of the stack (but don't pop!)
        --   the bound is operand 1
        when check_index =>
          if B.Operand1 <= Get_Top then
            raise Termination_Error with 
              "bounds error: index=" &
              Utilities.Trim(Byte'Image(Get_Top)) &
              " > length-1=" & Utilities.Trim(Byte'Image(B.Operand1-1));
          end if;

        -- Logical operators
        when logic_not =>
          if Pop = 0 then Push(1); else Push(0); end if;
        when logic_and | logic_or =>
          Op2 := Pop;
          Op1 := Pop;
          case B.Operator is
            when logic_and =>
              if Op1 /= 0 and Op2 /= 0 then Op := 1; else Op := 0; end if;
            when logic_or  =>
              if Op1 /= 0 or  Op2 /= 0 then Op := 1; else Op := 0; end if;
            when others => null;
          end case;
          Push(Op);
        when logic_else => Push(1);

        -- Increment/decrement
        when iinc | idec =>
          Op := Get_Byte_Variable_Value(Current, B.Operand1);
          case B.Operator is
            when iinc => Op := Op + B.Operand2;
            when idec => Op := Op - B.Operand2;
            when others => null;
          end case;
          Update_Byte_Variable(Current, B.Operand1, Op);

        -- Arithmetic operators
        when ineg => Push(-Pop);
        when iadd | isub | imul | idiv | irem =>
          Op2 := Pop;
          Op1 := Pop;
          case B.Operator is
            when iadd => Op := Op1 + Op2;
            when isub => Op := Op1 - Op2;
            when imul => Op := Op1 * Op2;
            when idiv => Op := Op1 / Op2;
            when irem => Op := Op1 rem Op2;
            when others => null;
          end case;
          Push(Op);

        -- Relational operators
        when icmpeq | icmpne | icmplt | icmple | icmpgt | icmpge =>
          Op2 := Pop;
          Op1 := Pop;
          Op  := 0;
          case B.Operator is
            when icmpeq => if Op1 =  Op2 then Op := 1; end if;
            when icmpne => if Op1 /= Op2 then Op := 1; end if;
            when icmplt => if Op1 <  Op2 then Op := 1; end if;
            when icmple => if Op1 <= Op2 then Op := 1; end if;
            when icmpgt => if Op1 >  Op2 then Op := 1; end if;
            when icmpge => if Op1 >= Op2 then Op := 1; end if;
            when others => null;
          end case;
          Push(Op);

        when fifo_send .. channel_nfull => Channel_Instructions;

        when others =>
          raise Internal_Error
            with "unrecognized byte code " & 
            Utilities.To_Lower(OpCode'Image(B.Operator));
      end case;
    end loop;

    -- Return the top of the stack
    Result := Pop;
  end Evaluate;
end Execute.Statement;
