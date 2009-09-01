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

with Ada.Assertions;
with Ada.Characters.Latin_1;
with Ada.IO_Exceptions;

with Types.Lexer; use Types.Lexer;

use type Types.Lexer.Token_Type;
use type Types.Lexer.Bounded_String_Hashed_Map.Cursor;

package body Lexer is

   procedure Lexical_Error_Output( Message : String ) is
   begin
     Logger.Error ( Message );
     raise Generic_Lexical_Error with Message;
   end Lexical_Error_Output;

   procedure Set_Source_Filename (filename : String) is
   begin
      Ada.Text_IO.Open (Source_File, Ada.Text_IO.In_File, filename);
      -- I think this is an appropriate place
      Set_Reserved_Keywords;
      Set_Special_Names;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         Lexical_Error_Output ("File cannot be opened! Please try again. Abandoning lexing...");
      when others =>
         Lexical_Error_Output ("Unforeseen Ada IO exception! Abandoning lexing...");
   end Set_Source_Filename;

   procedure Set_Reserved_Keywords is
   begin
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("active"),
         Types.Lexer.Token_Active);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("assert"),
         Types.Lexer.Token_Assert);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("atomic"),
         Types.Lexer.Token_Atomic);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("bit"),
         Types.Lexer.Token_Bit);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("bool"),
         Types.Lexer.Token_Bool);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("break"),
         Types.Lexer.Token_Break);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("byte"),
         Types.Lexer.Token_Byte);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("c_code"),
         Types.Lexer.Token_C_Code);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("c_decl"),
         Types.Lexer.Token_C_Decl);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("c_expr"),
         Types.Lexer.Token_C_Expr);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("c_state"),
         Types.Lexer.Token_C_State);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("c_track"),
         Types.Lexer.Token_C_Track);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("chan"),
         Types.Lexer.Token_Chan);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("d_proctype"),
         Types.Lexer.Token_D_Proctype);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("d_step"),
         Types.Lexer.Token_D_Step);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("do"),
         Types.Lexer.Token_Do);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("else"),
         Types.Lexer.Token_Else);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("empty"),
         Types.Lexer.Token_Empty);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("enabled"),
         Types.Lexer.Token_Enabled);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("eval"),
         Types.Lexer.Token_Eval);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("false"),
         Types.Lexer.Token_False);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("fi"),
         Types.Lexer.Token_Fi);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("full"),
         Types.Lexer.Token_Full);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("goto"),
         Types.Lexer.Token_Goto);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("hidden"),
         Types.Lexer.Token_Hidden);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("if"),
         Types.Lexer.Token_If);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("init"),
         Types.Lexer.Token_Init);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("int"),
         Types.Lexer.Token_Int);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("len"),
         Types.Lexer.Token_Len);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("local"),
         Types.Lexer.Token_Local);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("mtype"),
         Types.Lexer.Token_Mtype);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("nempty"),
         Types.Lexer.Token_Nempty);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("never"),
         Types.Lexer.Token_Never);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("nfull"),
         Types.Lexer.Token_Nfull);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("notrace"),
         Types.Lexer.Token_Notrace);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("np_"),
         Types.Lexer.Token_Np_Underscore);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("od"),
         Types.Lexer.Token_Od);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("of"),
         Types.Lexer.Token_Of);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("pc_value"),
         Types.Lexer.Token_Pc_Value);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("pid"),
         Types.Lexer.Token_Pid);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("print"),
         Types.Lexer.Token_Print);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("printf"),
         Types.Lexer.Token_Printf);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("printm"),
         Types.Lexer.Token_Printm);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("priority"),
         Types.Lexer.Token_Priority);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("proctype"),
         Types.Lexer.Token_Proctype);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("provided"),
         Types.Lexer.Token_Provided);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("run"),
         Types.Lexer.Token_Run);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("short"),
         Types.Lexer.Token_Short);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("show"),
         Types.Lexer.Token_Show);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("skip"),
         Types.Lexer.Token_Skip);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("timeout"),
         Types.Lexer.Token_Timeout);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("trace"),
         Types.Lexer.Token_Trace);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("true"),
         Types.Lexer.Token_True);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("typedef"),
         Types.Lexer.Token_Typedef);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("unless"),
         Types.Lexer.Token_Unless);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("unsigned"),
         Types.Lexer.Token_Unsigned);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("xr"),
         Types.Lexer.Token_Xr);
      Keyword_Table.Insert
        (Types.Lexer.Bound_String.To_Bounded_String ("xs"),
         Types.Lexer.Token_Xs);
   end Set_Reserved_Keywords;

   -- TODO: is Promela case-insensitive to names?
   procedure Set_Special_Names is
    dummy : Types.Lexer.Bounded_String_Hashed_Set.Cursor;
   begin
    dummy := Types.Lexer.Add_To_Name_Table
      (Types.Lexer.Bound_String.To_Bounded_String("_"));
    dummy := Types.Lexer.Add_To_Name_Table
      (Types.Lexer.Bound_String.To_Bounded_String("_last"));
    dummy := Types.Lexer.Add_To_Name_Table
      (Types.Lexer.Bound_String.To_Bounded_String("_nr_pr"));
    dummy := Types.Lexer.Add_To_Name_Table
      (Types.Lexer.Bound_String.To_Bounded_String("_pid"));
    dummy := Types.Lexer.Add_To_Name_Table
      (Types.Lexer.Bound_String.To_Bounded_String("accept"));
    dummy := Types.Lexer.Add_To_Name_Table
      (Types.Lexer.Bound_String.To_Bounded_String("progress"));
    dummy := Types.Lexer.Add_To_Name_Table
      (Types.Lexer.Bound_String.To_Bounded_String("STDIN"));
   end Set_Special_Names;

   -- originally used Gnat.OS_Lib
   procedure Read_Character is
      s : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
      if Buffer_Counter = Buffer_Size + 1 then
         Buffer := Ada.Strings.Unbounded.Null_Unbounded_String;
         -- I can't control the number of characters I read without
         --sacrificing newlines, so the idea is to read a certain number of
         --lines. Call it variable buffering.
         for I in 1 .. Buffer_Lines loop
            exit when Ada.Text_IO.End_Of_File (Source_File);
            s := Ada.Text_IO.Unbounded_IO.Get_Line (Source_File);
            Ada.Strings.Unbounded.Append (Buffer, s);
            -- add LF
            Ada.Strings.Unbounded.Append (Buffer, Ada.Characters.Latin_1.LF);
         end loop;
         if Ada.Text_IO.End_Of_File (Source_File) then
            -- add EOT
            Ada.Strings.Unbounded.Append (Buffer, Ada.Characters.Latin_1.EOT);
            -- clean up
            Cleanup;
            if State > 2 then
                Lexical_Error_Output
                ("Unexpected EOF encountered at line " &
                 Integer'Image (Line_Number) &
                 "in State " &
                 Integer'Image (State));
            end if;
         end if;
         Buffer_Size    := Ada.Strings.Unbounded.Length (Buffer);
         Buffer_Counter := 1;
      end if;

      Peek           :=
         Ada.Strings.Unbounded.Element (Buffer, Buffer_Counter);
      Buffer_Counter := Buffer_Counter + 1;

   end Read_Character;
   
   procedure Fill_Buffer_With_String( S : String ) is
   begin
    if  Ada.Text_IO.Is_Open( Source_File ) = True   or else
        Current_Token.Kind /= Types.Lexer.Token_EOF then
      Logger.Error( "Fill_Buffer_With_String: Source file is still open!" );
    end if;
    Reset;
    Line_Number := 1;
    Buffer := Ada.Strings.Unbounded.To_Unbounded_String( S );
    Ada.Strings.Unbounded.Append( Buffer, Ada.Characters.Latin_1.EOT );
    Buffer_Size    := Ada.Strings.Unbounded.Length( Buffer );
    Buffer_Counter := 1;
   end Fill_Buffer_With_String;

   procedure Reset is
   begin
      Current_Token.Kind          := Types.Lexer.Token_Null;
      Current_Token.Lexeme_Cursor := Types.Lexer.Bounded_String_Hashed_Set.No_Element;
      Current_Token.Value_Cursor  := Types.Lexer.Natural_Hashed_Set.No_Element;
   end Reset;

   procedure Scan is
      lexeme                   : Types.Lexer.Bound_String.Bounded_String :=
        Types.Lexer.Bound_String.Null_Bounded_String;
      lowercase                : Types.Lexer.Bound_String.Bounded_String :=
        Types.Lexer.Bound_String.Null_Bounded_String;
      value                    : Types.Lexer.Promela_Natural := 0;
      Keyword_Table_cursor     : Types.Lexer.Bounded_String_Hashed_Map.Cursor :=
        Types.Lexer.Bounded_String_Hashed_Map.No_Element;
      inserted                 : Boolean := False;
   begin
      -- return when EOF
      if Current_Token.Kind = Types.Lexer.Token_EOF then
        return;
      end if;
      -- tabula rasa
      Reset;
      -- initial state
      State := 1;
      loop
         case State is

         -- initial State
         when 1 =>
            if Retract = False then
               Read_Character;
            else
               Retract := False;
            end if;
            case Peek is
            when ' '                       |
                 Ada.Characters.Latin_1.CR |
                 Ada.Characters.Latin_1.LF |
                 Ada.Characters.Latin_1.HT =>
               State := 2;
            when 'a' .. 'z' | 'A' .. 'Z' | '_' =>
               Current_Token.Kind := Types.Lexer.Token_Name;
               Types.Lexer.Bound_String.Append (lexeme, Peek);
               State := 3;
            when '0' .. '9' =>
               Current_Token.Kind := Types.Lexer.Token_Number;
               value              := Character'Pos (Peek) -
                                     Character'Pos ('0');
               State              := 4;
            when '"' =>
               Current_Token.Kind := Types.Lexer.Token_String;
               State              := 5;
            when '{' =>
               Current_Token.Kind := Types.Lexer.Token_Left_Curly_Bracket;
               exit;
            when '}' =>
               Current_Token.Kind := Types.Lexer.Token_Right_Curly_Bracket;
               exit;
            when '[' =>
               Current_Token.Kind := Types.Lexer.Token_Left_Square_Bracket;
               exit;
            when ']' =>
               Current_Token.Kind := Types.Lexer.Token_Right_Square_Bracket;
               exit;
            when '(' =>
               Current_Token.Kind := Types.Lexer.Token_Left_Parentheses;
               exit;
            when ')' =>
               Current_Token.Kind := Types.Lexer.Token_Right_Parentheses;
               exit;
            when ';' =>
               Current_Token.Kind := Types.Lexer.Token_Semicolon;
               exit;
            when ',' =>
               Current_Token.Kind := Types.Lexer.Token_Comma;
               exit;
            when '.' =>
               Current_Token.Kind := Types.Lexer.Token_Dot;
               exit;
            when '@' =>
               Current_Token.Kind := Types.Lexer.Token_Alias;
               exit;
            when '*' =>
               Current_Token.Kind := Types.Lexer.Token_Multiply;
               exit;
            when '%' =>
               Current_Token.Kind := Types.Lexer.Token_Modulus;
               exit;
            when '^' =>
               Current_Token.Kind := Types.Lexer.Token_Xor_Bitwise;
               exit;
            when '~' =>
               Current_Token.Kind := Types.Lexer.Token_Ones_Complement;
               exit;
            when ':' =>
               State := 6;
            when '+' =>
               State := 7;
            when '&' =>
               State := 8;
            when '|' =>
               State := 9;
            when '=' =>
               State := 10;
            when '!' =>
               State := 11;
            when '?' =>
               State := 12;
            when '-' =>
               State := 13;
            when '<' =>
               State := 14;
            when '>' =>
               State := 15;
            when '/' =>
               State := 16;
            when ''' =>
               State := 19;
            when '#' =>
               Lexical_Error_Output
                 ("'#' detected at line " &
                  Integer'Image (Line_Number) & 
                  "! Perhaps source needs to be run through a C preprocessor first? Abandoning lexing..."
                 );
            -- EOT
            when Ada.Characters.Latin_1.EOT =>
               Current_Token.Kind := Types.Lexer.Token_EOF;
               exit;
            when others =>
               Lexical_Error_Output
                 ("Unknown character '" &
                  Peek &
                  "' detected at line " &
                  Integer'Image (Line_Number) & 
                  "! Is the input really a Promela source? Abandoning lexing...");
            end case;

         -- whitespace DFA
         when 2 =>
            if Peek = Ada.Characters.Latin_1.LF then
               Line_Number        := Line_Number + 1;
               Current_Token.Kind := Types.Lexer.Token_Newline;
               -- this is crucial!
               exit;
            end if;
            Read_Character;
            case Peek is
               when ' '                       |
                    Ada.Characters.Latin_1.CR |
                    Ada.Characters.Latin_1.LF |
                    Ada.Characters.Latin_1.HT =>
                  State := 2;
               when others =>
                  Retract := True;
                  State   := 1;
            end case;

         -- Identifier DFA
         when 3 =>
            Read_Character;
            case Peek is
               when 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' =>
                  Types.Lexer.Bound_String.Append (lexeme, Peek);
                  -- redundant but consistent
                  State := 3;
               when others =>
                  Retract := True;
                  -- NOTE: according to my little test, SPIN does not care about
                  -- case insensitivity; i.e. "a" /= "A", "proctype" /= "Proctype"
                  Keyword_Table_cursor := Keyword_Table.Find (lexeme);
                  if Keyword_Table_cursor /=
                     Types.Lexer.Bounded_String_Hashed_Map.No_Element
                  then
                     Current_Token.Kind := Types.Lexer.Bounded_String_Hashed_Map.Element
                     (Keyword_Table_cursor);
                  else
                     Current_Token.Kind := Types.Lexer.Token_Name;
                     Current_Token.Lexeme_Cursor := Types.Lexer.Add_To_Name_Table( lexeme );
                  end if;
                  exit;
            end case;

         -- Number DFA
         when 4 =>
            Read_Character;
            case Peek is
               when '0' .. '9' =>
                  value := (value * 10) +
                           Character'Pos (Peek) -
                           Character'Pos ('0');
                  -- redundant but consistent
                  State := 4;
               when others =>
                  Retract            := True;
                  Current_Token.Kind := Types.Lexer.Token_Number;
                  Current_Token.Value_Cursor := Types.Lexer.Add_To_Natural_Table( value );
                  exit;
            end case;

         -- String DFA
         when 5 =>
            Read_Character;
            case Peek is
               when '"' =>
                  Current_Token.Kind := Types.Lexer.Token_String;
                  Current_Token.Lexeme_Cursor := Types.Lexer.Add_To_String_Table( lexeme );
                  exit;
               when others =>
                  Types.Lexer.Bound_String.Append (lexeme, Peek);
                  State := 5;
            end case;

         -- Guard/Colon DFA
         when 6 =>
            Read_Character;
            case Peek is
               when ':' =>
                  Current_Token.Kind := Types.Lexer.Token_Guard;
                  exit;
               when others =>
                  Retract            := True;
                  Current_Token.Kind := Types.Lexer.Token_Colon;
                  exit;
            end case;

         -- Add/Increment DFA
         when 7 =>
            Read_Character;
            case Peek is
               when '+' =>
                  Current_Token.Kind := Types.Lexer.Token_Increment;
                  exit;
               when others =>
                  Retract            := True;
                  Current_Token.Kind := Types.Lexer.Token_Add;
                  exit;
            end case;

         -- And_Bitwise/And_Logical DFA
         when 8 =>
            Read_Character;
            case Peek is
               when '&' =>
                  Current_Token.Kind := Types.Lexer.Token_And_Logical;
                  exit;
               when others =>
                  Retract            := True;
                  Current_Token.Kind := Types.Lexer.Token_And_Bitwise;
                  exit;
            end case;

         -- Or_Bitwise/Or_Logical DFA
         when 9 =>
            Read_Character;
            case Peek is
               when '|' =>
                  Current_Token.Kind := Types.Lexer.Token_Or_Logical;
                  exit;
               when others =>
                  Retract            := True;
                  Current_Token.Kind := Types.Lexer.Token_Or_Bitwise;
                  exit;
            end case;

         -- Equals/Assignment DFA
         when 10 =>
            Read_Character;
            case Peek is
               when '=' =>
                  Current_Token.Kind := Types.Lexer.Token_Equals;
                  exit;
               when others =>
                  Retract            := True;
                  Current_Token.Kind := Types.Lexer.Token_Assignment;
                  exit;
            end case;

         -- Not_Or_Send_1/Not_Equals/Send_2 DFA
         when 11 =>
            Read_Character;
            case Peek is
               when '!' =>
                  Current_Token.Kind := Types.Lexer.Token_Send_2;
                  exit;
               when '=' =>
                  Current_Token.Kind := Types.Lexer.Token_Not_Equals;
                  exit;
               when others =>
                  Retract            := True;
                  Current_Token.Kind := Types.Lexer.Token_Not_Or_Send_1;
                  exit;
            end case;

         -- Receive_1/Receive_2 DFA
         when 12 =>
            Read_Character;
            case Peek is
               when '?' =>
                  Current_Token.Kind := Types.Lexer.Token_Receive_2;
                  exit;
               when others =>
                  Retract            := True;
                  Current_Token.Kind := Types.Lexer.Token_Receive_1;
                  exit;
            end case;

         -- Subtract/Decrement/Implies DFA
         when 13 =>
            Read_Character;
            case Peek is
               when '-' =>
                  Current_Token.Kind := Types.Lexer.Token_Decrement;
                  exit;
               when '>' =>
                  Current_Token.Kind := Types.Lexer.Token_Implies;
                  exit;
               when others =>
                  Retract            := True;
                  Current_Token.Kind := Types.Lexer.Token_Subtract;
                  exit;
            end case;

         -- Less_Than/Less_Than_Or_Equals_To/Left_Shift DFA
         when 14 =>
            Read_Character;
            case Peek is
               when '<' =>
                  Current_Token.Kind := Types.Lexer.Token_Left_Shift;
                  exit;
               when '=' =>
                  Current_Token.Kind := Types.Lexer.Token_Less_Than_Or_Equal_To;
                  exit;
               when others =>
                  Retract            := True;
                  Current_Token.Kind := Types.Lexer.Token_Less_Than;
                  exit;
            end case;

         -- Greater_Than/Greater_Than_Or_Equals_To/Right_Shift DFA
         when 15 =>
            Read_Character;
            case Peek is
               when '>' =>
                  Current_Token.Kind := Types.Lexer.Token_Right_Shift;
                  exit;
               when '=' =>
                  Current_Token.Kind := Types.Lexer.Token_Greater_Than_Or_Equal_To;
                  exit;
               when others =>
                  Retract            := True;
                  Current_Token.Kind := Types.Lexer.Token_Greater_Than;
                  exit;
            end case;

         -- Slash/Comment DFA; TODO: consider C-style comments?
         when 16 =>
            Read_Character;
            case Peek is
               when '*' =>
                  State := 17;
               when others =>
                  Retract            := True;
                  Current_Token.Kind := Types.Lexer.Token_Divide;
                  exit;
            end case;
         when 17 =>
            Read_Character;
            case Peek is
               when '*' =>
                  State := 18;
               when Ada.Characters.Latin_1.LF =>
                  -- crucial to keep proper track of line number accounting
                  Line_Number := Line_Number + 1;
                  -- redundant but consistent
                  State := 17;
               when others =>
                  -- redundant but consistent
                  State := 17;
            end case;
         -- if I fold this State into 19, i might as well fold the others too
         when 18 =>
            Read_Character;
            case Peek is
               when '/' =>
                  -- pretend nothing ever happened
                  State := 1;
               when others =>
                  -- not at the end of comment yet
                  State := 17;
            end case;
         
         -- Character DFA
         when 19 =>
            Read_Character;
            value := (value * 10) +
                     Character'Pos (Peek);
            State := 20;
         when 20 =>
            Read_Character;
            case Peek is
               when ''' =>
                  Current_Token.Kind := Types.Lexer.Token_Number;
                  Current_Token.Value_Cursor := Types.Lexer.Add_To_Natural_Table( value );
                  exit;
               when others =>
                  Lexical_Error_Output
                    ("Character is not closed at line " &
                     Integer'Image (Line_Number) &
                     "!");
            end case;

         end case;
      end loop;
      -- Line numbering
      Current_Token.Line_Number := Line_Number;
   
   exception
    when Generic_Lexical_Error =>
      raise;
    when Ada.Strings.Length_Error =>
      Lexical_Error_Output("The length of the name or string '" &
        Types.Lexer.Bound_String.To_String(Lexeme) &
        "' at line number " &
        Integer'Image(Line_Number) &
        " is beyond the scope of this lexer! Abandoning lexing...");
      Cleanup;
    when others =>
      Lexical_Error_Output("Unforeseen error at line number " &
        Integer'Image(Line_Number) &
        "! Abandoning lexing...");
      Cleanup;
   
   end Scan;

   procedure Cleanup is
   begin
      Ada.Text_IO.Close (Source_File);
   end Cleanup;

end Lexer;
