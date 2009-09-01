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

package body Types.Parser is

  procedure   Init_Globals is
  begin
    Keyword_Table.Insert
      ( Types.Lexer.Token_Active,
        Types.Lexer.Bound_String.To_Bounded_String ("active")     );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Assert,
        Types.Lexer.Bound_String.To_Bounded_String ("assert")     );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Atomic,
        Types.Lexer.Bound_String.To_Bounded_String ("atomic")     );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Bit,
        Types.Lexer.Bound_String.To_Bounded_String ("bit")        );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Bool,
        Types.Lexer.Bound_String.To_Bounded_String ("bool")       );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Break,
        Types.Lexer.Bound_String.To_Bounded_String ("break")      );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Byte,
        Types.Lexer.Bound_String.To_Bounded_String ("byte")       );
    Keyword_Table.Insert
      ( Types.Lexer.Token_C_Code,
        Types.Lexer.Bound_String.To_Bounded_String ("c_code")     );
    Keyword_Table.Insert
      ( Types.Lexer.Token_C_Decl,
        Types.Lexer.Bound_String.To_Bounded_String ("c_decl")     );
    Keyword_Table.Insert
      ( Types.Lexer.Token_C_Expr,
        Types.Lexer.Bound_String.To_Bounded_String ("c_expr")     );
    Keyword_Table.Insert
      ( Types.Lexer.Token_C_State,
        Types.Lexer.Bound_String.To_Bounded_String ("c_state")    );
    Keyword_Table.Insert
      ( Types.Lexer.Token_C_Track,
        Types.Lexer.Bound_String.To_Bounded_String ("c_track")    );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Chan,
        Types.Lexer.Bound_String.To_Bounded_String ("chan")       );
    Keyword_Table.Insert
      ( Types.Lexer.Token_D_Proctype,
        Types.Lexer.Bound_String.To_Bounded_String ("d_proctype") );
    Keyword_Table.Insert
      ( Types.Lexer.Token_D_Step,
        Types.Lexer.Bound_String.To_Bounded_String ("d_step")     );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Do,
        Types.Lexer.Bound_String.To_Bounded_String ("do")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Else,
        Types.Lexer.Bound_String.To_Bounded_String ("else")       );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Empty,
        Types.Lexer.Bound_String.To_Bounded_String ("empty")      );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Enabled,
        Types.Lexer.Bound_String.To_Bounded_String ("enabled")    );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Eval,
        Types.Lexer.Bound_String.To_Bounded_String ("eval")       );
    Keyword_Table.Insert
      ( Types.Lexer.Token_False,
        Types.Lexer.Bound_String.To_Bounded_String ("false")      );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Fi,
        Types.Lexer.Bound_String.To_Bounded_String ("fi")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Full,
        Types.Lexer.Bound_String.To_Bounded_String ("full")       );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Goto,
        Types.Lexer.Bound_String.To_Bounded_String ("goto")       );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Hidden,
        Types.Lexer.Bound_String.To_Bounded_String ("hidden")     );
    Keyword_Table.Insert
      ( Types.Lexer.Token_If,
        Types.Lexer.Bound_String.To_Bounded_String ("if")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Init,
        Types.Lexer.Bound_String.To_Bounded_String ("init")       );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Int,
        Types.Lexer.Bound_String.To_Bounded_String ("int")        );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Len,
        Types.Lexer.Bound_String.To_Bounded_String ("len")        );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Local,
        Types.Lexer.Bound_String.To_Bounded_String ("local")      );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Mtype,
        Types.Lexer.Bound_String.To_Bounded_String ("mtype")      );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Nempty,
        Types.Lexer.Bound_String.To_Bounded_String ("nempty")     );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Never,
        Types.Lexer.Bound_String.To_Bounded_String ("never")      );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Nfull,
        Types.Lexer.Bound_String.To_Bounded_String ("nfull")      );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Notrace,
        Types.Lexer.Bound_String.To_Bounded_String ("notrace")    );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Np_Underscore,
        Types.Lexer.Bound_String.To_Bounded_String ("np_")        );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Od,
        Types.Lexer.Bound_String.To_Bounded_String ("od")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Of,
        Types.Lexer.Bound_String.To_Bounded_String ("of")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Pc_Value,
        Types.Lexer.Bound_String.To_Bounded_String ("pc_value")   );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Pid,
        Types.Lexer.Bound_String.To_Bounded_String ("pid")        );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Print,
        Types.Lexer.Bound_String.To_Bounded_String ("print")      );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Printf,
        Types.Lexer.Bound_String.To_Bounded_String ("printf")     );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Printm,
        Types.Lexer.Bound_String.To_Bounded_String ("printm")     );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Priority,
        Types.Lexer.Bound_String.To_Bounded_String ("priority")   );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Proctype,
        Types.Lexer.Bound_String.To_Bounded_String ("proctype")   );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Provided,
        Types.Lexer.Bound_String.To_Bounded_String ("provided")   );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Run,
        Types.Lexer.Bound_String.To_Bounded_String ("run")        );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Short,
        Types.Lexer.Bound_String.To_Bounded_String ("short")      );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Show,
        Types.Lexer.Bound_String.To_Bounded_String ("show")       );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Skip,
        Types.Lexer.Bound_String.To_Bounded_String ("skip")       );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Timeout,
        Types.Lexer.Bound_String.To_Bounded_String ("timeout")    );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Trace,
        Types.Lexer.Bound_String.To_Bounded_String ("trace")      );
    Keyword_Table.Insert
      ( Types.Lexer.Token_True,
        Types.Lexer.Bound_String.To_Bounded_String ("true")       );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Typedef,
        Types.Lexer.Bound_String.To_Bounded_String ("typedef")    );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Unless,
        Types.Lexer.Bound_String.To_Bounded_String ("unless")     );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Unsigned,
        Types.Lexer.Bound_String.To_Bounded_String ("unsigned")   );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Xr,
        Types.Lexer.Bound_String.To_Bounded_String ("xr")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Xs,
        Types.Lexer.Bound_String.To_Bounded_String ("xs")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Left_Curly_Bracket,
        Types.Lexer.Bound_String.To_Bounded_String ("{")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Right_Curly_Bracket,
        Types.Lexer.Bound_String.To_Bounded_String ("}")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Left_Square_Bracket,
        Types.Lexer.Bound_String.To_Bounded_String ("[")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Right_Square_Bracket,
        Types.Lexer.Bound_String.To_Bounded_String ("]")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Left_Parentheses,
        Types.Lexer.Bound_String.To_Bounded_String ("(")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Right_Parentheses,
        Types.Lexer.Bound_String.To_Bounded_String (")")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Semicolon,
        Types.Lexer.Bound_String.To_Bounded_String (";")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Comma,
        Types.Lexer.Bound_String.To_Bounded_String (",")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Dot,
        Types.Lexer.Bound_String.To_Bounded_String (".")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Alias,
        Types.Lexer.Bound_String.To_Bounded_String ("@")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Ones_Complement,
        Types.Lexer.Bound_String.To_Bounded_String ("~")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Guard,
        Types.Lexer.Bound_String.To_Bounded_String ("::")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Colon,
        Types.Lexer.Bound_String.To_Bounded_String (":")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Implies,
        Types.Lexer.Bound_String.To_Bounded_String ("->")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Assignment,
        Types.Lexer.Bound_String.To_Bounded_String ("=")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Decrement,
        Types.Lexer.Bound_String.To_Bounded_String ("--")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Increment,
        Types.Lexer.Bound_String.To_Bounded_String ("++")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Not_Or_Send_1,
        Types.Lexer.Bound_String.To_Bounded_String ("!")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Receive_1,
        Types.Lexer.Bound_String.To_Bounded_String ("?")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Receive_2,
        Types.Lexer.Bound_String.To_Bounded_String ("??")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Send_2,
        Types.Lexer.Bound_String.To_Bounded_String ("!!")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Or_Logical,
        Types.Lexer.Bound_String.To_Bounded_String ("||")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_And_Logical,
        Types.Lexer.Bound_String.To_Bounded_String ("&&")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Or_Bitwise,
        Types.Lexer.Bound_String.To_Bounded_String ("|")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Xor_Bitwise,
        Types.Lexer.Bound_String.To_Bounded_String ("^")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_And_Bitwise,
        Types.Lexer.Bound_String.To_Bounded_String ("&")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Equals,
        Types.Lexer.Bound_String.To_Bounded_String ("==")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Not_Equals,
        Types.Lexer.Bound_String.To_Bounded_String ("!=")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Less_Than,
        Types.Lexer.Bound_String.To_Bounded_String ("<")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Less_Than_Or_Equal_To,
        Types.Lexer.Bound_String.To_Bounded_String ("<=")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Greater_Than,
        Types.Lexer.Bound_String.To_Bounded_String (">")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Greater_Than_Or_Equal_To,
        Types.Lexer.Bound_String.To_Bounded_String (">=")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Left_Shift,
        Types.Lexer.Bound_String.To_Bounded_String ("<<")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Right_Shift,
        Types.Lexer.Bound_String.To_Bounded_String (">>")         );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Add,
        Types.Lexer.Bound_String.To_Bounded_String ("+")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Subtract,
        Types.Lexer.Bound_String.To_Bounded_String ("-")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Divide,
        Types.Lexer.Bound_String.To_Bounded_String ("/")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Multiply,
        Types.Lexer.Bound_String.To_Bounded_String ("*")          );
    Keyword_Table.Insert
      ( Types.Lexer.Token_Modulus,
        Types.Lexer.Bound_String.To_Bounded_String ("%")          );
  end       Init_Globals;
  
  function  To_String( T : Types.Lexer.Token_Type )
  return    String is
  begin
    return Types.Lexer.Bound_String.To_String( Keyword_Table.Element( T ) );
  end       To_String;
   
  function  Token_Type_Hash( T : Types.Lexer.Token_Type )
  return    Ada.Containers.Hash_Type is
  begin
    return Ada.Containers.Hash_Type'Mod( Types.Lexer.Token_Type'Pos( T ) );
  end       Token_Type_Hash;

end Types.Parser;
