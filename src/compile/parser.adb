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
--
-- TODO:
-- 1. Optimization: each constant/identifier should have only 1 node
-- and everything should point to that.

with Logger;
with Ada.Assertions;
with Ada.Exceptions;

package body Parser is

  function Parse(Source_Filename : String)  return Types.AST.Spec_Node_Ptr is
    Root_Node : Types.AST.Spec_Node_Ptr;
  begin
    Lexer.Set_Source_Filename( Source_Filename );
    -- Lexer errors will be reported here too
    Logger.Open_Log_File( "Parser.log" );
    Logger.Set_Level_Threshold( Logger.Info_Level );
    Logger.Info( Source_Filename );
    Types.Parser.Init_Globals;
    Root_Node := Spec;
    Logger.Info( "SUCCESS! Input parsed." );
    Logger.Close_Log_File;
    return Root_Node;
  exception
    when E : others =>
      Logger.Info( "FAILURE! Input not parsed." );
      Logger.Close_Log_File;
      raise Syntax_Error
        with Ada.Exceptions.Exception_Name( E ) & " : " & Ada.Exceptions.Exception_Message( E );
  end Parse;
  
  function Parse_Any_Expr                   return Types.AST.Any_Expr_Node_Ptr is
    Generic_Any_Expr  : Types.AST.Any_Expr_Node_Ptr;
  begin
    Logger.Open_Log_File( "Parser.log" );
    Logger.Info( "LTL Expression Translation" );
    Get_Token;
    Generic_Any_Expr := Any_Expr;
    Logger.Close_Log_File;
    return Generic_Any_Expr;
  end Parse_Any_Expr;
  
  procedure Match(Terminal : Types.Lexer.Token_Type) is
    use type Ada.Strings.Unbounded.Unbounded_String;
    Terminal_Image : String := Types.Lexer.Token_Type'Image( Terminal );
  begin
    if Look_Ahead.Kind = Terminal then
      Logger.Info( "MATCH " & Terminal_Image );
      if Accumulate_Tokens = True then
        case Terminal is
          when  Types.Lexer.Token_Name    =>
            Statement_Text := Statement_Text
                              & Types.Lexer.Bound_String.To_String(
                                  Types.Lexer.Element_In_Table( Look_Ahead.Lexeme_Cursor) );
          when  Types.Lexer.Token_Number  =>
            Statement_Text := Statement_Text
                              & Types.Lexer.Promela_Natural'Image(
                                  Types.Lexer.Element_In_Table( Look_Ahead.Value_Cursor) );
          when  Types.Lexer.Token_String  =>
            Statement_Text := Statement_Text
                              & '"'
                              & Types.Lexer.Bound_String.To_String(
                                  Types.Lexer.Element_In_Table( Look_Ahead.Lexeme_Cursor) )
                              & '"' ;
          when  others                    =>
            -- COMMENTARY: There are a number of solutions to this problem;
            -- this one lends itself more readily to maintenance and efficiency.
            Statement_Text := Statement_Text & Types.Parser.To_String( Terminal );
        end case;
      end if;
      Get_Token;
    else
      Logger.Error( "MISMATCH near Line " & Integer'Image( Look_Ahead.Line_Number )
                    & "! Expected " & Terminal_Image
                    & " but found " & Types.Lexer.Token_Type'Image( Look_Ahead.Kind ) & "." );
      raise Mismatched_Token;
    end if;
  end Match;

  -- Reads from buffer if it is not empty; otherwise consults Types.Lexer.
  procedure Get_Token is
  begin
    if Buffer.Is_Empty = True then
      loop
        Lexer.Scan;
        if Lexer.Current_Token.Kind = Types.Lexer.Token_Null then
          Logger.Error("Types.Lexer found Token_Null! Is the input valid Promela code? Abandoning parsing...");
          raise Found_Token_Null;
        end if;
        -- ignore newlines
        exit when Lexer.Current_Token.Kind /= Types.Lexer.Token_Newline;
      end loop;
      Look_Ahead := Lexer.Current_Token;
      Logger.Debug( "GET_TOKEN (Types.Lexer): " & Types.Lexer.Token_Type'Image(Look_Ahead.Kind) );
    else
      Look_Ahead := Buffer.First_Element;
      Buffer.Delete_First;
      -- NOTE: FIXED the BUG of not updating Lexer.Current_Token.Lexeme Cursor
      Lexer.Current_Token.Lexeme_Cursor := Look_Ahead.Lexeme_Cursor;
      -- NOTE: Much later...after some profound awe at the mystery and confusion...
      -- FIXED the BUG of not updating Lexer.Current_Token.Value_Cursor! Doh!
      Lexer.Current_Token.Value_Cursor  := Look_Ahead.Value_Cursor;
      Logger.Debug( "GET_TOKEN (Buffer): " & Types.Lexer.Token_Type'Image(Look_Ahead.Kind) );
    end if;
  end Get_Token;

  procedure Prepend_Token(A_Token : Types.Lexer.Token) is
  begin
    Buffer.Prepend(A_Token);
  end Prepend_Token;

  procedure Syntax_Error_Output(S : String) is
    Message : String  :=  "Syntax Error near Line " & Integer'Image(Look_Ahead.Line_Number) & "! "
                          & Types.Lexer.Token_Type'Image(Look_Ahead.Kind) & " found in the "
                          & S & " production!";
  begin
    Logger.Error( Message );
    raise Syntax_Error with Message;
  end Syntax_Error_Output;

  function Spec return Types.AST.Spec_Node_Ptr is
    Root_Node : Types.AST.Spec_Node_Ptr;
  begin
    -- Except for productions that go to epsilon (which require manual control),
    -- I have decided to consistently allocate a new object before every CASE.
    -- In the case of a Syntax_Error, the overhead is negligible and inconsequential.
    Root_Node := new Types.AST.Spec_Node;
    Root_Node.Tag := Types.AST.Node_Spec;
    Get_Token;
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Active      |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Init        |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Never       |
            Types.Lexer.Token_Notrace     |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Proctype    |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Trace       |
            Types.Lexer.Token_Typedef     |
            Types.Lexer.Token_Unsigned    =>
        Logger.Info("Spec => Module Spec_Prime");
        Root_Node.Module_List.Append(Module); Spec_Prime(Root_Node);
      when  others                        =>
        Syntax_Error_Output( "Spec" );
    end case;
    return Root_Node;
  end Spec;
  
  procedure Spec_Prime(Root_Node : Types.AST.Spec_Node_Ptr) is
  begin
    Ada.Assertions.Assert( Root_Node /= null, "Root_Node is null!" );
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Active      |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Init        |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Never       |
            Types.Lexer.Token_Notrace     |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Proctype    |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Trace       |
            Types.Lexer.Token_Typedef     |
            Types.Lexer.Token_Unsigned    =>
        Logger.Info("Spec_Prime => Module Spec_Prime");
        Root_Node.Module_List.Append(Module); Spec_Prime(Root_Node);
      when  Types.Lexer.Token_Semicolon   =>
        Logger.Info("Spec_Prime => ; Module Spec_Prime");
        Match(Types.Lexer.Token_Semicolon); Root_Node.Module_List.Append(Module); Spec_Prime(Root_Node);
      when  Types.Lexer.Token_EOF         =>
        Logger.Info("Spec_Prime => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Spec_Prime" );
    end case;
  end Spec_Prime;

  function Module return Types.AST.Module_Node_Ptr is
    Token1            : Types.Lexer.Token;
    Token2            : Types.Lexer.Token;
    A_Module          : Types.AST.Module_Node_Ptr;
    Proctype_Module   : Types.AST.Proctype_Module_Node_Ptr;
    Init_Module       : Types.AST.Init_Module_Node_Ptr;
    Never_Module      : Types.AST.Never_Module_Node_Ptr;
    Trace_Module      : Types.AST.Trace_Module_Node_Ptr;
    Utype_Module      : Types.AST.Utype_Module_Node_Ptr;
    Mtype_Module      : Types.AST.Mtype_Module_Node_Ptr;
    Decl_Lst_Module   : Types.AST.Decl_Lst_Module_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Active      |
            Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_Proctype    =>
        Logger.Info("Module => Proctype");
        Proctype_Module := new Types.AST.Proctype_Module_Node;
        Proctype_Module.Tag := Types.AST.Node_Proctype_Module;
        Proctype_Module.Proctype := Proctype;
        A_Module := Types.AST.Module_Node_Ptr( Proctype_Module );
      when  Types.Lexer.Token_Init        =>
        Logger.Info("Module => Init");
        Init_Module := new Types.AST.Init_Module_Node;
        Init_Module.Tag := Types.AST.Node_Init_Module;
        Init_Module.Pinit := Init;
        A_Module := Types.AST.Module_Node_Ptr(Init_Module);
      when  Types.Lexer.Token_Never       =>
        Logger.Info("Module => Never");
        Never_Module := new Types.AST.Never_Module_Node;
        Never_Module.Tag := Types.AST.Node_Never_Module;
        Never_Module.Never := Never;
        A_Module := Types.AST.Module_Node_Ptr(Never_Module);
      when  Types.Lexer.Token_Notrace     |
            Types.Lexer.Token_Trace       =>
        Logger.Info("Module => Trace");
        Trace_Module := new Types.AST.Trace_Module_Node;
        Trace_Module.Tag := Types.AST.Node_Trace_Module;
        Trace_Module.Trace := Trace;
        A_Module := Types.AST.Module_Node_Ptr(Trace_Module);
      when  Types.Lexer.Token_Typedef     =>
        Logger.Info("Module => Utype");
        Utype_Module := new Types.AST.Utype_Module_Node;
        Utype_Module.Tag := Types.AST.Node_Utype_Module;
        Utype_Module.Utype := Utype;
        A_Module := Types.AST.Module_Node_Ptr(Utype_Module);
      when  Types.Lexer.Token_Mtype       =>
        -- CONFLICT: manual resolution
        Token1 := Look_Ahead;
        Get_Token;
        Token2 := Look_Ahead;
        Prepend_Token(Token2);
        Prepend_Token(Token1);
        case Look_Ahead.Kind is
          when  Types.Lexer.Token_Assignment          |
                Types.Lexer.Token_Left_Curly_Bracket  =>
            Logger.Info("Module => Mtype");
            Get_Token;
            Mtype_Module := new Types.AST.Mtype_Module_Node;
            Mtype_Module.Tag := Types.AST.Node_Mtype_Module;
            Mtype_Module.Mtype := Mtype;
            A_Module := Types.AST.Module_Node_Ptr(Mtype_Module);
          when  Types.Lexer.Token_Name                =>
            Logger.Info("Module => Decl_Lst");
            Get_Token;
            Decl_Lst_Module := new Types.AST.Decl_Lst_Module_Node;
            Decl_Lst_Module.Tag := Types.AST.Node_Decl_Lst_Module;
            Decl_Lst_Module.Decl_Lst := Decl_Lst;
            A_Module := Types.AST.Module_Node_Ptr(Decl_Lst_Module);
          when others                                 =>
            Syntax_Error_Output( "Module" );
        end case;
      when  Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Unsigned    =>
        Logger.Info("Module => Decl_Lst");
        Decl_Lst_Module := new Types.AST.Decl_Lst_Module_Node;
        Decl_Lst_Module.Tag := Types.AST.Node_Decl_Lst_Module;
        Decl_Lst_Module.Decl_Lst := Decl_Lst;
        A_Module := Types.AST.Module_Node_Ptr(Decl_Lst_Module);
      when others                         =>
        Syntax_Error_Output( "Module" );
    end case;
    return A_Module;
  end Module;

  function Init return Types.AST.Proctype_Node_Ptr is
    A_Proctype_Node : Types.AST.Proctype_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Init        =>
        Logger.Info("Init => INIT Init_Prime { Sequence }");
        A_Proctype_Node := new Types.AST.Proctype_Node;
        A_Proctype_Node.Tag := Types.AST.Node_Proctype;
        -- Pinit from SPIN's pan.h
        A_Proctype_Node.Name := Types.Lexer.Add_To_String_Table( Types.Lexer.Bound_String.To_Bounded_String ("Pinit") );
        A_Proctype_Node.Version := Types.Lexer.Token_Proctype;
        A_Proctype_Node.Active := new Types.AST.Active_Node;
        A_Proctype_Node.Active.Tag := Types.AST.Node_Active;
        Match(Types.Lexer.Token_Init); A_Proctype_Node.Priority := Init_Prime;
        Match(Types.Lexer.Token_Left_Curly_Bracket);
        A_Proctype_Node.Sequence := Sequence;
        Match(Types.Lexer.Token_Right_Curly_Bracket);
      when  others                        =>
        Syntax_Error_Output( "Init" );
    end case;
    return A_Proctype_Node;
  end Init;

  function Init_Prime return Types.AST.Priority_Node_Ptr is
    A_Priority_Node : Types.AST.Priority_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Priority    =>
        Logger.Info("Init_Prime => Priority");
        A_Priority_Node := Priority;
      when  Types.Lexer.Token_Left_Curly_Bracket
                                          =>
        Logger.Info("Init_Prime => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Init_Prime" );
    end case;
    return A_Priority_Node;
  end Init_Prime;

  function Priority return Types.AST.Priority_Node_Ptr is
    A_Priority_Node : Types.AST.Priority_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Priority    =>
        Logger.Info("Priority => PRIORITY Const");
        A_Priority_Node := new Types.AST.Priority_Node;
        A_Priority_Node.Tag := Types.AST.Node_Priority;
        Match(Types.Lexer.Token_Priority); A_Priority_Node.Number := Const;
      when  others                        =>
        Syntax_Error_Output( "Priority" );
    end case;
    return A_Priority_Node;
  end Priority;
  
  function Never return Types.AST.Never_Node_Ptr is
    A_Never_Node : Types.AST.Never_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Never       =>
        Logger.Info("Never => NEVER { Sequence }");
        A_Never_Node := new Types.AST.Never_Node;
        A_Never_Node.Tag := Types.AST.Node_Never;
        Match(Types.Lexer.Token_Never); Match(Types.Lexer.Token_Left_Curly_Bracket);
        A_Never_Node.Sequence := Sequence; Match(Types.Lexer.Token_Right_Curly_Bracket);
      when  others                        =>
        Syntax_Error_Output( "Never" );
    end case;
    return A_Never_Node;
  end Never;
  
  function Trace return Types.AST.Trace_Node_Ptr is
    A_Trace_Node : Types.AST.Trace_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Notrace     |
            Types.Lexer.Token_Trace       =>
        Logger.Info("Trace => Trace_Prime { Sequence }");
        A_Trace_Node := new Types.AST.Trace_Node;
        A_Trace_Node.Tag := Types.AST.Node_Trace;
        A_Trace_Node.Version := Trace_Prime; Match(Types.Lexer.Token_Left_Curly_Bracket);
        A_Trace_Node.Sequence := Sequence; Match(Types.Lexer.Token_Right_Curly_Bracket);
      when  others                        =>
        Syntax_Error_Output( "Trace" );
    end case;
    return A_Trace_Node;
  end Trace;

  function Trace_Prime return Types.Lexer.Token_Type is
    Attribute : Types.Lexer.Token_Type;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Notrace     =>
        Logger.Info("Trace_Prime => NOTRACE");
        Attribute := Types.Lexer.Token_Notrace;
        Match(Types.Lexer.Token_Notrace);
      when  Types.Lexer.Token_Trace       =>
        Logger.Info("Trace_Prime => TRACE");
        Attribute := Types.Lexer.Token_Trace;
        Match(Types.Lexer.Token_Trace);
      when  others                        =>
        Syntax_Error_Output( "Trace_Prime" );
    end case;
    return Attribute;
  end Trace_Prime;
  
  function Utype return Types.AST.Utype_Node_Ptr is
    An_Utype_Node : Types.AST.Utype_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Typedef     =>
        Logger.Info("Utype => TYPEDEF NAME { Decl_Lst }");
        An_Utype_Node := new Types.AST.Utype_Node;
        An_Utype_Node.Tag := Types.AST.Node_Utype;
        Match(Types.Lexer.Token_Typedef); An_Utype_Node.Name := Lexer.Current_Token.Lexeme_Cursor; Match(Types.Lexer.Token_Name);
        Match(Types.Lexer.Token_Left_Curly_Bracket); An_Utype_Node.Decl_Lst := Decl_Lst; Match(Types.Lexer.Token_Right_Curly_Bracket);
      when  others                        =>
        Syntax_Error_Output( "Utype" );
    end case;
    return An_Utype_Node;
  end Utype;

  function Mtype return Types.AST.Mtype_Node_Ptr is
    -- An_Mtype or A_Mtype? Hmm...
    A_Mtype_Node : Types.AST.Mtype_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Mtype       =>
        Logger.Info("Mtype => MTYPE Mtype_Prime1 { NAME Mtype_Prime2 }");
        A_Mtype_Node      := new Types.AST.Mtype_Node;
        A_Mtype_Node.Tag  := Types.AST.Node_Mtype;
        Match(Types.Lexer.Token_Mtype);
        Mtype_Prime1;
        Match(Types.Lexer.Token_Left_Curly_Bracket);
        -- Map every Mtype name to 1; we will correct this in the corresponding procedure in AST
        A_Mtype_Node.Names_List.Append( Types.Lexer.Element_In_Table( Lexer.Current_Token.Lexeme_Cursor ) );
        A_Mtype_Node.Names_Map.Insert( Types.Lexer.Element_In_Table( Lexer.Current_Token.Lexeme_Cursor ), 1 );
        Match(Types.Lexer.Token_Name);
        Mtype_Prime2( A_Mtype_Node );
        Match(Types.Lexer.Token_Right_Curly_Bracket);
      when  others                        =>
        Syntax_Error_Output( "Mtype" );
    end case;
    return A_Mtype_Node;
  end Mtype;

  procedure Mtype_Prime1 is
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Assignment  =>
        Logger.Info("Mtype_Prime1 => =");
        Match(Types.Lexer.Token_Assignment);
      when  Types.Lexer.Token_Left_Curly_Bracket
                                          =>
        Logger.Info("Trace_Prime => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Mtype_Prime1" );
    end case;
  end Mtype_Prime1;

  procedure Mtype_Prime2( A_Mtype_Node : Types.AST.Mtype_Node_Ptr ) is
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Comma       =>
        Logger.Info("Mtype_Prime2 => , NAME Mtype_Prime2");
        Match(Types.Lexer.Token_Comma);
        -- Map every Mtype name to 1; we will correct this in the corresponding procedure in AST
        A_Mtype_Node.Names_List.Append( Types.Lexer.Element_In_Table( Lexer.Current_Token.Lexeme_Cursor ) );
        A_Mtype_Node.Names_Map.Insert( Types.Lexer.Element_In_Table( Lexer.Current_Token.Lexeme_Cursor ), 1 );
        Match(Types.Lexer.Token_Name); Mtype_Prime2( A_Mtype_Node );
      when  Types.Lexer.Token_Right_Curly_Bracket
                                          =>
        Logger.Info("Mtype_Prime2 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Mtype_Prime2" );
    end case;
  end Mtype_Prime2;
  
  function Sequence return Types.AST.Sequence_Node_Ptr is
    A_Sequence_Node : Types.AST.Sequence_Node_Ptr;
    A_Step_Node     : Types.AST.Step_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Assert      |
            Types.Lexer.Token_Atomic      |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Break       |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_C_Code      |
            Types.Lexer.Token_C_Decl      |
            Types.Lexer.Token_C_Expr      |
            Types.Lexer.Token_C_State     |
            Types.Lexer.Token_C_Track     |
            Types.Lexer.Token_Do          | 
            Types.Lexer.Token_D_Step      |
            Types.Lexer.Token_Else        | 
            Types.Lexer.Token_Empty       | 
            Types.Lexer.Token_Enabled     | 
            Types.Lexer.Token_False       |
            Types.Lexer.Token_Full        | 
            Types.Lexer.Token_Goto        | 
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_If          |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Left_Curly_Bracket
                                          |
            Types.Lexer.Token_Len         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Nempty      |
            Types.Lexer.Token_Nfull       |
            Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_Np_Underscore
                                          |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Ones_Complement
                                          |
            Types.Lexer.Token_Pc_Value    |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Print       |
            Types.Lexer.Token_Printf      |
            Types.Lexer.Token_Printm      |
            Types.Lexer.Token_Run         |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Timeout     |
            Types.Lexer.Token_True        |
            Types.Lexer.Token_Unsigned    |
            Types.Lexer.Token_Xr          |
            Types.Lexer.Token_Xs          =>
        Logger.Info("Sequence => Step Sequence_Prime");
        A_Sequence_Node := new Types.AST.Sequence_Node;
        A_Sequence_Node.Tag := Types.AST.Node_Sequence;
        A_Step_Node := Step;
        -- conditional list compilation
        case A_Step_Node.Tag is
          when Types.AST.Node_Decl_Lst_Step =>
            A_Sequence_Node.Decl_Lst_Step_List.Append( A_Step_Node );
          when Types.AST.Node_Stmt_Step     =>
            A_Sequence_Node.Stmt_Step_List.Append( A_Step_Node );
          when Types.AST.Node_Xr_Xs_Step    =>
            A_Sequence_Node.Xr_Xs_Step_List.Append( A_Step_Node );
          when others                       =>
            Syntax_Error_Output( "Sequence" );
        end case;
        Sequence_Prime( A_Sequence_Node );
      when  others                        =>
        Syntax_Error_Output( "Sequence" );
    end case;
    return A_Sequence_Node;
  end Sequence;

  procedure Sequence_Prime( A_Sequence_Node : Types.AST.Sequence_Node_Ptr ) is
    A_Step_Node     : Types.AST.Step_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Assert      |
            Types.Lexer.Token_Atomic      |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Break       |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_C_Code      |
            Types.Lexer.Token_C_Decl      |
            Types.Lexer.Token_C_Expr      |
            Types.Lexer.Token_C_State     |
            Types.Lexer.Token_C_Track     |
            Types.Lexer.Token_Do          | 
            Types.Lexer.Token_D_Step      |
            Types.Lexer.Token_Else        | 
            Types.Lexer.Token_Empty       | 
            Types.Lexer.Token_Enabled     | 
            Types.Lexer.Token_False       |
            Types.Lexer.Token_Full        | 
            Types.Lexer.Token_Goto        | 
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_If          |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Left_Curly_Bracket
                                          |
            Types.Lexer.Token_Len         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Nempty      |
            Types.Lexer.Token_Nfull       |
            Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_Np_Underscore
                                          |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Ones_Complement
                                          |
            Types.Lexer.Token_Pc_Value    |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Print       |
            Types.Lexer.Token_Printf      |
            Types.Lexer.Token_Printm      |
            Types.Lexer.Token_Run         |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Timeout     |
            Types.Lexer.Token_True        |
            Types.Lexer.Token_Unsigned    |
            Types.Lexer.Token_Xr          |
            Types.Lexer.Token_Xs          =>
        Logger.Info("Sequence_Prime => Step Sequence_Prime");
        A_Step_Node := Step;
        -- conditional list compilation
        case A_Step_Node.Tag is
          when Types.AST.Node_Decl_Lst_Step =>
            A_Sequence_Node.Decl_Lst_Step_List.Append( A_Step_Node );
          when Types.AST.Node_Stmt_Step     =>
            A_Sequence_Node.Stmt_Step_List.Append( A_Step_Node );
          when Types.AST.Node_Xr_Xs_Step    =>
            A_Sequence_Node.Xr_Xs_Step_List.Append( A_Step_Node );
          when others                       =>
            Syntax_Error_Output( "Sequence" );
        end case;
        Sequence_Prime( A_Sequence_Node );
      when  Types.Lexer.Token_Implies     =>
        Logger.Info("Sequence_Prime => -> Step Sequence_Prime");
        Match(Types.Lexer.Token_Implies);
        A_Step_Node := Step;
        -- conditional list compilation
        case A_Step_Node.Tag is
          when Types.AST.Node_Decl_Lst_Step =>
            A_Sequence_Node.Decl_Lst_Step_List.Append( A_Step_Node );
          when Types.AST.Node_Stmt_Step     =>
            A_Sequence_Node.Stmt_Step_List.Append( A_Step_Node );
          when Types.AST.Node_Xr_Xs_Step    =>
            A_Sequence_Node.Xr_Xs_Step_List.Append( A_Step_Node );
          when others                       =>
            Syntax_Error_Output( "Sequence" );
        end case;
        Sequence_Prime( A_Sequence_Node );
      when  Types.Lexer.Token_Semicolon   =>
        Logger.Info("Sequence_Prime => ; Sequence_Prime_Prime");
        Match(Types.Lexer.Token_Semicolon); Sequence_Prime_Prime( A_Sequence_Node );
      when  Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Right_Curly_Bracket
                                          =>
        Logger.Info("Sequence_Prime => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Sequence_Prime" );
    end case;
  end Sequence_Prime;

  procedure Sequence_Prime_Prime( A_Sequence_Node : Types.AST.Sequence_Node_Ptr ) is
    A_Step_Node     : Types.AST.Step_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Assert      |
            Types.Lexer.Token_Atomic      |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Break       |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_C_Code      |
            Types.Lexer.Token_C_Decl      |
            Types.Lexer.Token_C_Expr      |
            Types.Lexer.Token_C_State     |
            Types.Lexer.Token_C_Track     |
            Types.Lexer.Token_Do          | 
            Types.Lexer.Token_D_Step      |
            Types.Lexer.Token_Else        | 
            Types.Lexer.Token_Empty       | 
            Types.Lexer.Token_Enabled     | 
            Types.Lexer.Token_False       |
            Types.Lexer.Token_Full        | 
            Types.Lexer.Token_Goto        | 
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_If          |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Left_Curly_Bracket
                                          |
            Types.Lexer.Token_Len         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Nempty      |
            Types.Lexer.Token_Nfull       |
            Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_Np_Underscore
                                          |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Ones_Complement
                                          |
            Types.Lexer.Token_Pc_Value    |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Print       |
            Types.Lexer.Token_Printf      |
            Types.Lexer.Token_Printm      |
            Types.Lexer.Token_Run         |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Timeout     |
            Types.Lexer.Token_True        |
            Types.Lexer.Token_Unsigned    |
            Types.Lexer.Token_Xr          |
            Types.Lexer.Token_Xs          =>
        Logger.Info("Sequence_Prime_Prime => Step Sequence_Prime");
        A_Step_Node := Step;
        -- conditional list compilation
        case A_Step_Node.Tag is
          when Types.AST.Node_Decl_Lst_Step =>
            A_Sequence_Node.Decl_Lst_Step_List.Append( A_Step_Node );
          when Types.AST.Node_Stmt_Step     =>
            A_Sequence_Node.Stmt_Step_List.Append( A_Step_Node );
          when Types.AST.Node_Xr_Xs_Step    =>
            A_Sequence_Node.Xr_Xs_Step_List.Append( A_Step_Node );
          when others                       =>
            Syntax_Error_Output( "Sequence" );
        end case;
        Sequence_Prime( A_Sequence_Node );
      when  Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Right_Curly_Bracket
                                          =>
        Logger.Info("Sequence_Prime_Prime => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Sequence_Prime_Prime" );
    end case;
  end Sequence_Prime_Prime;

   -- TODO: verify completeness and accuracy
  function Step return Types.AST.Step_Node_Ptr is
    Token1                : Types.Lexer.Token;
    Token2                : Types.Lexer.Token;
    A_Stmt_Step_Node      : Types.AST.Stmt_Step_Node_Ptr;
    A_Decl_Lst_Step_Node  : Types.AST.Decl_Lst_Step_Node_Ptr;
    A_Xr_Xs_Step_Node    : Types.AST.Xr_Xs_Step_Node_Ptr;
    A_Step_Node           : Types.AST.Step_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Name        =>
        -- CONFLICT: manual resolution
        Token1 := Look_Ahead;
        Get_Token;
        Token2 := Look_Ahead;
        Prepend_Token(Token2);
        Prepend_Token(Token1);
        case Look_Ahead.Kind is
          -- NOTE: heuristic
          when  Types.Lexer.Token_Name                =>
              Logger.Info("Step => Decl_Lst");
            Get_Token;
            A_Decl_Lst_Step_Node := new Types.AST.Decl_Lst_Step_Node;
            A_Decl_Lst_Step_Node.Tag := Types.AST.Node_Decl_Lst_Step;
            A_Decl_Lst_Step_Node.Decl_Lst := Decl_Lst;
            A_Step_Node := Types.AST.Step_Node_Ptr( A_Decl_Lst_Step_Node );
          -- NOTE: leaving syntax checking to Stmt
          when others                                 =>
            Logger.Info("Step => Stmt Step_Prime1");
            Get_Token;
            A_Stmt_Step_Node := new Types.AST.Stmt_Step_Node;
            A_Stmt_Step_Node.Tag := Types.AST.Node_Stmt_Step;
            A_Stmt_Step_Node.Statement := Stmt; A_Stmt_Step_Node.Unless := Step_Prime1;
            A_Step_Node := Types.AST.Step_Node_Ptr( A_Stmt_Step_Node );
        end case;
      when  Types.Lexer.Token_Assert      |
            Types.Lexer.Token_Atomic      |
            Types.Lexer.Token_Break       |
            Types.Lexer.Token_C_Code      |
            Types.Lexer.Token_C_Decl      |
            Types.Lexer.Token_C_Expr      |
            Types.Lexer.Token_C_State     |
            Types.Lexer.Token_C_Track     |
            Types.Lexer.Token_Do          | 
            Types.Lexer.Token_D_Step      |
            Types.Lexer.Token_Else        | 
            Types.Lexer.Token_Empty       | 
            Types.Lexer.Token_Enabled     | 
            Types.Lexer.Token_False       |
            Types.Lexer.Token_Full        | 
            Types.Lexer.Token_Goto        | 
            Types.Lexer.Token_If          |
            Types.Lexer.Token_Left_Curly_Bracket
                                          |
            Types.Lexer.Token_Len         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Nempty      |
            Types.Lexer.Token_Nfull       |
            Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_Np_Underscore
                                          |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Ones_Complement
                                          |
            Types.Lexer.Token_Pc_Value    |
            Types.Lexer.Token_Print       |
            Types.Lexer.Token_Printf      |
            Types.Lexer.Token_Printm      |
            Types.Lexer.Token_Run         |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Timeout     |
            Types.Lexer.Token_True        =>
        Logger.Info("Step => Stmt Step_Prime1");
        A_Stmt_Step_Node := new Types.AST.Stmt_Step_Node;
        A_Stmt_Step_Node.Tag := Types.AST.Node_Stmt_Step;
        A_Stmt_Step_Node.Statement := Stmt; A_Stmt_Step_Node.Unless := Step_Prime1;
        A_Step_Node := Types.AST.Step_Node_Ptr( A_Stmt_Step_Node );
      when  Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Unsigned    =>
        Logger.Info("Step => Decl_Lst");
        A_Decl_Lst_Step_Node := new Types.AST.Decl_Lst_Step_Node;
        A_Decl_Lst_Step_Node.Tag := Types.AST.Node_Decl_Lst_Step;
        A_Decl_Lst_Step_Node.Decl_Lst := Decl_Lst;
        A_Step_Node := Types.AST.Step_Node_Ptr( A_Decl_Lst_Step_Node );
      when  Types.Lexer.Token_Xr          =>
        Logger.Info("Step => XR Varref Step_Prime2");
        A_Xr_Xs_Step_Node := new Types.AST.Xr_Xs_Step_Node;
        A_Xr_Xs_Step_Node.Tag := Types.AST.Node_Xr_Xs_Step;
        A_Xr_Xs_Step_Node.Channel_Assertion := Types.Lexer.Token_Xr;
        Match(Types.Lexer.Token_Xr); A_Xr_Xs_Step_Node.Channel_Names_List.Append( Varref );
        Step_Prime2( A_Xr_Xs_Step_Node );
        A_Step_Node := Types.AST.Step_Node_Ptr( A_Xr_Xs_Step_Node );
      when  Types.Lexer.Token_Xs          =>
         A_Xr_Xs_Step_Node := new Types.AST.Xr_Xs_Step_Node;
         A_Xr_Xs_Step_Node.Tag := Types.AST.Node_Xr_Xs_Step;
        A_Xr_Xs_Step_Node.Channel_Assertion := Types.Lexer.Token_Xs;
        Logger.Info("Step => XS Varref Step_Prime2");
        Match(Types.Lexer.Token_Xs); A_Xr_Xs_Step_Node.Channel_Names_List.Append( Varref );
        Step_Prime2( A_Xr_Xs_Step_Node );
        A_Step_Node := Types.AST.Step_Node_Ptr( A_Xr_Xs_Step_Node );
      when  others                        =>
        Syntax_Error_Output( "Step" );
    end case;
    return A_Step_Node;
  end Step;

  function Step_Prime1 return Types.AST.Stmt_Node_Ptr is
    A_Stmt_Node : Types.AST.Stmt_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Unless      =>
        Logger.Info("Step_Prime1 => UNLESS Stmt");
        Match(Types.Lexer.Token_Unless); A_Stmt_Node := Stmt;
      -- NOTE: should be valid, it is FOLLOW(Step)
      when  Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Right_Curly_Bracket
                                    |
            Types.Lexer.Token_Implies     |
            Types.Lexer.Token_Semicolon   |
            -- NOTE: first(step); this is for the case where there is no semicolon separating one step from another
            Types.Lexer.Token_Assert      |
            Types.Lexer.Token_Atomic      |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Break       |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_C_Code      |
            Types.Lexer.Token_C_Decl      |
            Types.Lexer.Token_C_Expr      |
            Types.Lexer.Token_C_State     |
            Types.Lexer.Token_C_Track     |
            Types.Lexer.Token_Do          | 
            Types.Lexer.Token_D_Step      |
            Types.Lexer.Token_Else        | 
            Types.Lexer.Token_Empty       | 
            Types.Lexer.Token_Enabled     | 
            Types.Lexer.Token_False       |
            Types.Lexer.Token_Full        | 
            Types.Lexer.Token_Goto        | 
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_If          |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Left_Curly_Bracket
                                          |
            Types.Lexer.Token_Len         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Nempty      |
            Types.Lexer.Token_Nfull       |
            Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_Np_Underscore
                                          |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Ones_Complement
                                          |
            Types.Lexer.Token_Pc_Value    |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Print       |
            Types.Lexer.Token_Printf      |
            Types.Lexer.Token_Printm      |
            Types.Lexer.Token_Run         |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Timeout     |
            Types.Lexer.Token_True        |
            Types.Lexer.Token_Unsigned    |
            Types.Lexer.Token_Xr          |
            Types.Lexer.Token_Xs          =>
        Logger.Info("Step_Prime1 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Step_Prime1" );
    end case;
    return A_Stmt_Node;
  end Step_Prime1;
  
  procedure Step_Prime2 ( A_Xr_Xs_Step_Node : Types.AST.Xr_Xs_Step_Node_Ptr ) is
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Comma       =>
        Logger.Info("Step_Prime2 => , Varref Step_Prime2");
        Match(Types.Lexer.Token_Comma); A_Xr_Xs_Step_Node.Channel_Names_List.Append( Varref );
        Step_Prime2( A_Xr_Xs_Step_Node );
      -- NOTE: should be valid, it is FOLLOW(Step)
      when  Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Right_Curly_Bracket
                                    |
            Types.Lexer.Token_Implies     |
            Types.Lexer.Token_Semicolon   =>
        Logger.Info("Step_Prime2 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Step_Prime2" );
    end case;
  end Step_Prime2;
  
  -- NOTE: should be valid; I removed incorrect entries
  function Decl_Lst return Types.AST.Decl_Lst_Node_Ptr is
    A_Decl_Lst_Node : Types.AST.Decl_Lst_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Semicolon   |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Unsigned    =>
        Logger.Info("Decl_Lst => One_Decl Decl_Lst_Prime");
        A_Decl_Lst_Node := new Types.AST.Decl_Lst_Node;
        A_Decl_Lst_Node.Tag := Types.AST.Node_Decl_Lst;
        A_Decl_Lst_Node.One_Decl_List.Append( One_Decl ); Decl_Lst_Prime( A_Decl_Lst_Node );
      when  others                        =>
        Syntax_Error_Output( "Decl_Lst" );
    end case;
    return A_Decl_Lst_Node;
  end Decl_Lst;

  procedure Decl_Lst_Prime( A_Decl_Lst_Node : Types.AST.Decl_Lst_Node_Ptr ) is
    Token1 : Types.Lexer.Token;
    Token2 : Types.Lexer.Token;
    Token3 : Types.Lexer.Token;
  begin
    Ada.Assertions.Assert( A_Decl_Lst_Node /= null, "A_Decl_Lst_Node is null!" );
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Semicolon       =>
        -- CONFLICT: manual resolution
        Token1 := Look_Ahead;
        Get_Token;
        Token2 := Look_Ahead;
        case Look_Ahead.Kind is
          -- FIRST(one_decl)
          when  Types.Lexer.Token_Bit         |
                Types.Lexer.Token_Bool        |
                Types.Lexer.Token_Byte        |
                Types.Lexer.Token_Chan        |
                Types.Lexer.Token_Hidden      |
                Types.Lexer.Token_Int         |
                Types.Lexer.Token_Local       |
                Types.Lexer.Token_Mtype       |
                Types.Lexer.Token_Pid         |
                Types.Lexer.Token_Short       |
                Types.Lexer.Token_Show        |
                Types.Lexer.Token_Unsigned    =>
            Logger.Info("Decl_Lst_Prime => ; One_Decl; Decl_Lst_Prime;");
            Prepend_Token(Token2);
            Prepend_Token(Token1);
            Get_Token;
            Match(Types.Lexer.Token_Semicolon); A_Decl_Lst_Node.One_Decl_List.Append( One_Decl ); Decl_Lst_Prime( A_Decl_Lst_Node );
          when  Types.Lexer.Token_Name        =>
            Get_Token;
            Token3 := Look_Ahead;
            case Look_Ahead.Kind is
              when  Types.Lexer.Token_Name  =>
                Logger.Info("Decl_Lst_Prime => ; One_Decl; Decl_Lst_Prime;");
                Prepend_Token(Token3);
                Prepend_Token(Token2);
                Prepend_Token(Token1);
                Get_Token;
                Match(Types.Lexer.Token_Semicolon); A_Decl_Lst_Node.One_Decl_List.Append( One_Decl ); Decl_Lst_Prime( A_Decl_Lst_Node );
              when  others                  =>
                Logger.Info("Decl_Lst_Prime => epsilon");
                Prepend_Token(Token3);
                Prepend_Token(Token2);
                Prepend_Token(Token1);
                Get_Token;
            end case;
          -- NOTE: leaving syntax checking to the next production
          when  others                        =>
            Logger.Info("Decl_Lst_Prime => epsilon");
            Prepend_Token(Token2);
            Prepend_Token(Token1);
            Get_Token;
        end case;
      -- FOLLOW(decl_lst_prime) |= FOLLOW(decl_lst) |= FOLLOW(module) |= FOLLOW(spec)
      when  Types.Lexer.Token_Active      |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_EOF         |
            Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Init        |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Never       |
            Types.Lexer.Token_Notrace     |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Proctype    |
            Types.Lexer.Token_Right_Curly_Bracket
                                          |
            Types.Lexer.Token_Right_Parentheses
                                          |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Trace       |
            Types.Lexer.Token_Typedef     |
            Types.Lexer.Token_Unsigned    |
            -- NOTE: first(stmt); this is for a sequence with a decl_lst followed by a stmt without a semicolon
            Types.Lexer.Token_Do          |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Enabled     |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Assert      |
            Types.Lexer.Token_C_Decl      |
            Types.Lexer.Token_Atomic      |
            Types.Lexer.Token_Timeout     |
            Types.Lexer.Token_Print       |
            Types.Lexer.Token_C_State     |
            Types.Lexer.Token_True        |
            Types.Lexer.Token_Empty       |
            Types.Lexer.Token_C_Code      |
            Types.Lexer.Token_D_Step      |
            Types.Lexer.Token_Ones_Complement
                                          |
            Types.Lexer.Token_Nempty      |
            Types.Lexer.Token_Full        |
            Types.Lexer.Token_Goto        |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Else        |
            Types.Lexer.Token_Printf      |
            Types.Lexer.Token_Left_Curly_Bracket
                                          |
            Types.Lexer.Token_Run         |
            Types.Lexer.Token_Printm      |
            Types.Lexer.Token_If          |
            Types.Lexer.Token_False       |
            Types.Lexer.Token_C_Expr      |
            Types.Lexer.Token_Len         |
            Types.Lexer.Token_Break       |
            Types.Lexer.Token_Pc_Value    |
            Types.Lexer.Token_Np_Underscore
                                          |
            Types.Lexer.Token_Nfull       |
            Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_C_Track     =>
        Logger.Info("Decl_Lst_Prime => epsilon");
      when  others                  =>
        Syntax_Error_Output( "Decl_Lst_Prime" );
    end case;
  end Decl_Lst_Prime;
  
  function One_Decl return Types.AST.One_Decl_Node_Ptr is
    A_One_Decl_Node : Types.AST.One_Decl_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Unsigned    =>
        Logger.Info("One_Decl => One_Decl_Prime1 Typename Ivar One_Decl_Prime2");
        A_One_Decl_Node             := new Types.AST.One_Decl_Node;
        A_One_Decl_Node.Tag         := Types.AST.Node_One_Decl;
        A_One_Decl_Node.Visibility  := One_Decl_Prime1;
        A_One_Decl_Node.Typename    := Typename;
        A_One_Decl_Node.Variable_List.Append( Ivar ); One_Decl_Prime2( A_One_Decl_Node );
      when  others                        =>
        Syntax_Error_Output( "One_Decl" );
    end case;
    return A_One_Decl_Node;
  end One_Decl;

  function One_Decl_Prime1 return Types.Lexer.Token_Type is
    An_Attribute : Types.Lexer.Token_Type := Types.Lexer.Token_Null;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Show        =>
        Logger.Info("One_Decl_Prime1 => Visible");
        An_Attribute := Visible;
      when  Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Unsigned    =>
        Logger.Info("One_Decl_Prime1 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "One_Decl_Prime1" );
    end case;
    return An_Attribute;
  end One_Decl_Prime1;

  procedure One_Decl_Prime2( A_One_Decl_Node : Types.AST.One_Decl_Node_Ptr ) is
  begin
    Ada.Assertions.Assert( A_One_Decl_Node /= null, "A_One_Decl_Node is null!" );
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Comma       =>
        Logger.Info("One_Decl_Prime2 => , Ivar One_Decl_Prime2");
        Match(Types.Lexer.Token_Comma); A_One_Decl_Node.Variable_List.Append( Ivar ); One_Decl_Prime2( A_One_Decl_Node );
      -- FOLLOW(one_decl) |= FOLLOW(decl_lst_prime) |= FOLLOW(decl_lst) |= FOLLOW(module) |= FOLLOW(spec)
      when  Types.Lexer.Token_Active      |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_EOF         |
            Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Init        |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Never       |
            Types.Lexer.Token_Notrace     |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Proctype    |
            Types.Lexer.Token_Right_Curly_Bracket
                                          |
            Types.Lexer.Token_Right_Parentheses
                                          |
            Types.Lexer.Token_Semicolon   |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Trace       |
            Types.Lexer.Token_Typedef     |
            Types.Lexer.Token_Unsigned    |
            -- NOTE: first(stmt); this is for a sequence with a decl_lst followed by a stmt without a semicolon
            Types.Lexer.Token_Do          |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Enabled     |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Assert      |
            Types.Lexer.Token_C_Decl      |
            Types.Lexer.Token_Atomic      |
            Types.Lexer.Token_Timeout     |
            Types.Lexer.Token_Print       |
            Types.Lexer.Token_C_State     |
            Types.Lexer.Token_True        |
            Types.Lexer.Token_Empty       |
            Types.Lexer.Token_C_Code      |
            Types.Lexer.Token_D_Step      |
            Types.Lexer.Token_Ones_Complement
                                          |
            Types.Lexer.Token_Nempty      |
            Types.Lexer.Token_Full        |
            Types.Lexer.Token_Goto        |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Else        |
            Types.Lexer.Token_Printf      |
            Types.Lexer.Token_Left_Curly_Bracket
                                          |
            Types.Lexer.Token_Run         |
            Types.Lexer.Token_Printm      |
            Types.Lexer.Token_If          |
            Types.Lexer.Token_False       |
            Types.Lexer.Token_C_Expr      |
            Types.Lexer.Token_Len         |
            Types.Lexer.Token_Break       |
            Types.Lexer.Token_Pc_Value    |
            Types.Lexer.Token_Np_Underscore
                                          |
            Types.Lexer.Token_Nfull       |
            Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_C_Track     =>
        Logger.Info("One_Decl_Prime2 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "One_Decl_Prime2" );
    end case;
  end One_Decl_Prime2;

  function Visible return Types.Lexer.Token_Type is
    An_Attribute : Types.Lexer.Token_Type := Types.Lexer.Token_Null;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Hidden      =>
        Logger.Info("Visible => HIDDEN");
        An_Attribute := Types.Lexer.Token_Hidden;
        Match(Types.Lexer.Token_Hidden);
      when  Types.Lexer.Token_Local       =>
        Logger.Info("Visible => LOCAL");
        An_Attribute := Types.Lexer.Token_Local;
        Match(Types.Lexer.Token_Local);
      when  Types.Lexer.Token_Show        =>
        Logger.Info("Visible => SHOW");
        An_Attribute := Types.Lexer.Token_Show;
        Match(Types.Lexer.Token_Show);
      when  others                        =>
        Syntax_Error_Output( "Visible" );
    end case;
    return An_Attribute;
  end Visible;
  
  function Typename return Types.AST.Typename_Node_Ptr is
    A_Typename_Node             : Types.AST.Typename_Node_Ptr;
    A_Predefined_Typename_Node  : Types.AST.Predefined_Typename_Node_Ptr;
    An_Uname_Typename_Node       : Types.AST.Uname_Typename_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Bit         =>
        Logger.Info("Typename => BIT");
        A_Predefined_Typename_Node := new Types.AST.Predefined_Typename_Node;
        A_Predefined_Typename_Node.Tag := Types.AST.Node_Predefined_Typename;
        A_Predefined_Typename_Node.Name := Types.Lexer.Token_Bit;
        A_Typename_Node := Types.AST.Typename_Node_Ptr( A_Predefined_Typename_Node );
        Match(Types.Lexer.Token_Bit);
      when  Types.Lexer.Token_Bool        =>
        Logger.Info("Typename => BOOL");
        A_Predefined_Typename_Node := new Types.AST.Predefined_Typename_Node;
        A_Predefined_Typename_Node.Tag := Types.AST.Node_Predefined_Typename;
        A_Predefined_Typename_Node.Name := Types.Lexer.Token_Bool;
        A_Typename_Node := Types.AST.Typename_Node_Ptr( A_Predefined_Typename_Node );
        Match(Types.Lexer.Token_Bool);
      when  Types.Lexer.Token_Byte        =>
        Logger.Info("Typename => BYTE");
        A_Predefined_Typename_Node := new Types.AST.Predefined_Typename_Node;
        A_Predefined_Typename_Node.Tag := Types.AST.Node_Predefined_Typename;
        A_Predefined_Typename_Node.Name := Types.Lexer.Token_Byte;
        A_Typename_Node := Types.AST.Typename_Node_Ptr( A_Predefined_Typename_Node );
        Match(Types.Lexer.Token_Byte);
      when  Types.Lexer.Token_Chan        =>
        Logger.Info("Typename => CHAN");
        A_Predefined_Typename_Node := new Types.AST.Predefined_Typename_Node;
        A_Predefined_Typename_Node.Tag := Types.AST.Node_Predefined_Typename;
        A_Predefined_Typename_Node.Name := Types.Lexer.Token_Chan;
        A_Typename_Node := Types.AST.Typename_Node_Ptr( A_Predefined_Typename_Node );
        Match(Types.Lexer.Token_Chan);
      when  Types.Lexer.Token_Int         =>
        Logger.Info("Typename => INT");
        A_Predefined_Typename_Node := new Types.AST.Predefined_Typename_Node;
        A_Predefined_Typename_Node.Tag := Types.AST.Node_Predefined_Typename;
        A_Predefined_Typename_Node.Name := Types.Lexer.Token_Int;
        A_Typename_Node := Types.AST.Typename_Node_Ptr( A_Predefined_Typename_Node );
        Match(Types.Lexer.Token_Int);
      when  Types.Lexer.Token_Mtype       =>
        Logger.Info("Typename => MTYPE");
        A_Predefined_Typename_Node := new Types.AST.Predefined_Typename_Node;
        A_Predefined_Typename_Node.Tag := Types.AST.Node_Predefined_Typename;
        A_Predefined_Typename_Node.Name := Types.Lexer.Token_Mtype;
        A_Typename_Node := Types.AST.Typename_Node_Ptr( A_Predefined_Typename_Node );
        Match(Types.Lexer.Token_Mtype);
      when  Types.Lexer.Token_Name        =>
        Logger.Info("Typename => NAME");
        An_Uname_Typename_Node := new Types.AST.Uname_Typename_Node;
        An_Uname_Typename_Node.Tag := Types.AST.Node_Uname_Typename;
        An_Uname_Typename_Node.Uname := Uname;
        A_Typename_Node := Types.AST.Typename_Node_Ptr( An_Uname_Typename_Node );
      when  Types.Lexer.Token_Pid         =>
        Logger.Info("Typename => PID");
        A_Predefined_Typename_Node := new Types.AST.Predefined_Typename_Node;
        A_Predefined_Typename_Node.Tag := Types.AST.Node_Predefined_Typename;
        A_Predefined_Typename_Node.Name := Types.Lexer.Token_Pid;
        A_Typename_Node := Types.AST.Typename_Node_Ptr( A_Predefined_Typename_Node );
        Match(Types.Lexer.Token_Pid);
      when  Types.Lexer.Token_Short       =>
        Logger.Info("Typename => SHORT");
        A_Predefined_Typename_Node := new Types.AST.Predefined_Typename_Node;
        A_Predefined_Typename_Node.Tag := Types.AST.Node_Predefined_Typename;
        A_Predefined_Typename_Node.Name := Types.Lexer.Token_Short;
        A_Typename_Node := Types.AST.Typename_Node_Ptr( A_Predefined_Typename_Node );
        Match(Types.Lexer.Token_Short);
      when  Types.Lexer.Token_Unsigned    =>
        Logger.Info("Typename => UNSIGNED");
        A_Predefined_Typename_Node := new Types.AST.Predefined_Typename_Node;
        A_Predefined_Typename_Node.Tag := Types.AST.Node_Predefined_Typename;
        A_Predefined_Typename_Node.Name := Types.Lexer.Token_Unsigned;
        A_Typename_Node := Types.AST.Typename_Node_Ptr( A_Predefined_Typename_Node );
        Match(Types.Lexer.Token_Unsigned);
      when  others                        =>
        Syntax_Error_Output( "Typename" );
    end case;
    return A_Typename_Node;
  end Typename;

  function Ivar return Types.AST.Ivar_Node_Ptr is
    An_Ivar_Node : Types.AST.Ivar_Node_Ptr;
    Name         : Types.Lexer.Bounded_String_Hashed_Set.Cursor;
    Array_Length : Types.AST.Const_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Name        =>
        Logger.Info("Ivar => NAME Ivar_Prime1 Ivar_Prime2");
        Name          :=  Lexer.Current_Token.Lexeme_Cursor;
        Match(Types.Lexer.Token_Name);
        Array_Length  :=  Ivar_Prime1;
        An_Ivar_Node  :=  Ivar_Prime2( Name, Array_Length );
      when  others                        =>
        Syntax_Error_Output( "Ivar" );
    end case;
    return An_Ivar_Node;
  end Ivar;

  function Ivar_Prime1 return Types.AST.Const_Node_Ptr is
    A_Const_Node : Types.AST.Const_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Left_Square_Bracket
                                          =>
        Logger.Info("Ivar_Prime1 => [ Const ]");
        Match(Types.Lexer.Token_Left_Square_Bracket); A_Const_Node := Const; Match(Types.Lexer.Token_Right_Square_Bracket);
      -- FOLLOW(ivar_prime1) |= FOLLOW(ivar) |= FOLLOW(one_decl) |= FOLLOW(decl_lst) |= FOLLOW(module) |= FOLLOW(spec)
      when  Types.Lexer.Token_Active      |
            -- FIRST(ivar_prime2)
            Types.Lexer.Token_Assignment  |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_Comma       |
            Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_EOF         |
            Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Init        |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Never       |
            Types.Lexer.Token_Notrace     |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Proctype    |
            Types.Lexer.Token_Right_Curly_Bracket
                                          |
            Types.Lexer.Token_Right_Parentheses
                                          |
            Types.Lexer.Token_Semicolon   |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Trace       |
            Types.Lexer.Token_Typedef     |
            Types.Lexer.Token_Unsigned    =>
        Logger.Info("Ivar_Prime1 => epsilon");
      when  others                  =>
        Syntax_Error_Output( "Ivar_Prime1" );
    end case;
    return A_Const_Node;
  end Ivar_Prime1;
  
  function Ivar_Prime2( Name : Types.Lexer.Bounded_String_Hashed_Set.Cursor ; Array_Length : Types.AST.Const_Node_Ptr ) return Types.AST.Ivar_Node_Ptr is
    An_Ivar_Node : Types.AST.Ivar_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Assignment  =>
        Logger.Info("Ivar_Prime2 => = Ivar_Prime3");
        Match(Types.Lexer.Token_Assignment); An_Ivar_Node := Ivar_Prime3( Name, Array_Length );
      -- FOLLOW(ivar_prime2) |= FOLLOW(ivar) |= FOLLOW(one_decl) |= FOLLOW(decl_lst) |= FOLLOW(module) |= FOLLOW(spec)
      when  Types.Lexer.Token_Active      |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_Comma       |
            Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_EOF         |
            Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Init        |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Never       |
            Types.Lexer.Token_Notrace     |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Proctype    |
            Types.Lexer.Token_Right_Curly_Bracket
                                          |
            Types.Lexer.Token_Right_Parentheses
                                          |
            Types.Lexer.Token_Semicolon   |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Trace       |
            Types.Lexer.Token_Typedef     |
            Types.Lexer.Token_Unsigned    =>
        Logger.Info("Ivar_Prime2 => epsilon");
        An_Ivar_Node := new Types.AST.Ivar_Node;
        An_Ivar_Node.Tag := Types.AST.Node_Ivar;
        An_Ivar_Node.Name := Name;
        An_Ivar_Node.Array_Length := Array_Length;
      when  others                        =>
        Syntax_Error_Output( "Ivar_Prime2" );
    end case;
    return An_Ivar_Node;
  end Ivar_Prime2;
  
  function Ivar_Prime3( Name : Types.Lexer.Bounded_String_Hashed_Set.Cursor ; Array_Length : Types.AST.Const_Node_Ptr ) return Types.AST.Ivar_Node_Ptr is
    An_Ivar_Node          : Types.AST.Ivar_Node_Ptr;
    An_Any_Expr_Ivar_Node : Types.AST.Any_Expr_Ivar_Node_Ptr;
    A_Ch_Init_Ivar_Node   : Types.AST.Ch_Init_Ivar_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Left_Square_Bracket
                                          =>
        Logger.Info("Ivar_Prime3 => Ch_Init");
        A_Ch_Init_Ivar_Node := new Types.AST.Ch_Init_Ivar_Node;
        A_Ch_Init_Ivar_Node.Tag := Types.AST.Node_Ch_Init_Ivar;
        A_Ch_Init_Ivar_Node.Name := Name;
        A_Ch_Init_Ivar_Node.Array_Length := Array_Length;
        A_Ch_Init_Ivar_Node.Assignment := Ch_Init;
        An_Ivar_Node := Types.AST.Ivar_Node_Ptr( A_Ch_Init_Ivar_Node );
      -- FIRST(any_expr)
      when  Types.Lexer.Token_Enabled     |
            Types.Lexer.Token_False       |
            Types.Lexer.Token_Len         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_Np_Underscore
                                          |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Ones_Complement
                                          |
            Types.Lexer.Token_Pc_Value    |
            Types.Lexer.Token_Run         |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Timeout     |
            Types.Lexer.Token_True        =>
        Logger.Info("Ivar_Prime3 => Any_Expr");
        An_Any_Expr_Ivar_Node := new Types.AST.Any_Expr_Ivar_Node;
        An_Any_Expr_Ivar_Node.Tag := Types.AST.Node_Any_Expr_Ivar;
        An_Any_Expr_Ivar_Node.Name := Name;
        An_Any_Expr_Ivar_Node.Array_Length := Array_Length;
        An_Any_Expr_Ivar_Node.Assignment := Any_Expr;
        An_Ivar_Node := Types.AST.Ivar_Node_Ptr( An_Any_Expr_Ivar_Node );
      when  others                        =>
        Syntax_Error_Output( "Ivar_Prime3" );
    end case;
    return An_Ivar_Node;
  end Ivar_Prime3;

  function Ch_Init return Types.AST.Ch_Init_Node_Ptr is
    A_Ch_Init_Node : Types.AST.Ch_Init_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Left_Square_Bracket
                                    =>
        Logger.Info("Ch_Init => [ Const ] OF { Typename Ch_Init_Prime }");
        A_Ch_Init_Node := new Types.AST.Ch_Init_Node;
        A_Ch_Init_Node.Tag := Types.AST.Node_Ch_Init;
        Match(Types.Lexer.Token_Left_Square_Bracket); A_Ch_Init_Node.Size := Const; Match(Types.Lexer.Token_Right_Square_Bracket); Match(Types.Lexer.Token_Of);
        Match(Types.Lexer.Token_Left_Curly_Bracket); A_Ch_Init_Node.Typename_List.Append( Typename ); Ch_Init_Prime( A_Ch_Init_Node ); Match(Types.Lexer.Token_Right_Curly_Bracket);
      when  others                  =>
        Syntax_Error_Output( "Ch_Init" );
    end case;
    return A_Ch_Init_Node;
  end Ch_Init;

  procedure Ch_Init_Prime( A_Ch_Init_Node : Types.AST.Ch_Init_Node_Ptr ) is
  begin
    Ada.Assertions.Assert( A_Ch_Init_Node /= null, "A_Ch_Init_Node is null!" );
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Comma       =>
        Logger.Info("Ch_Init_Prime => , Typename Ch_Init_Prime");
        Match(Types.Lexer.Token_Comma); A_Ch_Init_Node.Typename_List.Append( Typename ); Ch_Init_Prime( A_Ch_Init_Node );
      when  Types.Lexer.Token_Right_Curly_Bracket
                                          =>
        Logger.Info("Ch_Init_Prime => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Ch_Init_Prime" );
    end case;
  end Ch_Init_Prime;
  
  function Proctype return Types.AST.Proctype_Node_Ptr is
    A_Proctype_Node : Types.AST.Proctype_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Active      |
            Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_Proctype    =>
        Logger.Info("Proctype => Proctype_Prime1 Proctype_Prime5 NAME ( Proctype_Prime2 ) Proctype_Prime3 Proctype_Prime4 { Sequence }");
        A_Proctype_Node := new Types.AST.Proctype_Node;
        A_Proctype_Node.Tag := Types.AST.Node_Proctype;
        A_Proctype_Node.Active := Proctype_Prime1;
        A_Proctype_Node.Version := Proctype_Prime5;
        A_Proctype_Node.Name := Lexer.Current_Token.Lexeme_Cursor;
        Match(Types.Lexer.Token_Name);
        Match(Types.Lexer.Token_Left_Parentheses); A_Proctype_Node.Parameters := Proctype_Prime2; Match(Types.Lexer.Token_Right_Parentheses);
        A_Proctype_Node.Priority := Proctype_Prime3;
        A_Proctype_Node.Enabler := Proctype_Prime4; Match(Types.Lexer.Token_Left_Curly_Bracket);
        A_Proctype_Node.Sequence := Sequence; Match(Types.Lexer.Token_Right_Curly_Bracket);
      when  others                        =>
        Syntax_Error_Output( "Proctype" );
    end case;
    return A_Proctype_Node;
  end Proctype;
  
  function Proctype_Prime1 return Types.AST.Active_Node_Ptr is
    An_Active_Node : Types.AST.Active_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Active      =>
        Logger.Info("Proctype_Prime1 => Active");
        An_Active_Node := Active;
      when  Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_Proctype    =>
        Logger.Info("Proctype_Prime1 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Proctype_Prime1" );
    end case;
    return An_Active_Node;
  end Proctype_Prime1;

  function Proctype_Prime2 return Types.AST.Decl_Lst_Node_Ptr is
    A_Decl_Lst_Node : Types.AST.Decl_Lst_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Unsigned    =>
        Logger.Info("Proctype_Prime2 => Decl_Lst");
        A_Decl_Lst_Node := Decl_Lst;
      when  Types.Lexer.Token_Right_Parentheses
                                          =>
        Logger.Info("Proctype_Prime2 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Proctype_Prime2" );
    end case;
    return A_Decl_Lst_Node;
  end Proctype_Prime2;
  
  function Proctype_Prime3 return Types.AST.Priority_Node_Ptr is
    A_Priority_Node : Types.AST.Priority_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Priority    =>
        Logger.Info("Proctype_Prime3 => Priority");
        A_Priority_Node := Priority;
      when  Types.Lexer.Token_Provided    |
            Types.Lexer.Token_Left_Curly_Bracket
                                          =>
        Logger.Info("Proctype_Prime3 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Proctype_Prime3" );
    end case;
    return A_Priority_Node;
  end Proctype_Prime3;
  
  function Proctype_Prime4 return Types.AST.Enabler_Node_Ptr is
    An_Enabler_Node : Types.AST.Enabler_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Provided    =>
        Logger.Info("Proctype_Prime4 => Enabler");
        An_Enabler_Node := Enabler;
      when  Types.Lexer.Token_Left_Curly_Bracket
                                          =>
        Logger.Info("Proctype_Prime4 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Proctype_Prime4" );
    end case;
    return An_Enabler_Node;
  end Proctype_Prime4;

  function Proctype_Prime5 return Types.Lexer.Token_Type is
    Attribute : Types.Lexer.Token_Type := Types.Lexer.Token_Null;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_D_Proctype  =>
        Logger.Info("Proctype_Prime5 => D_PROCTYPE");
        Match(Types.Lexer.Token_D_Proctype);
        Attribute := Types.Lexer.Token_D_Proctype;
      when  Types.Lexer.Token_Proctype    =>
        Logger.Info("Proctype_Prime5 => PROCTYPE");
        Match(Types.Lexer.Token_Proctype);
        Attribute := Types.Lexer.Token_Proctype;
      when  others                        =>
        Syntax_Error_Output( "Proctype_Prime5" );
    end case;
    return Attribute;
  end Proctype_Prime5;

  function Active return Types.AST.Active_Node_Ptr is
    An_Active_Node : Types.AST.Active_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Active      =>
        Logger.Info("Active => ACTIVE Active_Prime");
        An_Active_Node := new Types.AST.Active_Node;
        An_Active_Node.Tag := Types.AST.Node_Active;
        Match(Types.Lexer.Token_Active); An_Active_Node.Number := Active_Prime;
      when  others                        =>
        Syntax_Error_Output( "Active" );
    end case;
    return An_Active_Node;
  end Active;

  function Active_Prime return Types.AST.Const_Node_Ptr is
    A_Const : Types.AST.Const_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Left_Square_Bracket
                                          =>
        Logger.Info("Active_Prime => [ Const ]");
        Match(Types.Lexer.Token_Left_Square_Bracket); A_Const := Const; Match(Types.Lexer.Token_Right_Square_Bracket);
      when  Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_Proctype    =>
        Logger.Info("Active_Prime => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Active_Prime" );
    end case;
    return A_Const;
  end Active_Prime;

  function Enabler return Types.AST.Enabler_Node_Ptr is
    An_Enabler_Node : Types.AST.Enabler_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Provided    =>
        Logger.Info("Priority => PROVIDED ( Any_Expr )");
        An_Enabler_Node := new Types.AST.Enabler_Node;
        An_Enabler_Node.Tag := Types.AST.Node_Enabler;
        Match(Types.Lexer.Token_Provided); Match(Types.Lexer.Token_Left_Parentheses); An_Enabler_Node.Condition := Any_Expr; Match(Types.Lexer.Token_Right_Parentheses);
      when  others                        =>
        Syntax_Error_Output( "Enabler" );
    end case;
    return An_Enabler_Node;
  end Enabler;

  function Assign_Prime( A_Varref_Node : Types.AST.Varref_Node_Ptr ) return Types.AST.Assign_Node_Ptr is
    An_Assign_Node           : Types.AST.Assign_Node_Ptr;
    An_Any_Expr_Assign_Node  : Types.AST.Any_Expr_Assign_Node_Ptr;
    A_Decrement_Assign_Node  : Types.AST.Decrement_Assign_Node_Ptr;
    An_Increment_Assign_Node : Types.AST.Increment_Assign_Node_Ptr;
  begin
    Ada.Assertions.Assert( A_Varref_Node /= null, "A_Varref_Node is null!" );
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Assignment  =>
        Logger.Info("Assign_Prime => = Any_Expr");
        An_Any_Expr_Assign_Node := new Types.AST.Any_Expr_Assign_Node;
        An_Any_Expr_Assign_Node.Tag := Types.AST.Node_Any_Expr_Assign;
        An_Any_Expr_Assign_Node.L_Value := A_Varref_Node;
        Match(Types.Lexer.Token_Assignment); An_Any_Expr_Assign_Node.R_Value := Any_Expr;
        An_Assign_Node := Types.AST.Assign_Node_Ptr( An_Any_Expr_Assign_Node );
      when  Types.Lexer.Token_Decrement   =>
        Logger.Info("Assign_Prime => --");
        Match(Types.Lexer.Token_Decrement);
        A_Decrement_Assign_Node := new Types.AST.Decrement_Assign_Node;
        A_Decrement_Assign_Node.Tag := Types.AST.Node_Decrement_Assign;
        A_Decrement_Assign_Node.L_Value := A_Varref_Node;
        An_Assign_Node := Types.AST.Assign_Node_Ptr( A_Decrement_Assign_Node );
      when  Types.Lexer.Token_Increment   =>
        Logger.Info("Assign_Prime => ++");
        Match(Types.Lexer.Token_Increment);
        An_Increment_Assign_Node := new Types.AST.Increment_Assign_Node;
        An_Increment_Assign_Node.Tag := Types.AST.Node_Increment_Assign;
        An_Increment_Assign_Node.L_Value := A_Varref_Node;
        An_Assign_Node := Types.AST.Assign_Node_Ptr( An_Increment_Assign_Node );
      when  others                        =>
        Syntax_Error_Output( "Assign_Prime" );
    end case;
    return An_Assign_Node;
  end Assign_Prime;

  function Varref return Types.AST.Varref_Node_Ptr is
    A_Varref_Node : Types.AST.Varref_Node_Ptr;
  begin
    A_Varref_Node := new Types.AST.Varref_Node;
    A_Varref_Node.Tag := Types.ASt.Node_Varref;
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Name        =>
        Logger.Info("Varref => NAME Varref_Prime1 Varref_Prime2");
        A_Varref_Node.Name := Lexer.Current_Token.Lexeme_Cursor;
        -- COMMENTARY: I originally and accidentally wrote this:
        -- A_Varref_Node := Varref_Prime2;
        -- No compile error, but it sure was a heckuva logical error!
        Match(Types.Lexer.Token_Name); A_Varref_Node.Array_Offset := Varref_Prime1; A_Varref_Node.Struct_Offset := Varref_Prime2;
      when  others                        =>
        Syntax_Error_Output( "Varref" );
    end case;
    return A_Varref_Node;
  end Varref;

  -- TODO: verify completeness and accuracy
  function Varref_Prime1 return Types.AST.Any_Expr_Node_Ptr is
    An_Any_Expr_Node : Types.AST.Any_Expr_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Left_Square_Bracket
                                    =>
        Logger.Info("Varref_Prime1 => [ any_expr ]");
        Match(Types.Lexer.Token_Left_Square_Bracket); An_Any_Expr_Node := Any_Expr; Match(Types.Lexer.Token_Right_Square_Bracket);
      -- looks reasonable; FOLLOW(varref_prime1) |= FOLLOW(varref) |= FOLLOW(any_expr) |= FOLLOW(ivar_prime3) |= FOLLOW(ivar) |= FOLLOW(one_decl) |= FOLLOW(decl_lst) |= FOLLOW(step) & FOLLOW(module)
      when  Types.Lexer.Token_Active      |
            Types.Lexer.Token_Add         |
            -- FIRST(any_expr_prime5)
            Types.Lexer.Token_Alias       |
            Types.Lexer.Token_And_Bitwise |
            Types.Lexer.Token_And_Logical |
            Types.Lexer.Token_Assignment  |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_Colon       |
            Types.Lexer.Token_Comma       |
            Types.Lexer.Token_Decrement   |
            Types.Lexer.Token_Divide      |
            Types.Lexer.Token_Dot         |
            Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_Equals      |
            Types.Lexer.Token_EOF         | 
            Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Greater_Than_Or_Equal_To
                                          |
            Types.Lexer.Token_Greater_Than
                                          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Implies     |
            Types.Lexer.Token_Increment   |
            Types.Lexer.Token_Init        |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Left_Shift  |
            Types.Lexer.Token_Less_Than   |
            Types.Lexer.Token_Less_Than_Or_Equal_To
                                          |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Modulus     |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Multiply    |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Not_Equals  |
            Types.Lexer.Token_Never       |
            Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_Notrace     |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Or_Bitwise  |
            Types.Lexer.Token_Or_Logical  |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Proctype    |
            Types.Lexer.Token_Right_Curly_Bracket
                                          |
            Types.Lexer.Token_Receive_1   |
            Types.Lexer.Token_Receive_2   |
            Types.Lexer.Token_Right_Parentheses
                                          |
            Types.Lexer.Token_Right_Shift |
            Types.Lexer.Token_Right_Square_Bracket
                                          |
            Types.Lexer.Token_Semicolon   |
            Types.Lexer.Token_Send_2      |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Trace       |
            Types.Lexer.Token_Typedef     |
            Types.Lexer.Token_Unless      |
            Types.Lexer.Token_Unsigned    |
            Types.Lexer.Token_Xor_Bitwise =>
        Logger.Info("Varref_Prime1 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Varref_Prime1" );
    end case;
    return An_Any_Expr_Node;
  end Varref_Prime1;

  -- TODO: verify completeness and accuracy; see Varref_Prime1
  function Varref_Prime2 return Types.AST.Varref_Node_Ptr is
    A_Varref_Node : Types.AST.Varref_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Dot         =>
        Logger.Info("Varref_Prime2 => . Varref");
        Match(Types.Lexer.Token_Dot); A_Varref_Node := Varref;
      -- looks reasonable; FOLLOW(varref_prime2) |= FOLLOW(varref) |= FOLLOW(any_expr) |= FOLLOW(ivar_prime3) |= FOLLOW(ivar) |= FOLLOW(one_decl) |= FOLLOW(decl_lst) |= FOLLOW(step) & FOLLOW(module)
      when  Types.Lexer.Token_Active      |
            Types.Lexer.Token_Add         |
            -- FIRST(any_expr_prime5)
            Types.Lexer.Token_Alias       |
            Types.Lexer.Token_And_Bitwise |
            Types.Lexer.Token_And_Logical |
            Types.Lexer.Token_Assignment  |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_Colon       |
            Types.Lexer.Token_Comma       |
            Types.Lexer.Token_Decrement   |
            Types.Lexer.Token_Divide      |
            Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_Equals      |
            Types.Lexer.Token_EOF         | 
            Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Greater_Than_Or_Equal_To
                                          |
            Types.Lexer.Token_Greater_Than
                                          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Implies     |
            Types.Lexer.Token_Increment   |
            Types.Lexer.Token_Init        |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Left_Shift  |
            Types.Lexer.Token_Less_Than   |
            Types.Lexer.Token_Less_Than_Or_Equal_To
                                          |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Modulus     |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Multiply    |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Not_Equals  |
            Types.Lexer.Token_Never       |
            Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_Notrace     |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Or_Bitwise  |
            Types.Lexer.Token_Or_Logical  |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Proctype    |
            Types.Lexer.Token_Right_Curly_Bracket
                                          |
            Types.Lexer.Token_Receive_1   |
            Types.Lexer.Token_Receive_2   |
            Types.Lexer.Token_Right_Parentheses
                                          |
            Types.Lexer.Token_Right_Shift |
            Types.Lexer.Token_Right_Square_Bracket
                                          |
            Types.Lexer.Token_Semicolon   |
            Types.Lexer.Token_Send_2      |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Trace       |
            Types.Lexer.Token_Typedef     |
            Types.Lexer.Token_Unless      |
            Types.Lexer.Token_Unsigned    |
            Types.Lexer.Token_Xor_Bitwise =>
        Logger.Info("Varref_Prime2 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Varref_Prime2" );
    end case;
    return A_Varref_Node;
  end Varref_Prime2;

  function Send_Prime( A_Varref_Node : Types.AST.Varref_Node_Ptr ) return Types.AST.Send_Node_Ptr is
    A_Send_Node         : Types.AST.Send_Node_Ptr;
    A_Fifo_Send_Node    : Types.AST.Fifo_Send_Node_Ptr;
    A_Sorted_Send_Node  : Types.AST.Sorted_Send_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Not_Or_Send_1
                                          =>
        Logger.Info("Send_Prime => ! Send_Args");
        Match(Types.Lexer.Token_Not_Or_Send_1);
        A_Fifo_Send_Node        := new Types.AST.Fifo_Send_Node;
        A_Fifo_Send_Node.Tag    := Types.AST.Node_Fifo_Send;
        A_Send_Node             := Types.AST.Send_Node_Ptr( A_Fifo_Send_Node );
      when  Types.Lexer.Token_Send_2      =>
        Logger.Info("Send_Prime => !! Send_Args");
        Match(Types.Lexer.Token_Send_2);
        A_Sorted_Send_Node      := new Types.AST.Sorted_Send_Node;
        A_Sorted_Send_Node.Tag  := Types.AST.Node_Sorted_Send;
        A_Send_Node             := Types.AST.Send_Node_Ptr( A_Sorted_Send_Node );
      when  others                        =>
        Syntax_Error_Output( "Send_Prime" );
    end case;
    A_Send_Node.Source     := A_Varref_Node;
    A_Send_Node.Arguments  := Send_Args;
    return A_Send_Node;
  end Send_Prime;

  function Receive_Prime( A_Varref_Node : Types.AST.Varref_Node_Ptr ) return Types.AST.Recv_Node_Ptr is
    A_Recv_Node : Types.AST.Recv_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Receive_1   =>
        Logger.Info("Receive_Prime => ? Receive_Prime_Prime");
        Match(Types.Lexer.Token_Receive_1);
        A_Recv_Node := Receive_Prime_Prime( Is_Fifo => True, A_Varref_Node => A_Varref_Node );
      when  Types.Lexer.Token_Receive_2   =>
        Logger.Info("Receive_Prime => ?? Receive_Prime_Prime");
        Match(Types.Lexer.Token_Receive_2);
        A_Recv_Node := Receive_Prime_Prime( Is_Fifo => False, A_Varref_Node => A_Varref_Node );
      when  others                        =>
        Syntax_Error_Output( "Receive_Prime" );
    end case;
    return A_Recv_Node;
  end Receive_Prime;

  function Receive_Prime_Prime( Is_Fifo : Boolean ; A_Varref_Node : Types.AST.Varref_Node_Ptr ) return Types.AST.Recv_Node_Ptr is
    A_Recv_Node : Types.AST.Recv_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Less_Than   =>
        Logger.Info("Receive_Prime_Prime => < Recv_Args >");
        if Is_Fifo = True then
          A_Recv_Node     := new Types.AST.Copy_Fifo_Recv_Node;
          A_Recv_Node.Tag := Types.AST.Node_Copy_Fifo_Recv;
        else
          A_Recv_Node     := new Types.AST.Copy_Random_Recv_Node;
          A_Recv_Node.Tag := Types.AST.Node_Copy_Random_Recv;
        end if;
        A_Recv_Node.Target    := A_Varref_Node;
        Match(Types.Lexer.Token_Less_Than);
        A_Recv_Node.Arguments := Recv_Args;
        Match(Types.Lexer.Token_Greater_Than);
      -- NOTE: should be correct; FIRST(recv_args) = FIRST(recv_arg)
      when  Types.Lexer.Token_Eval        | 
            Types.Lexer.Token_False       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_True        =>
        Logger.Info("Receive_Prime_Prime => Recv_Args");
        if Is_Fifo = True then
          A_Recv_Node     := new Types.AST.Move_Fifo_Recv_Node;
          A_Recv_Node.Tag := Types.AST.Node_Move_Fifo_Recv;
        else
          A_Recv_Node     := new Types.AST.Move_Random_Recv_Node;
          A_Recv_Node.Tag := Types.AST.Node_Move_Random_Recv;
        end if;
        A_Recv_Node.Target    := A_Varref_Node;
        A_Recv_Node.Arguments := Recv_Args;
      when  others                        =>
        Syntax_Error_Output( "Receive_Prime_Prime" );
    end case;
    return A_Recv_Node;
  end Receive_Prime_Prime;

  -- Clearly some code can be refactored here, but at least it is readable.
  function Poll_Prime( A_Varref_Node : Types.AST.Varref_Node_Ptr ) return Types.AST.Poll_Node_Ptr is
    A_Poll_Node : Types.AST.Poll_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Receive_1   =>
        Logger.Info("Poll_Prime => ? [ Recv_Args ]");
        A_Poll_Node           := new Types.AST.Fifo_Poll_Node;
        A_Poll_Node.Tag       := Types.AST.Node_Fifo_Poll;
        A_Poll_Node.Target    := A_Varref_Node;
        Match(Types.Lexer.Token_Receive_1);
        Match(Types.Lexer.Token_Left_Square_Bracket);
        A_Poll_Node.Arguments := Recv_Args;
        Match(Types.Lexer.Token_Right_Square_Bracket);
      when  Types.Lexer.Token_Receive_2   =>
        Logger.Info("Poll_Prime => ?? [ Recv_Args ]");
        A_Poll_Node           := new Types.AST.Random_Poll_Node;
        A_Poll_Node.Tag       := Types.AST.Node_Random_Poll;
        A_Poll_Node.Target    := A_Varref_Node;
        Match(Types.Lexer.Token_Receive_2);
        Match(Types.Lexer.Token_Left_Square_Bracket);
        A_Poll_Node.Arguments := Recv_Args;
        Match(Types.Lexer.Token_Right_Square_Bracket);
      when  others                        =>
        Syntax_Error_Output( "Poll_Prime" );
    end case;
    return A_Poll_Node;
  end Poll_Prime;

  function Send_Args return Types.AST.Send_Args_Node_Ptr is
    A_Send_Args_Node  : Types.AST.Send_Args_Node_Ptr;
    An_Any_Expr_Node  : Types.AST.Any_Expr_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      -- FIRST(any_expr)
      when  Types.Lexer.Token_Enabled     |
            Types.Lexer.Token_False       |
            Types.Lexer.Token_Len         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_Np_Underscore
                                          |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Ones_Complement
                                          |
            Types.Lexer.Token_Pc_Value    |
            Types.Lexer.Token_Run         |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Timeout     |
            Types.Lexer.Token_True        =>
        Logger.Info("Send_Args => Any_Expr Send_Args_Prime");
        An_Any_Expr_Node := Any_Expr;
        A_Send_Args_Node := Send_Args_Prime( An_Any_Expr_Node ); 
      when  others                        =>
        Syntax_Error_Output( "Send_Args" );
    end case;
    return A_Send_Args_Node;
  end Send_Args;

  -- TODO: verify accuracy and completeness
  function Send_Args_Prime( An_Any_Expr_Node : Types.AST.Any_Expr_Node_Ptr ) return Types.AST.Send_Args_Node_Ptr is
    A_Send_Args_Node  : Types.AST.Send_Args_Node_Ptr;
    An_Arg_Lst_Node   : Types.AST.Arg_Lst_Node_Ptr;
  begin
    A_Send_Args_Node      := new Types.AST.Send_Args_Node;
    A_Send_Args_Node.Tag  := Types.AST.Node_Send_Args;
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Left_Parentheses
                                          =>
        Logger.Info("Send_Args_Prime => ( Arg_Lst )");
        Match(Types.Lexer.Token_Left_Parentheses);
        -- do the rest of the any_expr
        An_Arg_Lst_Node   := Arg_Lst;
        Match(Types.Lexer.Token_Right_Parentheses);
        -- now slip in the first any_expr
        An_Arg_Lst_Node.Any_Expr_List.Prepend( An_Any_Expr_Node );
      -- FIRST(comma)
      when  Types.Lexer.Token_Comma       |
            -- FOLLOW(send_args_prime) |= FOLLOW(send_args) |= FOLLOW(send_prime) |= FOLLOW(send) |= FOLLOW(stmt) |= FOLLOW(step) |= FOLLOW(sequence)
            Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Right_Curly_Bracket
                                          |
            Types.Lexer.Token_Implies     |
            Types.Lexer.Token_Semicolon   |
            Types.Lexer.Token_Unless      =>
        Logger.Info("Send_Args_Prime => Arg_Lst_Prime");
        -- replicate Arg_Lst functionality here
        An_Arg_Lst_Node       := new Types.AST.Arg_Lst_Node;
        An_Arg_Lst_Node.Tag   := Types.AST.Node_Arg_Lst;
        -- slip in the first any_expr
        An_Arg_Lst_Node.Any_Expr_List.Append( An_Any_Expr_Node );
        -- now do the rest of the any_expr
        Arg_Lst_Prime( An_Arg_Lst_Node );
      when  others                        =>
        Syntax_Error_Output( "Send_Args_Prime" );
    end case;
    A_Send_Args_Node.Arguments := An_Arg_Lst_Node;
    return A_Send_Args_Node;
  end Send_Args_Prime;

  function Arg_Lst return Types.AST.Arg_Lst_Node_Ptr is
    An_Arg_Lst_Node : Types.AST.Arg_Lst_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      -- FIRST(any_expr)
      when  Types.Lexer.Token_Enabled     |
            Types.Lexer.Token_False       |
            Types.Lexer.Token_Len         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_Np_Underscore
                                          |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Ones_Complement
                                          |
            Types.Lexer.Token_Pc_Value    |
            Types.Lexer.Token_Run         |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Timeout     |
            Types.Lexer.Token_True        =>
        Logger.Info("Arg_Lst => Any_Expr Arg_Lst_Prime");
        An_Arg_Lst_Node := new Types.AST.Arg_Lst_Node;
        An_Arg_Lst_Node.Tag := Types.AST.Node_Arg_Lst;
        An_Arg_Lst_Node.Any_Expr_List.Append( Any_Expr ); Arg_Lst_Prime( An_Arg_Lst_Node );
      when  others                        =>
        Syntax_Error_Output( "Arg_Lst" );
    end case;
    return An_Arg_Lst_Node;
  end Arg_Lst;

  -- TODO: verify; related to Send_Args_Prime
  procedure Arg_Lst_Prime( An_Arg_Lst_Node : Types.AST.Arg_Lst_Node_Ptr ) is
  begin
    Ada.Assertions.Assert( An_Arg_Lst_Node /= null, "An_Arg_Lst_Node is null!" );
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Comma
                                          =>
        Logger.Info("Arg_Lst_Prime => , Any_Expr Arg_Lst_Prime");
        Match(Types.Lexer.Token_Comma); An_Arg_Lst_Node.Any_Expr_List.Append( Any_Expr ); Arg_Lst_Prime( An_Arg_Lst_Node );
      -- FOLLOW(arg_lst_prime) |= FOLLOW(send_args_prime) |= FOLLOW(send_args) |= FOLLOW(send_prime) |= FOLLOW(send) |= FOLLOW(stmt) |= FOLLOW(step) |= FOLLOW(sequence)
      when  Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Right_Curly_Bracket
                                          |
            -- FOLLOW(arg_lst)
            Types.Lexer.Token_Right_Parentheses
                                          |
            Types.Lexer.Token_Implies     |
            Types.Lexer.Token_Semicolon   |
            Types.Lexer.Token_Unless      =>
        Logger.Info("Arg_Lst_Prime => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Arg_Lst_Prime" );
    end case;
  end Arg_Lst_Prime;

  function Recv_Args return Types.AST.Recv_Args_Node_Ptr is
    A_Recv_Arg_Node   : Types.AST.Recv_Arg_Node_Ptr;
    A_Recv_Args_Node  : Types.AST.Recv_Args_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      -- NOTE: should be correct; FIRST(recv_args) = FIRST(recv_arg) 
      when  Types.Lexer.Token_Eval        |
            Types.Lexer.Token_False       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_True        =>
        Logger.Info("Recv_Args => Recv_Arg Recv_Args_Prime");
        A_Recv_Arg_Node   := Recv_Arg;
        A_Recv_Args_Node  := Recv_Args_Prime( A_Recv_Arg_Node );
      when  others                        =>
        Syntax_Error_Output( "Recv_Args" );
    end case;
    return A_Recv_Args_Node;
  end Recv_Args;

  -- TODO: verify, just in case
  function Recv_Args_Prime( A_Recv_Arg_Node : Types.AST.Recv_Arg_Node_Ptr ) return Types.AST.Recv_Args_Node_Ptr is
    A_Recv_Args_Node  : Types.AST.Recv_Args_Node_Ptr;
  begin
    Ada.Assertions.Assert( A_Recv_Arg_Node /= null, "A_Recv_Arg_Node is null!" );
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Left_Parentheses
                                          =>
        Logger.Info("Recv_Args_Prime => ( Recv_Args )");
        Match(Types.Lexer.Token_Left_Parentheses);
        A_Recv_Args_Node := Recv_Args;
        Match(Types.Lexer.Token_Right_Parentheses);
        A_Recv_Args_Node.Arguments.Prepend( A_Recv_Arg_Node );
      -- FOLLOW(recv_args_prime) |= FOLLOW(recv_args) |= FOLLOW(receive_prime_prime) |= FOLLOW(receive_prime) |= FOLLOW(receive) |=
      -- FOLLOW(stmt) |= FOLLOW(step) |= FOLLOW(sequence)
      when  Types.Lexer.Token_Comma       |
            Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Right_Curly_Bracket
                                          |
            -- FOLLOW(recv_args)
            Types.Lexer.Token_Right_Parentheses
                                          |
            Types.Lexer.Token_Greater_Than|
            Types.Lexer.Token_Right_Square_Bracket
                                          |
            Types.Lexer.Token_Implies     |
            Types.Lexer.Token_Semicolon   |
            Types.Lexer.Token_Unless      =>
        Logger.Info("Recv_Args_Prime => Recv_Args_Prime_Prime");
        A_Recv_Args_Node := Recv_Args_Prime_Prime( A_Recv_Arg_Node );
      when  others                        =>
        Syntax_Error_Output( "Recv_Args_Prime" );
    end case;
    return A_Recv_Args_Node;
  end Recv_Args_Prime;

  -- COMMENTARY: I don't understand why I separated it this way. I must unify this.
  function Recv_Args_Prime_Prime( A_Recv_Arg_Node : Types.AST.Recv_Arg_Node_Ptr ) return Types.AST.Recv_Args_Node_Ptr is
    A_Recv_Args_Node : Types.AST.Recv_Args_Node_Ptr;
  begin
    Ada.Assertions.Assert( A_Recv_Arg_Node /= null, "A_Recv_Arg_Node is null!" );
    A_Recv_Args_Node      := new Types.AST.Recv_Args_Node;
    A_Recv_Args_Node.Tag  := Types.AST.Node_Recv_Args;
    A_Recv_Args_Node.Arguments.Append( A_Recv_Arg_Node );
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Comma       =>
        Logger.Info("Recv_Args_Prime_Prime => , Recv_Arg Recv_Args_Prime_Prime");
        Match(Types.Lexer.Token_Comma);
        A_Recv_Args_Node.Arguments.Append( Recv_Arg );
        Recv_Args_Prime_Prime( A_Recv_Args_Node );
      -- FOLLOW(recv_args_prime_prime) |= FOLLOW(recv_args_prime) |= FOLLOW(recv_args) |= FOLLOW(receive_prime_prime) |=
      -- FOLLOW(receive_prime) |= FOLLOW(receive) |= FOLLOW(stmt) |= FOLLOW(step) |= FOLLOW(sequence)
      when  Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Right_Curly_Bracket
                                          |
            -- FOLLOW(recv_args)
            Types.Lexer.Token_Right_Parentheses
                                          |
            Types.Lexer.Token_Greater_Than|
            Types.Lexer.Token_Right_Square_Bracket
                                          |
            Types.Lexer.Token_Implies     |
            Types.Lexer.Token_Semicolon   |
            Types.Lexer.Token_Unless      =>
        Logger.Info("Recv_Args_Prime_Prime => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Recv_Args_Prime_Prime" );
    end case;
    return A_Recv_Args_Node;
  end Recv_Args_Prime_Prime;

  procedure Recv_Args_Prime_Prime( A_Recv_Args_Node : Types.AST.Recv_Args_Node_Ptr ) is
  begin
    Ada.Assertions.Assert( A_Recv_Args_Node /= null, "A_Recv_Args_Node is null!" );
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Comma       =>
        Logger.Info("Recv_Args_Prime_Prime => , Recv_Arg Recv_Args_Prime_Prime");
        Match(Types.Lexer.Token_Comma);
        A_Recv_Args_Node.Arguments.Append( Recv_Arg );
        Recv_Args_Prime_Prime( A_Recv_Args_Node );
      -- FOLLOW(recv_args_prime_prime) |= FOLLOW(recv_args_prime) |= FOLLOW(recv_args) |= FOLLOW(receive_prime_prime) |=
      -- FOLLOW(receive_prime) |= FOLLOW(receive) |= FOLLOW(stmt) |= FOLLOW(step) |= FOLLOW(sequence)
      when  Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Right_Curly_Bracket
                                          |
            -- FOLLOW(recv_args)
            Types.Lexer.Token_Right_Parentheses
                                          |
            Types.Lexer.Token_Greater_Than|
            Types.Lexer.Token_Right_Square_Bracket
                                          |
            Types.Lexer.Token_Implies     |
            Types.Lexer.Token_Semicolon   |
            Types.Lexer.Token_Unless      =>
        Logger.Info("Recv_Args_Prime_Prime => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Recv_Args_Prime_Prime" );
    end case;
  end Recv_Args_Prime_Prime;

  function Recv_Arg return Types.AST.Recv_Arg_Node_Ptr is
    A_Recv_Arg_Node         : Types.AST.Recv_Arg_Node_Ptr;
    An_Eval_Recv_Arg_Node   : Types.AST.Eval_Recv_Arg_Node_Ptr;
    A_Varref_Recv_Arg_Node  : Types.AST.Varref_Recv_Arg_Node_Ptr;
    A_Const_Recv_Arg_Node   : Types.AST.Const_Recv_Arg_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Eval        =>
        Logger.Info("Recv_Arg => EVAL ( Varref )");
        An_Eval_Recv_Arg_Node := new Types.AST.Eval_Recv_Arg_Node;
        An_Eval_Recv_Arg_Node.Tag := Types.AST.Node_Eval_Recv_Arg;
        Match(Types.Lexer.Token_Eval); Match(Types.Lexer.Token_Left_Parentheses); An_Eval_Recv_Arg_Node.Argument := Varref; Match(Types.Lexer.Token_Right_Parentheses);
        A_Recv_Arg_Node := Types.AST.Recv_Arg_Node_Ptr( An_Eval_Recv_Arg_Node );
      when  Types.Lexer.Token_Name        =>
        Logger.Info("Recv_Arg => Varref");
        A_Varref_Recv_Arg_Node := new Types.AST.Varref_Recv_Arg_Node;
        A_Varref_Recv_Arg_Node.Tag := Types.AST.Node_Varref_Recv_Arg;
        A_Varref_Recv_Arg_Node.Variable := Varref;
        A_Recv_Arg_Node := Types.AST.Recv_Arg_Node_Ptr( A_Varref_Recv_Arg_Node );
      when  Types.Lexer.Token_False       |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_True        =>
        Logger.Info("Recv_Arg => Recv_Arg_Prime Const");
        A_Const_Recv_Arg_Node := new Types.AST.Const_Recv_Arg_Node;
        A_Const_Recv_Arg_Node.Tag := Types.AST.Node_Const_Recv_Arg;
        A_Const_Recv_Arg_Node.Negate := Recv_Arg_Prime;
        A_Const_Recv_Arg_Node.A_Constant := Const;
        A_Recv_Arg_Node := Types.AST.Recv_Arg_Node_Ptr( A_Const_Recv_Arg_Node );
      when  others                        =>
        Syntax_Error_Output( "Recv_Arg" );
    end case;
    return A_Recv_Arg_Node;
  end Recv_Arg;

  function Recv_Arg_Prime return Boolean is
    Negate : Boolean;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Subtract    =>
        Logger.Info("Recv_Arg_Prime => -");
        Match(Types.Lexer.Token_Subtract);
        Negate := True;
      when  Types.Lexer.Token_False       |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_True        =>
        Logger.Info("Recv_Arg_Prime => epsilon");
        Negate := False;
      when  others                        =>
        Syntax_Error_Output( "Recv_Arg_Prime" );
    end case;
    return Negate;
  end Recv_Arg_Prime;

  -- FIXME: This function is messed up when it comes to NAME as the first token.
  -- Well, this is what happens when you do not refactor your grammar properly!
  function Stmt return Types.AST.Stmt_Node_Ptr is
    Token1                : Types.Lexer.Token;
    Token2                : Types.Lexer.Token;
    Token3                : Types.Lexer.Token;
    Temp_Token_List       : Types.Parser.Token_List.List;
    Goto_Stmt_Prime3      : Boolean := False;

    A_Varref_Node         : Types.AST.Varref_Node_Ptr;
    An_Any_Expr_Stmt_Node : Types.AST.Any_Expr_Stmt_Node_Ptr;
    An_Assert_Stmt_Node   : Types.AST.Assert_Stmt_Node_Ptr;
    An_Atomic_Stmt_Node   : Types.AST.Atomic_Stmt_Node_Ptr;
    A_Break_Stmt_Node     : Types.AST.Break_Stmt_Node_Ptr;
    A_C_Code_Stmt_Node    : Types.AST.C_Code_Stmt_Node_Ptr;
    A_C_Decl_Stmt_Node    : Types.AST.C_Decl_Stmt_Node_Ptr;
    A_C_Expr_Stmt_Node    : Types.AST.C_Expr_Stmt_Node_Ptr;
    A_C_State_Stmt_Node   : Types.AST.C_State_Stmt_Node_Ptr;
    A_C_Track_Stmt_Node   : Types.AST.C_Track_Stmt_Node_Ptr;
    A_Do_Stmt_Node        : Types.AST.Do_Stmt_Node_Ptr;
    A_D_Step_Stmt_Node    : Types.AST.D_Step_Stmt_Node_Ptr;
    An_Else_Stmt_Node     : Types.AST.Else_Stmt_Node_Ptr;
    A_Goto_Stmt_Node      : Types.AST.Goto_Stmt_Node_Ptr;
    An_If_Stmt_Node       : Types.AST.If_Stmt_Node_Ptr;
    A_Name_Stmt_Node      : Types.AST.Name_Stmt_Node_Ptr;
    A_Printf_Stmt_Node    : Types.AST.Printf_Stmt_Node_Ptr;
    A_Printm_Stmt_Node    : Types.AST.Printm_Stmt_Node_Ptr;
    A_Sequence_Stmt_Node  : Types.AST.Sequence_Stmt_Node_Ptr;
    A_Stmt_Node           : Types.AST.Stmt_Node_Ptr;

    -- CodeGen
    Line_Number           : Natural;
  begin
    -- line numbering for CodeGen
    Line_Number       := Look_Ahead.Line_Number;
    -- FIXME: can we clear a String?
    Statement_Text    := Ada.Strings.Unbounded.Null_Unbounded_String;
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Assert      =>
        Logger.Info("Stmt => ASSERT Any_Expr");
        Accumulate_Tokens := True;
        An_Assert_Stmt_Node := new Types.AST.Assert_Stmt_Node;
        An_Assert_Stmt_Node.Tag := Types.AST.Node_Assert_Stmt;
        Match(Types.Lexer.Token_Assert); An_Assert_Stmt_Node.Expression := Any_Expr;
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( An_Assert_Stmt_Node );
        Accumulate_Tokens := False;
        A_Stmt_Node.Text := Statement_Text;
      when  Types.Lexer.Token_Atomic      =>
        Logger.Info("Stmt => ATOMIC { Sequence }");
        An_Atomic_Stmt_Node := new Types.AST.Atomic_Stmt_Node;
        An_Atomic_Stmt_Node.Tag := Types.AST.Node_Atomic_Stmt;
        Match(Types.Lexer.Token_Atomic); Match(Types.Lexer.Token_Left_Curly_Bracket); An_Atomic_Stmt_Node.Sequence := Sequence; Match(Types.Lexer.Token_Right_Curly_Bracket);
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( An_Atomic_Stmt_Node );
        A_Stmt_Node.Text := Ada.Strings.Unbounded.To_Unbounded_String( "atomic" );
      when  Types.Lexer.Token_Break       =>
        Logger.Info("Stmt => BREAK");
        Match(Types.Lexer.Token_Break);
        A_Break_Stmt_Node := new Types.AST.Break_Stmt_Node;
        A_Break_Stmt_Node.Tag := Types.AST.Node_Break_Stmt;
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_Break_Stmt_Node );
        A_Stmt_Node.Text := Ada.Strings.Unbounded.To_Unbounded_String( "break" );
      when  Types.Lexer.Token_C_Code      =>
        Logger.Info("Stmt => C_CODE { ... }");
        Match(Types.Lexer.Token_C_Code); Match(Types.Lexer.Token_Left_Curly_Bracket);
        loop
          Get_Token;
          -- NOTE: ignore everything in between, assuming the Types.Lexer properly tokenizes the code
          exit when Look_Ahead.Kind = Types.Lexer.Token_Right_Curly_Bracket;
        end loop;
        Logger.Info("Sorry, Pomegranate does not, as yet, support C_CODE! Ignoring all C code...");
        Match(Types.Lexer.Token_Right_Curly_Bracket);
        A_C_Code_Stmt_Node := new Types.AST.C_Code_Stmt_Node;
        A_C_Code_Stmt_Node.Tag := Types.AST.Node_C_Code_Stmt;
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_C_Code_Stmt_Node );
      when  Types.Lexer.Token_C_Decl      =>
        Logger.Info("Stmt => C_DECL { ... }");
        Match(Types.Lexer.Token_C_Decl); Match(Types.Lexer.Token_Left_Curly_Bracket);
        loop
          Get_Token;
          -- NOTE: ignore everything in between, assuming the Types.Lexer properly tokenizes the code
          exit when Look_Ahead.Kind = Types.Lexer.Token_Right_Curly_Bracket;
        end loop;
        Logger.Info("Sorry, Pomegranate does not, as yet, support C_DECL! Ignoring all C code...");
        Match(Types.Lexer.Token_Right_Curly_Bracket);
        A_C_Decl_Stmt_Node := new Types.AST.C_Decl_Stmt_Node;
        A_C_Decl_Stmt_Node.Tag := Types.AST.Node_C_Decl_Stmt;
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_C_Decl_Stmt_Node );
      when  Types.Lexer.Token_C_Expr      =>
        Logger.Info("Stmt => C_EXPR { ... }");
        Match(Types.Lexer.Token_C_Expr); Match(Types.Lexer.Token_Left_Curly_Bracket);
        loop
          Get_Token;
          -- NOTE: ignore everything in between, assuming the Types.Lexer properly tokenizes the code
          exit when Look_Ahead.Kind = Types.Lexer.Token_Right_Curly_Bracket;
        end loop;
        Logger.Info("Sorry, Pomegranate does not, as yet, support C_EXPR! Ignoring all C code...");
        Match(Types.Lexer.Token_Right_Curly_Bracket);
        A_C_Expr_Stmt_Node := new Types.AST.C_Expr_Stmt_Node;
        A_C_Expr_Stmt_Node.Tag := Types.AST.Node_C_Expr_Stmt;
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_C_Expr_Stmt_Node );
      when  Types.Lexer.Token_C_State     =>
        Logger.Info("Stmt => C_STATE { ... }");
        Match(Types.Lexer.Token_C_State); Match(Types.Lexer.Token_Left_Curly_Bracket);
        loop
          Get_Token;
          -- NOTE: ignore everything in between, assuming the Types.Lexer properly tokenizes the code
          exit when Look_Ahead.Kind = Types.Lexer.Token_Right_Curly_Bracket;
        end loop;
        Logger.Info("Sorry, Pomegranate does not, as yet, support C_STATE! Ignoring all C code...");
        Match(Types.Lexer.Token_Right_Curly_Bracket);
        A_C_State_Stmt_Node := new Types.AST.C_State_Stmt_Node;
        A_C_State_Stmt_Node.Tag := Types.AST.Node_C_State_Stmt;
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_C_State_Stmt_Node );
      when  Types.Lexer.Token_C_Track     =>
        Logger.Info("Stmt => C_TRACK { ... }");
        Match(Types.Lexer.Token_C_Track); Match(Types.Lexer.Token_Left_Curly_Bracket);
        loop
          Get_Token;
          -- NOTE: ignore everything in between, assuming the Types.Lexer properly tokenizes the code
          exit when Look_Ahead.Kind = Types.Lexer.Token_Right_Curly_Bracket;
        end loop;
        Logger.Info("Sorry, Pomegranate does not, as yet, support C_TRACK! Ignoring all C code...");
        Match(Types.Lexer.Token_Right_Curly_Bracket);
        A_C_Track_Stmt_Node := new Types.AST.C_Track_Stmt_Node;
        A_C_Track_Stmt_Node.Tag := Types.AST.Node_C_Track_Stmt;
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_C_Track_Stmt_Node );
      when  Types.Lexer.Token_Do          =>
        Logger.Info("Stmt => DO Options OD");
        A_Do_Stmt_Node := new Types.AST.Do_Stmt_Node;
        A_Do_Stmt_Node.Tag := Types.AST.Node_Do_Stmt;
        Match(Types.Lexer.Token_Do); A_Do_Stmt_Node.Options := Options; Match(Types.Lexer.Token_Od);
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_Do_Stmt_Node );
        A_Stmt_Node.Text := Ada.Strings.Unbounded.To_Unbounded_String( "do" );
      when  Types.Lexer.Token_D_Step      =>
        Logger.Info("Stmt => D_STEP { Sequence }");
        A_D_Step_Stmt_Node := new Types.AST.D_Step_Stmt_Node;
        A_D_Step_Stmt_Node.Tag := Types.AST.Node_D_Step_Stmt;
        Match(Types.Lexer.Token_D_Step); Match(Types.Lexer.Token_Left_Curly_Bracket); A_D_Step_Stmt_Node.Sequence := Sequence; Match(Types.Lexer.Token_Right_Curly_Bracket);
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_D_Step_Stmt_Node );
        A_Stmt_Node.Text := Ada.Strings.Unbounded.To_Unbounded_String( "d_step" );
      when  Types.Lexer.Token_Else        =>
        Logger.Info("Stmt => ELSE");
        Match(Types.Lexer.Token_Else);
        An_Else_Stmt_Node := new Types.AST.Else_Stmt_Node;
        An_Else_Stmt_Node.Tag := Types.AST.Node_Else_Stmt;
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( An_Else_Stmt_Node );
        A_Stmt_Node.Text := Ada.Strings.Unbounded.To_Unbounded_String( "else" );
      when  Types.Lexer.Token_Empty       |
            Types.Lexer.Token_Enabled     |
            Types.Lexer.Token_False       |
            Types.Lexer.Token_Full        |
            Types.Lexer.Token_Len         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Nempty      |
            Types.Lexer.Token_Nfull       |
            Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_Np_Underscore
                                          |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Ones_Complement
                                          |
            Types.Lexer.Token_Pc_Value    |
            Types.Lexer.Token_Run         |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Timeout     |
            Types.Lexer.Token_True        =>
        Logger.Info("Stmt => Any_Expr");
        Accumulate_Tokens := True;
        An_Any_Expr_Stmt_Node := new Types.AST.Any_Expr_Stmt_Node;
        An_Any_Expr_Stmt_Node.Tag := Types.AST.Node_Any_Expr_Stmt;
        An_Any_Expr_Stmt_Node.Expression := Any_Expr;
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( An_Any_Expr_Stmt_Node );
        Accumulate_Tokens := False;
        A_Stmt_Node.Text := Statement_Text;
      when  Types.Lexer.Token_Goto        =>
        Logger.Info("Stmt => GOTO NAME");
        A_Goto_Stmt_Node := new Types.AST.Goto_Stmt_Node;
        A_Goto_Stmt_Node.Tag := Types.AST.Node_Goto_Stmt;
        Match(Types.Lexer.Token_Goto); A_Goto_Stmt_Node.Name := Lexer.Current_Token.Lexeme_Cursor; Match(Types.Lexer.Token_Name);
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_Goto_Stmt_Node );
        A_Stmt_Node.Text := Ada.Strings.Unbounded.To_Unbounded_String( "goto" );
      when  Types.Lexer.Token_If          =>
        Logger.Info("Stmt => IF Options FI");
        An_If_Stmt_Node := new Types.AST.If_Stmt_Node;
        An_If_Stmt_Node.Tag := Types.AST.Node_If_Stmt;
        Match(Types.Lexer.Token_If); An_If_Stmt_Node.Options := Options; Match(Types.Lexer.Token_Fi);
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( An_If_Stmt_Node );
        A_Stmt_Node.Text := Ada.Strings.Unbounded.To_Unbounded_String( "if" );
      when  Types.Lexer.Token_Left_Curly_Bracket
                                          =>
        Logger.Info("Stmt => { Sequence }");
        A_Sequence_Stmt_Node := new Types.AST.Sequence_Stmt_Node;
        A_Sequence_Stmt_Node.Tag := Types.AST.Node_Sequence_Stmt;
        Match(Types.Lexer.Token_Left_Curly_Bracket); A_Sequence_Stmt_Node.Sequence := Sequence; Match(Types.Lexer.Token_Right_Curly_Bracket);
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_Sequence_Stmt_Node );

      when  Types.Lexer.Token_Name        =>

        -- FIXME: refactor CONFLICT: manual resolution; slows down compilation
        Token1 := Look_Ahead;
        Get_Token;
        Token2 := Look_Ahead;
        case Look_Ahead.Kind is

          when  Types.Lexer.Token_Colon               =>
            Logger.Info("Stmt => NAME : Stmt");
            A_Name_Stmt_Node            := new Types.AST.Name_Stmt_Node;
            A_Name_Stmt_Node.Tag        := Types.AST.Node_Name_Stmt;
            Prepend_Token(Token2);
            Prepend_Token(Token1);
            Get_Token;
            A_Name_Stmt_Node.Name       := Lexer.Current_Token.Lexeme_Cursor;
            Match(Types.Lexer.Token_Name);
            Match(Types.Lexer.Token_Colon);
            A_Name_Stmt_Node.Statement  := Stmt;
            A_Stmt_Node                 := Types.AST.Stmt_Node_Ptr( A_Name_Stmt_Node );

          when  Types.Lexer.Token_Assignment          |
                Types.Lexer.Token_Increment           |
                Types.Lexer.Token_Decrement           |
                Types.Lexer.Token_Not_Or_Send_1       |
                Types.Lexer.Token_Send_2              =>
            Prepend_Token(Token2);
            Prepend_Token(Token1);
            Get_Token;
            Logger.Info("Stmt => Varref Stmt_Prime3");
            Accumulate_Tokens := True;
            A_Varref_Node     := Varref;
            A_Stmt_Node       := Stmt_Prime3( A_Varref_Node );
            Accumulate_Tokens := False;
            A_Stmt_Node.Text  := Statement_Text;

          -- Workaround to distinguish between receive statements and poll expressions
          when  Types.Lexer.Token_Receive_1           |
                Types.Lexer.Token_Receive_2           =>
            Get_Token;
            Token3            := Look_Ahead;
            Prepend_Token(Token3);
            Prepend_Token(Token2);
            Prepend_Token(Token1);
            Get_Token;
            Accumulate_Tokens := True;
            case Token3.Kind is
              when  Types.Lexer.Token_Left_Square_Bracket =>
                Logger.Info("Stmt => Any_Expr");
                An_Any_Expr_Stmt_Node             := new Types.AST.Any_Expr_Stmt_Node;
                An_Any_Expr_Stmt_Node.Tag         := Types.AST.Node_Any_Expr_Stmt;
                An_Any_Expr_Stmt_Node.Expression  := Any_Expr;
                A_Stmt_Node                       := Types.AST.Stmt_Node_Ptr( An_Any_Expr_Stmt_Node );
              when  others                                =>
                Logger.Info("Stmt => Varref Stmt_Prime3");
                A_Varref_Node     := Varref;
                A_Stmt_Node       := Stmt_Prime3( A_Varref_Node );
            end case;
            Accumulate_Tokens := False;
            A_Stmt_Node.Text  := Statement_Text;

          -- fAST forward and find distinguishing tokens, then rewind and restart
          when  Types.Lexer.Token_Dot                 |
                Types.Lexer.Token_Left_Square_Bracket =>
            -- fAST forward
            while Look_Ahead.Kind /= Types.Lexer.Token_EOF        and then
                  Look_Ahead.Kind /= Types.Lexer.Token_Implies    and then
                  Look_Ahead.Kind /= Types.Lexer.Token_Semicolon  loop
              Get_Token;
              case Look_Ahead.Kind is
                when  Types.Lexer.Token_Assignment    |
                      Types.Lexer.Token_Increment     |
                      Types.Lexer.Token_Decrement     |
                      Types.Lexer.Token_Not_Or_Send_1 |
                      Types.Lexer.Token_Send_2        =>
                  Goto_Stmt_Prime3 := True;
                -- Workaround to distinguish between receive statements and poll expressions
                -- NOTE: Some real voodoo going on here, nobody except me is likely to understand it
                -- and is even less likely to hate it as much as I do.
                when  Types.Lexer.Token_Receive_1     |
                      Types.Lexer.Token_Receive_2     =>
                  Temp_Token_List.Append(Look_Ahead);
                  Get_Token;
                  Token3  := Look_Ahead;
                  case Token3.Kind is
                    when  Types.Lexer.Token_Left_Square_Bracket =>
                      Goto_Stmt_Prime3  := False;
                    when  others                                =>
                      Goto_Stmt_Prime3  := True;
                  end case;
                  Temp_Token_List.Append(Token3);
                  exit;
                when  others                    =>
                  Goto_Stmt_Prime3 := False;
              end case;
              Temp_Token_List.Append(Look_Ahead);
              exit when Goto_Stmt_Prime3 = True;
            end loop;
            -- rewind
            while Temp_Token_List.Is_Empty = False loop
              Prepend_Token( Temp_Token_List.Last_Element );
              Temp_Token_List.Delete_Last;
            end loop;
            Prepend_Token(Token2);
            Prepend_Token(Token1);
            Get_Token;
            -- restart and take the right path
            if Goto_Stmt_Prime3 = True then
              Logger.Info("Stmt => Varref Stmt_Prime3");
              Accumulate_Tokens := True;
              A_Varref_Node     := Varref;
              A_Stmt_Node       := Stmt_Prime3( A_Varref_Node );
              Accumulate_Tokens := False;
              A_Stmt_Node.Text  := Statement_Text;
            else
              Logger.Info("Stmt => Any_Expr");
              An_Any_Expr_Stmt_Node             := new Types.AST.Any_Expr_Stmt_Node;
              An_Any_Expr_Stmt_Node.Tag         := Types.AST.Node_Any_Expr_Stmt;
              Accumulate_Tokens                 := True;
              An_Any_Expr_Stmt_Node.Expression  := Any_Expr;
              Accumulate_Tokens                 := False;
              A_Stmt_Node                       := Types.AST.Stmt_Node_Ptr( An_Any_Expr_Stmt_Node );
              A_Stmt_Node.Text                  := Statement_Text;
            end if;

          -- NOTE: leaving syntax checking to Any_Expr
          when others                           =>
            Logger.Info("Stmt => Any_Expr");
            Accumulate_Tokens := True;
            Prepend_Token(Token2);
            Prepend_Token(Token1);
            Get_Token;
            An_Any_Expr_Stmt_Node             := new Types.AST.Any_Expr_Stmt_Node;
            An_Any_Expr_Stmt_Node.Tag         := Types.AST.Node_Any_Expr_Stmt;
            An_Any_Expr_Stmt_Node.Expression  := Any_Expr;
            A_Stmt_Node                       := Types.AST.Stmt_Node_Ptr( An_Any_Expr_Stmt_Node );
            Accumulate_Tokens                 := False;
            A_Stmt_Node.Text                  := Statement_Text;

        end case;

      when  Types.Lexer.Token_Print       |
            Types.Lexer.Token_Printf      =>
        Logger.Info("Stmt => Stmt_Prime2 ( STRING Stmt_Prime1 )");
        Accumulate_Tokens := True;
        A_Printf_Stmt_Node := new Types.AST.Printf_Stmt_Node;
        A_Printf_Stmt_Node.Tag := Types.AST.Node_Printf_Stmt;
        Stmt_Prime2; Match(Types.Lexer.Token_Left_Parentheses);
        A_Printf_Stmt_Node.String := Lexer.Current_Token.Lexeme_Cursor;
        Match(Types.Lexer.Token_String); A_Printf_Stmt_Node.Arguments := Stmt_Prime1; Match(Types.Lexer.Token_Right_Parentheses);
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_Printf_Stmt_Node );
        Accumulate_Tokens := False;
        A_Stmt_Node.Text := Statement_Text;
      when  Types.Lexer.Token_Printm      =>
        -- COMMENTARY: I have decided to one-up the official grammar spec here.
        -- There is no such nonterminal as an 'expression'!
        -- Besides, if a 'name' is the input, there is no reason why 'expr/any_expr/varref' should be used.
        Logger.Info("Stmt => PRINTM ( NAME )");
        Accumulate_Tokens := True;
        A_Printm_Stmt_Node := new Types.AST.Printm_Stmt_Node;
        A_Printm_Stmt_Node.Tag := Types.AST.Node_Printm_Stmt;
        Match(Types.Lexer.Token_Printm); Match(Types.Lexer.Token_Left_Parentheses);
        A_Printm_Stmt_Node.Name := Lexer.Current_Token.Lexeme_Cursor;
        Match(Types.Lexer.Token_Name); Match(Types.Lexer.Token_Right_Parentheses);
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_Printm_Stmt_Node );
        Accumulate_Tokens := False;
        A_Stmt_Node.Text := Statement_Text;
      when  others                        =>
        Syntax_Error_Output( "Stmt" );
    end case;
    -- CodeGen
    A_Stmt_Node.Line_Number := Line_Number;
    Logger.Info( "Statement_Text = " & Ada.Strings.Unbounded.To_String( Statement_Text ) );
    return A_Stmt_Node;
  end Stmt;
  
  function Stmt_Prime1 return Types.AST.Arg_Lst_Node_Ptr is
    An_Arg_Lst_Node : Types.AST.Arg_Lst_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Comma       =>
        Logger.Info("Stmt_Prime1 => , Arg_Lst");
        Match(Types.Lexer.Token_Comma); An_Arg_Lst_Node := Arg_Lst;
      when  Types.Lexer.Token_Right_Parentheses
                                          =>
        Logger.Info("Stmt_Prime1 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Stmt_Prime1" );
    end case;
    return An_Arg_Lst_Node;
  end Stmt_Prime1;
  
  procedure Stmt_Prime2 is
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Print       =>
        Logger.Info("Stmt_Prime2 => PRINT");
        Match(Types.Lexer.Token_Print);
      when  Types.Lexer.Token_Printf      =>
        Logger.Info("Stmt_Prime2 => PRINTF");
        Match(Types.Lexer.Token_Printf);
      when  others                        =>
        Syntax_Error_Output( "Stmt_Prime2" );
    end case;
  end Stmt_Prime2;

  function Stmt_Prime3( A_Varref_Node : Types.AST.Varref_Node_Ptr ) return Types.AST.Stmt_Node_Ptr is
    A_Stmt_Node         : Types.AST.Stmt_Node_Ptr;
    An_Assign_Stmt_Node : Types.AST.Assign_Stmt_Node_Ptr;
    A_Send_Stmt_Node    : Types.AST.Send_Stmt_Node_Ptr;
    A_Recv_Stmt_Node    : Types.AST.Recv_Stmt_Node_Ptr;
  begin
    Ada.Assertions.Assert( A_Varref_Node /= null, "A_Varref_Node is null!" );
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Assignment  |
            Types.Lexer.Token_Increment   |
            Types.Lexer.Token_Decrement   =>
        Logger.Info("Stmt_Prime3 => Assign_Prime");
        An_Assign_Stmt_Node := new Types.AST.Assign_Stmt_Node;
        An_Assign_Stmt_Node.Tag := Types.AST.Node_Assign_Stmt;
        An_Assign_Stmt_Node.Assignment := Assign_Prime( A_Varref_Node );
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( An_Assign_Stmt_Node );
      when  Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_Send_2      =>
        Logger.Info("Stmt_Prime3 => Send_Prime");
        A_Send_Stmt_Node := new Types.AST.Send_Stmt_Node;
        A_Send_Stmt_Node.Tag := Types.AST.Node_Send_Stmt;
        A_Send_Stmt_Node.Send := Send_Prime( A_Varref_Node );
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_Send_Stmt_Node );
      when  Types.Lexer.Token_Receive_1   |
            Types.Lexer.Token_Receive_2   =>
        Logger.Info("Stmt_Prime3 => Receive_Prime");
        A_Recv_Stmt_Node := new Types.AST.Recv_Stmt_Node;
        A_Recv_Stmt_Node.Tag := Types.AST.Node_Recv_Stmt;
        A_Recv_Stmt_Node.Receive := Receive_Prime( A_Varref_Node );
        A_Stmt_Node := Types.AST.Stmt_Node_Ptr( A_Recv_Stmt_Node );
      when  others                        =>
        Syntax_Error_Output( "Stmt_Prime3" );
    end case;
    return A_Stmt_Node;
  end Stmt_Prime3;
  
  function Options return Types.AST.Options_Node_Ptr is
    An_Options_Node : Types.AST.Options_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Guard       =>
        Logger.Info("Options => :: Sequence Options_Prime");
        An_Options_Node := new Types.AST.Options_Node;
        An_Options_Node.Tag := Types.AST.Node_Options;
        Match(Types.Lexer.Token_Guard); An_Options_Node.Sequence_List.Append( Sequence );
        Options_Prime( An_Options_Node );
      when  others                        =>
        Syntax_Error_Output( "Options" );
    end case;
    return An_Options_Node;
  end Options;

  procedure Options_Prime( An_Options_Node : Types.AST.Options_Node_Ptr ) is
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Guard       =>
        Logger.Info("Options_Prime => :: Sequence Options_Prime");
        Match(Types.Lexer.Token_Guard); An_Options_Node.Sequence_List.Append( Sequence );
        Options_Prime( An_Options_Node );
      when  Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Od          =>
        Logger.Info("Options_Prime => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Options_Prime" );
    end case;
  end Options_Prime;

  function Chanpoll return Types.AST.Channel_Poll_Any_Expr_Node_Ptr is
    A_Channel_Poll_Any_Expr_Node : Types.AST.Channel_Poll_Any_Expr_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Empty       =>
        Logger.Info("Chanpoll => EMPTY");
        Match(Types.Lexer.Token_Empty);
        A_Channel_Poll_Any_Expr_Node      := new Types.AST.Empty_Channel_Poll_Any_Expr_Node;
        A_Channel_Poll_Any_Expr_Node.Tag  := Types.AST.Node_Empty_Channel_Poll_Any_Expr;
      when  Types.Lexer.Token_Full        =>
        Logger.Info("Chanpoll => FULL");
        Match(Types.Lexer.Token_Full);
        A_Channel_Poll_Any_Expr_Node      := new Types.AST.Full_Channel_Poll_Any_Expr_Node;
        A_Channel_Poll_Any_Expr_Node.Tag  := Types.AST.Node_Full_Channel_Poll_Any_Expr;
      when  Types.Lexer.Token_Nempty      =>
        Logger.Info("Chanpoll => NEMPTY");
        Match(Types.Lexer.Token_Nempty);
        A_Channel_Poll_Any_Expr_Node      := new Types.AST.Nempty_Channel_Poll_Any_Expr_Node;
        A_Channel_Poll_Any_Expr_Node.Tag  := Types.AST.Node_Nempty_Channel_Poll_Any_Expr;
      when  Types.Lexer.Token_Nfull       =>
        Logger.Info("Chanpoll => NFULL");
        Match(Types.Lexer.Token_Nfull);
        A_Channel_Poll_Any_Expr_Node      := new Types.AST.Nfull_Channel_Poll_Any_Expr_Node;
        A_Channel_Poll_Any_Expr_Node.Tag  := Types.AST.Node_Nfull_Channel_Poll_Any_Expr;
      when  others                        =>
        Syntax_Error_Output( "Chanpoll" );
    end case;
    return A_Channel_Poll_Any_Expr_Node;
  end Chanpoll;

  function Any_Expr return Types.AST.Any_Expr_Node_Ptr is
    An_Any_Expr_Node                  : Types.AST.Any_Expr_Node_Ptr;
    A_Varref_Node                     : Types.AST.Varref_Node_Ptr;
    A_Higher_Precedence_Implies_Any_Expr_Node
                                      : Types.AST.Higher_Precedence_Implies_Any_Expr_Node_Ptr;
    An_Unary_Any_Expr_Node            : Types.AST.Unary_Any_Expr_Node_Ptr;
    A_Len_Any_Expr_Node               : Types.AST.Len_Any_Expr_Node_Ptr;
    A_Const_Any_Expr_Node             : Types.AST.Const_Any_Expr_Node_Ptr;
    A_Timeout_Any_Expr_Node           : Types.AST.Timeout_Any_Expr_Node_Ptr;
    A_Non_Progress_Any_Expr_Node      : Types.AST.Non_Progress_Any_Expr_Node_Ptr;
    An_Enabled_Any_Expr_Node          : Types.AST.Enabled_Any_Expr_Node_Ptr;
    A_Pc_Value_Any_Expr_Node          : Types.AST.Pc_Value_Any_Expr_Node_Ptr;
    A_Run_Any_Expr_Node               : Types.AST.Run_Any_Expr_Node_Ptr;
    A_Channel_Poll_Any_Expr_Node      : Types.AST.Channel_Poll_Any_Expr_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Enabled     =>
        Logger.Info("Any_Expr => ENABLED ( Any_Expr ) Any_Expr_Prime3");
        An_Enabled_Any_Expr_Node := new Types.AST.Enabled_Any_Expr_Node;
        An_Enabled_Any_Expr_Node.Tag := Types.AST.Node_Enabled_Any_Expr;
        Match(Types.Lexer.Token_Enabled); Match(Types.Lexer.Token_Left_Parentheses); An_Enabled_Any_Expr_Node.Generic_Any_Expr := Any_Expr; Match(Types.Lexer.Token_Right_Parentheses);
        An_Enabled_Any_Expr_Node.BinaryOp := Any_Expr_Prime3;
        An_Any_Expr_Node := Types.AST.Any_Expr_Node_Ptr( An_Enabled_Any_Expr_Node );
      when  Types.Lexer.Token_False       |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_True        =>
        Logger.Info("Any_Expr => Const Any_Expr_Prime3");
        A_Const_Any_Expr_Node := new Types.AST.Const_Any_Expr_Node;
        A_Const_Any_Expr_Node.Tag := Types.AST.Node_Const_Any_Expr;
        A_Const_Any_Expr_Node.A_Constant := Const; A_Const_Any_Expr_Node.BinaryOp := Any_Expr_Prime3;
        An_Any_Expr_Node := Types.AST.Any_Expr_Node_Ptr( A_Const_Any_Expr_Node );
      when  Types.Lexer.Token_Len         =>
        Logger.Info("Any_Expr => LEN ( Varref ) Any_Expr_Prime3");
        A_Len_Any_Expr_Node := new Types.AST.Len_Any_Expr_Node;
        A_Len_Any_Expr_Node.Tag := Types.AST.Node_Len_Any_Expr;
        Match(Types.Lexer.Token_Len); Match(Types.Lexer.Token_Left_Parentheses); A_Len_Any_Expr_Node.Channel_Name := Varref; Match(Types.Lexer.Token_Right_Parentheses);
        A_Len_Any_Expr_Node.BinaryOp := Any_Expr_Prime3;
        An_Any_Expr_Node := Types.AST.Any_Expr_Node_Ptr( A_Len_Any_Expr_Node );
      when  Types.Lexer.Token_Left_Parentheses
                                          =>
        Logger.Info("Any_Expr => ( Any_Expr Any_Expr_Prime4 ) Any_Expr_Prime3");
        A_Higher_Precedence_Implies_Any_Expr_Node := new Types.AST.Higher_Precedence_Implies_Any_Expr_Node;
        A_Higher_Precedence_Implies_Any_Expr_Node.Tag := Types.AST.Node_Higher_Precedence_Implies_Any_Expr;
        Match(Types.Lexer.Token_Left_Parentheses); A_Higher_Precedence_Implies_Any_Expr_Node.Generic_Any_Expr := Any_Expr;
        A_Higher_Precedence_Implies_Any_Expr_Node.Implies := Any_Expr_Prime4; Match(Types.Lexer.Token_Right_Parentheses);
        A_Higher_Precedence_Implies_Any_Expr_Node.BinaryOp := Any_Expr_Prime3;
        An_Any_Expr_Node := Types.AST.Any_Expr_Node_Ptr( A_Higher_Precedence_Implies_Any_Expr_Node );
      -- NON-CONFLICT: slightly screwing with grammar, correction later during semantic analysis
      when  Types.Lexer.Token_Name        =>
        Logger.Info("Any_Expr => Varref Any_Expr_Prime5 Any_Expr_Prime3");
        A_Varref_Node := Varref; An_Any_Expr_Node := Any_Expr_Prime5( A_Varref_Node );
        An_Any_Expr_Node.BinaryOp := Any_Expr_Prime3;
      when  Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_Ones_Complement
                                          |
            Types.Lexer.Token_Subtract    =>
        Logger.Info("Any_Expr => Unarop Any_Expr Any_Expr_Prime3");
        An_Unary_Any_Expr_Node := new Types.AST.Unary_Any_Expr_Node;
        An_Unary_Any_Expr_Node.Tag := Types.AST.Node_Unary_Any_Expr;
        An_Unary_Any_Expr_Node.Operator := Unarop; An_Unary_Any_Expr_Node.Generic_Any_Expr := Any_Expr;
        An_Unary_Any_Expr_Node.BinaryOp := Any_Expr_Prime3;
        An_Any_Expr_Node := Types.AST.Any_Expr_Node_Ptr( An_Unary_Any_Expr_Node );
      when  Types.Lexer.Token_Np_Underscore
                                          =>
        Logger.Info("Any_Expr => NP_ Any_Expr_Prime3");
        A_Non_Progress_Any_Expr_Node := new Types.AST.Non_Progress_Any_Expr_Node;
        A_Non_Progress_Any_Expr_Node.Tag := Types.AST.Node_Non_Progress_Any_Expr;
        Match(Types.Lexer.Token_Np_Underscore); A_Non_Progress_Any_Expr_Node.BinaryOp := Any_Expr_Prime3;
        An_Any_Expr_Node := Types.AST.Any_Expr_Node_Ptr( A_Non_Progress_Any_Expr_Node );
      when  Types.Lexer.Token_Pc_Value    =>
        Logger.Info("Any_Expr => PC_VALUE ( Any_Expr ) Any_Expr_Prime3");
        A_Pc_Value_Any_Expr_Node := new Types.AST.Pc_Value_Any_Expr_Node;
        A_Pc_Value_Any_Expr_Node.Tag := Types.AST.Node_Pc_Value_Any_Expr;
        Match(Types.Lexer.Token_Pc_Value); Match(Types.Lexer.Token_Left_Parentheses); A_Pc_Value_Any_Expr_Node.Generic_Any_Expr := Any_Expr; Match(Types.Lexer.Token_Right_Parentheses);
        A_Pc_Value_Any_Expr_Node.BinaryOp := Any_Expr_Prime3;
        An_Any_Expr_Node := Types.AST.Any_Expr_Node_Ptr( A_Pc_Value_Any_Expr_Node );
      when  Types.Lexer.Token_Run         =>
        Logger.Info("Any_Expr => RUN NAME ( Any_Expr_Prime1 ) Any_Expr_Prime2 Any_Expr_Prime3"); -- TODO: CHECK CONSECUTIVE EPSILON FIRSTS
        A_Run_Any_Expr_Node := new Types.AST.Run_Any_Expr_Node;
        A_Run_Any_Expr_Node.Tag := Types.AST.Node_Run_Any_Expr;
        Match(Types.Lexer.Token_Run); A_Run_Any_Expr_Node.Name := Lexer.Current_Token.Lexeme_Cursor; Match(Types.Lexer.Token_Name); Match(Types.Lexer.Token_Left_Parentheses);
        A_Run_Any_Expr_Node.Arguments := Any_Expr_Prime1; Match(Types.Lexer.Token_Right_Parentheses);
        A_Run_Any_Expr_Node.Priority := Any_Expr_Prime2; A_Run_Any_Expr_Node.BinaryOp := Any_Expr_Prime3;
        An_Any_Expr_Node := Types.AST.Any_Expr_Node_Ptr( A_Run_Any_Expr_Node );
      when  Types.Lexer.Token_Timeout     =>
        Logger.Info("Any_Expr => TIMEOUT Any_Expr_Prime3");
        A_Timeout_Any_Expr_Node := new Types.AST.Timeout_Any_Expr_Node;
        A_Timeout_Any_Expr_Node.Tag := Types.AST.Node_Timeout_Any_Expr;
        Match(Types.Lexer.Token_Timeout); A_Timeout_Any_Expr_Node.BinaryOp := Any_Expr_Prime3;
        An_Any_Expr_Node := Types.AST.Any_Expr_Node_Ptr( A_Timeout_Any_Expr_Node );
      -- TODO: NON-CONFLICT: slightly screwing with grammar, correction later during semantic analysis
      when  Types.Lexer.Token_Empty       |
            Types.Lexer.Token_Full        |
            Types.Lexer.Token_Nempty      |
            Types.Lexer.Token_Nfull       =>
        Logger.Info("Any_Expr => Chanpoll ( Varref ) Any_Expr_Prime3");
        A_Channel_Poll_Any_Expr_Node              := Chanpoll;
        Match(Types.Lexer.Token_Left_Parentheses);
        A_Channel_Poll_Any_Expr_Node.Channel_Name := Varref;
        Match(Types.Lexer.Token_Right_Parentheses);
        A_Channel_Poll_Any_Expr_Node.BinaryOp     := Any_Expr_Prime3;
        An_Any_Expr_Node                          := Types.AST.Any_Expr_Node_Ptr( A_Channel_Poll_Any_Expr_Node );
      when  others                        =>
        Syntax_Error_Output( "Any_Expr" );
    end case;
    return An_Any_Expr_Node;
  end Any_Expr;

  -- TODO:  just check to be sure later
  function Any_Expr_Prime1 return Types.AST.Arg_Lst_Node_Ptr is
    An_Arg_Lst_Node : Types.AST.Arg_Lst_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Enabled     |
            Types.Lexer.Token_False       |
            Types.Lexer.Token_Len         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Not_Or_Send_1
                                          |
            Types.Lexer.Token_Np_Underscore
                                          |
            Types.Lexer.Token_Number      |
            Types.Lexer.Token_Ones_Complement
                                          |
            Types.Lexer.Token_Pc_Value    |
            Types.Lexer.Token_Run         |
            Types.Lexer.Token_Skip        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Timeout     |
            Types.Lexer.Token_True        =>
        Logger.Info("Any_Expr_Prime1 => Arg_Lst");
        An_Arg_Lst_Node := Arg_Lst;
      when  Types.Lexer.Token_Right_Parentheses
                                          =>
        Logger.Info("Any_Expr_Prime1 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Any_Expr_Prime1" );
    end case;
    return An_Arg_Lst_Node;
  end Any_Expr_Prime1;

  -- TODO: check accuracy later
  function Any_Expr_Prime2 return Types.AST.Priority_Node_Ptr is
    A_Priority_Node : Types.AST.Priority_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Priority    =>
        Logger.Info("Any_Expr_Prime2 => Priority");
        A_Priority_Node := Priority;
      when  Types.Lexer.Token_Active      |
            Types.Lexer.Token_Add         |
            Types.Lexer.Token_And_Bitwise |
            Types.Lexer.Token_And_Logical |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_Colon       |
            Types.Lexer.Token_Comma       |
            Types.Lexer.Token_Divide      |
            Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_EOF         |
            Types.Lexer.Token_Equals      |
            Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Greater_Than|
            Types.Lexer.Token_Greater_Than_Or_Equal_To
                                          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Implies     |
            Types.Lexer.Token_Init        |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Left_Shift  |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Less_Than   |
            Types.Lexer.Token_Less_Than_Or_Equal_To
                                          |
            Types.Lexer.Token_Modulus     |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Multiply    |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Not_Equals  |
            Types.Lexer.Token_Never       |
            Types.Lexer.Token_Notrace     |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Or_Bitwise  |
            Types.Lexer.Token_Or_Logical  |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Proctype    |
            Types.Lexer.Token_Right_Curly_Bracket
                                          |
            Types.Lexer.Token_Right_Parentheses
                                          |
            Types.Lexer.Token_Right_Shift |
            Types.Lexer.Token_Right_Square_Bracket
                                          |
            Types.Lexer.Token_Semicolon   |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Trace       |
            Types.Lexer.Token_Typedef     |
            Types.Lexer.Token_Unless      |
            Types.Lexer.Token_Unsigned    |
            Types.Lexer.Token_Xor_Bitwise =>
        Logger.Info("Any_Expr_Prime2 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Any_Expr_Prime2" );
    end case;
    return A_Priority_Node;
  end Any_Expr_Prime2;

  function Any_Expr_Prime3 return Types.AST.Binar_Op_Node_Ptr is
    A_Binar_Op_Node : Types.AST.Binar_Op_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      -- FOLLOW(any_expr_prime3) |= FOLLOW(any_expr) |= FIRST(any_expr_prime3).
      -- Hence, there is no real conflict with FIRST(expr_prime) and FOLLOW(expr_prime).
      when  Types.Lexer.Token_Add         |
            Types.Lexer.Token_And_Bitwise |
            Types.Lexer.Token_And_Logical |
            Types.Lexer.Token_Divide      |
            Types.Lexer.Token_Equals      |
            Types.Lexer.Token_Greater_Than|
            Types.Lexer.Token_Greater_Than_Or_Equal_To
                                          |
            Types.Lexer.Token_Left_Shift  |
            Types.Lexer.Token_Less_Than   |
            Types.Lexer.Token_Less_Than_Or_Equal_To
                                          |
            Types.Lexer.Token_Modulus     |
            Types.Lexer.Token_Multiply    |
            Types.Lexer.Token_Not_Equals  |
            Types.Lexer.Token_Or_Bitwise  |
            Types.Lexer.Token_Or_Logical  |
            Types.Lexer.Token_Right_Shift |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Xor_Bitwise =>
        Logger.Info("Any_Expr_Prime3 => Binarop Any_Expr");
        A_Binar_Op_Node := new Types.AST.Binar_Op_Node;
        A_Binar_Op_Node.Tag := Types.AST.Node_Binar_Op;
        A_Binar_Op_Node.Operator := Binarop; A_Binar_Op_Node.Generic_Any_Expr := Any_Expr;
      when  Types.Lexer.Token_Active      |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_Colon       |
            Types.Lexer.Token_Comma       |
            Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_EOF         |
            Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Implies     |
            Types.Lexer.Token_Init        |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Never       |
            Types.Lexer.Token_Notrace     |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Proctype    |
            Types.Lexer.Token_Right_Curly_Bracket
                                          |
            Types.Lexer.Token_Right_Parentheses
                                          |
            Types.Lexer.Token_Right_Square_Bracket
                                          |
            Types.Lexer.Token_Semicolon   |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Trace       |
            Types.Lexer.Token_Typedef     |
            Types.Lexer.Token_Unless      |
            Types.Lexer.Token_Unsigned    =>
        Logger.Info("Any_Expr_Prime3 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Any_Expr_Prime3" );
    end case;
    return A_Binar_Op_Node;
  end Any_Expr_Prime3;

  function Any_Expr_Prime4 return Types.AST.Implies_Node_Ptr is
    An_Implies_Node : Types.AST.Implies_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Implies     =>
        Logger.Info("Any_Expr_Prime4 => -> Any_Expr : Any_Expr");
        An_Implies_Node := new Types.AST.Implies_Node;
        An_Implies_Node.Tag := Types.AST.Node_Implies;
        Match(Types.Lexer.Token_Implies); An_Implies_Node.IfTrue := Any_Expr; Match(Types.Lexer.Token_Colon); An_Implies_Node.IfFalse := Any_Expr;
      when  Types.Lexer.Token_Right_Parentheses
                                          =>
        Logger.Info("Any_Expr_Prime4 => epsilon");
      when  others                        =>
        Syntax_Error_Output( "Any_Expr_Prime4" );
    end case;
    return An_Implies_Node;
  end Any_Expr_Prime4;

  -- TODO: the Promela grammar spec is...incomplete, to say the least.
  -- There are 2 versions of remote references, one of which uses a colon token,
  -- which introduces an LL(1) conflict with the epsilon case. FIXME later.
  function Any_Expr_Prime5( A_Varref_Node : Types.AST.Varref_Node_Ptr ) return Types.AST.Any_Expr_Node_Ptr is
    An_Any_Expr_Node          : Types.AST.Any_Expr_Node_Ptr;
    A_Varref_Any_Expr_Node    : Types.AST.Varref_Any_Expr_Node_Ptr;
    A_Poll_Any_Expr_Node      : Types.AST.Poll_Any_Expr_Node_Ptr;
    A_Remoteref_Any_Expr_Node : Types.AST.Remoteref_Any_Expr_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Alias       =>
        Logger.Info("Any_Expr_Prime5 => @ NAME");
        A_Remoteref_Any_Expr_Node                 := new Types.AST.Remoteref_Any_Expr_Node;
        A_Remoteref_Any_Expr_Node.Tag             := Types.AST.Node_Remoteref_Any_Expr;
        A_Remoteref_Any_Expr_Node.Version         := Types.Lexer.Token_Alias;
        Match(Types.Lexer.Token_Alias);
        A_Remoteref_Any_Expr_Node.Process         := A_Varref_Node;
        A_Remoteref_Any_Expr_Node.Process_Target  := Lexer.Current_Token.Lexeme_Cursor;
        Match(Types.Lexer.Token_Name);
        An_Any_Expr_Node                          := Types.AST.Any_Expr_Node_Ptr( A_Remoteref_Any_Expr_Node );
      when  Types.Lexer.Token_Receive_1   |
            Types.Lexer.Token_Receive_2   =>
        Logger.Info("Any_Expr_Prime5 => Poll_Prime");
        A_Poll_Any_Expr_Node      := new Types.AST.Poll_Any_Expr_Node;
        A_Poll_Any_Expr_Node.Tag  := Types.AST.Node_Poll_Any_Expr;
        A_Poll_Any_Expr_Node.Poll := Poll_Prime( A_Varref_Node );
        An_Any_Expr_Node          := Types.AST.Any_Expr_Node_Ptr( A_Poll_Any_Expr_Node );
      when  Types.Lexer.Token_Add         |
            Types.Lexer.Token_And_Bitwise |
            Types.Lexer.Token_And_Logical |
            Types.Lexer.Token_Divide      |
            Types.Lexer.Token_Equals      |
            Types.Lexer.Token_Greater_Than|
            Types.Lexer.Token_Greater_Than_Or_Equal_To
                                          |
            Types.Lexer.Token_Left_Shift  |
            Types.Lexer.Token_Less_Than   |
            Types.Lexer.Token_Less_Than_Or_Equal_To
                                          |
            Types.Lexer.Token_Modulus     |
            Types.Lexer.Token_Multiply    |
            Types.Lexer.Token_Not_Equals  |
            Types.Lexer.Token_Or_Bitwise  |
            Types.Lexer.Token_Or_Logical  |
            Types.Lexer.Token_Right_Shift |
            Types.Lexer.Token_Subtract    |
            Types.Lexer.Token_Xor_Bitwise |
            Types.Lexer.Token_Active      |
            Types.Lexer.Token_Bit         |
            Types.Lexer.Token_Bool        |
            Types.Lexer.Token_Byte        |
            Types.Lexer.Token_Chan        |
            Types.Lexer.Token_Colon       |
            Types.Lexer.Token_Comma       |
            Types.Lexer.Token_D_Proctype  |
            Types.Lexer.Token_EOF         |
            Types.Lexer.Token_Fi          |
            Types.Lexer.Token_Guard       |
            Types.Lexer.Token_Hidden      |
            Types.Lexer.Token_Implies     |
            Types.Lexer.Token_Init        |
            Types.Lexer.Token_Int         |
            Types.Lexer.Token_Left_Parentheses
                                          |
            Types.Lexer.Token_Local       |
            Types.Lexer.Token_Mtype       |
            Types.Lexer.Token_Name        |
            Types.Lexer.Token_Never       |
            Types.Lexer.Token_Notrace     |
            Types.Lexer.Token_Od          |
            Types.Lexer.Token_Pid         |
            Types.Lexer.Token_Proctype    |
            Types.Lexer.Token_Right_Curly_Bracket
                                          |
            Types.Lexer.Token_Right_Parentheses
                                          |
            Types.Lexer.Token_Right_Square_Bracket
                                          |
            Types.Lexer.Token_Semicolon   |
            Types.Lexer.Token_Short       |
            Types.Lexer.Token_Show        |
            Types.Lexer.Token_Trace       |
            Types.Lexer.Token_Typedef     |
            Types.Lexer.Token_Unless      |
            Types.Lexer.Token_Unsigned    =>
        Logger.Info("Any_Expr_Prime5 => epsilon");
        A_Varref_Any_Expr_Node := new Types.AST.Varref_Any_Expr_Node;
        A_Varref_Any_Expr_Node.Tag := Types.AST.Node_Varref_Any_Expr;
        A_Varref_Any_Expr_Node.Variable := A_Varref_Node;
        An_Any_Expr_Node := Types.AST.Any_Expr_Node_Ptr( A_Varref_Any_Expr_Node );
      when  others                        =>
        Syntax_Error_Output( "Any_Expr_Prime5" );
    end case;
    return An_Any_Expr_Node;
  end Any_Expr_Prime5;

  function Binarop return Types.Lexer.Token_Type is
    Attribute : Types.Lexer.Token_Type := Types.Lexer.Token_Null;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Add         =>
        Logger.Info("Binarop => +");
        Match(Types.Lexer.Token_Add);
        Attribute := Types.Lexer.Token_Add;
      when  Types.Lexer.Token_And_Bitwise =>
        Logger.Info("Binarop => &");
        Match(Types.Lexer.Token_And_Bitwise);
        Attribute := Types.Lexer.Token_And_Bitwise;
      when  Types.Lexer.Token_And_Logical =>
        Logger.Info("Binarop => Andor");
        Attribute := Andor;
      when  Types.Lexer.Token_Divide      =>
        Logger.Info("Binarop => /");
        Match(Types.Lexer.Token_Divide);
        Attribute := Types.Lexer.Token_Divide;
      when  Types.Lexer.Token_Equals      =>
        Logger.Info("Binarop => ==");
        Match(Types.Lexer.Token_Equals);
        Attribute := Types.Lexer.Token_Equals;
      when  Types.Lexer.Token_Greater_Than=>
        Logger.Info("Binarop => >");
        Match(Types.Lexer.Token_Greater_Than);
        Attribute := Types.Lexer.Token_Greater_Than;
      when  Types.Lexer.Token_Greater_Than_Or_Equal_To 
                                          =>
        Logger.Info("Binarop => >=");
        Match(Types.Lexer.Token_Greater_Than_Or_Equal_To);
        Attribute := Types.Lexer.Token_Greater_Than_Or_Equal_To;
      when  Types.Lexer.Token_Left_Shift  =>
        Logger.Info("Binarop => <<");
        Match(Types.Lexer.Token_Left_Shift);
        Attribute := Types.Lexer.Token_Left_Shift;
      when  Types.Lexer.Token_Less_Than   =>
        Logger.Info("Binarop => <");
        Match(Types.Lexer.Token_Less_Than);
        Attribute := Types.Lexer.Token_Less_Than;
      when  Types.Lexer.Token_Less_Than_Or_Equal_To
                                          =>
        Logger.Info("Binarop => <=");
        Match(Types.Lexer.Token_Less_Than_Or_Equal_To);
        Attribute := Types.Lexer.Token_Less_Than_Or_Equal_To;
      when  Types.Lexer.Token_Modulus     =>
        Logger.Info("Binarop => %");
        Match(Types.Lexer.Token_Modulus);
        Attribute := Types.Lexer.Token_Modulus;
      when  Types.Lexer.Token_Multiply    =>
        Logger.Info("Binarop => *");
        Match(Types.Lexer.Token_Multiply);
        Attribute := Types.Lexer.Token_Multiply;
      when  Types.Lexer.Token_Not_Equals  =>
        Logger.Info("Binarop => !=");
        Match(Types.Lexer.Token_Not_Equals);
        Attribute := Types.Lexer.Token_Not_Equals;
      when  Types.Lexer.Token_Or_Bitwise  =>
        Logger.Info("Binarop => |");
        Match(Types.Lexer.Token_Or_Bitwise);
        Attribute := Types.Lexer.Token_Or_Bitwise;
      when  Types.Lexer.Token_Or_Logical  =>
        Logger.Info("Binarop => Andor");
        Attribute := Andor;
      when  Types.Lexer.Token_Right_Shift =>
        Logger.Info("Binarop => >>");
        Match(Types.Lexer.Token_Right_Shift);
        Attribute := Types.Lexer.Token_Right_Shift;
      when  Types.Lexer.Token_Subtract    =>
        Logger.Info("Binarop => -");
        Match(Types.Lexer.Token_Subtract);
        Attribute := Types.Lexer.Token_Subtract;
      when  Types.Lexer.Token_Xor_Bitwise =>
        Logger.Info("Binarop => ^");
        Match(Types.Lexer.Token_Xor_Bitwise);
        Attribute := Types.Lexer.Token_Xor_Bitwise;
      when  others                        =>
        Syntax_Error_Output( "Binarop" );
    end case;
    return Attribute;
  end Binarop;

  function Andor return Types.Lexer.Token_Type is
    Attribute : Types.Lexer.Token_Type := Types.Lexer.Token_Null;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_And_Logical =>
        Logger.Info("Andor => &&");
        Match(Types.Lexer.Token_And_Logical);
        Attribute := Types.Lexer.Token_And_Logical;
      when  Types.Lexer.Token_Or_Logical  =>
        Logger.Info("Andor => ||");
        Match(Types.Lexer.Token_Or_Logical);
        Attribute := Types.Lexer.Token_Or_Logical;
      when  others                        =>
        Syntax_Error_Output( "Andor" );
    end case;
    return Attribute;
  end Andor;

  function Unarop return Types.Lexer.Token_Type is
    Attribute : Types.Lexer.Token_Type := Types.Lexer.Token_Null;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Not_Or_Send_1
                                          =>
        Logger.Info("Unarop => !");
        Match(Types.Lexer.Token_Not_Or_Send_1);
        Attribute := Types.Lexer.Token_Not_Or_Send_1;
      when  Types.Lexer.Token_Ones_Complement
                                          =>
        Logger.Info("Unarop => ~");
        Match(Types.Lexer.Token_Ones_Complement);
        Attribute := Types.Lexer.Token_Ones_Complement;
      when  Types.Lexer.Token_Subtract    =>
        Logger.Info("Unarop => -");
        Match(Types.Lexer.Token_Subtract);
        Attribute := Types.Lexer.Token_Subtract;
      when  others                        =>
        Syntax_Error_Output( "Unarop" );
    end case;
    return Attribute;
  end Unarop;
  
  function Uname return Types.AST.Uname_Node_Ptr is
    An_Uname_Node : Types.AST.Uname_Node_Ptr;
  begin
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_Name        =>
        Logger.Info("Uname => NAME");
        An_Uname_Node := new Types.AST.Uname_Node;
        An_Uname_Node.Tag := Types.AST.Node_Uname;
        An_Uname_Node.Name := Lexer.Current_Token.Lexeme_Cursor;
        Match(Types.Lexer.Token_Name);
      when  others                        =>
        Syntax_Error_Output( "Uname" );
    end case;
    return An_Uname_Node;
  end Uname;
  
  function Const return Types.AST.Const_Node_Ptr is
    A_Const_Node          : Types.AST.Const_Node_Ptr;
  begin
    A_Const_Node          := new Types.AST.Const_Node;
    A_Const_Node.Tag      := Types.AST.Node_Const;
    case Look_Ahead.Kind is
      when  Types.Lexer.Token_False       =>
        Logger.Info("Const => FALSE");
        A_Const_Node.Number  := Types.Lexer.Add_To_Natural_Table( 0 );
        Match(Types.Lexer.Token_False);
      when  Types.Lexer.Token_Number      =>
        Logger.Info("Const => NUMBER");
        A_Const_Node.Number  := Lexer.Current_Token.Value_Cursor;
        Match(Types.Lexer.Token_Number);
      -- SPIN appears to treat 'skip' no differently from 'true',
      -- which makes one wonder why the former is there in the first place.
      when  Types.Lexer.Token_Skip        =>
        Logger.Info("Const => SKIP");
        A_Const_Node.Number  := Types.Lexer.Add_To_Natural_Table( 1 );
        Match(Types.Lexer.Token_Skip);
      when  Types.Lexer.Token_True        =>
        Logger.Info("Const => TRUE");
        A_Const_Node.Number  := Types.Lexer.Add_To_Natural_Table( 1 );
        Match(Types.Lexer.Token_True);
      when  others                        =>
        Syntax_Error_Output( "Const" );
    end case;
    return A_Const_Node;
  end Const;

end Parser;
