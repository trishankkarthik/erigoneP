-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
with Ada.Text_IO, Ada.Characters.Handling;
with Utilities;
package body LTL.Formulas is
  -- Atomic propositions can be of the form #(critical<=1)#
  Expression_Delimiter: constant Character := '#';

  -- Display a formula
  procedure Put_Formula(F: in Formula_Ptr) is
    use Ada.Text_IO;
  begin
    if F = null then 
      return;
    elsif F.Token = Atomic then
      Put(Utilities.Trim(F.Ident));
    elsif F.Token = Negation then
      Put("!");
      Put_Formula(F.Right);
    elsif F.Token = Always   then 
      Put("[]");
      Put_Formula(F.Right);
    elsif F.Token = Eventually then
      Put("<>");
      Put_Formula(F.Right);
    else
      Put("(");
      Put_Formula(F.Left);
      case F.Token is
        when Conjunction   => Put("&&");
        when Disjunction   => Put("||");
        when Until_Op      => Put("U");
        when Dual_Until_Op => Put("V");
        when others        => null;
      end case;
      Put_Formula(F.Right);
      Put(")");
    end if;
  end Put_Formula;

  -- Display a set of formulas
  procedure Put_Formula_Set(F_Set: in Formula_Sets.Set) is
    use Ada.Text_IO;
    procedure Put_One_Formula(Position: in Formula_Sets.Cursor) is
    begin
      Formulas.Put_Formula(Formula_Sets.Element(Position));
      Put(",");
    end Put_One_Formula;
  begin
    if Formula_Sets.Is_Empty(F_Set) then
      Put(",");
    else
      Put("{");
      Formula_Sets.Iterate(F_Set, Put_One_Formula'Access);
      Put("},");
    end if;
  end Put_Formula_Set;

  -- Allocate and construct a formula from its components
  -- Default null subformulas and blank identifier
  function Construct(
    Token: Tokens;
    Left:  Formula_Ptr := null;
    Right: Formula_Ptr := null;
    Ident: Name        := Blanks) 
      return Formula_Ptr is
  begin
    return new Formula'(Token, Ident, Left, Right);
  end Construct;

  -- Compile a string to a formula
  -- Atomic formulas must not have upper case U and V
  function To_Formula(S: String) return Formula_Ptr is
    I:           Positive := S'First;  -- String indices
    J:           Positive;
    N:           Name;                 -- Identifier
    Left, Right: Formula_Ptr;          -- Subformula pointers

    -- Inner recursive function
    function To_Formula return Formula_Ptr is

      -- Return a unary formula
      function To_Unary return Formula_Ptr is
        Right: Formula_Ptr;
      begin
        -- Skip blanks
        while S(I) = ' ' loop I := I + 1; end loop;

        -- Allow parentheses
        if S(I) = '(' then
          I := I + 1;
          Right := To_Formula;
          while S(I) = ' ' loop I := I + 1; end loop;
          if S(I) /= ')' then
            raise Unexpected_Input with "syntax error in LTL formula";
          else
            return Right;
          end if;

        elsif S(I) = '!' then
          I := I + 1;
          return Construct(Negation, Right => To_Unary);
        elsif S(I) = '[' and then S(I+1) = ']' then
          I := I + 2;
          return Construct(Always, Right => To_Unary);
        elsif S(I) = '<' and then S(I+1) = '>' then
          I := I + 2;
          return Construct(Eventually, Right => To_Unary);

        -- Get an expression delimited by Expression_Delimiter
        elsif S(I) = Expression_Delimiter then
          J := I+1;
          while J <= S'Length and then S(J) /= Expression_Delimiter loop
            J := J + 1;
          end loop;
          N := Utilities.Pad(S(I+1..J-1));
          I := J;
          return Construct(Atomic, null, null, N);

        -- Get an identifier which must start with lower case
        -- We do not allow upper-case U and V in identifiers
        elsif Ada.Characters.Handling.Is_Lower(S(I)) then
          J := I+1;
          while J <= S'Length and then 
                (Ada.Characters.Handling.Is_Alphanumeric(S(J)) 
                and S(J) /= 'U' and S(J) /= 'V') loop
            J := J + 1;
          end loop;
          N := Utilities.Pad(S(I..J-1));
          I := J;
          return Construct(Atomic, null, null, N);
        else
          raise Unexpected_Input with "syntax error in LTL formula";
        end if;
      end To_Unary;

    begin -- Inner function To_Formula
      -- Get left subformula
      Left := To_Unary;

      -- Skip blanks
      while I <= S'Length and then S(I) = ' ' loop I := I + 1; end loop;

      -- Look for a binary operator
      if I > S'Length then
        return Left;
      elsif S(I) = '&' and then S(I+1) = '&' then
        I := I + 2;
        Right := To_Unary;
        return Construct(Conjunction, Left, Right);
      elsif S(I) = '|' and then S(I+1) = '|' then
        I := I + 2;
        Right := To_Unary;
        return Construct(Disjunction, Left, Right);
      elsif S(I) = 'U' then
        I := I + 1;
        Right := To_Unary;
        return Construct(Until_Op, Left, Right);
      elsif S(I) = 'V' then
        I := I + 1;
        Right := To_Unary;
        return Construct(Dual_Until_Op, Left, Right);
      else
        return Left;
      end if;
    end To_Formula;

  begin  -- Outer function To_Formula
    return To_Formula;
  end To_Formula;

  -- Push negation inwards using duality of the operators
  function Push_Negation(F: Formula_Ptr) return Formula_Ptr is
    Dual_Operator: Tokens;
  begin
    if F = null then
      return F;
    elsif F.Token = Atomic then
      return F;
    elsif F.Token = Negation then
      -- Remove double negation
      if F.Right.Token = Negation then
        return F.Right.Right;
      -- If a negated atomic formula, return it
      elsif F.Right.Token = Atomic then
        return F;

      -- Unary temporal operators
      elsif F.Right.Token = Always then
        F.Right.Token := Negation;
        return Construct(Eventually, Right => Push_Negation(F.Right));
      elsif F.Right.Token = Eventually then
        F.Right.Token := Negation;
        return Construct(Always, Right => Push_Negation(F.Right));

      -- Binary propositional and temporal operators
      else
        case F.Right.Token is
          when Conjunction   => Dual_Operator := Disjunction;
          when Disjunction   => Dual_Operator := Conjunction;
          when Until_Op      => Dual_Operator := Dual_Until_Op;
          when Dual_Until_Op => Dual_Operator := Until_Op;
          -- Other operators already taken care of by the if-statement
          when others => null;
        end case;

        -- Recurse on subformulas
        return Construct(Dual_Operator,
          Push_Negation(Construct(Negation, Right => F.Right.Left)),
          Push_Negation(Construct(Negation, Right => F.Right.Right)));
      end if;
    else
      return Construct(
        F.Token, Push_Negation(F.Left), Push_Negation(F.Right));
    end if;
  end Push_Negation;

  -- Check for a contradiction
  function  Contradiction(Left, Right: Formula_Ptr) return Boolean is
  begin
    return 
     ((Left.Token = Atomic        and Right.Token = Negation) and then 
      (Right.Right.Token = Atomic and Left.Ident = Right.Right.Ident)) or
     ((Right.Token = Atomic       and Left.Token = Negation) and then
      (Left.Right.Token = Atomic  and Right.Ident = Left.Right.Ident));
  end Contradiction;

  -- Return the string representation of a literal
  -- A literal is an atomic formula or a negation of one
  -- If the formula is not a literal, return true (1)
  function  Get_Literal(F: Formula_Ptr) return Name is
  begin
    if F.Token = Atomic then
      return F.Ident;
    elsif F.Token = Negation and F.Right.Token = Atomic then
      return '!' & F.Right.Ident(Name'First .. Name'Last-1);
    else
      return Utilities.Pad("1");
    end if;
  end Get_Literal;

  -- Decompose a formula F into sets of subformulas New1, New2
  -- For temporal operators,
  --   there is a set that must be true in the Next node
  procedure Decompose(
    F: in Formula_Ptr; New1, New2, Next: out Formula_Sets.Set) is
    use Formula_Sets;
  begin
    if F.Token = Conjunction or F.Token = Disjunction then
      New1 := To_Set(F.Left);
      New2 := To_Set(F.Right);
      Next := Empty_Set;
    elsif F.Token = Always   or F.Token = Eventually then
      New1 := To_Set(F.Right);
      New2 := Empty_Set;
      Next := To_Set(F);
    elsif F.Token = Until_Op then
      New1 := To_Set(F.Right);
      New2 := To_Set(F.Left);
      Next := To_Set(F);
    elsif F.Token = Dual_Until_Op then
      New1 := Union(To_Set(F.Left), To_Set(F.Right));
      New2 := To_Set(F.Right);
      Next := To_Set(F);
    end if;
  end Decompose;
end LTL.Formulas;
