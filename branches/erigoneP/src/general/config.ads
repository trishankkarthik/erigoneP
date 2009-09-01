-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
--
-- Configuration constants for the transitions
--
with Global; use Global;
package Config is
  -- Number of processes
  subtype Process_Index    is Byte range 0..7;

  -- Number of symbols
  subtype Symbol_Index     is Byte range 0..31;

  -- Number of elements in a channel message
  subtype Message_Index    is Byte range 0..3;

  -- Number of channels (index starts with 1)
  subtype Channel_Index    is Byte range 1..8;

  -- Number of bytes of data in a state
  subtype Data_Index       is Byte range 0..31;

  -- Number of transitions per process
  subtype Transition_Index is Byte range 0..127;

  -- Number of transitions from a state
  subtype Location_Index   is Byte range 0..7;

  -- Number of byte codes per statement
  subtype Byte_Code_Index  is Byte range 0..63;

  -- Size of interpretation stack
  subtype Interpret_Index  is Byte range 0..63;

  -- The maximum number of future formulas
  Max_Futures: constant := 4;
end Config;
