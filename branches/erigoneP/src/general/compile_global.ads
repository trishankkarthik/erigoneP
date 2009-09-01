-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
--
--  Interface to the compiler
--  The only package with'ed by both the compiler and the model checker
--
package Compile_Global is
  -- Exceptions exported by the compiler
  File_Error:        exception;
  Compilation_Error: exception;

  -- The instructions for the virtual machine
  type Opcode is (
    noop,
    -- Arithmetic
    iadd, idec, idiv, iinc, imul, ineg, irem, isub,
    -- Access to memory
    bipush,     iconst,       check_index,
    bit_load,   byte_load,    short_load,   unsigned_load,    iload,
    bit_aload,  byte_aload,   short_aload,  unsigned_aload,   iaload,
    bit_store,  byte_store,   short_store,  unsigned_store,   istore,
    bit_astore, byte_astore,  short_astore, unsigned_astore,  iastore,
    -- I/O
    printf, printm,
    -- Logical operators and relations
    logic_and,  logic_else, logic_not,  logic_or, iand,
    icmpeq,     icmpne,     icmplt,     icmple,   icmpgt, icmpge,
    ifeq,       ifne,       iflt,       ifle,     ifgt,   ifge,
    inot,       ior,        ishl,       ishr,     iushr,  ixor,
    -- Channels
    fifo_send,            sorted_send,
    move_fifo_receive,    copy_fifo_receive,
    move_random_receive,  copy_random_receive,
    fifo_poll,            random_poll,
    channel_len,
    channel_empty,        channel_nempty,
    channel_full,         channel_nfull,
    -- Load address of variable for receive statement
    load_address,
    -- Built-in functions & others
    assert, halt
  );

  -- Compile a source file and write the automata file
  procedure Compile_File(
    Source_File_Name: in String;
    Automata_File_Name: in String;
    Logs: in Boolean);

  -- Compile an expression (from an LTL formula)
  --   Return a string with the byte codes
  function Compile_Expression(Expression: String) return String;
end Compile_Global;
