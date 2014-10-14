;; -------------------------------------------------------------------------
;; Constraints
;; -------------------------------------------------------------------------

(define_constraint "A"
  "An absolute address."
  (and (match_code "mem")
       (ior (match_test "GET_CODE (XEXP (op, 0)) == SYMBOL_REF")
	    (match_test "GET_CODE (XEXP (op, 0)) == LABEL_REF")
	    (match_test "GET_CODE (XEXP (op, 0)) == CONST"))))

(define_constraint "B"
  "An offset address."
  (and (match_code "mem")
       (match_test "GET_CODE (XEXP (op, 0)) == PLUS")))

(define_constraint "O"
  "The constant zero"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "W"
  "A register indirect memory operand."
  (and (match_code "mem")
       (match_test "REG_P (XEXP (op, 0))
		    && REGNO_OK_FOR_BASE_P (REGNO (XEXP (op, 0)))")))

(define_constraint "I"
  "An 13-bit signed constant (-8192..8191)"
  (and (match_code "const_int")
       (match_test "ival >= -8192 && ival <= 8191")))

(define_predicate "oldland_general_movsrc_operand"
  (match_code "mem,const_int,reg,subreg,symbol_ref,label_ref,const")
{
  /* Any (MEM LABEL_REF) is OK.  That is a pc-relative load.  */
  if (MEM_P (op) && GET_CODE (XEXP (op, 0)) == LABEL_REF)
    return 1;

  if (MEM_P (op)
      && GET_CODE (XEXP (op, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (op, 0), 0)) == CONST)
    return 1;

  return general_operand (op, mode);
})

; All instructions are four bytes long.
(define_attr "length" "" (const_int 4))

;; -------------------------------------------------------------------------
;; nop instruction
;; -------------------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

;; -------------------------------------------------------------------------
;; Arithmetic instructions
;; -------------------------------------------------------------------------

(define_predicate "oldland_arith_operand"
  (ior (match_code "reg")
       (and (match_code "const_int")
	    (match_test "IN_RANGE (INTVAL (op), -8192, 8191)"))))

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,=r")
	  (plus:SI
	   (match_operand:SI 1 "register_operand" "r,r")
	   (match_operand:SI 2 "oldland_arith_operand" "r,I")))]
  ""
  "add	 %0, %1, %2")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,=r")
	  (minus:SI
	   (match_operand:SI 1 "register_operand" "r,r")
	   (match_operand:SI 2 "oldland_arith_operand" "r,I")))]
  ""
  "sub	 %0, %1, %2")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,=r")
	  (mult:SI
	   (match_operand:SI 1 "register_operand" "r,r")
	   (match_operand:SI 2 "oldland_arith_operand" "r,I")))]
  ""
  "mul	 %0, %1, %2")

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "xor    %0, %1, -1")

;; -------------------------------------------------------------------------
;; Logical operators
;; -------------------------------------------------------------------------

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,=r")
	(and:SI (match_operand:SI 1 "register_operand" "r,r")
		(match_operand:SI 2 "oldland_arith_operand" "r,I")))]
  ""
{
  return "and    %0, %1, %2";
})

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,=r")
	(xor:SI (match_operand:SI 1 "register_operand" "r,r")
		(match_operand:SI 2 "oldland_arith_operand" "r,I")))]
  ""
{
  return "xor    %0, %1, %2";
})

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,=r")
	(ior:SI (match_operand:SI 1 "register_operand" "r,r")
		(match_operand:SI 2 "oldland_arith_operand" "r,I")))]
  ""
{
  return "or     %0, %1, %2";
})

;; -------------------------------------------------------------------------
;; Shifters
;; -------------------------------------------------------------------------

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "r,r")
		   (match_operand:SI 2 "oldland_arith_operand" "r,I")))]
  ""
{
  return "lsl   %0, %1, %2";
})

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "oldland_arith_operand" "r,I")))]
  ""
{
  return "asr   %0, %1, %2";
})

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,=r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "oldland_arith_operand" "r,I")))]
  ""
{
  return "lsr   %0, %1, %2";
})

;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

(define_expand "movsi"
   [(set (match_operand:SI 0 "general_operand" "")
 	(match_operand:SI 1 "general_operand" ""))]
   ""
  "
{
  /* If this is a store, force the value into a register.  */
  if (! (reload_in_progress || reload_completed))
  {
    if (MEM_P (operands[0]))
    {
      operands[1] = force_reg (SImode, operands[1]);
      if (MEM_P (XEXP (operands[0], 0)))
        operands[0] = gen_rtx_MEM (SImode, force_reg (SImode, XEXP (operands[0], 0)));
    }
    else 
      if (MEM_P (operands[1])
          && MEM_P (XEXP (operands[1], 0)))
        operands[1] = gen_rtx_MEM (SImode, force_reg (SImode, XEXP (operands[1], 0)));
  }
}")

(define_insn "*movsi"
  [(set (match_operand:SI 0 "general_operand" "=r,=r,=r,=A,=r,=r,=W,=B,=r")
	(match_operand:SI 1 "oldland_general_movsrc_operand" "O,r,i,r,W,A,r,r,B"))]
  "register_operand (operands[0], SImode)
   || register_operand (operands[1], SImode)"
  "@
   mov    %0, 0		/* =r, 0 */
   mov    %0, %1	/* r, r */
   movhi  %0, %%hi(%1)\n\torlo %0, %0, %%lo(%1) /* r, i */
   str32  %1, %0	/* A, r */
   ldr32  %0, %1	/* r, W */
   ldr32  %0, %1	/* r, A */
   str32  %1, %0	/* W, r */
   str32  %1, %0	/* B, r */
   ldr32  %0, %1        /* r, B */")

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  /* If this is a store, force the value into a register.  */
  if (MEM_P (operands[0]))
    operands[1] = force_reg (QImode, operands[1]);
}")

(define_insn "*movqi"
  [(set (match_operand:QI 0 "general_operand" "=r,=r,=r,=A,=r,=r,=W,=B,=r")
	(match_operand:QI 1 "oldland_general_movsrc_operand" "O,r,i,r,W,A,r,r,B"))]
  "register_operand (operands[0], QImode)
   || register_operand (operands[1], QImode)"
  "@
   mov    %0, 0		/* =r, 0 */
   mov    %0, %1	/* r, r */
   mov	 %0, %1
   str8  %1, %0	/* A, r */
   ldr8  %0, %1	/* r, W */
   ldr8  %0, %1	/* r, A */
   str8  %1, %0	/* W, r */
   str8  %1, %0	/* B, r */
   ldr8  %0, %1        /* r, B */")

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  /* If this is a store, force the value into a register.  */
  if (MEM_P (operands[0]))
    operands[1] = force_reg (HImode, operands[1]);
}")

(define_insn "*movhi"
  [(set (match_operand:HI 0 "general_operand" "=r,=r,=r,=A,=r,=r,=W,=B,=r")
	(match_operand:HI 1 "oldland_general_movsrc_operand" "O,r,i,r,W,A,r,r,B"))]
  "register_operand (operands[0], HImode)
   || register_operand (operands[1], HImode)"
  "@
   mov    %0, 0		/* =r, 0 */
   mov    %0, %1	/* r, r */
   movhi  %0, %%hi(%1)\n\torlo %0, %0, %%lo(%1) /* r, i */
   str16  %1, %0	/* A, r */
   ldr16  %0, %1	/* r, W */
   ldr16  %0, %1	/* r, A */
   str16  %1, %0	/* W, r */
   str16  %1, %0	/* B, r */
   ldr16  %0, %1        /* r, B */")

;; -------------------------------------------------------------------------
;; Compare instructions
;; -------------------------------------------------------------------------

(define_constants
  [(CC_REG 19)])

(define_expand "cbranchsi4"
  [(set (reg:CC CC_REG)
        (compare:CC
         (match_operand:SI 1 "general_operand" "")
         (match_operand:SI 2 "general_operand" "")))
   (set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
                       [(reg:CC CC_REG) (const_int 0)])
                      (label_ref (match_operand 3 "" ""))
                      (pc)))]
  ""
  "
  /* Force the compare operands into registers.  */
  if (GET_CODE (operands[1]) != REG)
	operands[1] = force_reg (SImode, operands[1]);
  if (GET_CODE (operands[2]) != REG)
	operands[2] = force_reg (SImode, operands[2]);
  ")

(define_insn "*cmpsi"
  [(set (reg:CC CC_REG)
	(compare
	 (match_operand:SI 0 "register_operand" "r")
	 (match_operand:SI 1 "register_operand"	"r")))]
  ""
  "cmp    %0, %1")


;; -------------------------------------------------------------------------
;; Branch instructions
;; -------------------------------------------------------------------------

(define_code_iterator cond [ne eq lt ltu gt gtu ge le geu leu])
(define_code_attr CC [(ne "ne") (eq "eq") (lt "lts") (ltu "lt") 
		      (gt "gts") (gtu "gt") (ge "gtes") (le "ltes")
		      (geu "gte") (leu "lte")])

(define_insn "*b<cond:code>"
  [(set (pc)
	(if_then_else (cond (reg:CC CC_REG)
			    (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  return "b<CC>   %l0";
})

;; -------------------------------------------------------------------------
;; Call and Jump instructions
;; -------------------------------------------------------------------------

(define_predicate "call_operand"
   (ior (match_code "symbol_ref")
           (match_operand 0 "register_operand")))

(define_expand "call"
  [(call (match_operand 0 "" "")
		(match_operand 1 "" ""))]
  ""
{
  rtx addr = XEXP (operands[0], 0);
  if (!CONSTANT_ADDRESS_P (addr))
    XEXP (operands[0], 0) = force_reg (Pmode, addr);
})

(define_insn "*call"
  [(call (mem:QI (match_operand:SI
		  0 "call_operand" "s,r"))
	 (match_operand 1 "" ""))]
  ""
  "call	  %0")

(define_expand "call_value"
  [(set (match_operand 0 "" "")
                   (call (match_operand 1 "" "")
                         (match_operand 2 "" "")))
             ]
  ""
  "
{
  rtx addr = XEXP (operands[1], 0);
  if (!CONSTANT_ADDRESS_P (addr))
    XEXP (operands[1], 0) = force_reg (Pmode, addr); 
}")

(define_insn "*call_value"
  [(set (match_operand 0 "register_operand" "=r,r")
	(call (mem:QI (match_operand:SI 1 "call_operand" "r,s"))
	      (match_operand 2 "" "")))]
  ""
  "call   %1")

(define_insn "*call_value_indirect"
  [(set (match_operand 0 "register_operand" "=r")
	(call (mem:QI (match_operand:SI
		       1 "register_operand" "r"))
	      (match_operand 2 "" "")))]
  ""
  "call    %1")

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "nonimmediate_operand" "r"))]
  ""
  "b    %0")

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "b   %l0")


;; -------------------------------------------------------------------------
;; Prologue & Epilogue
;; -------------------------------------------------------------------------

(define_expand "prologue"
  [(clobber (const_int 0))]
  ""
  "
{
  oldland_expand_prologue();
  DONE;
}
")

(define_expand "epilogue"
  [(return)]
  ""
  "
{
  oldland_expand_epilogue();
  DONE;
}
")

(define_insn "returner"
  [(return)]
  "reload_completed"
  "ret")
