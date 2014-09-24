#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "reload.h"
#include "diagnostic-core.h"
#include "obstack.h"
#include "tree.h"
#include "expr.h"
#include "optabs.h"
#include "except.h"
#include "function.h"
#include "ggc.h"
#include "target.h"
#include "target-def.h"
#include "tm_p.h"
#include "langhooks.h"
#include "df.h"

/* Per-function machine data.  */
struct GTY(()) machine_function {
	/* Number of bytes saved on the stack for callee saved registers.  */
	int callee_saved_reg_size;

	/* Number of bytes saved on the stack for local variables.  */
	int local_vars_size;

	/* The sum of 2 sizes: locals vars and padding byte for saving the
	 * registers.  Used in expand_prologue () and expand_epilogue().  */
	int size_for_adjusting_sp;
};

/* Zero initialization is OK for all current fields.  */

static struct machine_function *oldland_init_machine_status(void)
{
	return ggc_alloc_cleared_machine_function();
}


/* The TARGET_OPTION_OVERRIDE worker.
   All this curently does is set init_machine_status.  */
static void oldland_option_override(void)
{
	/* Set the per-function-data initializer.  */
	init_machine_status = oldland_init_machine_status;
}

/*
 * Oldland frame layout
 *
 * High address
 *
 * +-------------------+
 * | caller            |
 * +-------------------+
 * | stack args        |
 * +-------------------+ <-- SP at call ----------\
 * | return address    |                          |
 * +-------------------+                          |
 * | saved fp          |                          |
 * +-------------------+ <-- FP after prologue    |
 * | callee saved regs |                          |
 * +-------------------+                          |
 * | locals            |                          |
 * +-------------------+ <-- SP after prologue <--/
 */

/* Save return address + FP */
#define OLDLAND_FRAME_SAVE_SIZE (4 * 2)

static void oldland_compute_frame(void)
{
	/* For aligning the local variables.  */
	int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
	int padding_locals;
	int regno;

	/* Padding needed for each element of the frame.  */
	cfun->machine->local_vars_size = get_frame_size();

	/* Align to the stack alignment.  */
	padding_locals = cfun->machine->local_vars_size % stack_alignment;
	if (padding_locals)
		padding_locals = stack_alignment - padding_locals;

	cfun->machine->local_vars_size += padding_locals;

	cfun->machine->callee_saved_reg_size = 0;

	/* Save callee-saved registers.  */
	for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if (df_regs_ever_live_p (regno) && (! call_used_regs[regno]))
			cfun->machine->callee_saved_reg_size += 4;

	cfun->machine->size_for_adjusting_sp = 
		OLDLAND_FRAME_SAVE_SIZE + 
		crtl->args.pretend_args_size +
		cfun->machine->local_vars_size +
		(ACCUMULATE_OUTGOING_ARGS ? crtl->outgoing_args_size : 0);
}

static void frame_save_reg(int regno, int offset)
{
	rtx reg = gen_rtx_REG(Pmode, regno);

	rtx slot = gen_frame_mem(SImode,
				 gen_rtx_PLUS(Pmode, stack_pointer_rtx,
					      GEN_INT(UNITS_PER_WORD * offset)));
	emit_move_insn(slot, reg);
}

static void frame_load_reg(int regno, int offset)
{
	rtx reg = gen_rtx_REG(Pmode, regno);

	rtx slot = gen_frame_mem(SImode,
				 gen_rtx_PLUS(Pmode, stack_pointer_rtx,
					      GEN_INT(UNITS_PER_WORD * offset)));
	emit_move_insn(reg, slot);
}

static void set_new_fp(void)
{
	emit_insn(gen_subsi3(hard_frame_pointer_rtx, stack_pointer_rtx,
			     GEN_INT(8)));
}

static void restore_fp(void)
{
	emit_insn(gen_addsi3(hard_frame_pointer_rtx, stack_pointer_rtx,
			     GEN_INT(8)));
}

static void save_callee_save_regs(void)
{
	int regno;
	int save_offs = -3; /* FP+LR already saved. */

	if (flag_stack_usage_info)
		current_function_static_stack_size = cfun->machine->size_for_adjusting_sp;

	/* Save callee-saved registers.  */
	for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if (!fixed_regs[regno] && df_regs_ever_live_p(regno) &&
		    !call_used_regs[regno])
			frame_save_reg(regno, save_offs--);
}

static void restore_callee_save_regs(void)
{
	int regno;
	int save_offs = -3; /* FP+LR already saved. */

	if (flag_stack_usage_info)
		current_function_static_stack_size = cfun->machine->size_for_adjusting_sp;

	/* Save callee-saved registers.  */
	for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if (!fixed_regs[regno] && df_regs_ever_live_p(regno) &&
		    !call_used_regs[regno])
			frame_load_reg(regno, save_offs--);
}

static void set_new_sp(void)
{
	rtx insn;
	int adjustment = cfun->machine->size_for_adjusting_sp;

	if (adjustment == 0)
		return;

	insn = emit_insn(gen_subsi3(stack_pointer_rtx, stack_pointer_rtx,
				    GEN_INT(adjustment)));
	RTX_FRAME_RELATED_P(insn) = 1;
}

static void restore_sp(void)
{
	int adjustment = cfun->machine->size_for_adjusting_sp;

	if (adjustment == 0)
		return;

	emit_insn(gen_addsi3(stack_pointer_rtx, stack_pointer_rtx,
			     GEN_INT(adjustment)));
}

static void save_frame(void)
{
	frame_save_reg(OLDLAND_LR, -1);
	frame_save_reg(OLDLAND_FP, -2);
	set_new_fp();
	save_callee_save_regs();
	set_new_sp();
}

void oldland_expand_prologue(void)
{
	oldland_compute_frame();
	save_frame();
}

void oldland_expand_epilogue(void)
{
	restore_sp();
	restore_callee_save_regs();
	restore_fp();
	frame_load_reg(OLDLAND_FP, -2);
	frame_load_reg(OLDLAND_LR, -1);
	emit_jump_insn(gen_returner());
}

static void oldland_operand_lossage(const char *msgid, rtx op)
{
	debug_rtx(op);
	output_operand_lossage("%s", msgid);
}

#define LOSE_AND_RETURN(msgid, x) \
	do { \
		oldland_operand_lossage (msgid, x); \
		return;	\
	} while (0)

void oldland_print_operand_address (FILE *file, rtx x)
{
	switch (GET_CODE (x))
	{
	case REG:
		fprintf (file, "[%s, 0]", reg_names[REGNO (x)]);
		break;

	case PLUS:
		switch (GET_CODE (XEXP (x, 1))) {
		case CONST_INT:
			fprintf (file, "[%s, %ld]",
				 reg_names[REGNO (XEXP (x, 0))], INTVAL(XEXP (x, 1)));
			break;
		case SYMBOL_REF:
			output_addr_const (file, XEXP (x, 1));
			fprintf (file, "%s", reg_names[REGNO (XEXP (x, 0))]);
			break;
		case CONST:
			{
				rtx plus = XEXP (XEXP (x, 1), 0);
				if (GET_CODE (XEXP (plus, 0)) == SYMBOL_REF 
				    && CONST_INT_P (XEXP (plus, 1))) {
					output_addr_const(file, XEXP (plus, 0));
					fprintf (file, "[%s, %ld]", reg_names[REGNO (XEXP (x, 0))],
						 INTVAL (XEXP (plus, 1)));
				}
				else
					abort();
			}
			break;
		default:
			abort();
		}
		break;

	default:
		output_addr_const (file, x);
		break;
	}
}

void
oldland_print_operand (FILE *file, rtx x, int code)
{
	rtx operand = x;

	/* New code entries should just be added to the switch below.  If
	   handling is finished, just return.  If handling was just a
	   modification of the operand, the modified operand should be put in
	   "operand", and then do a break to let default handling
	   (zero-modifier) output the operand.  */

	switch (code) {
	case 0:
		/* No code, print as usual.  */
		break;

	default:
		LOSE_AND_RETURN ("invalid operand modifier letter", x);
	}

	/* Print an operand as without a modifier letter.  */
	switch (GET_CODE (operand)) {
	case REG:
		if (REGNO (operand) > OLDLAND_LR)
			internal_error ("internal error: bad register: %d", REGNO (operand));
		fprintf (file, "%s", reg_names[REGNO (operand)]);
		return;

	case MEM:
		output_address (XEXP (operand, 0));
		return;

	default:
		/* No need to handle all strange variants, let output_addr_const
		   do it for us.  */
		if (CONSTANT_P (operand)) {
			output_addr_const (file, operand);
			return;
		}

		LOSE_AND_RETURN ("unexpected operand", x);
	}
}

int
oldland_initial_elimination_offset (int from, int to)
{
	(void)from;
	(void)to;

	return 0;
}

static rtx oldland_function_value(const_tree valtype,
				  const_tree fntype_or_decl ATTRIBUTE_UNUSED,
				  bool outgoing ATTRIBUTE_UNUSED)

{
	return gen_rtx_REG(TYPE_MODE(valtype), OLDLAND_R0);
}
#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE oldland_function_value

static bool oldland_return_in_memory(const_tree type,
				     const_tree fntype ATTRIBUTE_UNUSED)
{
	const HOST_WIDE_INT size = int_size_in_bytes (type);

	return (size == -1 || size > 2 * UNITS_PER_WORD);
}
#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY oldland_return_in_memory

static rtx oldland_libcall_value(enum machine_mode mode,
				 const_rtx fun ATTRIBUTE_UNUSED)
{
	return gen_rtx_REG (mode, OLDLAND_R0);
}
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE oldland_libcall_value

static bool oldland_function_value_regno_p(const unsigned int regno)
{
	return (regno == OLDLAND_R0);
}
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P oldland_function_value_regno_p

static rtx oldland_function_arg(cumulative_args_t cum_v, enum machine_mode mode,
				const_tree type ATTRIBUTE_UNUSED,
				bool named ATTRIBUTE_UNUSED)
{
	CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

	if (*cum <= OLDLAND_R5)
		return gen_rtx_REG (mode, *cum);
	else 
		return NULL_RTX;
}
#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG oldland_function_arg

#define OLDLAND_FUNCTION_ARG_SIZE(MODE, TYPE)	\
  ((MODE) != BLKmode ? GET_MODE_SIZE (MODE)	\
   : (unsigned) int_size_in_bytes (TYPE))

static void oldland_function_arg_advance(cumulative_args_t cum_v,
					 enum machine_mode mode,
					 const_tree type,
					 bool named ATTRIBUTE_UNUSED)
{
	CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

	*cum = (*cum <= OLDLAND_R5
		? *cum + ((3 + OLDLAND_FUNCTION_ARG_SIZE (mode, type)) / 4)
		: *cum);
}
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE oldland_function_arg_advance

static bool oldland_pass_by_reference(cumulative_args_t cum ATTRIBUTE_UNUSED,
				      enum machine_mode mode, const_tree type,
				      bool named ATTRIBUTE_UNUSED)
{
	unsigned HOST_WIDE_INT size;

	if (type)
	{
		if (AGGREGATE_TYPE_P (type))
			return true;
		size = int_size_in_bytes (type);
	}
	else
		size = GET_MODE_SIZE (mode);

	return size > 4 * OLDLAND_R5;
}
#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE        oldland_pass_by_reference

static int oldland_arg_partial_bytes(cumulative_args_t cum_v,
				     enum machine_mode mode,
				     tree type, bool named)
{
	CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
	int bytes_left, size;

	if (*cum >= 8)
		return 0;

	if (oldland_pass_by_reference (cum_v, mode, type, named))
		size = 4;
	else if (type) {
		if (AGGREGATE_TYPE_P (type))
			return 0;
		size = int_size_in_bytes (type);
	} else
		size = GET_MODE_SIZE (mode);

	bytes_left = (4 * 6) - ((*cum - 2) * 4);

	if (size > bytes_left)
		return bytes_left;
	else
		return 0;
}
#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES        oldland_arg_partial_bytes

#undef  TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK	must_pass_in_stack_var_size

#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP ".byte "
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP  ".word "
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP  ".word "
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP ".long "
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP ".long "

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE oldland_option_override

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-oldland.h"
