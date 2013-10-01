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

	if (*cum < 6)
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

struct gcc_target targetm = TARGET_INITIALIZER;
