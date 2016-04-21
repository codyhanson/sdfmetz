#ifndef BUILTINS_H
#define BUILTINS_H
#include <MEPT.h>

/* DO NOT EDIT. This file is generated */

void initBuiltins(void);
PT_Tree forwardBuiltin(ATerm builtin, PT_Tree input);
PT_Tree ASFE_set_anno(PT_Tree input);
PT_Tree ASC_set_anno(ATerm arg3, ATerm arg2, ATerm arg1);

PT_Tree ASFE_get_anno(PT_Tree input);
PT_Tree ASC_get_anno(ATerm arg2, ATerm arg1);

PT_Tree ASFE_get_term_anno(PT_Tree input);
PT_Tree ASC_get_term_anno(ATerm arg2, ATerm arg1);

PT_Tree ASFE_read_term_from_file(PT_Tree input);
PT_Tree ASC_read_term_from_file(ATerm arg1);

PT_Tree ASFE_write_term_to_file(PT_Tree input);
PT_Tree ASC_write_term_to_file(ATerm arg2, ATerm arg1);

PT_Tree ASFE_shell(PT_Tree input);
PT_Tree ASC_shell(ATerm arg1);

PT_Tree ASFE_term_compare(PT_Tree input);
PT_Tree ASC_term_compare(ATerm arg2, ATerm arg1);

PT_Tree ASFE_lift_to_tree(PT_Tree input);
PT_Tree ASC_lift_to_tree(ATerm arg1);

PT_Tree ASFE_lift_to_term(PT_Tree input);
PT_Tree ASC_lift_to_term(ATerm arg1);

PT_Tree ASFE_unparse(PT_Tree input);
PT_Tree ASC_unparse(ATerm arg1);

#endif
