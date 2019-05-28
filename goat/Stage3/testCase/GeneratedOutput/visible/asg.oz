    call proc_main
    halt
proc_main:
    push_stack_frame 2
  # init variables
    int_const r0, 0
  # init variable: a
    store 0, r0
  # initialise variable b
    store 1, r0
  # Assign statement for variable a
    int_const r0, 42
    store 0, r0
    load r0, 0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable a
    load r0, 0
    store 0, r0
    load r0, 0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable a
    int_const r0, 4
    int_const r1, 5
    mul_int r0, r0, r1
    int_const r1, 6
    mul_int r0, r0, r1
    int_const r1, 1
    int_const r2, 2
    mul_int r1, r1, r2
    int_const r2, 3
    mul_int r1, r1, r2
    add_int r0, r0, r1
    store 0, r0
    load r0, 0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable a
    int_const r0, 4
    int_const r1, 5
    div_int r0, r0, r1
    int_const r1, 6
    div_int r0, r0, r1
    int_const r1, 1
    int_const r2, 2
    div_int r1, r1, r2
    int_const r2, 3
    div_int r1, r1, r2
    sub_int r0, r0, r1
    store 0, r0
    load r0, 0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable a
    int_const r0, 4
    int_const r1, 5
    add_int r0, r0, r1
    int_const r1, 6
    add_int r0, r0, r1
    int_const r1, 1
    int_const r2, 2
    add_int r1, r1, r2
    int_const r2, 3
    add_int r1, r1, r2
    mul_int r0, r0, r1
    store 0, r0
    load r0, 0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable a
    int_const r0, 4
    int_const r1, 5
    sub_int r0, r0, r1
    int_const r1, 6
    sub_int r0, r0, r1
    int_const r1, 1
    int_const r2, 2
    sub_int r1, r1, r2
    int_const r2, 3
    sub_int r1, r1, r2
    div_int r0, r0, r1
    store 0, r0
    load r0, 0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable a
    int_const r0, 0
    neg_int r0, r0
    store 0, r0
    load r0, 0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable a
    int_const r0, 1
    int_const r1, 2
    mul_int r0, r0, r1
    int_const r1, 3
    mul_int r0, r0, r1
    neg_int r0, r0
    store 0, r0
    load r0, 0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable b
    int_const r0, 1
    store 1, r0
  # Assign statement for variable b
    load r0, 1
    int_const r1, 1
    int_const r2, 0
    and r1, r1, r2
    and r0, r0, r1
    int_const r1, 1
    int_const r2, 0
    or r1, r1, r2
    int_const r2, 1
    or r1, r1, r2
    and r0, r0, r1
    store 1, r0
    load r0, 1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable b
    load r0, 1
    int_const r1, 1
    int_const r2, 0
    int_const r3, 1
    and r2, r2, r3
    and r1, r1, r2
    or r0, r0, r1
    int_const r1, 0
    int_const r2, 1
    or r1, r1, r2
    or r0, r0, r1
    store 1, r0
    load r0, 1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable b
    int_const r0, 1
    int_const r1, 0
    and r0, r0, r1
    int_const r1, 1
    or r0, r0, r1
    not r0, r0
    store 1, r0
    load r0, 1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable b
    int_const r0, 4
    int_const r1, 5
    mul_int r0, r0, r1
    int_const r1, 6
    mul_int r0, r0, r1
    int_const r1, 1
    int_const r2, 2
    add_int r1, r1, r2
    int_const r2, 3
    add_int r1, r1, r2
    cmp_gt_int r0, r0, r1
    store 1, r0
    load r0, 1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable b
    int_const r0, 4
    int_const r1, 5
    mul_int r0, r0, r1
    int_const r1, 6
    mul_int r0, r0, r1
    int_const r1, 1
    int_const r2, 2
    add_int r1, r1, r2
    int_const r2, 3
    add_int r1, r1, r2
    cmp_le_int r0, r0, r1
    store 1, r0
    load r0, 1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable b
    int_const r0, 1
    int_const r1, 2
    add_int r0, r0, r1
    int_const r1, 3
    add_int r0, r0, r1
    int_const r1, 4
    int_const r2, 5
    mul_int r1, r1, r2
    int_const r2, 6
    mul_int r1, r1, r2
    cmp_lt_int r0, r0, r1
    store 1, r0
    load r0, 1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable b
    int_const r0, 1
    int_const r1, 2
    add_int r0, r0, r1
    int_const r1, 3
    add_int r0, r0, r1
    int_const r1, 4
    int_const r2, 5
    mul_int r1, r1, r2
    int_const r2, 6
    mul_int r1, r1, r2
    cmp_ge_int r0, r0, r1
    store 1, r0
    load r0, 1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 2
    return
