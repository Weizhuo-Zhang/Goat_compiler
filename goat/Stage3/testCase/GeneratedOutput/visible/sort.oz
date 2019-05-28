    call proc_main
    halt
proc_main:
    push_stack_frame 12
  # init variables
    int_const r0, 0
  # init variable: h
    store 0, r0
  # init variable: j
    store 1, r0
  # init variable: k
    store 2, r0
  # init variable: tmp
    store 3, r0
  # initialise variable x
    store 4, r0
    store 5, r0
    store 6, r0
    store 7, r0
    store 8, r0
    store 9, r0
    store 10, r0
    store 11, r0
    string_const r0, "Provide 8 integers:\n"
    call_builtin print_string
    string_const r0, "> "
    call_builtin print_string
    call_builtin read_int
    store 4, r0
    string_const r0, "> "
    call_builtin print_string
    call_builtin read_int
    store 4, r0
    string_const r0, "> "
    call_builtin print_string
    call_builtin read_int
    store 4, r0
    string_const r0, "> "
    call_builtin print_string
    call_builtin read_int
    store 4, r0
    string_const r0, "> "
    call_builtin print_string
    call_builtin read_int
    store 4, r0
    string_const r0, "> "
    call_builtin print_string
    call_builtin read_int
    store 4, r0
    string_const r0, "> "
    call_builtin print_string
    call_builtin read_int
    store 4, r0
    string_const r0, "> "
    call_builtin print_string
    call_builtin read_int
    store 4, r0
  # Assign statement for variable h
    int_const r0, 0
    store 0, r0
main_19_a:
    load r0, 0
    int_const r1, 7
    cmp_lt_int r0, r0, r1
    branch_on_true r0, main_19_b
    branch_uncond main_19_c
main_19_b:
  # Assign statement for variable j
    load r0, 0
    store 1, r0
  # Assign statement for variable k
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 2, r0
main_19_3_3_a:
    load r0, 2
    int_const r1, 7
    cmp_le_int r0, r0, r1
    branch_on_true r0, main_19_3_3_b
    branch_uncond main_19_3_3_c
main_19_3_3_b:
  # Generate Array x
    load r0, 2
    load_address r1, 4
    sub_offset r0, r1, r0
    load_indirect r0, r0
  # Generate Array x
    load r1, 1
    load_address r2, 4
    sub_offset r1, r2, r1
    load_indirect r1, r1
    cmp_gt_int r0, r0, r1
    branch_on_true r0, main_19_3_3_3_1_a
    branch_uncond main_19_3_3_3_1_b
main_19_3_3_3_1_a:
  # Assign statement for variable j
    load r0, 2
    store 1, r0
main_19_3_3_3_1_b:
  # Assign statement for variable k
    load r0, 2
    int_const r1, 1
    add_int r0, r0, r1
    store 2, r0
    branch_uncond main_19_3_3_a
main_19_3_3_c:
  # Assign statement for variable tmp
  # Generate Array x
    load r0, 0
    load_address r1, 4
    sub_offset r0, r1, r0
    load_indirect r0, r0
    store 3, r0
  # Assign statement for variable x
  # Generate Array x
    load r0, 1
    load_address r1, 4
    sub_offset r0, r1, r0
    load_indirect r0, r0
  # Generate Array x
    load r1, 0
    load_address r2, 4
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Assign statement for variable x
    load r0, 3
  # Generate Array x
    load r1, 1
    load_address r2, 4
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Assign statement for variable h
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    branch_uncond main_19_a
main_19_c:
    string_const r0, "\nThe integers, in descending order:\n\n"
    call_builtin print_string
  # Assign statement for variable h
    int_const r0, 0
    store 0, r0
main_22_a:
    load r0, 0
    int_const r1, 8
    cmp_lt_int r0, r0, r1
    branch_on_true r0, main_22_b
    branch_uncond main_22_c
main_22_b:
  # Generate Array x
    load r0, 0
    load_address r1, 4
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
    string_const r0, "  "
    call_builtin print_string
  # Assign statement for variable h
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    branch_uncond main_22_a
main_22_c:
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 12
    return
