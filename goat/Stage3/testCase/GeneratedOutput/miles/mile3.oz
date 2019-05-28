    call proc_main
    halt
proc_main:
    push_stack_frame 1
  # init variables
    int_const r0, 0
  # initialise variable x
    store 0, r0
    string_const r0, "Integer, please: "
    call_builtin print_string
    call_builtin read_int
    store 0, r0
main_3_a:
    load r0, 0
    int_const r1, 10
    cmp_gt_int r0, r0, r1
    load r1, 0
    int_const r2, 100
    cmp_lt_int r1, r1, r2
    and r0, r0, r1
    branch_on_true r0, main_3_b
    branch_uncond main_3_c
main_3_b:
    load r0, 0
    int_const r1, 50
    cmp_lt_int r0, r0, r1
    branch_on_false r0, main_3_3_1_a
  # Assign statement for variable x
    load r0, 0
    int_const r1, 1
    sub_int r0, r0, r1
    store 0, r0
    branch_uncond main_3_3_1_b
main_3_3_1_a:
  # Assign statement for variable x
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
main_3_3_1_b:
    branch_uncond main_3_a
main_3_c:
    load r0, 0
    int_const r1, 50
    cmp_lt_int r0, r0, r1
    branch_on_false r0, main_4_a
    string_const r0, "Went down"
    call_builtin print_string
    branch_uncond main_4_b
main_4_a:
    string_const r0, "Went up"
    call_builtin print_string
main_4_b:
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 1
    return
