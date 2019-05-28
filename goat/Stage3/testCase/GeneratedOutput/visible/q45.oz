    call proc_main
    halt
proc_main:
    push_stack_frame 3
  # init variables
    int_const r0, 0
  # init variable: a
    store 0, r0
  # init variable: count
    store 1, r0
  # initialise variable x
    store 2, r0
  # Assign statement for variable count
    int_const r0, 0
    store 1, r0
    call_builtin read_real
    store 2, r0
main_3_a:
    load r0, 2
    int_const r1, 0
    int_to_real r1, r1
    cmp_gt_real r0, r0, r1
    branch_on_true r0, main_3_b
    branch_uncond main_3_c
main_3_b:
  # Assign statement for variable count
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
    call_builtin read_real
    store 2, r0
    branch_uncond main_3_a
main_3_c:
    pop_stack_frame 3
    return
