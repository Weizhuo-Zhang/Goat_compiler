    call proc_main
    halt
proc_hail:
    push_stack_frame 3
  # init parameters
  # initialise parameters in
    store 0, r0
  # initialise parameters out
    store 1, r1
  # init variables
    int_const r0, 0
  # initialise variable count
    store 2, r0
  # Assign statement for variable count
    int_const r0, 1
    store 2, r0
    load r0, 0
    int_const r1, 1
    cmp_eq_int r0, r0, r1
    branch_on_false r0, hail_2_a
  # Assign statement for variable out
    load r0, 2
    store 1, r0
    branch_uncond hail_2_b
hail_2_a:
    load r0, 0
    int_const r1, 2
    div_int r0, r0, r1
    int_const r1, 2
    mul_int r0, r0, r1
    load r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_false r0, hail_2_2_1_a
    load r0, 0
    int_const r1, 2
    div_int r0, r0, r1
    load r0, 0
