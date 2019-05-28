    call proc_main
    halt
proc_bell:
    push_stack_frame 5
  # init parameters
  # initialise parameters n
    store 1, r0
  # initialise parameters m
    store 0, r1
  # initialise parameters out
    store 2, r2
  # init variables
    int_const r0, 0
  # init variable: res1
    store 3, r0
  # initialise variable res2
    store 4, r0
    load r0, 0
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_false r0, bell_1_a
    load r0, 1
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_false r0, bell_1_1_1_a
  # Assign statement for variable out
    int_const r0, 1
    store 2, r0
    branch_uncond bell_1_1_1_b
bell_1_1_1_a:
    load r0, 1
    int_const r1, 1
    sub_int r0, r0, r1
    load r0, 0
