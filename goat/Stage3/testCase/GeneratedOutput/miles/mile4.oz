    call proc_main
    halt
proc_f:
    push_stack_frame 2
  # init parameters
  # initialise parameters x
    store 0, r0
  # initialise parameters y
    store 1, r1
    load r0, 0
    int_const r1, 4
    cmp_gt_int r0, r0, r1
    load r1, 1
    or r0, r0, r1
    call_builtin print_int
    pop_stack_frame 2
    return
proc_main:
    int_const r0, 8
    load r0, 0
