    call proc_main
    halt
proc_main:
    push_stack_frame 1
  # init variables
    int_const r0, 0
  # initialise variable result
    store 0, r0
    int_const r0, 4
    load r0, 0
