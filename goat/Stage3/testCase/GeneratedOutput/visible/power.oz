    call proc_main
    halt
proc_main:
    push_stack_frame 3
  # init variables
    int_const r0, 0
  # init variable: n
    store 0, r0
  # init variable: result
    store 1, r0
  # initialise variable x
    store 2, r0
    string_const r0, "Float x: "
    call_builtin print_string
    call_builtin read_real
    store 2, r0
    string_const r0, "Positive integer n: "
    call_builtin print_string
    call_builtin read_int
    store 0, r0
    load r0, 2
    load r0, 0
