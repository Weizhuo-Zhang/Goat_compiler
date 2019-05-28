    call proc_main
    halt
proc_main:
    push_stack_frame 1
  # init variables
    int_const r0, 0
  # initialise variable stuff
    store 0, r0
    string_const r0, "Type a floating point number: "
    call_builtin print_string
    call_builtin read_real
    store 0, r0
    load r0, 0
    call_builtin print_real
    string_const r0, "\n"
    call_builtin print_string
    int_const r0, 42
    call_builtin print_int
    real_const r0, 1.1234
    call_builtin print_real
    string_const r0, "\n"
    call_builtin print_string
    int_const r0, 1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    int_const r0, 1
    int_const r1, 2
    mul_int r0, r0, r1
    int_const r1, 3
    mul_int r0, r0, r1
    int_const r1, 4
    int_const r2, 5
    mul_int r1, r1, r2
    int_const r2, 6
    mul_int r1, r1, r2
    add_int r0, r0, r1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    int_const r0, 1
    int_const r1, 2
    add_int r0, r0, r1
    int_const r1, 3
    add_int r0, r0, r1
    int_const r1, 4
    int_const r2, 5
    add_int r1, r1, r2
    int_const r2, 6
    add_int r1, r1, r2
    mul_int r0, r0, r1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    string_const r0, "    "
    call_builtin print_string
    string_const r0, "indented line"
    call_builtin print_string
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 1
    return
