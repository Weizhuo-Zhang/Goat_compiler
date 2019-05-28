    call proc_main
    halt
proc_main:
    string_const r0, "     Hello darlings!"
    call_builtin print_string
    string_const r0, "\n"
    call_builtin print_string
    string_const r0, " "
    call_builtin print_string
    string_const r0, "\n"
    call_builtin print_string
    string_const r0, "QUIZ: As a float, nine is: "
    call_builtin print_string
    real_const r0, 3.0
    int_const r1, 3
    int_const r2, 2
    mul_int r1, r1, r2
    int_to_real r1, r1
    add_real r0, r0, r1
    call_builtin print_real
    string_const r0, "\n"
    call_builtin print_string
    int_const r0, 9
    int_const r1, 4
    int_const r2, 5
    add_int r1, r1, r2
    cmp_eq_int r0, r0, r1
    int_const r1, 1
    and r0, r0, r1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    return
