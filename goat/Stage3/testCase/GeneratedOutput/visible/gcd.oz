    call proc_main
    halt
proc_main:
    push_stack_frame 5
  # init variables
    int_const r0, 0
  # init variable: quotient
    store 0, r0
  # init variable: remainder
    store 1, r0
  # init variable: temp
    store 2, r0
  # init variable: x
    store 3, r0
  # initialise variable y
    store 4, r0
    string_const r0, "Input two positive integers: "
    call_builtin print_string
    call_builtin read_int
    store 3, r0
    call_builtin read_int
    store 4, r0
    string_const r0, "\n"
    call_builtin print_string
    load r0, 3
    load r1, 4
    cmp_lt_int r0, r0, r1
    branch_on_true r0, main_5_a
    branch_uncond main_5_b
main_5_a:
  # Assign statement for variable temp
    load r0, 3
    store 2, r0
  # Assign statement for variable x
    load r0, 4
    store 3, r0
  # Assign statement for variable y
    load r0, 2
    store 4, r0
main_5_b:
    string_const r0, "The gcd of "
    call_builtin print_string
    load r0, 3
    call_builtin print_int
    string_const r0, " and "
    call_builtin print_string
    load r0, 4
    call_builtin print_int
    string_const r0, " is "
    call_builtin print_string
  # Assign statement for variable quotient
    load r0, 3
    load r1, 4
    div_int r0, r0, r1
    store 0, r0
  # Assign statement for variable remainder
    load r0, 3
    load r1, 0
    load r2, 4
    mul_int r1, r1, r2
    sub_int r0, r0, r1
    store 1, r0
main_13_a:
    load r0, 1
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    branch_on_true r0, main_13_b
    branch_uncond main_13_c
main_13_b:
  # Assign statement for variable x
    load r0, 4
    store 3, r0
  # Assign statement for variable y
    load r0, 1
    store 4, r0
  # Assign statement for variable quotient
    load r0, 3
    load r1, 4
    div_int r0, r0, r1
    store 0, r0
  # Assign statement for variable remainder
    load r0, 3
    load r1, 0
    load r2, 4
    mul_int r1, r1, r2
    sub_int r0, r0, r1
    store 1, r0
    branch_uncond main_13_a
main_13_c:
    load r0, 4
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 5
    return
