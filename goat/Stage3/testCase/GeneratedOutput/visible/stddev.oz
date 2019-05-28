    call proc_main
    halt
proc_main:
    push_stack_frame 6
  # init variables
    int_const r0, 0
  # init variable: count
    store 0, r0
  # init variable: done
    store 1, r0
  # init variable: mean
    store 2, r0
  # init variable: sum
    store 3, r0
  # init variable: sumsq
    store 4, r0
  # initialise variable x
    store 5, r0
    string_const r0, "Type a list of floating point numbers, ending with a negative number\n"
    call_builtin print_string
    call_builtin read_real
    store 5, r0
  # Assign statement for variable done
    load r0, 5
    int_const r1, 0
    int_to_real r1, r1
    cmp_lt_real r0, r0, r1
    store 1, r0
main_4_a:
    load r0, 1
    not r0, r0
    branch_on_true r0, main_4_b
    branch_uncond main_4_c
main_4_b:
  # Assign statement for variable count
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
  # Assign statement for variable sum
    load r0, 3
    load r1, 5
    add_real r0, r0, r1
    store 3, r0
  # Assign statement for variable sumsq
    load r0, 4
    load r1, 5
    load r2, 5
    mul_real r1, r1, r2
    add_real r0, r0, r1
    store 4, r0
    call_builtin read_real
    store 5, r0
  # Assign statement for variable done
    load r0, 5
    int_const r1, 0
    int_to_real r1, r1
    cmp_lt_real r0, r0, r1
    store 1, r0
    branch_uncond main_4_a
main_4_c:
  # Assign statement for variable mean
    load r0, 3
    load r1, 0
    int_to_real r1, r1
    div_real r0, r0, r1
    store 2, r0
    string_const r0, "The mean is "
    call_builtin print_string
    load r0, 2
    call_builtin print_real
    string_const r0, "\n"
    call_builtin print_string
    string_const r0, "The variance is "
    call_builtin print_string
    load r0, 4
    load r1, 0
    int_to_real r1, r1
    div_real r0, r0, r1
    load r1, 2
    load r2, 2
    mul_real r1, r1, r2
    sub_real r0, r0, r1
    call_builtin print_real
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 6
    return
