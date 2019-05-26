    call proc_main
    halt
proc_main:
    int_const r0, 1
    branch_on_true r0, main_1_a
    branch_uncond main_1_b
main_1_a:
    string_const r0, "true 1\n"
    call_builtin print_string
main_1_b:
    int_const r0, 0
    branch_on_true r0, main_2_a
    branch_uncond main_2_b
main_2_a:
    string_const r0, "#####################\n"
    call_builtin print_string
main_2_b:
    int_const r0, 1
    int_const r1, 0
    or r0, r0, r1
    branch_on_true r0, main_3_a
    branch_uncond main_3_b
main_3_a:
    string_const r0, "true 2\n"
    call_builtin print_string
main_3_b:
    int_const r0, 1
    int_const r1, 1
    or r0, r0, r1
    branch_on_true r0, main_4_a
    branch_uncond main_4_b
main_4_a:
    string_const r0, "true 3\n"
    call_builtin print_string
main_4_b:
    int_const r0, 0
    int_const r1, 1
    or r0, r0, r1
    branch_on_true r0, main_5_a
    branch_uncond main_5_b
main_5_a:
    string_const r0, "true 4\n"
    call_builtin print_string
main_5_b:
    int_const r0, 0
    int_const r1, 0
    or r0, r0, r1
    branch_on_true r0, main_6_a
    branch_uncond main_6_b
main_6_a:
    string_const r0, "#############\n"
    call_builtin print_string
main_6_b:
    int_const r0, 1
    int_const r1, 0
    and r0, r0, r1
    branch_on_true r0, main_7_a
    branch_uncond main_7_b
main_7_a:
    string_const r0, "*****************\n"
    call_builtin print_string
main_7_b:
    int_const r0, 1
    int_const r1, 1
    and r0, r0, r1
    branch_on_true r0, main_8_a
    branch_uncond main_8_b
main_8_a:
    string_const r0, "true 5\n"
    call_builtin print_string
    int_const r0, 1
    int_const r1, 0
    or r0, r0, r1
    branch_on_true r0, main_8_0_2_a
    branch_uncond main_8_0_2_b
main_8_0_2_a:
    string_const r0, "true 6\n"
    call_builtin print_string
main_8_0_2_b:
main_8_b:
    int_const r0, 0
    int_const r1, 1
    and r0, r0, r1
    branch_on_true r0, main_9_a
    branch_uncond main_9_b
main_9_a:
    string_const r0, "))))))))))))))))))))00\n"
    call_builtin print_string
main_9_b:
    int_const r0, 0
    int_const r1, 0
    and r0, r0, r1
    branch_on_true r0, main_10_a
    branch_uncond main_10_b
main_10_a:
    string_const r0, "&&&&&&&&&&&&&&&&&&&&&\n"
    call_builtin print_string
main_10_b:
    int_const r0, 0
    branch_on_false r0, main_11_a
    string_const r0, "#########l#2222222222\n"
    call_builtin print_string
    branch_uncond main_11_b
main_11_a:
    string_const r0, "true 7\n"
    call_builtin print_string
main_11_b:
    int_const r0, 1
    branch_on_false r0, main_12_a
    string_const r0, "true 8\n"
    call_builtin print_string
    branch_uncond main_12_b
main_12_a:
    string_const r0, "#####4444444444444444\n"
    call_builtin print_string
main_12_b:
main_13_a:
    int_const r0, 0
    branch_on_true r0, main_13_b
    branch_uncond main_13_c
main_13_b:
    string_const r0, "while false do44444444444444\n"
    call_builtin print_string
    branch_uncond main_13_a
main_13_c:
    int_const r0, 1
    not r0, r0
    branch_on_true r0, main_14_a
    branch_uncond main_14_b
main_14_a:
    string_const r0, "!true (((((())))))\n"
    call_builtin print_string
main_14_b:
    int_const r0, 0
    not r0, r0
    branch_on_true r0, main_15_a
    branch_uncond main_15_b
main_15_a:
    string_const r0, "true 9\n"
    call_builtin print_string
main_15_b:
    int_const r0, 1
    not r0, r0
    int_const r1, 0
    or r0, r0, r1
    branch_on_true r0, main_16_a
    branch_uncond main_16_b
main_16_a:
    string_const r0, "!true || falsesssssssssssss\n"
    call_builtin print_string
main_16_b:
    int_const r0, 1
    not r0, r0
    int_const r1, 1
    not r1, r1
    or r0, r0, r1
    branch_on_true r0, main_17_a
    branch_uncond main_17_b
main_17_a:
    string_const r0, "!true || !true###############\n"
    call_builtin print_string
main_17_b:
    int_const r0, 0
    not r0, r0
    int_const r1, 1
    or r0, r0, r1
    branch_on_true r0, main_18_a
    branch_uncond main_18_b
main_18_a:
    string_const r0, "true 10\n"
    call_builtin print_string
main_18_b:
    int_const r0, 0
    int_const r1, 0
    not r1, r1
    or r0, r0, r1
    branch_on_true r0, main_19_a
    branch_uncond main_19_b
main_19_a:
    string_const r0, "true 11\n"
    call_builtin print_string
main_19_b:
    int_const r0, 1
    int_const r1, 0
    not r1, r1
    and r0, r0, r1
    branch_on_true r0, main_20_a
    branch_uncond main_20_b
main_20_a:
    string_const r0, "true 12\n"
    call_builtin print_string
    int_const r0, 1
    not r0, r0
    int_const r1, 0
    or r0, r0, r1
    branch_on_false r0, main_20_0_2_a
    string_const r0, "11111111111111111111\n"
    call_builtin print_string
    branch_uncond main_20_0_2_b
main_20_0_2_a:
    string_const r0, "true 13\n"
    call_builtin print_string
main_20_0_2_b:
main_20_b:
    int_const r0, 1
    not r0, r0
    int_const r1, 1
    and r0, r0, r1
    branch_on_true r0, main_21_a
    branch_uncond main_21_b
main_21_a:
    string_const r0, "rsfffagsdfasdfad\n"
    call_builtin print_string
main_21_b:
    int_const r0, 0
    not r0, r0
    int_const r1, 1
    and r0, r0, r1
    branch_on_true r0, main_22_a
    branch_uncond main_22_b
main_22_a:
    string_const r0, "true 14\n"
    call_builtin print_string
main_22_b:
    int_const r0, 0
    not r0, r0
    int_const r1, 0
    not r1, r1
    and r0, r0, r1
    branch_on_true r0, main_23_a
    branch_uncond main_23_b
main_23_a:
    string_const r0, "true 15\n"
    call_builtin print_string
main_23_b:
    int_const r0, 0
    not r0, r0
    branch_on_false r0, main_24_a
    string_const r0, "true 16\n"
    call_builtin print_string
    branch_uncond main_24_b
main_24_a:
    string_const r0, "#########l#2222222222\n"
    call_builtin print_string
main_24_b:
    int_const r0, 1
    not r0, r0
    branch_on_false r0, main_25_a
    string_const r0, "#####4444444444444444\n"
    call_builtin print_string
    branch_uncond main_25_b
main_25_a:
    string_const r0, "true 17\n"
    call_builtin print_string
main_25_b:
main_26_a:
    int_const r0, 0
    branch_on_true r0, main_26_b
    branch_uncond main_26_c
main_26_b:
    string_const r0, "while false do44444444444444\n"
    call_builtin print_string
    branch_uncond main_26_a
main_26_c:
    return
