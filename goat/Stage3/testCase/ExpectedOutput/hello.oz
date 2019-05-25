    call proc_main
    halt
proc_main:
    string_const r0, "Hello, world!\n"
    call_builtin print_string
    return
