/*!tests!
 *
 * {
 *      "input":    ["5", "true", "hello"],
 *      "output":   ["5", "true", "hello"]
 * }
 *
 * {
 *      "input":    ["-7", "false", "fred"],
 *      "output":   ["-7", "false", "fred"]
 * }
 *
 */



void main() {
    int x ;
    bool b ;
    char* s ;

    fscanf(stdin_lo, "%d", &x) ;
    fscanf(stdin_lo, "%b", &b) ;
    fscanf(stdin_lo, "%s", &s) ;

    fprintf(stdout_lo, "%d\n", x) ;
    fprintf(stdout_lo, "%b\n", b) ;
    fprintf(stdout_lo, "%s\n", s) ;

    return ;
}

