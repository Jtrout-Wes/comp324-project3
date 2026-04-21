/*!tests!
 *
 * {
 *      "input":    ["true"],
 *      "output":   ["0", "2"]
 * }
 *
 * {
 *      "input":    ["false"],
 *      "output":   ["1", "3"]
 * }
 *
 */



void main() {
    bool b ;
    fscanf(stdin_lo, "%b", &b) ;

    if (b) fprintf(stdout_lo, "%s\n", "0") ;
    else fprintf(stdout_lo, "%s\n", "1") ;

    if (b) fprintf(stdout_lo, "%s\n", "2") ;

    if (!b) fprintf(stdout_lo, "%s\n", "3") ;

    return ;
}
