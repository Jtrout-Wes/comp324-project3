/*!tests!
 *
 * {
 *      "input":    [],
 *      "output":   ["0", "2"]
 * }
 *
 */



void main() {
    int x = 0 ;

    while (x == 0) {
        fprintf(stdout_lo, "%d\n", 0) ;
        x = 1 ;
    }

    while (false) {
        fprintf(stdout_lo, "%d\n", 1) ;
    }

    fprintf(stdout_lo, "%d\n", 2) ;

    return ;
}
