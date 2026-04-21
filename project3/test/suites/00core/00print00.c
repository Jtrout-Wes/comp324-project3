/*!tests!
 *
 * {
 *      "input":    [],
 *      "output":   [
 *          "5",
 *          "true",
 *          "false",
 *          "Hello, world!"
 *      ]
 * }
 */



void main() {
    fprintf(stdout_lo, "%d\n", 5) ;
    fprintf(stdout_lo, "%b\n", true) ;
    fprintf(stdout_lo, "%b\n", false) ;
    fprintf(stdout_lo, "%s\n", "Hello, world!") ;

    return ;

}
