/*!tests!
 *
 * {
 *      "input":    [],
 *      "output":   ["1", "0"]
 * }
 *
 */



void main() {
    int x = 0 ;
    
    {
        int x = 1 ;
        fprintf(stdout_lo, "%d\n", x) ;
    }

    fprintf(stdout_lo, "%d\n", x) ;

    return ;

}
