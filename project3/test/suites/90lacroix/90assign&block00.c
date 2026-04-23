/*!tests!
 *
 * {
 *      "input":    [],
 *      "output":   ["8"]
 * }
 *
 */



void main() {
    int x = 3 + 5;
    {
        int x; 
        x = 9;
    }

    fprintf(stdout, "%d\n", x) ;

    return ;

}
