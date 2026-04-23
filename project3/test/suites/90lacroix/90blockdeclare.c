/*!tests!
 *
 * {
 *    "input":      [],
 *    "output":  [
 *          "9",
 *          "4"
 * ]
 * }
 *
 */



void main() {
    int x ;
    x = 4;
    {
    int x ;
    x = 9;
    fprintf(stdout, "%d\n", x);
    }
    fprintf(stdout, "%d\n", x) ;

    return ;
}