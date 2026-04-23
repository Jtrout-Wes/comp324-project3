/*!tests!
 *
 * {
 *      "input":      ["6", "0"],
 *      "exception":    "Division_by_zero"
 * }
 *
 */

void main() {
    int n ;
    fscanf(stdin, "%d", &n) ;

    int m ;
    fscanf(stdin, "%d", &m) ;

    fprintf(stdout, "%d", n/m);


    return ;
}