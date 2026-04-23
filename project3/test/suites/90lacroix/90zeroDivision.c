/*!tests!
 *
 * {
 *    "input":      ["0", "6"],
 *    "output":  ["0"]
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