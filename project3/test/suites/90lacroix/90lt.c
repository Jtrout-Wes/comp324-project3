/*!tests!
 *
 * {
 *    "input":      ["4", "4"],
 *    "output":  ["false"]
 * }
 *
 */

void main() {
    int n ;
    fscanf(stdin, "%d", &n) ;

    int m ;
    fscanf(stdin, "%d", &m) ;

    fprintf(stdout, "%b", n<m);


    return ;
}