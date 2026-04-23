/*!tests!
 *
 * {
 *    "input":      ["4", "4"],
 *    "output":  ["true"]
 * }
 *
 */

void main() {
    int n ;
    fscanf(stdin, "%d", &n) ;
    int m ;
    fscanf(stdin, "%d", &m) ;
    fprintf(stdout, "%b", n==m);


    return ;
}