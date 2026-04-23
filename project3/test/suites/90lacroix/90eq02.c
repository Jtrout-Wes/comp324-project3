/*!tests!
 *
 * {
 *    "input":      ["false", "false"],
 *    "output":  ["true"]
 * }
 *
 */

void main() {
    bool n ;
    fscanf(stdin, "%b", &n) ;
    bool m ;
    fscanf(stdin, "%b", &m) ;
    
    fprintf(stdout, "%b", n==m);


    return ;
}