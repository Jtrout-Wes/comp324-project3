/*!tests!
 *
 * {
 *    "input":      [],
 *    "exception":  "TypeError"
 * }
 *
 */

void main() {
    bool n = false;

    n = -n;

    fprintf(stdout, "%b", n);


    return ;
}