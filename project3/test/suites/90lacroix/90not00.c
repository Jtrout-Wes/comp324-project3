/*!tests!
 *
 * {
 *    "input":      [],
 *    "output":  ["true"]
 * }
 *
 */

void main() {
    bool b = false;
    b = !(b);

    fprintf(stdout, "%b", b);


    return ;
}