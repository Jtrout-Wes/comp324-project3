/*!tests!
 *
 * {
 *    "input":      [],
 *    "output":  ["true"]
 * }
 *
 */

void main() {
    bool n = true || false && true;
    fprintf(stdout, "%b", n);
    return;
}