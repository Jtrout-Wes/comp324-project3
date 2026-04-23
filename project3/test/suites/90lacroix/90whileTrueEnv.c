/*!tests!
 *
 * {
 *      "input":    [],
 *      "output":   ["3"]
 * }
 *
 */


void main() {
    int x = 0;
    while (x < 3){
        x = x + 1;
    }

    fprintf(stdout, "%d\n", x);
    return;
}