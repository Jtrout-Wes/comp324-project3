/*!tests!
 *
 * {
 *      "input":    [],
 *      "output":   ["4"]
 * }
 *
 */


void main() {
    int x = 4;
    while (x < 4){
        x = x + 1;
    }

    fprintf(stdout, "%d\n", x);
    return;
}