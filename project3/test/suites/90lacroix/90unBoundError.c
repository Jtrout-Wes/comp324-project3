/*!tests!
 *
 * {
 *  "input":        [],
 *  "exception":    "UnboundVariable"
 * }
 *
 */



void main() {
    {
        fprintf(stdout, "%d\n", y);
        int y = 3;
    }
    return ;
}
