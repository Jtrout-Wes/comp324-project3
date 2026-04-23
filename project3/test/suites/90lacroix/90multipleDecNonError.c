/*!tests!
 *
 * {
 *    "input":      [],
 *    "output":     ["3"]
 * }
 *
 */




void main() {
    int x = 3;
    {
        int x = 2;
    }

    fprintf(stdout, "%d\n", x);
    return; 
}
