/*!tests!
 *
 * {
 *    "input":      [],
 *    "output":  ["5"]
 * }
 *
 */


void main() {
    int x = 3;
    if (true){
        int x = 5;
        if (false){
            fprintf(stdout, "%d\n", x + x);
        }
        else{
            fprintf(stdout, "%d\n", x);
        }
    }

    return ;
}
