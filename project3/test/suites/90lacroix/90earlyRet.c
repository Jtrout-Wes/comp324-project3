/*!tests!
 *
 * {
 *      "input":    [],
 *      "output":   ["9"]
 * }
 *
 */



void main() {
    int x = 0 ;
    int sum = 3; 

    while (x < 3){
        sum = sum * sum;
        x = x + 1;
        if (sum >= 9){
            fprintf(stdout, "%d\n", sum);
            return;
        }
    }
    fprintf(stdout, "%d\n", 1);
    return; 
}
