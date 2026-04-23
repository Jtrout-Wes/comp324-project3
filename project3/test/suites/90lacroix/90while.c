/*!tests!
 *
 * {
 *      "input":    [],
 *      "output":   ["6561"]
 * }
 *
 */



void main() {
    int x = 0 ;
    int sum = 3; 

    while (x < 3){
        sum = sum * sum;
        x = x + 1;
    }
    fprintf(stdout, "%d\n", sum);
    
    return;
}