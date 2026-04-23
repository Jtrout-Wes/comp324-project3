/*!tests!
 *
 * {
 *      "input":    [],
 *      "output":   ["81"]
 * }
 *
 */



void main() {
    int x = 0 ;
    int sum = 3; 

    while (x < 3){
        sum = sum * 3;
        x = x + 1;
    }
    fprintf(stdout, "%d\n", sum);
    
    return;
}