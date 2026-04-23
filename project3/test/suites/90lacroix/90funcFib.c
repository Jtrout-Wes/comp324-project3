/*!tests!
 *
 * {
 *      "input":    ["7"],
 *      "output":   ["13"]
 * }
 *
 */



int calculate_fibonachi(int n) {
    if (n < 2){
        return n;
    }
    return calculate_fibonachi(n-1) + calculate_fibonachi(n-2);
}

 
void main() {
    int fib_result;
    int fib_n;

    fscanf(stdin, "%d", &fib_n);

    fib_result = calculate_fibonachi(fib_n) ;
    fprintf(stdout, "%d\n", fib_result);
    return ;
}
