/*!tests!
 *
 * {
 *      "input":        ["true"],
 *      "exception":    "NSU_Error"
 * }
 *
 * {
 *      "input":        ["false"],
 *      "exception":    "NSU_Error"
 * }
 *
 */

/* Make sure that `return` is only executed in a low security context.
 */

int f(bool b) {
    if (b) return true ;
    else return false ;
}

void main() {
    bool x ;
    fscanf(stdin_hi, "%b", &x) ;
    f(x) ;

    return ;
}
