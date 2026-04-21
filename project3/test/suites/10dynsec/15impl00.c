/*!tests!
 *
 * {
 *      "input":        ["0"],
 *      "exception":    "SecurityError"
 * }
 *
 * {
 *      "input":        ["1"],
 *      "exception":    "SecurityError"
 * }
 *
 */



void main() {
  int x ;
  fscanf(stdin_hi, "%d", &x) ;

  /* Because x is H, both 0 and 1 are evaluated under an H context,
   * and therefore are H, so this should raise Security.
   */
  if (x == 0) fprintf(stdout_lo, "%d\n", 0) ;
  else fprintf(stdout_lo, "%d\n", 1) ;

  return ;
}
