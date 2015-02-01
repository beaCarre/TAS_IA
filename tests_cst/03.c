/*******************************************************************/
/* Cas d'etudes pour le projet du cours d'interpratation abstraite */
/* Ecrit par Olivier Bouissou (olivier.bouissou@cea.fr)            */
/* Le but de ces cas d'etudes est de vous permettre de tester      */
/* votre projet sur des exemples de programmes contenant chacun    */
/* une difficulte que vous devriez rencontrer.                     */
/*******************************************************************/
/* Test du domaine des constantes.                                 */
/*******************************************************************/

int x,y,z;

void main() {

  x=0;
  y=0;

  while (x<100) {
    y=y-3;
    x=x+y;
    y=y+3;
  }

  /*!npk assert y==0*/


  if (y==0)
    z=1;

  if (2<=y) 
    z=0;
  else 
    z=1;

  /*!npk assert z<=0 */
  /* This assertion should be violated */


}

/*/Analysis starts
15: x -> ? y -> ? z -> ? 
16: x -> 0 y -> ? z -> ? 
18: x -> 0 y -> 0 z -> ? 
19: x -> 0 y -> 0 z -> ? 
20: x -> 0 y -> -3 z -> ? 
21: x -> -3 y -> -3 z -> ? 
19: x -> ? y -> 0 z -> ? 
20: x -> ? y -> -3 z -> ? 
20: potential invalid operation: +
21: x -> ? y -> -3 z -> ? 
24: x -> ? y -> 0 z -> ? 
24: assertion violation
27: x -> ? y -> 0 z -> ? 
28: x -> ? y -> 0 z -> ? 
30: x -> ? y -> 0 z -> 1 
31: {}
33: x -> ? y -> 0 z -> 1 
35: x -> ? y -> 0 z -> 1 
35: assertion violation
Final state: x -> ? y -> 0 z -> 1 
*/
