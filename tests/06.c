/*******************************************************************/
/* Cas d'etudes pour le projet du cours d'interpretation abstraite */
/* Ecrit par Olivier Bouissou (olivier.bouissou@cea.fr)            */
/* Le but de ces cas d'etudes est de vous permettre de tester      */
/* votre projet sur des exemples de programmes contenant chacun    */
/* une difficulte que vous devriez rencontrer.                     */
/*******************************************************************/
/* Branchement conditionnel 1.                                     */
/* On cherche ici a tester les fonctions de transfert liees aux    */
/* test abstraits.                                                 */
/*******************************************************************/

int i,j;
int x,y,z;

void main() {

  /*!npk x between 1 and 5 */ //i=? j=? x=[1;5] y=? z=?

  z = 1;//i=? j=? x=[1;5] y=? z=[1;1]
  j = 1;//i=? j=[1;1] x=[1;5] y=? z=[1;1]

  if (i < 0)
    j = 15/i;//i=? j=? x=[1;5] y=? z=[1;1]

  if (x > 0)
  {
    y = 1/x;//i=? j=? x=[1;5] y=[0;1] z=[1;1]
    z = x;//i=? j=? x=[1;5] y=[0;1] z=[1;5]
  }

}
