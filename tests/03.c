
/*******************************************************************/
/* Cas d'etudes pour le projet du cours d'interpretation abstraite */
/* Ecrit par Olivier Bouissou (olivier.bouissou@cea.fr)            */
/* Le but de ces cas d'etudes est de vous permettre de tester      */
/* votre projet sur des exemples de programmes contenant chacun    */
/* une difficultes que vous devriez rencontrer.                     */
/*******************************************************************/
/* Caclul arithmetique 3. Division.                                */
/* On cherche ici a tester les fonctions arithmetiques des domaine */
/* abstraits. Attention a la maniere de gerer les divisions par 0  */
/*******************************************************************/

int x,y,z,t;

void main() {

  /*!npk x between 2 and 3 */ // x=[2;3] y=? z=? t=?
  /*!npk y between 4 and 5 */ // x=[2;3] y=[4;5] z=? t=?

  t = x-x; // x=[2;3] y=[4;5] z=? t=[-1;1]
  z = (3*y*y+x)/t; // x=[2;3] y=[4;5] z=? t=[-1;1]

}
