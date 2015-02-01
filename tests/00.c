/*******************************************************************/
/* Cas d'etudes pour le projet du cours d'interpratation abstraite */
/* Ecrit par Olivier Bouissou (olivier.bouissou@cea.fr)            */
/* Le but de ces cas d'etudes est de vous permettre de tester      */
/* votre projet sur des exemples de programmes contenant chacun    */
/* une difficulte que vous devriez rencontrer.                     */
/*******************************************************************/
/* Caclul arithmetique 1.                                          */
/* On cherche ici a tester les fonctions arithmetiques des domaines */
/* abstraits.                                                      */
/*******************************************************************/

int a, b, c, d, e, f;

void main() {

  a = 23; // a=[23;23] b=? c=? d=? e=? f=?
  b = 5; // a=[23;23] b=[5;5] c=? d=? e=? f=?

  c = a + b; // a=[23;23] b=[5;5] c=[28;28] d=? e=? f=?
  d = a + c; // a=[23;23] b=[5;5] c=[28;28] d=[51;51] e=? f=?
  e = d + c + f; // a=[23;23] b=[5;5] c=[28;28] d=[51;51] e=? f=?

}
