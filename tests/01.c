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

int a, b, c, d, e;

void main() {

  /*!npk a between 10 and 100*/ // a=[10;100] b=? c=? d=? e=?
  /*!npk b between 0 and 47*/// a=[10;100] b=[0;47] c=? d=? e=?

  c = a + b; // a=[10;100] b=[0;47] c=[10;147] d=? e=?
  d = a * c; // a=[10;100] b=[0;47] c=[10;147] d=[100;14700] e=?
  e = d - c; // a=[10;100] b=[0;47] c=[10;147] d=[100;14700] e=[-47;14690]

}
