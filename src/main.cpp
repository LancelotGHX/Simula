#include "global.h"
#include "molecule.h"
#include "substrate.h"
#include <iostream>
#include <fstream>

using namespace std;
using namespace global; 

int main() 
{
  // build a substrate
  Substrate sub(subYsize, subYsize);

  cout << "here1" << endl;

  // build a molecule
  Molecule* mlist[5];
  for (simI1 i = 0; i < 5; ++i) {
    mlist[i] = new Molecule(i+1);
    mlist[i]->setRpos(5, _list_);
  }

  cout << "here2" << endl;

  // generate random position
  for (simI1 i = 0; i < 5; ++i) {
    
    simI1 Xpos = randInt(1,subXsize);
    simI1 Ypos = randInt(1,subYsize);
    cout << sub.land(*mlist[i], Xpos, Ypos) << endl;

  }

  cout << "here3" << endl;
 
  sub.print();

  return 0;

}

