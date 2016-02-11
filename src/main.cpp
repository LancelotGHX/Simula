#include "global.h"
#include "molecule_type.h"
#include "molecule.h"
#include "substrate.h"
#include <iostream>
#include <fstream>

using namespace std;
using namespace global; 

int main() 
{
  initRandSeed();

  //--- build a substrate
  Substrate sub(subYsize, subYsize);

  cout << "successfully created substrate" << endl;

  //--- build a molecule
  Molecule_Type tp;
  tp.set_name("TPyP");
  tp.set_id(1);
  tp.set_rpos(5,_list_);
  tp.set_amount(100);

  cout << "successfully created substratea molecule type" << endl;

  //--- build molecules
  Molecule* mlist[tp.amount()];
  for (simI1 id = 0; id < tp.amount();  ++id) {
    mlist[id] = new Molecule(&tp, id);
  }

  cout << "successfully created all molecules" << endl;

  //--- land all points
  simI1 id = 0;
  cout << tp.amount() << endl;
  while (id < tp.amount()) {
    
    simI1 Xpos = randInt(1,subXsize);
    simI1 Ypos = randInt(1,subYsize);
    if ( sub.land(*mlist[id], Xpos, Ypos, 0) ) { ++id; }

  }

  cout << "successfully landed all molecules on thr substrate" << endl;
 
  //--- output substrate to file
  sub.print("output.txt");

  cout << "successfully printed substrate" << endl;

  return 0;

}

