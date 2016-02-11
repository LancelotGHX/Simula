#include "global.h"
#include "molecule.h"
#include "substrate.h"
#include "init.h"
#include "reader_xml.h"
#include <iostream>
#include <fstream>

using namespace simula; 
using namespace std;

int main(int argc, char *argv[])
{
  if (argc != 2) {

    // error handling
    std::cerr << "the program need exactly one argument" << std::endl;
    return -1;

  } else {
    using namespace reader;

    parse_input(argv[1]);
  }

  initRandSeed();

  //--- build a substrate
  Substrate sub(subYsize, subYsize);

  cout << "successfully created substrate" << endl;

  //--- build a molecule
  Molecule_Type tp;
  tp.set_name("TPyP");
  tp.set_id(1);
  tp.set_rpos(5,_list_);
  tp.set_amount(10);

  cout << "successfully created substratea molecule type" << endl;

  //--- build molecules
  init(tp);

  cout << "successfully created all molecules" << endl;

  //--- land all points
  simI1 id = 0;
  while (id < tp.amount()) {
    
    simI1 Xpos = randInt(1,subXsize);
    simI1 Ypos = randInt(1,subYsize);
    if ( sub.land(rcd[id], Xpos, Ypos, 0) ) { ++id; }

  }

  cout << "successfully landed all molecules on thr substrate" << endl;
 
  //--- output substrate to file
  sub.print("output.txt");

  cout << "successfully printed substrate" << endl;

  return 0;

}

