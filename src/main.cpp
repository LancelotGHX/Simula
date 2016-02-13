#include "init.h"
#include "molecule.h"
#include "substrate.h"

using namespace simula; 

int main(int argc, char *argv[])
{
  if (argc != 2) {
#ifndef NDEBUG
    // insuffcient argument number
    std::cerr << "the program need exactly one argument" << std::endl;
#endif
    return -1;
  } else {
    init(argv[1]);
#ifndef NDEBUG
    cout << "successfully initialized program" << endl;
#endif
#ifndef NDEBUG
    cout << "number of molecule types: " << get_molecule_type_size() << endl;
    cout << "number of molecule types: " << get_molecule_size() << endl;
#endif
    // land all points
    simI1 id = 0;
    while (id < get_molecule_size()) {
      simI1 Xpos = randInt(1,subXsize);
      simI1 Ypos = randInt(1,subYsize);
      if ( get_sub().land(get_molecule(id), Xpos, Ypos, 0) ) { 
#ifndef NDEBUG
	get_molecule(id).debug();
#endif
	id++;
      }
    }
#ifndef NDEBUG
    cout << "successfully landed all molecules on thr substrate" << endl;
#endif
    // output substrate to file
    get_sub().print("output.txt");
#ifndef NDEBUG
    cout << "successfully printed substrate" << endl;
#endif
  }
  // clean up
  clean_sub();
  return 0;
}
