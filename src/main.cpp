#include "init.h"
#include "substrate.h"
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

    init(argv[1]);
    Substrate sub(subYsize, subYsize);
    cout << "successfully initialized program" << endl;

    // land all points
    cout << "number of molecule types: " << get_molecule_type_size() << endl;
    cout << "number of molecule types: " << get_molecule_size() << endl;
    
    simI1 id = 0;
    while (id < get_molecule_size()) {
      simI1 Xpos = randInt(1,subXsize);
      simI1 Ypos = randInt(1,subYsize);
      if ( sub.land(get_molecule(id), Xpos, Ypos, 0) ) { 
	//get_molecule(id).debug();
	id++;
      }      
    }
    
    cout << "successfully landed all molecules on thr substrate" << endl;
    // cout << sub << endl;
    // output substrate to file
    sub.print("output.txt");
    cout << "successfully printed substrate" << endl;

  }

  return 0;

}

