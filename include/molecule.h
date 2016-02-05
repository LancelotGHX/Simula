#ifndef _SIMULA_MOLECULE_
#define _SIMULA_MOLECULE_

#include "global.h"
#include <string>

using namespace std;
using namespace global;

class Molecule {
private:
  simString name;
  simI2* rpos; //< relative position of sub dots
  simI1  size; //< number of sub dots
  simI1 i;
  simI1 x;
  simI1 y;
  simI1 d;
public:
  /** @brief Constructor **/
  Molecule(simI1 index) : i(index) {}
  
  /** @brief Destructor **/
  ~Molecule() 
  {
    if (rpos) { delete[] rpos; }
  }

  /** @brief properties accessor **/
  simI1  I() { return this->i; }
  simI1  X() { return this->x; }
  simI1  Y() { return this->y; }
  simI1  D() { return this->d; }
  simI1  Size() { return this->size; }
  const simI2* Rpos() { return this->rpos; }

  /** @brief properties setters **/
  void setX(simI1 xpos) { this->x = xpos;  }
  void setY(simI1 ypos) { this->y = ypos;  }
  void setD(simI1 direction) { this->d = direction; }
  void setName(const simString& Name) { this->name = Name; }

  /** @brief set relative component position with a list of integer pairs **/
  void setRpos(simI1 Size, simI1* List) 
  {
    /// set size first
    this->size = Size; 
    this->rpos = new simI2[Size];
    /// set relative positions
    for (simI1 i = 0; i < this->size; ++i) {
      this->rpos[i].x = List[2 * i];
      this->rpos[i].y = List[2 * i + 1];
    }
  }

};

#endif // _SIMULA_MOLECULE_
