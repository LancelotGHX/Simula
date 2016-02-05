#ifndef _SIMULA_SUBSTRATE_
#define _SIMULA_SUBSTRATE_

#include "global.h"
#include "molecule.h"
#include <iostream>

using namespace global;
using namespace std;

class Substrate {
private:
  const simI1 xsize;
  const simI1 ysize;
  simI1** sub;
public:
  /** @brief Constructor **/
  Substrate(simI1 Xsize, simI1 Ysize)
    : xsize(Xsize), ysize(Ysize) 
  {
    /// initialize dynamic 2D array
    this->sub = new simI1*[this->xsize];
    for (simI1 i = 0; i < this->xsize; ++i) {
      this->sub[i] = new simI1[this->ysize];
    }
    /// initialize array with value 0
    this->init();
  }
  
  /** @brief initialize substrate **/
  void init()
  {
    for (simI1 i = 0; i < this->xsize; ++i) {
      for (simI1 j = 0; j < this->ysize; ++j) {
	this->sub[i][j] = 0;
      }
    }
  }

  simBool land(Molecule& m, simI1 xc, simI1 yc)
  {
    m.setX(xc);
    m.setY(yc);
    if ( isRegionEmpty(m.Rpos(), m.Size(), xc, yc) ) {
      for (simI1 i = 0; i < m.Size(); ++i) {
	simI1 x = xc + m.Rpos()[i].x;
	simI1 y = yc + m.Rpos()[i].y;
	this->sub[x][y] = m.I();
      }
      return true;
    } else {
      return false;
    }
  }

  simBool isEmpty(simI1 x, simI1 y) 
  {
    return this->sub[x][y] == 0;
  }

  simBool isRegionEmpty(const simI2* rpos, simI1 len, simI1 xc, simI1 yc)
  {
    simBool empty = true;
    for (simI1 i = 0; i < len; ++i) {
      simI1 x = xc + rpos[i].x;
      simI1 y = yc + rpos[i].y;
      if ( !isEmpty(x,y) ) { empty = false; break; }
    }
    return empty;
  }

  void print() 
  {
    for (simI1 i = 0; i < this->xsize; ++i) {
      for (simI1 j = 0; j < this->ysize; ++j) {
	cout << this->sub[i][j] << " ";
      }
      cout << endl;
    }
  }

};

#endif // _SIMULA_SUBSTRATE_
