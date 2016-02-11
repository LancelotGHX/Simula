#ifndef _SIMULA_GLOBAL_
#define _SIMULA_GLOBAL_

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string>

using namespace std;

namespace global
{
  /** @brief required types */
  typedef string simString;
  typedef bool   simBool;
  typedef char   simChar;
  typedef int    simI1;
  typedef double simF1;
  struct simI2 { simI1 x, y; };
  struct simF2 { simF1 x, y; };
  struct simI3 { simI1 x, y, z; };
  struct simF3 { simF1 x, y, z; };

  /** @brief global variables */
  const simI1 subYsize = 100;
  const simI1 subXsize = 100;

  simI1 _list_[10] = { 0,0,1,0,0,1,-1,0,0,-1 };

  /** @brief initialize random seed */
  void initRandSeed()
  {
    srand(time(NULL));
  }

  /**
   * @brief generate integer random number within [lowerBound, upperBound)
   * @param lowerBound
   * @param upperBound
   */
  simI1 randInt(simI1 lowerBound, simI1 upperBound)
  {
    return static_cast<simI1>(rand()) % (upperBound - lowerBound) + lowerBound;
  }

  /**
   * @brief positive modulo
   * @param x Input value
   * @param n Base integer
   **/
  simI1 pmod(simI1 x, simI1 n)
  {
    return (x % n + n) % n;
  }

};

#endif // _SIMULA_GLOBAL_
