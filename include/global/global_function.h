#ifndef _SIMULA_GLOBAL_FUNCTION_
#define _SIMULA_GLOBAL_FUNCTION_

#include <stdlib.h>
#include <time.h>

namespace simula {

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

#endif // _SIMULA_GLOBAL_FUNCTION_
