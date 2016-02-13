#include "global/global_function.h"

#include <cstdlib> // srand, rand
#include <ctime>   // time

using namespace simula;

/** @brief initialize random seed */
void simula::initRandSeed()
{
  srand(time(NULL));
}
