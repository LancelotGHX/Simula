#ifndef _SIMULA_GLOBAL_
#define _SIMULA_GLOBAL_

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string>

using namespace std;

// constants
namespace global
{
	// required types
	typedef bool         simBool;
	typedef string       simString;
	typedef unsigned int simI1;
	typedef double       simF1;
	struct simI2 { simI1 x, y; };
	struct simF2 { simF1 x, y; };
	struct simI3 { simI1 x, y, z; };
	struct simF3 { simF1 x, y, z; };

	// global variables
	const simI1 subYsize = 100;
	const simI1 subXsize = 100;
	simI1 sub[subXsize][subYsize];

	/** @brief initialize random seed */
	void initRandSeed()
	{
		// initialize random seed
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
};

#endif // _SIMULA_GLOBAL_