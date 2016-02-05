#include "global.h"
#include <iostream>
#include <fstream>

using namespace std;
using namespace global; 

int main() 
{
	// initialize substrate
	for (simI1 i = 0; i < subXsize; ++i) 
	{
		for (simI1 j = 0; j < subYsize; ++j)
		{
			sub[i][j] = 0;
		}
	}

	return 0;
}
