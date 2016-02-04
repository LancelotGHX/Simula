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

	// read input files
	simString singleObj;
	simString singleNum;
	ifstream inputFile("example.txt");
	if (inputFile.is_open())
	{
		while (getline(inputFile, singleObj, '>'))
		{
			cout << singleObj << '\n';
		}
		inputFile.close();
	}
	else {	cout << "Unable to open file\n";	}

	return 0;
}