#include "global.h"
#include <string>

using namespace std;
using namespace global;

class Molecule {
private:
	simString name;
	simI1  size;
	simI2* rpos;

	simI1 id;
	simI1 x;
	simI1 y;
	simI1 dir;

public:
	~Molecule() 
	{
		if (rpos) { delete[] rpos; }
	}

	// properties accessor
	simI1 id() { return this->id; }
	simI1 x() { return this->x; }
	simI1 y() { return this->y; }
	simI1 dir() { return this->dir; }

	// properties setter
	void setX(simI1 xPos) { this->x = xPos; }
	void setY(simI1 yPos) { this->y = yPos; }
	void setDir(simI1 direction) { this->dir = direction; }

	void setName(const simString& Name) { this->name = Name; }
	void setSize(simI1 Size) 
	{ 
		this->size = Size; 
		this->rpos = new simI2[Size];
	}
	void setRpos(simI1* List) 
	{
		for (simI1 i = 0; i < this->size; ++i)
		{
			this->rpos[i].x = List[2 * i];
			this->rpos[i].y = List[2 * i + 1];
		}
	}
};