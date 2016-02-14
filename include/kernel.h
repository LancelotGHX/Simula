#ifndef _SIMULA_KERNEL_
#define _SIMULA_KERNEL_

#include "substrate.h"

namespace simula {
	namespace simCuda {

		/** @brief CUDA kernel type **/
		struct kBonePos {	simI1 x, y, id;	};
		/** @brief CUDA kernel type **/
		struct kOneBond 
		{
			simF1 energy;
			simI1 target;
			simSize   size;
			kBonePos* rpos;
		};
		/** @brief CUDA kernel type **/
		struct kBonds {
			simSize   size;
			kOneBond* bond;
		};
		

	
	};	

};

int main_temp();

#endif // _SIMULA_KERNEL_