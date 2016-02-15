///////////////////////////////////////////////////////////////////////////////
//
// CUDA function definitions
//
///////////////////////////////////////////////////////////////////////////////
#include "kernel.h"

#include "cuda.h"
#include "cuda_runtime.h"
#include "device_launch_parameters.h"

#include <stdio.h>

///////////////////////////////////////////////////////////////////////////////
// error check function
#define cuda_safe_call(ans) { simCudaAssert((ans), __FILE__, __LINE__); }
void simCudaAssert(cudaError_t code, const char *file, int line, bool abort = true)
{
	if (code != cudaSuccess) {
		fprintf(stderr, "CUDA assert: %s %s %d\n", cudaGetErrorString(code), file, line);
		if (abort) exit(code);
	}
}

///////////////////////////////////////////////////////////////////////////////
// used namespace
using namespace simula;
using namespace simCuda;

///////////////////////////////////////////////////////////////////////////////
// local namespace
namespace {

	/////////////////////////////////////////////////////////////////////////////
	// device variable
	kMolecule* mlist_d;
	kMolecule* mlist_h;

	/////////////////////////////////////////////////////////////////////////////
	// deep copy molecule list to device
	cudaError_t mlist_to_dev(simBool free_flag = false)
	{
		// allocate memory
		simSize msize = get_molecule_size();
		mlist_h = (kMolecule*)malloc(msize * sizeof(kMolecule));
		cuda_safe_call(cudaMalloc((void**)&mlist_d, msize * sizeof(kMolecule)));
		// deep copy molecule data into C struct
		for (simI1 i = 0; i < msize; ++i) {
			mlist_h[i].x = get_molecule(i).x();
			mlist_h[i].y = get_molecule(i).y();
			mlist_h[i].d = get_molecule(i).d();
			mlist_h[i].i = get_molecule(i).self_id();
			mlist_h[i].t = get_molecule(i).type_id();
		}
		// copy data into device
		cudaError_t err = cudaMemcpy(mlist_d, mlist_h, msize * sizeof(kMolecule), cudaMemcpyHostToDevice);
		// free mlist_h
		if (free_flag) {
			free(mlist_h);
		}
		return err;
	}

	/////////////////////////////////////////////////////////////////////////////
	// deep copy molecule list to host
	cudaError_t mlist_to_host()
	{
		// allocate memory
		simSize msize = get_molecule_size();
		// copy data back to host
		cudaError_t err = cudaMemcpy(mlist_h, mlist_d, msize * sizeof(kMolecule), cudaMemcpyDeviceToHost);
		// deep copy back to struct
		if (err = cudaSuccess) {
			for (simI1 i = 0; i < msize; ++i) {
				get_molecule(i).set_x(mlist_h[i].x);
				get_molecule(i).set_y(mlist_h[i].y);
				get_molecule(i).set_d(mlist_h[i].d);
			}
		}
		return err;
	}
	/////////////////////////////////////////////////////////////////////////////
	// kernel function
	// ==> to check if its neighboring points are occupied
	__global__ void addKernel(kMolecule* mlist, simI1* r, simI1 size)
	{
		simI1 idx = threadIdx.x;
		simI1 sx = mlist[idx].x, sy = mlist[idx].y;

		r[idx * 4 + 0] = 1; 
		r[idx * 4 + 1] = 1;
		r[idx * 4 + 2] = 1; 
		r[idx * 4 + 3] = 1;
		
		for (simI1 i = 0; i < size; ++i) {
			simI1 tx = mlist[i].x, ty = mlist[i].y;
			if (sx + 1 == tx && sy == ty) { r[idx * 4 + 0] = 0; }
			if (sx == tx && sy + 1 == ty) { r[idx * 4 + 1] = 0; }
			if (sx - 1 == tx && sy == ty) { r[idx * 4 + 2] = 0; }
			if (sx == tx && sy - 1 == ty)	{ r[idx * 4 + 3] = 0; }
		}
		
	}

	simI1* result_d;
	simI1* result_h;

	// Helper function for using CUDA to add vectors in parallel.
	void funcCuda()
	{
		simI1 msize = get_molecule_size();

		// Choose which GPU to run on, change this on a multi-GPU system.
		cuda_safe_call(cudaSetDevice(0));

		// Allocate constant memory
		result_h = (simI1*)malloc(4 * msize * sizeof(simI1));
		cuda_safe_call(cudaMalloc((void**)&result_d, 4 * msize * sizeof(simI1)));
		
		// Copy data
		cuda_safe_call(mlist_to_dev(true));

		// Launch a kernel on the GPU with one thread for each element.
		addKernel <<< 1, msize >>> (mlist_d, result_d, msize);

		// Check for any errors launching the kernel
		cuda_safe_call(cudaGetLastError());

		// Check any errors encountered during the launch.
		cuda_safe_call(cudaDeviceSynchronize());

		// Copy output vector from GPU buffer to host memory.
		cuda_safe_call(cudaMemcpy(result_h, result_d, 4 * msize * sizeof(simI1), cudaMemcpyDeviceToHost));
	}

};

int simCuda::main_temp()
{
	// Test overlap.
	funcCuda();
	// print result
	for (simI1 i = 0; i < get_molecule_size(); ++i) {
		printf("{%d,%d,%d,%d}\n", result_h[4 * i + 0], result_h[4 * i + 1], result_h[4 * i + 2], result_h[4 * i + 3]);
	}

	// for tracing tools such as Nsight and Visual Profiler
	cuda_safe_call(cudaDeviceReset());
	return 0;
}