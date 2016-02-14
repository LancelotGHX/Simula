#include "kernel.h"

#include "cuda_runtime.h"
#include "device_launch_parameters.h"

#include <stdio.h>

cudaError_t funcCuda(int *c, const int *a, const int *b, const int* r, unsigned int size);

int main_temp()
{
	const int arraySize = 5;

	const int a[arraySize] = { 1, 2, 3, 4, 5 }; // x position
	const int b[arraySize] = { 1, 1, 1, 1, 1 }; // y position

	int c[arraySize * 4] = { 0 };

	const int r[2] = { 1, 0 };      // constant memory

	// Test overlap.
	cudaError_t cudaStatus = funcCuda(c, a, b, r, arraySize);
	if (cudaStatus != cudaSuccess) {
		fprintf(stderr, "function failed!");
		return 1;
	}

	printf("#1 {1,2,3,4} = {%d,%d,%d,%d}\n", c[0], c[1], c[2], c[3]);
	printf("#2 {1,2,3,4} = {%d,%d,%d,%d}\n", c[4], c[5], c[6], c[7]);
	printf("#3 {1,2,3,4} = {%d,%d,%d,%d}\n", c[8], c[9], c[10], c[11]);
	printf("#4 {1,2,3,4} = {%d,%d,%d,%d}\n", c[12], c[13], c[14], c[15]);
	printf("#5 {1,2,3,4} = {%d,%d,%d,%d}\n", c[16], c[17], c[18], c[19]);

	// cudaDeviceReset must be called before exiting in order for profiling and
	// tracing tools such as Nsight and Visual Profiler to show complete traces.
	cudaStatus = cudaDeviceReset();
	if (cudaStatus != cudaSuccess) {
		fprintf(stderr, "cudaDeviceReset failed!");
		return 1;
	}

	return 0;
}

// constant memory test
__constant__ int dev_r[2];

// kernel function
__global__ void addKernel(int *c, const int *x, const int *y, const int size)
{
	int idx = threadIdx.x;
	int i = idx % size;
	int j = idx / size;

	int sx = x[i];
	int sy = y[i];
	int tx = x[j];
	int ty = y[j];

	if (sx + 1 == tx && sy == ty) {
		c[i * 4 + 0] = dev_r[0];
	}
	else { 
		c[i * 4 + 0] = dev_r[1];
	}

	if (sx == tx && sy + 1 == ty) {
		c[i * 4 + 1] = dev_r[0];
	}
	else {
		c[i * 4 + 1] = dev_r[1];
	}

	if (sx - 1 == tx && sy == ty) {
		c[i * 4 + 2] = dev_r[0];
	}
	else {
		c[i * 4 + 2] = dev_r[1];
	}

	if (sx == tx && sy - 1 == ty) {
		c[i * 4 + 3] = dev_r[0];
	}
	else {
		c[i * 4 + 3] = dev_r[1];
	}

}

// Helper function for using CUDA to add vectors in parallel.
cudaError_t funcCuda(int *c, const int *a, const int *b, const int* r, unsigned int size)
{
	int *dev_a = 0;
	int *dev_b = 0;
	int *dev_c = 0;

	cudaError_t cudaStatus;

	printf("r: %d, %d\n", r[0], r[1]);

	try {

		// Choose which GPU to run on, change this on a multi-GPU system.
		cudaStatus = cudaSetDevice(0);
		if (cudaStatus != cudaSuccess) {
			fprintf(stderr, "cudaSetDevice failed!  Do you have a CUDA-capable GPU installed?");
			throw - 1;
		}

		// Allocate constant memory
		cudaStatus = cudaMemcpyToSymbol(dev_r, r, 2 * sizeof(int));
		if (cudaStatus != cudaSuccess) {
			fprintf(stderr, "cudaMemcpyToSymbol failed! Fail to allocate constant memory");
			throw - 1;
		}

		// Allocate GPU buffers for three vectors (two input, one output)    .
		cudaStatus = cudaMalloc((void**)&dev_c, 4 * size * sizeof(int));
		if (cudaStatus != cudaSuccess) {
			fprintf(stderr, "cudaMalloc failed!");
			throw - 1;
		}

		cudaStatus = cudaMalloc((void**)&dev_a, size * sizeof(int));
		if (cudaStatus != cudaSuccess) {
			fprintf(stderr, "cudaMalloc failed!");
			throw - 1;
		}

		cudaStatus = cudaMalloc((void**)&dev_b, size * sizeof(int));
		if (cudaStatus != cudaSuccess) {
			fprintf(stderr, "cudaMalloc failed!");
			throw - 1;
		}

		// Copy input vectors from host memory to GPU buffers.
		cudaStatus = cudaMemcpy(dev_a, a, size * sizeof(int), cudaMemcpyHostToDevice);
		if (cudaStatus != cudaSuccess) {
			fprintf(stderr, "cudaMemcpy failed!");
			throw - 1;
		}

		cudaStatus = cudaMemcpy(dev_b, b, size * sizeof(int), cudaMemcpyHostToDevice);
		if (cudaStatus != cudaSuccess) {
			fprintf(stderr, "cudaMemcpy failed!");
			throw - 1;
		}

		// Launch a kernel on the GPU with one thread for each element.
		addKernel <<<1, size * size>>>(dev_c, dev_a, dev_b, size);

		// Check for any errors launching the kernel
		cudaStatus = cudaGetLastError();
		if (cudaStatus != cudaSuccess) {
			fprintf(stderr, "addKernel launch failed: %s\n", cudaGetErrorString(cudaStatus));
			throw - 1;
		}

		// cudaDeviceSynchronize waits for the kernel to finish, and returns
		// any errors encountered during the launch.
		cudaStatus = cudaDeviceSynchronize();
		if (cudaStatus != cudaSuccess) {
			fprintf(stderr, "cudaDeviceSynchronize returned error code %d after launching addKernel!\n", cudaStatus);
			throw - 1;
		}

		// Copy output vector from GPU buffer to host memory.
		cudaStatus = cudaMemcpy(c, dev_c, 4 * size * sizeof(int), cudaMemcpyDeviceToHost);
		if (cudaStatus != cudaSuccess) {
			fprintf(stderr, "cudaMemcpy failed!");
			throw - 1;
		}

	}

	catch (int e) {

		if (e == -1) {
			cudaFree(dev_c);
			cudaFree(dev_a);
			cudaFree(dev_b);
		}
		else {
			throw;
		}

	}

	return cudaStatus;
}
