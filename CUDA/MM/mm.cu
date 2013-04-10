#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

/*
__global__ void kernel(float* input0,float* input1,float* output0){
  
    extern __shared__ __attribute__ ((aligned(16))) uint8_t sbase[];
    float v3;
    v3 = 0.0;
    for (int i4 = 0;i4 < 256;i4++){
      
        v3 = (v3+(input0[((blockIdx.x*256)+i4)]*input1[((i4*256)+threadIdx.x)]));
        
    }
    ((float*)(sbase+1024))[threadIdx.x] = v3;
    ((float*)sbase)[threadIdx.x] = ((float*)(sbase+1024))[threadIdx.x];
    output0[((blockIdx.x*256)+threadIdx.x)] = ((float*)sbase)[threadIdx.x];
    
  
}

__global__ void kernel(float* input0,float* input1,float* output0){
  
    extern __shared__ __attribute__ ((aligned(16))) uint8_t sbase[];
    float v1;
    v1 = 0.0;
    for (int i2 = 0;i2 < 256;i2++){
      
        v1 = (v1+(input0[((blockIdx.x*256)+i2)]*input1[((i2*256)+threadIdx.x)]));
        
    }
    output0[((blockIdx.x*256)+threadIdx.x)] = v1;
    
  
}

__global__ void kernel(float* input0,float* input1,float* output0){
  
    float v1;
    v1 = 0.0;
    for (int i2 = 0;i2 < 256;i2++){
      
        v1 = (v1+(input0[((blockIdx.x*256)+i2)]*input1[((i2*256)+threadIdx.x)]));
        
    }
    output0[((blockIdx.x*256)+threadIdx.x)] = v1;
     
}
*/ 

/* number of threads needed 256*/
__global__ void kernel(float* input0,float* input1,float* output0){
  
    float v1;
    v1 = 0.0;
    for (int i2 = 0;i2 < 256;i2++){
      
        v1 = (v1+(input0[((threadIdx.x*256)+i2)]*input1[((i2*256)+threadIdx.x)]));
        
    }
    __syncthreads();
    output0[((blockIdx.x*256)+threadIdx.x)] = v1;
    
  
}



#define N (256*256)

int main(int argc, char **argv){


  float *v1; 
  float *v2;
  float *r;
  float *rc;
  float *dv1;
  float *dv2; 
  float *dr;
 
  v1 = (float*)malloc(sizeof(float) *N);
  v2 = (float*)malloc(sizeof(float) *N); 
  r  = (float*)malloc(sizeof(float) *N); 
  rc = (float*)malloc(sizeof(float) *N); 
    
  //generate input data
  for (int i = 0; i < N; ++i) {     
    v1[i] = i % 4 ; 
    v2[i] = 1.34; 
    //int j = i / 256; 
    //int k = i % 256; 
    //if (j == k) v2[i] = 1.0;
  }

  cudaMalloc((void**)&dv1, sizeof(float) * N ); 
  cudaMalloc((void**)&dv2, sizeof(float) * N ); 
  cudaMalloc((void**)&dr, sizeof(float) * N ); 


  cudaMemcpy(dv1, v1, sizeof(float) * N, cudaMemcpyHostToDevice);
  cudaMemcpy(dv2, v2, sizeof(float) * N, cudaMemcpyHostToDevice);


  kernel<<<256, 256,2048* sizeof(float)>>>(dv1,dv2,dr);

  cudaMemcpy(r, dr, sizeof(float) * N , cudaMemcpyDeviceToHost);
  cudaFree(dv1);
  cudaFree(dv2);
 
  cudaFree(dr);
  
  // show results 
  for (int i = 0; i < N; ++i) { 
    printf("%f ", r[i]);
  }


  /* CPU Matrix mult */ 

  for (int i=0;i<256;i++){
    for(int j=0;j<256;j++){
      for(int k=0;k<256;k++){
       rc[i*256+j] += v1[i*256+k] * v2[k*256+j];
      }
    }
  }

  for (int i = 0; i < N; ++i) {
    if (rc[i] != r[i]) {
      printf("differs! %f %f \n ", rc[i], r[i]);
    }
  }
  
  /* compare few first */ 
  
  printf("\n Compare a few  \n"); 
  for (int i = 0; i < 10; ++i) {
    printf("%f %f \n", rc[i], r[i]);

  }
  


}
