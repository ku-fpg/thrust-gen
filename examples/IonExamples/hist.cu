#include <thrust/host_vector.h>
#include <iostream>
#include <thrust/iterator/counting_iterator.h>
#include <thrust/binary_search.h>
#include <thrust/adjacent_difference.h>


int main (void){
	thrust::host_vector<int> v0(4);
	v0[0] = 0;
	v0[1] = 2;
	v0[2] = 3;
	v0[3] = 5;
	

	for (int i = 0; i < 4; ++i){std::cout << v0[i] << " ";}
	std::cout << std::endl;
	thrust::host_vector<int> v1(6);
	v1[0] = 0;
	v1[1] = 0;
	v1[2] = 0;
	v1[3] = 0;
	v1[4] = 0;
	v1[5] = 0;
	
	thrust::counting_iterator<int> i2(0);
	thrust::upper_bound(v0.begin(),v0.end(),i2,i2 + 6,v1.begin());
	thrust::adjacent_difference(v1.begin(),v1.end(),v1.begin());

	for (int i = 0; i < 6; ++i){std::cout << v1[i] << " ";}
	std::cout << std::endl;
}
