#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/generate.h>
#include <thrust/random.h>
#include <thrust/reduce.h>

struct f4 : public thrust::binary_function<const int&, const int&, int>
 {
	__host__ __device__ 
  int operator()(const int a, const int b) const{
		return ((0 + a) + b);
	}
};


int main (void){
	thrust::host_vector<int> v0(101);
	v0[0] = 0;
	v0[1] = 1;
	v0[2] = 2;
	v0[3] = 3;
	v0[4] = 4;
	v0[5] = 5;
	v0[6] = 6;
	v0[7] = 7;
	v0[8] = 8;
	v0[9] = 9;
	v0[10] = 10;
	v0[11] = 11;
	v0[12] = 12;
	v0[13] = 13;
	v0[14] = 14;
	v0[15] = 15;
	v0[16] = 16;
	v0[17] = 17;
	v0[18] = 18;
	v0[19] = 19;
	v0[20] = 20;
	v0[21] = 21;
	v0[22] = 22;
	v0[23] = 23;
	v0[24] = 24;
	v0[25] = 25;
	v0[26] = 26;
	v0[27] = 27;
	v0[28] = 28;
	v0[29] = 29;
	v0[30] = 30;
	v0[31] = 31;
	v0[32] = 32;
	v0[33] = 33;
	v0[34] = 34;
	v0[35] = 35;
	v0[36] = 36;
	v0[37] = 37;
	v0[38] = 38;
	v0[39] = 39;
	v0[40] = 40;
	v0[41] = 41;
	v0[42] = 42;
	v0[43] = 43;
	v0[44] = 44;
	v0[45] = 45;
	v0[46] = 46;
	v0[47] = 47;
	v0[48] = 48;
	v0[49] = 49;
	v0[50] = 50;
	v0[51] = 51;
	v0[52] = 52;
	v0[53] = 53;
	v0[54] = 54;
	v0[55] = 55;
	v0[56] = 56;
	v0[57] = 57;
	v0[58] = 58;
	v0[59] = 59;
	v0[60] = 60;
	v0[61] = 61;
	v0[62] = 62;
	v0[63] = 63;
	v0[64] = 64;
	v0[65] = 65;
	v0[66] = 66;
	v0[67] = 67;
	v0[68] = 68;
	v0[69] = 69;
	v0[70] = 70;
	v0[71] = 71;
	v0[72] = 72;
	v0[73] = 73;
	v0[74] = 74;
	v0[75] = 75;
	v0[76] = 76;
	v0[77] = 77;
	v0[78] = 78;
	v0[79] = 79;
	v0[80] = 80;
	v0[81] = 81;
	v0[82] = 82;
	v0[83] = 83;
	v0[84] = 84;
	v0[85] = 85;
	v0[86] = 86;
	v0[87] = 87;
	v0[88] = 88;
	v0[89] = 89;
	v0[90] = 90;
	v0[91] = 91;
	v0[92] = 92;
	v0[93] = 93;
	v0[94] = 94;
	v0[95] = 95;
	v0[96] = 96;
	v0[97] = 97;
	v0[98] = 98;
	v0[99] = 99;
	v0[100] = 100;
	
	static thrust::default_random_engine r1;
	static thrust::uniform_int_distribution<int> r2(0,9999);
	thrust::generate(v0.begin(), v0.end(), r2(r1));
  thrust::device_vector<int> zz = v0;

	int v5 = thrust::reduce(zz.begin(), zz.end(), 0, f4());
}
