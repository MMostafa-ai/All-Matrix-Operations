#include <iostream>
#include <math.h>
#include <string>
#include <algorithm>
#define  maxSize  10

using namespace std;

float** declareMat();
float** addMat(float* arr1[maxSize], float* arr2[maxSize], int m1, int n1, int m2, int n2);
float** swapRow(float* a[maxSize], int m, int n, int r);
float** constMultMat(float* arr[maxSize], int m, int n, float c);
float** InverseMat(float* arr[maxSize], int m, int n);

float** declareMat()
{
	float** arr = new float*[maxSize];
	for (int i = 0; i < maxSize; i++)
	{
		arr[i] = new float[maxSize];
		for (int j = 0; j < maxSize; j++)
		{
			arr[i][j] = 0;
		}
	}
	return arr;
}

float** stoMat(string x, int* row, int* column)
{
	string placeHolder = "";
	int len = x.length();
	int m =0, n=0 ;
	float** arr = declareMat();

	if (x[0] != '[' || x[len - 1] != ']')	return 0;

	for (int i = 0; i < len; i++)
	{
		int num = abs((int)x[i] - 48);

		if (x[i] != ';' && x[i] != ' '&& x[0] != '[' &&  x[i] != ']' && x[i] != '.' && x[i] != '-' && num > 9 ) return 0;

		if (x[i] == ';')
		{
			m++;
			n = 0;
		}
		else if (num >= 0 && num <= 9 || x[i] == '-')
		{
			while ((x[i] != ' ' && x[i] != ';' && x[i] != ']') || (x[i] == '.'   || x[i] == '-'))
			{

				placeHolder += x[i];
				i++;
			}
			if (x[i] == ';') i--;
			if (placeHolder == "-0") placeHolder = "0";
			arr[m][n] = stof(placeHolder);
			placeHolder = "";
			n++;
		}
	}

	*row = m+1;
	*column = n;

	return arr;
}


void printMat(float *arr[maxSize],int m ,int n)
{
	for (int i = 0; i < m ; i++)
	{
		cout << "[";
		for (int j = 0; j < n; j++) 
		{
			cout << arr[i][j] ;
			if (j != n - 1) cout << " ";
		}
		cout << "]\n";
	}
}

float** multMat(float* arr1[maxSize], float* arr2[maxSize],int m1,int n1,int m2,int n2)
{
	if (n1 != m2) return 0;

	float** mult = declareMat();

	for (int i = 0; i < m1; i++)
	{
		for (int j = 0; j < n2; j++)
		{
			for (int k = 0; k < n1; k++)
			{
				mult[i][j] += arr1[i][k] * arr2[k][j];
			}
		}
			
	}
	
	return mult;
}

float** transMat(float* arr[maxSize],int m, int n)
{
	float** trans = declareMat();
	for (int i = 0; i < n; i++)
	{
		for (int j = 0; j < m; j++)
		{
			trans[i][j] = arr[j][i];
		}
	}
	return trans;
}

float** addMat(float* arr1[maxSize], float* arr2[maxSize], int m1, int n1, int m2, int n2)
{
	if (m1 != m2 || n1 != n2) return 0;

	float** sumArr = declareMat();

	for (int i = 0; i < m1; i++)
	{
		for (int j = 0; j < n1; j++)
		{
			sumArr[i][j] = arr1[i][j] + arr2[i][j];
		}
	}
	
	return sumArr;
}

float** subMat(float* arr1[maxSize], float* arr2[maxSize], int m1, int n1, int m2, int n2)
{
	for (int i = 0; i < m1; i++)
	{
		for (int j = 0; j < n1; j++)
		{
			arr2[i][j] *= -1;
		}
	}
	if (!addMat(arr1,arr2,m1,n1,m2,n2)) return 0;
	else return addMat(arr1, arr2, m1, n1, m2, n2);
}

float detMat(float* a[maxSize], int m, int n) {
	float d = 0;
	float** submatrix = declareMat();
	if (n != m) return 0;
	else if (n == 2)
	{
		return a[0][0] * a[1][1] - a[1][0] * a[0][1];
	}
	else {
		for (int i = 0; i < n; i++) {
			int subi = 0;
			for (int j = 1; j < n; j++) {
				int subj = 0;
				for (int k = 0; k < n; k++) {
					if (k == i)
						continue;
					submatrix[subi][subj] = a[j][k];
					subj++;
				}
				subi++;
			}
			d = d + (pow(-1, i) * a[0][i] * detMat(submatrix, n - 1, m - 1));
		}
	}
	return d;
}

float** cofMat(float* a[maxSize], int m, int n)
{
	float** submatrix = declareMat();
	float** coMat = declareMat();
	float** negAndPos= declareMat();

	 for (int i=0 ; i<maxSize;i++)
	 {
		 for(int j=0;j<maxSize;j++)
		 {
			 negAndPos[i][j]+=pow(-1,i+j);
		 }
	 }
	if (n != m) return 0;
	if (n == 2)
	{
		coMat[0][0] = a[1][1];
		coMat[1][1] = a[0][0];
		coMat[0][1] = a[1][0];
		coMat[1][0] = a[0][1];

		for (int i = 0; i < m; i++)
		{
			for (int j = 0; j < n; j++)
			{
			
					coMat[i][j] *= negAndPos[i][j];
				
			}
		}
		return coMat;
	}
	for (int l = 0; l < n; l++)
	{
		int d = 0;
		for (int i = 0; i < n; i++)
		{
			int subi = 0;
			for (int j = 1; j < n; j++)
			{
				int subj = 0;
				for (int k = 0; k < n; k++)
				{
					if (k == i)
						continue;
					submatrix[subi][subj] = a[j][k];
					subj++;
				}
				subi++;
			}
			coMat[l][i] = detMat(submatrix, n - 1, m - 1);
		}
		swapRow(a, m, n, l + 1);
	}

	for (int i = 0; i < m; i++)
	{
		for (int j = 0; j < n; j++)
		{
			coMat[i][j] *= negAndPos[i][j];
		}
	}

	return coMat;
}

float** swapRow(float* a[maxSize], int m, int n, int r)
{
	float** matrixHolder = declareMat();
	if (r == 0) return a;

	for (int j = 0; j < n; j++)
	{
		float t = a[0][j];
		a[0][j] = a[r][j];
		a[r][j] = t;
	}

	return a;
}

float** adjMat(float* arr[maxSize],int m,int n)
{
	return transMat(cofMat(arr, m, n), m, n);
}

float** InverseMat(float* arr[maxSize], int m, int n)
{
	float d = detMat(arr, m, n);
	if (d == 0)
	{
		return 0;
	}
	else
	{
		return constMultMat(adjMat(arr, m, n), m, n, pow(d,-1));
	}
}
float** constMultMat(float* arr[maxSize], int m, int n, float c)
{
	float** prodMat = declareMat();
	for (int i = 0; i < m; i++)
	{
		for (int j = 0; j < n; j++)
		{
			prodMat[i][j] = arr[i][j] * c;
		}
	}
	return prodMat;
}

float**  getFromUserA(string strA,float* A[maxSize],  int* row1, int* col1)
{
	int mA = 0, nA = 0;
	do
	{
		cout << "please Enter Matrix A\n";
		getline(cin, strA);

		if (!stoMat(strA, &mA, &nA))
		{
			for_each(strA.begin(), strA.end(), [](char & c)
			{
				c = ::tolower(c);
			});
			if (strA == "exit") return 0;

			cout << "please enter a valid value for matrix A as in the form \n";

		}
		else
		{
			A = stoMat(strA, &mA, &nA);
			*row1 = mA;
			*col1 = nA;
		}
	} while (!stoMat(strA, &mA, &nA));

	return A;

}
float** getFromUserB(string strB, float* B[maxSize], int *row2, int* col2)
{
	int  mB = 0, nB = 0;
	do
	{
		cout << "please Enter Matrix B\n";
		getline(cin, strB);

		if (!stoMat(strB, &mB, &nB))
		{
			for_each(strB.begin(), strB.end(), [](char & c)
			{
				c = ::tolower(c);
			});
			if (strB == "exit") return 0;

			cout << "please enter a valid value for matrix B as in the form \n";

		}
		else
		{
			B = stoMat(strB, &mB, &nB);
			*row2 = mB;
			*col2 = nB;

		}
	} while (!stoMat(strB, &mB, &nB));

	return B;
}

