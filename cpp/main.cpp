#include <vector>
#include <iostream>
#include <cmath>

using namespace std;

const int MAX = 100000000;

int main(){
    cout << "start" << endl;
    int SQRT_MAX = (int)sqrt((double)MAX);
    vector<bool> sieve(MAX+1, true);
    sieve[0] = false;
    sieve[1] = false;
    for(auto i=0; i<=SQRT_MAX; i++){
        if(sieve[i]){
            for(auto j = i*i; j<=MAX; j+=i) sieve[j] = false;
        }
    }
    vector<int> primes(MAX+1);
    int pcount = 0;
    for(auto i=0; i<=MAX; i++){
        if(sieve[i]){
            primes[pcount] = i;
            pcount++;
        }
    }

    cout << "found " << pcount << " primes" << endl;
    cout << primes[pcount-1] << endl;
    
    cout << "start" << endl;
    return 0;
}
