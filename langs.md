メモ：
## 発表に入れるやつ
- Python
- Julia
- C++
- Rust
- JS
- Haskell

## スクリプトみの強いやつ
### Python
  - とてもおそい(最速組比20倍以上)
  - PyPy(高速な実装)とかCython(一部コードをCに変換するツール)を使えば人道的な実行時間で済む
  - やれと言わなくてもやることになる
  - ライブラリが豊富
  - 比較的簡単
  - 型アノテーションはあるらしい
  - 京大の資料読め

code:
```py
import math

print("start")
MAX = 100000000

sieve = [True]*(MAX+1)

sieve[0] = False
sieve[1] = False

for i in range(2,int(math.sqrt(MAX))):
    if sieve[i]:
        for j in range(i*i,MAX+1,i):
            sieve[j] = False

primes = [0]*(MAX+1)
pcount = 0

for i in range(2,MAX+1):
    if sieve[i]:
        primes[pcount] = i
        pcount += 1

print(primes[pcount-1])
print("end")
```

result:
```
$ :
$ time python main.py
start
99999989
end

real	0m28.604s
user	0m27.971s
sys	0m0.414s
```

result:
```
$ :
$ time pypy main.py
start
99999989
end

real	0m4.767s
user	0m4.201s
sys	0m0.505s
```


code:
```py
cimport numpy as np
import numpy as np
import math
import cython

def main():
    cdef:
        int MAX, TRUE, FALSE, pcount
        int[:] sieve
        int[:] primes
        int i, j

    MAX = 100000000
    TRUE  = 1
    FALSE = 0

    sieve = np.full(MAX+1, TRUE, dtype="int32") 
    
    sieve[0] = FALSE
    sieve[1] = FALSE
    
    for i in range(2,int(math.sqrt(MAX))):
        if sieve[i]:
            for j in range(i*i,MAX+1,i):
                sieve[j] = FALSE
    
    primes = np.zeros(MAX+1, dtype="int32") 
    pcount = 0
    
    for i in range(2,MAX+1):
        if sieve[i]:
            primes[pcount] = i
            pcount += 1
    
    return primes, pcount
```

result:
```
$ python setup.py build_ext --inplace
$ time python cymain.py
start
99999989
end

real	0m3.800s
user	0m3.869s
sys	0m0.817s
```

### Julia
  - はやい
  - やるといいらしい
  - pythonのライブラリが使える
  - 比較的簡単らしい
  - あなばすが残りを書いてくれる
{sample:jl}
### PHP
  - おそい
  - やれとは全く言わないけどたのしいよ
  - Web屋さん(サーバーサイド)専用
{sample:php}

## コンパイルして使うやつ
### C
  - 最速組
  - 工学erはやることになりそう
  - 静的型、型推論なし
  - 自己責任系プログラミング言語

code:
```c
#include <stdio.h>
#include <stdbool.h>

const int MAX = 100000000;

int main(){
    static bool sieve[MAX+1];
    static int primes[MAX+1];
    puts("start");
    for(int i=0; i<=MAX; i++) sieve[i]=true;
    sieve[0] = false;
    sieve[1] = false;
    for(int i=0; i<=MAX; i++){
        if(sieve[i]){
            for(int j = i*i; j<=MAX; j+=i) sieve[j] = false;
        }
    }
    int pcount=0;
    for(int i=0; i<=MAX; i++){
        if(sieve[i]){
            primes[pcount]=i;
            pcount++;
        }
    }

    printf("%d\n", primes[pcount-1]);
    puts("end");
    return 0;
}
```

result:
```
$ clang   main.c   -O3
$ time ./a.out
start
99999989
end

real	0m0.838s
user	0m0.797s
sys	0m0.036s
```


### C++
  - 最速組
  - 競プロerはやっとけ
  - 自分のこと現代的だと思ってる古参言語
  - 静的型、型推論ややあり
  - Cにない型がいっぱいある

code:
```cpp
#include <vector>
#include <iostream>

using namespace std;

const int MAX = 100000000;

int main(){
    cout << "start" << endl;
    vector<bool> sieve(MAX+1, true);
    sieve[0] = false;
    sieve[1] = false;
    for(auto i=0; i<=MAX; i++){
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

    cout << primes[pcount-1] << endl;
    
    cout << "start" << endl;
    return 0;
}
```

result:
```
$ clang++ main.cpp -O3
$ time ./a.out
start
99999989
start

real	0m0.771s
user	0m0.655s
sys	0m0.113s
```

  
### Rust
  - 最速組
  - やれ
  - 2言語目推奨
  - プログラマのことを信頼していない
  - 型が強い
  - 静的型、型推論強い
  - 新世代の勝者？
  - パッケージ管理ツールが優秀
  - ライブラリが豊富
  - ビルドが遅い（特に初回）
  - The Rust Programming Language 読んどけ

code:
```rs
const MAX :usize = 100000000;
fn main() {
    println!("start");

    let mut sieve = vec![true; MAX+1];
    sieve[0] = false;
    sieve[1] = false;
    for i in 2..=(f32::sqrt(MAX as f32) as usize) {
        if sieve[i]{
            for j in (i*i..=MAX).step_by(i) {
                sieve[j] = false;
            }
        }
    }

    let mut primes = vec![0; MAX+1];
    let mut pcount = 0;
    for (i,s) in sieve.iter().enumerate() {
        if *s {
            primes[pcount] = i;
            pcount += 1;
        }
    }
    println!("{}",primes[pcount-1]);
    println!("end");
}
```

result:
```
$ cargo build --release
$ time ./target/release/rs
start
99999989
end

real	0m0.909s
user	0m0.869s
sys	0m0.030s
```


## コンパイル言語でクラスベースオブジェクト指向のやつ
### Java
  - 五十歩百歩組
  - 業務で使われがち
  - ザ・クラスベースオブジェクト指向
  - 静的型
{sample:java}
### C#
  - 五十歩百歩組2
  - MS製のJava
  - Win向けのGUIアプリを書くのが簡単（後述のVB.netとF#も）
  - Unityでつかうらしい
  - 静的型、型推論ややあり
{sample:cs}
### VB.net
  - C#。
  - C#(Basic風味)。
{sample:vb}

## 関数型プログラミングに対するサポートが強いやつ
### OCaml
  - 五十歩百歩組3
  - ML族
  - 非純粋
  - 静的型、型推論強い
{sample:ocaml}
### Haskell
  - 五十歩百歩組4
  - __†純粋†__ 関数型言語
  - 中二病患者におすすめ
  - メタ記述能力の鬼
  - こみいった副作用に対する制御とか複雑なルールの組み合わせとかに滅法強い
  - ビルドが遅い（特に初回）
  - 静的型、型推論強い

code:
```hs
module Main where

import Lib
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Foldable
import Control.Monad
import Control.Monad.ST
import Data.STRef
import GHC.Float

num :: Int
num = 100000000

main :: IO ()
main = do
  putStrLn "start"
  let ps = generatePrimes num
  print $ V.last ps
  putStrLn "end"


generatePrimes :: Int -> V.Vector Int
generatePrimes max =
  let sieve = runST (do
          msieve <- MV.replicate (max+1) True
          MV.write msieve 0 False
          MV.write msieve 1 False
          
          forM_ [2..(double2Int $ sqrt $ fromIntegral max)] $ \i -> do
            isprime <- MV.unsafeRead msieve i
            when isprime (forM_ [i..max `div` i] (\j -> MV.unsafeWrite msieve (i*j) False))
          
          V.unsafeFreeze msieve
        )

      primes = runST (do
          mprimes <- MV.replicate (max+1) 0

          pcount <- foldM (\pcount i -> do
            let isprime = V.unsafeIndex sieve i
            if isprime
            then (do
              MV.unsafeWrite mprimes pcount i
              pure (pcount+1)
              )
            else pure pcount
            ) 0 [2..max]
          V.unsafeFreeze $ MV.unsafeSlice 0 pcount mprimes
        )
  --in V.filter (V.unsafeIndex sieve) (V.generate (max+1) id)
  in primes
```

result:
```
$ stack install --local-bin-path ./
$ time ./hs-exe
start
99999989
end

real	0m0.949s
user	0m0.682s
sys	0m0.258s
```


## ブラウザでつかえるやつ
### javascript
  - 五十歩百歩組6（なんで？？？きみ動的型のスクリプト言語だろ？？？？？）
  - GUI作りたくなったらHTML+CSS+JSでやるのが一番かんたんかもしれない
  - サーバーサイドもスタンドアロンも書ける
  - 動的型、型アノテーションすらない
  - 愛すべきカス
{sample:js}
### Typescript
  - AltJSのデファクトスタンダード、型のあるJS
  - 型がある！！！しかも強い！！！！！！！（とても重要）
{sample:ts}
### (WebAsssamplery)
  - JS一族ではない
  - 他の言語(RustとかC++とか)からWASMにコンパイルしてブラウザで使える
### coffeescript
  - Rubyのようななにか
{sample:coffee}
### purescript
  - AltJSの異端児、Haskellの生き写し
{sample:purs}
### scala.js
  - scalaがjsにコンパイルされる
{sample:scalajs}
### GHCjs
  - Haskellがjsにコンパイルされる
{sample:ghcjs}
### js_of_ocaml
  - OCamlがjsにコンパイルされる
{sample:jsocaml}

## 速度ランキング
実行時間：
| rank | lang | time |
| - | - | - |
| 1 | C++ | 0.771 sec. |
| 2 | C | 0.838 sec. |
| 3 | Rust | 0.909 sec. |
| 4 | Haskell | 0.949 sec. |
| 5 | Cython | 3.8 sec. |
| 6 | PyPy | 4.767 sec. |
| 7 | Python | 28.604 sec. |

CPU時間：
| rank | lang | time |
| - | - | - |
| 1 | C++ | 0.655 sec. |
| 2 | Haskell | 0.682 sec. |
| 3 | C | 0.797 sec. |
| 4 | Rust | 0.869 sec. |
| 5 | Cython | 3.869 sec. |
| 6 | PyPy | 4.201 sec. |
| 7 | Python | 27.971 sec. |
