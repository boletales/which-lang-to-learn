# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
この記事では、「これからプログラミングを学習したいけど何をやったらいいかわからないっ！」「新しくプログラミング言語を学びたいのでおすすめを知りたいっ！」というひとたちのために、各言語の特徴と(題材が簡単すぎて各言語の味がしない)コードサンプルを掲載しています。

- <a href='#anchor0'>スクリプトみの強いやつ</a>
  - <a href='#anchor0-0'>Python</a>
  - <a href='#anchor0-1'>Ruby</a>
  - <a href='#anchor0-2'>Julia</a>

- <a href='#anchor1'>コンパイルして使うやつ</a>
  - <a href='#anchor1-0'>C</a>
  - <a href='#anchor1-1'>C++</a>
  - <a href='#anchor1-2'>Rust</a>
  - <a href='#anchor1-3'>Crystal</a>
  - <a href='#anchor1-4'>Assembly</a>

- <a href='#anchor2'>クラスベースオブジェクト指向一味</a>
  - <a href='#anchor2-0'>Java</a>
  - <a href='#anchor2-1'>C#</a>
  - <a href='#anchor2-2'>VB.net</a>

- <a href='#anchor3'>関数型プログラミングに対するサポートが強いやつ</a>
  - <a href='#anchor3-0'>OCaml</a>
  - <a href='#anchor3-1'>Haskell</a>
  - <a href='#anchor3-2'>Lisp</a>

- <a href='#anchor4'>Web屋さん向け</a>
  - <a href='#anchor4-0'>javascript</a>
  - <a href='#anchor4-1'>PHP</a>
  - <a href='#anchor4-2'>WebAssembly</a>
  - <a href='#anchor4-3'>以下星の数ほどあるAltJS(JSにコンパイルされる代替言語)の一部</a>
  - <a href='#anchor4-4'>Typescript</a>
  - <a href='#anchor4-5'>coffeescript</a>
  - <a href='#anchor4-6'>purescript</a>
  - <a href='#anchor4-7'>scala.js</a>
  - <a href='#anchor4-8'>GHCjs</a>
  - <a href='#anchor4-9'>js_of_ocaml</a>

- <a href='#anchor5'>統計とかシミュレーションに使うやつ</a>
  - <a href='#anchor5-0'>R</a>
  - <a href='#anchor5-1'>MATLAB</a>
  - <a href='#anchor5-2'>Fortran</a>

- <a href='#anchor6'>実行環境について</a>

- <a href='#anchor7'>速度ランキング（あんまり参考にならない）</a>

- <a href='#anchor8'>貢献者一覧</a>


## <a name='anchor0'></a>スクリプトみの強いやつ
### <a name='anchor0-0'></a>Python
- 理系大学生のリンガフランカ
- いいところ
  - ライブラリが豊富（機械学習とかスクレイピングとか）
  - 使ってる人が多い
  - 文法が簡単
  - PyPy(高速な実装)とかCython(一部コードをCに変換するツール)を使えば人道的な実行時間で済む
- わるいところ
  - 環境構築がややこしい
  - とてもおそい(最速組比20倍以上)
  - デバッグがしづらい
  - オフサイドルールはクソ
- 有用なリソース
  - 京大の資料(https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/265459/1/Version2021_10_08_01.pdf)

高速なライブラリ(numpyとか)を使えばPythonらしからぬ速度を出せる

code:
```py
import math
import numpy as np

print("start")
MAX = 100000000

sieve = np.ones(MAX+1, dtype="bool")

sieve[0] = False
sieve[1] = False

for i in range(2,int(math.sqrt(MAX))):
    if sieve[i]:
        sieve[np.arange(i,MAX//i + 1)*i] = False

nums   = np.arange(0,MAX+1)
primes = nums[sieve[nums]]
print(primes[-1])
print("end")
```

result:
```
$ :
$ time python main_nd.pystart
99999989
end

real	0m1.726s
user	0m1.640s
sys	0m1.165s
```

普通のPythonのfor文は遅いが、PyPyで実行するとだいぶマシになる(ただしnumpyは使えない)

result:
```
$ :
$ time pypy main.pystart
99999989
end

real	0m4.569s
user	0m4.026s
sys	0m0.515s
```

Cythonで重い部分をCに変換しても速くなる

code:
```py
import cylib

print("start")
primes = cylib.main()
print(primes[-1])
print("end")
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
$ time python cymain.pystart
5761455
end

real	0m3.673s
user	0m3.808s
sys	0m0.939s
```


### <a name='anchor0-1'></a>Ruby
- 松江在住のプログラマが作った言語
- C言語的なfor文が存在しないので、文字列や配列や連想配列などの操作はmapやselectなどの各種メソッドで行う文化
  - `gets.split.map(&:to_i).sort.first(3).reduce(&:+)`みたいにメソッドをつなげて書いていくの、慣れると爽快
- いいところ
  - 書きやすい(個人の感想)
  - 標準ライブラリが充実している
  - サードパーティーのライブラリも豊富
  - CLIツールやWebサービスのサーバサイド(バックエンド)を書くのに向いてる
  - Ruby on RailsというWebアプリ用の超有名フレームワークがある(いろいろ悪く言われがちだが、なんだかんだ強力)
  - パッケージ管理ツール(Bundler)がまとも
  - 環境構築は特に難しくない
- わるいところ
  - とてもおそい(スクリプト言語の中では普通くらいだが、それでも)
  - 正直、データサイエンスや機械学習周りは弱い(おとなしくPythonとか使った方がいい)
  - 型アノテーションや静的な型チェックは存在するが、いろいろ未成熟
- どうしても速度が欲しい人、あるいは静的型付けの安心感が欲しいRuby書きはCrystalを検討してみよう

code:
```rb
puts "start"

MAX = 100000000
sieve = Array.new(MAX + 1, true)
sieve[0] = sieve[1] = false

2.upto(Integer.sqrt(MAX)) do |i|
  (i * i).step(by: i, to: MAX) { |j| sieve[j] = false } if sieve[i]
end

primes = sieve.filter_map.with_index { |v, i| i if v }
puts primes.last, "end"
```

result:
```
$ :
$ time ruby main.rbstart
99999989
end

real	0m19.925s
user	0m19.621s
sys	0m0.239s
```


### <a name='anchor0-2'></a>Julia
- FortranとmatlabとPythonの後釜をいっぺんに狙おうとしている言語
- いいところ
  - 文法がシンプル
  - 事前にコンパイルする必要がないが、JITのおかげで比較的速い（最速組には勝てない）
  - PyCallでpythonのライブラリが使える
  - 数式・行列が扱いやすい
  - numpyやmatplotlibがpythonよりだいぶ使いやすい
  - PythonとCとMATLABの良いところどり的存在 by あなばす
  - Jupyter notebookで使える
  - 並列化に強い
- わるいところ
  - いまのところ入門向けの良書はあまりない
- 特徴
  - 1-indexed、縦行列基本など癖がある（数式をそのまま使えるから数学屋とかシミュレーション屋には便利）
  - クラスはない（Pythonのサンプルコードを脳死で移植はできない）（構造体で似たようなものは作れる）
  - ちゃんと考えてコーディングしないといまいち速度が出ない
- 有用なリソース
  - 公式wiki

コード全体を関数で包まないと死ぬほど遅くなるので注意（一敗）

code:
```jl
function main()
    MAX = 100000000
    
    println("start")
    
    sieve = trues(MAX) :: BitVector
    
    sieve[1] = false
    @inbounds for i :: Int64 in 2:Int64(floor(sqrt(MAX)))
        if sieve[i]
            @inbounds for j in (i*i):i:MAX
                sieve[j] = false
            end
        end
    end

    primes = [i for i in 1:MAX if sieve[i]] :: Vector{Int64}
    
    println(primes[length(primes)])
    println("end")
end
main()
```

result:
```
$ julia main.jl
$ time julia main.jlstart
99999989
end

real	0m0.834s
user	0m0.735s
sys	0m0.089s
```


## <a name='anchor1'></a>コンパイルして使うやつ
### <a name='anchor1-0'></a>C
- 組み込みやさんの必携、恐怖の自己責任系プログラミング言語
- いいところ
  - 最速組。とにかく速い
  - メモリを直接扱える
  - マイコンみたいな制限された環境下でも動く
  - 勉強にはなる
- わるいところ
  - メモリを直接扱わされる
  - 初学者には難しい
  - 何もかも自己責任

code:
```c
#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <string.h>

const int MAX = 100000000;

int main() {
    int SQRT_MAX = (int)sqrt((double)MAX);
    static bool sieve[MAX + 1];
    static int primes[MAX + 1];
    puts("start");
    memset(sieve, 1, (MAX + 1) * sizeof(sieve[0]));
    sieve[0] = false;
    sieve[1] = false;
    for (int i = 0; i <= SQRT_MAX; i++) {
        if (sieve[i]) {
            for (int j = i * i; j <= MAX; j += i) sieve[j] = false;
        }
    }
    int pcount = 0;
    for (int i = 0; i <= MAX; i++) {
        if (sieve[i]) {
            primes[pcount] = i;
            pcount++;
        }
    }

    printf("%d\n", primes[pcount - 1]);
    puts("end");
    return 0;
}
```

result:
```
$ clang   main.c   -O3
$ time ./a.outstart
99999989
end

real	0m0.734s
user	0m0.710s
sys	0m0.017s
```


### <a name='anchor1-1'></a>C++
- 自分のことを最新鋭だと思っている古参言語
- いいところ
  - 最速組。とにかく速い2
  - メモリを直接扱える2
  - 現代的な機能がたくさん追加されている
- わるいところ
  - 何もかも自己責任
  - 初学者には難しい
- Cにない型がいっぱいある

コンパイル時に最適化フラグを渡さないと10倍以上遅い(一敗)

code:
```cpp
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

    cout << primes[pcount-1] << endl;
    
    cout << "start" << endl;
    return 0;
}
```

result:
```
$ clang++ main.cpp -O3
$ time ./a.outstart
99999989
start

real	0m0.641s
user	0m0.525s
sys	0m0.106s
```

  
### <a name='anchor1-2'></a>Rust
- マルチパラダイム言語界の新星 新時代の勝者……！？
- いいところ
  - 最速組。とにかく速い3
  - メモリを直接扱えるが、直接扱わずに済む！！！ココ重要！！
  - メモリ安全
  - 型の力でより安全なコードが書ける
  - 他の言語から大量に有用な機能を輸入している
  - コンパイラが世話を焼いてくれる
  - パッケージマネージャが優秀
  - 並列化に強い
- わるいところ
  - 難易度が、高い。（さすがに一言語目にはおすすめできない）
  - コンパイラが口うるさい
  - ビルドが遅い（特に初回）
- 有用なリソース
  - The Rust Programming Language (和訳): https://doc.rust-jp.rs/book-ja/

code:
```rs
const MAX :usize = 100000000;
fn main() {
    println!("start");

    let mut sieve = vec![true; MAX+1];
    sieve[0] = false;
    sieve[1] = false;
    let sqrtmax = f32::sqrt(MAX as f32) as usize;
    for i in 2..=sqrtmax {
        if sieve[i]{
            for j in i*i..=MAX/i {
                sieve[j*i] = false;
            }
        }
    }

    let mut primes = vec![0; MAX+1];
    let mut pcount = 0;
    for i in 2..=sqrtmax {
        if sieve[i] {
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
$ time ./target/release/rsstart
9991
end

real	0m0.602s
user	0m0.577s
sys	0m0.020s
```


### <a name='anchor1-3'></a>Crystal
- いいところ
  - 最速組。とにかく速い4
  - Rubyの書きやすさとCの速度が合わさり完璧に見える
  - 静的型付け、型推論が強い(というか明示的に型を書く必要がほぼない)
  - Rubyのいいところはだいたい受け継いでいる
- わるいところ
  - ビルドが遅い
  - ライブラリがまだまだ少ない
- Ruby風の言語ではあるが、文法は多少違うし、標準ライブラリにもそれなりに差異があるので注意

code:
```cr
puts "start"

MAX = 100000000
sieve = Array.new(MAX + 1, true)
sieve[0] = sieve[1] = false

2.upto(Math.isqrt(MAX)) do |i|
  (i * i).step(by: i, to: MAX) { |j| sieve[j] = false } if sieve[i]
end

primes = (1..MAX).select{ |i| sieve[i] }
puts primes.last, "end"
```

result:
```
$ crystal build --release main.cr
$ time ./mainstart
99999989
end

real	0m0.878s
user	0m0.841s
sys	0m0.037s
```


### <a name='anchor1-4'></a>Assembly
- これどうやって書くんですか
- いいところ
  - 最速組（書く人間にオプティマイザが搭載されていれば）
  - CPU の気持ちになれる
- わるいところ
  - 生で書くのは苦行
  - デバッグはアホほど大変
  - 生で書くのは苦行
- アドバイス
  - `gcc -S main.c` をすればアセンブリが見れるので、そこから修正するのがおすすめ。
  - [Compiler Explorer](https://gcc.godbolt.org/)を使うのも良い。
  - (C/C++などの) コンパイラはたまにいい感じのコードを吐いてくれないので、コンパイルされたものを見て、適宜手動でアセンブリ最適化するのが良い。

code:
```asm
.STR_START:
	.string	"start"
.STR_END:
	.string	"%d\nend\n"
.STR_COUNT:
	.string	"count %d\n\n"

	.globl	main
main:
	# puts("start");
	leaq	.STR_START(%rip), %rdi
	call	puts@PLT

	# sieve[0] = 0b11;
	movb	$3, sieve(%rip)

	# uint i = 1;
	mov 	$1, %esi

	# rdi = sieve;
	leaq	sieve(%rip), %rdi

	# r9d = 1;
	mov 	$1, %r9d


	jmp	.L2
.L3:
	add 	$1, %esi
.L2:
	cmp 	$100000001, %esi
	je	.L19

	# edx = sieve[esi >> 3];
	movl	%esi, %eax
	shrl	$3, %eax
	movzbl	(%rdi, %rax), %edx

	# if(edx & (1 << (si & 7))) continue;
	mov 	%si, %ax
	and 	$7, %al
	bt	 	%ax, %dx
	jc	.L3
	
	# rax = esi * esi // rsi = esi
	mov 	%esi, %eax
	imulq	%rsi, %rax

	# if (rax > 99999999) continue;
	cmp 	$99999999, %eax
	ja	.L3
.L4:
	# edx = eax >> 3;
	movl	%eax, %edx
	shrl	$3, %edx

	# cl = ax & 7;
	mov 	%ax, %cx
	and 	$7, %cl
	
	# eax += esi;
	addl	%esi, %eax

	# r10b = 1 << cl;
	mov 	$1, %r10b
	sal 	%cl, %r10b
	
	# sieve[rdx] |= r10b;
	orb	%r10b, (%rdi,%rdx)

	cmp 	$100000000, %eax
	jbe	.L4
	jmp	.L3



.L19:
	# rcx = primes;
	leaq	primes(%rip), %rcx

	# edx = 0; ebx = -1;
	xorl	%edx, %edx
	movl	$-1, %ebx
.L14:
	# al = eax = *rdi;
	movzbl	(%rdi), %eax

	# if (al & 1) goto L6;
	testb	$1, %al
	jne	.L6

	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = edx;
	lea 	1(%ebx), %r10d
	mov 	%r10d, %ebx
	movl	%edx, (%rcx,%r10,4)
.L6:
	# if (al & 2) goto L7;
	testb	$2, %al
	jne	.L7

	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r8d = edx + 1;
	lea 	1(%ebx), %r10d
	lea 	1(%edx), %r8d
	mov 	%r10d, %ebx
	movl	%r8d, (%rcx,%r10,4)
.L7:
	testb	$4, %al
	jne	.L8
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r8d = edx + 2;
	lea 	1(%ebx), %r10d
	lea 	2(%edx), %r8d
	mov 	%r10d, %ebx
	movl	%r8d, (%rcx,%r10,4)
.L8:
	testb	$8, %al
	jne	.L9
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r8d = edx + 3;
	lea 	1(%ebx), %r10d
	lea 	3(%edx), %r8d
	mov 	%r10d, %ebx
	movl	%r8d, (%rcx,%r10,4)
.L9:
	testb	$16, %al
	jne	.L10
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r8d = edx + 4;
	lea 	1(%ebx), %r10d
	lea 	4(%edx), %r8d
	mov 	%r10d, %ebx
	movl	%r8d, (%rcx,%r10,4)
.L10:
	testb	$32, %al
	jne	.L11
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r8d = edx + 5;
	lea 	1(%ebx), %r10d
	lea 	5(%edx), %r8d
	mov 	%r10d, %ebx
	movl	%r8d, (%rcx,%r10,4)
.L11:
	testb	$64, %al
	jne	.L12
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r8d = edx + 6;
	lea 	1(%ebx), %r10d
	lea 	6(%edx), %r8d
	mov 	%r10d, %ebx
	movl	%r8d, (%rcx,%r10,4)
.L12:
	testb	%al, %al
	js	.L13
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r8d = edx + 7;
	lea 	1(%ebx), %r10d
	lea 	7(%edx), %r8d
	mov 	%r10d, %ebx
	movl	%r8d, (%rcx,%r10,4)
.L13:
	# ++rdi; // ++sieve;
	addq	$1, %rdi

	# edx += 8;
	addl	$8, %edx
	cmpl	$100000000, %edx
	jne	.L14


	# eax = ebx;
	movl	%ebx, %eax

	# edx = primes[rax = eax];
	movl	(%rcx,%rax,4), %edx
	# rsi = STR_END;
	leaq	.STR_END(%rip), %rsi
	# printf
	xorl	%eax, %eax
	call	__printf_chk@PLT

	# printf(STR_COUNT)
	leaq	.STR_COUNT(%rip), %rsi
	movl	%ebx, %edx
	xorl	%eax, %eax
	call	__printf_chk@PLT

	# return 0;
	xorl	%eax, %eax
	ret

	.bss
primes:
	.zero	400000004
sieve:
	.zero	100000001
```

result:
```
$ gcc main.s
$ time ./a.outstart
99999989
end
count 5717621


real	0m0.603s
user	0m0.599s
sys	0m0.003s
```


## <a name='anchor2'></a>クラスベースオブジェクト指向一味

### <a name='anchor2-0'></a>Java
- いいところ
  - コードの規模が大きくなることに大して耐性が強い
- わるいところ
  - コードが冗長
- 特徴
  - 業務で使われがち
  - ザ・クラスベースオブジェクト指向

code:
```java
import java.util.Arrays;
class Primes {
    public static void main(String[] args){
        System.out.println("start");
        final int max = 100000000;
        boolean[] sieve = new boolean[max+1];
        Arrays.fill(sieve, true);
        var sqrtmax = (int)Math.sqrt(max);
        for(var i = 2; i <= sqrtmax; i++){
            if(sieve[i]){
              for(var j = i*i; j <= max; j+=i){
                    sieve[j] = false;
              }
            }
        }
        var primes = new int[max + 1];
        var pcount = 0;
        for(var i = 2; i <= max; i++){
            if(sieve[i]){
                primes[pcount] = i;
                pcount += 1;
            }
        }
        System.out.println(primes[pcount-1]);
        
        System.out.println("end");
    }
}
```

result:
```
$ javac main.java
$ time java Primesstart
99999989
end

real	0m0.976s
user	0m0.852s
sys	0m0.136s
```

### <a name='anchor2-1'></a>C#
- MS製のJavaのようななにか
- いいところ
  - コードの規模が大きくなることに大して耐性が強い2
  - Win向けのGUIアプリを書くのが簡単
  - Unityで使うらしい
- わるいところ
  - コードが冗長2(Javaよりややマシかも)

code:
```cs
﻿// See https://aka.ms/new-console-template for more information
Console.WriteLine("start");
const int max = 100000000;
bool[] sieve = Enumerable.Repeat(true, max + 1).ToArray();
var sqrtmax = (int)Math.Sqrt(max);
for(var i = 2; i <= sqrtmax; i++){
    if(sieve[i]){
      for(var j = i*i; j <= max; j+=i){
            sieve[j] = false;
      }
    }
}
var primes = new int[max + 1];
var pcount = 0;
for(var i = 2; i <= max; i++){
    if(sieve[i]){
        primes[pcount] = i;
        pcount += 1;
    }
}

Console.WriteLine(primes[pcount - 1]);
Console.WriteLine("end");
```

result:
```
$ dotnet publish -c release -r linux-x64
$ time ./bin/release/net6.0/linux-x64/csstart
99999989
end

real	0m0.845s
user	0m0.777s
sys	0m0.033s
```

### <a name='anchor2-2'></a>VB.net
- C#(VisualBasic風味)
- いいところ
  - 文法が初学者にもわかりやすい(Basicの血を引いているだけある)
  - C#との相互運用が容易。めっちゃ容易。
  - Win向けのGUIアプリを書くのが簡単2
  - コードの規模が大きくなることに大して耐性が強い3
  - C#
- わるいところ
  - Windows以外での使いみちがあまりない
  - 構文が冗長なので補完がないとつらい(VisualStudioで書く分には補完が強いので気にならない)
  - 不遇
  - C#

code:
```vb
Imports System

Module Program
    Sub Main(args As String())
        Console.WriteLine("start")
        Const max  As Integer = 100000000
        Dim sieve() As Boolean = Enumerable.Repeat(True, max + 1).ToArray()
        Dim sqrtmax As Integer = CType(Math.Sqrt(max), Integer)
        For i As Integer = 2 To sqrtmax
            If sieve(i) Then
                For j As Integer= i * i To max Step i
                    sieve(j) = False
                Next
            End If
        Next
        Dim primes(max + 1) As Integer
        Dim pcount As Integer = 0
        For i As Integer = 2 To max
            If sieve(i) Then
                primes(pcount) = i
                pcount += 1
            End If
        Next
 
        Console.WriteLine(primes(pcount - 1))
 
        Console.WriteLine("end")
    End Sub
End Module
```

result:
```
$ dotnet publish -c release -r linux-x64
$ time ./bin/release/net6.0/linux-x64/vbstart
99999989
end

real	0m0.850s
user	0m0.775s
sys	0m0.030s
```


## <a name='anchor3'></a>関数型プログラミングに対するサポートが強いやつ
### <a name='anchor3-0'></a>OCaml
- 実用非純粋関数型言語
- いいところ
  - 速い
  - 変数の再代入が禁止されている（同名変数の再宣言で見た目再代入っぽいことはできる）
  - 

速いはずの言語でもコードがよろしくないと速度もよろしくなくなるのはよくある話である

code:
```ocaml
let main = 
  Printf.printf "start\n";;

  let max = 100000000 in
  let sqrtmax = truncate (sqrt (float_of_int max))
  and sieve = Array.make (max+1) true in

  sieve.(0) <- false;
  sieve.(1) <- false;
  for i = 2 to sqrtmax do
    if sieve.(i) then
      for j = i to Int.div max i do
        sieve.(j*i) <- false;
        ()
      done
    else ()
  done;


  let pcount = ref 0
  and primes = Array.make (max+1) 0 in
  for i = 0 to max do
    if sieve.(i) then begin
      primes.(!pcount) <- i;
      pcount := !pcount+1;
      ()
    end else ()
  done;

  Printf.printf "%d\n" primes.(!pcount - 1);;

  Printf.printf "end\n";;
```

result:
```
$ ocamlopt main.ml -O3
$ time ./a.outstart
99999989
end

real	0m2.106s
user	0m1.727s
sys	0m0.362s
```

### <a name='anchor3-1'></a>Haskell
- 中二病患者向けへそ曲がり実用†純粋†関数型言語
- いいところ
  - 副作用が常に明示され、細かい制御ができる
  - 文法は一応シンプル
  - パッケージマネージャが優秀
  - 機械語から遠そうなわりに非最速組と同程度の速度で動く
  - 自己拡張性が高い
  - 学べば学ぶほど生産性が上がる
  - （すくなくとも世間の噂より）実用性は高い
  - 言語もツールも進化が速い
  - 並列化に強い
- わるいところ
  - 他の言語と設計思想がだいぶ違うので、ハードルは高い
  - 言語もツールも進化が速すぎる
  - 名前だけ物々しい概念が多すぎる
  - 生産性を上げようとすると底なしの勉強を強いられる
  - オフサイドルールはクソ
  - パフォーマンスのチューニングには知識と努力が必要
  - 過去のしがらみが多い（ツールとライブラリと言語拡張の力で殴ってなんとかする）
  - ビルドが遅い（特に初回）
- 有用なリソース
  - 日本Haskellユーザーグループ: https://haskell.jp/ と、そこのslack（初心者にやさしい）

最適化すると十分速いが、知識と試行錯誤が必要

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
import Control.Loop
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
          
          numLoop 2 (double2Int $ sqrt $ fromIntegral max) $ \i -> do
            isprime <- MV.unsafeRead msieve i
            when isprime (numLoop i (max `div` i) (\j -> MV.unsafeWrite msieve (i*j) False))
          
          V.unsafeFreeze msieve
        )

      primes = runST (do
          mprimes <- MV.unsafeNew (max+1)
            
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
$ time ./hs-exestart
99999989
end

real	0m0.809s
user	0m0.766s
sys	0m0.037s
```

### <a name='anchor3-2'></a>Lisp
  - 古代の関数型言語
  - アーティファクト
  - 方言が多い
  - きもいモンスター
  - かっこかっこかっこかっこ

## <a name='anchor4'></a>Web屋さん向け
### <a name='anchor4-0'></a>javascript
- もうぜんぶこれでいいや
- いいところ
  - GUI作りたくなったらHTML+CSS+JSでやるのが一番かんたんかもしれない
  - ブラウザで使える（非常に重要）というより、これを除くとWebAssembry以外の選択肢が存在しない
  - サーバーサイドでもローカルでも使える
  - "スクリプト言語にしては"異常に速い(非最速組と同格)
  - パッケージマネージャが優秀
- わるいところ
  - 型がない。型アノテーションすらない
  - 過去のしがらみが多い（マシになりつつある）
  - 愛すべきカス
- 有用なリソース
  - MDNのチュートリアル: https://developer.mozilla.org/ja/docs/Web/Tutorials
  - 困ったらMDN

TypedArray使ったら最速組と張り合える速さが出た
普通の配列を使ったらメモリ確保ができなくなって落ちた

code:
```js
console.log("start");

const max = 100000000;

let sieve = new Int8Array(max+1);
for(let i=2; i<=max; i++) sieve[i] = 1;

for(let i=2; i<=(Math.floor(Math.sqrt(max)) | 0); i++){
    if(sieve[i]){
        for(let j=i*i; j<=max; j+=i) sieve[j] = false;
    }
}

let primes = new Int32Array(max+1);
let pcount = 0;

for(let i=2; i<=max; i++){
    if(sieve[i]){
        primes[pcount] = i;
        pcount++;
    }
}

console.log(primes[pcount-1]);

console.log("end")
```

result:
```
$ :
$ time node main.jsstart
99999989
end

real	0m0.888s
user	0m0.842s
sys	0m0.036s
```


### <a name='anchor4-1'></a>PHP
- Web屋さん専用のかんたんサーバーサイド言語
- いいところ
  - 動的サイトを作りたいなら一番かんたん
  - HTMLを埋め込むだけなのでクライアントサイドの知識しかなくてもとっつきやすい
  - ホスティングしてくれるサービスが多い(スターサーバーとかエックスサーバーとか)
- わるいところ
  - カオス
{sample:php}

### <a name='anchor4-2'></a>WebAssembly
  - 直接は書かない
  - 他の言語(RustとかC++とか)からWASMにコンパイルしてブラウザで使える

### <a name='anchor4-3'></a>以下星の数ほどあるAltJS(JSにコンパイルされる代替言語)の一部
### <a name='anchor4-4'></a>Typescript
  - AltJSのデファクトスタンダード、型のあるJS
  - 型がある！！！しかも強い！！！！！！！（とても重要）
{sample:ts}
### <a name='anchor4-5'></a>coffeescript
  - Rubyのようななにか
{sample:coffee}
### <a name='anchor4-6'></a>purescript
  - AltJSの異端児、Haskellの生き写し
{sample:purs}
### <a name='anchor4-7'></a>scala.js
  - scalaがjsにコンパイルされる
{result:scjs}
### <a name='anchor4-8'></a>GHCjs
  - Haskellがjsにコンパイルされる
{sample:hsjs}
### <a name='anchor4-9'></a>js_of_ocaml
  - OCamlがjsにコンパイルされる
{sample:jsocaml}
{sample:ocjs}

## <a name='anchor5'></a>統計とかシミュレーションに使うやつ
### <a name='anchor5-0'></a>R
- いいところ
  - 統計にめっちゃ強い。デファクトスタンダード。どうせやらされる（文系含む）
  - 検定がコマンド一発でできる
  - 図が綺麗で細かく指定できる
  - データフレームが使いやすい
  - 機械学習のライブラリが多い
  - パッケージマネージャが優秀
- わるいところ
  - 複雑なことをすると遅い
  - カオス

code:
```r
main <- function(){
    print("start")
    
    max <- 100000000
    
    sieve <- rep(TRUE, max)
    
    sieve[1] = FALSE
    
    for(i in 1:floor(sqrt(max))){
        if(sieve[i]){
            #for(j in seq(i*i,max,by=i)) sieve[j] = FALSE
            sieve[(i:(max%/%i))*i] = FALSE
        }
    }
    
    nums   <- 1:max
    primes <- nums[sieve[nums]]
    
    print(primes[length(primes)])
    
    print("end")
}

main()
```

result:
```
$ :
$ time Rscript main.r[1] "start"
[1] 99999989
[1] "end"

real	0m3.288s
user	0m2.547s
sys	0m0.724s
```

### <a name='anchor5-1'></a>MATLAB
- いいところ
  - 数式とかシミュレーションが強い
  - グラフィクスが強い
- わるいところ
  - 有料(東大生は大学がアカウントくれるので使える)
  - そんなに速くはない
### <a name='anchor5-2'></a>Fortran
- いいところ
  - 数値計算に強い。めっちゃ強い。
  - 速い
  - 並列化がすごく簡単（プログラムに2行付け足してコンパイラに渡すフラグを一個増やすだけ）
  - スパコンでよく使われている
- わるいところ
  - 文法が独特
  - さすがにつくりが古くてつらい

なにを間違えたのか大して速くならなかった

code:
```f95
program eratosthenes
    implicit none
    
    integer,parameter :: pmax = 100000000
    integer :: i
    integer :: j
    integer :: pcount = 1
    logical,allocatable,dimension(:) :: sieve
    integer,allocatable,dimension(:) :: primes
    allocate(sieve(pmax))
    sieve=.true.
    allocate(primes(pmax))
    primes=0
   
    print *, "start"
    do i=2 , int(sqrt(real(pmax)))
        if(sieve(i))then
            !$omp parallel do
            do j = i , pmax/i
                sieve(j*i) = .false.
            end do
            !$omp end parallel do
        end if
    end do

    do i=2 , pmax
        if(sieve(i))then
            primes(pcount) = i
            pcount = pcount + 1
        end if
    end do

    print *, primes(pcount-1)

    print *, "end"
end program eratosthenes
```

result:
```
$ gfortran -fopenmp -Ofast main.f95
$ time ./a.out start
    99999989
 end

real	0m0.859s
user	0m9.485s
sys	0m0.127s
```


## <a name='anchor6'></a>実行環境について
サンプルコードの実行時間は以下の環境で計測しました
- CPU: Ryzen 7 PRO 4750G
- メモリ: 32GB
- OS: Manjaro Linux

## <a name='anchor7'></a>速度ランキング（あんまり参考にならない）
おことわり：今回題材とした「素数の計算」は比較的単純なコードなので、一部言語を除いて実際以上に似たりよったりな結果になっています。どちらかといえば「どの言語が速いか」より「どのサンプルがうまく書けているか」のほうがだいぶ影響が大きそうです。

実行時間：
| rank | lang | time | ratio | 
| - | - | - | - |
| 1 | Rust | 0.60 sec. |1.00x |
| 2 | Assembly | 0.60 sec. |1.00x |
| 3 | C++ | 0.64 sec. |1.06x |
| 4 | C | 0.73 sec. |1.22x |
| 5 | Haskell | 0.81 sec. |1.34x |
| 6 | Julia | 0.83 sec. |1.39x |
| 7 | C# | 0.84 sec. |1.40x |
| 8 | VB.net | 0.85 sec. |1.41x |
| 9 | Fortran | 0.86 sec. |1.43x |
| 10 | Crystal | 0.88 sec. |1.46x |
| 11 | JS | 0.89 sec. |1.48x |
| 12 | Java | 0.98 sec. |1.62x |
| 13 | Python | 1.73 sec. |2.87x |
| 14 | OCaml | 2.11 sec. |3.50x |
| 15 | F# | 2.35 sec. |3.90x |
| 16 | R | 3.29 sec. |5.46x |
| 17 | Cython | 3.67 sec. |6.10x |
| 18 | PyPy | 4.57 sec. |7.59x |
| 19 | Ruby | 19.92 sec. |33.10x |

CPU時間：
| rank | lang | time | ratio | 
| - | - | - | - |
| 1 | C++ | 0.52 sec. |1.00x |
| 2 | Rust | 0.58 sec. |1.10x |
| 3 | Assembly | 0.60 sec. |1.14x |
| 4 | C | 0.71 sec. |1.35x |
| 5 | Julia | 0.74 sec. |1.40x |
| 6 | Haskell | 0.77 sec. |1.46x |
| 7 | VB.net | 0.78 sec. |1.48x |
| 8 | C# | 0.78 sec. |1.48x |
| 9 | Crystal | 0.84 sec. |1.60x |
| 10 | JS | 0.84 sec. |1.60x |
| 11 | Java | 0.85 sec. |1.62x |
| 12 | Python | 1.64 sec. |3.12x |
| 13 | OCaml | 1.73 sec. |3.29x |
| 14 | F# | 2.20 sec. |4.19x |
| 15 | R | 2.55 sec. |4.85x |
| 16 | Cython | 3.81 sec. |7.25x |
| 17 | PyPy | 4.03 sec. |7.67x |
| 18 | Fortran | 9.48 sec. |18.07x |
| 19 | Ruby | 19.62 sec. |37.37x |


## <a name='anchor8'></a>貢献者一覧
- メタリックはんぺん 
  - 説明: C, C++, Rust, Python, Haskell, Fortran, JS, PHP, VB.net, C#, Java
  - サンプル: C, C++, Rust, Python, Haskell, Fortran, JS, R, VB.net, C#, F#, OCaml, Java
  - 一言: Haskellはいい言語ですよ、やれ！お前も蓮沼に落ちろ！！！
- あなばす
  - 説明: Julia, Lisp, R, MATLAB, Fortran
  - サンプル: Julia
  - 一言: Julia最高！Juliaしか勝たん！
- 綿谷 雫
  - サンプル: Fortran
  - 一言: 古典を学ぶことは物事の根幹に触れることであり，そこから派生してできたものの理解が深まります．古典語をやりましょう．
- TumoiYorozu
  - サンプル: Assembly
  - 一言: 生のアセンブリ、もう書かない
- ふぁぼん
  - 説明: Ruby, Crystal
  - サンプル: Ruby, Crystal
  - 一言: なんだかんだRubyはいい言語だと思う。コードゴルフにも向いてるし。
  