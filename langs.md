# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
この記事では、「これからプログラミングを学習したいけど何をやったらいいかわからないっ！」「新しくプログラミング言語を学びたいのでおすすめを知りたいっ！」というひとたちのために、各言語の特徴と(題材が簡単すぎて各言語の味がしない)コードサンプルを掲載しています。改善･修正の提案は https://github.com/boletales/which-lang-to-learn で常に受け付けているので、推し言語を布教したいみなさんはぜひプルリクを投げつけてください！

## 目次
- <a href='#anchor0'>そもそもプログラミング言語ってどうやって学べばいいのさ</a>

- <a href='#anchor1'>用語解説</a>

- <a href='#anchor2'>スクリプトみの強いやつ</a>
  - <a href='#anchor2-0'>Python</a>
  - <a href='#anchor2-1'>Perl</a>
  - <a href='#anchor2-2'>Ruby</a>
  - <a href='#anchor2-3'>Julia</a>

- <a href='#anchor3'>コンパイルして使うやつ</a>
  - <a href='#anchor3-0'>C</a>
  - <a href='#anchor3-1'>C++</a>
  - <a href='#anchor3-2'>Rust</a>
  - <a href='#anchor3-3'>Crystal</a>
  - <a href='#anchor3-4'>Assembly</a>

- <a href='#anchor4'>クラスベースオブジェクト指向一味</a>
  - <a href='#anchor4-0'>Java</a>
  - <a href='#anchor4-1'>C#</a>
  - <a href='#anchor4-2'>VB.net</a>

- <a href='#anchor5'>関数型プログラミングに対するサポートが強いやつ</a>
  - <a href='#anchor5-0'>OCaml</a>
  - <a href='#anchor5-1'>F#</a>
  - <a href='#anchor5-2'>Scala</a>
  - <a href='#anchor5-3'>Haskell</a>
  - <a href='#anchor5-4'>Lisp</a>

- <a href='#anchor6'>Web屋さん向け</a>
  - <a href='#anchor6-0'>javascript</a>
  - <a href='#anchor6-1'>PHP</a>
  - <a href='#anchor6-2'>WebAssembly</a>
  - <a href='#anchor6-3'>Typescript</a>
  - <a href='#anchor6-4'>coffeescript</a>
  - <a href='#anchor6-5'>purescript</a>
  - <a href='#anchor6-6'>scala.js</a>
  - <a href='#anchor6-7'>GHCjs</a>
  - <a href='#anchor6-8'>js_of_ocaml</a>

- <a href='#anchor7'>統計とかシミュレーションに使うやつ</a>
  - <a href='#anchor7-0'>R</a>
  - <a href='#anchor7-1'>MATLAB</a>
  - <a href='#anchor7-2'>Fortran</a>

- <a href='#anchor8'>実行環境について</a>

- <a href='#anchor9'>速度ランキング（あんまり参考にならない）</a>

- <a href='#anchor10'>貢献者一覧</a>


## <a name='anchor0'></a>そもそもプログラミング言語ってどうやって学べばいいのさ
新しいプログラミング言語を学びたいときには基本的に以下のような流れで選ぶことになります。
1. 習得の目的を考える
  - なんでもよい
  - 極論「かっこよさそうだから」でもよい（筆者はこれでHaskell使いになってしまった）
  - 本当になにも思いつかないなら潔くPythonやっとけ
2. 言語を選ぶ
  - 深く考えなくてよい。どうせ1年も経たないうちにもう一言語習得する羽目になるのだから
  - とにかく「プログラミング」を学習したい！
    - プログラミングできる友人が使ってる言語。友人のふりがなは当然「サポートデスク」である
    - Python 簡単だし、教養の授業で使わされることがある、という意味で有用だし、ライブラリも多い
    - 「プログラミング教育用」と銘打たれている言語 (Scratch, SmallBasic, HSP, その他たくさん)。有用かどうかはともかくとして学習には向いている
  - シミュレーションとかやりたい
    - Julia 文法が簡単で、配列や数式の扱いが得意で、それなりの速度で動く
    - Rust 速くて素直で安全で汎用。コンパイラは口うるさい。マジで良い言語だけど一言語目にはおすすめしない
    - Fortran スパコンとかでよく使われているらしい
  - Webサイトを作って見せびらかしたい
    - HTML, CSS, JavaScript それぞれ使えないとおはなしにならない。(なおHTMLとCSSはプログラミング言語ではない。)
    - Ruby なんかいろんなところでつかわれてたきがする
    - PHP 将来有望かと言われると首をかしげたくなるが、簡単
  - 自分用の便利用ツールをつくって生活を豊かにしたい
    - スクリプト言語(スクリプトみの強いやつ+JavaScript)のどれか。事前コンパイルが不要(うれしい)で、書き捨てに向いた文法をしていがちなので
    - Haskell, 【心優しいコントリビューターがここにスクリプト扱いできる言語を書いてくれる】 コンパイラ言語だけどスクリプトっぽい使い方もできる。書き味も軽め
    - Rust 他の用途に使い回せるので
    - (初学者なら)VB.net, (Cっぽい文法の言語が既習なら)C# Win民限定だけどGUIをつくるのが簡単
  - ふつうのプログラミングに飽きた
    - Haskell お望み通り副作用すら禁止されてる言語を連れてきたぞ。「結局Haskellもふつうの実用プログラミング言語なんだな」となる頃には飽きも治ってるだろ、たぶん
    - C++ 極めると黒魔術師になれる闇の言語。AtCoderで緑になるぐらいなら全く黒魔術には触れないが
    - 難解プログラミング言語 お前は実用もできないようなプログラミング言語を習得していったい何がしたいんだ？？？
3. 学習をする
  - プログラミング自体はじめてなら本があったほうが安心
  - そうでなければインターネット上の資料だけでなんとかなる
  - 公式のコミュニティとか公然性の高いコミュニティは有用。たとえ英語であっても。
  - 本文の説明欄に知ってるものは載せといたので、読め
4. 1に戻る
  逃れられない運命。どうせこうなる

## <a name='anchor1'></a>用語解説
  - コンパイル
    - プログラムをコンピュータで実行できる形式に変換すること
    - これが遅い言語はちょっとイラッとする(Rust, Haskell, おまえらのことだぞ)
  - コンパイル言語・スクリプト言語
    - コンパイルが必要な言語・不要な言語
    - いまではあまりあてにならない分類
    - 一般的にコンパイル言語のほうが速いが、実行中にコンパイル(JIT)を行うスクリプト言語は比較的速い(JuliaとかJSとか)
    - コンパイル言語のほうが実行前にエラーを出してくれることが多くて親切
  - (データ)型
    - プログラム中の値に与えられたタグで、それが何なのかを表すもの
    - 整数、真偽値の配列、32ビット浮動小数点数の3つ組、文字列をとって文字列を返す関数、整数をとって「文字をとって文字列を返す関数」を返す関数、など、まあ、いろいろある
    - 型の表現力が強いと「関数を取る関数」「無効な値を返すかもしれない」「これらのうちいずれかになる」みたいなのが表せて嬉しい
  - 静的型付け・動的型付け
    - 実行される前に値や変数の型が決まっている言語・そうでない言語
    - 動的型付け言語は実行するまでわからないエラーがあったり遅かったりしてややつらい
    - 型推論のない静的型付け言語は自分で型を明示しないといけないので面倒
    - 逆に、型推論の強いやつはほぼ自分で型を書かなくても動いたりする（けどコード読むときに拡張機能つかわないと型わからないのつらいので書いてくれ）
    - __静的型付けの言語は最低一つ習得するべき__
  - オブジェクト指向
    - プログラムの書き方･設計の方針の一種
      - 物事を、固有のデータ「フィールド」と固有の機能「メソッド」を持つ「オブジェクト」に切り分ける
      - プログラム全体を「オブジェクト」の相互作用として記述する
    - いくつか流派がある
      - クラスベース：かっちりしたオブジェクトの雛型「クラス」や元になるクラスに新たな機能を持たせる「継承」、複数のクラスが共通して持つデータ･機能を表す「インターフェース」を用いる
      - プロトタイプベース：クラスより緩いひな形「プロトタイプ」や継承に似た「プロトタイプチェーン」を用いる
    - 一時期めっちゃ流行った
  - 手続き型プログラミング
    - プログラムの書き方の一種
    - プログラムを逐次的な処理の流れとして表すもの
    - 手続きが縦に伸びていくイメージ
    - 後述の「関数型プログラミング」とは対になりがち
    - だいたいどんなプログラミング言語でもできる
  - 関数型プログラミング
    - プログラムの書き方の一種
    - プログラムを大きな関数の適用として表すもの
    - 式が横に伸びていくイメージ（実際には行を分割して書くけど）
    - 関数型プログラミングに対するサポートが手薄な言語でやろうとすると辛い
    - 最低でも言語に以下の特徴がほしい
      - 関数が第一級オブジェクト（関数の引数として渡したり変数に代入したりできる）であること
      - 匿名の関数を簡単に宣言できること
      - (静的型付けなら)関数を表す型が存在すること
    - 可能なら次のような特徴もほしい
      - 標準ライブラリに「関数を引数に取る関数」（高階関数）が多く含まれていること
      - 標準ライブラリに「関数を引数に取る関数」（高階関数）が多く含まれていること
      - (静的型付けなら)型システムが強力であること。高階関数の型は往々にして複雑になるので
      - (静的型付けなら)型推論が強いこと。複雑な型を手書きしたくはないので
      - 「名前付き定数を束縛して式中で使う」みたいな文が用意されていること。ローカルconstでもいいけどスコープが広がって面倒なので
    - ↑のような特徴を多く持つ、関数型プログラミングでコードを書くことを目的に設計されたプログラミング言語は「関数型プログラミング言語」と呼ばれる

## <a name='anchor2'></a>スクリプトみの強いやつ
### <a name='anchor2-0'></a>Python
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
$ time python main_nd.py
start
99999989
end

real	0m1.772s
user	0m1.675s
sys	0m1.153s
```

普通のPythonのfor文は遅いが、PyPyで実行するとだいぶマシになる(ただしnumpyは使えない)

result:
```
$ :
$ time pypy main.py
start
99999989
end

real	0m4.081s
user	0m3.446s
sys	0m0.545s
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
$ time python cymain.py
start
5761455
end

real	0m3.831s
user	0m3.923s
sys	0m0.944s
```


### <a name='anchor2-1'></a>Perl
- 古参スクリプト言語
- いいところ
  - 文字列処理が得意
- わるいところ
  - おそい

code:
```pl
print("start\n");

my $max = 100000000;
my $sqrtmax = sqrt($max);
my @sieve = (1) x ($max+1);
$sieve[0] = 0;
$sieve[1] = 0;
for($i = 0; $i <= $sqrtmax; $i++){
  if($sieve[$i]){
    for($j = $i*$i; $j <= $max; $j += $i){
      $sieve[$j] = 0;
    }
  }
}

my @primes;
my $pcount = 0;
for($i = 0; $i <= $max; $i++){
  if($sieve[$i]){
    @primes[$pcount] = $i;
    $pcount++;
  }
}

print($primes[$pcount-1]);
print("\n");

print("end\n");
```

result:
```
$ :
$ time perl main.pl
start
99999989
end

real	0m33.246s
user	0m32.045s
sys	0m1.114s
```


### <a name='anchor2-2'></a>Ruby
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

一応、Pythonにおけるnumpyに対応するnumoというライブラリが存在するため、numpyで高速化できる処理ならまあ、なんとかなる(numoはAtCoderでも使える)。ユーザが少なすぎてググっても情報出ないし、開発も活発とはとても言えない状況だが……。

code:
```rb
require 'numo/narray'

puts "start"

MAX = 100000000
sieve = Numo::Bit.ones(MAX + 1)
sieve[0] = sieve[1] = 0

2.upto(Integer.sqrt(MAX)) do |i|
  sieve[((i * i)..-1) % i] = 0 if sieve[i]
end

primes = Numo::Int32.new(MAX + 1).seq(0, 1)[sieve[0..MAX]]
puts primes[-1], "end"
```

result:
```
$ bundle
$ time bundle exec ruby main.rb
start
99999989
end

real	0m4.834s
user	0m4.594s
sys	0m0.149s
```

Rubyの高速化テクはいろいろあるが、とりあえず各種メソッドによるループをwhileに変換するとかなり速くなる(ブロックはオーバーヘッドが大きい)。もっとも、while文を使うと「これRubyでやる意味ある?」という感じになるし、それなら他の言語を使った方がいい(ネイティブ拡張を書くという選択肢は一応ある)。何度も言うがRubyistのサブ言語としてCrystalマジでオススメ。

### <a name='anchor2-3'></a>Julia
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
$ time julia main.jl
start
99999989
end

real	0m0.840s
user	0m0.761s
sys	0m0.076s
```


## <a name='anchor3'></a>コンパイルして使うやつ
### <a name='anchor3-0'></a>C
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
$ time ./a.out
start
99999989
end

real	0m0.769s
user	0m0.726s
sys	0m0.040s
```


### <a name='anchor3-1'></a>C++
- 自分のことを最新鋭だと思っている古参言語
- いいところ
  - 最速組。とにかく速い2
  - メモリを直接扱える2
  - 現代的な機能がたくさん追加されている
- わるいところ
  - 何もかも自己責任
  - 初学者には難しい
- Cにない型がいっぱいある

コンパイル時に最適化フラグを渡さないと10倍以上遅くなってしまった(一敗)

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
$ time ./a.out
start
99999989
start

real	0m0.672s
user	0m0.560s
sys	0m0.109s
```

  
### <a name='anchor3-2'></a>Rust
- マルチパラダイム言語界の新星 新時代の勝者……！？
- いいところ
  - 最速組。とにかく速い3
  - メモリを直接扱えるが、直接扱わずに済む！！！ココ重要！！
  - メモリ安全
  - 型の力でより安全なコードが書ける
  - 標準ライブラリに文句がない
  - エラーハンドリングする人にやさしい（型レベルでハンドルする）
  - 他の言語から大量に有用な機能を輸入している
  - コンパイラが世話を焼いてくれる
  - マクロが別言語を実装できるレベルで自由度高い
  - パッケージマネージャが優秀
  - 並列化に強い
  - コミュニティの民度が高い
- わるいところ
  - 難易度が、高い。（さすがに一言語目にはおすすめできない）
  - コンパイラが口うるさい
  - エラーハンドリングしない人に厳しい
  - ビルドが遅い（特に初回）
- 有用なリソース
  - The Rust Programming Language (和訳): https://doc.rust-jp.rs/book-ja/
  - crates.io: https://crates.io/ パッケージの情報がまとまっています

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
            for j in i..=MAX/i {
                sieve[j*i] = false;
            }
        }
    }

    let mut primes = vec![0; MAX+1];
    let mut pcount = 0;
    for i in 2..=MAX {
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
$ rustc -O main.rs
$ time ./main
start
99999989
end

real	0m0.786s
user	0m0.736s
sys	0m0.047s
```


sieveにbit演算を用いたら非常に高速になった

code:
```rs
const MAX: usize = 100000000;
fn main() {
    println!("start");

    let mut sieve = [0; MAX / 64 + 1];
    sieve[0] ^= 3;
    let sqrtmax = f32::sqrt(MAX as f32) as usize;
    for i in 2..=sqrtmax {
        if (sieve[i / 64] >> i % 64) & 1 == 0 {
            let mut j = i * i;
            while j <= MAX {
                sieve[j / 64] |= 1 << j % 64;
                j += i;
            }
        }
    }

    let mut primes = Vec::new();
    for i in 2..=MAX {
        if (sieve[i / 64] >> i % 64) & 1 == 0 {
            primes.push(i);
        }
    }
    println!("{}", primes.last().unwrap());
    println!("end");
}
```

result:
```
$ rustc -O bit.rs
$ time ./bit
start
99999991
end

real	0m0.307s
user	0m0.287s
sys	0m0.010s
```


### <a name='anchor3-3'></a>Crystal
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
$ time ./main
start
99999989
end

real	0m0.883s
user	0m0.826s
sys	0m0.053s
```


### <a name='anchor3-4'></a>Assembly
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
$ time ./a.out
start
99999989
end
count 5717621


real	0m0.664s
user	0m0.652s
sys	0m0.010s
```


## <a name='anchor4'></a>クラスベースオブジェクト指向一味

### <a name='anchor4-0'></a>Java
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
$ time java Primes
start
99999989
end

real	0m1.001s
user	0m0.875s
sys	0m0.156s
```


### <a name='anchor4-1'></a>C#
- MS製のJavaのようななにか
- いいところ
  - コードの規模が大きくなることに大して耐性が強い2
  - Win向けのGUIアプリを書くのが簡単
  - Unityで使うらしい
- わるいところ
  - コードが冗長2(Javaよりややマシかも)
- 有用なリソース
  - Visual Studio: https://visualstudio.microsoft.com/ja/free-developer-offers/ win向けなら必須。

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
$ time ./bin/release/net6.0/linux-x64/cs
start
99999989
end

real	0m0.991s
user	0m0.903s
sys	0m0.049s
```


### <a name='anchor4-2'></a>VB.net
- C#(VisualBasic風味)
- いいところ
  - 文法が初学者にもわかりやすい(Basicの血を引いているだけある)
  - C#との相互運用が容易。めっちゃ容易。
  - Win向けのGUIアプリを書くのが簡単2
  - コードの規模が大きくなることに大して耐性が強い3
  - C#
- わるいところ
  - Windows以外での使いみちがあまりない
  - 構文が冗長なので補完がないとつらい(Visual Studioで書く分には補完がはちゃめちゃに強いので気にならない)
  - 不遇
  - C#
- 有用なリソース
  - Visual Studio: https://visualstudio.microsoft.com/ja/free-developer-offers/ 実質必須。

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
$ time ./bin/release/net6.0/linux-x64/vb
start
99999989
end

real	0m0.900s
user	0m0.793s
sys	0m0.056s
```


## <a name='anchor5'></a>関数型プログラミングに対するサポートが強いやつ
### <a name='anchor5-0'></a>OCaml
- 実用非純粋関数型言語
- いいところ
  - 速い(と聞いているのでだれか最適化してくれ)
  - 変数の再代入が禁止されている（同名変数の再宣言で見た目再代入っぽいことはできる）
  - 型の力が強い、型推論がめっちゃ強い

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
$ time ./a.out
start
99999989
end

real	0m2.186s
user	0m1.784s
sys	0m0.392s
```


### <a name='anchor5-1'></a>F#
- MS製のF#のようななにか
- いいところ
  - .NET一族唯一の関数型枠
  - ほかの.NET族との連携ができる
- 有用なリソース
  - Visual Studio: https://visualstudio.microsoft.com/ja/free-developer-offers/ win向けなら必須。

改善求む2

code:
```fs
﻿printfn "start"

let max = 100000000
let sqrtmax = int (sqrt (double max))
let mutable sieve = Array.create (max+1) true

sieve[0] <- false
sieve[1] <- false
for i in 2 .. sqrtmax do
  if sieve[i] then
    for j in i*i .. i .. max do
      sieve[j] <- false
    ()

let mutable primes = Array.create (max+1) 0
let mutable pcount = 0
for i in 2 .. max do
  if sieve[i] then
    primes[pcount] <- i
    pcount <- pcount+1
    ()

printfn "%d" primes[pcount-1]

printfn "end"
```

result:
```
$ dotnet publish -c release -r linux-x64
$ time ./bin/release/net6.0/linux-x64/fs
start
99999989
end

real	0m2.418s
user	0m2.239s
sys	0m0.133s
```


### <a name='anchor5-2'></a>Scala
- Java族の関数型言語
- いいところ
  - Javaとの連携ができる

改善求む3

code:
```scala
import scala.math.sqrt

object prime {
  def main(args: Array[String]) : Unit = {
    println("start")
    val max = 100000000
    val sqrtmax = sqrt(max).toInt
    var sieve = Array.fill[Boolean](max+1)(true)
    sieve(0) = false
    sieve(1) = false
    for (i <- 2 to sqrtmax){
      if(sieve(i)){
        for (j <- i*i to max by i){
          sieve(j) = false
        }
      }
    }

    var primes = new Array[Int](max+1) 
    var pcount = 0
    for (i <- 2 to max){
      if(sieve(i)){
        primes(pcount) = i
        pcount += 1
      }
    }

    println(primes(pcount-1))

    println("end")
  }
}
```

result:
```
$ scalac main.scala
$ time scala prime -J-Xmx1g
start
99999989
end

real	0m2.249s
user	0m2.519s
sys	0m0.232s
```


### <a name='anchor5-3'></a>Haskell
- 中二病患者向けへそ曲がり実用†純粋†関数型言語
- いいところ
  - 副作用が常に明示され、細かい制御ができる
  - パッケージマネージャが優秀
  - 機械語から遠そうなわりに非最速組と同程度の速度で動く
  - 型の力が強い、型推論がめっちゃ強い2
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
  - GHCup: https://www.haskell.org/ghcup/ バージョン管理ツール。開発に必要なソフト一式を簡単に導入できます
  - Hoogle: https://hoogle.haskell.org/ 型を入力するとそれに当てはまる関数を検索できます。死ぬほど便利。

最適化すると十分速いが、知識と試行錯誤が必要

code:
```hs
module MVector where

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
$ time ./hs-exe
start
99999989
end

real	0m0.791s
user	0m0.741s
sys	0m0.047s
```


Haskellらしく純粋なデータ構造だけで書いても動きはするが、遅かった

code:
```hs
-- 筆者の環境で20~25秒

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Immutable where

import Lib
import qualified Data.Vector.Unboxed as V
import Data.Foldable
import Control.Monad
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
  let sieve = 
        foldl (\v i -> 
            if V.unsafeIndex v i
            --then V.unsafeUpdate_ v (V.generate (max `div` i - i + 1) (\j -> i * (i + j))) (V.replicate (max `div` i - i  + 1) False)
            then v V.// ((,False) <$> [i*i,i*i+i..max])
            else v
          ) (V.replicate (max+1) True V.// [(0,False),(1,False)]) [2..(double2Int $ sqrt $ fromIntegral max)]

  in V.filter (V.unsafeIndex sieve) (V.generate (max+1) id)
```



### <a name='anchor5-4'></a>Lisp
  - 古代の関数型言語
  - アーティファクト
  - 方言が多い
  - きもいモンスター
  - かっこかっこかっこかっこ

## <a name='anchor6'></a>Web屋さん向け
### <a name='anchor6-0'></a>javascript
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
  - コミュニティの民度がカス
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
$ time node main.js
start
99999989
end

real	0m0.910s
user	0m0.845s
sys	0m0.066s
```


### <a name='anchor6-1'></a>PHP
- Web屋さん専用のかんたんサーバーサイド言語
- いいところ
  - 動的サイトを作りたいなら一番かんたん
  - HTMLを埋め込むだけなのでクライアントサイドの知識しかなくてもとっつきやすい
  - ホスティングしてくれるサービスが多い(スターサーバーとかエックスサーバーとか)
- わるいところ
  - カオス
{sample:php}

### <a name='anchor6-2'></a>WebAssembly
- 直接は書かない
- 他の言語(RustとかC++とか)からWASMにコンパイルしてブラウザで使える
- （一応直接書けるが、その名の通りAssembly級にわけわからない）

### <a name='anchor6-3'></a>Typescript
- AltJS(JSにコンパイルされる代替言語)のデファクトスタンダード、型のあるJS
- 特徴
  - 構造的部分型
    - クラスの継承ベースではなく，現にプロパティの型があっているかで代入可能性を判断する
  - リテラル型（`'world'`だけが代入できる型，のような）がある
  - 非常に複雑な型定義が可能
- いいところ
  - 型がある！！！しかも強い！！！！！！！（とても重要）
  - 型推論強め（型を手書きするのは基本的に関数の引数だけ）
  - Microsoft製でサポート手厚い
  - コミュニティの民度がJavaScriptより幾分良い
- わるいところ
  - 型はあくまで「飾り」でしかない（ランタイムでは無視される）
  - けしからんやつが使うと型の恩恵を全く受けられない（`any`滅ぶべし慈悲はない）
- 豆知識・備考
  - 型システムだけでチューリング完全である（brainfxxkを実装できる）
  - いまどきのJavaScriptパッケージは必ずと言っていいほどTypeScriptで書かれている
    - 型がないとパッケージとして使いにくいため
  - 実はVSCodeでJavaScriptを書くときはコード補完の恩恵をTypeScriptから受けている
  - 型推論のおかげでコードがJavaScriptとほぼ変わらないのでサンプルなし
  - 書いてる最中の型チェック・コード補完が死ぬほど速い（言語全体がその意図のもと設計されている）

### <a name='anchor6-4'></a>coffeescript
- AltJSその2
- Rubyのようななにか

### <a name='anchor6-5'></a>purescript
- AltJSその3
- AltJSの異端児、Haskellの生き写し

### <a name='anchor6-6'></a>scala.js
- AltJSその4
- scalaがjsにコンパイルされる

### <a name='anchor6-7'></a>GHCjs
- AltJSその5
- Haskellがjsにコンパイルされる

### <a name='anchor6-8'></a>js_of_ocaml
- AltJSその6
- OCamlがjsにコンパイルされる

## <a name='anchor7'></a>統計とかシミュレーションに使うやつ
### <a name='anchor7-0'></a>R
- いいところ
  - 統計にめっちゃ強い。デファクトスタンダード。どうせやらされる（文系含む）
  - 検定がコマンド一発でできる
  - 図が綺麗で細かく指定できる
  - データフレームが使いやすい
  - 機械学習のライブラリが多い
  - パッケージマネージャが優秀
- わるいところ
  - 複雑なことをすると遅い
  - メソッドの呼び出し方が`method(target)`
  - 1-indexed, 縦基本行列
  - カオス
- 有用なリソース
  - 奥村研究室: https://oku.edu.mie-u.ac.jp/~okumura/stat/ もう三重に足を向けて寝られない

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
$ time Rscript main.r
[1] "start"
[1] 99999989
[1] "end"

real	0m3.682s
user	0m2.724s
sys	0m0.934s
```


### <a name='anchor7-1'></a>MATLAB
- いいところ
  - 数式とかシミュレーションが強い
  - グラフィクスが強い
- わるいところ
  - 有料(東大生は大学がアカウントくれるので使える)
  - そんなに速くはない

### <a name='anchor7-2'></a>Fortran
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
$ time ./a.out
 start
    99999989
 end

real	0m1.267s
user	0m14.571s
sys	0m0.189s
```


## <a name='anchor8'></a>実行環境について
サンプルコードの実行時間は以下の環境で計測しました
- CPU: Ryzen 7 PRO 4750G
- メモリ: 32GB
- OS: Manjaro Linux

## <a name='anchor9'></a>速度ランキング（あんまり参考にならない）
おことわり：今回題材とした「素数の計算」は比較的単純なコードなので、一部言語を除いて実際以上に似たりよったりな結果になっています。どちらかといえば「どの言語が速いか」より「どのサンプルがうまく書けているか」のほうがだいぶ影響が大きそうです。それぞれの言語について、速度を大きく改善するようなライブラリや機能を用いたものはカッコ書きで付記しました。

実行時間：
| rank | lang | time | ratio | 
| - | - | - | - |
| 1 | Rust (bit operation) | 0.31 sec. |1.00x |
| 2 | Assembly | 0.66 sec. |2.16x |
| 3 | C++ | 0.67 sec. |2.19x |
| 4 | C | 0.77 sec. |2.50x |
| 5 | Rust | 0.79 sec. |2.56x |
| 6 | Haskell (MVector) | 0.79 sec. |2.58x |
| 7 | Julia | 0.84 sec. |2.74x |
| 8 | Crystal | 0.88 sec. |2.88x |
| 9 | VB.net | 0.90 sec. |2.93x |
| 10 | JavaScript (TypedArray) | 0.91 sec. |2.96x |
| 11 | C# | 0.99 sec. |3.23x |
| 12 | Java | 1.00 sec. |3.26x |
| 13 | Fortran (parallel) | 1.27 sec. |4.13x |
| 14 | Python (numpy) | 1.77 sec. |5.77x |
| 15 | OCaml | 2.19 sec. |7.12x |
| 16 | Scala | 2.25 sec. |7.33x |
| 17 | F# | 2.42 sec. |7.88x |
| 18 | R | 3.68 sec. |11.99x |
| 19 | Cython (numpy) | 3.83 sec. |12.48x |
| 20 | PyPy | 4.08 sec. |13.29x |
| 21 | Ruby (numo) | 4.83 sec. |15.75x |
| 22 | Perl | 33.25 sec. |108.29x |

CPU時間：
| rank | lang | time | ratio | 
| - | - | - | - |
| 1 | Rust (bit operation) | 0.29 sec. |1.00x |
| 2 | C++ | 0.56 sec. |1.95x |
| 3 | Assembly | 0.65 sec. |2.27x |
| 4 | C | 0.73 sec. |2.53x |
| 5 | Rust | 0.74 sec. |2.56x |
| 6 | Haskell (MVector) | 0.74 sec. |2.58x |
| 7 | Julia | 0.76 sec. |2.65x |
| 8 | VB.net | 0.79 sec. |2.76x |
| 9 | Crystal | 0.83 sec. |2.88x |
| 10 | JavaScript (TypedArray) | 0.84 sec. |2.94x |
| 11 | Java | 0.88 sec. |3.05x |
| 12 | C# | 0.90 sec. |3.15x |
| 13 | Python (numpy) | 1.68 sec. |5.84x |
| 14 | OCaml | 1.78 sec. |6.22x |
| 15 | F# | 2.24 sec. |7.80x |
| 16 | Scala | 2.52 sec. |8.78x |
| 17 | R | 2.72 sec. |9.49x |
| 18 | PyPy | 3.45 sec. |12.01x |
| 19 | Cython (numpy) | 3.92 sec. |13.67x |
| 20 | Ruby (numo) | 4.59 sec. |16.01x |
| 21 | Fortran (parallel) | 14.57 sec. |50.77x |
| 22 | Perl | 32.04 sec. |111.66x |


## <a name='anchor10'></a>貢献者一覧
- メタリックはんぺん 
  - 説明: C, C++, Rust, Python, Haskell, Fortran, JS, PHP, VB.net, C#, Java
  - サンプル: C, C++, Rust, Python, Haskell, Fortran, JS, R, VB.net, C#, F#, OCaml, Java, Julia
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
- 🌱🌿☘️🍀
  - 説明: Rust, TypeScript(, R)
  - サンプル: Rust (bit operation)
  - 一言: Rustの真価はWebサーバーなどのシビアな用途で初めて発揮される．
- femshima(Nanigashi)
  - サンプル: C
  - 一言: いつか組み込みのasmを書けるようになりたい(願望)
  