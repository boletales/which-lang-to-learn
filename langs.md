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

<a href='#anchor0'></a>
## スクリプトみの強いやつ# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
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

<a href='#anchor0-0'></a>
### Python
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

高速なライブラリ(numpyとか)を使えば十分な速度を出せる
{sample:py}
普通のPythonのfor文は遅いが、PyPyで実行するとだいぶマシになる(ただしnumpyは使えない)
{result:pypy}
Cythonで重い部分をCに変換しても速くなる

code:
```py
import cylib

print("start")
primes = cylib.main()
print(primes[-1])
print("end")
```

{sample:cy}
<a href='#anchor0-1'></a>
### Ruby
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
{sample:rb}
<a href='#anchor0-2'></a>
### Julia
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
{sample:jl}
<a href='#anchor1'></a>
## コンパイルして使うやつ# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
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

<a href='#anchor0'></a>
## スクリプトみの強いやつ# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
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

<a href='#anchor0-0'></a><a href='#anchor1-0'></a>
### C
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
{sample:c}
<a href='#anchor1-1'></a>
### C++
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
{sample:cpp}
  <a href='#anchor1-2'></a>
### Rust
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
{sample:rs}
<a href='#anchor1-3'></a>
### Crystal
- いいところ
  - 最速組。とにかく速い4
  - Rubyの書きやすさとCの速度が合わさり完璧に見える
  - 静的型付け、型推論が強い(というか明示的に型を書く必要がほぼない)
  - Rubyのいいところはだいたい受け継いでいる
- わるいところ
  - ビルドが遅い
  - ライブラリがまだまだ少ない
- Ruby風の言語ではあるが、文法は多少違うし、標準ライブラリにもそれなりに差異があるので注意
{sample:cr}
<a href='#anchor1-4'></a>
### Assembly
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
{sample:asm}
<a href='#anchor2'></a>
## クラスベースオブジェクト指向一味
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

<a href='#anchor0'></a>
## スクリプトみの強いやつ# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
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

<a href='#anchor0-0'></a><a href='#anchor2-0'></a>
### Java
- いいところ
  - コードの規模が大きくなることに大して耐性が強い
- わるいところ
  - コードが冗長
- 特徴
  - 業務で使われがち
  - ザ・クラスベースオブジェクト指向
{sample:java}<a href='#anchor2-1'></a>
### C#
- MS製のJavaのようななにか
- いいところ
  - コードの規模が大きくなることに大して耐性が強い2
  - Win向けのGUIアプリを書くのが簡単
  - Unityで使うらしい
- わるいところ
  - コードが冗長2(Javaよりややマシかも)
{sample:cs}<a href='#anchor2-2'></a>
### VB.net
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
{sample:vb}
<a href='#anchor3'></a>
## 関数型プログラミングに対するサポートが強いやつ# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
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

<a href='#anchor0'></a>
## スクリプトみの強いやつ# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
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

<a href='#anchor0-0'></a><a href='#anchor3-0'></a>
### OCaml
- 五十歩百歩組3
- ML族
- 非純粋
- 静的型、型推論強い
{sample:ocaml}<a href='#anchor3-1'></a>
### Haskell
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
{sample:hs}<a href='#anchor3-2'></a>
### Lisp
  - 古代の関数型言語
  - アーティファクト
  - 方言が多い
  - きもいモンスター
  - かっこかっこかっこかっこ
<a href='#anchor4'></a>
## Web屋さん向け# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
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

<a href='#anchor0'></a>
## スクリプトみの強いやつ# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
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

<a href='#anchor0-0'></a><a href='#anchor4-0'></a>
### javascript
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
{sample:js}
<a href='#anchor4-1'></a>
### PHP
- Web屋さん専用のかんたんサーバーサイド言語
- いいところ
  - 動的サイトを作りたいなら一番かんたん
  - HTMLを埋め込むだけなのでクライアントサイドの知識しかなくてもとっつきやすい
  - ホスティングしてくれるサービスが多い(スターサーバーとかエックスサーバーとか)
- わるいところ
  - カオス
{sample:php}
<a href='#anchor4-2'></a>
### WebAssembly
  - 直接は書かない
  - 他の言語(RustとかC++とか)からWASMにコンパイルしてブラウザで使える
<a href='#anchor4-3'></a>
### 以下星の数ほどあるAltJS(JSにコンパイルされる代替言語)の一部<a href='#anchor4-4'></a>
### Typescript
  - AltJSのデファクトスタンダード、型のあるJS
  - 型がある！！！しかも強い！！！！！！！（とても重要）
{sample:ts}<a href='#anchor4-5'></a>
### coffeescript
  - Rubyのようななにか
{sample:coffee}<a href='#anchor4-6'></a>
### purescript
  - AltJSの異端児、Haskellの生き写し
{sample:purs}<a href='#anchor4-7'></a>
### scala.js
  - scalaがjsにコンパイルされる
{result:scjs}<a href='#anchor4-8'></a>
### GHCjs
  - Haskellがjsにコンパイルされる
{sample:hsjs}<a href='#anchor4-9'></a>
### js_of_ocaml
  - OCamlがjsにコンパイルされる
{sample:jsocaml}
{sample:ocjs}
<a href='#anchor5'></a>
## 統計とかシミュレーションに使うやつ# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
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

<a href='#anchor0'></a>
## スクリプトみの強いやつ# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
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

<a href='#anchor0-0'></a><a href='#anchor5-0'></a>
### R
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
{sample:r}<a href='#anchor5-1'></a>
### MATLAB
- いいところ
  - 数式とかシミュレーションが強い
  - グラフィクスが強い
- わるいところ
  - 有料(東大生は大学がアカウントくれるので使える)
  - そんなに速くはない<a href='#anchor5-2'></a>
### Fortran
- いいところ
  - 数値計算に強い。めっちゃ強い。
  - 速い
  - スパコンでよく使われている
- わるいところ
  - 文法が独特
  - さすがにつくりが古くてつらい

なにを間違えたのか大して速くならなかった
{sample:f95}
<a href='#anchor6'></a>
## 実行環境について
サンプルコードの実行時間は以下の環境で計測しました
- CPU: Ryzen 7 PRO 4750G
- メモリ: 32GB
- OS: Manjaro Linux
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

<a href='#anchor0'></a>
## スクリプトみの強いやつ# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
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

<a href='#anchor0-0'></a><a href='#anchor7'></a>
## 速度ランキング（あんまり参考にならない）
おことわり：今回題材とした「素数の計算」は比較的単純なコードなので、一部言語を除いて実際以上に似たりよったりな結果になっています。どちらかといえば「どの言語が速いか」より「どのサンプルがうまく書けているか」のほうがだいぶ影響が大きそうです。

実行時間：
| rank | lang | time | ratio | 
| - | - | - | - |

CPU時間：
| rank | lang | time | ratio | 
| - | - | - | - |

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

<a href='#anchor0'></a>
## スクリプトみの強いやつ# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
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

<a href='#anchor0-0'></a><a href='#anchor8'></a>
## 貢献者一覧
- メタリックはんぺん 
  - 説明: C, C++, Rust, Python, Haskell, Fortran, JS, PHP, VB.net, C#, Java
  - サンプル: C, C++, Rust, Python, Haskell, Fortran, JS, R, VB.net, C#, Java
  - 一言: Haskellはいい言語ですよ、やれ！お前も蓮沼に落ちろ！！！
- あなばす
  - 説明: Julia, Lisp, R, MATLAB, Fortran
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

<a href='#anchor0'></a>
## スクリプトみの強いやつ# 新入生のための「結局俺は何のプログラミング言語を学べばいいんだ」
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

<a href='#anchor0-0'></a>