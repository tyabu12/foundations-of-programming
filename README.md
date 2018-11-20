# foundations-of-programming

[![Build Status](https://travis-ci.org/tyabu12/foundations-of-programming.svg?branch=master)](https://travis-ci.org/tyabu12/foundations-of-programming)

プログラミングの基礎 (浅井健一著) の演習問題。

## メトロネットワークデータ

メトロネットワークデータ (`metro.ml`) は下記サポートページからダウンロードできる。

<http://pllab.is.ocha.ac.jp/~asai/book/Metro.html>

ただし、**文字コードが EUC-JP なので UTF-8 に変換**する必要がある。

make、curl、iconv が使えるなら、下記コマンドで UTF-8 に変換後の `metro.ml` が取得できる。

    $ make src/metro.ml

