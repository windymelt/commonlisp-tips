% Common lisp tips
% Windymelt <windymelt 0x40 3qe.us>

<!-- Build: pandoc -t html -s --toc --template marx.html < commonlisp.md > commonlisp.html -->
Common lispで書くために調べた事柄の実用的メモ．CLでのやり方を集めることを目指す．

メモを残していきます．不定期に更新します．

## インストール
TBD

## 略称を定義する
```commonlisp
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(abbrev dbind destructuring-bind) ; too hard to remember

(dbind (x y) '(a b)
  (format t "~s, ~s~%" x y))
```

```commonlisp
A, B
```

cf. http://www.bookshelf.jp/texi/onlisp/onlisp_17.html

## パッケージ定義
asdファイルに使うファイルを記述する（面倒）
`defpackage` ではパッケージ名はキーワードにする

## Common Lispにsplitはないの？
文字列を指定の文字で分割するには， `split-sequence:split-sequence` を使う．

```commonlisp
(quicklisp:quickload :split-sequence)
(split-sequence:split-sequence #\Space "this is sample CL program")
```

リストも分割できる．

```
(split-sequence:split-sequence 'a '(p a n a m a n o b a n a n a))
```

```commonlisp
((P) (N) (M) (N O B) (N) (N) NIL)
14 ; split-sequenceは多値を返す
```

## 標準入力から行を読み込む
`read-line` で1行読み込むことができる．全ての行を読み込むには `loop` マクロなどを使う．

```commonlisp
(loop for line = (read-line nil nil :eof)
      until (eq line :eof)
      do (print t "%s%~" line))
```

## Octet streamを文字列に変換したい
`flexi-streams:octets-to-string` を使う．

## http clientを使いたい
`drakma:http-request` を使う．

## jsonを書きたい
`jsown:to-json` を使う．オブジェクトを生成するには `(:obj key val)` を使う．
jsonをパースするには `jsown:parse` を使う．

```commonlisp
(quicklisp:quickload :jsown)
(jsown:to-json '(:obj (hello . "world")))
```

```json
{"HELLO":"world"}
```

```commonlisp
(jsown:parse "[1, 2, 3]")
```

## 分割代入

記述力に勝る `optima.extra:let-match` か， `destructuring-bind` を使う．

### destructuring-bind
スペルを覚えにくい． _de-structur-ing_ である．

```commonlisp
(destructuring-bind
  (first second) #| <= bound |# '(alpha omega)
  (format t "head is ~s, another is ~s~%" first second))
```

### `match` / `let-match`

`let-match` は `match` の `let` 風の記法である．

```commonlisp
(quicklisp:quickload :optima)

;; define object to match
(setq lis '(alpha beta gamma))
(format t "lis is ~s~%" lis)

;; pattern-matching
(optima:match lis
  ((list first second third)
   (format t "first is ~s, second is ~s, third is ~s.~%" first second third)))

;; code above is equivalent to below
(optima.extra:let-match (((list first second third) lis))
  (format t "first is ~s, second is ~s, third is ~s.~%" first second third))
```

```
To load "optima":
  Load 1 ASDF system:
    optima
; Loading "optima"

lis is (ALPHA BETA GAMMA)
first is ALPHA, second is BETA, third is GAMMA.
first is ALPHA, second is BETA, third is GAMMA.
```

## 繰り返し
`loop` マクロを使う．

```commonlisp
(setq xs '(alpha beta gamma))
(loop for x in xs
      do (format t "value is ~s~%" x))
```

```
value is ALPHA
value is BETA
value is GAMMA
```

cf. [http://smpl.seesaa.net/article/29800843.html](http://smpl.seesaa.net/article/29800843.html)

## #pとは

パスオブジェクトのリテラル．

## #sとは

構造体のリテラル．

## doubleとして数値を読む
[`*read-default-float-format*`](http://www.lispworks.com/documentation/HyperSpec/Body/v_rd_def.htm) を使う．

```commonlisp
(format t "~s~%" *read-default-float-format*)
(setq *read-default-float-format* 'double-float)
(format t "~s~%" *read-default-float-format*)
```

```
SINGLE-FLOAT
DOUBLE-FLOAT
```
## コマンドラインオプションを読み取る
`unix-opts` などのパッケージでコマンドラインオプションを読み取ることができる．

### `unix-opts`

TBD

`define-opts` でオプションを定義し，

```commonlisp
;;; opts.ros

(quicklisp:quickload :unix-opts)

;; defining CLI options
(define-opts :name some-option ; mandatory.
             :description "This is test option." ; optional but recommended.
             :short #\o ; can omit if you specify :long
             :long "option" ; can omit if you specify :short
             :meta-var hoge)
```
