# ADR-0002: ユニットテストを取得済みスキーマから切り離す

- **ステータス**: Accepted
- **日付**: 2026-09-13

## 背景

Makefile のテストターゲットは `schema` に依存していた。

```make
.PHONY: test
test: schema
	stack test --trace --fast
```

`schema` ターゲットは Bazel の `http_archive` 経由で EDItEUR の zip をダウンロードし、
`schema/v2` / `schema/v3` に展開する。つまり `make test` は毎回 editeur.org への
ネットワークアクセスを要求していた。

2026-09-13 時点で、この取得が機能しなくなった。

```
WARNING: Download from https://www.editeur.org/files/ONIX%202.1/ONIX_for_Books_Release2-1_rev03_schema+codes_Issue_36.zip
         failed: UnrecoverableHttpException GET returned 202 Accepted
ERROR: An error occurred during the fetch of repository 'org_editeur_v2'
make: *** [Makefile:35: schema/v2] Error 1
```

その結果、`Test haskell codes` ジョブはテストを 1 件も実行しないまま失敗する。
取得できない理由そのものは ADR-0003 で扱う。

### この依存は本物だった

当初、この依存は単なる過剰指定だと考えた。テストコードが直接読むのは
`fixtures/test_*.xsd` だけで、`./schema` という文字列は `src/Lib.hs` の `schemaRoot`
にしか現れないからである。しかしこれは誤りだった。

`fixtures/test_mixed_html.xsd` が、EDItEUR の配布物を直接 include していた。

```xml
<xs:include schemaLocation="../schema/v2/ONIX_XHTML_Subset.xsd" />
```

`Xsd.getSchema` は include を再帰的にたどり (`src/Xsd.hs` の `go` / `goInclude`)、
相対パスを `combineURIs` でローカルパスに解決して `Text.XML.readFile` で読む。
ファイルが無ければ IOException で落ちる。このフィクスチャは
`test/TestMixed.hs` と `test/TestModel.hs` の両方から読まれており、`schema/` は
`.gitignore` されているので、clean checkout では必ず存在しない。

つまり `test: schema` は正しい依存だった。git の履歴もそれを裏づけている。
`test: schema` を追加したコミット (9352123) は、リポジトリに置かれていた
`2_1_rev03_schema/` を削除し、このフィクスチャの include を `../schema/v2/` に
向け直した、まさにそのコミットである。

## 決定

依存を消すのではなく、**依存の対象をリポジトリ内に移す**。

1. `fixtures/test_mixed_html_xhtml_subset.xsd` を追加する。`test_mixed_html.xsd` が
   `ref` している 40 個の要素名だけを宣言した、ONIX_XHTML_Subset.xsd の代替物である。
2. `test_mixed_html.xsd` の include をそちらに向ける。
3. そのうえで `test` ターゲットから `schema` 依存を外す。

`build` ターゲットの `schema` 依存は残す。実際にコードを生成するにはスキーマの実体が
要るため、こちらは今も本物の依存である。

## 理由

代替物は、テストが実際に検証している性質を保つように書いた。
`test/TestModel.hs` は、このフィクスチャを読んだうえで `Model.collectElements` が
空になることを表明している。`collectElements` は `complexMixed = False` の要素だけを
拾うフィルタなので、この表明の意味は「XHTML の要素がモデルに漏れてこない」ことである。
XHTML の内容要素はいずれも mixed content なので、代替物でも全要素を
`<xs:complexType mixed="true" />` として宣言した。要素名の集合が実物と一致していること、
フィクスチャ側の `ref` 40 個すべてに宣言が対応することは機械的に確認した。

代替物で足りるのは、テストがこのファイルから必要としているのが**要素の宣言の存在と
mixed であること**だけだからである。ONIX_XHTML_Subset.xsd の完全な内容 (属性、
コンテンツモデルの詳細) は、ここで検証されている性質に寄与していない。

そのうえで、テストを外部サービスの可用性から切り離す価値は大きい。パーサのバグを
直したいときに editeur.org の状態に左右されるのは、依存の向きとして誤っている。

なお、この変更は 202 の問題そのものを解決しない。コード生成と、生成物を最新スキーマへ
追従させる作業は依然としてダウンロードを必要とする (ADR-0003)。

## 検討した他の選択肢

- **`test: schema` を残す**: 事実としては正しい依存なので、これは筋が通っている。
  ただし、たった 1 ファイルの XSD のためにテスト全体を外部サービスに縛り続けることになる。
- **ONIX_XHTML_Subset.xsd をそのまま `fixtures/` に取り込む**: 代替物より忠実だが、
  EDItEUR の配布物の再配布にあたる。ライセンスの判断が要るため、テストを通すためだけに
  踏み込むべきではない (ADR-0003 と同じ理由)。代替物は自前で書いたものなのでこの問題がない。
- **include を単に削除する**: `Annotation` 以外に要素が無くなるので `collectElements` は
  空になり、表明は通ってしまう。だが「XHTML 要素が漏れてこない」ことを何も検証しなくなり、
  テストが意味を失う。**採らない。**

## 結果

- `make test` はネットワークなしで実行できる。CI の Haskell ジョブは editeur.org に依存しない。
- `fixtures/test_mixed_html_xhtml_subset.xsd` は実物の代替物である。実物側の構造が変わって
  テストの前提が動くことはあり得るので、スキーマの版を上げるときはこのファイルも見直す。
- コード生成 (`make build`、`make generated/...`) は引き続きダウンロードを必要とする。
- 新しいテストを書くときは `fixtures/` に最小の XSD を足す、という既存の慣習が
  そのまま「テストを外部依存から切り離す」ことにもなる。この性質は維持する。
  フィクスチャから `fixtures/` の外を include しないこと。
