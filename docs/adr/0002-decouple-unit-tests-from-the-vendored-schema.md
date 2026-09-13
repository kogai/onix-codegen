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

2026-09-13 時点で、CI からこのダウンロードが失敗するようになった。

```
WARNING: Download from https://www.editeur.org/files/ONIX%202.1/ONIX_for_Books_Release2-1_rev03_schema+codes_Issue_36.zip
         failed: UnrecoverableHttpException GET returned 202 Accepted
ERROR: An error occurred during the fetch of repository 'org_editeur_v2'
make: *** [Makefile:35: schema/v2] Error 1
```

editeur.org が zip の代わりに `202 Accepted` を返しており、Bazel はこれを回復不能な
ダウンロードエラーとして扱う。結果として、`Test haskell codes` ジョブはテストを 1 件も
実行しないまま失敗する。

ここで重要なのは、**テストコードはそもそも `schema/` を読んでいない**という事実である。
`test/` 配下が読むのは `fixtures/test_*.xsd` だけで、`./schema` を参照するのは
`src/Lib.hs` の `schemaRoot` (コード生成の実行時パス) のみ。つまりこの依存は、
テストの実行に必要ではないのに、テストの実行可能性を外部サービスの可用性に縛り付けていた。

## 決定

`test` ターゲットから `schema` 依存を外す。

```make
.PHONY: test
test:
	stack test --trace --fast
```

`build` ターゲットの `schema` 依存はそのまま残す。実際にコードを生成するには
スキーマの実体が要るため、こちらは本物の依存である。

## 理由

ユニットテストは「XSD をどう解釈して中間表現に落とすか」を検証するものであり、
その入力は意図的に最小化された `fixtures/test_*.xsd` である。EDItEUR の完全なスキーマは
テストの対象ではない。依存として書かれていたのは事実の反映ではなく、単なる過剰指定だった。

この過剰指定には実害がある。外部サービスの都合で、リポジトリ内のロジックに対する
フィードバックループ全体が止まる。パーサのバグを直したいときに editeur.org の状態に
左右されるのは、依存の向きとして誤っている。

なお、この変更は 202 の問題そのものを解決しない。コード生成と、生成物を最新スキーマへ
追従させる作業は依然としてダウンロードを必要とする。これは別の判断として切り出す。

## 検討した他の選択肢

- **`http_archive` にリトライやフォールバック URL を足す**: Bazel 3.7.0 の `http_archive` は
  カスタムヘッダに対応しておらず、202 を返す bot 対策を回避する手段がない。
  そもそもテストにダウンロードは不要なので、この層で解決するのは筋が悪い。
- **スキーマをリポジトリに取り込む (vendoring)**: テストは通るようになるが、
  EDItEUR の配布物の再配布はライセンス上の検討を要する。テストを通すためだけに
  踏み込む判断ではない。生成のために必要かどうかは別途検討する。
- **CI でだけ `stack test` を直接呼ぶ**: CI とローカルで手順が食い違い、
  「手元で通るのに CI で落ちる」を生む。Makefile が唯一の入口である状態を保つ。

## 結果

- `make test` はネットワークなしで実行できる。CI の Haskell ジョブは editeur.org に依存しない。
- コード生成 (`make build`、`make generated/...`) は引き続きダウンロードを必要とする。
  最新スキーマへの追従が止まっている場合、原因はこちら側にある。
- 新しいテストを書くときは `fixtures/` に最小の XSD を足す、という既存の慣習が
  そのまま「テストを外部依存から切り離す」ことにもなっている。この性質は維持する。
