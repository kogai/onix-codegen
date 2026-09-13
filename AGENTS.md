# AGENTS.md

このリポジトリで作業するエージェント／コントリビュータ向けのガイド。
まず「このリポジトリの価値」を読み、それに反する変更をしないこと。

## このリポジトリの価値

onix-codegen は EDItEUR が配布する ONIX for Books の XSD スキーマから、
各言語向けのクライアントコードを自動生成するジェネレータ。価値は次の 2 点に集約される。

1. **ONIX 仕様への追従を、なるべく後方互換性を保ったまま行うこと。**
   スキーマの Issue 更新・リリース更新に追従しつつ、既に生成済みのコードを使っている
   利用者のビルドを壊さない。互換性を壊す変更は、壊さない選択肢が本当に無いときだけ。
2. **コード生成の仕組みで、サポートできる言語をなるべく増やすこと。**
   言語固有の知識はテンプレート (`template/<language>/<version>/*.mustache`) に閉じ込め、
   スキーマ解析側 (`src/`) は言語非依存に保つ。新しい言語の追加が「テンプレートを書くだけ」に
   近づくほど、この設計は正しい方向にある。

判断に迷ったら、この 2 つのどちらをより良くするかで選ぶ。

## 全体のパイプライン

```
EDItEUR の zip (WORKSPACE の http_archive)
  └─ bazel genrule (BUILD.bazel: onix_v2 / onix_v3)
      └─ schema/v2, schema/v3        … .gitignore 済み。生成物なのでコミットしない
          └─ src/Xsd/*.hs            … XSD を Haskell の AST へパース (言語非依存)
              └─ src/{Model,Code,Mixed}.hs
                                     … AST を「生成用の中間表現」へ変換 (言語非依存)
                  └─ src/Lib.hs + mustache テンプレート
                      └─ generated/<language>/<version>/{model,code,mixed,reader}.<ext>
```

中間表現は `Util.GenSchema` クラス (`readSchema :: Schema -> a`) と
`Text.Mustache.ToMustache` インスタンスで表現される。テンプレートから見えるキーは
各 `toMustache` 実装がすべて。テンプレートに新しい情報が必要になったら、
まず `toMustache` にキーを足す。

- `Model` … 要素・属性の木 (`shortname` / `xmlReferenceName` / `typeName` / `optional` / `iterable`)
- `Code`  … コードリスト (列挙値と説明)
- `Mixed` … mixed content を持つ要素

## セットアップとよく使うコマンド

前提: `stack` (GHC 8.8.3 / lts-16.27)、`node` (bazelisk を npm 経由で実行)、`go`。
スキーマ取得で editeur.org への外部ネットワークアクセスが必要。

```sh
npm install                 # bazelisk などを入れる
make schema                 # EDItEUR の zip を取得して schema/v2, schema/v3 に展開
make build                  # schema + stack build --fast
make test                   # stack test (HUnit) のみ。ネットワーク不要
make generated/go/v3        # v3 の Go コードを再生成 (ターゲット名の末尾がスキーマ版)
make generated/ts/v2        # v2 の TypeScript コードを再生成
npx bazelisk test //e2e/go:snapshot_test   # 生成済み Go クライアントの e2e スナップショット
make debug                  # プロファイル付きで v3/go を生成 (例外の発生箇所を追うとき)
```

CI (`.github/workflows/test.yml`) は `make test` と `//e2e/go:snapshot_test` の 2 ジョブ。
この 2 つがローカルで通ることを、push 前に確認する。どちらもネットワークを必要としない。

`make schema` は現在 editeur.org から取得できない (202 が返る)。生成系を動かすには
手元に zip を用意する必要がある。事情と手順は `docs/adr/0003-editeur-schema-acquisition.md`。

既知の古さ: Makefile の `json` ターゲットは存在しない `run` に依存し、
import path も `go/helper` と古い (実体は `e2e/go`)。スナップショットの更新は
`//e2e/go:snapshot` の出力を `fixtures/20201200.json` に反映する形で行う。

## 後方互換性の守り方

生成コードの「公開 API」は、型名・フィールド名・XML/JSON タグ。ここが実質的な互換性境界。

- **生成物の diff を必ず読む。** `make generated/go/v2` 等を実行し、`git diff generated/` を確認する。
  既存の型・フィールドの **削除やリネーム** が出ていたら、それは破壊的変更。意図した場合のみ、
  PR 本文に理由と影響範囲を書く。追加のみの diff は基本的に安全。
- **スキーマの版はディレクトリで分ける。** 新しい Issue / リリースに対応するときは、
  既存の `v2` / `v3` の出力を置き換えるのではなく、必要なら新しいバージョンとして足す。
- **未対応は黙って落とさない。** 扱えない構造に遭遇したら `Util.unimplemented`、
  起こり得ないはずの分岐は `Util.unreachable` で、理由の文字列を付けて明示する。
  暗黙にフィールドを落とすと、後方互換性の問題が静かに発生する。
- **`generated/` を手で編集しない。** 差分は必ずテンプレートか `src/` を直して再生成する。
- **`fixtures/20201200.json` は e2e のスナップショット。** ここが変わる = 生成物の
  ランタイム挙動が変わっている。変更する場合は、それが意図した互換性変更か確認する。

### ONIX スキーマの版を上げる手順

1. `WORKSPACE` の `http_archive` (`org_editeur_v2` / `org_editeur_v3`) に URL と `sha256` を設定する。
   既存の版を差し替えるのではなく、追従先を増やす方向を先に検討する。
2. `org_editeur_*.bazel` の `glob` と `BUILD.bazel` の `filegroup` (`onix2p1` / `onix3p0p7`) に
   ファイル名を反映する。
3. `src/Lib.hs` の `schemaRoot` にあるルート XSD のパスを確認する。
4. `make test` と生成物の diff で、既存の型が消えていないことを確認する。

## 新しい言語を足す手順

言語非依存な `src/` には手を入れないのが理想。触る必要が出たら、それは中間表現に
情報が足りていないサインなので、言語別分岐ではなく中間表現の拡張として実装する。

1. `src/Lib.hs`
   - `data Language` にコンストラクタを追加
   - `ext` … 出力拡張子
   - `template` … `template/<language>/<version>` のパス
   - `generateTo` … `generated/<language>/<version>` のパス
2. `app/Main.hs` の `run` に `--language <name>` のパターンを追加する。
   未対応の組み合わせは `unimplemented` を返す (v3 × typescript が既にその例)。
3. `template/<language>/<version>/` に 4 つの mustache を置く。
   `model` / `code` / `mixed` / `reader` の 4 つは `Renderer` と 1 対 1 で、すべて必須。
   `reader` だけはスキーマを受け取らない (`substitute t ()`) ので、静的なテンプレートでよい。
4. `generated/<language>/<version>/` を作り、生成物をコミットする。
5. `Makefile` に生成ターゲットを追加する (`generated/go/%` を参考に)。
6. 可能なら e2e を足す。`e2e/go` が手本: 生成クライアントで `fixtures/20201200.onix` を読み、
   JSON にして `fixtures/20201200.json` と突き合わせる。言語をまたいで同じ
   スナップショットに一致することが、生成器の正しさの一番強い証拠になる。
7. `README.md` の Current Status 表を更新する。

## テストの書き方

`test/` は HUnit。`test/Spec.hs` が `TestModel` / `TestParser` / `TestCode` / `TestMixed` を束ねる。

新しい XSD の構文や生成パターンに対応するときは、**まず `fixtures/test_*.xsd` に
最小の XSD を足す**。既存の fixture は 1 ファイル 1 論点で、命名は
`test_<領域>_<論点>.xsd` (`test_code_space_separated.xsd`、`test_model_iterable_choice.xsd` など)。
テスト側では `getSchema "./fixtures/test_xxx.xsd"` で読み、期待値を AST リテラルで書いて
`assertEqual` する。コードリストを参照する fixture は `*_codelists.xsd` を隣に置く慣習。

**fixture から `fixtures/` の外を include しないこと。** `Xsd.getSchema` は `xs:include` を
再帰的にたどってローカルパスを解決するので、`../schema/` を参照した瞬間、テストは
`make schema` のダウンロードに依存する。実際に `test_mixed_html.xsd` がそうなっていて、
テスト全体が editeur.org の可用性に縛られていた (ADR-0002)。必要な定義は
`test_mixed_html_xhtml_subset.xsd` のように、代替物を `fixtures/` 内に置く。

## コーディング規約

- Haskell は ormolu 相当の整形。既存ファイルのスタイル (import の並び、レコード記法) に合わせる。
- `package.yaml` の `library` は `-Wall -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns`。
  警告を増やさない。パターンマッチは網羅するか、`unreachable` / `unimplemented` で明示的に落とす。
- 依存は `package.yaml` でバージョン固定。追加したら `stack.yaml.lock` の更新も確認する。
- モジュールの役割を混ぜない。XSD の形の話は `src/Xsd/`、中間表現は `Model`/`Code`/`Mixed`、
  出力先や言語の対応表は `Lib`、CLI は `app/Main.hs`。

## 設計判断は ADR に残す

「なぜそうなっているか」は `docs/adr/` に ADR として記録する。この AGENTS.md は
「今どうすべきか」を書く場所、ADR は「どういう制約のもとにそう決めたか」を残す場所。

次のいずれかに影響する判断をしたら ADR を足す (`docs/adr/0000-template.md` が雛形)。

- **後方互換性**: 生成コードの公開 API に影響する判断、スキーマの版の扱い方
- **サポート言語の増やしやすさ**: 中間表現とテンプレートの責務分担、言語追加の手順
- **ビルドと CI の前提**: 外部依存 (EDItEUR の配布物、ツールチェーンのバージョン) の扱い方

判断が変わったときは既存の ADR を書き換えず、新しい ADR を書いて古いものを
`Superseded` にする。決定の履歴が消えないことが重要。

## 触らない / コミットしないもの

- `schema/` … `make schema` の生成物 (`.gitignore` 済み)
- `.stack-work/`, `bazel-*` … ビルド成果物
- `generated/` を手編集したもの … 必ず再生成した結果をコミットする
