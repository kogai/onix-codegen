# ADR-0004: e2e スナップショット比較から rules_nodejs を外す

- **ステータス**: Accepted
- **日付**: 2026-09-13

## 背景

`build_bazel_rules_nodejs` 3.1.0 は WORKSPACE の依存として宣言されていたが、
実際に使われていたのは次の 2 箇所だけだった。

```
WORKSPACE:18          name = "build_bazel_rules_nodejs",
WORKSPACE:25          load("@build_bazel_rules_nodejs//:index.bzl", "npm_install")
e2e/go/BUILD.bazel:2  load("@build_bazel_rules_nodejs//:index.bzl", "generated_file_test")
```

このうち `npm_install` が定義する `@npm` リポジトリは、**どの BUILD ターゲットからも
参照されていない**。`//e2e/go:snapshot_test` は Go のバイナリと JSON ファイルしか使わない。
つまり実質的な用途は `generated_file_test` ただ 1 つだった。

この依存はライブラリ更新の妨げにもなっていた。3.1.0 は 2021 年のリリースで、
新しい Bazel との組み合わせは動作が保証されない。一方 rules_nodejs は 5.x → 6.x で
大きく整理され、`generated_file_test` は現行版には無い。つまり「rules_nodejs を上げる」には
どのみち比較の仕組みを書き換える必要があり、上げても得るものが無い。

なお、Bazel 自体の起動に使っている `npx bazelisk` は npm の devDependency であって
rules_nodejs とは無関係なので、この判断の影響を受けない。

## 決定

`generated_file_test` を、外部ルールに依存しない `sh_test` (`e2e/go/snapshot_test.sh`) に
置き換え、`build_bazel_rules_nodejs` を WORKSPACE から削除する。

スクリプトは生成された JSON とコミット済みスナップショットを `diff -u` で比較し、
差分があれば差分そのものを表示して失敗する。

## 理由

比較の中身は「2 つのファイルが同一か」でしかない。そのために外部リポジトリを 1 つ
丸ごと取得するのは釣り合っていない。`diff` は Bazel のバージョンにも依存しないので、
今後 Bazel を上げるときにこの部分が障害にならない。

`bazel_skylib` の `diff_test` に置き換える案もあったが、それは依存を別の依存に
入れ替えるだけで、しかも skylib のどのバージョンが Bazel 3.7.0 と組み合わせられるかを
この環境では検証できない (`releases.bazel.build` に到達できない)。検証できない選択肢を
2 つ抱えるより、依存を減らして 1 つに絞るほうがよい。

失敗時の出力はむしろ改善する。`generated_file_test` は不一致の事実を報告するだけだったが、
`diff -u` は何がどう違うかを出す。スナップショットの更新手順もスクリプトの
コメントに書いた。

## 検討した他の選択肢

- **rules_nodejs を最新に上げる**: `generated_file_test` が現行版に無いので、
  どのみち比較の仕組みを書き換えることになる。書き換えたうえで依存が残るだけ損。
- **`bazel_skylib` の `diff_test` を使う**: 標準的で堅い。ただし上記のとおり、
  依存を入れ替えるだけで減らず、バージョン組み合わせを検証できない。
- **`npm_install` だけ残す**: 参照するターゲットが無いので、残す理由が無い。

## 結果

- WORKSPACE の外部依存が 1 つ減り、Bazel のバージョンを上げるときの制約も 1 つ減る。
- `package.json` / `package-lock.json` は Bazel のビルドグラフから完全に外れた。
  これらが影響するのは `npx bazelisk` の取得だけになる。
- スナップショットの更新は手作業になった (`bazel build //e2e/go:snapshot` の出力を
  `fixtures/20201200.json` にコピー)。手順は `e2e/go/snapshot_test.sh` の冒頭にある。
