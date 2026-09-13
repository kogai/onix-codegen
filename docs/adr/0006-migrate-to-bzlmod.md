# ADR-0006: Bazel を bzlmod に移行する

- **ステータス**: Accepted
- **日付**: 2026-09-13

## 背景

Bazel 側の依存は 2020 年から止まっていた。

| 依存           | 固定されていた版 | 現行     |
| -------------- | ---------------- | -------- |
| Bazel          | 3.7.0            | 9.2.0    |
| rules_go       | 0.24.7           | 0.63.0   |
| bazel-gazelle  | 0.22.2           | 0.54.0   |

これを上げようとすると、WORKSPACE の存続そのものが問題になる。**Bazel 9 は WORKSPACE を
廃止し、bzlmod のみになった。** rules_go 0.63.0 も Bazel 8 / 9 でテストされている。
つまり「WORKSPACE を保ったままバージョンだけ上げる」という中間の選択肢は、
上げ幅を小さく取らない限り成立しない。

小さく上げる案 (たとえば Bazel 6 系に留める) は、いずれ同じ移行を後日やることになるうえ、
その時点でまた古い組み合わせの検証が必要になる。

## 決定

bzlmod に移行する。`WORKSPACE` を削除し、`MODULE.bazel` を追加する。
Bazel 9.2.0、rules_go 0.63.0、gazelle 0.54.0 に上げる。

`bazel_dep` には `repo_name` を指定する。

```python
bazel_dep(name = "rules_go", version = "0.63.0", repo_name = "io_bazel_rules_go")
bazel_dep(name = "gazelle", version = "0.54.0", repo_name = "bazel_gazelle")
```

これにより、既存の BUILD ファイルが使っているラベル (`@io_bazel_rules_go//go:def.bzl`、
`@bazel_gazelle//:def.bzl`) がそのまま通る。

**ただし BUILD ファイルの変更が 1 行だけ必要になった。** Bazel 9 は `sh_test` を
ネイティブのルールとして提供しなくなり、`rules_shell` に移された。CI が最初に返した
エラーはこれである。

```
ERROR: e2e/go/BUILD.bazel:35:1: name 'sh_test' is not defined (did you mean 'cc_test'?)
```

`bazel_dep(name = "rules_shell", version = "0.8.0")` を足し、`e2e/go/BUILD.bazel` に
`load("@rules_shell//shell:sh_test.bzl", "sh_test")` を追加した。

EDItEUR の zip は `use_repo_rule` で従来どおり `http_archive` として宣言する。
`sha256` も URL も変えていないので、取得の問題 (ADR-0003) の状況は変わらない。

## 理由

`repo_name` を使えば移行の影響を `MODULE.bazel` の中だけに閉じ込められる。BUILD ファイルを
書き換えずに済むということは、この PR で壊れうる箇所が依存の宣言だけに限られるということで、
ローカルで Bazel を実行できない状況 (`releases.bazel.build` が egress ポリシーで到達不可) では
特に価値が大きい。

一度に上げるのは、段階を刻んでも各段階を検証する手段が結局 CI しか無いためである。
中間バージョンを経由しても、検証の回数が増えるだけで確度は上がらない。

`gazelle update-repos` を呼んでいた Makefile のターゲットは削除した。bzlmod では
外部 Go 依存の管理が `go_deps` 拡張に移っており、このコマンドは無い。なお `go.sum` は
空で、外部 Go 依存は存在しないので、`go_deps` の宣言自体が不要である。

`go.mod` の `go` ディレクティブは 1.14 のままだったので 1.21 に上げた。Go SDK は 1.27.1 を使う。
この 2 つが違うのは意図的で、役割が別である。`go` ディレクティブは生成された Go
クライアントを使う側に要求する最低バージョン、SDK はこのリポジトリがビルドに使う
処理系である。前者を上げると利用者を絞ることになるので、揃える理由は無い。

## 検討した他の選択肢

- **Bazel 6 系に留めて WORKSPACE を維持する**: 今回の変更量は減るが、移行を後回しにするだけ。
  そのうえ、その時点の rules_go / gazelle の対応版を別途調べ直す必要がある。
- **Bazel 8 を選ぶ**: 8 は WORKSPACE をフォールバックとして残しているぶん保守的だが、
  どのみち bzlmod に移行するなら差は小さい。実在を確認できた最新版が 9.2.0 だったので、
  検証済みのバージョン番号を使うことを優先した。
- **Bazel をやめる**: スキーマの取得と Go の e2e に使っているだけなので不可能ではないが、
  取得の仕組み (ADR-0003) が未決の状態で基盤を入れ替えるのは順序が悪い。

## 結果

- BUILD ファイルの変更は `sh_test` の load 1 行のみ。ラベルの互換性は `repo_name` が
  担保しているので、それ以外は無変更で済んだ。
- Bazel 8 以降、ネイティブルールの Starlark 化が進んでいる。今後 Bazel を上げるときは、
  「無くなったネイティブルールを提供する rules_* を足す」作業が同様に発生しうる。
- `make schema` の挙動は変わらない。取得できない問題も変わらない (ADR-0003)。
- `MODULE.bazel.lock` は**コミットすべき**である。`go_sdk.download` は sha256 を持たないため、
  ロックが無いと毎回 Go のバージョンインデックスを取りに行き、BCR の解決も毎回ネットワーク越しになる。
  ただしこのファイルは Bazel を実行しないと生成できず、この環境では実行できない。
  `.gitignore` からは外したので、次に手元で Bazel を動かした人がコミットしてほしい。
  ファイルが入ったら `.bazelrc` に `common --lockfile_mode=error` を足すとよい
  (ファイルが無い状態でこのフラグを足すとビルドが落ちるので、今は入れていない)。
- **この移行はローカルで検証できていない。** Bazel をこの環境で実行できないため、
  唯一の検証手段は CI の e2e ジョブ (`//e2e/go:snapshot_test`) である。実際、
  `sh_test` の件は CI が最初に教えてくれた。
