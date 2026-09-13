# ADR-0003: EDItEUR スキーマの取得方法

- **ステータス**: Proposed (メンテナの判断待ち)
- **日付**: 2026-09-13

## 背景

このリポジトリは、EDItEUR が配布する zip を Bazel の `http_archive` で取得して
スキーマの実体を得ている (`WORKSPACE`)。

```python
http_archive(
    name = "org_editeur_v2",
    sha256 = "8fe93242...",
    url = "https://www.editeur.org/files/ONIX%202.1/ONIX_for_Books_Release2-1_rev03_schema+codes_Issue_36.zip",
)
```

2026-09-13 時点で、この取得が機能しない。観測された事実は次の 2 つ。

**1. CI から: `202 Accepted` が返る**

```
WARNING: Download from https://www.editeur.org/files/ONIX%202.1/...Issue_36.zip
         failed: UnrecoverableHttpException GET returned 202 Accepted
```

zip の代わりに 202 が返り、Bazel はこれを回復不能なエラーとして扱う。1 回再実行しても
同じ結果だったため、一時的な不調ではない。202 を返すのは bot 対策の中間応答として
一般的な挙動で、GitHub Actions のような自動化された経路が弾かれていると考えられる。

**2. 開発用のサンドボックス環境から: そもそも到達できない**

egress ポリシーにより `www.editeur.org:443` への CONNECT が 403 で拒否される。
このため、URL の確認も、`http_archive` が要求する `sha256` の計算もできない。

この 2 つが重なった結果、次のことがすべて止まっている。

- コード生成 (`make build`、`make generated/...`)
- 最新スキーマへの追従 (現在 ONIX 2.1 Issue 36 / 3.0 Issue 52)

追従先の最新版がどれかは、**この ADR の時点では確定できていない**。二次情報では
ONIX 3.1.3 + codelists Issue 73 (2026-04) とされているが、editeur.org に到達できない以上、
一次情報での裏取りができていない。そして `WORKSPACE` が実際に必要とするもの、すなわち
**zip の URL とファイル名、その sha256 は完全に不明**である。追従作業は、まずここの確認から始まる。

なお、ユニットテストは ADR-0002 でこの依存から切り離したため影響を受けない。
止まっているのは生成系だけである。

## 決定

**この ADR は決定を保留し、選択肢と判断材料を提示する。** 有力な選択肢のうち 2 つが
第三者著作物の再配布を伴い、ライセンスの判断はメンテナが行うべきものであるため。

暫定の運用としては、**手元に落とした zip を Bazel の `--distdir` 経由で使う**方法を推奨する。
これは EDItEUR が意図する配布経路 (ブラウザでのダウンロード) をそのまま使い、
何も回避せず、リポジトリに再配布物も置かない。

```sh
# ブラウザで zip を取得し、任意のディレクトリに置く
mkdir -p third_party/distdir
mv ~/Downloads/ONIX_BookProduct_XSD_schema+codes_Issue_52.zip third_party/distdir/

# WORKSPACE の sha256 と一致すれば、Bazel はネットワークに出ずにこれを使う
npx bazelisk build --distdir=third_party/distdir onix_v3
```

`schema/v2` / `schema/v3` を手で用意する場合は、**展開の仕方に注意が要る**。zip には
`ONIX_BookProduct_XSD_schema+codes_Issue_52/` のようなトップレベルディレクトリがあるが、
`src/Lib.hs` は `./schema/v3/ONIX_BookProduct_3.0_reference.xsd` という平坦なパスを読む。
これは `BUILD.bazel` の genrule (`cp $(SRCS) $(RULEDIR)/v3`) が平坦化した結果に合わせたもので、
素の `unzip` では階層がひとつ深くなって読めない。`-j` で平坦に展開する。

```sh
mkdir -p schema/v3
unzip -j ONIX_BookProduct_XSD_schema+codes_Issue_52.zip -d schema/v3
```

こうして置いたディレクトリは尊重される。`schema/%` は prerequisite を持たないパターンルールなので、
ディレクトリが既に存在すれば make はそのターゲットを最新とみなし、ダウンロードを試みない
(スクラッチの Makefile で再現して確認済み。`schema/v2` だけがある状態では `schema/v3` のみ取得しにいく)。

なお `make build` から `--distdir` を渡す口は今のところ無い。Makefile はフラグを素通ししないので、
恒久的に使うなら `.bazelrc` に書くのが早い。

```
common --distdir=third_party/distdir
```

指定したディレクトリが存在しない場合、Bazel はエラーにせず INFO を出して素通りする。
つまり **パスを間違えても、元の 202 と見分けがつかない失敗になる**。まずディレクトリの存在を確かめること。

## 理由

`--distdir` は Bazel が公式に用意している、まさにこの状況 (取得元に到達できないが、
ファイルの実体はある) のための仕組みである。`sha256` による同一性の検証も効いたままなので、
取得経路が変わっても再現性は落ちない。

決定を保留するのは、恒久的な解決策が技術的な選択ではなくライセンスの判断だからである。
ここで勝手に vendoring すると、判断を経ずに再配布を既成事実にしてしまう。

## 検討した他の選択肢

- **スキーマをリポジトリに取り込む (vendoring)**: 最も確実で、CI もサンドボックスも
  ネットワークなしで完結する。生成物の再現性も上がる。ただし EDItEUR の配布物の再配布に
  あたるため、ライセンス条件の確認が要る。**メンテナが確認のうえ問題なければ、これが本命。**
- **自前のミラーを用意する (GitHub Releases、S3 など)**: `http_archive` の `urls` に
  フォールバックとして並べれば、EDItEUR 側の可用性に左右されなくなる。ただし
  再配布である点は vendoring と変わらず、加えてミラーの維持コストが乗る。
- **User-Agent を偽装してダウンロードする**: 202 が bot 対策なら、ブラウザを装えば通る可能性がある。
  ただし Bazel 3.7.0 の `http_archive` はカスタムヘッダに対応していない。`repository_ctx.download`
  の `auth` も `Authorization` ヘッダ専用なので、独自の repository rule を書いても足りず、
  結局 curl などを呼び出すことになる。何より、配布元が設けたアクセス制御を迂回する行為であり、
  権利者の意図を確認せずに実装すべきではない。**採らない。**
- **`--override_repository` を使う**: `--override_repository=org_editeur_v3=/path/to/dir` で、
  取得済みのディレクトリを外部リポジトリの代わりに使える (3.7.0 にある)。`--distdir` の変種ではなく、
  **sha256 の検証を経由しない**点が本質的に違う。この ADR の出発点は「sha256 が計算できない」ことなので、
  新しいリリースを追う場面ではむしろこちらしか使えない。既知の版を再現するなら `--distdir`、
  未知の版を試すなら `--override_repository`、と使い分ける。
- **`sha256` を省略する**: `http_archive` は `sha256` なしでも動くが、202 の問題は解決しないうえ、
  取得物の同一性検証を捨てることになる。さらに Bazel の distdir 探索は sha256 が
  与えられている場合にしか走らないので、`sha256` を捨てるとこの ADR が推奨する
  `--distdir` 自体が効かなくなる。**採らない。**

## 結果

- 生成系は、メンテナが手元に zip を用意できる環境でのみ実行できる。CI では実行できない。
- CI で検証できるのは、ユニットテスト (ADR-0002) と、コミット済み生成物に対する
  e2e スナップショット (`//e2e/go:snapshot_test`) の 2 つ。ただし後者が依存しているのは
  `//generated/go/v2:go` だけで、`generated/go/v3` と `generated/typescript/v2` には
  CI のカバレッジが無い。生成物の回帰を検出できるのは Go の v2 に限られる。
- 取得の問題が解けても、3.1.x への追従はそれだけでは終わらない。版が
  `org_editeur_v3.bazel` の glob、`BUILD.bazel` の filegroup 名 (`onix3p0p7`)、
  `src/Lib.hs` の `schemaRoot` の 3 箇所にハードコードされている。
- 最新スキーマへの追従は、この ADR が Accepted になるまで着手できない。
  vendoring かミラーのいずれかが決まれば、CI でも生成と差分確認ができるようになり、
  AGENTS.md が要求する「生成物の diff を読んで後方互換性を確認する」手順が
  レビューの中で回せるようになる。
