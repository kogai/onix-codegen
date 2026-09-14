# ADR-0003: EDItEUR スキーマの取得方法

- **ステータス**: Accepted (2026-09-13, #58 にて承認)
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

**1. CI から: CAPTCHA チャレンジが返る**

```
WARNING: Download from https://www.editeur.org/files/ONIX%202.1/...Issue_36.zip
         failed: UnrecoverableHttpException GET returned 202 Accepted
```

zip の代わりに 202 が返り、Bazel はこれを回復不能なエラーとして扱う。

この 202 の正体は、CI ランナーから素の GET を投げて確認した。**SiteGround の
CAPTCHA チャレンジ**である。

```
HTTP/2 202
server: nginx
sg-captcha: challenge
x-robots-tag: noindex
content-type: text/html
content-length: 248

<html><head><meta http-equiv="refresh"
  content="0;/.well-known/sgcaptcha/?r=%2Ffiles%2FONIX%202.1%2F...zip&y=ipc:...">
</head></html>
```

重要なのは、**これがファイル固有の問題ではない**ことである。同じ応答が返るのは
zip だけでなく、ダウンロードページ (`/93/Release-3.0-and-3.1-Downloads/`) 自体も同様だった。
`sg-captcha: challenge` と、クライアントの IP を埋め込んだリダイレクト先
(`y=ipr:<runner の IP>`) から、判定はリクエスト元に対して行われていると分かる。

したがって次のことが言える。

- URL が古いから失敗しているのではない。新しいリリースの URL に変えても結果は同じである。
- リトライやミラー URL の追加では解決しない。
- **CI から自動でダウンロードすることは、CAPTCHA を解かない限り不可能である。**
  そして CAPTCHA はまさに、配布元が自動アクセスを制限するために置いたものである。

**2. 開発用のサンドボックス環境から: そもそも到達できない**

egress ポリシーにより `www.editeur.org:443` への CONNECT が 403 で拒否される。
このため、URL の確認も、`http_archive` が要求する `sha256` の計算もできない。

この 2 つが重なった結果、次のことがすべて止まっている。

- コード生成 (`make build`、`make generated/...`)
- 最新スキーマへの追従 (現在 ONIX 2.1 Issue 36 / 3.0 Issue 52)

追従先の最新版がどれかは、**この ADR の時点では確定できていない**。二次情報では
ONIX 3.1.x + codelists Issue 74 とされているが (当初 Issue 73 と書いていたが、より新しい情報では 74 が 3.0 / 3.1 共通の現行版)、editeur.org に到達できない以上、
一次情報での裏取りができていない。そして `WORKSPACE` が実際に必要とするもの、すなわち
**zip の URL とファイル名、その sha256 は完全に不明**である。追従作業は、まずここの確認から始まる。

なお、ユニットテストは ADR-0002 でこの依存から切り離したため影響を受けない。
止まっているのは生成系だけである。

## ライセンスについて調べたこと

「ライセンスの判断が要る」で止めるのは雑なので、条件そのものを調べた。ただし
**editeur.org に到達できないため、一次情報は確認できていない**。以下は二次情報である。

EDItEUR は、仕様・XML ツール・ガイダンスの利用を「無料、登録不要、ロイヤリティ不要、
highly-permissive licence のもとで提供」と説明している。ここだけ見ると再配布も
問題なさそうに読める。

しかし利用許諾の本文とされる記述は、より限定的である。要旨は次のとおり。

> DTD または XML Schema の一部を、**自組織内での厳密に内部的な利用を除いて**、
> 追加・削除・改変したり、外部での利用のために複製したりしないことに同意する。
> 内部的でない目的で追加・改変・抜粋を行いたい場合は、まず EDItEUR に通知すること。

これが正しければ、**公開リポジトリへの取り込みもミラーの設置も「厳密に内部的な利用」には
あたらない**。つまり vendoring とミラーは、単にライセンスを読めば済む話ではなく、
**EDItEUR への事前通知を要する行為**ということになる。

一方 `--distdir` 運用は、メンテナが自分で使うために手元にダウンロードするだけなので、
何も再配布しない。この点でも他の選択肢と質的に違う。

## 決定

**暫定運用として `--distdir` を採用する。** 恒久策 (vendoring / ミラー) の選択は保留する。

この ADR は #58 で承認された。承認されたのは以下の内容であり、**vendoring と
ミラーの実施を承認したものではない** (それらは下記のとおり EDItEUR への通知を
伴う可能性が高く、この ADR 自身が判断を保留している)。

- `--distdir` を正式な取得手段とすること。`.bazelrc` に
  `build --distdir=third_party/distdir` を追加し、`third_party/distdir/` に
  手順を置いた。アーカイブ自体は gitignore される。
- 恒久策の選択は引き続き保留であること。

以下は承認時点の判断材料であり、記録として残す。

**選択肢と判断材料:** 有力な選択肢のうち 2 つが
第三者著作物の再配布を伴い、上記のとおり EDItEUR への通知が前提になる可能性が高いため。
これはメンテナが行うべき判断であり、実装で既成事実にしてよいものではない。

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
  ネットワークなしで完結する。生成物の再現性も上がる。ただし上記のとおり、公開リポジトリへの
  取り込みは「内部的な利用」を超えるため、**EDItEUR への事前通知が要る可能性が高い**。
  通知して問題なければ、これが本命。
- **自前のミラーを用意する (GitHub Releases、S3 など)**: `http_archive` の `urls` に
  フォールバックとして並べれば、EDItEUR 側の可用性に左右されなくなる。ただし
  再配布である点は vendoring と変わらず (同じく通知が要る)、加えてミラーの維持コストが乗る。
- **User-Agent を偽装してダウンロードする**: 上の調査で、これは筋が悪いだけでなく
  技術的にも足りないことが分かった。返ってくるのは CAPTCHA チャレンジであり、
  通過するには JS の実行とクッキーの保持が要る。ヘッダを 1 つ足して済む話ではない。
  そして何より、配布元が自動アクセスを制限するために置いた仕組みを迂回する行為である。
  **採らない。**
- **v2 だけは git 履歴から復元する**: 見落としていたが、**このリポジトリの履歴に
  ONIX 2.1 rev03 のスキーマ一式が残っている**。2021-01-19 のコミット 9352123 が
  `2_1_rev03_schema/` を削除した際の親コミットから、ネットワークなしで取り出せる。

  ```sh
  mkdir -p schema/v2
  for f in ONIX_BookProduct_CodeLists.xsd ONIX_BookProduct_Release2.1_reference.xsd \
           ONIX_BookProduct_Release2.1_short.xsd ONIX_XHTML_Subset.xsd \
           ONIX_XHTML_Subset_reference.xsd ONIX_XHTML_Subset_short.xsd ; do
    git show 9352123^:2_1_rev03_schema/$f > schema/v2/$f
  done
  ```

  これで v2 のコード生成は editeur.org なしで動く。**再配布という観点でも他の選択肢と
  質的に違う**: これらのファイルは 2021 年から現在まで、このリポジトリの公開履歴に
  存在し続けている。復元は新たな公開ではなく、既に起きている公開の追認である。
  そして**これは 2.1 のみで、3.0 / 3.1 は履歴にも無い**。

  **追記 (2026-09-14):** この手順は `make schema-from-history` として実装した。
  復元先は gitignore 済みの `schema/v2` で、EDItEUR のファイルをリポジトリに
  コミットするわけではない。したがって「メンテナが意図的に削除したものを戻すか」という
  判断は発生せず、再配布の状態も変わらない。なお、これで `make generated/go/v2` が
  動くようになるわけではない。`build` → `schema` の連鎖が v3 も要求するため、
  v2 だけを生成するには実行ファイルを直接呼ぶ必要がある。
- **`--override_repository` を使う**: `--override_repository=org_editeur_v3=/path/to/dir` で、
  取得済みのディレクトリを外部リポジトリの代わりに使える (3.7.0 にある)。`--distdir` の変種ではなく、
  **sha256 の検証を経由しない**点が本質的に違う。この ADR の出発点は「sha256 が計算できない」ことなので、
  新しいリリースを追う場面ではむしろこちらしか使えない。既知の版を再現するなら `--distdir`、
  未知の版を試すなら `--override_repository`、と使い分ける。
- **`sha256` を省略する**: `http_archive` は `sha256` なしでも動くが、202 の問題は解決しないうえ、
  取得物の同一性検証を捨てることになる。さらに Bazel の distdir 探索は sha256 が
  与えられている場合にしか走らないので、`sha256` を捨てるとこの ADR が推奨する
  `--distdir` 自体が効かなくなる。**採らない。**

## ライセンス条項の一次情報について

上記の履歴から、EDItEUR の配布物そのもの (`readme.txt` / `readme2.txt` および各 XSD の
ヘッダ) を確認した。記載されているのは著作権表示のみで、**許諾条項は含まれていない**。

```
COPYRIGHT (c) EDItEUR 2005–2013
(c) 2004-2006 EDItEUR / http://www.editeur.org/
```

つまり許諾の本文は editeur.org 上にしか存在せず、到達できない以上、この ADR の
ライセンスに関する記述は二次情報のままである。この点は解消できなかった。

## 結果

- 生成系は、メンテナが手元に zip を用意できる環境でのみ実行できる。**CI では実行できない。**
  これは当面の不便ではなく、CAPTCHA がある限り恒久的な制約である。この点は、
  vendoring かミラーかを判断する材料として重い。「そのうち直る」類の問題ではない。
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
