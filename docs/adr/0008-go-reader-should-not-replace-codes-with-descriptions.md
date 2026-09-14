# ADR-0008: Go の reader が ONIX コードを説明文に置き換えるのをやめる

- **ステータス**: Proposed（要判断 — 生成コードの公開 API を壊す変更を伴う）
- **日付**: 2026-09-14

## 背景

生成された Go クライアントは、ONIX のコード値を**人間可読な説明文に置き換えて**返す。
`template/go/{v2,v3}/code.mustache` の `UnmarshalXML` がそれを行っている。

```go
// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CurrencyCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	// Afghanistan. DEPRECATED, replaced by AFN
	case "AFA":
		c.Body = `Afghani`
	// Afghanistan (prices normally quoted as integers)
	case "AFN":
		c.Body = `Afghani`
	...
	default:
		return fmt.Errorf("undefined code for CurrencyCode has been passed, got [%s]", v)
	}
}
```

生成物を実測した結果は次のとおり。

|                                 | v2    | v3    |
| ------------------------------- | ----- | ----- |
| コード型の数                    | 117   | 209   |
| `case` 節（コード値）の総数     | 6,314 | 7,688 |
| 説明文が他と衝突する `case`     | 72（19 型） | 254（33 型） |
| `default:` で `error` を返す型  | 117   | 209   |
| **`case` を 1 つも持たない型**  | **9** | **45** |
| `MarshalXML` の実装             | 0     | 0     |

### 観測された問題 (1): v3 の Go クライアントは ONIX 3.0 をそもそも読めない

`case` を 1 つも持たない型の `UnmarshalXML` は、こうなる。

```go
func (c *DtDotNonEmptyString) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotNonEmptyString has been passed, got [%s]", v)
	}
}
```

**どんな値が来ても必ず `error` を返す。** これらはコードリストではなく XSD の
データ型（`dt:NonEmptyString` など）なので、列挙値が存在せず `case` が 0 個になる。

属性としてしか使われない型なら `UnmarshalXMLAttr` の方が呼ばれるので実害はない。
問題は**要素として使われている**ものである。

| 版  | `case` 0 個の型が要素として使われているフィールド数 |
| --- | --- |
| v2  | **2**（`ReligiousTextID` ×1、`IntermediaryAvailabilityCode` ×1） |
| v3  | **225**（21 型） |

v3 の内訳の筆頭は `DtDotNonEmptyString` の **152 フィールド**で、その中には
ONIX 3.0 の必須要素が含まれる。

```go
IDValue DtDotNonEmptyString `xml:"b244"`
```

`<b244>` を含む ONIX 3.0 ファイルは、**値が何であれ必ずパースに失敗する。**
つまり **v3 の Go クライアントは、実在する ONIX 3.0 ファイルを読めない。**
README の「Schema Version 3 with Codes Issue52: OK」は成り立っていない。

e2e テストは v2 しか通していない（`e2e/go/main.go` が
`generated/go/v2` を import している）ため、これは検出されていなかった。

### 観測された問題 (2): 変換が単射でない

ONIX のコードリストには、値は違うが説明文が同一のものがある。`CurrencyCode` が最も悪い。

| コード | ONIX 側の注記（v2 の生成物より、全文） | Go が返す値 |
| ------ | ------------------------------------- | ----------- |
| `AFA`  | "Afghanistan. DEPRECATED, replaced by AFN" | `Afghani` |
| `AFN`  | "Afghanistan (prices normally quoted as integers)" | `Afghani` |

AFA と AFN は 2002 年のデノミで **1,000:1** の関係にある。つまり `Price` が
`100000` で通貨が `Afghani` という Go の値からは、**それが 100000 AFA
（= 100 AFN）なのか 100000 AFN（= 1 億 AFA）なのかを復元できない。**
`RUB`/`RUR` も同じ形で衝突する（1998 年のデノミ、1,000:1）。

`BYR`/`BYN` は **v2 でのみ**衝突する。v2 はどちらも `Belarussian Ruble` を返すが、
v3 は `(Old) Belarussian Ruble` / `Belarussian Ruble` と書き分けられているため
衝突しない。**版によって壊れ方が違う**、という点自体がこの設計の脆さを示している。

言語コードでも `hrv`/`scr` がともに `Croatian`、`scc`/`srp` がともに `Serbian` になる
（両版）。

### 観測された問題 (3): 未知のコードでドキュメント全体のパースが失敗する

`default:` 節は全 117（v3 は 209）の型で `error` を返す。`Read()` はこれを
`decoder.Decode()` 経由でそのまま呼び出し元に返すので、**巨大な ONIX ファイルの中に
1 つでも未知のコードがあれば、ファイル全体が読めない。**

ただし**版によって意味が違う**点に注意が要る。

- **v2**: 同梱の Issue 36 は、EDItEUR が ONIX 2.1 向けに出した**最後のコードリスト**で
  ある（Issue 37 以降は 2.1 用のリストを含まない）。したがって v2 側は「古い」のでは
  なく、**これ以上新しくならない**。この経路での破綻は起きにくい。
- **v3**: 同梱の Issue 52 に対して、現行は Issue 74（2026-07-21）。**22 版ぶんの
  コード値が未知として扱われる。** ここは「ONIX の仕様をなるべく後方互換性を保って
  追従する」という価値と正面から衝突する。

### 観測された問題 (4): ONIX として書き戻せない

`MarshalXML` はどこにも実装されていない。デコードした構造体を `encoding/xml` で
書き出すと `<CurrencyCode>Afghani</CurrencyCode>` になり、**妥当な ONIX ではない。**
e2e テストは JSON に書き出しているだけなので、これを検出しない。

**置き換えをやめるだけでは、これは直りきらない。** スペース区切りのコードリスト型
（v2 で 5 型、v3 で 6 型）は `[]string` を土台にしているため、`encoding/xml` の既定の
挙動では**繰り返し要素**になり、スペース区切りの単一要素には戻らない。go1.24.7 で実測:

```
CountryCodeList{"GB", "US"}  ->  <b090><b091>GB</b091><b091>US</b091></b090>
```

これらの型には、置き換えをやめたうえで**独自の `MarshalXML` が要る**。

### 観測された問題 (5): 言語ごとに返る値が違う

`generated/typescript/v2/code.ts` は全 116 型が `export type X = string` で、
**Go のような説明文への置き換えはしない。** 同じ ONIX ファイルから
Go は `"Afghani"`、TypeScript は `"AFN"` を返す。

（なお TypeScript 側にも別種の値の変換がある。`main` の `reader.ts` は
fast-xml-parser の既定設定で `xml.parse()` を呼んでいるため、数値に見える値を
number に変換してしまう。これは ADR-0007 と #64 で扱っており、本 ADR の対象外。
ADR-0007 は本 ADR 執筆時点で未マージ。）

## 決定

**Go の `UnmarshalXML` がコード値を説明文に置き換えるのをやめ、コード値をそのまま
保持する。** 未知のコードはエラーにせず通す。説明文は `Description()` メソッドとして
別途生成する（案 D）。

この ADR は方針の決定のみで、**実装は含まない**（後述の「結果」を参照）。

## 理由

- **(1) は「壊れている」で済む話ではなく、v3 のクライアントが機能していない。**
  コード値をそのまま返すようにすれば、`case` を 1 つも持たない型は
  「素通しする型」になり、225 フィールドが動き出す。
- **(2) は正しさの問題であって、好みの問題ではない。** 通貨のデノミを跨いだ金額を
  復元できないのは、このライブラリを使った時点で発生するデータ破壊であり、
  利用側で回避する手段がない（元のコードはもう構造体に残っていない）。
- **(3) は後方互換性そのもの。** コードリストの版が上がるたびに既存の利用者のパースが
  壊れる設計は、ONIX を追従するライブラリとして成立しない。
- コード値を保持する側が可逆で、説明文を保持する側が不可逆。**可逆な方を既定にして、
  不可逆な変換は利用側が必要なときに呼ぶ**、という向きが自然。
- 案 D を採るのは、値の形（`string` / `[]string`）を変えずに説明文も提供できるため。
  現在の生成物には `Description` という型名もフィールド名も存在しない
  （v2 / v3 の `code.go` と `model.go` で確認）ので、名前は衝突しない。
  ただしフィールド名はスキーマ由来なので、コードリストの版を上げるたびに再確認が要る。

## 検討した他の選択肢

- **案 A: 現状維持。** 採らない。v3 が読めない状態が残る。
- **案 B: 置き換えはやめるが、未知のコードは引き続きエラーにする。** 採らない。
  (1) と (2) は直るが (3) が残り、v3 は Issue 52 より新しいコードで落ち続ける。
- **案 C: 構造体にコードと説明文の両方のフィールドを持たせる。** 採らない。
  情報は失われないが、全コード型の struct 形が変わるため利用側の破壊が案 D より大きい。
- **案 D: 説明文を `Description()` メソッドとして生成する。** **これを採る。**
- **案 E: 既存の `generated/go/v2`・`v3` は据え置き、新しい出力先を足す。**
  AGENTS.md の「既存の出力を置き換えず新しい版として足す」方針に沿う案で、
  既存利用者を一切壊さない。ただし v3 が読めない問題は「新しい方を使ってください」
  でしか解決せず、**壊れた成果物を配布し続けることになる。**
  破壊的変更を避けたい場合の次善策として、メンテナの判断に委ねる。

## 結果

- **これは生成コードの利用者にとって破壊的変更である。** `CurrencyCode` の値として
  `"Afghani"` を期待していたコードは `"AFN"` を受け取るようになる。
  `generated/go/{v2,v3}` はこのリポジトリが配布している成果物なので、
  **この ADR を Accepted にするかどうかはメンテナの判断を要する。**
  実装 PR はこの ADR が受理されてから出す。
- 実装時に併せて対処すべきもの:
  - `d.DecodeElement(&v, &start)` の戻り値が捨てられている（テンプレートの 2 箇所、
    生成物では全コード型）。
  - スペース区切りのコードリスト型に `MarshalXML` が要る (4)。
  - **e2e テストが v2 しか通していない。** v3 を通すケースを足さない限り、
    (1) のような破綻はまた検出されない。
- **見直す条件**: 説明文を値として受け取ることに依存した利用者が実在すると分かった
  場合は、案 C / 案 E のどちらに寄せるかを、その利用形態に合わせて選び直す。
