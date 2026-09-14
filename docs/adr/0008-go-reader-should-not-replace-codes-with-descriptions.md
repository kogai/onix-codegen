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

|                                  | v2    | v3    |
| -------------------------------- | ----- | ----- |
| コード型の数                     | 117   | 209   |
| `case` 節（コード値）の総数      | 6,314 | 7,688 |
| **説明文が他と衝突する `case`**  | **72**（19 型） | **254**（33 型） |
| `default:` で `error` を返す型   | 117   | 209   |
| `MarshalXML` の実装              | 0     | 0     |

TypeScript 側は同じジェネレータから生成されているが、**この変換を行わない**。
`generated/typescript/v2/code.ts` はすべて `export type CurrencyCode = string` で、
reader は値をそのまま返す（ADR-0007 も参照）。

### 観測された具体的な問題

**(1) 変換が単射でない。実データが壊れる。**

ONIX のコードリストには、値は違うが説明文が同一のものがある。`CurrencyCode` が最も悪い。

| コード | ONIX 側の注記 | Go が返す値 |
| ------ | ------------- | ----------- |
| `BYR`  | "Now replaced by new Belarussian Ruble (BYN): use only for historical prices that pre-date the introduction of the new Belarussian Ruble" | `Belarussian Ruble` |
| `BYN`  | "Belarus" | `Belarussian Ruble` |
| `AFA`  | "Afghanistan. DEPRECATED, replaced by AFN" | `Afghani` |
| `AFN`  | "Afghanistan (prices normally quoted as integers)" | `Afghani` |

BYR と BYN は 2016 年のデノミで **10,000:1**、AFA と AFN は 2002 年のデノミで **1,000:1** の関係にある。
つまり `Price` が `100000` で通貨が `Belarussian Ruble` という Go の値からは、
**それが 100000 BYR（= 10 BYN）なのか 100000 BYN（= 10 億 BYR）なのかを復元できない。**
金額に 10,000 倍の差が出る。

言語コードでも `hrv`/`scr` がともに `Croatian`、`scc`/`srp` がともに `Serbian` になる。

**(2) 未知のコードでドキュメント全体のパースが失敗する。**

`default:` 節は全 117（v3 は 209）の型で `error` を返す。`Read()` はこれを
`decoder.Decode()` 経由でそのまま呼び出し元に返すので、**巨大な ONIX ファイルの中に
1 つでも未知のコードがあれば、ファイル全体が読めない。**

ONIX のコードリストはスキーマ本体とは別に版が上がり、新しいコード値が随時追加される。
現在同梱しているのは v2 が Issue 36、v3 が Issue 52 で、いずれも実際の最新より古い。
**より新しい Issue のコードを含む正当な ONIX ファイルは、このクライアントでは読めない。**
これは「ONIX の仕様をなるべく後方互換性を保って追従する」という、このリポジトリの
第一の価値と正面から衝突する。

**(3) ONIX として書き戻せない。**

`MarshalXML` はどこにも実装されていない。デコードした構造体を `encoding/xml` で
書き出すと `<CurrencyCode>Afghani</CurrencyCode>` になり、**妥当な ONIX ではない。**
e2e テストは JSON に書き出しているだけなので、これを検出しない。

**(4) 言語ごとに返る値が違う。**

同じ ONIX ファイルから、Go は `"Afghani"`、TypeScript は `"AFN"` を返す。
「コード生成の仕組みでなるべくサポートできる言語を増やす」という価値に照らすと、
言語を増やすたびに「この言語はどちらの流儀か」が増えることになる。

## 決定

**Go の `UnmarshalXML` がコード値を説明文に置き換えるのをやめ、コード値をそのまま保持する。**
説明文は、値を潰す形ではなく別の経路で提供する。

そのうえで、未知のコードはエラーにせず、そのまま通す。

この ADR は方針の決定のみで、**実装は含まない**（後述の「結果」を参照）。

## 理由

- **(1) は正しさの問題であって、好みの問題ではない。** 通貨のデノミを跨いだ金額を
  復元できないのは、このライブラリを使った時点で発生するデータ破壊であり、
  利用側で回避する手段がない（元のコードはもう構造体に残っていない）。
- **(2) は後方互換性そのもの。** コードリストの版が上がるたびに既存の利用者のパースが
  壊れる設計は、ONIX を追従するライブラリとして成立しない。「未知のコードは通す」なら、
  同梱コードリストが古いままでも新しいファイルが読める。
- コード値を保持する側が可逆で、説明文を保持する側が不可逆。**可逆な方を既定にして、
  不可逆な変換は利用側が必要なときに呼ぶ**、という向きが自然。
- TypeScript が既にそうなっており（ADR-0007）、言語間で挙動が揃う。

## 検討した他の選択肢

- **案 A: 現状維持。** 採らない。(1) のデータ破壊と (2) の後方互換性の破れが残る。
  既存利用者の API を壊さない、という利点はあるが、壊れているのは API ではなく返る値。
- **案 B: 置き換えはやめるが、未知のコードは引き続きエラーにする。** 採らない。
  (1) は直るが (2) が残る。同梱コードリストより新しいファイルが読めない状態は変わらない。
- **案 C: 構造体にコードと説明文の両方のフィールドを持たせる。**
  情報は失われないので (1) は直り、(3) も `MarshalXML` を足せば直せる。
  ただし全コード型の struct 形が変わり、生成物のサイズが説明文の分だけ増える
  （v2 の 30,342 行が更に伸びる）。案の採否は実装時に改めて判断する余地がある。
- **案 D: 説明文を `Description()` メソッドとして生成する。**
  値はコードのまま、説明文は `c.Description()` で取れる。可逆性を保ったまま
  説明文も提供でき、既存の `switch` をそのまま流用できる。**現時点ではこれが有力。**
  なお現在の生成物には `Description` という型名もフィールド名も存在しない
  （v2 / v3 の `code.go` と `model.go` を確認）ので、今のところ名前は衝突しない。
  ただしフィールド名はスキーマ由来なので、コードリストの版を上げるたびに再確認は要る。

## 結果

- **これは生成コードの利用者にとって破壊的変更である。** `CurrencyCode` の値として
  `"Afghani"` を期待していたコードは `"AFN"` を受け取るようになる。
  `generated/go/{v2,v3}` はこのリポジトリが配布している成果物なので、
  **この ADR を Accepted にするかどうかはメンテナの判断を要する。**
  実装 PR はこの ADR が受理されてから出す。
- 実装時に併せて対処すべきもの:
  - `d.DecodeElement(&v, &start)` の戻り値が捨てられている（テンプレートの 2 箇所、
    生成物では全コード型）。
  - `MarshalXML` が無いため ONIX として書き戻せない (3)。置き換えをやめれば
    `encoding/xml` の既定の挙動で正しく書き出せるようになる。
  - e2e テストが JSON 出力しか見ていないため (3) を検出できない。
- **見直す条件**: 説明文を値として受け取ることに依存した利用者が実在すると分かった場合は、
  案 C / 案 D のどちらで説明文を提供するかを、その利用形態に合わせて選び直す。
