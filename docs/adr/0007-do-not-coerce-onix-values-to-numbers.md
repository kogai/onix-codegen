# ADR-0007: TypeScript reader で値の型変換を行わない

- **ステータス**: Accepted
- **日付**: 2026-09-13
- **関連**: ADR-0005 が既知の問題として記録していた件への対応

## 背景

fast-xml-parser は既定で、要素の値が数値に見えればその型に変換する。ONIX のデータに
これを適用すると壊れる。実際に測った結果が次である。

```
既定                  {"NotificationType":1,"ProductIDType":2,"IDValue":62124983,
                       "ISBN13":9784062124983,"Price":1200.5}
parseTagValue:false   {"NotificationType":"01","ProductIDType":"02","IDValue":"062124983",
                       "ISBN13":"9784062124983","Price":"1200.50"}
```

- `NotificationType` の `"01"` が `1` になる。ONIX のコード値は先頭のゼロを含めて意味を持つ
  2 桁の文字列であり、`1` は別物である。
- `IDValue` の `"062124983"` が `62124983` になる。識別子から先頭ゼロが落ちている。
- `Price` の `"1200.50"` が `1200.5` になる。金額の桁が落ちている。
- `ISBN13` は 13 桁なので値としては保たれるが、型が `number` になる。

これは fast-xml-parser v5 で入った挙動ではない。v3 でも同じで、`parseTrueNumberOnly: false`
と v5 の `numberParseOptions.leadingZeros: true` は同じ結果を出す。つまり**最初から壊れていた**。

生成されるコードはこの挙動と矛盾している。`generated/typescript/v2/code.ts` は
コード型を文字列として宣言している。

```ts
export type NotificationType = string
```

宣言は `string`、実際に返るのは `number`。型が嘘をついている状態だった。

Go 側の生成コードは、同じスキーマから一貫して文字列型を生成している
(`type {{xmlReferenceName}} string`)。同じ入力に対して言語ごとに違う型が返るのは、
「同じスキーマから複数言語のクライアントを生成する」というこのリポジトリの目的
(AGENTS.md) に照らして不整合である。

## 決定

`parseTagValue: false` を指定し、要素の値を一切変換しない。

## 理由

ONIX のスキーマにおいて、要素の値はすべて文字列である。数値に見えるものも、
コード値・識別子・金額といった「たまたま数字で構成された文字列」であって、数値ではない。
パーサに推測させる余地は無い。

個別に対処する案 (コード型だけ文字列に戻す、など) は採らない。どの要素が数値に見えるかは
入力データ次第で、`IDValue` のように値によって変換されたりされなかったりする。
入力に依存して型が変わるほうが、常に文字列であるより扱いにくい。

## 結果

- 生成される TypeScript reader の出力は、これまで数値だった値が文字列になる。
  **これは後方互換性を壊す変更である。** ただし、壊れる側が正しい出力なので受け入れる。
  生成される型宣言 (`= string`) と実際の値がこれで一致する。
- 属性の扱いはこの ADR の範囲外。fast-xml-parser v5 は既定で `ignoreAttributes: true` なので、
  `refname` / `shortname` / `datestamp` といった ONIX の属性は現在そもそも読まれていない。
  これも実装上の欠落だが、修正すると出力の形が変わるため別途扱う。
- TypeScript reader は CI で実行されていない。この ADR の測定は手作業であり、
  回帰を検出する仕組みは無い。
