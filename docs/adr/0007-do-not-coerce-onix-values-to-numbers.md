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

当初この ADR は「Go 側は一貫して文字列型を生成しているので、言語間で型が揃う」と
主張していたが、**これは誤りだった**。レビューで指摘され、確認した結果は次のとおり。

- `generated/go/v2/code.go` の型は **struct 107 / string 8 / []string 2** で、一様ではない。
  `template/go/v2/code.mustache` は 4 分岐あり、`type X string` になるのは
  `spaceSeparatable` でも `hasElements` でもない場合だけである。
  この ADR が例に挙げている `NotificationType` は Go では struct である。
- さらに Go の `UnmarshalXML` は、コード値を**人間可読な説明文に置換する**。

```go
	switch v {
	// Use for a complete record issued earlier than ...
	case "01":
		c.Body = `Early notification`
```

つまりこの修正を入れても、同じ入力に対して TypeScript は `"01"`、Go は
`"Early notification"` を返す。**言語間の一貫性は、この修正では回復しない。**
それは型の問題ではなく、Go 側だけがコードを説明文に展開しているという、より大きな
設計上の食い違いである。別途扱う。

したがってこの ADR の根拠は、言語間の一貫性ではなく**データを壊さないこと**の一点に絞る。

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
  **その際は `parseAttributeValue` を既定の `false` のまま保つこと。** さもないと
  同じ型変換のバグが属性側で再発する。
- 型宣言と値が一致する、と書いたが、それを型検査が強制するわけではない。
  `generated/typescript/v2/code.ts` はどこからも import されておらず、
  `reader.ts` の戻り値型 `ONIXMessage` は `model.ts` で空の interface
  (`export interface ONIXMessage {}`) として定義されている。一致は規約であって、
  検査で守られてはいない。
- `parseTagValue: false` は数値化を止めるが、値の加工をすべて止めるわけではない。
  `trimValues` は既定で true のままなので前後の空白は落ちる。真偽値らしき文字列の
  変換も同時に止まる。
- TypeScript reader は CI で実行されていない。この ADR の測定は手作業であり、
  回帰を検出する仕組みは無い。
