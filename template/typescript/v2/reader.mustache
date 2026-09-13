import { promises as fs } from "fs";
import { XMLParser } from "fast-xml-parser";
import { ONIXMessage } from "./model"

// v5 surfaces the XML declaration and any other processing instruction as
// `?name` keys, which v3 did not; both are suppressed to keep the parsed shape
// to the document's own elements. Note that v5 also decodes entity references
// (v3 returned "&amp;" verbatim) — that difference is deliberate, see
// docs/adr/0005-fast-xml-parser-v5-behaviour-changes.md
const parser = new XMLParser({ ignoreDeclaration: true, ignorePiTags: true });

export const read = async (input: string): Promise<ONIXMessage> => {
  try {
    const file = await fs.readFile(input);
    const parsed = parser.parse(file.toString());
    return parsed as ONIXMessage;
  } catch (error) {
    throw error;
  }
};
