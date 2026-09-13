import { promises as fs } from "fs";
import { XMLParser } from "fast-xml-parser";
import { ONIXMessage } from "./model"

// Every ONIX element value is a string, and the generated types say so, so the
// parser must not coerce: without parseTagValue: false it turns the code "01"
// into 1 and the price "1200.50" into 1200.5.
//
// ignoreDeclaration and ignorePiTags keep the parsed shape to the document's
// own elements. Note that v5 decodes entity references where v3 did not; that
// change is deliberate, see
// docs/adr/0005-fast-xml-parser-v5-behaviour-changes.md
const parser = new XMLParser({
  ignoreDeclaration: true,
  ignorePiTags: true,
  parseTagValue: false,
});

export const read = async (input: string): Promise<ONIXMessage> => {
  try {
    const file = await fs.readFile(input);
    const parsed = parser.parse(file.toString());
    return parsed as ONIXMessage;
  } catch (error) {
    throw error;
  }
};
