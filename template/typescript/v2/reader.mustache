import { promises as fs } from "fs";
import { XMLParser } from "fast-xml-parser";
import { ONIXMessage } from "./model"

// v5 reports the XML declaration as a `?xml` key, which v3 did not. Ignoring it
// keeps the parsed shape identical to what this reader used to return.
const parser = new XMLParser({ ignoreDeclaration: true });

export const read = async (input: string): Promise<ONIXMessage> => {
  try {
    const file = await fs.readFile(input);
    const parsed = parser.parse(file.toString());
    return parsed as ONIXMessage;
  } catch (error) {
    throw error;
  }
};
