import { promises as fs } from "fs";
import xml from "fast-xml-parser";

type ONIXMessage = {};

export const read = async (input: string): Promise<ONIXMessage> => {
  try {
    const file = await fs.readFile(input);
    const parsed = xml.parse(file.toString());
    return parsed as ONIXMessage;
  } catch (error) {
    throw error;
  }
};
