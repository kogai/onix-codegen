import path from "path";
import { promises as fs } from "fs";
import { parse } from "node-html-parser";
import { safeDump } from "js-yaml";
import strip from "strip-indent";

const main = async () => {
  const codelistsRoot = path.join(
    process.cwd(),
    "external",
    "org_editeur_release",
    "ONIX_for_Books_Release2-1_rev03_docs+codes_Issue_36",
    "codelists"
  );

  const files = await fs.readdir(codelistsRoot);
  const yml = await Promise.all(
    files.map(async (file) => {
      const data = await fs.readFile(path.join(codelistsRoot, file), {
        encoding: "utf-8",
      });

      // TODO: codelist.html does not contain xmlReferenceName. maybe need to parse pdf?
      const xmlReferenceName = parse(data).querySelector("title").text;
      const codeDescription = parse(data).querySelector(".listHeading").text;

      const codes = parse(data)
        .querySelectorAll("table tr")
        .filter((_, i) => i > 2)
        .filter((t) => t.querySelectorAll("td").length > 2)
        .map((t) => {
          const [value, description, notes] = t.querySelectorAll("td");
          return {
            value: value.text,
            description: description.text,
            notes: notes.text
              .split("\n")
              .map((s) => strip(s))
              .join(" ")
              .trimEnd(),
          };
        });
      return {
        xmlReferenceName,
        description: codeDescription,
        codes,
      };
    })
  );
  await fs.writeFile("code.yml", safeDump(yml).trim());
};

main();
