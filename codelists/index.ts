import path from "path";
import { promises as fs } from "fs";

const codelistsRoot = path.join(
  process.cwd(),
  "external",
  "org_editeur_release",
  "ONIX_for_Books_Release2-1_rev03_docs+codes_Issue_36",
  "codelists"
);

console.log(codelistsRoot);

// console.log(
//   "Hello",
//   fs.readdirSync(
//     "./external///"
//   )
// );
