load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "org_editeur_release",
    build_file = "//:org_editeur_release.bazel",
    sha256 = "e929f37b5e63fc44a82873c66430df9d0479d3e1b965b1609f439688d002647d",
    url = "https://www.editeur.org/files/ONIX%202.1/ONIX_for_Books_Release2-1_rev03_docs+codes_Issue_36.zip",
)

http_archive(
    name = "org_editeur_release_2_1_rev03_schema",
    build_file = "//:org_editeur_release.bazel",
    # sha256 = "e929f37b5e63fc44a82873c66430df9d0479d3e1b965b1609f439688d002647d",
    url = "https://www.editeur.org/files/ONIX%202.1/ONIX_for_Books_Release2-1_rev03_schema+codes_Issue_36.zip",
)

http_archive(
    name = "build_bazel_rules_nodejs",
    sha256 = "b3521b29c7cb0c47a1a735cce7e7e811a4f80d8e3720cf3a1b624533e4bb7cb6",
    urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/2.3.2/rules_nodejs-2.3.2.tar.gz"],
)

# The npm_install rule runs yarn anytime the package.json or package-lock.json file changes.
# It also extracts any Bazel rules distributed in an npm package.
load("@build_bazel_rules_nodejs//:index.bzl", "npm_install")

npm_install(
    name = "npm",
    package_json = "//:package.json",
    package_lock_json = "//:package-lock.json",
)
