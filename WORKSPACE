load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "org_editeur_v2",
    build_file = "//:org_editeur_v2.bazel",
    sha256 = "8fe932426066538a1d4ef17f41a70432d3eef7bb09c54e9262b24cc883cdb461",
    url = "https://www.editeur.org/files/ONIX%202.1/ONIX_for_Books_Release2-1_rev03_schema+codes_Issue_36.zip",
)

http_archive(
    name = "org_editeur_v3",
    build_file = "//:org_editeur_v3.bazel",
    sha256 = "c77c5b23ac8ea66f14ed75a05c61629623e5b1cf8146ab2bf2512d3faf695c74",
    url = "https://www.editeur.org/files/ONIX%203/ONIX_BookProduct_XSD_schema+codes_Issue_52.zip",
)

http_archive(
    name = "build_bazel_rules_nodejs",
    sha256 = "dd4dc46066e2ce034cba0c81aa3e862b27e8e8d95871f567359f7a534cccb666",
    urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/3.1.0/rules_nodejs-3.1.0.tar.gz"],
)

# The npm_install rule runs yarn anytime the package.json or package-lock.json file changes.
# It also extracts any Bazel rules distributed in an npm package.
load("@build_bazel_rules_nodejs//:index.bzl", "npm_install")

npm_install(
    name = "npm",
    package_json = "//:package.json",
    package_lock_json = "//:package-lock.json",
)

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "207fad3e6689135c5d8713e5a17ba9d1290238f47b9ba545b63d9303406209c6",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.24.7/rules_go-v0.24.7.tar.gz",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.24.7/rules_go-v0.24.7.tar.gz",
    ],
)

http_archive(
    name = "bazel_gazelle",
    sha256 = "b760f7fe75173886007f7c2e616a21241208f3d90e8657dc65d36a771e916b6a",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.39.1/bazel-gazelle-v0.39.1.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.39.1/bazel-gazelle-v0.39.1.tar.gz",
    ],
)

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")

go_rules_dependencies()

go_register_toolchains()

gazelle_dependencies()
