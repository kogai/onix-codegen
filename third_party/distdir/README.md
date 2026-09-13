# distdir

Bazel looks here for archives before going to the network, so the ONIX schema
zips can be placed by hand when editeur.org will not serve them to an automated
request. See `docs/adr/0003-editeur-schema-acquisition.md`.

Download the archive named in `MODULE.bazel` (or `WORKSPACE`) with a browser,
drop the file in this directory unrenamed, and build as usual:

    npx bazelisk build onix_v3

Bazel matches the file by its sha256, so a wrong or truncated download fails
loudly rather than silently producing bad output.

Archives themselves are gitignored — nothing from EDItEUR is committed here.
