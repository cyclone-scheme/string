(package
  (name string)
  (version 0.1)
  (license "BSD")
  (authors "Arthur Maciel <arthurmaciel at gmail dot com")
  (maintainers "Arthur Maciel <arthurmaciel at gmail dot com")
  (description "Cursor-oriented string library ported from Chibi")
  (tags "data-structures")
  (docs "https://github.com/cyclone-scheme/cyclone-winds/wiki/string")
  (test "test.scm")
  (dependencies (cyclone char-set))
  (test-dependencies ())
  (foreign-dependencies ())
  (library
    (name (cyclone string))
    (description "Only library provided by the package")))