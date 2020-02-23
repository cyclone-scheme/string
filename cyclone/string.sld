;; A cursor-oriented string library.  Provides efficient string
;; utilities for implementations with or without fast random-access
;; strings.
;; Copyright (c) 2012-2017 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define-library (cyclone string)
  (export
   string-cursor?
   string-cursor-start string-cursor-end string-cursor-ref
   string-cursor<? string-cursor<=? string-cursor>? string-cursor>=?
   string-cursor=? string-cursor-next string-cursor-prev substring-cursor
   string-cursor->index string-index->cursor
   string-cursor-forward string-cursor-back
   string-null? string-every string-any
   string-join string-split string-count
   string-trim string-trim-left string-trim-right
   string-mismatch string-mismatch-right
   string-prefix? string-suffix?
   string-find string-find-right string-find? string-skip string-skip-right
   string-fold string-fold-right string-map string-for-each
   string-contains make-string-searcher
   string-downcase-ascii string-upcase-ascii)
  (import (scheme base)
          (scheme char)
          (cyclone char-set base))
  (include "string.scm"))
