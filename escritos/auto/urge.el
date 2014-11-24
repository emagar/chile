(TeX-add-style-hook "urge"
 (lambda ()
    (LaTeX-add-bibliographies
     "../bib/magar")
    (LaTeX-add-labels
     "T:webInconsistencies"
     "T:yearProp")
    (TeX-add-symbols
     "mc")
    (TeX-run-style-hooks
     "natbib"
     "sort"
     "longnamesfirst"
     "times"
     "url"
     "amsmath"
     "fontenc"
     "T1"
     "inputenc"
     "utf8"
     "latex2e"
     "art10"
     "article"
     "")))

