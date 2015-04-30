(TeX-add-style-hook "litrev"
 (lambda ()
    (LaTeX-add-bibliographies
     "../bib/magar")
    (TeX-run-style-hooks
     "natbib"
     "sort"
     "longnamesfirst"
     "latex2e"
     "art12"
     "article"
     "12pt"
     "letter")))

