(TeX-add-style-hook
 "urge10"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "letter" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper" "right=1.25in" "left=1.25in" "top=1in" "bottom=1in") ("inputenc" "utf8") ("fontenc" "T1") ("url" "hyphens") ("graphicx" "pdftex") ("helvet" "scaled=.90") ("natbib" "longnamesfirst" "sort") ("todonotes" "colorinlistoftodos" "textsize=small")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "geometry"
    "setspace"
    "inputenc"
    "fontenc"
    "amsmath"
    "url"
    "graphicx"
    "tikz"
    "mathptmx"
    "helvet"
    "courier"
    "natbib"
    "rotating"
    "caption"
    "dcolumn"
    "arydshln"
    "todonotes")
   (TeX-add-symbols
    '("ges" 1)
    '("vp" 1)
    '("emm" 1)
    "mc")
   (LaTeX-add-labels
    "T:billDescriptives"
    "T:example"
    "f:agendaUrg"
    "F:game"
    "F:example"
    "F:predictions"
    "T:chairsSeats"
    "t:urgenLogit"
    "F:avgMg"
    "F:sims")
   (LaTeX-add-bibliographies
    "/home/eric/Dropbox/mydocs/magar"))
 :latex)

