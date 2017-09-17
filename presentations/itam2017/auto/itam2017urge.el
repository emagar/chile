(TeX-add-style-hook
 "itam2017urge"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "xcolor=dvipsnames")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1") ("inputenc" "ansinew") ("babel" "spanish")))
   (add-to-list 'LaTeX-verbatim-environments-local "semiverbatim")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "color"
    "pifont"
    "beamerthemesplit"
    "url"
    "ae"
    "fontenc"
    "inputenc"
    "babel"
    "amsmath"
    "graphicx"
    "hyperref"
    "tikz"
    "multirow"
    "dcolumn"
    "arydshln")
   (TeX-add-symbols
    "mc"
    "be"
    "ee"
    "bq"
    "eq"
    "bd"
    "ed"
    "bi"
    "ei"
    "beq"
    "eeq"
    "bc"
    "ec"
    "circleH"
    "circleU"
    "circleR"))
 :latex)

