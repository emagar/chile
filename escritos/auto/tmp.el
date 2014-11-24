(TeX-add-style-hook "tmp"
 (lambda ()
    (LaTeX-add-labels
     "sec-1"
     "sec-1-1"
     "sec-1-1-0-0-1"
     "sec-1-1-0-0-2"
     "sec-1-1-0-0-3"
     "sec-1-2")
    (TeX-run-style-hooks
     "hyperref"
     "amssymb"
     "wasysym"
     "marvosym"
     "textcomp"
     "amsmath"
     "ulem"
     "normalem"
     "wrapfig"
     "float"
     "longtable"
     "graphicx"
     "fixltx2e"
     ""
     "fontenc"
     "T1"
     "inputenc"
     "utf8"
     "latex2e"
     "art11"
     "article"
     "11pt")))

