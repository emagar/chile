(TeX-add-style-hook "urge"
 (lambda ()
    (LaTeX-add-bibliographies
     "../bib/magar")
    (LaTeX-add-labels
     "f:costly"
     "f:skip"
     "T:agendaPayoffs"
     "t:congressSeats"
     "T:billDescriptives"
     "T:billFreqByNurg"
     "T:sponsorsOfUrgBills"
     "f:depvarHistog"
     "f:depvar"
     "t:negbin"
     "f:senChile"
     "F:billPaths")
    (TeX-add-symbols
     "mc")
    (TeX-run-style-hooks
     "arydshln"
     "dcolumn"
     "caption"
     "rotating"
     "natbib"
     "sort"
     "longnamesfirst"
     "courier"
     "helvet"
     "scaled=.90"
     "mathptmx"
     "tikz"
     "graphicx"
     "pdftex"
     "url"
     "amsmath"
     "fontenc"
     "T1"
     "inputenc"
     "utf8"
     "setspace"
     ""
     "geometry"
     "bottom=1in"
     "top=1in"
     "left=1.25in"
     "right=1.25in"
     "letterpaper"
     "latex2e"
     "art12"
     "article"
     "12pt"
     "letter")))

