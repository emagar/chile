(TeX-add-style-hook "urge04"
 (lambda ()
    (LaTeX-add-bibliographies
     "/home/eric/Dropbox/mydocs/magar")
    (LaTeX-add-labels
     "f:costly"
     "t:exBillFail"
     "f:chiUruEql"
     "t:congressSeats"
     "t:freqUrg"
     "T:billDescriptives"
     "T:billFreqByNurg"
     "f:depvarHistog"
     "T:sponsorsOfUrgBills"
     "t:logit"
     "F:billPaths"
     "t:negbin")
    (TeX-add-symbols
     '("emm" 1)
     "mc")
    (TeX-run-style-hooks
     "todonotes"
     "textsize=small"
     "colorinlistoftodos"
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

