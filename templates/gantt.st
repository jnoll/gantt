\\documentclass[12pt,pgf]{standalone}
\\usepackage{pgfgantt}
\\usepackage[T1]{fontenc}
\\renewcommand*\\familydefault{\\sfdefault} % set base font to sans serif.
$if(font)$
\\usepackage{$font$}
$endif$
\\begin{document}

\\newganttlinktype{slipstart}{
\\ganttsetstartanchor{north west}
\\ganttsetendanchor{north west}
\\draw [dashed, |->|]
(\\xLeft, \\yUpper+2) --
(\\xRight, \\yLower+2);
}

\\newganttlinktype{slipend}{
\\ganttsetstartanchor{south east}
\\ganttsetendanchor{south east}
\\draw [dashed, |->|]
(\\xLeft, \\yUpper-2) --
(\\xRight, \\yLower-2);
}

\\newganttlinktype{slipms}{
\\ganttsetstartanchor{east}
\\ganttsetendanchor{west}
\\draw [dashed, ->]
(\\xLeft, \\yUpper) --
(\\xRight, \\yLower);
}

\\def\\pgfcalendarweekdayletter#1{\\ifcase#1M\\or T\\or W\\or R\\or F\\or S\\or U\\fi}

\\def\\pgfcalendarmonthletter#1{\\ifcase#1o\\or J\\or F\\or M\\or A\\or M\\or J\\or J\\or A\\or S\\or O\\or N\\or D\\fi}

\\begin{ganttchart}[%
  $if(chartoptions)$$chartoptions$$else$
  hgrid,   
  $vgrid$
  time slot format=isodate,
  y unit chart=\\baselineskip,
  y unit title=\\baselineskip,
  title height=1,
%
  $todayDate$
%
  group left shift=0,
  group right shift=0,
  group height=.5,
  group top shift = 0.2,
  group right peak height = 0.2,
  group left peak height = 0.2,
  group peaks tip position=0,
  group label node/.append style={align=left, text width=9em},
  group/.append style={draw=none, fill=black!50},
%
  bar label font=\\mdseries\\small\\color{black},
  bar label node/.append style={left=-1em, align=left, text width=9em},
  bar/.append style={draw=none, fill=black},
%
  link/.append style={[-tothick, blue},
$endif$]{$start$}{$end$}
$if(start)$
$calendar$
$endif$
$body$
\\end{ganttchart}
\\end{document}
