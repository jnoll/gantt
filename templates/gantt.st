\\documentclass[12pt,pgf]{standalone}
\\usepackage{pgfgantt}
\\usepackage[T1]{fontenc}
\\renewcommand*\\familydefault{\\sfdefault} % set base font to sans serif.
\\usepackage{$if(font)$$font$$else$lmodern$endif$}
\\begin{document}
\\begin{ganttchart}[$if(chartoptions)$$chartoptions$$else$
  hgrid, 
  vgrid={*2{green, dashed},*1{blue, solid}},
  y unit chart=\\baselineskip,
  y unit title=\\baselineskip,
  title height=1,
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
  milestone height=1,
  milestone top shift=0,
  milestone label node/.append style={left=-.5em, align=left, text width=9em},
%
%  include title in canvas=false,
%  compress calendar
$endif$]{1}{$dur$}
$if(start)$
\\gantttitlecalendar*[time slot format=isodate-yearmonth, compress calendar]{$start$}{$end$}{year} 	\\\\
\\gantttitlecalendar*[time slot format=isodate-yearmonth, compress calendar, title label font=\\tiny]{$start$}{$end$}{month=shortname} 	\\\\
\\gantttitlelist{1,...,$dur$}{1}	\\\\
$endif$
$body$
\\end{ganttchart}
\\end{document}
