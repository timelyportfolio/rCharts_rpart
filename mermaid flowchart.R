# use mermaid.js to explain R rpart and other clustering
# mermaid.js is a markdown-like language for flowcharts
# http://github.com/knsv/mermaid

library(htmltools)
library(pipeR)


# first we'll do the simple example offered in the Readme.md
# note: this is an exact copy/paste

tagList(
  tags$div( id = "mermaidChart", class = "mermaid"
,"graph LR;
  A[Hard edge]-->|Link text|B(Round edge);
  B-->C{Decision};
  C-->|One|D[Result one];
  C-->|Two|E[Result two];
"
  )
  ,tags$script(
    
  )
) %>>%
attachDependencies(
  htmlDependency(
    name = "mermaid"
    ,version = "0.2.1"
    ,src = c("href"="http://www.sveido.com/mermaid/dist/")
    ,script = "mermaid.full.min.js"
  )
) %>>%
html_print