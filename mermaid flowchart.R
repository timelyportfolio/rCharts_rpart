# use mermaid.js to explain R rpart and other clustering
# mermaid.js is a markdown-like language for flowcharts
# http://github.com/knsv/mermaid

library(htmltools)
library(pipeR)
library(rpart)
library(partykit)

# first we'll do the simple example offered in the Readme.md
# note: this is an exact copy/paste of example
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

# now let's see if we can integrate with rpart/partykit
#set up a little rpart as an example
rpk <- rpart(
  hp ~ cyl + disp + mpg + drat + wt + qsec + vs + am + gear + carb,
  method = "anova",
  data = mtcars,
  control = rpart.control(minsplit = 4)
) %>>% as.party

# get partykit in source/target
rpNet <- function(n){
  l = unclass(n)$kids
  list.map(l, .$id)
}

nodeapply(
  rpk
  ,1:length(rpk)
  ,rpNet
) %>>%
  list.search(!is.null(.)) %>>%
  (
    lapply(
      names(.),
      function(node){
        data.frame(
          "source" = node
          ,"target" = unlist(.[[node]])
        )
      }
    )
  ) %>>%
  list.stack -> rpk_sourcetarget


# get descriptions for nodes
rpk_text <- capture.output( print(rpk) ) %>>%
  ( .[grep( x = ., pattern = "(\\[)([0-9]*)(\\])")] ) %>>%
  strsplit( "[\\[\\|\\]]" , perl = T) %>>%
  list.map(
    tail(.,2) %>>%
      (
        data.frame(
          "id" = as.numeric(.[1])
          , description = .[2]
          , stringsAsFactors = F )
      )
  ) %>>% list.stack

# will have to strip characters mermaid does not like
# do separately to determine the secret reserved list
rpk_text %>>%
  gsub( x= .[,"description"], pattern = "[()]",replacement = "") -> rpk_text[,"description"]

tagList(
  tags$div( id = "mermaidChart", class = "mermaid"
    ,paste0(
  "graph LR;"
      ,paste0(
        apply(rpk_sourcetarget,MARGIN=1,function(node){
          sprintf(
            "%s[%s]-->%s[%s];"
            ,LETTERS[node[["source"]]%>>%as.numeric]
            ,rpk_text[node[["source"]]%>>%as.numeric,"description"]
            ,LETTERS[node[["target"]]%>>%as.numeric]
            ,rpk_text[node[["target"]]%>>%as.numeric,"description"]
          )
        })
        ,collapse="\n"
      ) 
    )  %>>% HTML
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
