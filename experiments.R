library(shiny)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(partykit)

rp <- rpart(
  hp ~ cyl + disp + mpg + drat + wt + qsec + vs + am + gear + carb,
  method = "anova",
  data = mtcars,
  control = rpart.control(minsplit = 4)
)

fancyRpartPlot(rp)


library(SVGAnnotation)
library(pipeR)
library(htmltools)

svgPlot(fancyRpartPlot(rp)) %>>% saveXML %>>% HTML %>>% html_print()


rpart:::summary.rpart(rp)


rsq.rpart(rp)


rpk <- as.party(rp)
plot(rpk)
nodeids(rpk)
labels.rpart(rp)
path.rpart(rp,1)
nodeids(rpk,1)

rpk[[1]]


nodeapply(rpk,1,unclass)[[1]]$kids


rapply(nodeapply(rpk,1)[[1]]$kids,unclass,how="list") -> l

list.filter(l, ll->{ names(ll)[1] == "id" }) %>>% list.group( "kids" )

rapply(nodeapply(rpk,1)[[1]]$kids,unclass,how="replace") -> r.rep
list.map(r.rep, .$id)
list.filter(r.rep, rr->{ names(rr)[1] == "id" })


rpNet <- function(l){
  list.map(l, .$id)
}

nodeapply(
  rpk
  ,1:length(rpk)
  ,function(z){rpNet(unclass(z)$kids)}
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
  list.stack


rp.text <- capture.output(print(rp))
gsub(x=rp.text,pattern="([1-9]*)(\\))(.*)",replacement="\\1,\\3")

rpk.text <- capture.output( print(rpk) ) %>>%
  ( .[grep( x = ., pattern = "(\\[)([1-9]*)(\\])")] ) %>>%
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