library(shiny)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(partykit)
library(htmltools)
library(SVGAnnotation)
library(pipeR)

rp <- rpart(
  hp ~ cyl + disp + mpg + drat + wt + qsec + vs + am + gear + carb,
  method = "anova",
  data = mtcars,
  control = rpart.control(minsplit = 4)
)

fancyRpartPlot(rp)


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


rpNet <- function(n){
  l = unclass(n)$kids
  list.map(l, .$id)
}

rpNet2 <- function(l){
  list.map(l, list(id = .$id, children = lapply(unclass(.$kids),function(k){return(rpNet(k))})))
}


nodeapply(
  rpk
  ,4
  ,rpNet
) %>>% list.all( length(.) == 0)

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
  list.stack


rp.text <- capture.output(print(rp))
gsub(x=rp.text,pattern="([0-9]*)(\\))(.*)",replacement="\\1,\\3")

rpk.text <- capture.output( print(rpk) ) %>>%
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



l_levels <- list()
n = 1
level = 1
while( list.all( nodeapply(rpk, n,rpNet), length(.) > 0 ) ) {
  n = unlist(nodeapply(rpk,n,rpNet)) %>>% as.numeric
  l_levels <- list.append(l_levels,nodeapply(rpk,n,rpNet))
}



l_levels <- list( id = 1, children = nodeapply( rpk, 1, rpNet) )
lapply(
  l_levels$children
  ,function(n){
    list(children = nodeapply(rpk,unlist(n),rpNet))
  }
)


rapply(rpk$node,unclass,how="replace") %>>%
  jsonlite::toJSON( auto_unbox = T ) %>>% 
  ( gsub( x=., pattern = "kids", replacement="children")) %>>% 
  ( gsub ( x=., pattern = '"id":([0-9]*)', replacement = '"name":"node\\1"' ) )# %>>%
  #cat( file = "../8850659195485a5c5838/rp.json" )


rl <- modifyList(
  rl
  ,list.match(rl, "kids")
)


