library(rpart)
library(partykit)
library(rCharts)
library(RJSONIO)
library(rlist)
library(pipeR)
#set up a little rpart as an example
rp <- rpart(
  hp ~ cyl + disp + mpg + drat + wt + qsec + vs + am + gear + carb,
  method = "anova",
  data = mtcars,
  control = rpart.control(minsplit = 4)
)

str(rp)

rpk <- as.party(rp)

## changed pattern from [1-9] to [0-9] because we were missing node 10 
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

# binding the node names from rpk with more of the relevant meta data from rp
# i don't think that partykit imports this automatically for the inner nodes, so i did it manually
rpk.text <- cbind(rpk.text, rp$frame)

# rounding the mean DV value
rpk.text$yval <- round(rpk.text$yval, 2)

# terminal nodes have descriptive stats in their names, so I stripped these out
# so the final plot wouldn't have duplicate data
rpk.text$description <- sapply(strsplit(rpk.text[,2], ":"), "[", 1)
                                                      
#set up rCharts
#key is to define how to handle the data
rChartsRpart <- setRefClass(
  "rChartsRpart",
  contains = "Dimple",
  methods = list(
    initialize = function(){
      callSuper();
    },
    getPayload = function (chartId) {
      data = jsonlite::toJSON(
        rapply(params$data$node,unclass,how="replace")
        ,auto_unbox = T
      )
      data = gsub( x=data, pattern = "kids", replacement="children")
      data = gsub ( x=data, pattern = '"id":([0-9]*)', replacement = '"name":"node\\1"' )
      # calling the root node by the dataset name, but it might make more sense to call it
      # "root" so that the code can be generalized
      data = sub (x = data, pattern = "node1", replacement = "mtcars")
      # replacing the node names from node1, node2, etc., with the extracted node names and metadata from
      # rpk.text, and rp$table. 
      for (i in 2:nrow(rpk.text)) {
        data = sub (x = data, pattern = paste("node", i, sep = ""), 
        replacement = paste(rpk.text[i,2], ", mean = ", rpk.text[i,7], ", n = ", rpk.text[i,4], sep = ""), fixed = T)}
      chart = toChain(params$chart, "myChart")
      controls_json = toJSON(params$controls)
      controls = setNames(params$controls, NULL)
      opts = toJSON2(params[!(names(params) %in% c("data", "chart",
                                                   "controls"))])
      list(opts = opts, data = data, chart = chart, chartId = chartId,
           controls = controls, controls_json = controls_json)
    }
  )
)

rm(rpRc)
rpRc <- rChartsRpart$new()
rpRc$setLib(".") 
#rpRc$setLib("http://timelyportfolio.github.io/rCharts_rpart")
rpRc$lib = "rpart_tree"
rpRc$LIB$name = "rpart_tree"
#rpRc$setTemplate(
#  chartDiv = "<{{container}} id = '{{ chartId }}' class = '{{ lib }}' style = 'height:100%;width:100%;'></{{ container}}>"
#)
rpRc$set(
  data = rpk
  , height = 400
  , width = 400
  , nodeHeight = 100
  , maxLabelLength = 10
)
rpRc