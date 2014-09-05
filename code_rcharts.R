library(rpart)
library(partykit)
library(rCharts)


#set up a little rpart as an example
rp <- rpart(
  hp ~ cyl + disp + mpg + drat + wt + qsec + vs + am + gear + carb,
  method = "anova",
  data = mtcars,
  control = rpart.control(minsplit = 4)
)

#convert it to partykit rpart so we can use structure
rpk <- as.party(rp)


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
      data =  jsonlite::toJSON(
        rapply(params$data$node,unclass,how="replace")
        ,auto_unbox = T
      )
      data = gsub( x=data, pattern = "kids", replacement="children") 
      data = gsub ( x=data, pattern = '"id":([0-9]*)', replacement = '"name":"node\\1"' ) 
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


# now make a rChart with our rpart

rpRc <- rChartsRpart$new()
rpRc$setLib("http://timelyportfolio.github.io/rCharts_rpart")
rpRc$lib = "rpart_tree"
rpRc$LIB$name = "rpart_tree"
rpRc$setTemplate(
  chartDiv = "<{{container}} id = '{{ chartId }}' class = '{{ lib }}' style = 'height:100%;width:100%;'></{{ container}}>"
)

rpRc$set(
  data = rpk
  , height = 800
  , width = 800
)
rpRc$show()
