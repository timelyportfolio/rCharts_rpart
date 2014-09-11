library(htmltools)
library(partykit)
library(rpart)
library(pipeR)
library(rlist)
library(whisker)


#key is to define how to handle the data
#for the parallel coordinates
#expect the data = list ( partykit object, original data )
rpart_Parcoords <- function( pk = NULL, data = NULL  ){
  # transform our data in the way we would like
  # to send it to JSON and plug into our template
  # since this will be parallel coordinates
  # data should be in the records form (jsonlite default)
  # 1) combine the fitted data from partykit with the original data  
  #    which allow us to see which groups each row belongs
  #    we'll try to be smart and sort column order by the order of splits
  colorder = rapply(pk,unclass,how="unlist") %>>%  #unclass and unlist the partykit
    list.match("varid") %>>%                        #get all varids
    unique %>>% unlist  %>>%                        #squash into a vector
    ( names(attr(pk$terms,"dataClasses"))[c(1,.)] ) %>>%  #match them with column names
    ( unique( c(.,colnames(data) ) ) )              #get other column names from data
  # get the column name of the varids from above
  data = jsonlite::toJSON( cbind( pk$fitted, data[colorder] ) )
  
  t <- tagList(
    
    tags$div( id = "par_container", class = "parcoords", style = "height:400px;width:100%;" )
    
    ,tags$script(
      whisker.render( readLines("./layouts/chart_parcoords.html") ) %>>% HTML
      #whisker.render( readLines("http://timelyportfolio.github.io/rCharts_rpart/layouts/chart_parcoords.html") ) %>>% HTML
    )
    
    
  ) %>>%
  attachDependencies(list(
    htmlDependency(
      name="d3"
      ,version="3.0"
      ,src=c("href"="http://d3js.org/")
      ,script="d3.v3.js"
    )
    ,htmlDependency(
      name="pc"
      ,version="0.4.0"
      ,src=c("href"="http://syntagmatic.github.com/parallel-coordinates/")
      ,script="d3.parcoords.js"
      ,stylesheet="d3.parcoords.css"
    )
  ))
  
  return(t)
}

#set up a little rpart as an example
rp <- rpart(
  hp ~ cyl + disp + mpg + drat + wt + qsec + vs + am + gear + carb,
  method = "anova",
  data = mtcars,
  control = rpart.control(minsplit = 4)
)

str(rp)

rpk <- as.party(rp)

#now make it a parallel coordinates
#with our rpart_Parcoords function

rpart_Parcoords( rpk, mtcars ) %>>% html_print() -> fpath
#rCharts:::publish_.gist(fpath,description="R + d3.js Parallel Coordinates of partykit",id=NULL)
