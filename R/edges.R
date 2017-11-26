#'Consolidate list of edgelists to a single edge list
#'
#'@param listE a list of edgelists
#'@export
consolidateE <- function(listE){
  consolidated <- NULL
  sapply(listE, function(x){
    if(is.null(consolidated)){
      consolidated <<- x
    } else {
      consolidated <<- union(consolidated, x)
    }
  })
  consolidated
}

#'Ask the gremlin about it's edges
#'
#'Find out which nodes connect with edges
#'@param gremlin a gremlin object
#'@param mode 'in', 'out', 'all', this is ignored for undirected graphs
#'@export
queryE <- function(gremlin, mode){
  if(gremlin$last == 'vertices'){
    results <- incident_edges(gremlin$graph, gremlin$vertices, mode)
    query <- str_c(mode, 'E')
  }
  gremlin <- query_record(gremlin, query, results)
  gremlin$edges <- consolidateE(results)
  gremlin$last <- 'edges'
  gremlin
}

#'Wrapper for queryE
#'
#'find the head (arrow) of an edge
#'@export
outE <- function(gremlin){
  queryE(gremlin, 'out')
}

#'Wrapper for queryE
#'
#'find the tail of an edge
#'@export
inE <- function(gremlin){
  queryE(gremlin, 'in')
}