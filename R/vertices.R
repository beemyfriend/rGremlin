#'Ask the gremlin about it's nodes
#'
#'Find out which nodes connect with which
#'@param gremlin a gremlin object
#'@param mode 'in', 'out', 'all', 'total'
#'@export
queryV <- function(gremlin, mode){
  if(gremlin$last == 'vertices'){
    results <- neighbors(gremlin$graph, gremlin$vertices, mode)
    query <- str_c(mode, 'V')
  } else if(gremlin$last == 'edges'){
    if(mode == 'out'){
      results <- head_of(gremlin$graph, gremlin$edges)
    } else if(mode == 'in'){
      results <- tail_of(gremlin$graph, gremlin$edges)
    }
    query <- str_c(mode, 'E')
  }
  results <- unique(results)
  gremlin <- query_record(gremlin, query, results)
  gremlin$vertices <- results
  gremlin$last <- 'vertices'
  gremlin
}

#'Wrapper for queryV
#'
#'from gremlins nodes to other nodes
#'@export
outV <- function(gremlin){
  queryV(gremlin, 'out')
}

#'Wrapper for queryV
#'
#'from other nodes to gremlins nodes
#'@export
inV <- function(gremlin){
  queryV(gremlin, 'in')
}
