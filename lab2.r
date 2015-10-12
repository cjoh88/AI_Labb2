
#dijkstra = function(start, goal, edges) {
  #create vertex set Q
  #for each vertex v in graph
    #dist[v] <- infinity
    #prev[v] <- undefined
    #add v to Q
  #dist[start] <- 0
  #while Q not empty
    #u <- vertex in Q with min dist[u]
    #remove u from Q
    #for each neighbor v of u
      #alt <- dist[u] + length(u, v)
      #if alt < dist[u]
        #dist[v] <- alt
        #prev[v] <- u
  #return dist[], prev[]
#}

getNeighbors = function(node, edges, Q) {
  #print("Entering getNeighbors")
  r = c()
  for(i in 1:nrow(edges)) {
    if(edges[i,1] == node && Q[edges[i,2]]){
      r = c(r, edges[i,2])
    }
    else if(edges[i,2] == node && Q[edges[i,1]]){
      r = c(r, edges[i,1])
    }
  }
  #print(r)
  return(r)
}

getMin = function(Q, dist){
  #print("Entering getMin")
  min = Inf
  index = 0
  for(i in 1:length(Q)) {
    if(Q[i] == TRUE){
      if(dist[i] <= min){
        min = dist[i]
        index = i
      }
    }
  }
  QQ <<- Q
  DIST <<- dist
  MIN <<- min
  INDEX <<- index
  #print(index)
  return(index)
}

nextStep = function(start, goal, prev) {
  #print("Entering nextStep")
  if(start == goal) {
    return(0)
  }
  curr = goal
  n = prev[goal]
  #print(curr)
  #print(n)
  #print(start)
  #Bad solution?
  if(is.nan(n)){
    return(0)
  }
  #stop()
  while(n != start) {
    curr = n
    n = prev[curr]
  }
  return(curr)
}

# nextStep = function(start, goal, prev) {
#   curr = goal
#   n = prev[goal]
#   if(is.nan(n)){
#     return(c(0,0))
#   }
#   nn = prev[n]
#   if(is.nan(nn)){
#     return(c(0,0))  #check this
#   }
#   while(nn != start || n != start) {
#     curr = n
#     n = nn
#     #print("n")
#     #print(n)
#     nn = prev[n]
#     if(is.nan(nn)){
#       return(c(n,0))
#     }
#     #print("nn")
#     #print(nn)
#   }
#   return(c(n, nn))
# }  

dijkstra = function(start, goal, edges) {
  #print("Entering dijkstra")
  #len = nrow(edges)
  len = 40
  #create vertex set Q
  Q = rep(TRUE, len)
  #for each vertex v in graph
    #dist[v] <- infinity
    #prev[v] <- undefined
    #add v to Q
  dist = rep(Inf, len)
  prev = rep(NaN, len)
  #dist[start] <- 0
  dist[start] = 0
  #while Q not empty
  while(is.nan(prev[goal]) && is.element(TRUE, Q)) {
    #u <- vertex in Q with min dist[u]
    u = getMin(Q, dist)
    #remove u from Q
    Q[u] = FALSE
    #for each neighbor v of u
    for(v in getNeighbors(u,edges, Q)){
      #alt <- dist[u] + length(u, v)
      alt = dist[u] + 1
      #if alt < dist[v]
      if(alt < dist[v]) {
        #dist[v] <- alt
        dist[v] = alt
        prev[v] = u
        #prev[v] <- u
      }
    }
  }
  #p <<- prev
  #print(prev)
  #stop()
  #return dist[], prev[]
  return(prev)
}

normalizeMatrix = function(A) {
  #print("Entering normalizeMatrix")
  for(i in 1:nrow(A)) {
    rowSum = sum(A[i,])
    if(rowSum != 0) {
      A[i,] = A[i,] / rowSum
    }
  }
  return(A)
}

getEmissionMatrix = function(reading, probs) {
  #print("Entering getEmissionMatrix")
  A = matrix(1,40,2)
  for(i in 1:nrow(probs)){
    #if(reading > probs[i,1] - probs[i,2] && reading < probs[i,1] + probs[i,2]) {
    #  A[i,1] = 19
    #}
    if(reading > probs[i,1] - 2*probs[i,2] && reading < probs[i,1] + 2*probs[i,2]) {
      A[i,1] = 19
    }
    else {
      A[i,2] = 19
    }
  }
  A = normalizeMatrix(A)
  return(A)
}

getTransitionMatrix = function(edges) {
  #print("Entering getTransitionMatrix")
  A = matrix(0,40,40)
  for(i in 1:nrow(edges)) {
    A[edges[i,1], edges[i,2]] = 1
    A[edges[i,2], edges[i,1]] = 1
  }
  for(i in 1:nrow(A)){
    A[i,i] = 1
  }
  A = normalizeMatrix(A)
  return(A)
}

#only works for 40 nodes
getPrevState = function(moveInfo, len) {
  #print("Entering getPrevState")
  #if(moveInfo$mem == NULL) {
  if(is.null(moveInfo$mem$prev)) {
    v = rep(1,len) / len
  }
  else {
    v = moveInfo$mem$prev
  }
  return(v)
}

#probs$salinity, probs$phosphate, probs$nitrogen
makeMoves = function(moveInfo, readings, positions, edges, probs) {
  #if(!is.na(positions[1])){
  if(!is.na(positions[1]) && positions[1] < 0){
    #A <<- abs(positions[1])
    e <<- rep(0,40)
    e[abs(positions[1])] = 1
    print("tourist 1 killed")
  }
  else if(!is.na(positions[2]) && positions[2] < 0) {
    #goal = abs(positions[2])
    e <<- rep(0,40)
    e[abs(positions[2])] = 1
    print("tourist 2 killed")
  }
  else {
  #salinity
  E1 <<- getEmissionMatrix(readings[1], probs$salinity)
  #phosphate
  E2 <<- getEmissionMatrix(readings[2], probs$phosphate)
  #nitrogen
  E3 <<- getEmissionMatrix(readings[3], probs$nitrogen)
  e <<- E1[,1] * E2[,1] * E3[,1]
  e <<- e / sum(e)
  }
  A <<- getTransitionMatrix(edges)

  prevState <<- getPrevState(moveInfo, nrow(A))
  
  t = rep(0, nrow(A))
  
  for(i in 1:nrow(A)) {
    for(j in 1:nrow(A)) {
      t[i] = t[i] + (prevState[j] * A[i,j]) 
    }
  }
  k <<- t
  
  #e = rep(0, nrow(A))
  #e <<- e / sum(e)
  
  newState <<- t * e;
  #newState <<- newState / sum(newState)
  #moveInfo$mem$prev = newState
  #moveInfo$moves = c(0,0)
  goal = which.max(newState)
  start = positions[3]
  #print(goal)
  path <<- dijkstra(start, goal, edges)
  nextMove = nextStep(start, goal, path)
  nextMove2 = nextStep(nextMove, goal, path)
  if(nextMove == 0){
    newState[positions[3]] = 0
    #newState <<- newState / sum(newState)
  }
  else if(nextMove2 == 0) {
    newState[nextMove] = 0
    #newState <<- newState / sum(newState)
  }
  
  if(!is.na(positions[1]) && positions[1] > 0){
    newState[positions[1]] = 0
  }
  if(!is.na(positions[2]) && positions[2] > 0){
    newState[positions[2]] = 0
  }
  
  newState <<- newState / sum(newState)
  

  #print(nextMove2)
  
  #moveInfo$moves = c(nextStep(start, goal, path),0)
  moveInfo$moves = c(nextMove, nextMove2)
  moveInfo$mem$prev = newState
  
  #print(sum(newState))
  #print(which.max(newState))
  #print(max(newState))
  
  return(moveInfo)
}

run = function(noi){
  #runWheresCroc(makeMoves, showCroc = T)
  result <<- rep(0, noi)
  for(i in 1:noi) {
    #sprintf("Iteration: %d", i)
    print(i)
    #runWheresCroc(makeMoves, showCroc = T, pause = 1)
    result[i] <<- runWheresCroc(makeMoves, showCroc = F, pause = 0)
  }
  
  #lines(result)
  sprintf("Avarage time: %f, Median time: %f", mean(result), median(result))
  #sprintf("Median time to find Croc is %f turns", median(result))
}




