

getNeighbors = function(node, edges, Q) {
  r = c()
  for(i in 1:nrow(edges)) {
    if(edges[i,1] == node && Q[edges[i,2]]){
      r = c(r, edges[i,2])
    }
    else if(edges[i,2] == node && Q[edges[i,1]]){
      r = c(r, edges[i,1])
    }
  }
  return(r)
}

getMin = function(Q, dist){
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
  return(index)
}

nextStep = function(start, goal, prev) {
  if(start == goal) {
    return(0)
  }
  curr = goal
  n = prev[goal]
  if(is.nan(n)){
    return(0)
  }
  while(n != start) {
    curr = n
    n = prev[curr]
  }
  return(curr)
}

dijkstra = function(start, goal, edges) {
  len = 40
  Q = rep(TRUE, len)
  dist = rep(Inf, len)
  prev = rep(NaN, len)
  dist[start] = 0
  while(is.nan(prev[goal]) && is.element(TRUE, Q)) {
    u = getMin(Q, dist)
    Q[u] = FALSE
    for(v in getNeighbors(u,edges, Q)){
      alt = dist[u] + 1
      if(alt < dist[v]) {
        dist[v] = alt
        prev[v] = u
      }
    }
  }
  return(prev)
}

normalizeMatrix = function(A) {
  for(i in 1:nrow(A)) {
    rowSum = sum(A[i,])
    if(rowSum != 0) {
      A[i,] = A[i,] / rowSum
    }
  }
  return(A)
}

getEmissionMatrix = function(reading, probs) {
  A = matrix(1,40,2)
  for(i in 1:nrow(probs)){
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


getPrevState = function(moveInfo, len) {
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
    e <<- rep(0,40)
    e[abs(positions[1])] = 1
    print("tourist 1 killed")
  }
  else if(!is.na(positions[2]) && positions[2] < 0) {
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
  k <<- k / sum(k)
  
  newState <<- t * e;
  goal = which.max(newState)
  start = positions[3]
  path <<- dijkstra(start, goal, edges)
  nextMove = nextStep(start, goal, path)
  nextMove2 = nextStep(nextMove, goal, path)
  if(nextMove == 0){
    newState[positions[3]] = 0
  }
  else if(nextMove2 == 0) {
    newState[nextMove] = 0
  }
  
  if(!is.na(positions[1]) && positions[1] > 0){
    newState[positions[1]] = 0
  }
  if(!is.na(positions[2]) && positions[2] > 0){
    newState[positions[2]] = 0
  }
  
  newState <<- newState / sum(newState)

  moveInfo$moves = c(nextMove, nextMove2)
  moveInfo$mem$prev = newState
 
  return(moveInfo)
}

run = function(noi){
  result <<- rep(0, noi)
  for(i in 1:noi) {
    print(i)
    result[i] <<- runWheresCroc(makeMoves, showCroc = F, pause = 0)
  }
  
  sprintf("Avarage time: %f, Median time: %f", mean(result), median(result))
}




