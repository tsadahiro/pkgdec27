permutation_from_word=function(word,k){
  n = max(word)+1
  return(1:n)
}


#' generate the permutation matrix from the reduced word
#'
#' return the signed permutation matrix corresponding to the
#' word
#'
#' @param word reduced word
#' @param k position
#' @return signed permutation matrix
#' @export
permutation_matrix=function(word, k){
  n = max(word)+1
  if (k > length(word)){
    return(diag(-1,n))
  }
  if (k<1){
    return(diag(1,n))
  }
  perm = permutation_from_word(word, k)
  mat = matrix(0, n, n)
  return(mat)
}
