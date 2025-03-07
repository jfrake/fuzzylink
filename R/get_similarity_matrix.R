#' Create matrix of embedding similarities
#'
#' @description
#' Create a matrix of pairwise similarities between each string in `strings_A` and `strings_B`.
#'
#'
#' @param embeddings A matrix of text embeddings
#' @param strings_A A string vector
#' @param strings_B A string vector
#'
#' @return A matrix of cosine similarities between the embeddings of strings_A and the embeddings of strings_B
#' @export
#'
#' @examples
#' embeddings <- get_embeddings(c('UPS', 'USPS', 'Postal Service'))
#' get_similarity_matrix(embeddings)
#' get_similarity_matrix(embeddings, 'Postal Service')
#' get_similarity_matrix(embeddings, 'Postal Service', c('UPS', 'USPS'))
get_similarity_matrix <- function(embeddings, strings_A = NULL, strings_B = NULL) {
  # Use default strings if not provided
  if(is.null(strings_A)) {
    strings_A <- rownames(embeddings)
  }
  if(is.null(strings_B)) {
    strings_B <- rownames(embeddings)
  }
  
  # Direct indexing instead of string lookups (much faster)
  idx_A <- match(strings_A, rownames(embeddings))
  idx_B <- match(strings_B, rownames(embeddings))
  
  # Extract matrix rows by index rather than by name
  A <- embeddings[idx_A, , drop = FALSE]
  B <- embeddings[idx_B, , drop = FALSE]
  
  # Keep the fast Rfast implementation
  sim <- Rfast::Tcrossprod(A, B)
  
  # Set rownames and colnames after computation
  rownames(sim) <- strings_A
  colnames(sim) <- strings_B
  
  return(sim)
}
