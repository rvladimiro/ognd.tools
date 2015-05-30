#' @author Ricardo Vladimiro
#' @title random_strings
#' @name random_strings
#' @description Generates random strings
#' @details The random string generation will generate a vector of n random strings, created by
#' sampling from upper and lower letters. If include_numbers is true, then numbers will also be
#' used to build the random strings. Random strings are created taking into consideration a
#' random seed if it is set by set.seed() function.
#' @param n The number of random strings
#' @param length The length of each individual string
#' @param include_numbers If true, numbers are used to create the string
#' @return A vector of n random strings of length length
#' @seealso set.seed()
#' @export
random_strings = function(n = NULL, length = 8, include_numbers = T) {
    
    # Check if inputs are correct
    if(!is.numeric(n)) stop('n must be integer.')
    if(n != as.integer(n)) stop('n must be integer.')
    if(n < 1) stop('n must be 1 or bigger.')
    if(!is.numeric(length)) stop('length must be integer.')
    if(length != as.integer(length)) stop('length must be integer.')
    if(length < 1) stop('length must be 1 or bigger.')
    if(!is.logical(include_numbers)) stop('include_numbers must be logical.')
    
    
    # Create the pool to get random characters from
    # This pool has upper and lower letters
    random_pool = c(LETTERS, letters)
    
    # If the random strings should include numbers concatenate them to the pool
    if(include_numbers){
        random_pool = c(random_pool, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0')
    }
    
    # Create n random strings with the given length
    # This is achieved through sampling with replacement
    sapply(
        X = 1:n,
        FUN = function(X){
            paste(sample(x = random_pool, size = length, replace = T), collapse = '')
        }
    )
    
}
