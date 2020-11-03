hangman_env <- new.env()

#' Play a game of hangman in the console
#'
#' @param word_list A character vector of words used to play the hangman game.
#'   Defaults to a list of 10,000 common English words with four or
#'   more letters.
#'
#' @source The default word list is derived from the
#' [Internet corpus](http://corpus.leeds.ac.uk/list.html) created by the
#' [Centre for Translation Studies, University of Leeds](https://ahc.leeds.ac.uk/centre-for-translation-studies),
#' distributed under the
#' [Creative Commons Attribution 2.5 Generic](https://creativecommons.org/licenses/by/2.5/)
#' license.
#'
#' This word list is passed through the
#' [Offensive/Profane Word List](https://www.cs.cmu.edu/~biglou/resources/)
#' created by [Luis Von Ahn](https://www.cs.cmu.edu/~biglou/) to filter out
#' potentially unwanted words.
#'
#' @return
#' @aliases hangman
#' @export
#' @examples
#'
#' play_hangman()
#'
#' play_hangman("test")
#'
#' cars <- unique(gsub(" .*", "", rownames(mtcars)))
#' play_hangman(cars)

play_hangman <- function(word_list = hangman_word_list) {
  hangman_env$game <- Hangman$new(word_list)
}

quit_game <- function() {
  do.call(return, list(cat("\014")), envir = sys.frame(-1))
}
