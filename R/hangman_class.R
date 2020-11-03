Hangman <- R6::R6Class(
  "Hangman",
  public = list(
    initialize = function(word_list = hangman_word_list) {
      if (!identical(word_list, hangman_word_list)) {
        word_list      <- gsub("[^a-z]", "", tolower(word_list))
        self$word_list <- word_list[nchar(word_list) != 0]
      }

      if (!length(self$word_list)) {self$word_list <- hangman_word_list}

      private$refresh()
    },

    print = function() {
      if (!interactive()) {return(invisible(NULL))}

      cat("\014")
      cat(crayon::silver('Type "quit" to exit. Type "help" for commands.\n'))

      switch(
        private$n_wrong,
        `1` = {private$body[2] <- private$head},
        `2` = {private$body[3] <- private$torso},
        `3` = {private$body[3] <- private$one_arm},
        `4` = {private$body[3] <- private$two_arm},
        `5` = {private$body[4] <- private$one_leg},
        `6` = {private$body[4] <- private$two_leg}
      )

      if (private$n_wrong > 0) {
        private$board[1] <- paste(
          c("\u250c", rep("\u2500", private$n_wrong), "\u2510"), collapse = ""
        )
        private$board[2] <- paste(
          c("\u2502", private$wrong, "\u2502"), collapse = ""
        )
        private$board[3] <- paste(
          c("\u2514", rep("\u2500", private$n_wrong), "\u2518"), collapse = ""
        )
      }

      private$board[5] <- paste(
        rep("\u203e", length(private$word)), collapse = ""
      )

      display <- private$word
      display[!private$word %in% private$guesses] <- " "
      private$board[4] <- paste(display, collapse = "")

      cat(
        paste(
          paste0(private$gallows, private$body, private$board),
          collapse = "\n"
        )
      )

      cat("\n")

      if (private$surrendered) {
        cat("You surrendered.")
        private$ask_restart()
      } else if (private$n_wrong >= 6) {
        cat(paste0('The word was "', paste(private$word, collapse = ''), '".'))
        private$ask_restart()
      } else if (any(!private$word %in% private$guesses)) {
        private$guess()
      } else {
        cat("You win!")
        private$ask_restart()
      }
    },

    word_list = NULL,
    game_over = FALSE
  ),
  private = list(
    word        = NULL,
    letters     = NULL,
    guesses     = NULL,
    wrong       = NULL,
    n_wrong     = 0,
    body        = NULL,
    surrendered = FALSE,

    guess = function() {
      cat(crayon::silver("Guess a letter."))
      letter <- gsub(" ", "", tolower(readline("> ")))

      if (grepl("help", letter)) {
        cat(
          'Type "restart" for a new word,',
          '"surrender" to reveal the current word,',
          'or "quit" to exit.',
          sep = "\n"
        )
        return(private$guess())
      }

      if (grepl("restart", letter)) {return(private$refresh())}
      if (grepl("quit", letter))    {quit_game()}

      if (grepl("surrender", letter)) {
        private$surrendered <- TRUE
        private$guesses     <- c(private$guesses, private$letters)
        return(self$print())
      }

      if (nchar(letter) > 1 || grepl("[^a-z]", letter)) {
        cat(crayon::silver("I couldn't understand that input. "))
        return(private$guess())
      }

      private$guesses <- unique(c(private$guesses, letter))
      private$wrong   <- private$guesses[!private$guesses %in% private$letters]
      private$n_wrong <- length(private$wrong)

      return(self$print())
    },

    refresh = function() {
      private$word        <- strsplit(sample(self$word_list, 1), "")[[1]]
      private$letters     <- unique(private$word)
      private$body        <- private$air
      private$board       <- rep("", 5)
      private$guesses     <- NULL
      private$wrong       <- NULL
      private$n_wrong     <- 0
      private$surrendered <- FALSE
      self$game_over      <- FALSE

      self$print()
    },

    ask_restart = function() {
      self$game_over <- TRUE

      cat(' Press ENTER to try again or type "quit" to exit.')

      if (grepl("quit", tolower(readline("> ")))) {
        cat("\014")
      } else {
        private$refresh()
      }
    },

    gallows = c(
      "\u250c\u2500\u2500\u2510",
      "\u2502  ",
      "\u2502 ",
      "\u2502 ",
      "\u2534"
    ),

    air = c(
      "  ",
      "   ",
      "    ",
      "    ",
      "     "
    ),

    board = rep("", 5),

    head    = "O  ",
    torso   = " |  ",
    one_arm = " |\\ ",
    two_arm = "/|\\ ",
    one_leg = "  \\ ",
    two_leg = "/ \\ "
  )
)
