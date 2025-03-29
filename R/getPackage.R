#' Discover Functions in a Package Using LLM
#'
#' This function uses the llama3.2:3b model through Ollama to predict
#' which functions might be exported by a given R package.
#'
#' @param package_name Character string containing the name of an R package
#' @param model_name Character string with the name of the Ollama model to use (default: "llama3.2:3b-instruct-q4_K_M")
#' @return A character vector of function names
#' @export
get_package_functions <- function(package_name,
                         model_name = "llama3.2:3b-instruct-q4_K_M") {
  # Input validation
  if (!is.character(package_name) || length(package_name) != 1) {
    stop("package_name must be a single character string")
  }

  if (!is.character(model_name) || length(model_name) != 1) {
    stop("model_name must be a single character string")
  }

  # Attempt to initialize chat with Ollama
  chat <- NULL
  tryCatch({
    chat <- ellmer::chat_ollama(model = model_name)
  }, error = function(e) {
    stop(paste("Failed to connect to Ollama server. Error:", e$message,
               "\nPlease ensure Ollama is running at the default address or specify with base_url."))
  })

  # Create the prompt

  prompt <- paste0(
    "List ONLY the exported function names of the R package '",
    package_name, "'. Return each function name on a new line. Do NOT include any other text, explanations, introductory phrases, or formatting. The output should be a plain list of function names separated by newlines. For example:\nfilter\narrange\nselect. Try to return maximum function possible"
  )


  # Get response from LLM
  response <- NULL

  # Set a timeout using options
  old <- options(timeout = 60)  # Set HTTP timeout to 60 seconds
  on.exit(options(old))  # Restore original timeout on exit

  tryCatch({
    # Use the chat connection with the timeout settings applied
    response <- chat$chat(prompt, echo = FALSE)
  }, error = function(e) {
    if (grepl("timed out", e$message, ignore.case = TRUE)) {
      stop("The request to Ollama timed out. Please check if the server is responding.")
    } else {
      stop(paste("Error communicating with Ollama:", e$message))
    }
  })

  # Process the response and ensure vector output
  if (is.null(response) || nchar(response) == 0) {
    warning("Received empty response from LLM, returning empty vector")
    return(character(0))
  }

  # Parse the response and convert it into a proper character vector
  tryCatch({
    functions <- strsplit(response, "\n")[[1]]  # Split response by newlines

    # Clean up the function names
    functions <- trimws(functions)

    # Remove empty strings
    functions <- functions[functions != ""]

    # Remove any lines that don't look like function names
    functions <- functions[grepl("^[a-zA-Z0-9._]+$", functions)]

    if (length(functions) == 0) {
      warning("No valid function names found in the response")
      return(character(0))
    }

    return(functions)  # Now properly returns a character vector
  }, error = function(e) {
    stop(paste("Failed to parse LLM response:", e$message))
  })
}
