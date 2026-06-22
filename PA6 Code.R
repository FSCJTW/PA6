# Programming Assignment 6
# T. Wallace
# 06/21/2026
# Custom Function with For Loop

# Custom function that squares each number and adds 5
# Includes vectorized operations and error handling for non-numeric input
square_and_add_five <- function(input_vector) {
  
  # Check if input contains non-numeric values
  if (!is.numeric(input_vector)) {
    return(rep(NA, length(input_vector)))
  }
  
  # Attempt the vectorized operation (wrapped in try for safety)
  temp <- try((input_vector^2 + 5), silent = TRUE)
  
  # Handle any unexpected error
  if (class(temp) == "try-error") {
    return(rep(NA, length(input_vector)))
  } else {
    return(temp)
  }
}

# 
# Test Section
# 

# Test 1: Using sapply on multiple numeric vectors
test_list <- list(
  c(2, 5, 8),
  c(1, 3, 7, 10),
  c(4, 6)
)

results1 <- sapply(test_list, square_and_add_five)
print(results1)

# Test 2: Mixed vector with a string (tests error handling)
mixed_vector <- c(3, 8, "hello", 12, 7)
results2 <- square_and_add_five(mixed_vector)
print(results2)