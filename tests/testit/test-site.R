library(testit)

assert("suppress_output_message works as expected", {
  catch = function(expr) tryCatch(expr, message = function(e) e$message)
  (catch(suppress_output_message(message("a"))) %==% "a\n")
  (catch(suppress_output_message(message("\nOutput created: "))) %==% NULL)
  (catch(suppress_output_message(message("a"), "^a")) %==% NULL)
})
