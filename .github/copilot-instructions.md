# Repository Instructions for Copilot

## Build and test the package

- Build the R package:

   ```bash
   R CMD build .
   ```

   - Creates a `.tar.gz` package file

- Install the package:

   ```bash
   R CMD INSTALL *_*.tar.gz
   ```

- Run tests:

   ```bash
   R CMD check --no-manual *_*.tar.gz
   ```

   or directly:

   ```bash
   Rscript tests/*.R
   ```

   - All tests should pass without errors


## R Code Style

- **Indentation**: Use 2 spaces (not 4 spaces or tabs)
- **Compact code**: Avoid `{}` for single-expression if statements; prefer compact forms when possible
- **Roxygen documentation**: Don't use `@description` or `@details` explicitly - just write the description text directly after the title
- **Examples**: Avoid `\dontrun{}` unless absolutely necessary (e.g., requires external resources, takes very long time, or has side effects that could harm user's system). Prefer runnable examples that can be tested automatically.
- **Function definitions**: For functions with many arguments, break the line right after the opening `(`, indent arguments by 2 spaces, and try to wrap them at 80-char width, e.g.:
   ```r
   f = function(
     arg1, arg2, ...,
     argN, ...
   ) {
     ...
   }
   ```
- **Re-wrap code**: Always re-wrap the code after making changes to maintain consistent formatting and line length.

## Build and Package Conventions

- **Always re-roxygenize**: Run `Rd2roxygen::rab('.')` after changing any roxygen documentation.
- **R CMD check before EVERY commit**: You must run `R CMD check` successfully before submitting any code changes. If R or required R packages are not available, you must install them first.
- **Wait for CI to be green**: After pushing code, you must wait for GitHub Actions CI to complete successfully before claiming the task is done. Never quit while CI is running or failing. You may check CI every 30 seconds and start fixing issues as soon as any CI job has failed instead of waiting for all CI jobs to finish.
- **NEVER BREAK CI**: Breaking CI is completely unacceptable. If CI fails, you must immediately fix it. This policy must be followed strictly for ALL changes without exception.
- **Bump version in PRs**: Bump the patch version number in DESCRIPTION once per PR (on the first commit or when you first make changes), not on every commit to the PR.
