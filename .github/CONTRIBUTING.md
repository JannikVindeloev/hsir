# Contributing to hsir

We welcome contributions to the hsir package! Please follow these guidelines to ensure a smooth collaboration process.

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## How to Contribute

### Reporting Bugs

- **Check existing issues**: Before creating a new issue, please check if the bug has already been reported.
- **Reproducible example**: Include a minimal reproducible example that demonstrates the problem.
- **Session information**: Include the output of `sessionInfo()` and `devtools::session_info()`.
- **Expected vs actual behavior**: Clearly describe what you expected to happen and what actually happened.

### Suggesting Enhancements

- **Use cases**: Describe the use case that would benefit from the enhancement.
- **Implementation ideas**: If you have ideas for how to implement the enhancement, please include them.
- **Alternatives**: Consider whether the enhancement could be implemented in a different way.

### Pull Requests

1. **Fork the repository**: Create your own fork of the repository.
2. **Create a feature branch**: Use a descriptive branch name (e.g., `feature/optimize-matrix-ops`).
3. **Follow coding standards**:
   - Use tidyverse style guide
   - Include roxygen2 documentation for all functions
   - Add tests for new functionality
   - Keep functions lightweight and memory efficient
4. **Update documentation**: Ensure all documentation is up to date.
5. **Pass all tests**: Make sure all existing tests pass and add new tests as needed.
6. **Submit the PR**: Create a pull request with a clear description of the changes.

## Development Setup

```r
# Install development dependencies
devtools::install_deps(dependencies = TRUE)

# Install the package in development mode
devtools::document()
devtools::install()

# Run tests
devtools::test()

# Check the package
devtools::check()
```

## Coding Standards

- **Function naming**: Use snake_case for function names
- **Variable naming**: Use snake_case for variable names
- **Documentation**: Use roxygen2 for all functions
- **Imports**: Use `@importFrom` for specific functions rather than whole packages
- **Performance**: Always consider memory usage and speed
- **Error handling**: Include meaningful error messages

## Testing

- All functions should have corresponding tests
- Use testthat for testing
- Aim for high test coverage
- Include edge cases in tests

## Documentation

- All exported functions must have roxygen2 documentation
- Include examples in documentation
- Use markdown for README and vignettes
- Keep documentation up to date with code changes

## Performance Guidelines

- Use data.table for data frame operations when possible
- Use matrixStats for matrix operations
- Avoid unnecessary copying of data
- Use vectorized operations when possible
- Profile code to identify bottlenecks

## License

By contributing to this project, you agree to license your contributions under the same proprietary license as the project.
