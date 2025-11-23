# Contributing to the HFT System

Thank you for your interest in contributing! This project demonstrates polyglot architecture and welcomes contributions that improve any component.

## Getting Started

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run tests (`make test`)
5. Commit your changes (`git commit -m 'Add amazing feature'`)
6. Push to the branch (`git push origin feature/amazing-feature`)
7. Open a Pull Request

## Component Guidelines

### Ada Components
- Follow Ada 2022 style guidelines
- Use contracts (Pre/Post conditions) where appropriate
- Ensure type safety is maintained
- Run GNAT compiler with all warnings enabled

### Lean Components
- Write clear, well-documented proofs
- Follow Lean 4 conventions
- Ensure all theorems compile and verify
- Add examples demonstrating the proofs

### Akka Components
- Use typed actors
- Follow Scala style guide
- Write unit tests with Akka TestKit
- Ensure thread safety

### Java Components
- Use modern Java features (records, pattern matching, etc.)
- Follow Google Java Style Guide
- Write JUnit tests
- Ensure Netty configuration is optimal

### Erlang Components
- Follow OTP design principles
- Use proper supervision strategies
- Write EUnit tests
- Maintain backward compatibility

## Code Style

Each language has its own style guide:
- **Ada**: Ada Quality and Style Guide
- **Lean**: Lean 4 style conventions
- **Scala**: Scala Style Guide
- **Java**: Google Java Style Guide
- **Erlang**: Erlang Programming Rules

## Testing

All contributions must include tests:

```bash
make test           # Run all tests
make test-ada       # Ada tests only
make test-java      # Java tests only
make test-erlang    # Erlang tests only
```

## Documentation

- Update relevant documentation in `docs/`
- Add comments for complex logic
- Update README if adding new features
- Include examples for new functionality

## Pull Request Process

1. Update documentation if needed
2. Ensure all tests pass
3. Update the CHANGELOG (if exists)
4. Request review from maintainers
5. Address any feedback

## Questions?

Open an issue for:
- Bug reports
- Feature requests
- Architecture discussions
- General questions

## Code of Conduct

Be respectful, constructive, and professional in all interactions.

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
