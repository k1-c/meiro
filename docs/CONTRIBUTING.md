# Contributing to Meiro

Thank you for your interest in contributing to Meiro! This document provides guidelines and instructions for contributing to the project.

## 📋 Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [How to Contribute](#how-to-contribute)
- [Commit Message Guidelines](#commit-message-guidelines)
- [Pull Request Process](#pull-request-process)
- [Testing](#testing)
- [Documentation](#documentation)

## 🤝 Code of Conduct

By participating in this project, you agree to abide by our Code of Conduct:

- Use welcoming and inclusive language
- Be respectful of differing viewpoints and experiences
- Gracefully accept constructive criticism
- Focus on what is best for the community
- Show empathy towards other community members

## 🚀 Getting Started

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/YOUR_USERNAME/meiro.git
   cd meiro
   ```
3. **Add upstream remote**:
   ```bash
   git remote add upstream https://github.com/k1-c/meiro.git
   ```

## 🛠️ Development Setup

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) (Haskell build tool)
- GHC 9.10.2 (will be installed by Stack)
- Git

### Setup Instructions

1. **Install Stack**:
   ```bash
   curl -sSL https://get.haskellstack.org/ | sh
   ```

2. **Build the project**:
   ```bash
   stack build
   ```

3. **Run tests**:
   ```bash
   stack test
   ```

4. **Run the application**:
   ```bash
   stack run
   ```

### Development Workflow

```bash
# Create a feature branch
git checkout -b feature/your-feature-name

# Make your changes
# ...

# Run tests
stack test

# Build the project
stack build

# Test your changes
stack run

# Commit your changes (see commit guidelines below)
git add .
git commit -m "✨ feat: add amazing feature"

# Push to your fork
git push origin feature/your-feature-name
```

## 💡 How to Contribute

### Reporting Bugs

Before creating bug reports, please check existing issues to avoid duplicates.

**When reporting a bug, include:**
- A clear and descriptive title
- Steps to reproduce the issue
- Expected behavior
- Actual behavior
- System information (OS, version, etc.)
- Any relevant error messages or logs

### Suggesting Enhancements

**When suggesting enhancements, include:**
- A clear and descriptive title
- Detailed description of the proposed feature
- Use cases and examples
- Possible implementation approach (if you have ideas)

### Code Contributions

1. **Find an issue** to work on, or create one
2. **Comment on the issue** to let others know you're working on it
3. **Create a feature branch** from `main`
4. **Make your changes** following our coding standards
5. **Write or update tests** as needed
6. **Update documentation** if necessary
7. **Submit a pull request**

## 📝 Commit Message Guidelines

Since we use **squash and merge** for all pull requests, you don't need to follow strict commit message conventions in your feature branch. Feel free to use any commit messages that help you during development.

For maintainers working directly on the main branch, please refer to [docs/git/COMMIT.md](git/COMMIT.md) for the complete commit message policy.

### Before Creating a PR

**Always ensure:**
- ✅ All tests pass (`stack test`)
- ✅ Code builds without warnings (`stack build`)
- ✅ Code follows Haskell style guidelines
- ✅ PR title follows our convention

## 🔄 Pull Request Process

1. **Update your fork**:
   ```bash
   git fetch upstream
   git checkout main
   git merge upstream/main
   ```

2. **Rebase your feature branch**:
   ```bash
   git checkout feature/your-feature-name
   git rebase main
   ```

3. **Push your changes**:
   ```bash
   git push origin feature/your-feature-name
   ```

4. **Create a Pull Request** on GitHub

### Pull Request Guidelines

**Your PR should:**
- Have a clear and descriptive title
- Reference any related issues
- Include a description of changes
- Pass all CI checks
- Have no merge conflicts
- Include tests for new functionality
- Update documentation as needed

### PR Title Format

Follow the same convention as commit messages:
```
✨ feat: add dark mode support
🐛 fix: resolve maze generation bug
```

### PR Description Template

```markdown
## Description
Brief description of what this PR does

## Related Issue
Fixes #(issue number)

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change
- [ ] Documentation update

## Testing
- [ ] Tests pass locally
- [ ] New tests added (if applicable)

## Checklist
- [ ] Passed All Lint and Tests
- [ ] Self-review completed
- [ ] Documentation updated
- [ ] No new warnings
```

## 🧪 Testing

### Running Tests

```bash
# Run all tests
stack test

# Run with coverage
stack test --coverage

# Run specific test suite
stack test meiro:test:meiro-test
```

### Writing Tests

- Place tests in the `test/` directory
- Use Hspec for test framework
- Use QuickCheck for property-based testing
- Ensure good test coverage for new features

Example test structure:
```haskell
spec :: Spec
spec = describe "Feature" $ do
  it "should do something" $ do
    result <- doSomething
    result `shouldBe` expected
    
  prop "property holds" $ \input ->
    property $ checkProperty input
```

## 📚 Documentation

### Code Documentation

- Add Haddock comments for public functions
- Include usage examples in comments
- Keep documentation up-to-date with code changes

### Project Documentation

- Update README.md for user-facing changes
- Update this CONTRIBUTING.md for process changes
- Document new features in appropriate places

## 🏗️ Project Structure

```
meiro/
├── src/                 # Source code
│   ├── Meiro/
│   │   ├── Types.hs    # Core data types
│   │   ├── Utils.hs    # Utility functions
│   │   ├── Rendering.hs # Terminal rendering
│   │   ├── Gameplay.hs # Game logic
│   │   └── Algorithms/ # Maze generation algorithms
├── test/               # Test files
├── scripts/            # Build and utility scripts
├── docs/               # Documentation
└── .github/            # GitHub specific files
    └── workflows/      # CI/CD workflows
```

## 🎯 Coding Standards

### Haskell Style Guide

- Use 2 spaces for indentation (no tabs)
- Keep lines under 80 characters when possible
- Use meaningful variable and function names
- Follow [Haskell Style Guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)

### Code Quality

- Write pure functions when possible
- Handle errors explicitly (avoid partial functions)
- Use appropriate data structures
- Comment complex algorithms
- Keep functions small and focused

## 🚀 Release Process

Releases are automated through GitHub Actions:

1. Maintainer creates and pushes a tag: `git tag v1.2.0`
2. GitHub Actions builds binaries for all platforms
3. Release notes are auto-generated from commit messages
4. GitHub Release is created with artifacts

## 💬 Getting Help

- **Discord**: [Join our community](https://discord.gg/meiro) (if available)
- **Issues**: [GitHub Issues](https://github.com/k1-c/meiro/issues)
- **Discussions**: [GitHub Discussions](https://github.com/k1-c/meiro/discussions)

## 🙏 Recognition

Contributors will be:
- Listed in the release notes
- Added to CONTRIBUTORS.md
- Thanked in our community channels

Thank you for contributing to Meiro! 🎮
