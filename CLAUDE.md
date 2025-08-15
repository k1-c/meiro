# Claude Code Development Guide

This document provides guidelines and context for Claude Code when working on this project.

## 📝 Commit Message Convention

This project follows a strict commit message convention with emoji prefixes and Conventional Commits format.

**Please refer to [docs/git/COMMIT.md](docs/git/COMMIT.md) for the complete commit message policy.**

### Quick Reference

- Always use emoji prefix + type: `✨ feat: add new feature`
- Follow Conventional Commits specification
- Keep subject line under 72 characters
- Use present tense ("add" not "added")

## 🧪 Before Committing

Always ensure the following checks pass:

```bash
# Run tests
stack test

# Build the project
stack build

# Run the application to verify
stack run
```

## 🏗️ Project Structure

- `src/` - Source code
- `test/` - Test files
- `scripts/` - Build and utility scripts
- `.github/workflows/` - CI/CD pipelines
- `docs/` - Documentation

## 🚀 Release Process

This project uses automated release notes generation based on commit messages:

1. Commits with proper emoji prefixes are automatically categorized
2. When a tag is pushed, GitHub Actions generates release notes
3. The release is created with categorized changelog

## 📚 Key Documentation

- [Commit Message Policy](docs/git/COMMIT.md)
- [README](README.md)
