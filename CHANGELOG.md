# Changelog for `meiro`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 1.0.1 - 2025-08-15

### Added
- **Restructured CLI**: Clean command-line interface with modular architecture
- **Comprehensive Testing**: Full test coverage for CLI functionality (`CommandSpec.hs`)
- **Better Error Messages**: Clear guidance for invalid inputs with practical examples
- **Command Documentation**: Detailed command usage guide in README

### Changed
- **Default Maze Size**: Changed from 30x15 to 25x15 for better gameplay experience
- **Code Organization**: Extracted CLI logic from `Main.hs` to dedicated `Meiro.Command` module
- **Documentation**: Updated README with comprehensive command & argument guide
- **Size Constraint Documentation**: Clearly documented odd number requirement (≥ 5)

### Fixed
- **Default Configuration**: Fixed default width to use odd number (25 instead of 30)
- **Import Warnings**: Removed unused imports causing CI/CD build failures
- **Test Compatibility**: Updated tests to match new default size and error messages
- **Release Script**: Fixed regex syntax error in `generate-release.sh`
- **Error Messages**: Improved validation messages for size constraints

### Security
- **Input Validation**: Enhanced command-line argument validation with strict type checking

## 1.0.0 - 2025-08-15

### Added
- Initial release of meiro
- Core maze generation functionality
- Support for various maze algorithms
- Command-line interface
