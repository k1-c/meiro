# Homebrew Formula for Meiro

## Setup Homebrew Tap

1. Create a new repository named `homebrew-tap` on GitHub
2. Copy the Formula file:

```bash
# In your homebrew-tap repository
mkdir Formula
cp meiro.rb Formula/
```

## User Installation

```bash
# Add the tap
brew tap k1-c/tap

# Install meiro
brew install meiro
```

## Update Formula

When releasing a new version:

1. Update version number in `meiro.rb`
2. Update URLs and SHA256 checksums
3. Commit and push to homebrew-tap repository

## Generate SHA256

```bash
# For each release archive
shasum -a 256 meiro-darwin-x86_64.tar.gz
shasum -a 256 meiro-darwin-arm64.tar.gz  
shasum -a 256 meiro-linux-x86_64.tar.gz
```

## Cross-platform Support

The formula automatically detects:
- macOS Intel (x86_64)
- macOS Apple Silicon (arm64) 
- Linux (x86_64)

And downloads the appropriate binary for each platform.