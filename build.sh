#!/bin/bash

set -e

echo "Building Meiro for Linux distribution..."

# Clean previous builds
echo "Cleaning previous builds..."
rm -rf dist release
mkdir -p dist release

# Build static binary
echo "Building static binary..."
stack build --copy-bins --local-bin-path ./dist

# Create archive
echo "Creating release archive..."
tar -czf release/meiro-linux-x86_64.tar.gz -C dist meiro

# Build Docker image (optional)
if command -v docker &> /dev/null; then
    echo "Building Docker image..."
    docker build -t meiro:latest .
    echo "Docker image built: meiro:latest"
fi

echo "Build complete!"
echo "Binary: dist/meiro"
echo "Archive: release/meiro-linux-x86_64.tar.gz"

# Test the binary
echo "Testing binary..."
if ./dist/meiro --help > /dev/null 2>&1; then
    echo "✓ Binary test passed"
else
    echo "✗ Binary test failed"
    exit 1
fi
