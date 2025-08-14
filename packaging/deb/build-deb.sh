#!/bin/bash
set -e

VERSION="1.0.0"
PKG_DIR="meiro_${VERSION}_amd64"

# Create package directory structure
mkdir -p "$PKG_DIR/usr/bin"
mkdir -p "$PKG_DIR/usr/share/doc/meiro"
mkdir -p "$PKG_DIR/DEBIAN"

# Copy binary
cp dist/meiro "$PKG_DIR/usr/bin/"
chmod 755 "$PKG_DIR/usr/bin/meiro"

# Copy control files
cp packaging/deb/DEBIAN/control "$PKG_DIR/DEBIAN/"

# Copy documentation
cp README.md "$PKG_DIR/usr/share/doc/meiro/"
cp LICENSE "$PKG_DIR/usr/share/doc/meiro/"
cp CHANGELOG.md "$PKG_DIR/usr/share/doc/meiro/" 2>/dev/null || true

# Set permissions
chmod 644 "$PKG_DIR/DEBIAN/control"
chmod -R 755 "$PKG_DIR/DEBIAN"
chmod -R 644 "$PKG_DIR/usr/share/doc/meiro/"*

# Build package
dpkg-deb --build "$PKG_DIR"

echo "Debian package created: ${PKG_DIR}.deb"