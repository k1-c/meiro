#!/bin/bash
set -e

APPDIR="meiro.AppDir"
VERSION="1.0.0"

# Create AppDir structure
mkdir -p "$APPDIR/usr/bin"
mkdir -p "$APPDIR/usr/share/applications"
mkdir -p "$APPDIR/usr/share/icons/hicolor/256x256/apps"

# Copy binary
cp dist/meiro "$APPDIR/usr/bin/"
chmod +x "$APPDIR/usr/bin/meiro"

# Copy desktop file
cp packaging/appimage/meiro.desktop "$APPDIR/"
cp packaging/appimage/meiro.desktop "$APPDIR/usr/share/applications/"

# Create simple icon (text-based for terminal app)
convert -size 256x256 xc:black \
        -font DejaVu-Sans-Bold -pointsize 24 \
        -fill white -gravity center \
        -annotate +0+0 'MEIRO' \
        "$APPDIR/meiro.png" 2>/dev/null || \
echo "Warning: ImageMagick not found, using placeholder icon"

# Copy icon
cp "$APPDIR/meiro.png" "$APPDIR/usr/share/icons/hicolor/256x256/apps/" 2>/dev/null || true

# Create AppRun
cat > "$APPDIR/AppRun" << 'EOF'
#!/bin/bash
exec "$APPDIR/usr/bin/meiro" "$@"
EOF
chmod +x "$APPDIR/AppRun"

# Download appimagetool
if [ ! -f appimagetool ]; then
    wget https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage
    mv appimagetool-x86_64.AppImage appimagetool
    chmod +x appimagetool
fi

# Build AppImage
./appimagetool "$APPDIR" "meiro-$VERSION-x86_64.AppImage"

echo "AppImage created: meiro-$VERSION-x86_64.AppImage"