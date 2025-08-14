# Meiro - Installation Guide

## Package Managers (Recommended)

### Homebrew (macOS & Linux)
```bash
# Add the tap
brew tap k1-c/tap

# Install meiro
brew install meiro
```

### Snap Store (Linux)
```bash
sudo snap install meiro
```

### AUR - Arch Linux
```bash
# Using yay
yay -S meiro

# Using paru
paru -S meiro
```

### Debian/Ubuntu (.deb)
```bash
# Download .deb from releases, then:
sudo dpkg -i meiro_1.0.0_amd64.deb
sudo apt-get install -f  # Fix dependencies
```

## Binary Release

### Download Pre-built Binary
1. Go to [Releases](https://github.com/k1-c/meiro/releases)
2. Download the appropriate archive:
   - **Linux**: `meiro-linux-x86_64.tar.gz`
   - **macOS Intel**: `meiro-darwin-x86_64.tar.gz`
   - **macOS Apple Silicon**: `meiro-darwin-arm64.tar.gz`
3. Extract and install:
```bash
tar -xzf meiro-*.tar.gz
sudo mv meiro /usr/local/bin/
```

### AppImage (Linux)
```bash
# Download AppImage from releases
chmod +x meiro-1.0.0-x86_64.AppImage
./meiro-1.0.0-x86_64.AppImage
```

### Verify Installation
```bash
meiro --help
```

## Docker

### Run with Docker
```bash
docker run --rm -it meiro:latest
```

### Build Docker Image
```bash
git clone https://github.com/k1-c/meiro.git
cd meiro
docker build -t meiro:latest .
```

## Build from Source

### Prerequisites
- [Stack](https://docs.haskellstack.org/en/stable/README/)
- GCC and build tools

### Ubuntu/Debian
```bash
sudo apt-get update
sudo apt-get install -y build-essential libgmp-dev zlib1g-dev

git clone https://github.com/k1-c/meiro.git
cd meiro
./build.sh
```

### Manual Build
```bash
stack setup
stack build
stack exec meiro
```

## Usage

Start the maze game:
```bash
meiro
```

Controls:
- **WASD** or **Arrow Keys**: Move
- **Q**: Quit

## System Requirements

### Supported Platforms
- **Linux x86_64** (Ubuntu, Debian, Arch, Fedora, etc.)
- **macOS Intel** (x86_64)
- **macOS Apple Silicon** (arm64)

### Requirements
- Terminal with Unicode support
- Minimum 4MB RAM
- No additional dependencies for binary distribution

### Package Manager Availability
| Platform | Homebrew | Snap | AUR | .deb | AppImage |
|----------|----------|------|-----|------|----------|
| macOS    | ✅       | ❌   | ❌  | ❌   | ❌       |
| Linux    | ✅       | ✅   | ✅¹ | ✅²  | ✅       |

¹ Arch Linux only  
² Debian/Ubuntu only

## Troubleshooting

### Permission Denied
```bash
chmod +x meiro
```

### Homebrew Tap Not Found
```bash
# Ensure you've added the tap first
brew tap k1-c/tap
brew update
```

### macOS Security Warning
```bash
# If macOS blocks execution, allow in Security & Privacy settings
# Or use:
sudo xattr -r -d com.apple.quarantine meiro
```

### Missing Libraries (Source Build)
**Linux:**
```bash
sudo apt-get install -y libc6-dev libgmp-dev zlib1g-dev libtinfo-dev
```

**macOS:**
```bash
# Usually not needed, but if required:
brew install gmp
```

### Terminal Issues
- Ensure your terminal supports Unicode/UTF-8 encoding for proper maze rendering
- For best experience, use a terminal with emoji support (e.g., iTerm2, modern GNOME Terminal)
- Some minimal terminals may not display emojis correctly
