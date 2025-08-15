#!/usr/bin/env bash

set -euo pipefail

VERSION="${MEIRO_VERSION:-latest}"
INSTALL_DIR="${MEIRO_INSTALL_DIR:-/usr/local/bin}"
REPO="k1-c/meiro"
GITHUB_URL="https://github.com/${REPO}"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
RESET='\033[0m'

log_info() {
    echo -e "${BLUE}[INFO]${RESET} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${RESET} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${RESET} $1" >&2
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${RESET} $1"
}

detect_os() {
    local os=""
    case "$(uname -s)" in
        Darwin)
            os="darwin"
            ;;
        Linux)
            os="linux"
            ;;
        *)
            log_error "Unsupported operating system: $(uname -s)"
            exit 1
            ;;
    esac
    echo "$os"
}

detect_arch() {
    local arch=""
    case "$(uname -m)" in
        x86_64|amd64)
            arch="x86_64"
            ;;
        arm64|aarch64)
            arch="arm64"
            ;;
        *)
            log_error "Unsupported architecture: $(uname -m)"
            exit 1
            ;;
    esac
    echo "$arch"
}

check_command() {
    if ! command -v "$1" &> /dev/null; then
        log_error "$1 is required but not installed"
        exit 1
    fi
}

get_download_url() {
    local os="$1"
    local arch="$2"
    local version="$3"
    
    local url=""
    if [ "$version" = "latest" ]; then
        url="${GITHUB_URL}/releases/latest/download/meiro-${os}-${arch}.tar.gz"
    else
        url="${GITHUB_URL}/releases/download/${version}/meiro-${os}-${arch}.tar.gz"
    fi
    
    echo "$url"
}

download_and_install() {
    local url="$1"
    local install_dir="$2"
    
    local temp_dir
    temp_dir=$(mktemp -d)
    trap "rm -rf $temp_dir" EXIT
    
    log_info "Downloading meiro from: $url"
    
    if ! curl -fsSL "$url" -o "$temp_dir/meiro.tar.gz"; then
        log_error "Failed to download meiro"
        exit 1
    fi
    
    log_info "Extracting archive..."
    if ! tar -xzf "$temp_dir/meiro.tar.gz" -C "$temp_dir"; then
        log_error "Failed to extract archive"
        exit 1
    fi
    
    if [ ! -f "$temp_dir/meiro" ]; then
        log_error "meiro binary not found in archive"
        exit 1
    fi
    
    chmod +x "$temp_dir/meiro"
    
    if [ -w "$install_dir" ]; then
        log_info "Installing meiro to $install_dir..."
        mv "$temp_dir/meiro" "$install_dir/"
    else
        log_warning "Need sudo permissions to install to $install_dir"
        sudo mv "$temp_dir/meiro" "$install_dir/"
    fi
    
    log_success "meiro installed successfully!"
}

verify_installation() {
    local install_dir="$1"
    
    if [ -f "$install_dir/meiro" ] && [ -x "$install_dir/meiro" ]; then
        local version
        version=$("$install_dir/meiro" --version 2>/dev/null || echo "unknown")
        log_success "Installation verified: meiro $version"
        
        if command -v meiro &> /dev/null; then
            log_info "You can now run: meiro"
        else
            log_warning "$install_dir is not in your PATH"
            log_info "Add this to your shell config:"
            echo "    export PATH=\"$install_dir:\$PATH\""
            log_info "Or run directly: $install_dir/meiro"
        fi
    else
        log_error "Installation verification failed"
        exit 1
    fi
}

main() {
    echo -e "${BLUE}🧩 Meiro Installer${RESET}"
    echo "================================"
    
    check_command curl
    check_command tar
    
    local os
    os=$(detect_os)
    log_info "Detected OS: $os"
    
    local arch
    arch=$(detect_arch)
    log_info "Detected architecture: $arch"
    
    if [ "$os" = "darwin" ] && [ "$arch" = "arm64" ]; then
        log_info "Apple Silicon detected"
    fi
    
    local download_url
    download_url=$(get_download_url "$os" "$arch" "$VERSION")
    
    download_and_install "$download_url" "$INSTALL_DIR"
    
    verify_installation "$INSTALL_DIR"
    
    echo ""
    echo "================================"
    echo -e "${GREEN}🎉 Ready to play!${RESET}"
    echo ""
    echo "Run 'meiro' to start the game"
    echo "Use WASD or arrow keys to navigate"
    echo "Press Q to quit"
    echo ""
    echo "Enjoy the maze! 🎮"
}

main "$@"