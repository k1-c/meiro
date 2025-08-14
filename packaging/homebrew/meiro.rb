class Meiro < Formula
  desc "Terminal-based maze generation and solving game"
  homepage "https://github.com/k1-c/meiro"
  license "MIT"
  version "1.0.0"

  if OS.mac?
    if Hardware::CPU.arm?
      url "https://github.com/k1-c/meiro/releases/download/v1.0.0/meiro-darwin-arm64.tar.gz"
      sha256 "" # Will be filled when creating actual release
    else
      url "https://github.com/k1-c/meiro/releases/download/v1.0.0/meiro-darwin-x86_64.tar.gz"
      sha256 "" # Will be filled when creating actual release
    end
  elsif OS.linux?
    url "https://github.com/k1-c/meiro/releases/download/v1.0.0/meiro-linux-x86_64.tar.gz"
    sha256 "" # Will be filled when creating actual release
  end

  depends_on "glibc" => :build if OS.linux?

  def install
    bin.install "meiro"
  end

  test do
    system bin/"meiro", "--help"
  end
end