# Multi-stage build for Haskell application
FROM haskell:9.10.2-slim as builder

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    libgmp-dev \
    zlib1g-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy Stack configuration
COPY stack.yaml stack.yaml.lock package.yaml ./

# Copy source code
COPY src/ src/
COPY app/ app/
COPY test/ test/

# Build the application
RUN stack setup
RUN stack build --copy-bins --local-bin-path /usr/local/bin

# Runtime stage
FROM ubuntu:22.04

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libc6 \
    libgmp10 \
    libtinfo6 \
    && rm -rf /var/lib/apt/lists/*

# Copy binary from builder stage
COPY --from=builder /usr/local/bin/meiro /usr/local/bin/meiro

# Set the entrypoint
ENTRYPOINT ["/usr/local/bin/meiro"]
