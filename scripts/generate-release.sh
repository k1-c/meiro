#!/usr/bin/env bash

set -euo pipefail

# Configuration
REPO_URL="${GITHUB_SERVER_URL:-https://github.com}/${GITHUB_REPOSITORY:-k1-c/meiro}"

# Get version tags
CURRENT_TAG="${1:-$(git describe --tags --abbrev=0 2>/dev/null || echo "HEAD")}"
PREVIOUS_TAG="${2:-$(git describe --tags --abbrev=0 ${CURRENT_TAG}^ 2>/dev/null || echo "")}"

# Determine commit range
if [ -z "$PREVIOUS_TAG" ]; then
    COMMIT_RANGE=""
else
    COMMIT_RANGE="${PREVIOUS_TAG}..${CURRENT_TAG}"
fi

# Initialize categories
declare -A categories=(
    ["breaking"]=""
    ["feat"]=""
    ["fix"]=""
    ["perf"]=""
    ["refactor"]=""
    ["docs"]=""
    ["test"]=""
    ["build"]=""
    ["ci"]=""
    ["chore"]=""
    ["style"]=""
    ["other"]=""
)

# Parse commits and categorize them
while IFS= read -r line; do
    hash=$(echo "$line" | cut -d' ' -f1)
    message=$(echo "$line" | cut -d' ' -f2-)
    body=$(git log -1 --format=%b $hash)
    
    # Extract type from conventional commit format
    # Handle emoji prefix: "✨ feat: description" or "feat: description"
    if [[ "$message" =~ ^[^a-zA-Z]*([a-zA-Z]+)[^:]*:[[:space:]](.+)$ ]]; then
        type="${BASH_REMATCH[1]}"
        scope=""
        description="${BASH_REMATCH[2]}"
        
        # Check for breaking changes
        if [[ "$message" == *"!"* ]] || [[ "$body" == *"BREAKING CHANGE"* ]]; then
            categories["breaking"]+="- ${message}\n"
        fi
        
        # Categorize by type
        case "$type" in
            feat|feature)
                categories["feat"]+="- ${message}\n"
                ;;
            fix|bugfix)
                categories["fix"]+="- ${message}\n"
                ;;
            perf|performance)
                categories["perf"]+="- ${message}\n"
                ;;
            refactor)
                categories["refactor"]+="- ${message}\n"
                ;;
            docs|documentation)
                categories["docs"]+="- ${message}\n"
                ;;
            test|tests)
                categories["test"]+="- ${message}\n"
                ;;
            build)
                categories["build"]+="- ${message}\n"
                ;;
            ci)
                categories["ci"]+="- ${message}\n"
                ;;
            chore)
                categories["chore"]+="- ${message}\n"
                ;;
            style)
                categories["style"]+="- ${message}\n"
                ;;
            *)
                categories["other"]+="- ${message}\n"
                ;;
        esac
    else
        # Commits that don't follow the convention
        categories["other"]+="- ${message}\n"
    fi
done < <(git log ${COMMIT_RANGE} --pretty=format:"%H %s" --reverse 2>/dev/null || echo "")

# Generate Markdown output
generate_section() {
    local title="$1"
    local content="$2"
    
    if [ -n "$content" ]; then
        echo "### ${title}"
        echo ""
        echo -e "$content"
    fi
}

# Output changelog
echo "## What's Changed"
echo ""

# Breaking changes first (most important)
generate_section "⚠️ BREAKING CHANGES" "${categories["breaking"]}"

# Features
generate_section "✨ New Features" "${categories["feat"]}"

# Bug fixes
generate_section "🐛 Bug Fixes" "${categories["fix"]}"

# Performance
generate_section "⚡ Performance Improvements" "${categories["perf"]}"

# Other sections
generate_section "♻️ Code Refactoring" "${categories["refactor"]}"
generate_section "📚 Documentation" "${categories["docs"]}"
generate_section "✅ Tests" "${categories["test"]}"
generate_section "🏗️ Build System" "${categories["build"]}"
generate_section "👷 CI/CD" "${categories["ci"]}"
generate_section "🧹 Maintenance" "${categories["chore"]}"
generate_section "🎨 Code Style" "${categories["style"]}"
generate_section "📦 Other Changes" "${categories["other"]}"

# Contributors
echo "### 👥 Contributors"
echo ""
if [ -n "$COMMIT_RANGE" ]; then
    git shortlog -sn --no-merges ${COMMIT_RANGE} | while read count name; do
        echo "- @${name} (${count})"
    done
else
    git shortlog -sn --no-merges | while read count name; do
        echo "- @${name} (${count})"
    done
fi

echo ""

# Full changelog link
if [ -n "$PREVIOUS_TAG" ] && [ "$CURRENT_TAG" != "HEAD" ]; then
    echo "**Full Changelog**: [${PREVIOUS_TAG}...${CURRENT_TAG}](${REPO_URL}/compare/${PREVIOUS_TAG}...${CURRENT_TAG})"
fi