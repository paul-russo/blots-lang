#!/bin/bash

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if version argument is provided
if [ $# -eq 0 ]; then
    print_error "Usage: $0 <version> [--no-publish]"
    print_error "Example: $0 0.3.1"
    print_error "Example: $0 0.3.1 --no-publish  # Skip publishing to crates.io"
    exit 1
fi

VERSION=$1
PUBLISH_TO_CRATES_IO=true

# Check for --no-publish flag
if [ "$2" == "--no-publish" ]; then
    PUBLISH_TO_CRATES_IO=false
fi

# Validate version format (basic semver check)
if ! [[ $VERSION =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    print_error "Invalid version format. Please use semantic versioning (e.g., 1.2.3)"
    exit 1
fi

print_info "Starting release process for version $VERSION..."

# Check if we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    print_error "Not in a git repository"
    exit 1
fi

# Check if working directory is clean
if [ -n "$(git status --porcelain)" ]; then
    print_error "Working directory is not clean. Please commit or stash your changes first."
    git status --short
    exit 1
fi

# Check if we're on the main branch
CURRENT_BRANCH=$(git branch --show-current)
if [ "$CURRENT_BRANCH" != "main" ]; then
    print_error "You are on branch '$CURRENT_BRANCH', not 'main'. Releases can only be made from the main branch."
    exit 1
fi

# Show what will be done and ask for confirmation
print_info "Release plan:"
print_info "- Update workspace version to $VERSION in Cargo.toml"
print_info "- Run cargo check to update Cargo.lock with new version"
print_info "- Commit changes (Cargo.toml + Cargo.lock) with message 'Release v$VERSION'"
print_info "- Create and push git tag 'v$VERSION'"
print_info "- Push changes to branch '$CURRENT_BRANCH'"
if [ "$PUBLISH_TO_CRATES_IO" = true ]; then
    print_info "- Publish all crates to crates.io in dependency order (all-or-nothing)"
    print_info "  If any crate fails to publish, all successfully published crates will be yanked"
else
    print_info "- Skip publishing to crates.io (--no-publish flag specified)"
fi
print_info ""
print_warning "This will trigger a GitHub Actions build. Continue? (y/N)"
read -r response
if [[ ! "$response" =~ ^[Yy]$ ]]; then
    print_info "Release cancelled."
    exit 0
fi

# Update version in workspace Cargo.toml
print_info "Updating version in workspace Cargo.toml..."

WORKSPACE_CARGO="Cargo.toml"
if [ ! -f "$WORKSPACE_CARGO" ]; then
    print_error "Workspace Cargo.toml not found"
    exit 1
fi

print_info "Updating workspace version..."

# Update workspace version only
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS sed
    sed -i '' "s/^version = \".*\"/version = \"$VERSION\"/" "$WORKSPACE_CARGO"
else
    # Linux sed
    sed -i "s/^version = \".*\"/version = \"$VERSION\"/" "$WORKSPACE_CARGO"
fi

# Verify the changes
if grep -q "version = \"$VERSION\"" "$WORKSPACE_CARGO"; then
    print_info "✓ Updated workspace version to $VERSION"
else
    print_error "Failed to update workspace version"
    exit 1
fi

# Run cargo check to ensure everything still compiles and update Cargo.lock
print_info "Running cargo check to verify compilation and update Cargo.lock..."
if ! cargo check --quiet; then
    print_error "Cargo check failed. Please fix compilation errors before releasing."
    exit 1
fi

print_info "✓ All packages compile successfully"

# Commit the changes (including Cargo.lock)
print_info "Committing version updates..."
git add "$WORKSPACE_CARGO" Cargo.lock
git commit -m "Release v$VERSION"

# Create and push tag
TAG_NAME="v$VERSION"
print_info "Creating and pushing tag $TAG_NAME..."
git tag "$TAG_NAME"

# Push changes and tag
print_info "Pushing changes and tag to origin..."
git push origin "$CURRENT_BRANCH"
git push origin "$TAG_NAME"

# Publish to crates.io if requested
if [ "$PUBLISH_TO_CRATES_IO" = true ]; then
    print_info "Publishing crates to crates.io..."
    
    # Check if cargo login is configured
    if ! cargo search blots-core --limit 1 > /dev/null 2>&1; then
        print_warning "Unable to connect to crates.io. Please ensure you're logged in with 'cargo login'"
        print_warning "Skipping crates.io publishing"
    else
        print_info "Publishing crates to crates.io..."
        
        # Track successfully published crates for potential yanking
        PUBLISHED_CRATES=()
        
        # Function to yank all successfully published crates
        yank_published_crates() {
            if [ ${#PUBLISHED_CRATES[@]} -gt 0 ]; then
                print_error "Publishing failed! Yanking successfully published crates to maintain consistency..."
                for published_crate in "${PUBLISHED_CRATES[@]}"; do
                    print_info "Yanking $published_crate version $VERSION..."
                    if cargo yank --version "$VERSION" "$published_crate"; then
                        print_info "✓ Yanked $published_crate"
                    else
                        print_error "Failed to yank $published_crate - you may need to yank manually"
                    fi
                done
            fi
        }
        
        # Publish in dependency order: core first, then the others
        CRATES_TO_PUBLISH=(
            "blots-core"
            "blots"
            "blots-wasm"
        )
        
        for crate in "${CRATES_TO_PUBLISH[@]}"; do
            print_info "Publishing $crate..."
            
            # Change to crate directory and publish
            if (cd "$crate" && cargo publish); then
                print_info "✓ Successfully published $crate"
                
                # Track this crate as successfully published
                PUBLISHED_CRATES+=("$crate")
            else
                print_error "Failed to publish $crate"
                yank_published_crates
                print_error "All successfully published crates have been yanked to maintain consistency."
                print_error "You can retry the full release with: $0 $VERSION"
                exit 1
            fi
        done
        
        print_info "✓ All crates published to crates.io!"
    fi
fi

print_info "✓ Release v$VERSION completed successfully!"
print_info "GitHub Actions should now be triggered by the new tag."
print_info ""
print_info "Summary:"
print_info "- Updated workspace version to $VERSION"
print_info "- Updated Cargo.lock with new version"
print_info "- Committed changes with message 'Release v$VERSION'"
print_info "- Created and pushed tag '$TAG_NAME'"
print_info "- Pushed changes to branch '$CURRENT_BRANCH'"
if [ "$PUBLISH_TO_CRATES_IO" = true ]; then
    print_info "- Published all crates to crates.io successfully"
else
    print_info "- Skipped publishing to crates.io (--no-publish flag specified)"
fi
