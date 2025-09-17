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

# Function to get current version from workspace Cargo.toml
get_current_version() {
    grep '^version = ' Cargo.toml | head -1 | sed 's/version = "\([^"]*\)"/\1/'
}

# Function to calculate version bump
calculate_version() {
    local current_version="$1"
    local bump_type="$2"
    
    # Parse current version into components
    IFS='.' read -ra VERSION_PARTS <<< "$current_version"
    local major="${VERSION_PARTS[0]}"
    local minor="${VERSION_PARTS[1]}"
    local patch="${VERSION_PARTS[2]}"
    
    case "$bump_type" in
        "patch")
            echo "$major.$minor.$((patch + 1))"
            ;;
        "minor")
            echo "$major.$((minor + 1)).0"
            ;;
        "major")
            echo "$((major + 1)).0.0"
            ;;
        *)
            echo "$bump_type"  # Return as-is if not a bump command
            ;;
    esac
}

# Check if version argument is provided
if [ $# -eq 0 ]; then
    print_error "Usage: $0 <version|patch|minor|major> [--no-publish]"
    print_error "Examples:"
    print_error "  $0 0.3.1                 # Use exact version"
    print_error "  $0 patch                 # Increment patch version (0.4.7 -> 0.4.8)"
    print_error "  $0 minor                 # Increment minor version (0.4.7 -> 0.5.0)"
    print_error "  $0 major                 # Increment major version (0.4.7 -> 1.0.0)"
    print_error "  $0 patch --no-publish    # Skip publishing to crates.io"
    exit 1
fi

VERSION_INPUT=$1
PUBLISH_TO_CRATES_IO=true

# Check for --no-publish flag
if [ "$2" == "--no-publish" ]; then
    PUBLISH_TO_CRATES_IO=false
fi

# Get current version and calculate new version
CURRENT_VERSION=$(get_current_version)
if [ -z "$CURRENT_VERSION" ]; then
    print_error "Could not find current version in Cargo.toml"
    exit 1
fi

VERSION=$(calculate_version "$CURRENT_VERSION" "$VERSION_INPUT")

print_info "Current version: $CURRENT_VERSION"
if [ "$VERSION_INPUT" != "$VERSION" ]; then
    print_info "Bumping $VERSION_INPUT version: $CURRENT_VERSION -> $VERSION"
fi

# Validate version format (basic semver check)
if ! [[ $VERSION =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    print_error "Invalid version format. Please use semantic versioning (e.g., 1.2.3) or bump commands (patch, minor, major)"
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
print_info "- Sync internal workspace dependency versions in [workspace.dependencies] to $VERSION"
print_info "- Update current-version.json to $VERSION and set releaseDate"
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
print_info "- Deploy website to production (bun run deploy)"
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

# Update current-version.json to match the new version
print_info "Updating current-version.json..."
JSON_FILE="current-version.json"
RELEASE_DATE=$(date -u +%F)
if printf '{\n  "version": "%s",\n  "releaseDate": "%s"\n}\n' "$VERSION" "$RELEASE_DATE" > "$JSON_FILE"; then
    if grep -q '"version": '"\"$VERSION\"" "$JSON_FILE" && grep -q '"releaseDate": '"\"$RELEASE_DATE\"" "$JSON_FILE"; then
        print_info "✓ Updated $JSON_FILE to $VERSION (releaseDate $RELEASE_DATE)"
    else
        print_warning "Wrote $JSON_FILE but could not verify contents; please check manually."
    fi
else
    print_error "Failed to write $JSON_FILE"
    exit 1
fi

# Update versions of internal workspace dependencies in [workspace.dependencies]
print_info "Syncing internal workspace dependency versions in [workspace.dependencies]..."

# Use awk for portability across macOS and Linux. We:
# - Enter the [workspace.dependencies] table
# - For any inline dep with path = ..., replace existing version or add it if missing
TMP_FILE="${WORKSPACE_CARGO}.tmp"
awk -v ver="$VERSION" '
  BEGIN { inwd = 0 }
  /^\[workspace\.dependencies\]/ { inwd = 1; print; next }
  /^\[/ { if (inwd) inwd = 0 }
  {
    if (inwd && $0 ~ /path[[:space:]]*=/) {
      if ($0 ~ /version[[:space:]]*=/) {
        sub(/version[[:space:]]*=[[:space:]]*\"[^\"]*\"/, "version = \"" ver "\"")
      } else {
        sub(/\}[[:space:]]*$/, ", version = \"" ver "\" }")
      }
    }
    print
  }
' "$WORKSPACE_CARGO" > "$TMP_FILE" && mv "$TMP_FILE" "$WORKSPACE_CARGO"

# Verify at least blots-core was updated if present
if grep -qE '^\s*blots-core\s*=\s*\{[^}]*version\s*=\s*\"'"$VERSION"'\"' "$WORKSPACE_CARGO"; then
    print_info "✓ Updated [workspace.dependencies] internal versions to $VERSION"
else
    # If blots-core isn't present, still proceed but warn (covers future refactors)
    if grep -qE '^\s*blots-core\s*=\s*\{' "$WORKSPACE_CARGO"; then
        print_warning "blots-core entry found but version did not update. Please verify [workspace.dependencies]."
    else
        print_info "No blots-core entry detected in [workspace.dependencies]; nothing to sync."
    fi
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
git add "$WORKSPACE_CARGO" Cargo.lock "$JSON_FILE"
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

# Deploy website
print_info "Deploying website..."
if (cd website && bun run deploy); then
    print_info "✓ Website deployed successfully!"
else
    print_error "Failed to deploy website"
    print_warning "The release has been completed, but the website deployment failed."
    print_warning "You can manually deploy the website with: cd website && bun run deploy"
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
print_info "- Deployed website to production"
