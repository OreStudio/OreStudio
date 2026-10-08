## Summary

The latest tag and release in this repository is `v0.6.0`. `main` has moved on, and `CMakeLists.txt` has not moved with it. Downstream consumers that track sqlgen through vcpkg have no tag to pin, so they pin a raw commit instead.

We consume sqlgen through a vcpkg overlay that pins commit `47e571149b5e40f63cf7afb5fded134872cc68c0` and labels the port `0.8.0`, because no tag names that state of the tree.

## Evidence

- Tags: `v0.1.0`, `v0.2.0`, `v0.3.0`, `v0.4.0`, `v0.5.0`, `v0.6.0`. There is no `v0.7.0` and no `v0.8.0`.
- Releases stop at `v0.6.0`.
- `47e571149b5e40f63cf7afb5fded134872cc68c0` is 5 commits ahead of `v0.6.0` on `main`.
- `CMakeLists.txt` at that commit still declares `project(sqlgen VERSION 0.6.0 LANGUAGES CXX)`.
- `vcpkg.json` at that commit carries no version field.

## Impact

- A downstream package cannot express a version dependency; it must pin a commit.
- Version-based resolution through vcpkg or Conan cannot map a release to a tree.
- The git tags, the `project(... VERSION ...)` declaration, and the version downstream labels the port disagree, so "which version is this build" has no single answer.

## Request

- Tag the releases you consider `0.7.0` and `0.8.0`, or whatever the current `main` should be called.
- Keep `project(sqlgen VERSION ...)` in step with the latest tag.
- If `0.8.0` was never released as a tag and downstream should pin a different commit, please say which one.

Raised alongside the transaction defect in #140.
