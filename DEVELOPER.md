# Quickstart

## Build

Builds must pass with Haddock enabled and no warnings in order for your PR to be accepted.

```bash
stack build

# with haddock
stack build --haddock
```

## Lint

Linters are run in CI and must pass in order for your PR to be accepted.

To enable locally:

1. Install [`pre-commit`](https://pre-commit.com)
1. Enable it: `pre-commit install`

## Run tests

All tests must pass CI in order for your PR to be accepted.

```bash
stack test
```

# Documentation

All code should be fully documented, whether it's adding comments for future
developers or adding Haddock docs for functionality exposed in Haddock.

Changes that affect users should be mentioned in `CHANGELOG.md`. When doing so,
add an entry under under the `Upcoming` header containing:
* A description of the change
* The type of change (breaking, bugfix, etc.)
* If applicable,
    * How to migrate existing code
    * When it should be used
    * What it supersedes

The format is not important, as the list will be curated when releasing.

# Release

Follow these steps to release this project:

1. Create a new branch
    1. Bump version in `package.yaml`
        * All version bumps should follow [PvP](https://pvp.haskell.org/)
    1. Curate `CHANGELOG.md`, creating a new section for this version and
       moving everything previously in `Upcoming` into the new section
       (keeping `Upcoming` as a section)
    1. Add comments to new features indicating when it was added (e.g.
       `-- @since v2.0.0`)
    1. Run `stack haddock` and skim through documentation

1. Create PR as usual and merge into `main`
    1. In the `run_sdist` CI job, check the output of the `stack sdist`
       step for any warnings.

1. Create a release on GitHub on the merge commit
    1. The tag version should be of the format `vX.Y.Z`
    1. The release title should be the same as the tag version
    1. The release body should contain everything in `CHANGELOG.md` in the
       section for this version

1. Upload the package to Hackage
    1. Download the `http-api-data-qq-*.tar.gz` file from CI artifacts
    1. Upload tarball to Hackage
