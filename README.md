# Bumpi - a get next semantic version bump cli tool

Interactivly get the next [semantic version](https://semver.org/) based on your latest git version tag and your last commits:

![Usage](./docs/Usage.png)

That's it!

# Usage

Binaries available soon!

```bash
# Cd into your repository root and run
get-next-version
```

You can now review which version you are at, based on the latest git-tag.

After that, you can decide how you want your next version to be by entering one of the following commands:

| Bump to | Command | Alternatives         |
| ------- | ------- | -------------------- |
| Major   | `major` | `breaking, br, b, !` |
| Minor   | `minor` | `feature, f`         |
| Patch   | `patch` | `fix, x`             |

The output will be safed into a `VERSION` file.

# Install

## Prerequisites

1. Install [ghcup](https://www.haskell.org/ghcup/)
2. Install `cabal` via `ghcup tui`

## With cabal installed

1. Clone this repo and cd into it
2. Run `cabal install`

# Motivation

> Write programs that do one thing and do it well. [Unix philosophy](https://en.wikipedia.org/wiki/Unix_philosophy)

I tried conventional commits with a tool that generates the next version. I did not like conventional commits after all since they are for machines, not humans.

I wanted a tool that just bumps my version based on my git. But all of the tools out there are fully featured and highly specialized tools that do a bunch of stuff. For example:

  - [mroth/bump](https://github.com/mroth/bump) Can only be used with GitHub repos.
  - [sindresorhus/np](https://github.com/sindresorhus/np) For publishing NPM packages.
  - [thenativeweb/get-next-version](https://github.com/thenativeweb/get-next-version) Uses conventional commits.
  - [c4urself/bump2version](https://github.com/c4urself/bump2version) For Python.

So bumpi does just one thing and that's it. :)
