---
layout: post
title: "Alpha v0.2.0a Released"
categories:
  - Releases
tags:
  - Release
---

This is a minor bugfix update to v0.2.0 which fixes occasional runtime errors when loading auto save states for RTC cartridges.
This should help prevent loss of progress in games that have a clock, like Pokémon Gold and Silver.
As always though, it's best practice not to rely entirely on auto save states, but to save in-game as well.

You can download the release from [GitHub](https://github.com/calc84maniac/tiboyce/releases).

## Changelog

### Fixed
-   Fix an intermittent code corruption when loading RTC save states.
