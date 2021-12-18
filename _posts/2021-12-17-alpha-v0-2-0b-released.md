---
layout: post
title: "Alpha v0.2.0b Released"
categories:
  - Releases
tags:
  - Release
---

This is a minor bugfix update to v0.2.0a, which fixes occasional freezes in some games like Donkey Kong Land as well as some potential runtime errors.

You can download the release from [GitHub](https://github.com/calc84maniac/tiboyce/releases).

## Changelog

### Fixed
-   Fix a game freeze occuring in certain conditions after a delayed EI is followed by a HALT. Fixes Donkey Kong Land.
-   Fix a likely runtime error when a memory access instruction reads an I/O register and later reads other memory.
