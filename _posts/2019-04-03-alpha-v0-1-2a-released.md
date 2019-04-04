---
layout: post
title: "Alpha v0.1.2a Released"
categories:
  - Releases
tags:
  - Release
---

This is a minor release to update the converter executables. If you exclusively use the web converters, you can stick with v0.1.2.

You can download the release from [GitHub](https://github.com/calc84maniac/tiboyce/releases).

## Changelog

### Fixed
-   Updated ROM converter to respect a new AppVar size limitation on OS 5.3.5 (at this time, only on TI-83 Premium CE).
-   Dynamically linked the converter EXEs against the Universal C Runtime, avoiding false positives on many antivirus engines.
