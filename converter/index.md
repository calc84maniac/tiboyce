---
layout: page
title: ROM Converter
sidebar_link: true
---

<link rel="stylesheet" href="{{ site.baseurl }}/assets/css/bootstrap.css">
<link rel="stylesheet" href="{{ site.baseurl }}/assets/css/bootstrap-theme.css">
<style>
    #buttonsContainer button { margin-right: 5px; }
    #dlList li { display: inline-block; }
    #dlList li:before { content: '\2022'; margin-left: 0.5em; margin-right: 0.25em; }
</style>

Create .8xv files from your Game Boy (Color) ROM file: <input style="display: inline-block" type="file" accept=".gb,.gbc,.rom,.zip" onChange="fileLoad(event)"/>

File links will appear here (click to download):
<ul id="dlList"></ul>

<div id="buttonsContainer"></div>

-----

*Online version implemented by [Adriweb](https://github.com/adriweb) with [Emscripten](http://emscripten.org/).*

*Files are converted in-browser, no copyrighted data will be uploaded.*

<script src="romgen-utils.js"></script>
<script>
    var script = document.createElement('script');
    script.src = "romgen.js?v=2";
    document.body.appendChild(script);
</script>
