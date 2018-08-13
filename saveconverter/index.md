---
layout: page
title: Save Converter
sidebar_link: true
---

<link rel="stylesheet" href="{{ site.baseurl }}/assets/css/bootstrap.css">
<link rel="stylesheet" href="{{ site.baseurl }}/assets/css/bootstrap-theme.css">
<style>
    #buttonsContainer button { margin-right: 5px; }
    #dlList li { display: inline-block; }
    #dlList li:before { content: '\2022'; margin-left: 0.5em; margin-right: 0.25em; }
</style>

Convert save files between SAV.8xv and .sav/.srm formats (experimental): <input style="display: inline-block" type="file" accept=".8xv,.sav,.srm" onChange="fileLoad(event)"/>

File link will appear here (click to download):
<ul id="dlList"></ul>

<div id="buttonsContainer"></div>

-----

*Online version implemented by [Adriweb](https://github.com/adriweb) with [Emscripten](http://emscripten.org/).*

*Files are converted in-browser, no personal data will be uploaded.*

<script src="convertsav-utils.js"></script>
<script>
    var script = document.createElement('script');
    script.src = "convertsav.js";
    document.body.appendChild(script);
</script>
