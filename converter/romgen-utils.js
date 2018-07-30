/* TI-Boy CE online ROM converter - helper script - Adriweb */

Module = {
  'printErr': function(text) { alert(text); }
};

(function() {

    var buttonsContainer = document.getElementById("buttonsContainer");
    var dlList = document.getElementById("dlList");

    var addBlobFileLink = function(file, name) {
        var li = document.createElement("li");
        var a = document.createElement("a");
        a.href = window.URL.createObjectURL((new Blob([file], {type: 'application/octet-stream'})));
        a.download = name;
        a.innerText = name;
        li.appendChild(a);
        dlList.appendChild(li);
    };

    var makeCompressedAndDownload = function(buttonLink, mode, fileName, romTitle, inputFileName, outputPrefix)
    {
        Module.callMain([ mode, '-t', romTitle, inputFileName, outputPrefix ]);
        var file = FS.readFile(fileName, {encoding: 'binary'});
        if (file) {
            buttonLink.onclick = null;
            buttonLink.href = window.URL.createObjectURL((new Blob([file], {type: 'application/octet-stream'})));
            buttonLink.download = fileName;
            buttonLink.click();
        } else {
            alert('Oops, something went wrong converting the ROM (see console)');
            return;
        }
    };

    fileLoaded = function(event, inputFileName)
    {
        dlList.innerHTML = buttonsContainer.innerHTML = "";

        if (event.target.readyState === FileReader.DONE)
        {
            var fnameNoExt = inputFileName.replace(/\.(gb|gbc|rom|zip)$/, '');
            var outputPrefix = fnameNoExt.replace(/[^A-Z0-9]/gi, '');
            outputPrefix = (outputPrefix.substr(0, 1).toUpperCase() + outputPrefix.substr(1)).substr(0, 5);
            var savedOutputPrefix = outputPrefix;
            var cond = '(5 alphanumerical characters max, 1st must be uppercase letter)';
            do {
                if (!outputPrefix || outputPrefix.length === 0 || (! /^[A-Z][a-zA-Z0-9]{0,4}$/.test(outputPrefix))) {
                    alert(cond);
                }
                outputPrefix = prompt("Enter the output filename prefix - " + cond, savedOutputPrefix);
                if (outputPrefix === null) { return; } // early exit if the dialog was cancelled
            } while (!outputPrefix || ! /^[A-Z][a-zA-Z0-9]{0,4}$/.test(outputPrefix));

            var romTitle = fnameNoExt;
            do {
                romTitle = prompt("Enter the ROM title to display in TI-Boy CE (ASCII characters)", fnameNoExt);
                if (romTitle === null) { return; } // early exit if the dialog was cancelled
            } while (!romTitle || romTitle.length == 0 || (! /^[\x20-\x7F]{0,255}$/.test(romTitle)));

            FS.writeFile(inputFileName, new Uint8Array(event.target.result), {encoding: 'binary'});

            var cleanupFiles = function()
            {
                try { FS.unlink(outputPrefix + ".8xv"); } catch (e){}
                for (var i=0; i<50; i++) {
                    try { FS.unlink(outputPrefix + "R" + ('00'+i).slice(-2) + ".8xv"); } catch (e){}
                }
            }

            Module.callMain([ '-t', romTitle, inputFileName, outputPrefix ]);

            // Display meta file
            var fileName = outputPrefix + ".8xv";
            var file = FS.readFile(fileName, {encoding: 'binary'});
            if (file) {
                addBlobFileLink(file, fileName);
            } else {
                alert('Oops, something went wrong converting the ROM (see console)');
                return;
            }

            // Display part files
            for (var i=0; i<50; i++) {
                try {
                    fileName = outputPrefix + "R" + ('00'+i).slice(-2) + ".8xv";
                    file = FS.readFile(fileName, {encoding: 'binary'});
                    if (file) {
                        addBlobFileLink(file, fileName);
                    } else {
                        break;
                    }
                } catch (e) {
                    console.log('[Error] Oops, probably reached the end of the files ' + e.message, fileName);
                    break;
                }
            }

            // Launch a b84 download
            var btn = document.createElement("a");
            btn.href = "#";
            btn.className = "btn btn-default";
            btn.innerHTML = "<span class='glyphicon glyphicon-compressed' aria-hidden='true'></span> Download TI-84 Plus CE bundle";
            btn.onclick = (function(btn) {
                return function(event) {
                    cleanupFiles();
                    makeCompressedAndDownload(btn, '-b84', outputPrefix+".b84", romTitle, inputFileName, outputPrefix);
                    event.stopPropagation();
                    return false;
                }
            })(btn);
            buttonsContainer.appendChild(btn);

            // Launch a b83 download
            var btn = document.createElement("a");
            btn.href = "#";
            btn.className = "btn btn-default";
            btn.innerHTML = "<span class='glyphicon glyphicon-compressed' aria-hidden='true'></span> Download TI-83 Premium CE bundle";
            btn.onclick = (function(btn) {
                return function(event) {
                    cleanupFiles();
                    makeCompressedAndDownload(btn, '-b83', outputPrefix+".b83", romTitle, inputFileName, outputPrefix);
                    event.stopPropagation();
                    return false;
                }
            })(btn);
            buttonsContainer.appendChild(btn);

            buttonsContainer.appendChild(document.createElement("br"));

            // Add download buttons
            if (/firefox/i.test(navigator.userAgent))
            {
                // Launch all downloads
                var btn = document.createElement("btn");
                btn.href = "#";
                btn.className = "btn btn-default";
                btn.innerHTML = "<span class='glyphicon glyphicon-download-alt' aria-hidden='true'></span> Launch downloads (all files)";
                btn.onclick = function() {
                    [].forEach.call(document.querySelectorAll('#dlList a'), function (a) { a.click(); });
                };
                buttonsContainer.appendChild(btn);
            }

            // Launch a zip download
            var btn = document.createElement("a");
            btn.href = "#";
            btn.className = "btn btn-default";
            btn.innerHTML = "<span class='glyphicon glyphicon-compressed' aria-hidden='true'></span> Download all files in a .zip";
            btn.onclick = (function(btn) {
                return function(event) {
                    cleanupFiles();
                    makeCompressedAndDownload(btn, '-z', outputPrefix+".zip", romTitle, inputFileName, outputPrefix);
                    event.stopPropagation();
                    return false;
                }
            })(btn);
            buttonsContainer.appendChild(btn);
        }
    };

    fileLoad = function(event)
    {
        var file = event.target.files[0];
        if (!file || !(/\.(gb|gbc|rom|zip)$/.test(file.name)))
        {
            alert('Error: a .gb/gbc/rom/zip file is needed');
            return;
        }

        var reader = new FileReader();
        reader.onloadend = function(event) {
            fileLoaded(event, file.name);
        };
        reader.readAsArrayBuffer(file);
    };

})()
