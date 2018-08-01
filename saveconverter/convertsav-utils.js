/* TI-Boy CE online save converter - helper script - Adriweb */

Module = {
  'collectedErrors': [],
  'printErr': function(text) { Module.collectedErrors.push(text); }
};

(function() {

    var buttonsContainer = document.getElementById("buttonsContainer");
    var dlList = document.getElementById("dlList");
    var cleanupFiles = function() {};

    var addBlobFileLink = function(file, name) {
        var li = document.createElement("li");
        var a = document.createElement("a");
        a.href = window.URL.createObjectURL((new Blob([file], {type: 'application/octet-stream'})));
        a.download = name;
        a.innerText = name;
        li.appendChild(a);
        dlList.appendChild(li);
    };

    var fnameRegExp = /^.*\.[sS]([aA][vV]|[rR][mM])|[A-Z][a-zA-Z0-9]{0,4}SAV\.8[xX][vV]$/
    var cond = '*.sav, *.srm, *SAV.8xv';

    fileLoaded = function(event, inputFileName)
    {
        dlList.innerHTML = buttonsContainer.innerHTML = "";
        cleanupFiles();
        cleanupFiles = function() {};

        if (event.target.readyState === FileReader.DONE)
        {
            do {
                outputFileName = prompt("Enter the output filename - " + cond);
                if (outputFileName === null) { return; } // early exit if the dialog was cancelled
                if (!outputFileName || !fnameRegExp.test(outputFileName)) {
                    alert("Output filename must match one of " + cond);
                }
            } while (!outputFileName || !fnameRegExp.test(outputFileName));

            FS.writeFile(inputFileName, new Uint8Array(event.target.result), {encoding: 'binary'});

            cleanupFiles = function() {
                try { FS.unlink(inputFileName); } catch (e){}
                try { FS.unlink(outputFileName); } catch (e){}
            }

            Module.collectedErrors = [];
            Module.callMain([ inputFileName, outputFileName ]);
            if (Module.collectedErrors.length > 0) {
                alert(Module.collectedErrors.join("\n"));
                return;
            }

            // Display save file
            try {
                var file = FS.readFile(outputFileName, {encoding: 'binary'});
                addBlobFileLink(file, outputFileName);
            } catch(e) {
                alert('Oops, something went wrong converting the save file: ' + e.message);
                return;
            }
        }
    };

    fileLoad = function(event)
    {
        var file = event.target.files[0];
        if (!file || !fnameRegExp.test(file.name))
        {
            alert('Error: file name must match one of ' + cond);
            return;
        }

        var reader = new FileReader();
        reader.onloadend = function(event) {
            fileLoaded(event, file.name);
        };
        reader.readAsArrayBuffer(file);
    };

})()
