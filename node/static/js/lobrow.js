/**
 * Normalized name (extension ".js" is cut off)
 * - "../foo" = file foo in the directory above the current directory
 * - "foo" = a sibling of the current file
 * - "bar/foo" = descend into bar to get to file foo
 *
 * Import name:
 * - "./foo" look for a sibling of the current file
 * - "../foo" look for "foo" in the directory above the current file
 */
var lobrow = function(global) {
    "use strict";
    
    // A function whose name starts with an underscore is exported for unit tests only
    var e = {};

    //----------------- Internal constants
        
    /**
     * The file where everything starts.
     * Never used directly, only to resolve import names.
     * Example – correctly resolve a reference to a sibling:
     * "./foo" resolved against e._START_FILE must become "foo".
     */
    e._START_FILE = "__START__"; // pseudo-name
    e._CURRENT_DIRECTORY = "";

    //----------------- Public interface

    /**
     * @param body is optional – has the signature function(module1, module2, ...)
     */
    e.require = function (importNames, body) {
        loadModules(resolveImportNames(e._START_FILE, importNames),
            function (modules) {
                if (body) {
                    body.apply(null, modules);
                }
            }
        );
    };
    /**
     * Waits for window.onload in addition to the loaded modules.
     * @param body is optional – has the signature function(module1, module2, ...)
     */
    e.onload = function (importNames, body) {
        var oldOnload = window.onload;
        var countDown = loadModules(resolveImportNames(e._START_FILE, importNames),
            function (modules) {
                if (body) {
                    body.apply(null, modules);
                }
                if (oldOnload) {
                    oldOnload();
                }
            },
            1 // prevents firing before we do the final decrement
        );
        window.onload = function () {
            countDown.dec();
        }
    };
    /**
     * Warning: Only works if the browser keeps a function’s source code (minified or not)!
     *
     * @param body has the signature function(require, exports, module, global)
     */
    e.module = function (body) {
        var bodySource = body.toString();
        if (!startsWith(bodySource, "function")) {
            throw new Error("Could not access the source of the body: "+bodySource);
        }
        runEvaluatedModule(e._START_FILE, body, bodySource);
    };

    //----------------- Loading

    var moduleCache = {};
    var currentlyLoading = {};
    
    /**
     * @param callback optional – has the signature function(moduleArray).
     *        Receives an array with the values of the modules identified via `normalizedNames`
     * @param countDownIncrement default is 0
     */
    function loadModules(normalizedNames, callback, countDownIncrement) {
        var countDownIncrement = (countDownIncrement === undefined ? 0 : countDownIncrement);
        var modules = [];
        function finalAction() {
            if (callback) {
                callback(modules);
            }
        }
        var countDown = new CountDown(normalizedNames.length + countDownIncrement, finalAction);
        
        normalizedNames.forEach(function (normalizedName, i) {
            // The callbacks can be called in any order (fork-join-style)!
            loadModule(normalizedName, function (value) {
                modules[i] = value;
                countDown.dec();
            });
        });
        
        return countDown;
    }
    
    /**
     * @param callback mandatory
     */
    function loadModule(normalizedName, callback) {
        if (moduleCache.hasOwnProperty(normalizedName)) {
            callback(moduleCache[normalizedName]);
            return;
        }
        
        if (currentlyLoading[normalizedName]) {
            throw new Error("Cycle: module '"+normalizedName+"' is already loading");
        }
        currentlyLoading[normalizedName] = true;
        var fileName = normalizedName+".js";
        try {
            var req = new XMLHttpRequest();
            req.open('GET', fileName, true);
            // In Firefox, a JavaScript MIME type means that the script is immediately eval-ed
            req.overrideMimeType("text/plain");
            req.onreadystatechange = function(event) {
                if (req.readyState === 4 /* complete */) {
                    evaluateRawModuleSource(normalizedName, req.responseText, function (result) {
                        delete currentlyLoading[normalizedName];
                        moduleCache[normalizedName] = result;
console.log('got module',result)
                        callback(result);
                    });
                }
            }
            req.send();
        } catch (e) {
            throw new Error("Could not load: "+fileName
                + ". Possible reasons:\n"
                + "- File does not exist\n"
                + "- Firefox blocks XHR for local files above the current directory\n"
                + "- Chrome blocks XHR for local files (use command line option --allow-file-access-from-files)"
            );
        }
    }
    
    function evaluateRawModuleSource(normalizedModuleName, bodySource, callback) {
        var body = eval("(function (require,exports,module,global) {"+bodySource+"})");
        runEvaluatedModule(normalizedModuleName, body, bodySource, callback);
    }

    /**
     * @param body has the signature function(require,exports,module)
     * @param bodySource is only needed to extract and pre-cache the required modules
     * @param callback is optional
     */
    function runEvaluatedModule(normalizedModuleName, body, bodySource, callback) {
        var importNames = e._extractImportNames(bodySource);
        var normalizedNames = resolveImportNames(normalizedModuleName, importNames);
        loadModules(normalizedNames, function(modules) {
            var moduleDict = e._zipToObject(importNames, modules);
            runEvaluatedBody(body, moduleDict, callback);
        });
    }
    
    /**
     * @param moduleBody has the signature function(require,exports,module)
     * @param callback is optional
     */
    function runEvaluatedBody(moduleBody, moduleDict, callback) {
        var module = {
            require: function (importName) {
                return moduleDict[importName];
            },
            exports: {}
        };
        moduleBody(module.require, module.exports, module, global);
        if (callback) {
            callback(module.exports);
        }
    }
    
    // Match quoted text non-greedily (as little as possibly)
    var REQUIRE_REGEX = /require\s*\(\s*(["'])(.*?)\1\s*\)/g;
    e._extractImportNames = function (source) {
        var importNames = [];
        var match;
        while(match = REQUIRE_REGEX.exec(source)) {
            importNames.push(match[2]);
        }
        return importNames;
    }
    
    //----------------- Normalize module names

    /**
     * Either:
     * - Object: maps a global module name to either a path or an object (the module)
     * - Function: takes a global name and returns a path or an object
     */
    e.globalNames = {};

    function resolveImportNames(baseName, importNames) {
        return importNames.map(function (importName) {
            return e._resolveImportName(baseName, importName);
        });
    }

    /**
     * The behavior of this function is modeled after Node’s url.resolve
     * http://nodejs.org/docs/latest/api/url.html#url.resolve
     */
    e._resolveImportName = function (baseName, importName) {
        if (!e._isLegalNormalizedName(baseName)) {
            throw new Error("Illegal normalized name: "+baseName);
        }
        if (startsWith(importName, "/")) {
            // absolute name
            return importName;
        }
        if (startsWith(importName, "./") || startsWith(importName, "../")) {
            // relative name: go down in current directory (possibly after going up)
            baseName = e._goToParentDir(baseName); // go to current directory
        
            if (startsWith(importName, "./")) {
                importName = removePrefixMaybe("./", importName);
            } else {
                while (startsWith(importName, "../")) {
                    // going up
                    importName = removePrefixMaybe("../", importName);
                    baseName = e._goToParentDir(baseName);
                }
            }
            // Now go down
            return e._descend(baseName, importName);
        } else {
            // global name
            var resolvedName;
            if (typeof e.globalNames === "function") {
                resolvedName = e.globalNames(importName);
            } else {
                resolvedName = e.globalNames[importName];
            }
            switch(typeof resolvedName) {
                case "object": // also result for null, but can't happen here
                    if (!moduleCache[importName]) {
                        moduleCache[importName] = resolvedName;
                    }
                    return importName;
                case "string":
                    return resolvedName;
                case "undefined":
                    throw new Error("Unknown global name: "+importName);
                default:
                    throw new Error("Illegal mapping value: "+resolvedName);
            }
        }
    };

    e._goToParentDir = function (name) {
        if (name === "") {
            return "..";
        }
        if (/^[.][.]([/][.][.])*$/.test(name)) {
            // We are currently *only* going up (as opposed to going up and down)
            return "../"+name;
        }
        
        // We are going down (possibly after going up)
        // => we can go up by removing the last path name segment
        var slashIndex = name.lastIndexOf("/");
        if (slashIndex < 0) {
            return e._CURRENT_DIRECTORY;
        } else {
            return name.slice(0, slashIndex);
        }
    };
    
    e._descend = function (base, path) {
        if (base.length === 0) {
            return path;
        } else {
            return base + "/" + path;
        }
    };

    e._isLegalNormalizedName = function (name) {
        // Can be absolute
        // Can be relative: "../foo" or "bar", but not "./bar"
        // Must be a JS file after appending ".js"
        return !endsWith(name, "/") && !endsWith(name, ".js") && !startsWith(name, "./");
    };
    
    //----------------- Generic helpers
    
    e._zipToObject = function (keys, values) {
        if (keys.length !== values.length) {
            throw new Error("Both arrays must have the same length: "+keys+" "+values);
        }
        var obj = {};
        for (var i=0; i<keys.length; i++) {
            obj[keys[i]] = values[i];
        }
        return obj;
    }
    
    function removePrefixMaybe(prefix, str) {
        if (str.indexOf(prefix) === 0) {
            return str.slice(prefix.length);
        } else {
            return str;
        }
    }

    function startsWith(str, prefix) {
        return str.indexOf(prefix) === 0;
    }
    
    function endsWith(str, suffix) {
        var index = str.lastIndexOf(suffix);
        return index >= 0 && index === str.length - suffix.length;
    }

    function CountDown(counter, finalAction) {
        this.counter = counter;
        this.finalAction = finalAction;
        // Important! finalAction must always be executed
        this.maybeFire();
    }
    CountDown.prototype.maybeFire = function () {
        if (this.counter <= 0 && this.finalAction) {
            this.finalAction();
        }
    };
    CountDown.prototype.dec = function () {
        this.counter--;
        this.maybeFire();
    };

    //----------------- Done
    return e;
}(this); // hand in global object