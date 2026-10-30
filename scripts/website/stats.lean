/-
Copyright (c) 2024 Joseph Tooby-Smith. All rights reserved.
Released under Apache 2.0 license.
Authors: Joseph Tooby-Smith
-/
import Physlib.Meta.Informal.Post
import Mathlib.Lean.CoreM
/-!

# Physlib Stats

This file concerns with statistics of Physlib.

-/

open Lean System Meta Physlib

def getStats : MetaM String := do
  let noDefsVal ← noDefs
  let noLemmasVal ← noLemmas
  let noImportsVal ← noImports
  let noDefsNoDocVal ← noDefsNoDocString
  let noLemmasNoDocVal ← noLemmasNoDocString
  let noLinesVal ← noLines
  let noInformalLemmasVal ← noInformalLemmas
  let s :=
    s!"\n    Number of Files 📄: {noImportsVal}\
      \n    Number of lines 💻: {noLinesVal}\
      \n    Number of Definitions (incl. instances): {noDefsVal - noInformalLemmasVal}\
      \n    - Of which {noDefsNoDocVal} do not have doc-strings\
      \n    Number of Lemmas: {noLemmasVal + noInformalLemmasVal}\
      \n    - Of which {noLemmasNoDocVal} do not have doc-strings"
  pure s

unsafe def Stats.toHtml : MetaM String := do
  let noDefsVal ← noDefs
  let noLemmasVal ← noLemmas
  let noImportsVal ← noImports
  let noDefsNoDocVal ← noDefsNoDocString
  let noLemmasNoDocVal ← noLemmasNoDocString
  let noLinesVal ← noLines
  let noInformalDefsVal ← noInformalDefs
  let noInformalLemmasVal ← noInformalLemmas
  let noTODOsVal ← noTODOs
  let noFilesWithTODOsVal ← noFilesWithTODOs
  let header :=
    "---\
      \nlayout: default\
      \n---\
      \n<!DOCTYPE html>\
      \n<html>\
      \n<head>\
      \n    <title>Stats for Physlib</title>\
      \n     <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\
      \n     <style>\
      \n        /* Style the progress bar to be wider and take up more space */\
      \n        progress {\
      \n            width: 80%; /* Adjust to take up more horizontal space */\
      \n            height: 30px; /* Increase height to make it wider */\
      \n            appearance: none; /* Remove default browser styles */\
      \n        }\
      \n\
      \n        /* Change the color for WebKit browsers (Chrome, Safari, etc.) */\
      \n        progress::-webkit-progress-bar {\
      \n            background-color: #f3f3f3; /* Background color */\
      \n            border-radius: 5px;\
      \n            overflow: hidden;\
      \n        }\
      \n\
      \n        progress::-webkit-progress-value {\
      \n            background-color: #157878; /* Change this to your desired color */\
      \n            border-radius: 5px;\
      \n        }\
      \n\
      \n        /* Change the color for Firefox */\
      \n        progress::-moz-progress-bar {\
      \n            background-color: #157878; /* Change this to your desired color */\
      \n        }\
      \n    </style>\
      \n</head>\
      \n<body>"
  let body :=
    s!"\n<h1>Stats for Physlib</h1>\
      \n<h3>Number of Files 📄: {noImportsVal}</h3>\
      \n<h3>Number of lines 💻: {noLinesVal}</h3>\
      \n<h3>Number of Definitions (incl. instances): {noDefsVal - noInformalLemmasVal}</h3>\
      \n<p>- Of which {noDefsVal - noDefsNoDocVal - noInformalLemmasVal} have doc-strings:</p>\
      \n<progress value=\"{noDefsVal - noDefsNoDocVal - noInformalLemmasVal}\" \
      max=\"{noDefsVal - noInformalLemmasVal}\"></progress>\
      \n<p>- Of which {noDefsVal - noInformalLemmasVal - noInformalDefsVal} are not <a \
      href=\"https://physlib.io/InformalGraph.html\">informal definitions</a>:</p>\
      \n<progress value=\"{noDefsVal - noInformalLemmasVal - noInformalDefsVal}\" \
      max=\"{noDefsVal - noInformalLemmasVal}\"></progress>\
      \n\
      \n<h3>Number of Lemmas: {noLemmasVal + noInformalLemmasVal}</h3>\
      \n<p>- Of which {noLemmasVal - noLemmasNoDocVal + noInformalLemmasVal} have doc-strings:</p>\
      \n<progress value=\"{noLemmasVal - noLemmasNoDocVal + noInformalLemmasVal}\" \
      max=\"{noLemmasVal + noInformalLemmasVal}\"></progress>\
      \n<p>- Of which {noLemmasVal} are not <a \
      href=\"https://physlib.io/InformalGraph.html\">informal lemmas</a>:</p>\
      \n<progress value=\"{noLemmasVal}\" max=\"{noLemmasVal + noInformalLemmasVal}\"></progress>\
      \n<h3>Number of <a href=\"https://physlib.io/TODOList\">TODOs</a>: {noTODOsVal}</h3>\
      \n<p>- There are {noImportsVal - noFilesWithTODOsVal} (of {noImportsVal}) files which are \
      TODO free:</p>\
      \n<progress value=\"{noImportsVal - noFilesWithTODOsVal}\" max=\"{noImportsVal}\"></progress>\
      \n   "
  let footer :=
    "\n</body>\
      \n</html>\n"
  pure (header ++ "\n" ++ body ++ "\n" ++ footer)

unsafe def main (args : List String) : IO UInt32 := do
  let _ ← noImports
  Lean.enableInitializersExecution
  let statString ← CoreM.withImportModules #[`Physlib] (getStats).run'
  println! statString
  if "mkHTML" ∈ args then
    let html ← CoreM.withImportModules #[`Physlib] (Stats.toHtml).run'
    let htmlFile : System.FilePath := { toString := "./docs/Stats.html" }
    IO.FS.writeFile htmlFile html
    IO.println (s!"HTML file made.")
  pure 0
