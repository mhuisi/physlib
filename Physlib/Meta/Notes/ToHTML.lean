/-
Copyright (c) 2024 Joseph Tooby-Smith. All rights reserved.
Released under Apache 2.0 license.
Authors: Joseph Tooby-Smith
-/
module

public meta import Physlib.Meta.Notes.HTMLNote
public import Physlib.Meta.Notes.NoteFile
/-!

## Turns a declaration into a html note structure.

-/

@[expose] public section

namespace Physlib
open Lean System Meta

namespace NoteFile
variable (N : NoteFile)

/-- Sorts `NoteInfo`'s by file name then by line number. -/
meta def sortLE (ni1 ni2 : HTMLNote) : Bool :=
  if N.files.idxOf ni1.fileName ≠ N.files.idxOf ni2.fileName then
    N.files.idxOf ni1.fileName ≤ N.files.idxOf ni2.fileName
  else
    ni1.line ≤ ni2.line

/-- Returns a sorted list of NodeInfos for a file system. -/
meta unsafe def getNodeInfo : CoreM (List HTMLNote) := do
  let env ← getEnv
  let allNotes := (noteExtension.getState env)
  let allDecl := (noteDeclExtension.getState env)
  let allInformalDecl := noteInformalDeclExtension.getState env
  let allNoteInfo :=
    allNotes.map HTMLNote.ofNodeInfo ++ (← allDecl.mapM HTMLNote.ofFormal)
      ++ (← allInformalDecl.mapM HTMLNote.ofInformal)
  let noteInfo := allNoteInfo.filter (fun x => x.fileName ∈ N.files)
  let noteInfoSort := noteInfo.toList.mergeSort N.sortLE
  pure noteInfoSort

/-- The HTML code needed to have syntax highlighting. -/
meta def codeBlockHTML : String :=
  "\n<meta charset=\"UTF-8\">\
    \n    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\
    \n    <!-- Include Highlight.js CSS -->\
    \n    <link rel=\"stylesheet\" \
    href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/styles/default.min.css\">\
    \n    <!-- Include Highlight.js -->\
    \n    <script \
    src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/highlight.min.js\"></script>\
    \n    <!-- Enable Highlight.js -->\
    \n    <script>hljs.highlightAll();</script>\
    \n    <script>\
    \n      hljs.registerLanguage('lean', function(hljs) {\
    \n          return {\
    \n              keywords: 'def theorem axiomatic structure lemma',\
    \n              contains: [\
    \n                  hljs.COMMENT('--', '$'),\
    \n                  hljs.C_NUMBER_MODE,\
    \n                  // Operators (custom definition)\
    \n                {\
    \n                    className: 'operator', // Define a class for styling\
    \n                    begin: /[:=+\\-*/<>|&!~^{}]/ // Regex for operators\
    \n                }]\
    \n          };\
    \n      });\
    \n      hljs.highlightAll();\
    \n      </script>\n"

/-- The html styles for informal definitions. -/
meta def informalDefStyle : String :=
  "\n<style>\
    \n        .informal-def {\
    \n            position: relative; /* Establish a relative positioning context for the button */\
    \n            background-color: #f8d7da; /* Light red background */\
    \n            border: 2px solid #dc3545; /* Solid darker red border */\
    \n            margin: 10px; /* Space outside the block */\
    \n            padding: 20px; /* Space inside the block */\
    \n        }\
    \n\
    \n        .informal-def .button {\
    \n            position: absolute; /* Position relative to the parent */\
    \n            top: 5px; /* Distance from the top of the block */\
    \n            right: 5px; /* Distance from the right of the block */\
    \n            background-color: rgba(220, 53, 69, 0.4);\
    \n            color: white; /* White text */\
    \n            border: none; /* No border for the button */\
    \n            padding: 5px 10px; /* Padding for the button */\
    \n            text-decoration: none; /* Remove underline from the link */\
    \n            border-radius: 5px; /* Rounded corners for the button */\
    \n            font-size: 12px; /* Smaller font size */\
    \n            cursor: pointer; /* Pointer cursor on hover */\
    \n        }\
    \n\
    \n        .informal-def .button:hover {\
    \n            background-color: #0056b3; /* Darker blue on hover */\
    \n        }\
    \n    body {\
    \n            color: #000000; /* Change the default text color to dark gray */\
    \n        }\
    \n</style>\n"

/-- The html styles for code button. -/
meta def codeButton : String :=
  "\n<style>\
    \n  .code-block-container {\
    \n    position: relative;\
    \n  }\
    \n\
    \n  .code-button {\
    \n    position: absolute; /* Position relative to the parent */\
    \n    top: 5px; /* Distance from the top of the block */\
    \n    right: 5px; /* Distance from the right of the block */\
    \n    background-color: rgba(220, 53, 69, 0.4);\
    \n    color: white; /* White text */\
    \n    border: none; /* No border for the button */\
    \n    padding: 5px 10px; /* Padding for the button */\
    \n    text-decoration: none; /* Remove underline from the link */\
    \n    border-radius: 5px; /* Rounded corners for the button */\
    \n    font-size: 12px; /* Smaller font size */\
    \n    cursor: pointer; /* Pointer cursor on hover */\
    \n  }\
    \n\
    \n  .code-button:hover {\
    \n    background-color: #0056b3;\
    \n  }\
    \n</style>\n"

/-- HTML allowing the use of mathjax. -/
meta def mathJaxScript : String :=
  "\n<!-- MathJax code -->\
    \n<script type=\"text/javascript\">\
    \n        window.MathJax = {\
    \n            tex: {\
    \n                inlineMath: [['$', '$']], // Use $...$ for inline math\
    \n                displayMath: [['$$', '$$']] // Use $$...$$ for block math\
    \n            }\
    \n        };\
    \n</script>\
    \n<script type=\"text/javascript\" id=\"MathJax-script\" async\
    \n    src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js\">\
    \n</script>\n"

/-- The header to the html code. -/
meta def headerHTML : String :=
  "---\
      \nlayout: default\
      \n---\
      \n<!DOCTYPE html>\
      \n<html>\
      \n<head>"
    ++ codeBlockHTML ++ mathJaxScript ++ informalDefStyle ++ codeButton
    ++ "</head>\
      \n</head>\
      \n<body>"

/-- The html code corresponding to the title, abstract and authors. -/
meta def titleHTML : String :=
  "<center><h1 style=\"font-size: 50px;\">" ++ N.title
    ++ "</h1></center>\
      \n<center><b>Authors:</b> "
    ++ String.intercalate ", " N.authors
    ++ "</center>\
      \n<center><b>Abstract:</b> "
    ++ N.abstract ++ "</center>"

/-- The html code corresponding to a note about Lean and its use. -/
meta def leanNote : String :=
  "\n<br>\
    \n<div style=\"border: 1px solid black; padding: 10px;\">\
    \n  <p>Note: These are not ordinary notes. They are created using an interactive theorem\
    \n  prover called <a href=\"https://lean-lang.org\">Lean</a>.\
    \n  Lean formally checks definitions, theorems and proofs for correctness.\
    \n  These notes are part of a much larger project called\
    \n  <a href=\"https://github.com/leanprover-community/physlib\">Physlib</a>, which aims to \
    digitalize\
    \n  high energy physics into Lean. Please consider contributing to this project.\
    \n  <br><br>\
    \n  Please provide feedback or suggestions for improvements by creating a GitHub issue\
    \n  <a href=\"https://github.com/leanprover-community/physlib/issues\">here</a>.\
    \n  </p>\
    \n</div>\n"
/-- The footer of the html file. -/
meta def footerHTML : String :=
  "</body>\
    \n</html>"

/-- The html file associated to a NoteFile string. -/
meta unsafe def toHTMLString : MetaM String := do
  let string := String.intercalate "\n" ((← N.getNodeInfo).map (fun x => x.content))
  pure (headerHTML ++ N.titleHTML ++ leanNote ++ string ++ footerHTML)

end NoteFile
end Physlib
