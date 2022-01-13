# Clojure-Docs Peek (CDP)

A simple popup to show colorized docstrings above point, for _any_
function and resolved namespace. Also shows some basic summary
statistics for functions and namespaces.

![Non-local function (showing namespace) with see-also](./img/cdp2.png
"Non-local function (showing namespace) with see-also")

<sub>_**Caption:** Point sitting on non-local function, showing
colorized and resolved namespace, function name, parameters, docstring,
and optional see-alsos._</sub>

You can see in the screenshots that the gray box is
[quick-peek](https://github.com/cpitclaudel/quick-peek) at work
inserting the documentation as an overlay.

This is a lot like `cider-clojuredocs` but:

- is lighter in that it only shows a **bare minimum of info** (no
  examples)

- **keeps you from switching** to docs or source buffers to hunt for info

- shows docstrings for **even your own functions and namespaces**,
  encouraging you to add more of them!

- enables using **shorter namespace aliases** since their resolution is
  right at your fingertips

## Installation

Install the dependencies:

- [Cider](https://github.com/clojure-emacs/cider)
- [Quick-Peek](https://github.com/cpitclaudel/quick-peek)

_**Note:** Cider is a huge dependency, but it is assumed that you've
already bought into that. quick-peek is tiny and has none._

``` elisp
(add-to-list 'load-path "path to your clone of this")
(require 'clojure-docs-peek)
```

## Usage

Invoke `clojure-docs-peek-toggle` to open and close the peek.

Use universal argument (prefix `C-u`) to include _See also_ section.

I use this constantly and like to bind this to be really quick and
easy to use, so it's `CD` (as a key-chord):

``` elisp
(key-chord-define-global "CD" 'clojure-docs-peek-toggle)
```

## More Screenshots

![Local function (no namespace)](./img/cdp4.png
"Local function (no namespace)")

![Namespace](./img/cdp3.png
"Namespace")

## Beyond

I'm sure this could be altered to depend on LSP rather than Cider.

Please let me know in a ticket if you have more ideas for getting
other clojure documentation into popups.
