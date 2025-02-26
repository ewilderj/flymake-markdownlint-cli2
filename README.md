# flymake-markdownlint-cli2, a markdown linter for Emacs

Lint your Markdown files with
[flymake](https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html)
(built into Emacs) and
[markdownlint-cli2](https://github.com/DavidAnson/markdownlint-cli2).

## Usage

```lisp
(add-hook 'markdown-mode-hook #'flymake-mode)
(require 'flymake-markdownlint-cli2)
(add-hook 'markdown-mode-hook 'flymake-markdownlint-cli2-setup)
```

### With use-package

``` lisp
(use-package flymake-markdownlint-cli2
  :vc (:url "https://github.com/ewilderj/flymake-markdownlint-cli2"
            :rev :newest
            :branch "main")
  :config
  (add-hook 'markdown-mode-hook 'flymake-mode)
  (add-hook 'markdown-mode-hook 'flymake-markdownlint-cli2-setup))
```

## Configuration

By default the mode looks for `.markdownlint-cli2.mjs` in the buffer's
directory, and searches recursively upwards in the file system until
it finds a config file.  The configuration filename can be configured,
and you can also specify an absolute path to the configuration file.

Type `M-x customize-group` followed by `flymake-markdownlint-cli2` to
configure these options.

### Example

If you want to extend or override the default rules, e.g. by using rules
from [github/markdownlint-github](https://github.com/github/markdownlint-github),
install these into the root of your project, and create a configuration
file `.markdownlint-cli2.mjs` accordingly.

## Credits

This mode was adapted from Micah Elliott's work at
[flymake-mdl](https://github.com/MicahElliott/flymake-mdl).
