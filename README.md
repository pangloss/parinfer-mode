# Why fork

[Parinfer Mode](https://github.com/DogLooksGood/parinfer-mode) is unmaintained.

[parinfer-rust-mode](https://github.com/justinbarclay/parinfer-rust-mode) seems okay if it works, but it...

- requires either having a Rust compiler or downloading a prebuilt binary
- relies on a [fork](https://github.com/justinbarclay/parinfer-rust) of [parinfer-rust](https://github.com/eraserhd/parinfer-rust), and
- has itself not had an update since April 2021.

As I rely on this mode to do all my Lisp editing, I figured I might as well make some changes to my local copy.

# What Is Parinfer

> ***Parinfer*** is a proof-of-concept editor mode for Lisp programming languages. It simplifies the way we write Lisp by auto-adjusting parens when indentation changes and vice versa. The hope is to make basic Lisp-editing easier for newcomers and experts alike, while still allowing existing plugins like Paredit to satisfy the need for more advanced operations.
>
> — <https://shaunlebron.github.io/parinfer/>

There are two modes, *Indent* and *Paren*. Indent mode allows you to edit indentation, while Parinfer automatically adjusts parens; Paren mode allows you to edit parens, while Parinfer automatically adjusts indentation.

# Installation

```elisp
(straight-use-package '(parinfer :host github :repo "kisaragi-hiu/parinfer-mode"))
```

Recommended configuration:

```elisp
(use-package parinfer-mode
  :init
  (setq parinfer-extensions
        (list 'defaults       ; should be included.
              'pretty-parens  ; different paren styles for different modes.
              'lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
              'paredit        ; Bindings for Paredit commands
              'smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
              'smart-yank))   ; Yank behavior depend on mode.
  (add-hook 'clojure-mode-hook #'parinfer-mode)
  (add-hook 'scheme-mode-hook #'parinfer-mode)
  ;; Major modes run hooks of their entire ancestry. Adding this to
  ;; lisp-data-mode is enough for it to take effect in lisp-mode and
  ;; emacs-lisp-mode, both of which are derived from lisp-data-mode.
  (if (boundp 'lisp-data-mode-hook)
      (add-hook 'lisp-data-mode-hook #'parinfer-mode)
    ;; Before Emacs 28, emacs-lisp-mode derives from lisp-mode, so add
    ;; the hook there.
    (add-hook 'lisp-mode-hook #'parinfer-mode))
  :config
  (define-key parinfer-mode-map
    (kbd "C-,") #'parinfer-toggle-mode))
```

# Commands

- `parinfer-toggle-mode`:   toggle between indent mode and paren mode
- `parinfer-diff`:   show how switching to indent mode will modify the buffer
- `parinfer-auto-fix`:   manual trigger for auto adjustment of indentation for the whole buffer (when parens are balanced)

# Configuration

- `parinfer-auto-switch-indent-mode`{.verbatim}

    Possible values: `nil`{.verbatim} (default), `t`{.verbatim}, `closing`{.verbatim}

    Switch to indent mode whenever parens are balanced in paren mode.

    Set this to `closing`{.verbatim} to only do so after inserting a closing paren.

- `parinfer-delay-invoke-threshold`{.verbatim}

    Possible values: number of characters; defaults to 6000

    Text processing is normally done immediately after a command completes. If the text to be processed has more than this number of characters, it will instead be done in an idle timer (after `parinfer-delay-invoke-idle`{.verbatim} seconds, which defaults to 0.3).

- `parinfer-lighters`{.verbatim} (default: `("Parinfer:Indent" . "Parinfer:Paren")`{.verbatim})

    The mode line shows "Parinfer:Indent" when Parinfer is active in indent mode, "Parinfer:Paren" if it is in paren mode. The car is the indent mode string, while the cdr is the paren mode string.

    There is no need to add the leading space. It is added automatically.

- `parinfer-extensions`{.verbatim} (default: `(defaults pretty-parens smart-yank)`{.verbatim})

    Enabled extensions.

    An extension contains different pieces of code that run at different stages (entering indent mode, entering paren mode, enabling and disabling `parinfer-mode`{.verbatim}).

    Possible extensions:

| Extension     | Function                                                                                                             |
|---------------|----------------------------------------------------------------------------------------------------------------------|
| defaults      | Should be enabled, basic compatibility                                                                               |
| pretty-parens | Dim parens in **Indent Mode**, use rainbow delimiters in **Paren Mode**                                              |
| smart-yank    | Make yank (paste) preserve indentation in **Indent Mode** & preserve parens in **Paren Mode**                        |
| smart-tab     | Make `C-f` & `C-b` on an empty line go to next/previous indentation, possibly inserting extra spaces in the process. |
| paredit       | Introduce some paredit commands from paredit-mode.                                                                   |
| lispy         | Integration with Lispy.                                                                                              |

# Current Issues

`parinfer-mode` currently assumes space-based indentation and will break if tabs are being used.

# Credits

- [Shi Tianshu](https://github.com/DogLooksGood): Original creator of parinfer-mode

## Original credits

- [shaunlebron](https://github.com/shaunlebron): Create [Parinfer](https://shaunlebron.github.io/parinfer/).
- [oakmac](https://github.com/oakmac): Bring Parinfer to Emacs with [parinfer-elisp](https://github.com/oakmac/parinfer-elisp).
- [tumashu](https://github.com/tumashu): Help me a lot in writing this plugin.
- [purcell](https://github.com/purcell) & [syohex](https://github.com/syohex): Advice and Tips for writing emacs plugin

# License

parinferlib.el from [parinfer-elisp](https://github.com/oakmac/parinfer-elisp), is licensed under the [ISC](https://github.com/oakmac/parinfer-elisp/blob/master/LICENSE.md).

The rest is licensed under the GPLv3.
