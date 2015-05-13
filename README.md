mutant.el
=========

An Emacs interface for the [Mutant](https://github.com/mbj/mutant) testing tool.

Installation:
-------------

As I've just started to develop it, **mutant.el** hasn't been published yet
on any package repository (though I plan to do it soon), so I suggest you
to do the following:

First, install the package `dash` using `install-package`.

Then clone the project's repository:

    $ git clone git://github.com/p-lambert/mutant.el.git ~/.emacs.d/vendor/mutant

Finally, add this to your init file:

```el
(add-to-list 'load-path "~/.emacs.d/vendor/mutant")
(require 'mutant)
```

**mutant.el requires Emacs 24.4 or above**

Commands:
---------

You can change the default keymap prefix `C-c .` by doing:

```el
(setq mutant-keymap-prefix (kbd "C-c C-."))
```

###### `mutant-check-file`

Run `Mutant` over the file on the current buffer.

Default keybinding: <kbd>C-c . f</kbd>

###### `mutant-check-custom`

Prompts the user for a custom `match-expression` and then run `Mutant` over it.

Default keybinding: <kbd>C-c . c</kbd>

Dired integration:
------------------

You can also launch `Mutant` from Dired buffers. To do that, add this:

```el
(add-hook 'dired-mode-hook 'mutant-dired-mode)
```

By doing so, you may mark files and then press <kbd>C-c . f</kbd> which will
call `mutant-check-from-dired`.