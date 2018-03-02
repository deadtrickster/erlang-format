# erlang-format

erlang-format command for erlang-mode. Based on https://github.com/anildigital/mix-format, powered by https://github.com/erlang/sourcer.

```emacs-lisp


(with-eval-after-load 'erlang-mode
  (bind-key [f12] (lambda ()
                    (interactive)
                    (delete-trailing-whitespace)
                    (erlang-format))
            erlang-mode-map))

(require 'erlang-format)

(setq erlfmt-erlang-ls "~/bin/erlang_ls")

(add-hook 'erlang-format-hook '(lambda ()
                              (if (projectile-project-p)
                                  (let ((sourcer-config-filename (concat (projectile-project-root) "/.sourcer.config")))
                                    (when (file-exists-p sourcer-config-filename)
                                      (setq erlfmt-args (list "--config" sourcer-config-filename))))
                                (setq erlfmt-args nil))))
```
