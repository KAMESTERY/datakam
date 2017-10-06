;; https://projectile.readthedocs.io/en/latest/configuration/
;; Projectile Configuration
((nil . ((eval . (setq projectile-project-root
                       (locate-dominating-file buffer-file-name
                                               ".dir-locals.el")))
         (eval . (setq compile-command
                       `(format "cd %s && make"
                                (locate-dominating-file buffer-file-name
                                                        ".dir-locals.el"))))
         (eval . (setq pyhome "~/miniconda3/envs/SLAPENV"))
         (eval . (pythonic-activate pyhome))
         (eval . (setq hy-mode-inferior-lisp-command (concat pyhome "/bin/hy")))
         (eval . (setenv "GOPATH"
                         (concat
                          (getenv "GOPATH") ":" projectile-project-root "slapman/build")))
         )))

