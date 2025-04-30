(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(dap-netcore-install-dir
   "c:/Users/BRIO/AppData/Roaming/.emacs.d/.cache/lsp/netcoredbg")
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(sharper js2-mode rg git-gutter yaml-mode sequential-yank ag smartparens dap-mode lsp-treemacs lsp-ivy lsp-ui lsp-mode general treemacs helpful magit flycheck company multiple-cursors counsel-projectile projectile which-key rainbow-delimiters all-the-icons-dired all-the-icons telephone-line ivy-avy counsel ivy-rich ivy command-log-mode use-package gcmh))
 '(tool-bar-mode nil)
 '(treemacs-python-executable "C:\\Program Files\\Python312\\python.exe"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "outline" :slant normal :weight regular :height 120 :width normal)))))

;;; On Windows, commands run by flycheck may have CRs (\r\n line endings).
;;; Strip them out before parsing.
(defun flycheck-parse-output (output checker buffer)
  "Parse OUTPUT from CHECKER in BUFFER.

OUTPUT is a string with the output from the checker symbol
CHECKER.  BUFFER is the buffer which was checked.

Return the errors parsed with the error patterns of CHECKER."
  (let ((sanitized-output (replace-regexp-in-string "\r" "" output))
        )
    (funcall (flycheck-checker-get checker 'error-parser) sanitized-output checker buffer)))
