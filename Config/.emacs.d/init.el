;パッケージマネージャー用の設定を読み込み
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;---------------------
;不足するパッケージは自動的に追加インストールする
;---------------------
(require 'cl)

(defvar installing-package-list
  '(
    ;; ここに使っているパッケージを書く。
    shell-pop
    auto-complete
    auto-complete-clang-async
    foreign-regexp
    switch-window
    popwin
    helm
    ))

(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))



;ロードパスを設定
(setq load-path (cons "~/.emacs.d/site-lisp/" load-path))

;;; 対応する括弧を光らせる。
(show-paren-mode 1)

;;; emacs -nw で起動した時にメニューバーを消す
(if window-system (menu-bar-mode 1) (menu-bar-mode -1))

;;; モードラインに時間を表示する
(display-time)

;;; 現在の関数名をモードラインに表示
(which-function-mode 1)

;---------------------
;shift + 十字キーでバッファ切り替えが出来るようにする
;---------------------
(setq windmove-wrap-around t)
(windmove-default-keybindings)

;---------------------
;shell-pop設定
;---------------------
(require 'shell-pop)

;; ショートカットも好みで変更してください
(global-set-key [f8] 'shell-pop)

;---------------------
;popwin設定
;---------------------
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'bottom)

;---------------------
;switch-window設定
;---------------------
;; (install-elisp "http://www.emacswiki.org/emacs-en/download/switch-window.el")
; C-x o が dim:switch-window になる
; 通常のwindow切り替え関数はother-windowなので、必要な時はこっち側を呼ぶ。
(require 'switch-window) 
(global-set-key (kbd "C-x o") 'switch-window)
;(define-key global-map (kbd "C-t") 'other-window) ; C-t に other-window

;---------------------
;外部正規表現の設定
;---------------------
(cond ((require 'foreign-regexp nil t)
       (custom-set-variables
       '(foreign-regexp/regexp-type 'ruby)
       '(reb-re-syntax 'foreign-regexp))))

;.hファイルもc++modeで扱う
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;---------------------
;auto-completeの設定
;---------------------

;; auto-complete-clang 設定
(require 'auto-complete-config)
(ac-config-default)

;;補完キー指定
(ac-set-trigger-key "TAB")
;;ヘルプ画面が出るまでの時間（秒）
(setq ac-quick-help-delay 0.2)

;; auto-complete-clang-async
(require 'auto-complete-clang-async)

;C言語モードの設定
(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/local/bin/clang-complete")
;  (setq ac-clang-prefix-header "~/.emacs.d/site-lisp/pre.h.pch")
  (setq ac-clang-cflags
    (mapcar (lambda (item)(concat "-I" item))
        (split-string
         "
"
         )))

  ;(setq ac-clang-cflags (append '("-std=c++11") ac-clang-cflags))
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process)
  )


(defun my-ac-config ()
  (global-set-key "\M-/" 'ac-start)
  ;; C-n/C-p で候補を選択
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)

  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)


;---------------
;helm
;--------------
(global-set-key (kbd "C-x b") 'helm-mini)
(helm-mode 1)

