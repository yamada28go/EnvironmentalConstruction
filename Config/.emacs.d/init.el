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
    hlinum
    anzu
    volatile-highlights
    js2-mode
    ac-js2
    js2-refactor
    powerline
    tss
    typescript
    ))

;;テーマを設定
;;デフォルトのターミナルでは、色が8色までしか使えないため、
;;以下のような感じで、ターミナルを騙す
;;TERM=xterm-256color emacs -nw
(load-theme 'tsdh-dark t)

(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))

;;; 対応する括弧を光らせる。
(show-paren-mode 1)

;;; emacs -nw で起動した時にメニューバーを消す
(if window-system (menu-bar-mode 1) (menu-bar-mode -1))

;;; モードラインに時間を表示する
(display-time)

;;;現在の行をハイライトする
(global-hl-line-mode t)

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

;; 自動補完を無効
(custom-set-variables '(helm-ff-auto-update-initial-value nil))

;; For find-file etc.
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

;---------------
;行表示
;--------------
(require 'hlinum)
(custom-set-variables
 '(global-linum-mode t))

;---------------
;検索箇所の強化表示
;--------------
(require 'anzu)
(global-anzu-mode +1)

;---------------
;操作箇所の強化表示
;--------------
(require 'volatile-highlights)
(volatile-highlights-mode t)

;---------------
;Js2mode
;--------------
;本来は、autoloadで読めるはずであるが、
;正しく読めないのでrequireしておく
(require 'js2-mode)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;---------------
;同一名称のバッファをわかりやすい名称に調整する
;--------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;---------------
;空白行を可視化
;--------------
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
			 trailing       ; 行末
			 tabs           ; タブ
			 spaces         ; スペース
			 empty          ; 先頭/末尾の空行
			 space-mark     ; 表示のマッピング
			 tab-mark
			 ))

(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
	;; WARNING: the mapping below has a problem.
	;; When a TAB occupies exactly one column, it will display the
	;; character ?\xBB at that column followed by a TAB which goes to
	;; the next TAB column.
	;; If this is a problem for you, please, comment the line below.
	(tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")

;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))

(global-whitespace-mode 1)

(defvar my/bg-color "#232323")
(set-face-attribute 'whitespace-trailing nil
		    :background my/bg-color
		    :foreground "DeepPink"
		    :underline t)
(set-face-attribute 'whitespace-tab nil
		    :background my/bg-color
		    :foreground "LightSkyBlue"
		    :underline t)
(set-face-attribute 'whitespace-space nil
		    :background my/bg-color
		    :foreground "GreenYellow"
		    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
		    :background my/bg-color)

;;-----------
;;powerlineの設定
;;-----------
(require 'powerline)

(set-face-attribute 'mode-line nil
		    :foreground "#fff"
		    :background "#FF0066"
		    :box nil)

(set-face-attribute 'powerline-active1 nil
		    :foreground "#fff"
		    :background "#FF6699"
		    :inherit 'mode-line)

(set-face-attribute 'powerline-active2 nil
		    :foreground "#000"
		    :background "#ffaeb9"
		    :inherit 'mode-line)

(powerline-default-theme)

;---------------
;TypeScript
;--------------

;(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
;(autoload 'typescript-mode "TypeScript" "Major mode for editing typescript." t)

;; 同梱されたtypescript.elを使う場合
;(require 'typescript)
;(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; (require 'tss)

;; ;; キーバインド
;; (setq tss-popup-help-key "C-:")
;; (setq tss-jump-to-definition-key "C->")
;; (setq tss-implement-definition-key "C-c i")

;; ;; 必要に応じて適宜カスタマイズして下さい。以下のS式を評価することで項目についての情報が得られます。
;; ;; (customize-group "tss")

;; ;; 推奨設定を行う
;; (tss-config-default)
