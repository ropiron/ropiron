(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(require 'package)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(package-initialize)

(require 'mozc)
(load-library "mozc")
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(global-set-key [zenkaku-hankaku] 'toggle-input-method)
(prefer-coding-system 'utf-8)
(add-to-list 'load-path "~/.emacs.d/elisp/mozc-el-extensions")

(require 'mozc-im)
(require 'mozc-popup)
(require 'mozc-cursor-color)
(require 'wdired)

(setq default-input-method "japanese-mozc-im")
(setq mozc-candidate-style 'popup)
(setq mozc-cursor-color-alist '((direct        . "red")
                                (read-only     . "yellow")
                                (hiragana      . "green")
                                (full-katakana . "goldenrod")
                                (half-ascii    . "dark orchid")
                                (full-ascii    . "orchid")
                                (half-katakana . "dark goldenrod")))

(blink-cursor-mode 0)

(defun enable-input-method (&optional arg interactive)
  (interactive "P\np")
  (if (not current-input-method)
      (toggle-input-method arg interactive)))

(defun disable-input-method (&optional arg interactive)
  (interactive "P\np")
  (if current-input-method
      (toggle-input-method arg interactive)))

(defun isearch-enable-input-method ()
  (interactive)
  (if (not current-input-method)
      (isearch-toggle-input-method)
    (cl-letf (((symbol-function 'toggle-input-method)
               (symbol-function 'ignore)))
      (isearch-toggle-input-method))))

(defun isearch-disable-input-method ()
  (interactive)
  (if current-input-method
      (isearch-toggle-input-method)
    (cl-letf (((symbol-function 'toggle-input-method)
               (symbol-function 'ignore)))
      (isearch-toggle-input-method))))

(global-set-key (kbd "C-o") 'toggle-input-method)
(define-key isearch-mode-map (kbd "C-o") 'isearch-toggle-input-method)
(define-key wdired-mode-map (kbd "C-o") 'toggle-input-method)

(global-set-key (kbd "C-<f1>") 'disable-input-method)
(define-key isearch-mode-map (kbd "C-<f1>") 'isearch-disable-input-method)
(define-key wdired-mode-map (kbd "C-<f1>") 'disable-input-method)

;; (global-set-key (kbd "C-j") 'disable-input-method)
;; (define-key isearch-mode-map (kbd "C-j") 'isearch-disable-input-method)
;; (define-key wdired-mode-map (kbd "C-j") 'disable-input-method)

(global-set-key (kbd "C-<f2>") 'enable-input-method)
(define-key isearch-mode-map (kbd "C-<f2>") 'isearch-enable-input-method)
(define-key wdired-mode-map (kbd "C-<f2>") 'enable-input-method)

;; (global-set-key (kbd "C-o") 'enable-input-method)
;; (define-key isearch-mode-map (kbd "C-o") 'isearch-enable-input-method)
;; (define-key wdired-mode-map (kbd "C-o") 'enable-input-method)

(defvar-local mozc-im-mode nil)
(add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
(add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil)))
(advice-add 'mozc-cursor-color-update
            :around (lambda (orig-fun &rest args)
                      (let ((mozc-mode mozc-im-mode))
                        (apply orig-fun args))))

;; isearch
(add-hook 'isearch-mode-hook (lambda () (setq im-state mozc-im-mode)))
(add-hook 'isearch-mode-end-hook
          (lambda ()
            (unless (eq im-state mozc-im-mode)
              (if im-state
                  (activate-input-method default-input-method)
                (deactivate-input-method)))))

;; wdired IME Off
(advice-add 'wdired-finish-edit
            :after (lambda (&rest args)
                     (deactivate-input-method)))

;; Windowsにおけるフォントの設定（Consolasとメイリオ）
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :family "Consolas" :height 110)
  (set-fontset-font 'nil 'japanese-jisx0208
                    (font-spec :family "メイリオ"))
  (add-to-list 'face-font-rescale-alist
               '(".*メイリオ.*" . 1.08))
  )

;; GNU/Linuxにおけるフォントの設定（IncosolataとIPA exGothic）
(when (eq system-type 'gnu/linux)
  ;; Fontの設定
  (set-face-font 'default "Hack-12:bold")
  ;;(set-frame-font "Inconsolata-14")
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "IPAExGothic"))
  )

;; anythingの設定
(setq
 browse-url-browser-function 'eww-browse-url ; Use eww as the default browser
 shr-use-fonts  nil                          ; No special fonts
 shr-use-colors nil                          ; No colours
 shr-indentation 2                           ; Left-side margin
 shr-width 70                                ; Fold text to 70 columns
 eww-search-prefix "https://wiby.me/?q=")    ; Use another engine for searching
(setq browse-url-mosaic-program nil)
(setq browse-url-galeon-program nil)
(setq browse-url-netscape-program nil)

(add-to-list 'load-path "~/.emacs.d/elisp/anything")
(require 'anything-config)
(setq anything-enable-shortcuts 'prefix)
(define-key anything-map (kbd "@") 'anything-select-with-prefix-shortcut)
(global-set-key (kbd "C-x b") 'anything-mini)


;; Orgの設定

(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(
	("i" "Idea" entry (file+headline "~/.emacs.d/org-dir/idea.org" "Idea")
	 "* %? %U %i")
	("r" "Remember" entry (file+headline "~/.emacs.d/org-dir/remember.org" "Remember")
	 "* %? %U %i")
	("m" "Memo" entry (file+headline "~/.emacs.d/org-dir/memo.org" "Memo")
	 "* %? %U %i")
	("t" "Task" entry (file+headline "~/.emacs.d/org-dir/task.org" "Task")
	 "** TODO %? \n   SCHEDULED: %^t \n")))

(setq org-refile-targets
      (quote (("~/.emacs.d/org-dir/archives.org" :level . 1)
	      ("~/.emacs.d/org-dir/remember.org" :level . 1)
	      ("~/.emacs.d/org-dir/memo.org" :level . 1)
	      ("~/.emacs.d/org-dir/task.org" :level . 1))))

(defun show-org-buffer (file)
  "Show an org-file FILE on the current buffer."
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat "~/.emacs.d/org-dir/" file))))


(global-set-key (kbd "C-M-^") '(lambda () (interactive)
                                 (show-org-buffer "memo.org")))


(global-set-key (kbd "\C-ci") '(lambda () (interactive)
                                 (find-file "~/.emacs.d/init.el")))


;;===========================================
;;  絶対必要な基本設定
;;===========================================
;;----
;; スタートアップページを表示しない
;;----
(setq inhibit-startup-message t)

;;----
;; 行番号表示
;;----
;;(global-linum-mode t)
;;(setq linum-format "%5d ")
(global-nlinum-mode t)
(setq nlinum-format "%5d ")
;;----
;; カラム番号
;;----
(column-number-mode t)

;;----
;; ビープ音を消す
;;----
(setq ring-bell-function 'ignore)

;;----
;; カーソル行に下線を表示
;;----
(setq hl-line-face 'underline)
(global-hl-line-mode)

;;----
;; 対応する括弧を強調表示
;;----
(show-paren-mode t)

;;----
;; 時計表示
;;----
;; 不採用    ;; 時間を表示
;; 不採用    (display-time)
(setq display-time-day-and-date t)  ;; 曜日・月・日
(setq display-time-24hr-format t)   ;; 24時表示
(display-time-mode t)

;;----
;; TABの表示幅
;;----
(setq-default tab-width 4)

;;----
;; ファイルサイズ表示
;;----
(size-indication-mode t)

;;----
;; ツールバーを非表示
;; M-x tool-bar-mode で表示非表示を切り替えられる
;;----
(tool-bar-mode -1)

;;----
;; タイトルバーにフルパス表示
;;----
(setq frame-title-format "%f")

;;----
;; カラーテーマ
;;----
(load-theme 'deeper-blue t)

;;----
;; 全角空白とタブを可視化
;; 参考：http://d.hatena.ne.jp/t_ume_tky/20120906/1346943019
;;----
;; タブや全角空白などを強調表示
(global-whitespace-mode 1)
;; whitespace-mode の 色設定
;;http://ergoemacs.org/emacs/whitespace-mode.html
(require 'whitespace)
(setq whitespace-style 
      '(face tabs tab-mark spaces space-mark newline newline-mark))
(setq whitespace-display-mappings
      '(
        (tab-mark   ?\t     [?\xBB ?\t])  ; タブ
        (space-mark ?\u3000 [?□])        ; 全角スペース
;        (space-mark ?\u0020 [?\xB7])      ; 半角スペース
        (newline-mark ?\n   [?$ ?\n])     ; 改行記号
        ) )
(setq whitespace-space-regexp "\\([\x0020\x3000]+\\)" )
;正規表現に関する文書。 Emacs Lispには、正規表現リテラルがないことへの言及
;http://www.mew.org/~kazu/doc/elisp/regexp.html
;
;なぜか、全体をグループ化 \(\) しておかないと、うまくマッチしなかった罠
;
(set-face-foreground 'whitespace-space "DimGray")
(set-face-background 'whitespace-space 'nil)
;(set-face-bold-p 'whitespace-space t)

(set-face-foreground 'whitespace-tab "DimGray")
(set-face-background 'whitespace-tab "nil")

(set-face-foreground 'whitespace-newline  "DimGray")
(set-face-background 'whitespace-newline 'nil)


;;===========================================
;; キーボード操作系
;;===========================================
;;----
;; キーの入れ替えの例
;;----
;; global-set-keyはdefine-keyのラッパーなので、どっちを使ってもOK
(define-key global-map (kbd "C-t") 'other-window)
(global-set-key (kbd "") 'other-window)


;;----
;; ウィンドウ切り替え
;; SはShiftキーのこと
;; 参考：http://qiita.com/saku/items/6ef40a0bbaadb2cffbce
;;----
;; (defun other-window-or-split (val)
;;   (interactive)
;;   (when (one-window-p)
;;     (split-window-horizontally) ;split horizontally 縦分割にしたい場合はこちら
;; ;;    (split-window-vertically) ;split vertically   横分割にしたい場合はこちら
;;   )
;;   (other-window val))
;; (global-set-key (kbd "") (lambda () (interactive) (other-window-or-split 1)))
;; (global-set-key (kbd "") (lambda () (interactive) (other-window-or-split -1)))

;;----
;; 折り返しトグルコマンド
;;----
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)

(require 'cl-lib)

;; 改行文字の文字列表現
(set 'eol-mnemonic-dos "(CRLF)")
(set 'eol-mnemonic-unix "(LF)")
(set 'eol-mnemonic-mac "(CR)")
(set 'eol-mnemonic-undecided "(?)")

;; 文字エンコーディングの文字列表現
(defun my-coding-system-name-mnemonic (coding-system)
  (let* ((base (coding-system-base coding-system))
         (name (symbol-name base)))
    (cond ((string-prefix-p "utf-8" name) "U8")
          ((string-prefix-p "utf-16" name) "U16")
          ((string-prefix-p "utf-7" name) "U7")
          ((string-prefix-p "japanese-shift-jis" name) "SJIS")
          ((string-match "cp\\([0-9]+\\)" name) (match-string 1 name))
          ((string-match "japanese-iso-8bit" name) "EUC")
          (t "???")
          )))

(defun my-coding-system-bom-mnemonic (coding-system)
  (let ((name (symbol-name coding-system)))
    (cond ((string-match "be-with-signature" name) "[BE]")
          ((string-match "le-with-signature" name) "[LE]")
          ((string-match "-with-signature" name) "[BOM]")
          (t ""))))

(defun my-buffer-coding-system-mnemonic ()
  "Return a mnemonic for `buffer-file-coding-system'."
  (let* ((code buffer-file-coding-system)
         (name (my-coding-system-name-mnemonic code))
         (bom (my-coding-system-bom-mnemonic code)))
    (format "%s%s" name bom)))

;; `mode-line-mule-info' の文字エンコーディングの文字列表現を差し替える
(setq-default mode-line-mule-info
              (cl-substitute '(:eval (my-buffer-coding-system-mnemonic))
                             "%z" mode-line-mule-info :test 'equal))

(setq eshell-visual-commands
  '("vi"                                ; what is going on??
    "screen" "top"                      ; ok, a valid program...
    "less" "more"                       ; M-x view-file
    "lynx" "ncftp"                      ; w3.el, ange-ftp
    "pine" "tin" "trn" "elm")           ; GNUS!!
  )

(require 'em-term)
(defun eshell-exec-visual (&rest args)
  (apply 'start-process
         "eshell-exec-visual" " eshell-exec-visual"
         "urxvt" "-title" "eshell-exec-visual" "-e" args)
  nil)
;; OS起動後Org-captureを実行する。
;;(org-capture)
