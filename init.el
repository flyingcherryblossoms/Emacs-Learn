;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f3>") 'open-init-file)

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)
;; 关闭文件滑动控件
(scroll-bar-mode -1)
;; 显示行号
(global-linum-mode 1)
;; 更改光标的样式
(setq cursor-type 'bar)
;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)
;; 文本模式语法高亮
(require 'org)
(setq org-src-fontify-natively t)
;; 更改光标样式为普通的条状
(setq-default cursor-type 'bar)
;; 关闭自动备份
(setq make-backup-files nil)
;; 菜单栏显示打开最近打开文件
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)
;; 绑定打开最近文件的快捷键
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
;; windows修改默认字体为中文字体防止解决卡顿问题
(set-default-font "-outline-萍方0-normal-normal-normal-sans-20-*-*-*-p-*-iso8859-1")
;; 更改显示字体大小 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
;; (set-face-attribute 'default nil :height 115)
;; 设置窗口位置为屏库左上角(0,0)
(set-frame-position (selected-frame) 0 0)
;; 启动窗口大小
(set-frame-width (selected-frame) 140)
(set-frame-height (selected-frame) 30)
;; 默认全屏
(setq initial-frame-alist (quote ((fullscreen . maximized))))
;; 设置默认 Org Agenda 文件目录
(setq org-agenda-files '("~/org"))
;; 设置 org-agenda 打开快捷键
(global-set-key (kbd "C-c a") 'org-agenda)
;; 输入字符替换选中部分的文字
(delete-selection-mode 1)
;; 高亮当前行
(global-hl-line-mode 1)
;; 高亮括号匹配
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;; (show-paren-mode 1)
;; 代码块注释和反注释
(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region)
;; 自动加载更新内容
(global-auto-revert-mode t)
;; 使用 'y/n' 代替 'yes/no'
(fset 'yes-or-no-p 'y-or-n-p)

;; 下面部分为需要安装的插件
;; 添加清华Elpa镜像源
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; 初始化包
(package-initialize)
 ;; cl - Common Lisp Extension
 (require 'cl)
;; Add Packages
 (defvar my/packages '(
		;; --- Auto-completion ---
		company
		;; --- Better Editor ---
		;; smex
		hungry-delete
		swiper
		counsel
		smartparens
		popwin
		;; --- Major Mode ---
		js2-mode
		;; --- Minor Mode ---
		nodejs-repl
		;; exec-path-from-shell
		;; --- Themes ---
		monokai-theme
		;; solarized-theme
		) "Default packages")
;; 将自定义的包设置为默认的包
 (setq package-selected-packages my/packages)

 (defun my/packages-installed-p ()
     (loop for pkg in my/packages
	   when (not (package-installed-p pkg)) do (return nil)
	   finally (return t)))

 (unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
	 (package-install pkg))))

 ;; Find Executable Path on OS X，用于当在MacOS上找不到路径时问题的解决办法
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

;; 自动生成括号匹配
(require 'smartparens-config)
(smartparens-global-mode t)
;; 打开自动补全
(global-company-mode t)
;; 加载主题
(add-to-list 'my/packages 'monokai-theme)
(load-theme 'monokai 1)
;; 配置不同类型文件的Major Mode
;; auto-mode-alist用于正则表达式匹配
(setq auto-mode-alist
  (append
   '(("\\.js\\'" . js2-mode))
   ;; File name (within directory) starts with a dot.
   '(("/\\.[^/]*\\'" . fundamental-mode)
     ;; File name has no dot.
     ("/[^\\./]*\\'" . fundamental-mode)
     ;; File name ends in ‘.C’.
     ("\\.C\\'" . c++-mode))
   auto-mode-alist))

;; smex，增强M-x
;; (require 'smex)  ; Not needed if you use package.el
;; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                    ; when Smex is auto-initialized on its first run.
;; (global-set-key (kbd "M-x") 'smex)

;; swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.05)
 '(company-minimum-prefix-length 3)
 '(package-selected-packages
   (quote
    (popwin company hungry-delete swiper counsel smartparens js2-mode nodejs-repl monokai-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-external-variable ((t (:foreground "SlateGray3")))))

;; 方便关闭的查询窗口
(require 'popwin)
(popwin-mode t)
