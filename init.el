;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)


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

;; 输入字符替换选中部分的文字
(delete-selection-mode 1)
;; 高亮当前行
(global-hl-line-mode 1)
;; 高亮括号匹配
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;; (show-paren-mode 1)
;; 自动生成括号匹配
(smartparens-global-mode t)
;; 代码块注释和反注释
(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region)
;; 自动加载更新内容
(global-auto-revert-mode 1)
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
		hungry-delete
		swiper
		counsel
		smartparens
		;; --- Major Mode ---
		js2-mode
		;; --- Minor Mode ---
		nodejs-repl
		exec-path-from-shell
		;; --- Themes ---
		monokai-theme
		;; solarized-theme
		) "Default packages")

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

;; 设置默认 Org Agenda 文件目录
(setq org-agenda-files '("~/org"))
;; 设置 org-agenda 打开快捷键
(global-set-key (kbd "C-c a") 'org-agenda)
