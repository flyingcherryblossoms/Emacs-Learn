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

;; 关闭缩进
;; (electric-indent-mode -1)

;;文本模式语法高亮
(require 'org)
(setq org-src-fontify-natively t)

;;windows修改默认字体为中文字体防止解决卡顿问题
(set-default-font "-outline-萍方0-normal-normal-normal-sans-20-*-*-*-p-*-iso8859-1")

;;更改鼠标指针样式
(setq-default cursor-type 'bar)

;;关闭自动备份
(setq make-backup-files nil)

;;菜单栏显示打开最近打开文件
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)
;;这个快捷键绑定可以用 counsel 代替
;;(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; 更改显示字体大小 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
;; (set-face-attribute 'default nil :height 115)

;;设置窗口位置为屏库左上角(0,0)
;; (set-frame-position (selected-frame) 500 200)

;;默认全屏
;;(setq initial-frame-alist (quote ((fullscreen . maximized))))

;;输入字符替换选中部分的文字
(delete-selection-mode 1)

;;高亮当前行
(global-hl-line-mode 1)

;;高亮括号匹配
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;;安装主题
;; (add-to-list 'my/packages 'monokai-theme)
;; (load-theme 'monokai-theme 1);;每次打开编辑器自动加载主题

;;打开自动补全
(global-company-mode 1)

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)

