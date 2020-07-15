Lisp基础：

光标要放在表达式后面

C-j 在buffer调整计算结果

C-x C-e 显示在minibuffer区域

#### S表达式：

```lisp
(+ 2 2)
(+ 2 (+ 1 1))
(+ 3 (+ 1 2))
;;           ^ 光标放到这里
;; 按下`C-j' 就会输出 6
;; `C-j' 会在buffer中插入当前运算的结果
;; 而`C-xC-e' 则会在emacs最底部显示结果，也就是被称作"minibuffer"的区域

```

#### 变量：

```lisp
(setq my-name "Quosimodo")
;;C-x C-e 输出"Quosimodo"
;;定义变量 my-name 赋值 "Quosimodo"
```

#### 插入字符串：

```lisp
(insert "Hello")
(insert "Hello" " world!")
;; `C-xC-e' 输出 "Hello world!"
;; 你也可以用变量名来代替字符串
(insert "Hello, I am " my-name)
;; `C-xC-e' 输出 "Hello, I am Quosimodo"
```

#### 函数：

```lisp
;; 你可以把s式嵌入函数中
(defun hello () (insert "Hello, I am " my-name))
;; `C-xC-e' 输出 hello
;; 现在执行这个函数
(hello)
;; `C-xC-e' 输出 Hello, I am Quosimodo
;; 函数中空括号的意思是我们不需要接受任何参数
;; 但是我们不能一直总是用my-name这个变量
;; 所以我们现在使我们的函数接受一个叫做"name"的参数 
(defun hello (name) (insert "Hello " name))
;; `C-xC-e' 输出 hello

;; 现在我们调用这个函数，并且将"you"作为参数传递

(hello "you")
;; `C-xC-e' 输出 "Hello you"
```

#### 建立新的Buffer:

```lisp
;; 下面我们在新的窗口中新建一个名为 "*test*" 的buffer:
(switch-to-buffer-other-window "*test*")
;; `C-xC-e' 这时屏幕上会显示两个窗口，而光标此时位于*test* buffer内
```

#### 将s式子结合起来：

```lisp
;; 你可以用 `progn'命令将s式结合起来:
(progn
  (switch-to-buffer-other-window "*test*")
  (hello "you"))
;; `C-xC-e' 此时屏幕分为两个窗口，并且在*test* buffer中显示"Hello you"
;; 现在为了简洁，我们需要在每个s式后面都使用`C-xC-e'来执行，后面就不再说明了
;; 记得可以用过鼠标或者`C-xo'回到*scratch*这个buffer。
;; 清除当前buffer也是常用操作之一：
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "there"))
;; 也可以回到其他的窗口中
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "you")
  (other-window 1))
```

#### 将值和局部变量绑定：

```lisp
;; 你可以用 `let' 将一个值和一个局部变量绑定:
(let ((local-name "you"))
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello local-name)
  (other-window 1))
;; 这里我们就不需要使用 `progn' 了， 因为 `let' 也可以将很多s式组合起来。
```

#### 格式化输出：

```lisp
;; 格式化字符串的方法：
(format "Hello %s!\n" "visitor")

;; %s 是字符串占位符，这里被"visitor"替代.
;; \n 是换行符。

;; 现在我们用格式化的方法再重写一下我们的函数:
(defun hello (name)
  (insert (format "Hello %s!\n" name)))

(hello "you")

;; 我们再用`let'新建另一个函数:
(defun greeting (name)
  (let ((your-name "Bastien"))
    (insert (format "Hello %s!\n\nI am %s."
                    name       ; the argument of the function
                    your-name  ; the let-bound variable "Bastien"
                    ))))

;; 之后执行:
(greeting "you")

;; 有些函数可以和用户交互:
(read-from-minibuffer "Enter your name: ")

;; 这个函数会返回在执行时用户输入的信息

;; 现在我们让`greeting'函数显示你的名字:
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "Enter your name: ")))
    (insert (format "Hello!\n\nI am %s and you are %s." from-name your-name))))

(greeting "Bastien")

;; 我们让结果在另一个窗口中显示:
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "Enter your name: ")))
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (insert (format "Hello %s!\n\nI am %s." your-name from-name))
    (other-window 1)))

;; 测试一下：
(greeting "Bastien")
```

#### quote:

```lisp
;;quote 的意思是不要执行后面的内容，返回它原本的内容
(print '(+ 1 1)) ;; -> (+ 1 1)
(print (+ 1 1))  ;; -> 2
```

#### loop:

```lisp
;;这段配置文件中用到了 loop for ... in ，它来自 cl 即 Common Lisp 扩展。 for , in, collect 均为 cl-loop 中的 保留关键字。下面是一些简单的 cl-loop 的使用示例：
;; 遍历每一个缓冲区（Buffer）
(cl-loop for buf in (buffer-list)
	 collect (buffer-file-name buf))

;; 寻找 729 的平方根（设置最大为 100 为了防止无限循环）
(cl-loop for x from 1 to 100
	 for y = (* x x)
	 until (>= y 729)
	 finally return (list x (= y 729)))
```

