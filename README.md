iicl
====
iicl -- это нода для ii-сетей, написанная на Common Lisp.

Зависимости
-----------
  * SBCL (>= 1.2.7)
  * quicklisp

Работа на SBCL < 1.2.7 возможна, но не гарантируется.

Дополнительные зависимости
--------------------------
Эти зависимости будут удовлетворены автоматически с помощью quicklisp.

  * split-sequence
  * hunchentoot

Установка
---------
Для работы клиента необходимо установить реализацию lisp [SBCL](http://sbcl.org/platform-table.html).

После установки SBCL необходимо скачать менеджер пакетов [Quicklisp](https://www.quicklisp.org/beta/).

Установка quicklisp производится очень просто

```cl
* (load "quicklisp.lisp")
* (quicklisp-quickstart:install)
* (ql:add-to-init-file)
```

Для установки в каталог отличный от ~/quicklisp или настройки параметров прокси-сервера необходимо прочитать

```cl
* (quicklisp-quickstart:help)
```

После установки quicklisp и добавления его в автозагрузку SBCL можно выйти из REPL.

```cl
* (quit)
```