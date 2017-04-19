;;; -*- lexical-binding: t -*-

;;; NOTE: this is completely broken right now!!! that's fine, see README

(defcustom bind-do-throw nil
  "Whether to throw errors in `bind' (this is dereferenced at macro
expansion time!). If this is nil, `bind' will generate a sexp that
returns nil on error. Can be temporarily `let'-bound.")
(defcustom bind-extra-vars nil
  "Whether to bind the `:extra' variables in `bind' (this is
dereferenced at macro expansion time!). If this is nil, additional variables are
not bound. Can be temporarily `let'-bound.")

(defun bind-validate-plist (pl &optional pred)
  (cl-loop
   with pred = (cond ((functionp pred) pred)
                     (pred #'symbolp)
                     (t #'keywordp))
   for (k v .) on pl by cddr
   unless (funcall pred k) return nil
   finally return t))

(cl-defmacro bind-in-error (args &rest sexps)
  nil)

(cl-defmacro bind-error-subprocess (&rest args)
  `(bind-in-error ,args
   ;; $ before with only name assigns it to that key
   ($stdout> $standard-output (buffer-string))
   ($stderr> $standard-error (buffer-string))
   ;; can also access assignments by their index (0 and 3 have no names, but
   ;; can say '$@1 to get $stdout, for example)

   ;; any names you assign are the default output expression, unless final expr
   ;; has no assignments (just a single sexp), as here
   (!subprocess cmd:cmd $stdout $stderr)
   ;; (throw 'bind-subprocess-error
   ;;  (list :cmd <cmd used> :stdout $stdout :stderr $stderr))
   ;; $stdout and $stderr are dereferenced
   ))

(defvar bind-in--cur-arg nil)
(defvar bind-in--cur-sym-name-arg nil)
(defvar bind-in--cur-$-var-alist nil)

(defun bind-in--format-sym (fmt &rest args)
  (make-symbol (apply #'format (cons fmt args))))

(defun bind-in--concat-symbols (&rest symbols)
  (make-symbol
   (mapconcat
    #'identity (cl-mapcar #'symbol-name symbols) "")))

(defconst bind-in--symbol-regex "[[:alnum:]\\-]+")

(defun bind-in--build-metacharacters-regex ()
  (or bind-in--metacharacters-regex
      (format "\\(%s\\)\\(%s\\)?"
              (regexp-opt-charset
               (cl-loop for e in (append bind-in-basic-metacharacters-alist
                                         bind-in-user-metacharacters-alist)
                        for sn = (symbol-name (car e))
                        if (not (= 1 (length sn))) do
                        (error "invalid metachar '%s'; should be one character"
                               sn)
                        collect (string-to-char sn)))
              bind-in--symbol-regex)))

(cl-defmacro bind-in--deconstruct-symbol (sym &rest reduced)
  )

(cl-defmacro bind-in--deconstruct-expr (clause &rest reduced)
  (pcase clause
    (`(,first . ,others)
     `(bind-in--deconstruct-expr
       first ,@`(bind-in--deconstruct-expr ,@others)))
    ((pred symbolp)
     `(bind-in--deconstruct-symbol ,clause ,@reduced))
    (_ (error "???"))))

(defun bind-in-accept (&rest _) t)

(defconst bind-in-basic-metacharacters-alist
  '((>)
    (<)
    (|)
    (~)
    (/ . bind-in--cur-sym-name-arg)
    (:)
    ($)
    (=)
    (^)
    (@))
  "Any alphanumeric or hyphen characters ([[:alnum:]\\-]+) will be substituted
for the value of `bind-in--cur-sym-name-arg', which is represented by (???)!")

(defvar bind-in--metacharacters-regex nil)

(defcustom bind-in-user-metacharacters-alist nil
  "Any metacharacters to add to types for `bind-in'."
  :type '(alist :key-type symbol :value-type list))

(defconst bind-in-basic-type-alist
  ;; see above usage of $@ (which grows as we traverse every argspec or sexp --
  ;; we can use $* to get the index of the current and $% for all the
  ;; unevaluated argspecs the macro receieved at once, and $- for the rest of
  ;; the bindings!). make sure we only bind these variables if the user uses
  ;; them, though, and also make a macro that turns off or changes the way those
  ;; are denoted temporarily. can refer to vars as =buf, even if that buf was
  ;; assigned a name (e.g. =buf$this-buf), but if has multiple layered
  ;; assignments in macro, will only have the most recent
  `((~and =list)
    (~or =plist-car)
    (=buf |buffer-live-p (> with-current-buffer =. /body))
    (=proc |process-live-p =buf<process-buffer)
    (=mark |markerp =buf<mark-buffer)
    (=sym |symbolp)
    (=interned =sym|intern-soft)
    (=fsym =interned|fboundp
           +fun<symbol-function)
    (=fun |functionp)
    (=var =interned|boundp =val<symbol-value)
    (=val |@accept)
    (=int |integerp)
    (=str |stringp)
    (=list |listp)
    (=expr <@deconstruct-expr)
    (=key =sym|keywordp)
    (=bool |booleanp)
    (=plist =list
            +bool$colon
            (@validate-plist =. $colon))
    (=plist-car =sym$head<car
                =plist+colon<cdr)
    (=file =str|file-exists-p
           =file$expanded<expand-file-name
           =file$relative<file-relative-name)
    (=directory =file|file-directory-p)
    (=cmd =str!
          >with-temp-buffer
          =buf$stdout
          =buf$stderr
          =int<$exit-code^call-process-shell-command^zerop!@subprocess
          (! (zerop =.$exit-code)
             ())
          $validate (($expr (zerop exit-code)
                                     $if-error ($output (buffer-string buf)
                                                        $error ))))
    (=exe $parent str
          $validate (or

                    file-executable-p))
    (=argv $validate (($priority before $pred listp)
                     ($type exe! $get car)
                     ($type bool $name +sync)
                     ()))))

(defcustom bind-in-user-type-alist nil
  "Alist that `bind-in' uses to build its output sexp.")

(defvar-local bind-in-local-type-overrides nil)

(defun bind-in-get-table ()
  (append
   bind-basic-type-alist bind-in-user-type-alist bind-in-local-type-overrides))

(cl-defmacro bind-in (&rest sexps)
  (declare (indent 0))
  (let ((bind-in--metacharacters-regex
         (or bind-in--metacharacters-regex ))))
  nil)

(provide 'bind-in)
