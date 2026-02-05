(sb-ext:restrict-compiler-policy 'speed 3 3)
(sb-ext:restrict-compiler-policy 'debug 0 0)
(sb-ext:restrict-compiler-policy 'safety 0 0)
(setf *block-compile-default* t)
;(sb-ext:restrict-compiler-policy 'speed 0 0)
;(sb-ext:restrict-compiler-policy 'debug 3 3)
;(sb-ext:restrict-compiler-policy 'safety 3 3)
;(setf *block-compile-default* t)

(ql:quickload :cl-mpm/examples/virtual-stress/thick-cylinder)
(in-package :cl-mpm/examples/virtual-stress/thick-cylinder)

(ql:quickload :local-time)
(ql:quickload :parse-float)

(setf cl-mpm/settings::*optimise-setting* cl-mpm/settings::*optimise-speed*)

(defun main (&optional args)
	(load "thick-wall.lisp"))

(sb-ext:save-lisp-and-die
   "worker"
    :executable t
    :toplevel #'main
    :compression nil
    :save-runtime-options t
    )
(uiop:quit)
