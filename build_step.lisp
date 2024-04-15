(restrict-compiler-policy 'speed 3 3)
(restrict-compiler-policy 'debug 0 0)
(restrict-compiler-policy 'safety 0 0)
(setf *block-compile-default* t)
(setf *load-verbose* nil)
(setf *load-print* nil)
(setf *compile-verbose* nil)
(setf *compile-print* nil)
(ql:quickload :cl-mpm-worker)
(in-package :cl-mpm-worker)
(ql:quickload :cl-mpm)
(ql:quickload :cl-mpm/setup)
(ql:quickload :cl-mpm/particle)
(ql:quickload "magicl")
(ql:quickload :cl-mpm/mpi)
(ql:quickload :parse-float)
(ql:quickload "cl-mpm/examples/tpb")

(sb-ext:save-lisp-and-die
   "mpi-worker"
    :executable t
    :toplevel #'main
    :save-runtime-options t)
(uiop:quit)
