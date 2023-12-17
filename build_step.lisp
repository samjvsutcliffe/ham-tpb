(restrict-compiler-policy 'speed 0 0)
(restrict-compiler-policy 'debug 3 3)
(restrict-compiler-policy 'safety 3 3)
;(setf *block-compile-default* t)
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
