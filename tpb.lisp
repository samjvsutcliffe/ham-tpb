(setf *load-verbose* nil)
(setf *load-print* nil)
(setf *compile-verbose* nil)
(setf *compile-print* nil)
(ql:quickload :parse-float)
(ql:quickload :cl-mpm/examples/tpb)
(in-package :cl-mpm/examples/tpb)
(defun plot (sim)
  (format t "~A ~%" (local-time:now)))
(defun plot-domain ()
  (format t "~A ~%" (local-time:now)))
(let ((threads (parse-integer (if (uiop:getenv "OMP_NUM_THREADS") (uiop:getenv "OMP_NUM_THREADS") "16"))))
  (setf lparallel:*kernel* (lparallel:make-kernel threads :name "custom-kernel"))
  (format t "Thread count ~D~%" threads))

(defparameter *ref* (parse-float:parse-float (if (uiop:getenv "REFINE") (uiop:getenv "REFINE") "1")))

(defparameter *top-dir* (merge-pathnames "./vtk_data/"))

(let* ((mps 2)
       (output-dir (merge-pathnames  (format nil "./output-~f-~f/" *ref* mps) *top-dir*)))
  (format t "Outputting to ~A~%" output-dir)
  (let* ()
    (format t "Running sim size ~a ~a ~%" *ref* mps)
    (let* ((lstps 10)
           (total-disp -0.2d-3))
      (setup :refine *ref* :mps 3)
      (setf cl-mpm/damage::*enable-reflect-x* t)
      (setf (cl-mpm::sim-gravity *sim*) 0d0)
      (defparameter *data-displacement* '(0d0))
      (defparameter *data-load* '(0d0))
      (cl-mpm/setup::set-mass-filter *sim* 2.2d3 :proportion 1d-15)

      (setf (cl-mpm::sim-nonlocal-damage *sim*) t)
      (setf *disp* 0d0)
      (cl-mpm/dynamic-relaxation::run-adaptive-load-control
       *sim*
       :output-dir output-dir
       :plotter (lambda (sim)
                  (format t "Load ~E ~%" (cl-mpm/penalty::resolve-load *penalty*)))
       :loading-function (lambda (percent)
                           (setf *displacement* (* total-disp percent))
                           (let ((delta (- *displacement* *last-pos*)))
                             (cl-mpm/penalty::bc-increment-center *penalty* (cl-mpm/utils:vector-from-list (list 0d0 delta 0d0)))
                             (setf *last-pos* *displacement*))
                           )
       :pre-step (lambda ()
                   (output-disp-header output-dir)
                   (output-disp-data output-dir)
                   )
       :post-conv-step (lambda (sim)
                         ;;Save data
                         (push (* 2d0 (cl-mpm/penalty::resolve-load *penalty*)) *data-load*)
                         (push *displacement* *data-displacement*)
                         (output-disp-data output-dir))
       :load-steps lstps
       :enable-damage t
       :damping (sqrt 2)
       :substeps 20
       :criteria 1d-3
       :max-adaptive-steps 10
       :save-vtk-dr t
       :save-vtk-loadstep nil
       :max-damage-inc 0.5d0
       :plotter (lambda (sim))
       :dt-scale 1d0))

    ))
