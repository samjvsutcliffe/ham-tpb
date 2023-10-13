(restrict-compiler-policy 'speed 3 3)
(restrict-compiler-policy 'debug 0 0)
(restrict-compiler-policy 'safety 0 0)
(setf *block-compile-default* t)
(setf *features* (delete :cl-mpm-pic *features*))
(ql:quickload "magicl")
(ql:quickload "cl-mpm")
(asdf:compile-system :cl-mpm :force T)
(ql:quickload "cl-mpm/examples/slump")
(asdf:compile-system :cl-mpm/examples/slump :force T)
(in-package :cl-mpm/examples/slump)

(defun rectangle-sdf (position size)
  (lambda (pos)
    (let* ((pos (magicl:from-list (list
                                        (magicl:tref pos 0 0)
                                        (magicl:tref pos 1 0)
                                        ) '(2 1) :type 'double-float))
           (position (magicl:from-list position '(2 1) :type 'double-float))
           (dist-vec (magicl:.- (magicl:map! #'abs (magicl:.- pos position))
                                (magicl:from-list size '(2 1) :type 'double-float))))

      (+ (sqrt (magicl::sum
                (magicl:map! (lambda (x) (* x x))
                             (magicl:map! (lambda (x) (max 0d0 x)) dist-vec))))
         (min (max (magicl:tref dist-vec 0 0)
                   (magicl:tref dist-vec 1 0)
                   ) 0d0)))))

(defun apply-pullout (sim load-mps push-rate)
  (with-accessors ((mps cl-mpm:sim-mps)
                   (mesh cl-mpm:sim-mesh))
      sim
    (loop for mp in load-mps
          do
             (cl-mpm::iterate-over-neighbours
              mesh
              mp
              (lambda (mesh mp node svp grad fsvp fgrad)
                (with-accessors ()
                    mp
                  (with-accessors ((pos cl-mpm/mesh::node-position)
                                   (vel cl-mpm/mesh::node-velocity)
                                   (active cl-mpm/mesh::node-active)
                                   (acc cl-mpm/mesh::node-acceleration)
                                   )
                      node
                    (when active
                      (setf (magicl:tref vel 1 0) push-rate)))))))))

(defparameter *current-load* 0d0)
(defun apply-force (sim load-mps push-rate)
  (with-accessors ((mps cl-mpm:sim-mps)
                   (mesh cl-mpm:sim-mesh))
      sim
    (loop for mp in load-mps
          do
             (setf (magicl:tref (cl-mpm/particle:mp-body-force mp) 1 0)
                   (* *current-load* (/ 1d0 (* (cl-mpm/particle:mp-volume mp) (length load-mps))))))
    ))

(defun energy-norm (sim)
  (/ (loop for mp across (cl-mpm:sim-mps *sim*)
          sum (magicl:norm (cl-mpm/particle:mp-velocity mp))) (length (cl-mpm:sim-mps *sim*))))

(defun get-disp (load-mps)
  ;; (* *t* *tip-velocity*)
  (- (/ (loop for mp in load-mps
              sum (+
                   (magicl:tref (cl-mpm/particle::mp-position mp) 1 0)
                   (* 0.5d0 (magicl:tref (cl-mpm/particle::mp-domain-size mp) 1 0))
                   )) (length load-mps))
     *initial-surface*
     ))

(defun get-force-mps (sim load-mps)
  (with-accessors ((mps cl-mpm:sim-mps)
                   (mesh cl-mpm:sim-mesh))
      sim
    (let ((force 0d0))
      (loop for mp in load-mps
            do
               (cl-mpm::iterate-over-neighbours
                mesh
                mp
                (lambda (mesh mp node svp &rest args)
                  (incf force
                        (* 1d0
                           (cl-mpm/fastmath::mag (cl-mpm/mesh::node-force node))
                           )
                        ))))
      force
      )))

(defun get-reaction-force (load-nodes)
  ;; (cl-mpm/fastmath::mag (cl-mpm/mesh::node-force (nth 0 load-nodes)))
  (* 2
     (/ (loop for mp in load-nodes
              sum
              ;; (cl-mpm/fastmath::mag (cl-mpm/mesh::node-force mp))
              (- (magicl:tref (cl-mpm/mesh::node-force mp) 1 0))
              )
        1d0
                                        ;(length load-nodes)
        ))
  )

(defparameter *target-displacement* 0d0)
(defun apply-disp-penalty (sim load-mps)
  (with-accessors ((mps cl-mpm:sim-mps)
                   (mesh cl-mpm:sim-mesh))
      sim
    (let* ((penalty 1d5)
           (displacement *target-displacement*)
           (pos
             (get-disp load-mps))
           (force (* penalty (- displacement pos))))
      (incf *current-load* force)
      (loop for mp in load-mps
            do
               (setf (magicl:tref (cl-mpm/particle:mp-body-force mp) 1 0)
                     (* force (/ 1d0 (* (cl-mpm/particle:mp-volume mp) (length load-mps))))
                     ))
      )))

(defparameter *tip-velocity* -0.005d-3)
(defun setup-test-column (size block-size offset &optional (e-scale 1) (mp-scale 1))
  (let* ((sim (cl-mpm/setup::make-block
               (/ 1d0 e-scale)
               (mapcar (lambda (x) (* x e-scale)) size)
               #'cl-mpm/shape-function:make-shape-function-bspline
               ;; 'cl-mpm::mpm-sim-usf
               'cl-mpm/damage::mpm-sim-damage
               ))
         (h (cl-mpm/mesh:mesh-resolution (cl-mpm:sim-mesh sim)))
         (h-x (/ h 1d0))
         (h-y (/ h 1d0))
         (density 2.2d3)
         (elements (mapcar (lambda (s) (* e-scale (/ s 2))) size))
         )
    (declare (double-float h density))
    (progn
      (let* ((impactor-size (list (* 0.99 h-x) (* 0.99 h-x)))
            (impactors
              (cl-mpm/setup::make-block-mps-list
               (mapcar #'+ offset
                       (list
                        0d0
                        ;; (- (* 0.5d0 (first block-size)) (* 0.5d0 (first impactor-size)))
                        (+ (second block-size) (* h-x 0.5))
                        ))
               impactor-size
               (mapcar (lambda (e) (* e e-scale mp-scale)) impactor-size)
               density
               'cl-mpm/particle::particle-elastic
               :E 20d9
               :nu 0.20d0
               :gravity -0.0d0
               :gravity-axis (cl-mpm/utils:vector-from-list '(0d0 1d0 0d0))
               :fixed-velocity (list 0 *tip-velocity*)
               :index 1
               )
              ))
        (setf (cl-mpm:sim-mps sim)
              (cl-mpm/setup::make-mps-from-list
               (append
                (cl-mpm/setup::make-block-mps-list
                 offset
                 block-size
                 ;; (mapcar (lambda (e) (ceiling (* e e-scale mp-scale))) block-size)
                 (mapcar (lambda (e) (* e e-scale mp-scale)) block-size)
                 density
                 'cl-mpm/particle::particle-concrete
                 :E 20d9
                 :nu 0.20d0
                 :fracture-energy 90d0
                 :initiation-stress (* 2.4d6 1d0)
                 :critical-damage 0.999d0
                 :local-length 5d-3
                 :local-length-damaged 5d-3
                 ;; :local-length-damaged 0.01d0
                 :gravity -0.0d0
                 :gravity-axis (cl-mpm/utils:vector-from-list '(0d0 0d0 0d0))
                 )
                impactors
                )
               )))
      (setf (cl-mpm:sim-allow-mp-split sim) t)
      (setf (cl-mpm::sim-enable-damage sim) t)
      (setf (cl-mpm::sim-nonlocal-damage sim) t)
      (setf (cl-mpm::sim-allow-mp-damage-removal sim) nil)
      (setf (cl-mpm::sim-mp-damage-removal-instant sim) nil)
      (setf (cl-mpm::sim-mass-filter sim) 0d0)
      (let ((ms 1d5))
        (setf (cl-mpm::sim-mass-scale sim) ms)
        ;; (setf (cl-mpm:si-damping-factor sim) (* 1d-2 density ms))
        ;; (setf (cl-mpm:sim-damping-factor sim) 10.0d0)
        ;; (setf (cl-mpm:sim-damping-factor sim) (* 1d-1 density ms))
        ;; (setf (cl-mpm:sim-damping-factor sim) (* 1d-2 density ms))
        (setf (cl-mpm:sim-damping-factor sim)
              ;; 1d4
              (* 0.05d0 density ms)))

      (dotimes (i 0)
        (dolist (dir (list :x :y))
          (cl-mpm::split-mps-criteria
           sim
           (lambda (mp h)
             (when
                 (and
                  (> (magicl:tref (cl-mpm/particle:mp-position mp) 0 0)
                     120)
                  (< (magicl:tref (cl-mpm/particle:mp-position mp) 0 0)
                     200 
                     )
                  (> (magicl:tref (cl-mpm/particle:mp-position mp) 1 0)
                     10
                     )
                  )
               dir
               )))))

      (let ((dt-scale 1d0))
        (setf
         (cl-mpm:sim-dt sim)
         (* dt-scale h
            (sqrt (cl-mpm::sim-mass-scale sim))
            (sqrt (/ density (cl-mpm/particle::mp-p-modulus (aref (cl-mpm:sim-mps sim) 0)))))))

      (format t "Estimated dt ~F~%" (cl-mpm:sim-dt sim))

      (let* ((crack-width 1d-2)
             ;; (crack-left (- (+ (first offset)  (* 0.5d0 (first block-size))) crack-width))
             ;; (crack-right (+ (+ (first offset) (* 0.5d0 (first block-size)) crack-width)))
             (crack-left 0d0)
             (crack-right crack-width)
             (above-crack
               (loop for mp across (cl-mpm:sim-mps sim)
                     when
                     (and
                      (not (= (cl-mpm/particle::mp-index mp) 1))
                      (>= (magicl:tref (cl-mpm/particle:mp-position mp) 0 0) crack-left)
                      (<= (magicl:tref (cl-mpm/particle:mp-position mp) 0 0) crack-right))
                     collect mp)
               )
             (max-pos (loop for mp in above-crack
                            maximize (magicl:tref(cl-mpm/particle:mp-position mp) 1 0)))
             )
        (defparameter *terminus-mps*
          (loop for mp in above-crack
                when (= max-pos (magicl:tref
                                 (cl-mpm/particle:mp-position mp)
                                 1 0))
                  collect mp)))

      (setf *terminus-mps* (list (nth 0 *terminus-mps*)))
      ;; (setf *terminus-mps* (mapcar (lambda (i) (nth i *terminus-mps*))
      ;;                              (list (floor (- (length *terminus-mps*) 1) 2))))
      ;; (when (> (length *terminus-mps*) 2)
      ;;   (setf *terminus-mps* (mapcar (lambda (i) (nth i *terminus-mps*))
      ;;                                (list (floor (- (length *terminus-mps*) 1) 2)
      ;;                                      (+ (floor (- (length *terminus-mps*) 1) 2) 1)))))

      (let ((left-node-pos
              (list
               (round (first offset) h-x)
               (round (second offset) h-x)
               0))
            (right-node-pos
              (list
               (round (+ (first offset) (first block-size)) h-x)
               (round (second offset) h-x)
               0)
              ))
        (defparameter *fixed-nodes* (mapcar (lambda (id) (cl-mpm/mesh:get-node (cl-mpm:sim-mesh sim)
                                                                                     id))
                                                  (list left-node-pos right-node-pos)
                                                  ))
        (format t "Fixed node ~A ~%" left-node-pos)
        (format t "Roller node ~A ~%" right-node-pos)
        (setf (cl-mpm:sim-bcs sim)
              (cl-mpm/bc::make-bcs-from-list
               (append
                (cl-mpm/bc::make-outside-bc-var-list
                 (cl-mpm:sim-mesh sim)
                 (lambda (i) (cl-mpm/bc::make-bc-fixed i '(0 nil nil)))
                 (lambda (i) (cl-mpm/bc::make-bc-fixed i '(0 nil nil)))
                 (lambda (i) (cl-mpm/bc::make-bc-fixed i '(nil 0 nil)))
                 (lambda (i) (cl-mpm/bc::make-bc-fixed i '(nil 0 nil)))
                 (lambda (i) (cl-mpm/bc::make-bc-fixed i '(nil nil 0)))
                 (lambda (i) (cl-mpm/bc::make-bc-fixed i '(nil nil 0)))
                 )
                (list
                 ;; (cl-mpm/bc::make-bc-fixed left-node-pos
                 ;;                           '(0 0 nil))

                 (cl-mpm/bc::make-bc-fixed right-node-pos
                                           '(nil 0 nil)))
                ))))

      (defparameter *floor-bc*
        (cl-mpm/penalty::make-bc-penalty-point-normal
         sim
         (cl-mpm/utils:vector-from-list '(0d0 1d0 0d0))
         (cl-mpm/utils:vector-from-list (list 00d0 (second offset) 0d0))
         (* density 1d5)
         0.0d0
         ;; 1d1
         ))
      (defparameter *initial-surface*
        (loop for mp across (cl-mpm:sim-mps sim)
              when (not (= 1 (cl-mpm/particle::mp-index mp)))
              maximize (magicl:tref
                        (magicl:.+ (cl-mpm/particle:mp-position mp)
                                   (magicl:scale (cl-mpm/particle::mp-domain-size mp) 0.5d0)
                                   )
                        1 0
                        )))

      (format t "~A~%" h-x)
      ;; (setf (cl-mpm::sim-bcs-force-list sim)
      ;;       (list
      ;;        (cl-mpm/bc:make-bcs-from-list
      ;;         (list
      ;;          ;; *floor-bc*
      ;;          (cl-mpm/bc::make-bc-closure
      ;;           '(0 0 0)
      ;;           (lambda ()
      ;;             (with-accessors ((mesh cl-mpm:sim-mesh)
      ;;                              (dt cl-mpm::sim-dt)
      ;;                              )
      ;;                 sim
      ;;               (let ((datum (* -1d0 (+ *initial-surface* *target-displacement*)))
      ;;                     (normal (cl-mpm/utils:vector-from-list  '(0d0 -1d0 0d0))))
      ;;                 (cl-mpm/penalty::apply-displacement-control-mps mesh (coerce *terminus-mps* 'vector )
      ;;                                                  dt
      ;;                                                  normal
      ;;                                                  datum
      ;;                                                  (* density 1d7)
      ;;                                                  ;; (* 0d0 density 1d10)
      ;;                                                  0d0))
      ;;               )))
      ;;          ))))

      ;; (let* ((terminus-size (second block-size))
      ;;        (ocean-y (* terminus-size 1.0d0)))
      ;;   (setf (cl-mpm::sim-bcs-force-list sim)
      ;;         (list
      ;;          (cl-mpm/bc:make-bcs-from-list
      ;;           (list
      ;;            (cl-mpm/buoyancy::make-bc-buoyancy-clip
      ;;             sim
      ;;             ocean-y
      ;;             1000d0
      ;;             (lambda (pos datum)
      ;;               t)
      ;;             ))))))

      sim)))


(defparameter *sim* nil)
(defparameter *run-sim* t)
(defparameter *t* 0)
(defparameter *sim-step* 0)
(defparameter *refine* (/ 1d0 2d0))
(let ((refine (uiop:getenv "REFINE")))
  (when refine
    (setf *refine* (parse-integer (uiop:getenv "REFINE")))
    ))

(defun setup (&key (undercut 0d0))
  ;; (let ((mps-per-dim 4))
  ;;   (defparameter *sim* (setup-test-column '(16 16) '(8 8)  '(0 0) *refine* mps-per-dim)))
  ;; (defparameter *sim* (setup-test-column '(1 1 1) '(1 1 1) 1 1))

  (let* ((mesh-size (/ 0.025 4))
         (mps-per-cell 2)
         (shelf-height 0.100d0)
         ;(shelf-length 0.55d0)
         (shelf-length 0.225d0)
         (domain-length (+ shelf-length (* 4 mesh-size)))
         (offset (list
                  0d0
                  ;(* 2 mesh-size)
                       (* shelf-height 1)))
         )
    (defparameter *sim*
      (setup-test-column (list domain-length (+ mesh-size (* shelf-height 3)))
                         (list shelf-length shelf-height)
                         offset
                         (/ 1d0 mesh-size) mps-per-cell))
    (let ((cut-depth (* 1 50d-3))
          (cut-width 2.5d-3))
      (cl-mpm/setup::remove-sdf
       *sim*
       (rectangle-sdf
        (list
         0d0
         ;(+ (first offset) (* 0.5d0 shelf-length))
              (+ (second offset) 0d0)
              )
        (list
         ;; 3.0d-3
         10d-3
         cut-depth
         )))
      ;; (cl-mpm/setup::damage-sdf
      ;;  *sim*
      ;;  (rectangle-sdf
      ;;   (list
      ;;    0d0
      ;;                                   ;(+ (first offset) (* 0.5d0 shelf-length))
      ;;    (+ (second offset) 0d0)
      ;;    )
      ;;   (list
      ;;    ;; 3.0d-3
      ;;    10d-3
      ;;    1000d0
      ;;    ))
      ;;  0.9d0
      ;;  )
      )
    (format t "Total weight ~F~%"
            (loop for mp across (cl-mpm:sim-mps *sim*)
                  sum (* 9.8d0 (cl-mpm/particle:mp-mass mp))))

    (defparameter *current-load* 0d0)
    ;; (loop for mp across (cl-mpm:sim-mps *sim*)
    ;;       do
    ;;          (setf (cl-mpm/particle:mp-damage mp) (random 0.1d0)))
    ;; (cl-mpm/setup::damage-sdf
    ;;  *sim*
    ;;  (lambda (p)
    ;;    (cl-mpm/setup::line-sdf (magicl:from-list (list (magicl:tref p 0 0)
    ;;                                                    (magicl:tref p 1 0))
    ;;                                              '(2 1))
    ;;                            (list (- shelf-length shelf-height) shelf-height)
    ;;                            (list shelf-length soil-boundary)
    ;;                            10d0
    ;;                            )) 0.8d0)
    ;(let ((sdf
    ;        (lambda (p)
    ;          (cl-mpm/setup::line-sdf (magicl:from-list (list (magicl:tref p 0 0)
    ;                                                          (magicl:tref p 1 0))
    ;                                                    '(2 1))
    ;                                  (list (- shelf-length shelf-height) shelf-height)
    ;                                  (list shelf-length 0d0)
    ;                                  20d0
    ;                                  ))
    ;        ))
    ;  (loop for mp across (cl-mpm:sim-mps *sim*)
    ;        do (with-accessors ((pos cl-mpm/particle:mp-position)
    ;                            (damage cl-mpm/particle:mp-damage)) mp
    ;             (when (>= 0 (funcall sdf pos))
    ;               (setf damage (min 1d0 (max 0d0 (coerce (* (funcall sdf pos) -0.1d0) 'double-float)))))
    ;             )))


    )
  (defparameter *target-displacement* 0d0)
  (format t "MPs: ~D~%" (length (cl-mpm:sim-mps *sim*)))
  (loop for f in (uiop:directory-files (uiop:merge-pathnames* "./outframes/")) do (uiop:delete-file-if-exists f))
  (defparameter *run-sim* t)
  (defparameter *t* 0)
  (defparameter *sim-step* 0))

(defun run ()
  (vgplot:close-all-plots)
  (cl-mpm/output:save-vtk-mesh (merge-pathnames "output/mesh.vtk")
                          *sim*)
  (defparameter *data-force* '())
  (defparameter *data-displacement* '(0d0))
  (defparameter *data-load* '(0d0))
  (defparameter *data-node-load* '(0d0))

  (with-open-file (stream (merge-pathnames "output/disp.csv") :direction :output :if-exists :supersede)
    (format stream "disp,load~%"))

  (let* ((target-time 1d0)
         (dt (cl-mpm:sim-dt *sim*))
         (substeps (floor target-time dt))
         (dt-scale 1d0)
         )

    (defparameter *target-displacement* 0d0)
    (setf cl-mpm/penalty::*debug-force* 0d0)
    (setf cl-mpm/penalty::*debug-force-count* 0d0)
    (cl-mpm::update-sim *sim*)
    (multiple-value-bind (dt-e substeps-e) (cl-mpm:calculate-adaptive-time *sim* target-time :dt-scale dt-scale)
                    (format t "CFL dt estimate: ~f~%" dt-e)
                    (format t "CFL step count estimate: ~D~%" substeps-e)
                    (setf substeps substeps-e))
    (format t "Substeps ~D~%" substeps)
    ;; (incf *target-displacement* -1.0d-3)
    (time (loop for steps from 0 to 100
                while *run-sim*
                do
                   (progn
                     ;; (when (= steps 5)
                     ;;   (setf (cl-mpm::sim-enable-damage *sim*) t)
                     ;;   )
                     (format t "Step ~d ~%" steps)
                     (cl-mpm/output:save-vtk (merge-pathnames (format nil "output/sim_~5,'0d.vtk" *sim-step*)) *sim*)
                     (cl-mpm/output::save-vtk-nodes (merge-pathnames (format nil "output/sim_nodes_~5,'0d.vtk" *sim-step*)) *sim*)


                     (let ((average-force 0d0)
                           (average-reaction 0d0))
                       (time
                        (dotimes (i substeps);)
                          (push
                           (get-disp *terminus-mps*)
                           *data-displacement*)
                          (push
                           (get-reaction-force *fixed-nodes*)
                           *data-load*)

                          (incf average-force (/
                                               (/ cl-mpm/penalty::*debug-force*
                                                  (max 1 cl-mpm/penalty::*debug-force-count*))
                                               substeps
                                               ))
                          (incf average-reaction (/ (get-reaction-force *fixed-nodes*) substeps))
                          (setf cl-mpm/penalty::*debug-force* 0d0)
                          (setf cl-mpm/penalty::*debug-force-count* 0d0)
                          (cl-mpm::update-sim *sim*)
                          (incf *target-displacement* (* dt *tip-velocity*))
                          (setf *t* (+ *t* (cl-mpm::sim-dt *sim*))))
                        )
                       ;; (incf *target-displacement* -0.01d-3)
                       ;; (push
                       ;;  ;; *target-displacement*
                       ;;   (get-disp *terminus-mps*)
                       ;;  *data-displacement*)
                       ;; (push
                       ;;  average-force
                       ;;  *data-load*)
                       ;; (push
                       ;;  average-reaction
                       ;;  *data-node-load*)


                       (format t "Displacement: ~f - Load: ~f~%"
                               (get-disp *terminus-mps*)
                               (get-reaction-force *fixed-nodes*)
                               )
                       ;; (format t "Target load: ~f~%" (* *target-displacement* 20d9))
                       ;; (format t "Current load: ~f~%" (* (get-disp *terminus-mps*) 1d9))
                       ;; (format t "Node Load: ~f~%" (get-reaction-force *fixed-nodes*))
                       ;; (format t "Pen load: ~f~%" average-force)

                       (with-open-file (stream (merge-pathnames "output/disp.csv") :direction :output :if-exists :append)
                         (format stream "~f,~f~%"
                                 (get-disp *terminus-mps*)
                                 (get-reaction-force *fixed-nodes*)
                                 ))
                       )

                     (format t "Target: ~f - Current: ~f Error: ~f - energy ~F~%"
                             *target-displacement*
                             (get-disp *terminus-mps*)
                             (* 100d0 (/ (- *target-displacement* (get-disp *terminus-mps*)) *target-displacement*))
                             (energy-norm *sim*))

                     (multiple-value-bind (dt-e substeps-e) (cl-mpm:calculate-adaptive-time *sim* target-time :dt-scale dt-scale)
                       (format t "CFL dt estimate: ~f~%" dt-e)
                       (format t "CFL step count estimate: ~D~%" substeps-e)
                       (setf substeps substeps-e))
                     (incf *sim-step*)
                     (swank.live:update-swank)
                     ))))
  (cl-mpm/output:save-vtk (merge-pathnames (format nil "output/sim_~5,'0d.vtk" *sim-step*)) *sim*))



(setf lparallel:*kernel* (lparallel:make-kernel 8 :name "custom-kernel"))
(defparameter *run-sim* nil)
(setup)
(format t "MP count:~D~%" (length (cl-mpm:sim-mps *sim*)))
(run)

