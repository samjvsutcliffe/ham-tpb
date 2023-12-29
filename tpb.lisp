;(restrict-compiler-policy 'speed 3 3)
;(restrict-compiler-policy 'debug 0 0)
;(restrict-compiler-policy 'safety 0 0)
;(setf *block-compile-default* t)
;(setf *features* (delete :cl-mpm-pic *features*))
(in-package :cl-mpm/examples/tpb)
(setf *load-verbose* nil)
(setf *load-print* nil)
(setf *compile-verbose* nil)
(setf *compile-print* nil)

(defun rectangle-sdf (position size)
  (lambda (pos)
    (let* (
           (pos (magicl:from-list (list
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
  (if (> (length load-mps) 0d0)
    (- (/ (loop for mp in load-mps
                sum (+
                     (magicl:tref (cl-mpm/particle::mp-position mp) 1 0)
                     (* 0.5d0 (magicl:tref (cl-mpm/particle::mp-domain-size mp) 1 0))
                     )) (length load-mps))
       *initial-surface*
       )
    0d0))

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
                        (* svp
                           (cl-mpm/fastmath::mag (cl-mpm/mesh::node-force node))
                           )
                        ))))
      (/ force (length load-mps))
      )))

(defun get-reaction-force (load-nodes)
  ;; (cl-mpm/fastmath::mag (cl-mpm/mesh::node-force (nth 0 load-nodes)))
  (loop for mp in load-nodes
        sum
        ;; (cl-mpm/fastmath::mag (cl-mpm/mesh::node-force mp))
        (- (magicl:tref (cl-mpm/mesh::node-force mp) 1 0))
        ))

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

;(defparameter *tip-velocity* -0.02d-3)
(defparameter *tip-velocity* -0.000d-3)

(defun setup-test-column (size block-size offset &optional (e-scale 1) (mp-scale 1)
                               (kappa-scale 1d0)
                               (lc-scale 1d0)
                               )
  (let* ((sim (cl-mpm/setup::make-block
               (/ 1d0 e-scale)
               (mapcar (lambda (x) (* x e-scale)) size)
               ;; 'cl-mpm::mpm-sim-usf
               ;; 'cl-mpm/damage::mpm-sim-damage
               :sim-type 'cl-mpm/mpi::mpm-sim-mpi-nodes-damage
               ))
         (h (cl-mpm/mesh:mesh-resolution (cl-mpm:sim-mesh sim)))
         (h-x (/ h 1d0))
         (h-y (/ h 1d0))
         (density 2.2d3)
         (elements (mapcar (lambda (s) (* e-scale (/ s 2))) size))
         )
    (declare (double-float h density))
    (progn
      (let* ((impactor-size (list 10d-3
                                  (* 0.99 h-x))))
        ;;Crack-scale 4
        ;;Kappa 0.8
        (let ((lc 5.3d-3)
              (crack-scale (* 1d0 lc-scale))
              (kappa (* (sqrt 7) kappa-scale)))
        (setf (cl-mpm:sim-mps sim)
              (cl-mpm/setup::make-mps-from-list
               (append
                (cl-mpm/setup::make-block-mps-list
                 offset
                 block-size
                 (mapcar (lambda (e)
                           (round  (* e mp-scale) h-x)
                           ) block-size)
                 density
                 'cl-mpm/particle::particle-limestone
                 :E 15.3d9
                 ;:E 18d9
                 :nu 0.15d0
                 ;; :elastic-approxmation :
                 :fracture-energy (* 48d0 1d0)
                 :initiation-stress 3.4d6
                 :critical-damage 1.000d0
                 :compression-ratio 8d0
                 :ductility 7.1d0
                 :internal-length (* lc crack-scale 1d0)
                 :local-length (* lc crack-scale kappa) 
                 :local-length-damaged (* lc crack-scale kappa) 
                 :gravity -0.0d0
                 :gravity-axis (cl-mpm/utils:vector-from-list '(0d0 1d0 0d0))
                 )
                ;; impactors
                )
               ))
        ))
      (setf (cl-mpm:sim-allow-mp-split sim) nil)
      (setf (cl-mpm::sim-enable-damage sim) t)
      (setf (cl-mpm::sim-nonlocal-damage sim) t)
      (setf (cl-mpm::sim-allow-mp-damage-removal sim) nil)
      (setf (cl-mpm::sim-mp-damage-removal-instant sim) nil)
      (setf (cl-mpm::sim-mass-filter sim) 0d0)
      (let ((ms 1d0))
        (setf (cl-mpm::sim-mass-scale sim) ms)
        (setf (cl-mpm:sim-damping-factor sim)
              (* 1d-1 density ms)
              ;; 1d0
              ))

      (dotimes (i 0)
        (dolist (dir (list :x :y))
          (cl-mpm::split-mps-criteria
           sim
           (lambda (mp h)
             (when
                 (and
                  (> (magicl:tref (cl-mpm/particle:mp-position mp) 0 0)
                     (+ (first offset) (* (first block-size) 0.45))
                     )
                  (< (magicl:tref (cl-mpm/particle:mp-position mp) 0 0)
                     (+ (first offset) (* (first block-size) 0.55))
                     )
                  )
               dir
               )))))

      (let ((dt-scale 0.5d0))
        (setf
         (cl-mpm:sim-dt sim)
         (* dt-scale h
            (sqrt (cl-mpm::sim-mass-scale sim))
            (sqrt (/ density (cl-mpm/particle::mp-p-modulus (aref (cl-mpm:sim-mps sim) 0)))))))

      (format t "Estimated dt ~F~%" (cl-mpm:sim-dt sim))

      (let* ((crack-width 1d-1)
             ;(crack-left (- (+ (first offset)  (* 0.5d0 (first block-size))) crack-width))
             ;(crack-right (+ (+ (first offset) (* 0.5d0 (first block-size))) crack-width))
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
                            maximize (magicl:tref (cl-mpm/particle:mp-position mp) 1 0)))
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
      (loop for mp in *terminus-mps*
            do (setf (cl-mpm/particle::mp-index mp) 1))

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
                                                  (list
                                                   ;; left-node-pos
                                                   right-node-pos)
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

      ;; (defparameter *floor-bc*
      ;;   (cl-mpm/penalty::make-bc-penalty-point-normal
      ;;    sim
      ;;    (cl-mpm/utils:vector-from-list '(0d0 1d0 0d0))
      ;;    (cl-mpm/utils:vector-from-list (list 00d0 (second offset) 0d0))
      ;;    (* density 1d3)
      ;;    0.0d0
      ;;    ;; 1d1
      ;;    ))
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
      (setf (cl-mpm::sim-bcs-force-list sim)
            (list
             (cl-mpm/bc:make-bcs-from-list
              (list
               ;; *floor-bc*
               (cl-mpm/bc::make-bc-closure
                '(0 0 0)
                (lambda ()
                  (with-accessors ((mesh cl-mpm:sim-mesh)
                                   (dt cl-mpm::sim-dt)
                                   )
                      sim
                    (let ((datum (* -1d0 (+ *initial-surface* *target-displacement*)))
                          (normal (cl-mpm/utils:vector-from-list  '(0d0 -1d0 0d0)))
                          (terminus-mps (loop for mp across (cl-mpm:sim-mps *sim*)
                                              when (= (cl-mpm/particle::mp-index mp) 1)
                                              collect mp))
                          )
                      (cl-mpm/penalty::apply-displacement-control-mps mesh (coerce terminus-mps 'vector )
                                                       dt
                                                       normal
                                                       datum
                                                       (* density 1d6)
                                                       ;; (* 0d0 density 1d10)
                                                       0d0))
                    )))
               ))))

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

(defun setup (&optional (kappa-scale 1d0)
                        (lc-scale 1d0)
                        (refine 1d0)
                        )
  ;; (let ((mps-per-dim 4))
  ;;   (defparameter *sim* (setup-test-column '(32 16) '(8 8)  '(0 0) *refine* mps-per-dim)))
  ;; (defparameter *sim* (setup-test-column '(1 1 1) '(1 1 1) 1 1))

  (let* ((mesh-size (/ 0.0102 (* refine 1.00)))
         (mps-per-cell 2)
         (shelf-height 0.102d0)
         (shelf-length (* shelf-height 2))
         ;; (shelf-length 0.225d0)
         (domain-length (+ shelf-length (* 5 mesh-size)))
         (offset (list
                  0d0
                  ;(* 2 mesh-size)
                  (* 4 mesh-size)
                       ;; (* shelf-height 1)
                       ))
         )
    (defparameter *sim*
      (setup-test-column (list domain-length (+ shelf-height (* mesh-size 8)))
                         (list shelf-length shelf-height)
                         offset
                         (/ 1d0 mesh-size) mps-per-cell
                         kappa-scale
                         lc-scale))
    (let ((cut-depth (* 0.4d0 shelf-height))
          (cut-width 2.5d-3))
      (cl-mpm/setup::remove-sdf
       *sim*
       (rectangle-sdf
        (list
         0d0
         ;;(+ (first offset) (* 0.5d0 shelf-length))
              (+ (second offset) 0d0)
              )
        (list
         ;10.0d-3
         ;5d-3
         ;1.33d-3
         ;; 10d-3
         mesh-size
         cut-depth
         )))
      )
    (format t "Crack width:~F~%" (* 2d0 mesh-size))
    (format t "Total weight ~F~%"
            (loop for mp across (cl-mpm:sim-mps *sim*)
                  sum (* 9.8d0 (cl-mpm/particle:mp-mass mp))))

    (defparameter *current-load* 0d0)


    )
  (defparameter *target-displacement* 0d0)
  (format t "MPs: ~D~%" (length (cl-mpm:sim-mps *sim*)))
  (loop for f in (uiop:directory-files (uiop:merge-pathnames* "./outframes/")) do (uiop:delete-file-if-exists f))
  (defparameter *run-sim* t)
  (defparameter *t* 0)
  (defparameter *sim-step* 0))


(defparameter *data-force* '())
(defparameter *data-displacement* '(0d0))
(defparameter *data-load* '(0d0))
(defparameter *data-time* '(0d0))
(defparameter *data-node-load* '(0d0))
(defparameter *target-displacement* 0d0)
(defparameter *data-averaged* t)

(defparameter *data-full-time* '(0d0))
(defparameter *data-full-load* '(0d0))
(defmacro rank-0-time (body)
  `(if (= (cl-mpi:mpi-comm-rank) 0)
       (time 
         ,body)
       (progn 
         ,body))
  )

(defun run ()
  (cl-mpm/output:save-vtk-mesh (merge-pathnames "output/mesh.vtk")
                          *sim*)
  (defparameter *data-force* '())
  (defparameter *data-displacement* '(0d0))
  (defparameter *data-load* '(0d0))
  (defparameter *data-node-load* '(0d0))
  (defparameter *data-mp-load* '(0d0))
  (defparameter *data-time* '(0d0))
  (defparameter *target-displacement* 0d0)
  (defparameter *data-full-time* '(0d0))
  (defparameter *data-full-load* '(0d0))

  (with-open-file (stream (merge-pathnames "output/disp.csv") :direction :output :if-exists :supersede)
    (format stream "disp,load,nload~%")
    (format stream "~f,~f,~f~%" 0d0 0d0 0d0)
    )

  (let* ((target-time 0.5d0)
         (dt (cl-mpm:sim-dt *sim*))
         (substeps (floor target-time dt))
         (dt-scale 1d0)
         (disp-step -0.002d-3)
         )

    (setf cl-mpm/penalty::*debug-force* 0d0)
    (setf cl-mpm/damage::*enable-reflect-x* t)
    (setf cl-mpm/penalty::*debug-force-count* 0d0)
    (time (cl-mpm::update-sim *sim*))

    (format t "Calculating dt~%")
    (multiple-value-bind (dt-e substeps-e) (cl-mpm:calculate-adaptive-time *sim* target-time :dt-scale dt-scale)
                    (format t "CFL dt estimate: ~f~%" dt-e)
                    (format t "CFL step count estimate: ~D~%" substeps-e)
                    (setf substeps substeps-e))
    (format t "Substeps ~D~%" substeps)
    (time (loop for steps from 0 to 100
                while *run-sim*
                do
                   (progn
                     (format t "Step ~d ~%" steps)
                     (cl-mpm/output:save-vtk (merge-pathnames (format nil "output/sim_~5,'0d.vtk" *sim-step*)) *sim*)
                     (let ((average-force 0d0)
                           (average-disp 0d0)
                           (average-reaction 0d0))
                       (time
                        (dotimes (i 1);)
                          (incf average-force (/
                                               cl-mpm/penalty::*debug-force*
                                               substeps
                                               ))
                          (incf average-reaction
                                (/ (get-reaction-force *fixed-nodes*) substeps)
                                )
                          (incf average-disp
                                (/ (get-disp *terminus-mps*) substeps)
                                )
                          (incf average-reaction (/ (get-reaction-force *fixed-nodes*) substeps))
                          (setf cl-mpm/penalty::*debug-force* 0d0)
                          (setf cl-mpm/penalty::*debug-force-count* 0d0)
                          (cl-mpm::update-sim *sim*)
                          (incf *target-displacement* (/ disp-step substeps))
                          (setf *t* (+ *t* (cl-mpm::sim-dt *sim*))))
                        )
                       ;; (incf *target-displacement* -0.01d-3)
                       (push
                        average-disp
                        *data-displacement*)
                       ;; (push
                       ;;  average-force
                       ;;  *data-mp-load*)
                       (push
                        average-reaction
                        *data-load*)

                       ;; (format t "Target load: ~f~%" (* *target-displacement* 20d9))
                       ;; (format t "Current load: ~f~%" (* (get-disp *terminus-mps*) 1d9))
                       ;; (format t "Node Load: ~f~%" (get-reaction-force *fixed-nodes*))
                       ;; (format t "Pen load: ~f~%" average-force)
                       
                       (with-open-file (stream (merge-pathnames "output/disp.csv") :direction :output :if-exists :append)
                         (format stream "~f,~f,~f~%"
                                 average-disp
                                 average-force
                                 average-reaction
                                 )))


                     (format t "Target: ~f - Current: ~f Error: ~f - energy ~F~%"
                             *target-displacement*
                             (get-disp *terminus-mps*)
                             (* 100d0 (/ (- *target-displacement* (get-disp *terminus-mps*)) *target-displacement*))
                             (energy-norm *sim*))
                     ;; (format t "Debug load: ~f~%" cl-mpm/penalty::*debug-force*)
                     ;; (format t "Debug load count: ~f~%" cl-mpm/penalty::*debug-force-count*)

                     ;; (incf *target-displacement* -0.01d-3)

                     ;; (incf *current-load* (* -1d3 1d0))

                     ;; (loop for mp across (cl-mpm:sim-mps *sim*)
                     ;;       do
                     ;;          (when (>= (cl-mpm/particle:mp-damage mp) 1d0)
                     ;;            (let ((ms 1d2))
                     ;;              (setf (cl-mpm::sim-mass-scale *sim*) ms
                     ;;                    ;; target-time 1d0
                     ;;                    (cl-mpm:sim-damping-factor *sim*) (* 1d-8 ms)
                     ;;                    ))))

                     (multiple-value-bind (dt-e substeps-e) (cl-mpm:calculate-adaptive-time *sim* target-time :dt-scale dt-scale)
                       (format t "CFL dt estimate: ~f~%" dt-e)
                       (format t "CFL step count estimate: ~D~%" substeps-e)
                       (setf substeps substeps-e))
                     ;; (setf (cl-mpm:sim-damping-factor *sim*)
                     ;;       (* (cl-mpm:sim-damping-factor *sim*) (expt 1d-3 1/40)))

                     (incf *sim-step*)
                     (swank.live:update-swank)
                     ))))
  (cl-mpm/output:save-vtk (merge-pathnames (format nil "output/sim_~5,'0d.vtk" *sim-step*)) *sim*))
(defun mpi-loop ()
  (let ((rank (cl-mpi:mpi-comm-rank))
        (kappa (uiop:getenv "KAPPA"))
        (lc (uiop:getenv "lc"))
        (refine (uiop:getenv "REFINE")))
    ;; (cl-mpi:mpi-init)
      (setf kappa (if kappa 
                      (parse-float:parse-float kappa)
                      1d0))
      (setf lc (if lc 
                   (parse-float:parse-float lc)
                   1d0))
      (setf refine (if refine
                       (parse-float:parse-float refine)
                       1d0))
      (format t "Kappa ~F - LC ~F - Refine ~F ~%" kappa lc refine)
    (setup kappa lc refine)
    
    ;;Domain
    (setf (cl-mpm/mpi::mpm-sim-mpi-domain-count *sim*) (list (floor (cl-mpi:mpi-comm-size) 1) 1 1))
    (let ((dhalo-size (* 4 (cl-mpm/particle::mp-local-length (aref (cl-mpm:sim-mps *sim*) 0)))))
	    (setf (cl-mpm/mpi::mpm-sim-mpi-halo-damage-size *sim*) dhalo-size))
    (when (= rank 0)
      (format t "Sim MPs: ~a~%" (length (cl-mpm:sim-mps *sim*)))
      (format t "Decompose~%")
    (let ((dhalo-size (cl-mpm/particle::mp-local-length (aref (cl-mpm:sim-mps *sim*) 0))))
		(format t "Damage halo size: ~F ~%" dhalo-size))
      )
    (cl-mpm/mpi::domain-decompose *sim*)
      (format t "New Sim MPs: ~a~%" (length (cl-mpm:sim-mps *sim*)))
    ;; (when (= rank 0)
    ;;   (format t "Run mpi~%"))
    ;; ;(cl-mpi:mpi-barrier)
    ;; (cl-mpm::update-sim *sim*)
    ;; (format t "Time step~%")
    ;; (if (= rank 0)
    ;;     (time (cl-mpm::update-sim *sim*))
    ;;     (cl-mpm::update-sim *sim*))
    ;; (format t "Time sync~%")
    ;; (if (= rank 0)
    ;;     (time (cl-mpm/mpi::mpi-sync-momentum *sim*))
    ;;     (cl-mpm/mpi::mpi-sync-momentum *sim*)
    ;;     )
    ;; (format t "Time damage sync ~%")
    ;; (if (= rank 0)
    ;;     (time  (cl-mpm/mpi::mpi-sync-damage-mps *sim* (cl-mpm/mpi::mpm-sim-mpi-halo-damage-size *sim*)))
    ;;     (cl-mpm/mpi::mpi-sync-damage-mps *sim* (cl-mpm/mpi::mpm-sim-mpi-halo-damage-size *sim*))
    ;;     )
    (format t "Running")
    (run-mpi-static (format nil "output-~f-~f-~f/" refine lc kappa))
    (when (= rank 0)
      (format t "Done mpi~%"))
    )
  (cl-mpi:mpi-finalize)
  (sb-ext:quit)
  )
(defun mpi-average (value mp-count)
  (let ((sum 0d0))
    (static-vectors:with-static-vector (source 1 :element-type 'double-float :initial-element value)
      (static-vectors:with-static-vector (dest 1 :element-type 'double-float :initial-element 0d0)
        (cl-mpi:mpi-allreduce source dest cl-mpi:+mpi-sum+)
        (setf sum (aref dest 0))
        ))
    (static-vectors:with-static-vector (source 1 :element-type 'double-float :initial-element (coerce mp-count 'double-float))
      (static-vectors:with-static-vector (dest 1 :element-type 'double-float :initial-element 0d0)
        (cl-mpi:mpi-allreduce source dest cl-mpi:+mpi-sum+)
        (setf sum (/ sum (min 1d0 (aref dest 0))))
        ))

    sum))
(defun run-mpi (&optional (output-folder "output/"))
  (ensure-directories-exist (merge-pathnames output-folder))
  (defparameter *data-force* '())
  (defparameter *data-displacement* '(0d0))
  (defparameter *data-load* '(0d0))
  (defparameter *data-node-load* '(0d0))
  (defparameter *data-mp-load* '(0d0))
  (defparameter *data-time* '(0d0))
  (defparameter *target-displacement* 0d0)
  (defparameter *data-full-time* '(0d0))
  (defparameter *data-full-load* '(0d0))


  (let* ((target-time 0.5d0)
         (dt (cl-mpm:sim-dt *sim*))
         (substeps (floor target-time dt))
         (dt-scale 1d0)
         (disp-step -0.002d-3)
         (rank (cl-mpi:mpi-comm-rank))
         )

    (when (= rank 0)
      (format t "Outputting mesh and load-disp graph")
      (cl-mpm/output:save-vtk-mesh (merge-pathnames output-folder "mesh.vtk") *sim*)
      (with-open-file (stream (merge-pathnames output-folder "disp.csv") :direction :output :if-exists :supersede)
        (format stream "disp,load,nload~%")
        (format stream "~f,~f,~f~%" 0d0 0d0 0d0)
        ))

    (setf cl-mpm/penalty::*debug-force* 0d0)
    (setf cl-mpm/penalty::*debug-force-count* 0d0)
    (when (= rank 0)
      (format t "Test run~%"))
    (cl-mpm::update-sim *sim*)
    (when (= rank 0)
      (format t "Caluclating dt estimate~%"))
    (multiple-value-bind (dt-e substeps-e) (cl-mpm:calculate-adaptive-time *sim* target-time :dt-scale dt-scale)
      (when (= rank 0)
        (format t "CFL dt estimate: ~f~%" dt-e)
        (format t "CFL step count estimate: ~D~%" substeps-e))
      (setf substeps substeps-e))
    (when (= rank 0)
      (format t "Substeps ~D~%" substeps))
    (incf *target-displacement* disp-step)
    (time (loop for steps from 0 to 100
                while *run-sim*
                do
                (progn
                  (when (= rank 0)
                    (format t "Step ~d ~%" steps))
                  (let ((average-force 0d0)
                        (average-disp 0d0)
                        (average-reaction 0d0))
                    (time
                      (dotimes (i substeps)
                        (defparameter *terminus-mps*
                          (loop for mp across (cl-mpm:sim-mps *sim*)
                                when (= (cl-mpm/particle::mp-index mp) 1)
                                collect mp))
                        (incf average-force (/
                                              cl-mpm/penalty::*debug-force*
                                              substeps
                                              ))
                        (incf average-reaction
                              (/ (get-reaction-force *fixed-nodes*) substeps)
                              )
                        (incf average-disp
                              (/ (get-disp *terminus-mps*) substeps)
                              )
                        ;(incf average-reaction (/ (get-reaction-force *fixed-nodes*) substeps))
                        (setf cl-mpm/penalty::*debug-force* 0d0)
                        (setf cl-mpm/penalty::*debug-force-count* 0d0)

                        (setf (cl-mpm::sim-enable-damage *sim*) nil)
                        (setf cl-mpm/damage::*delocal-counter-max* 0)
                        (when (= i (- substeps 1))
                          (setf (cl-mpm::sim-enable-damage *sim*) t))

                        (cl-mpm::update-sim *sim*)
                        (incf *target-displacement* (/ disp-step substeps))
                        (setf *t* (+ *t* (cl-mpm::sim-dt *sim*))))
                      )
                    ;; (incf *target-displacement* -0.01d-3)
                    (setf average-disp (cl-mpm/mpi::mpi-average average-disp (length *terminus-mps*)))
                    (setf average-force (cl-mpm/mpi::mpi-average average-force (length *terminus-mps*)))
                    (push
                      average-disp
                      *data-displacement*)
                    (push
                      average-force
                      *data-load*)

                    (when (= rank 0)
                      (with-open-file (stream (merge-pathnames output-folder "disp.csv") :direction :output :if-exists :append)
                        (format stream "~f,~f,~f~%"
                                average-disp
                                average-force
                                average-reaction
                                ))))


                  (when (= rank 0)
                    (format t "Target: ~f - Current: ~f Error: ~f - energy ~F~%"
                            *target-displacement*
                            (get-disp *terminus-mps*)
                            (* 100d0 (/ (- *target-displacement* (get-disp *terminus-mps*)) *target-displacement*))
                            (energy-norm *sim*)))
                  (multiple-value-bind (dt-e substeps-e) (cl-mpm:calculate-adaptive-time *sim* target-time :dt-scale dt-scale)
                    ;; (format t "CFL dt estimate: ~f~%" dt-e)
                    ;; (format t "CFL step count estimate: ~D~%" substeps-e)
                    (setf substeps substeps-e))
                  (incf *sim-step*)
                  (swank.live:update-swank)
                  )))
    (cl-mpm/output:save-vtk (merge-pathnames output-folder (format nil "sim_final_~2,'0d.vtk" rank)) *sim*))
  ;(cl-mpm/output:save-vtk (merge-pathnames (format nil "output/sim_~5,'0d.vtk" *sim-step*)) *sim*)
  )

(declaim (notinline converge-quasi-static))
(defun converge-quasi-static-mpi (sim)
  (let* ((fnorm 0d0)
        (oobf 0d0)
        (rank (cl-mpi:mpi-comm-rank))
        (estimated-t 1d-4)
        (dt-scale 0.5d0)
        (substeps (floor estimated-t (cl-mpm:sim-dt sim)))
         ;(substeps 100)
        (converged nil))
    (when (= rank 0)
      (format t "Substeps ~D~%" substeps))
    (loop for i from 0 to 100
          while (and *run-sim*
                     (not converged))
          do
             (progn
               (dotimes (j substeps)
                 (defparameter *terminus-mps*
                   (loop for mp across (cl-mpm:sim-mps sim)
                         when (= (cl-mpm/particle::mp-index mp) 1)
                           collect mp))
                 (setf cl-mpm/penalty::*debug-force* 0d0)
                (cl-mpm:update-sim sim))
               (multiple-value-bind (dt-e substeps-e) (cl-mpm:calculate-adaptive-time sim estimated-t :dt-scale dt-scale)
                 (when (= rank 0)
                   (format t "CFL dt estimate: ~f~%" dt-e)
                   (format t "CFL step count estimate: ~D~%" substeps-e))
                 (setf substeps substeps-e))
               (let ((reaction (cl-mpm/mpi::mpi-average (get-reaction-force *fixed-nodes*) (count-if #'cl-mpm/mesh::node-active *fixed-nodes*)))
                     (penalty (cl-mpm/mpi::mpi-sum cl-mpm/penalty::*debug-force*)))
                 (when (= (cl-mpi:mpi-comm-rank) 0)
                   (format t "Conv step ~D - KE norm: ~E - OOBF: ~E - P: ~E R: ~E~%" i fnorm oobf penalty reaction))
                 (when (and ;(< fnorm 1d-6) (< oobf 5d0)
                            (< (abs (- penalty reaction)) 100d0)
                            )
                   (setf converged t)))
               ;(swank.live:update-swank)
               ))))
(defun run-mpi-static (&optional (output-folder "output/"))
  (ensure-directories-exist (merge-pathnames output-folder))
  (defparameter *data-force* '())
  (defparameter *data-displacement* '(0d0))
  (defparameter *data-load* '(0d0))
  (defparameter *data-node-load* '(0d0))
  (defparameter *data-mp-load* '(0d0))
  (defparameter *data-time* '(0d0))
  (defparameter *target-displacement* 0d0)
  (defparameter *data-full-time* '(0d0))
  (defparameter *data-full-load* '(0d0))


  (let* ((target-time 0.5d0)
         (dt (cl-mpm:sim-dt *sim*))
         (substeps (floor target-time dt))
         (dt-scale 1d0)
         (disp-step -0.002d-3)
         (rank (cl-mpi:mpi-comm-rank))
         )

    (when (= rank 0)
      (format t "Outputting mesh and load-disp graph")
      (cl-mpm/output:save-vtk-mesh (merge-pathnames output-folder "mesh.vtk") *sim*)
      (with-open-file (stream (merge-pathnames output-folder "disp.csv") :direction :output :if-exists :supersede)
        (format stream "disp,load,nload~%")
        (format stream "~f,~f,~f~%" 0d0 0d0 0d0)
        ))

    (setf cl-mpm/penalty::*debug-force* 0d0)
    (setf cl-mpm/penalty::*debug-force-count* 0d0)
    ;; (when (= rank 0)
    ;;   (format t "Test run~%"))
    ;; (cl-mpm::update-sim *sim*)
    ;; (when (= rank 0)
    ;;   (format t "Caluclating dt estimate~%"))
    ;; (multiple-value-bind (dt-e substeps-e) (cl-mpm:calculate-adaptive-time *sim* target-time :dt-scale dt-scale)
    ;;   (when (= rank 0)
    ;;     (format t "CFL dt estimate: ~f~%" dt-e)
    ;;     (format t "CFL step count estimate: ~D~%" substeps-e))
    ;;   (setf substeps substeps-e))

    (cl-mpm/output:save-vtk (merge-pathnames output-folder (format nil "sim_final_~2,'0d.vtk" rank)) *sim*)
    (when (= rank 0)
      (format t "Substeps ~D~%" substeps))
    (incf *target-displacement* disp-step)
    (time (loop for steps from 0 to 100
                while *run-sim*
                do
                (progn
                  (when (= rank 0)
                    (format t "Step ~d ~%" steps))
                  (let ((average-force 0d0)
                        (average-disp 0d0)
                        (average-reaction 0d0))
                    (time
                     (progn
                       ;(setf (cl-mpm::sim-enable-damage *sim*) t)
                       ;(setf cl-mpm/damage::*delocal-counter-max* 0)
                       ;(setf cl-mpm/penalty::*debug-force* 0d0)
                       ;(cl-mpm::update-sim *sim*)
                       (setf (cl-mpm::sim-enable-damage *sim*) nil)
                       (incf *target-displacement* disp-step)
                       (converge-quasi-static-mpi *sim*)
                       (cl-mpm/damage::calculate-damage *sim*)
                       ))
                    ;; (incf *target-displacement* -0.01d-3)
                    (setf average-disp (cl-mpm/mpi::mpi-sum (get-disp *terminus-mps*)))
                    (setf average-force (cl-mpm/mpi::mpi-sum (* cl-mpm/penalty::*debug-force* 2d0)))
                    (setf average-reaction (cl-mpm/mpi::mpi-average (* (get-reaction-force *fixed-nodes*) 2d0) (count-if #'cl-mpm/mesh::node-active *fixed-nodes*)))
                    (push
                     average-disp
                     *data-displacement*)
                    (push
                     average-force
                     *data-load*)

                    (when (= rank 0)
                      (with-open-file (stream (merge-pathnames output-folder "disp.csv") :direction :output :if-exists :append)
                        (format stream "~f,~f,~f~%"
                                average-disp
                                average-force
                                average-reaction
                                )))


                    (when (= rank 0)
                      (format t "Target: ~E - Current: ~E"
                              *target-displacement*
                              average-disp)))
                  ;; (multiple-value-bind (dt-e substeps-e) (cl-mpm:calculate-adaptive-time *sim* target-time :dt-scale dt-scale)
                  ;;   (setf substeps substeps-e))
                  (incf *sim-step*)
                  (swank.live:update-swank)
                  )))
    (cl-mpm/output:save-vtk (merge-pathnames output-folder (format nil "sim_final_~2,'0d.vtk" rank)) *sim*))
  )



(setf lparallel:*kernel* (lparallel:make-kernel 16 :name "custom-kernel"))
(defparameter *run-sim* nil)
;(setup)
;(format t "MP count:~D~%" (length (cl-mpm:sim-mps *sim*)))
;(run)

(mpi-loop) 
