;;
;;(in-package :3d-engine)
(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INIT
;;(pushnew "/home/malune/.sbcl/systems/" asdf:*central-registry*)

;;
(asdf:operate 'asdf:load-op :cl-glut)
(asdf:operate 'asdf:load-op :split-sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BLENDER

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NETWORK (CMUCL)
;(defun start-server (port)
;  (let ((server (wire:create-request-server port)))
;    server))

;(defun close-server (server)
;  (wire:destroy-request-server server))

;(defun connect (host port)
;  (wire:remote (wire:connect-to-remote-server host port)
;    (write-string "omgzor")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WINDOW
;; base window class
(defclass gl-window (glut:window) ()
  (:default-initargs :width 800 :height 600 :title "3D Engine v0.0.0"
    :mode '(:double :rgba :depth)))

;;
(defmethod glut:reshape ((wnd gl-window) width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:frustum -1.0 1.0 -1.0 1.0 1.0 100.0)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:viewport 0 0 width height))

;;
(defvar *object* nil)

;;
(defvar *mesh* nil)

;;
(defvar *texture* nil)

;;
(defvar *char* nil)
(defvar *plane* nil)

;;
(defmethod glut:display-window :before ((wnd gl-window))
  (gl:clear-color 0 0 1.0 0)
  ;;(gl:depth-func :less)
  (gl:shade-model :smooth)

 
  ;;
  (gl:enable :texture-2d)

  ;;
  (setf *texture* (make-instance 'gl-texture :width 256 :height 256))
  (load-texture *texture* "/home/malune/projects/texture0.bmp")

  ;;
  (enable-diffuse-shader)

  ;;
  (init-freetype)
  (setf *char* (bind-character (char-code #\a)))
  
  ;;
  (setf *object* (make-instance 'gl-object 
				:enable-normals-p t
				:enable-tex-coords-p t))
  (import-object *object* "/home/malune/projects/blender/4x4quad.obj" t t)
  (activate *object*)

  ;;
  (setf *mesh* (make-instance 'gl-mesh))
  (create-mesh *mesh* 35 35 10.0 10.0)
  (activate *mesh*))

;;
(defvar *last-update* 0)
(defvar *fps-counter* 0)

(defmethod glut:idle ((wnd gl-window))
  (incf *fps-counter*)

  (when (< (+ *last-update* internal-time-units-per-second)
	   (get-internal-real-time))
    (format t "~a fps ~%" *fps-counter*)
    (setf *fps-counter* 0)
    (setf *last-update* (get-internal-real-time)))

  (glut:post-redisplay))

;;
(defvar *camera-rot-x* 0.0)
(defvar *camera-rot-y* 0.0)
(defvar *camera-rot-z* 0.0)

(defmethod glut:display ((wnd gl-window))
  (gl:clear :color-buffer :depth-buffer)
  (gl:load-identity)
  
  ;;
;  (gl:translate 0.0 0.0 -2.0)
;  (gl:bind-texture :texture-2d (first (first *char*)))

  ;(render-character (first (first *char*)) 1.0 1.0 1.0 1.0)

  (gl:translate 0.0 0.0 -2.0)

  (gl:rotate *camera-rot-x* 1.0 0.0 0.0)
  (gl:rotate *camera-rot-y* 0.0 1.0 0.0)
  (gl:rotate *camera-rot-z* 0.0 0.0 1.0)

  
  (gl:push-matrix)

  (bind *texture*)

  (activate *object*)

  (render *object* :triangle-strip)

  (deactivate *object*)

  (gl:pop-matrix)

  (gl:translate 0.0 0.0 -4.0)
  ;;
  (gl:push-matrix)
  (gl:load-identity)
  (gl:translate -5.0 -5.0 -10.0)

  (bind *texture*)

  (activate *mesh* )

  (render *mesh* :line-strip)

  (deactivate *mesh*)
  (gl:pop-matrix)
  
  (glut:swap-buffers))

;;
(defmethod glut:close ((wnd gl-window))
  (destroy *mesh*)
  (destroy *object*))

;;
(defmethod glut:keyboard ((wnd gl-window) key x y)
  (case key
    (#\Esc (glut:destroy-current-window))
    (#\q (incf *camera-rot-x* 0.5))
    (#\w (incf *camera-rot-y* 0.5))
    (#\e (incf *camera-rot-z* 0.5))
    (#\a (decf *camera-rot-x* 0.5))
    (#\s (decf *camera-rot-y* 0.5))
    (#\d (decf *camera-rot-z* 0.5))))

;;
(defun start ()
  (glut:display-window (make-instance 'gl-window)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CAMERA
(defclass gl-camera (node)
  ((near-distance)
   (far-distance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TEXTURE
(defun load-bmp (path)
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence seq in)
      seq)))

(defclass gl-texture (node)
  ((id      :initform (gl:gen-textures 1)
            :accessor id)
   (width   :initarg :width)
   (height  :initarg :height)))

(defmethod load-texture ((tx gl-texture) path)
  (with-slots (id width height) tx
    (let ((image (load-bmp path)))
      (gl:bind-texture :texture-2d (car id))
      (gl:tex-image-2d :texture-2d 0 3 width height 0 :rgb :unsigned-byte image)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear))))

(defmethod set-texture ((tx gl-texture) image)
  (with-slots (id width height) tx
    (gl:bind-texture :texture-2d (car id))
    (gl:tex-image-2d :texture-2d 0 3 width height 0 :luminance-alpha :unsigned-byte image)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)))
   
(defmethod bind ((tx gl-texture))
  (with-slots (id) tx
    (gl:bind-texture :texture-2d (car id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FREETYPE
(cffi:define-foreign-library freetype-binding 
  (:unix "/home/malune/projects/freetype-binding.so"))

(cffi:use-foreign-library freetype-binding)

(cffi:defcstruct freetype-glyph
  (data   :pointer)
  (width  :int)
  (height :int))

(cffi:defcfun "init_freetype_lib" :uint
  (font :string)
  (h    :uint))

(cffi:defcfun "close_freetype_lib" :void)

(cffi:defcfun "free_character_bitmap" :void
  (allocated-char :pointer))

(cffi:defcfun "get_character_bitmap" freetype-glyph
  (ch   :char))

;;
(defun char->ascii (ch) (char-code ch))
(defun ascii->char (i) (code-char i))

;;
(defun init-freetype ()
  (init-freetype-lib "/var/lib/defoma/x-ttcidfont-conf.d/dirs/TrueType/Vera.ttf" 10))

;;
(defun get-freetype-glyph (ch)
  (let ((glyph-ptr (get-character-bitmap ch)))
    (cffi:with-foreign-slots ((data width height) glyph-ptr freetype-glyph)
      (list data width height))))

;;
(defun close-freetype ()
  (close-freetype-lib))

;;
(defun push-font-matrix ()
  (gl:push-attrib :transform-bit)
  (gl:viewport 0 0 800 600)
  (gl:matrix-mode :projection)
  (gl:push-matrix)
  (gl:load-identity)
  (gl:ortho -1.0 1.0 -1.0 1.0 1.0 100.0)
  (gl:translate 0.0 0.0 -2.0)
  (gl:pop-attrib))

;;
(defun bind-character (ch)
  (let* ((glyph  (get-freetype-glyph ch))
	 (tex-id (gl:gen-textures 1))
	 (data   (first glyph))
	 (width  (second glyph))
	 (height (third glyph)))
    (gl:bind-texture :texture-2d (first tex-id))
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rgba width height 0
		     :luminance-alpha :unsigned-byte data)
    (free-character-bitmap data)
    (list tex-id width height)))

;;
(defun generate-character-id (display-list-id tex-id w h)
  (gl:new-list display-list-id :compile)
  (gl:bind-texture :texture-2d tex-id)
  (gl:push-matrix)
  (gl:begin :quads)
  (gl:tex-coord 0 0) (gl:vertex 0 h)
  (gl:tex-coord 0 1.0) (gl:vertex 0 0)
  (gl:tex-coord 1.0 1.0) (gl:vertex w 0)
  (gl:tex-coord 1.0 0) (gl:Vertex w h)
  (gl:end)
  (gl:pop-matrix)
  (gl:end-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FONT
(defclass gl-font (node) ())

(defmethod build-font ((ft gl-font) w h)
  (loop for i from 0 to 255 do
       (let ((glyph (bind-character i)))
	 (generate-character-id i 
				(first glyph)
				(second glyph)
				(third glyph)))))

(defmethod render-string ((ft gl-font) str)
  (loop for ch across str do
       (gl:translate 0.1 0.0 0.0)
       (gl:call-list (char-code ch))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SHADER
(defclass shader (node)
  ((program   :initform (gl:create-program)
	      :accessor program)
   (vx-shader :initform (gl:create-shader :vertex-shader)
	      :accessor vx-shader)
   (vx-source :initform '()
	      :initarg :vx-source
	      :accessor vx-source)
   (px-shader :initform (gl:create-shader :fragment-shader)
	      :accessor px-shader)
   (px-source :initform '()
	      :initarg :px-source
	      :accessor px-source)))

;;
(defmethod create ((sh shader))
  (with-slots (program vx-shader vx-source px-shader px-source) sh
    (gl:shader-source vx-shader vx-source)
    (gl:shader-source px-shader px-source)
    (gl:compile-shader vx-shader)
    (gl:compile-shader px-shader)
    (gl:attach-shader program vx-shader)
    (gl:attach-shader program px-shader)
    (gl:link-program program)
    (format t (gl:get-program-info-log program))
    (gl:use-program program)))

;;
(defun read-file (path)
  (with-open-file (in path :direction :input)
    (loop for line = (read-line in nil) 
       while line collect line)))

;;
(defun enable-ambient-shader ()
  (let ((ambient-shader 
	 (make-instance 'shader 
			:vx-source (read-file "/home/malune/projects/ambient-shader.vx")
			:px-source (read-file "/home/malune/projects/ambient-shader.px"))))
    (create ambient-shader)))

;;
(defun enable-diffuse-shader ()
  (let ((diffuse-shader
	 (make-instance 'shader
			:vx-source (read-file "/home/malune/projects/diffuse-shader.vx")
			:px-source (read-file "/home/malune/projects/diffuse-shader.px"))))
    (create diffuse-shader)
    (with-slots (program) diffuse-shader
      (let ((tex-loc (gl:get-uniform-location program "texture0")))
	(with-slots (id) *texture*
	  (gl:active-texture (car id))
	  (bind *texture*)
	  (gl:uniformi tex-loc (car id)))))))
      

;;
(defun enable-toon-shader ()
  (let ((toon-shader
	 (make-instance 'shader
			:vx-source (read-file "/home/malune/projects/toon-shader.vx")
			:px-source (read-file "/home/malune/projects/toon-shader.px"))))
    (create toon-shader)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VBO
(defclass vbo (node)
  ((id-ptr    :initform (cffi:foreign-alloc :uchar :initial-contents '(0)))
   (data-ptr  :initform 0
	      :initarg :data-ptr)
   (gl-type   :accessor gl-type
	      :initarg :gl-type)
   (el-count  :accessor el-count)
   (target    :accessor target
	      :initarg :target)
   (usage     :accessor usage
	      :initarg :usage)))

;;
(defmethod allocate ((v vbo) data-list)
  (with-slots (id-ptr data-ptr gl-type el-count target usage) v
    (setf el-count (length data-list))
    (setf data-ptr (cffi:foreign-alloc gl-type :initial-contents data-list))
    ;;
    (%gl:gen-buffers 1 id-ptr)
    (%gl:bind-buffer target (cffi:mem-aref id-ptr :uchar))
    (%gl:buffer-data target 
		     (* (cffi:foreign-type-size gl-type) el-count) 
		     data-ptr 
		     usage)))

;;
(defmethod destroy ((v vbo))
  (with-slots (id-ptr data-ptr) v
    (%gl:delete-buffers 1 id-ptr)
    (cffi:foreign-free id-ptr)
    (cffi:foreign-free data-ptr)))

;;
(defmethod destroy-id ((v vbo))
  (with-slots (id-ptr) v
    (%gl:delete-buffers 1 id-ptr)
    (cffi:foreign-free id-ptr)))

;;
(defmethod bind ((v vbo))
  (with-slots (id-ptr target) v
    (%gl:bind-buffer target (cffi:mem-aref id-ptr :uchar))))

;;
(defmethod enable :before ((v vbo) fn)
  (declare (ignore fn))
  (with-slots (id-ptr target) v
    (%gl:bind-buffer target (cffi:mem-aref id-ptr :uchar))))

(defmethod enable ((v vbo) fn)
  (case fn
    (:vertex    (%gl:enable-client-state :vertex-array)
	        (%gl:vertex-pointer 3 :float 0 (cffi:null-pointer)))
    (:normal    (%gl:enable-client-state :normal-array)
	        (%gl:normal-pointer :float 0 (cffi:null-pointer)))
    (:tex-coord (%gl:enable-client-state :texture-coord-array)
		(%gl:tex-coord-pointer 3 :float 0 (cffi:null-pointer)))))
    
(defmethod disable ((v vbo) fn)
  (case fn
    (:vertex     (%gl:disable-client-state :vertex-array))
    (:normal     (%gl:disable-client-state :normal-array))
    (:tex-coord  (%gl:disable-client-state :texture-coord-array))))

(defmethod disable :after ((v vbo) fn)
  (declare (ignore fn))
  (with-slots (target) v
    (%gl:bind-buffer target 0)))

;;
(defmethod render ((v vbo) mode)
  (with-slots (el-count gl-type) v
    (%gl:draw-elements mode el-count (gl::cffi-type-to-gl :unsigned-int) (cffi:null-pointer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OBJECT
(defclass gl-object (node)
  ((indices               :initform 
			  (make-instance 'vbo
					 :gl-type :uint
					 :target :element-array-buffer
					 :usage :static-draw)
			  :accessor indices)
   (vertices              :initform 
			  (make-instance 'vbo
					 :gl-type :float
					 :target :array-buffer
					 :usage :static-draw)
			  :accessor vertices)
   (enable-normals-p      :initform nil
			  :initarg :enable-normals-p)
   (normals               :initform 
			  (make-instance 'vbo
					 :gl-type :float
					 :target :array-buffer
					 :usage :static-draw)
			  :accessor normals)
   (enable-tex-coords-p   :initform nil
			  :initarg :enable-tex-coords-p)
   (tex-coords            :initform
			  (make-instance 'vbo
					 :gl-type :float
					 :target :array-buffer
					 :usage :static-draw))))

(defun split-string (str)
  (loop for i = 0 then (1+ j) 
     as j = (position #\Space str :start i)
     collect (subseq str i j) 
     while j))

(defun string-list->number-list (str-list)
  (loop for str in str-list 
     collect (read-from-string str)))

(defun read-split-file (path)
  (with-open-file (in path :direction :input)
    (loop for line = (read-line in nil)
       while line collect (split-string line))))

(defmethod import-object ((obj gl-object) path nm-flag tx-flag)
  (with-slots (indices vertices normals tex-coords) obj
    (let ((text     (read-split-file path))
	  (ix-count 0)
	  (vx-count 0)
	  (ix-list nil)
	  (vx-list nil)
	  (ixn-list nil)
	  (vn-list nil)
	  (ixt-list nil)
	  (vt-list nil))
      ;;
      (cond ((not (and nm-flag tx-flag))
	     (loop for parameter in text do
		  (cond ((equal (first parameter) "f")
			 (incf ix-count (length (rest parameter)))
			 (setf ix-list (append ix-list (rest parameter))))
			((equal (first parameter) "v") ;; vertex
			 (incf vx-count)
			 (setf vx-list (append vx-list (rest parameter)))))))
	    ((and nm-flag tx-flag)
	     (loop for parameter in text do
		  (cond ((equal (first parameter) "f")
			 (incf ix-count (length (rest parameter)))
			 (setf ix-list 
			       (append ix-list
				       (loop for ix in (rest parameter)
					  collect (first (split-sequence:split-sequence #\/ ix)))))
			 (setf ixt-list
			       (append ixt-list
				       (loop for ix in (rest parameter)
					    collect (second (split-sequence:split-sequence #\/ ix)))))
			 (setf ixn-list
			       (append ixn-list
				       (loop for ix in (rest parameter)
					    collect (third (split-sequence:split-sequence #\/ ix))))))
			((equal (first parameter) "v")
			 (incf vx-count)
			 (setf vx-list (append vx-list (rest parameter))))
			((equal (first parameter) "vt")
			 (format t "reeest:~a~%~%" (rest parameter))
			 (setf vt-list (append vt-list (rest parameter))))
			((equal (first parameter) "vn")
			 (setf vn-list (append vn-list (rest parameter)))))))
	    ((and nm-flag (not tx-flag))
	     (loop for parameter in text do
		  (cond ((equal (first parameter) "f")
			 (incf ix-count (length (rest parameter)))
			 (setf ix-list
			       (loop for ix in ix-list do
				    (append ix-list
					    (first
					     (split-sequence:split-sequence #\/ ix)))))
			 (setf ixn-list
			       (loop for ix in ix-list do
				    (append ixn-list
					    (second
					     (split-sequence:split-sequence #\/ ix))))))
			((equal (first parameter) "v")
			 (incf vx-count)
			 (setf vx-list (append vx-list (rest parameter))))
			((equal (first parameter) "vn")
			 (setf vn-list (append vn-list (rest parameter)))))))
	    ((and tx-flag (not nm-flag))
	     (loop for parameter in text do
		  (cond ((equal (first parameter) "f")
			 (incf ix-count (length (rest parameter)))
			 (setf ix-list
			       (loop for ix in ix-list do
				    (append ix-list
					    (first
					     (split-sequence:split-sequence #\/ ix)))))
			 (setf ixt-list
			       (loop for ix in ix-list do
				    (append ixt-list
					    (second
					     (split-sequence:split-sequence #\/ ix))))))
			((equal (first parameter) "v")
			 (incf vx-count)
			 (setf vx-list (append vx-list (rest parameter))))
			((equal (first parameter) "vt")
			 (setf vt-list (append vt-list (rest parameter))))))))

      ;;
      (format t "vt-list: ~a~%~%~%" vt-list)
      
      ;;
      (format t "ix-count: ~a, vx-count: ~a~%" ix-count vx-count)

      ;;
      (format t "ix-list: ~a ~%~%~% ixn-list: ~a~%~%~% ixt-list: ~a~%~%~%" ix-list ixn-list ixt-list)

      ;; fix broken .obj normals
      (defun element-with (n lst0 lst1)
	(loop for el in lst0
	     for i from 0 do 
	     (when (eql el n) (return (nth i lst1)))))

      (cond ((not (or tx-flag nm-flag))
	     ;;
	     (allocate indices (map 'list #'(lambda (x) (- x 1))
				    (string-list->number-list ix-list)))
	     (allocate vertices   (string-list->number-list vx-list))
	     (allocate normals    '())
	     (allocate tex-coords '()))
	    ((and tx-flag nm-flag) 
	     (pack-model indices vertices normals
			 ix-count 
			 (string-list->number-list ix-list) 
			 (string-list->number-list ixn-list)
			 (string-list->number-list vx-list)
			 (string-list->number-list vn-list))
	     (format t "vt-list: ~a~%~%~%" vt-list)
	     (allocate tex-coords (map 'list #'(lambda (x) (- x 1))
				       (string-list->number-list vt-list)))
	     (format t "vt-list: ~a~%~%~%" (map 'list #'(lambda (x) (- x 1))
						(string-list->number-list vt-list))))))))

;; model conversion algorithm
;; use the ix buffer to generate new ix, vx, tx and nm space
;; grab vx at (element-at vx-list ivx)
;; copy vx to (aref vx-arr ivt)
;; grab nm at (element-at nm-list ivn)
;; copy nm to (aref nm-arr ivt)
;; set (aref ix-arr i) to ivt
(defun pack-model (indices vertices normals
		   ix-count ix-list ixn-list vx-list nm-list)
  (let ((ix-arr (make-array ix-count))
	(vx-arr (make-array (* ix-count 3)))
	(nm-arr (make-array (* ix-count 3))))
    (loop for ix in (map 'list #'(lambda (x) (- x 1)) ix-list)
	 for ixn in (map 'list #'(lambda (x) (- x 1)) ixn-list)
       for ivt from 0 by 1 do
	 (setf (aref vx-arr (* ivt 3)) (nth (* ix 3) vx-list))
	 (setf (aref vx-arr (+ (* ivt 3) 1)) (nth (+ (* ix 3) 1) vx-list))
	 (setf (aref vx-arr (+ (* ivt 3) 2)) (nth (+ (* ix 3) 2) vx-list))
	 (setf (aref nm-arr (* ivt 3)) (nth (* ixn 3) nm-list))
	 (setf (aref nm-arr (+ (* ivt 3) 1)) (nth (+ (* ixn 3) 1) nm-list))
	 (setf (aref nm-arr (+ (* ivt 3) 2)) (nth (+ (* ixn 3) 2) nm-list))
	 (setf (aref ix-arr ivt) ivt))
    (format t "indices: ~a~%~%~%vertices: ~a~%~%~%normals: ~a~%~%~%"
	    ix-arr vx-arr nm-arr)
    (allocate indices ix-arr)
    (allocate vertices vx-arr)
    (allocate normals nm-arr)))
    
;;
(defmethod bind ((obj gl-object))
  (with-slots (indices vertices enable-normals-p normals
		       enable-tex-coords-p tex-coords) obj
    (bind indices)
    (bind vertices)

    (when (eql enable-normals-p t)
      (bind normals))

    (when (eql enable-tex-coords-p t)
      (bind tex-coords))))

;;
(defmethod activate ((obj gl-object))
  (with-slots (indices vertices enable-normals-p normals 
		       enable-tex-coords-p tex-coords) obj
    (enable indices :index)
    (enable vertices :vertex)
    
    (when (eql enable-normals-p t)
      (enable normals :normal))

    (when (eql enable-tex-coords-p t)
      (enable tex-coords :tex-coord))))

;;
(defmethod deactivate ((obj gl-object))
  (with-slots (indices vertices enable-normals-p normals 
		       enable-tex-coords-p tex-coords) obj
    (when (eql enable-tex-coords-p t)
      (disable tex-coords :tex-coord))

    (when (eql enable-normals-p t)
      (disable normals :normal))

    (disable vertices :vertex)
    (disable indices :index)))

;;
(defmethod destroy ((obj gl-object))
  (with-slots (indices vertices normals tex-coords) obj
    (destroy indices)
    (destroy vertices)
    (destroy normals)
    (destroy tex-coords)))
  
;;
(defmethod destroy-id ((obj gl-object))
  (with-slots (indices vertices normals tex-coords) obj
    (destroy-id indices)
    (destroy-id vertices)
    (destroy-id normals)
    (destroy-id tex-coords)))

;;
(defmethod render ((obj gl-object) mode)
  (with-slots (indices) obj
    (render indices mode)))

;;
(defclass gl-triangle (gl-object) ())

(defmethod create ((tri gl-triangle))
  (with-slots (indices vertices normals) tri
    (allocate indices '(0 1 2))
    (allocate vertices '(0.0 0.0 0.0 1.0 0.0 0.0 0.5 1.0 0.0))
    (allocate normals '(0.0 0.0 1.0 0.0 0.0 1.0 0.0 0.0 1.0))))

;;
(defclass gl-mesh (gl-object) ())

(defmethod create-mesh ((mesh gl-mesh) n-slices n-stacks w h)
  (with-slots (indices vertices normals tex-coords) mesh
    ;;
    (let ((vx-array (make-array (* 3
				   (+ (* 2 (+ n-slices 1))
				      (* (+ n-stacks 1) (- n-stacks 1))))))
	  (nm-array (make-array (* 3
				   (+ (* 2 (+ n-slices 1))
				      (* (+ n-stacks 1) (- n-stacks 1))))))
	  (tx-array (make-array (* 2
				   (+ (* 2 (+ n-slices 1))
				      (* (+ n-stacks 1) (- n-stacks 1)))))))
      (let ((i 0))
	(loop for x from 0.0 to w by (/ w n-slices) do
	     (loop for y from 0.0 to h by (/ h n-stacks) do
		  (setf (aref vx-array (* i 3)) x
			(aref nm-array (* i 3)) 0.0
			(aref tx-array (* i 2)) (+ x i)
			(aref vx-array (+ (* i 3) 1)) y
			(aref nm-array (+ (* i 3) 1)) 0.0
			(aref tx-array (+ (* i 2) 1)) (+ y i)
			(aref vx-array (+ (* i 3) 2)) (random 0.5)
			(aref nm-array (+ (* i 3) 2)) 1.0)
		  (incf i))))
      (allocate vertices vx-array)
      (allocate normals nm-array)
      (allocate tex-coords tx-array))
    ;;
    (let ((n-ix (- (+ (* n-stacks (+ 1 1 (* 2 n-slices))) 1) 1400)))
      (allocate indices
		(loop for i from 0 below n-ix 
		   collect (+ i n-slices)
		   collect i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MATH

;; vector
;; matrix
;; quaternion
(defun quaternion (x y z w) (vector x y z w))

(defun qlength (quat)
  (sqrt (+ (* (svref quat 0) (svref quat 0))
	   (* (svref quat 1) (svref quat 1))
	   (* (svref quat 2) (svref quat 2))
	   (* (svref quat 3) (svref quat 3)))))

(defun qnorm (quat)
  (let ((len (qlength quat)))
    (setf (svref quat 0) (/ (svref quat 0) len)
	  (svref quat 1) (/ (svref quat 1) len)
	  (svref quat 2) (/ (svref quat 2) len)
	  (svref quat 3) (/ (svref quat 3) len))
    quat))

(defun qconjugate (quat)
  (setf (svref quat 0) (- (svref quat 0))
	(svref quat 1) (- (svref quat 1))
	(svref quat 2) (- (svref quat 2)))
  quat)

(defun qmul (quat-a quat-b)
  (let ((quat-c (quaternion 0 0 0 0)))
    (setf (svref quat-c 0) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TERRAIN 
;; perlin

;; init
;; bias
(defun bias (a b) 
  (expt a (/ (log b) (log 0.5))))

;; gain
(defun gain (a b)
  (let ((p (/ (log (- 1.0 b)) 
	      (log 0.5))))
    (cond ((< a 0.001) 0.0)
	  ((> a 0.999) 1.0)
	  ((< a 0.5) (/ (expt (* 2 a) p) 2))
	  (t (- 1.0
		(expt (* 2.0 (- 1.0 a))
		      (/ p 2.0)))))))

;;
(defun s-curve (t0)  (* t0 t0 (- 3.0 (* 2.0 t0))))
(defun lerp (t0 a b) (+ a (* t0 (- b a))))

;; noise1, noise2, noise3
(defun noise1 (vec) (declare (ignore vec)) 0)
(defun noise2 (vec) (declare (ignore vec)) 0)
(defun noise3 (vec) (declare (ignore vec)) 0)

(defun noise (vec)
  (case (length vec)
    (0 0)
    (1 (noise1 (svref vec 0)))
    (2 (noise2 vec)))
  (noise3 vec))

;; turbulence
(defun turbulence (v freq)
  (let ((vec #(0.0 0.0 0.0)))
    (loop for tur = 0.0
	 while (>= freq 1.0) do
	 (setf (svref vec 0) (* freq (svref v 0))
	       (svref vec 1) (* freq (svref v 1))
	       (svref vec 2) (* freq (svref v 2)))
	 (incf tur (/ (abs (noise3 vec)) freq))
	 (setf freq (/ freq 2)))))
	 

;; normalize2, normalize3


;; fractal
(defclass fractal (node) ())

;; fractional-brownian-motion
;; hybrid-multi-fractal
;; ridged-multi-fractal

;; neighbour-list
;; add ((n neighbour-list) neighbour)
;; is-neighbour-of ((n neighbour) (n neighbour))

;; fractal-mesh
(defclass fractal-mesh (node) ())

;; destroy (mesh)

;; get-max-elevation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LINDENMAYER FRACTALS (PLANTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PHYSICS
(defclass bounding-volume (node) ())

(defclass bounding-box (bounding-volume)
  ((width  :initform 0.0)
   (height :initform 0.0)))

(defmethod render-volume ((bb bounding-box))
  (with-slots (width height) bb
    (gl:push-matrix)
    (gl:begin :lines)
    (gl:vertex 0.0 0.0 0.0)
    (gl:end)
    (gl:pop-matrix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OBJECT
(defclass node ()
  ((label        :initform 'default
		 :initarg :label
	         :accessor node-label)
   (node-list    :initform nil
	         :initarg  :node-list
	         :accessor node-list)))

;; simple breadth first search
(defmethod lookup ((nd node) label)
  (cond ((equal (node-label nd) label) nd)
	((null (node-list nd)) nil)
	(t (loop for obj in (node-list nd) 
		collect (lookup obj label)))))

(defmethod add-object ((nd node) (cam gl-camera) label)
  (with-slots (node-list) (lookup nd label)
    (setf node-list (cons cam node-list))))

(defmethod add-object ((nd node) (obj gl-object) label)
  (with-slots (node-list) (lookup nd label)
    (setf node-list (cons obj node-list))))