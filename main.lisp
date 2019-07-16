(defpackage shady
  (:use :cl :glfw))

(in-package :shady)

(declaim (optimize (speed 3) (safety 0)))

(defparameter *vs* '(:vertex-shader . "
#version 330 core
const vec4 vertice[4] = vec4[4] (
    vec4(-1.0,  1.0, 0.0, 1.0),vec4(-1.0, -1.0, 0.0, 1.0),
    vec4( 1.0, -1.0, 0.0, 1.0),vec4( 1.0,  1.0, 0.0, 1.0)
);
void main() {
    gl_Position = vertice[gl_VertexID];
}"))

(defparameter *ps* '(:fragment-shader . "
#version 330 core
out vec4 fragColor;
uniform vec2 iResolution;
const float esc = 20.0;
const int depth = 420;
const float p = 30.0;
vec3 mandel(vec2 z0) {
    float k = 0.0;
    vec2 z = vec2(0.0);
    for(int i = 0; i < depth; ++i) {
        z = vec2(z.x*z.x-z.y*z.y, z.x*z.y*2.0) + z0;
        if (length(z) > esc)
            break;
        k += 1.0;
    }
    float mu = k + 1.0 - log2(log(length(z)));
    return sin(mu*0.1 + vec3(0.0,0.5,1.0));
}
void main() {
    float ar = iResolution.x / iResolution.y;
    vec2 uv = gl_FragCoord.xy / iResolution.yy - vec2(0.66 * ar, 0.5);
    float scale = 0.5;
    vec2 offset = vec2(-0.3, 0.0);
    uv += offset*scale;
    uv /= scale;
    fragColor = vec4(mandel(uv), 1.0);
}"))

(define-condition compile-error (error)
  ((message :initarg :message :reader message)))

(defun check-shader-error (shader)
  "Get the current error status of a shader, throw error if status"
  (let ((error-string (gl:get-shader-info-log shader)))
    (unless (equalp error-string "")
      (progn (format t "~A~%" error-string)
             (error 'compile-error :message error-string)))))

(defun create-shader (src)
  (let ((shader (gl:create-shader (car src))))
    (gl:shader-source shader (cdr src))
    (gl:compile-shader shader)
    (check-shader-error shader)
    shader))

(defun create-render-program (shaders)
  (let ((program (gl:create-program)))
    (dolist (shader shaders) (gl:attach-shader program shader))
    (gl:link-program program)
    (dolist (shader shaders) (gl:delete-shader shader))
    program))

(defun setup-shaders ()
  (create-render-program
   (list (create-shader *vs*)
         (create-shader *ps*))))

(defun render (width height program vao)
  (declare (type (integer 0 4096) width height))
  (gl:viewport 0 0 width height)
  (gl:clear-color 0 0 0 0)
  (gl:clear :color-buffer)
  (gl:bind-vertex-array vao)
  (gl:use-program program)
  (let ((res (gl:get-uniform-location program "iResolution")))
    (gl:uniformf res width height))
  (gl:draw-arrays :triangle-fan 0 4))

(defun save-png (pixels width height path)
  (declare (type (integer 0 4096) width height))
  (let ((stride (* width 4))
        (color-depth 32))
  (cffi:with-foreign-array (p pixels `(:array :unsigned-char ,(* height stride)))
    (let ((surface (sdl2:create-rgb-surface-from
                    p width height color-depth stride)))
      (sdl2-image:save-png surface path)))))

(defun create-image (width height path)
  (with-init-window (:title "shady" :width width :height height
                     :context-version-major 3
                     :context-version-minor 3
                     :opengl-profile :opengl-core-profile
                     :visible nil)
    (let ((program (setup-shaders))
          (vao (gl:create-vertex-array)))
      (render width height program vao))
    (let ((pixels (gl:read-pixels 0 0 width height :rgba :unsigned-byte)))
      (save-png pixels width height path))))

;; ---- standalone app---------------------

(defun read-file-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(opts:define-opts
  (:name :shader
   :description "the shader file to run"
   :short #\s
   :long "shader"
   :arg-parser #'identity)
  (:name :width
   :description "output width"
   :short #\w
   :long "width"
   :arg-parser #'parse-integer)
  (:name :height
   :description "output height"
   :short #\h
   :long "height"
   :arg-parser #'parse-integer)
  (:name :out
   :description "output file"
   :short #\o
   :long "out"
   :arg-parser #'identity))

(defun print-help (condition)
  (declare (ignore condition))
  (write-line "Usage: shady -s FILENAME -w WIDTH -h HEIGHT")
  (opts:exit -1))

(defun handle-compiler-error (condition)
  (format *error-output* "Shader compilation error:~%~A~%" (message condition))
  (opts:exit -1))

(defun main ()
  (let* ((options
          (handler-bind ((opts:unknown-option #'print-help)
                         (opts:missing-arg #'print-help)
                         (opts:arg-parser-failed #'print-help))
            (opts:get-opts)))
         (shader (getf options :shader))
         (width (or (getf options :width) 400))
         (height (or (getf options :height) 300))
         (output (or (getf options :out) "output.png")))
    (when shader
      (setf (cdr *ps*)
            (concatenate 'string
             "#version 330 core
              out vec4 fragColor;
              uniform vec2 iResolution;"
             (read-file-contents (parse-namestring shader)))))
    (handler-bind ((compile-error #'handle-compiler-error))
      (create-image width height output))))

;; (ql:quickload '("cl-glfw3" "cl-opengl" "sdl2-image" "unix-opts"))
;; (create-image 400 300 "out.png")
