sbcl --eval "(ql:quickload (list :cl-glfw3 :cl-opengl :sdl2-image :unix-opts))" ^
     --load main.lisp ^
     --eval "(sb-ext:save-lisp-and-die #p"""shady.exe^""" :toplevel #'shady::main :executable t)"
