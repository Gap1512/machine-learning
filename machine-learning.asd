(asdf:defsystem #:machine-learning
  :description "Codigos desenvolvidos durante curso de aprendizagem de maquina"
  :author "Gustavo Alves Pacheco <gap1512@gmail.com>"
  :serial t
  :depends-on (#:eazy-gnuplot)
  :components ((:file "package")
	       (:file "./hebb/hebb")
	       (:file "./perceptron-adaline/perceptron-adaline")))