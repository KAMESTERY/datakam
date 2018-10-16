
(in-package :controlkam)

(defun header (title)
  (eco-template:header title))

(defun footer (script)
  (eco-template:footer script))

(defun render (content &key (title "Control Kam") (script ""))
  (respond
   (eco-template:layout
    (header title)
    content
    (footer script))))

