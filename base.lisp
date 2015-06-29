(in-package #:defclass-triv)


(defun process-class-slot (slot)
  (let ((name (first slot))
        (initform (unless (keywordp (second slot))
                    (second slot))))
    (let* ((slot (if (not (member :initform slot))
                     (append slot `(:initform ,initform))
                     slot))
           (slot (if (not (member-any '(:accessor :reader :writer) slot))
                     (append slot `(:accessor ,name))
                     slot))
           (slot (if (not (member :initarg slot))
                     (append slot `(:initarg ,(intern (symbol-name name) :keyword)))
                     slot))
           (slot (if (null (second (member :initarg slot)))
                     (let ((ia-pos (position :initarg slot)))
                       (append (subseq slot 0 ia-pos)
                               (subseq slot (+ 2 ia-pos))))
                     slot)))
      slot)))

(defmacro defclass-triv (name direct-superclasses &body args)
  (labels ((sym-only-name (x) (when (symbolp x) (symbol-name x))))
    (let* ((option-start (position "&OPTIONS" args :key #'sym-only-name
                                   :test #'equal))
           (options (when option-start (subseq args option-start)))
           (direct-slots (subseq args 0 (or option-start (length args)))))
      `(cl::defclass ,name ,direct-superclasses
         ,(mapcar #'process-class-slot
                  (mapcar #'listify direct-slots))
         ,@options))))
