(in-package :cl-wechat-official-accounts-client)

(defmacro -> (input &rest rest)
  "Examples:

\(-> 1
    \(+ 2\)
    \(* 3\)
    \(- 5\)\)
->
\(- \(* \(+ 1 2\)
      3\)
   5\)

\(-> 1
    1+
    \(* 3\)
    \(- 5\)\)
->
\(- \(* \(1+ 1\)
      3\)
   5\)

\(-> 1
    \(lambda \(x\) \(+ x 2\)\)
    \(* 3\)
    \(- 5\)\)\)
->
\(- \(* \(\(lambda \(x\) \(+ x 2\)\)
       1\)
      3\)
   5\)"
  (loop with result = input
     for lambda-list in rest
     do  (setf result
               (if (and (listp lambda-list)
                        (not (or (eq 'lambda (car lambda-list)))))
                   (append (list (car lambda-list) result)
                           (rest lambda-list))
                   (list lambda-list result)))
     finally (return result)))

(defmacro ->> (input &rest rest)
  "Examples:
\(->> 1
     \(+ 2)
     \(* 3)
     \(- 5))
->
\(- 5
   \(* 3
      \(+ 2 1)))"
  (loop with result = input
     for lambda-list in rest
     do (setf result
              (if (and (listp lambda-list)
                       (not (eq 'lambda (car lambda-list))))
                  (append (list (car lambda-list))
                          (rest lambda-list)
                          (list result))
                  (list lambda-list result)))
     finally (return result)))

(defmacro some-> (input &rest rest)
  "Examples:
\(some-> 1
        \(1+\)
        \(/ 3\)
        \(* 4\)\)
->
\(let* \(\(g_xxx 1\)
       \(g_xxx (when g_xxx \(-> g_xxx \(1+\)\)\)\)
       \(g_xxx (when g_xxx \(-> g_xxx \(/ 3\)\))\)\)
  \(when g_xxx
    \(-> g_xxx \(* 4\)\)\)\)"
  (let* ((symbol (gensym))
         (steps (mapcar (lambda (step)
                          `(when ,symbol
                             (-> ,symbol ,step)))
                        rest)))
    `(let* ((,symbol ,input)
            ,@(mapcar #'(lambda (step)
                          (list symbol step))
                      (butlast steps)))
       (when ,symbol
         ,@(last steps)))))
