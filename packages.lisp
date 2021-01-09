(in-package :cl-user)

(defpackage cl-wechat-official-accounts-client
  (:use :cl)
  (:nicknames :wechat-official-accounts-client :wxoa-client :wxoa)
  #+:sbcl (:shadow :defconstant)
  #+:sb-package-locks (:lock t)
  (:export
   ;; constants
   #:+wechat-api-protocol+
   #:+wechat-api-host+
   ;; classes
   #:wechat-official-accounts-client
   ;; accessors
   #:protocol
   #:host
   #:app-id
   #:app-secret
   #:access-token
   #:auto-refresh-access-token-p
   #:proxy                     ;; e.g. '("127.0.0.1" 8080)
   #:proxy-basic-authorization ;; e.g. '("username" "password")
   #:encoding-aes-key
   #:server-token
   ;; functions
   #:make-wechat-official-accounts-client
   #:decrypt-message
   #:encrypt-message
   #:material-batch-get-material
   #:message-custom-send
   #:menu-create
   ;; utility functions
   #:message-body-signature
   #:decrypt-message-inner
   #:encrypt-message-inner
   #:generate-text-message
   #:generate-encrypted-message
   #:text-message-dom-value
   #:expand-text-message-dom-value
   #:text-message-dom->alist
   #:message-length
   #:message-length-byte-vector
   #:pkcs7-padding-count
   #:pkcs7-padding-vector))

(in-package :cl-wechat-official-accounts-client)

#+sbcl
(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
