(defsystem "cl-wechat-official-accounts-client"
  :name "cl-wechat-official-accounts-client"
  :description "WeChat Official Accounts client for server API in Common Lisp."
  :version "0.0.1"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "ISC"
  :depends-on ("drakma"
               "jsown"
               "cl-cron"
               "ironclad"
               "cxml"
               "cl-datetime"
               "babel")
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "cl-wechat-official-accounts-client")))
