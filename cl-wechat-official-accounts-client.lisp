(in-package :cl-wechat-official-accounts-client)

;;; Common

(defconstant +wechat-api-protocol+ "https")
(defconstant +wechat-api-host+     "api.weixin.qq.com")

(defvar *errcode-to-errmsg-alist*
  '((-1 . "System is busy. Try again later.")
    (0 . "Request successful")
    (40001 . "Incorrect AppSecret or invalid access_token. Check the accuracy of AppSecret or check whether the API is called for a proper Official Account.")
    (40002 . "Invalid credential type")
    (40003 . "Invalid OpenID. Check whether the OpenID (the user) has followed the Official Account or belongs to another Official Account.")
    (40004 . "Invalid media file type")
    (40005 . "Invalid file type")
    (40006 . "Invalid file size")
    (40007 . "Invalid media file ID")
    (40008 . "Invalid message type")
    (40009 . "Invalid image file size")
    (40010 . "Invalid voice file size")
    (40011 . "Invalid video file size")
    (40012 . "Invalid thumbnail file size")
    (40013 . "Invalid AppID. Check whether AppID is correct. Avoid unsupported characters and ensure case sensitivity.")
    (40014 . "Invalid access_token. Check the validity of access_token (whether it is expired) or check whether the API is called for a proper Official Account.")
    (40015 . "Invalid menu type")
    (40016 . "Invalid number of buttons")
    (40017 . "Invalid button type")
    (40018 . "Invalid button name length")
    (40019 . "Invalid button KEY length")
    (40020 . "Invalid button URL length")
    (40021 . "Invalid menu version")
    (40022 . "Invalid sub-menu level")
    (40023 . "Invalid number of sub-menu buttons")
    (40024 . "Invalid sub-menu button type")
    (40025 . "Invalid sub-menu button name length")
    (40026 . "Invalid sub-menu button KEY length")
    (40027 . "Invalid sub-menu button URL length")
    (40028 . "Invalid custom menu user")
    (40029 . "Invalid oauth_code")
    (40030 . "Invalid refresh_token")
    (40031 . "Invalid openid list")
    (40032 . "Invalid openid list length")
    (40033 . "Invalid request characters that are expressed in the form of \uxxxx")
    (40035 . "Invalid parameter")
    (40038 . "Invalid request format")
    (40039 . "Invalid URL length")
    (40048 . "Invalid URL")
    (40050 . "Invalid group ID")
    (40051 . "Invalid group name")
    (40060 . "The specified article_idx is invalid when deleting an article")
    (40117 . "Invalid group name")
    (40118 . "Invalid media_id size")
    (40119 . "Invalid button type")
    (40120 . "Invalid sub-button type")
    (40121 . "Invalid media_id type")
    (40125 . "Invalid appsecret")
    (40132 . "Invalid Weixin ID")
    (40137 . "Invalid image format")
    (40155 . "Do not add a link to the homepage of another Official Account")
    (40163 . "The oauth_code is already used")
    (41001 . "Parameter access_token is missing")
    (41002 . "Parameter appid is missing")
    (41003 . "Parameter refresh_token is missing")
    (41004 . "Parameter secret is missing")
    (41005 . "Media file data is missing")
    (41006 . "Parameter media_id is missing")
    (41007 . "Sub-menu data is missing")
    (41008 . "oauth code is missing")
    (41009 . "openid is missing")
    (42001 . "access_token expired. Check the validity period of access_token. See Get access_token API in Basic Support for details.")
    (42002 . "refresh_token expired")
    (42003 . "oauth_code expired")
    (42007 . "Both access_token and refresh_token became invalid because the user modified the Weixin password. Re-authorization is required.")
    (43001 . "Use GET method")
    (43002 . "Use POST method")
    (43003 . "Use HTTPS method")
    (43004 . "Recipient is not a follower")
    (43005 . "Recipient is not a friend")
    (43019 . "Recipient is in the blacklist")
    (44001 . "Empty media file")
    (44002 . "Empty POST data packet")
    (44003 . "Empty article")
    (44004 . "Empty text message")
    (45001 . "Media file size exceeds the limit")
    (45002 . "Message length exceeds the limit")
    (45003 . "Title length exceeds the limit")
    (45004 . "Description length exceeds the limit")
    (45005 . "URL length exceeds the limit")
    (45006 . "Image URL length exceeds the limit")
    (45007 . "Voice file playback timed out")
    (45008 . "Article length exceeds the limit")
    (45009 . "API calls exceed the limit")
    (45010 . "The number of menus created exceeds the limit")
    (45011 . "API is called at a high frequency. Try again later.")
    (45015 . "Response timed out")
    (45016 . "System groups cannot be modified")
    (45017 . "Group name exceeds the limit")
    (45018 . "The number of groups exceeds the limit")
    (45047 . "The number of downstream customer service messages exceeds the limit")
    (46001 . "Media data does not exist")
    (46002 . "Menu version does not exist")
    (46003 . "Menu data does not exist")
    (46004 . "User does not exist")
    (47001 . "An error occurred while parsing JSON/XML content")
    (48001 . "Unable to access the API. Check whether the Official has access to the API. See API permissions in the Official Accounts Platform > Developer Center.")
    (48002 . "Message rejected by the follower (Receive Messages in the setting options of the Official Account is disabled by the follower)")
    (48004 . "API is suspended. Log in to mp.weixin.qq.com for details.")
    (48005 . "Assets that are referenced by auto replies and custom menus cannot be deleted via API")
    (48006 . "Number of calls cannot be cleared via API because the number of clears exceeds the limit")
    (48008 . "No permission to send this type of message")
    (50001 . "The user is not authorized to use this API")
    (50002 . "The user permission is limited probably because the API is suspended due to unauthorized operation")
    (50005 . "The user has not followed the Official Account")
    (61451 . "Invalid parameter")
    (61452 . "Invalid kf_account")
    (61453 . "kf_account already exists")
    (61454 . "kf_acount length exceeds the limit of 10 characters, excluding @ and the Weixin ID of the Official Account after @.")
    (61455 . "kf_acount name contains invalid characters (only letters and numbers are allowed)")
    (61456 . "The number of kf_accounts exceeds the limit of 10")
    (61457 . "Invalid profile photo file type")
    (61450 . "System error")
    (61500 . "Date format error")
    (63001 . "Empty parameter")
    (63002 . "Invalid signature")
    (65301 . "No personalized menu corresponding to this menuid exists")
    (65302 . "No appropriate user exists")
    (65303 . "Cannot create personalized menus without the default menu")
    (65304 . "Empty MatchRule")
    (65305 . "The number of personalized menus is limited")
    (65306 . "This account does not support personalized menus")
    (65307 . "Personalized menu information is empty")
    (65308 . "There is a button with no response type")
    (65309 . "Personalized menu is disabled")
    (65310 . "Country is required if province and city are specified")
    (65311 . "Province is required if city is specified")
    (65312 . "Invalid country information")
    (65313 . "Invalid province information")
    (65314 . "Invalid city information")
    (65316 . "Up to 3 external redirect URLs can be set in the Official Account menu")
    (65317 . "Invalid URL")
    (87009 . "Invalid signature")
    (9001001 . "Invalid POST data parameter")
    (9001002 . "Unavailable remote service")
    (9001003 . "Invalid ticket")
    (9001004 . "Failed to get the user information via Shake Nearby")
    (9001005 . "Failed to get the merchant information")
    (9001006 . "Failed to get the OpenID")
    (9001007 . "Uploaded file is missing")
    (9001008 . "The file type of uploaded asset is invalid")
    (9001009 . "The file size of uploaded asset is invalid")
    (9001010 . "Upload failed")
    (9001020 . "Invalid account")
    (9001021 . "There is a device with an activation rate of less than 50%. Cannot add a new one.")
    (9001022 . "The number of devices requested must be greater than 0")
    (9001023 . "The application for the device ID is already under review")
    (9001024 . "Up to 50 device IDs can be queried at a time")
    (9001025 . "Invalid device ID")
    (9001026 . "Invalid page ID")
    (9001027 . "Invalid page parameter")
    (9001028 . "Up to 10 page IDs can be delete at a time")
    (9001029 . "The page has been linked to the device. It cannot be deleted unless unlinked from the device.")
    (9001030 . "Up to 50 page IDs can be queried at a time")
    (9001031 . "Invalid time period")
    (9001032 . "The parameter used to save the linking relationship between device and page is invalid")
    (9001033 . "Invalid shop ID")
    (9001034 . "Device remarks exceed the limit")
    (9001035 . "Invalid device application parameter")
    (9001036 . "Invalid begin value"))
  "Generate this alist on page Global Return Code: https://developers.weixin.qq.com/doc/offiaccount/en/Getting_Started/Global_Return_Code.html with JavaScript code: var result = ''; document.querySelectorAll('.table-wrp table tr:not(:first-child)').forEach(function(tr) { result += '(' + tr.children[0].innerText + ' . \"' + tr.children[1].innerText + '\")\n'; }); result = '\'(' + result + ')'; console.log(result);")

;;; WeChat access token API

(defun wechat-api-get-access-token (app-id app-secret
                                    &key
                                      (protocol +wechat-api-protocol+)
                                      (host +wechat-api-host+)
                                      (uri "/cgi-bin/token"))
  "Return access_token\(string\).
  API doc of Getting Access Token: https://developers.weixin.qq.com/doc/offiaccount/Basic_Information/Get_access_token.html
  IMPORTANT NOTE: Before calling the API, you need to log in to the Weixin Official Accounts Platform, and go to Development > Basic Settings to add the server IP address to the IP whitelist; otherwise it will not be called successfully."
  (multiple-value-bind (data status-code headers uri stream must-close-p status-text)
      (drakma:http-request (format nil "~A://~A~A"
                                   protocol
                                   host
                                   uri)
                           :parameters (list (cons "grant_type" "client_credential")
                                             (cons "appid" app-id)
                                             (cons "secret" app-secret)))
    (declare (ignore headers uri stream must-close-p status-text))
    (let ((json (jsown:parse (babel:octets-to-string data :encoding :utf-8))))
      (if (eq 200 status-code)
          (values (jsown:val json "access_token")
                  status-code)
          (values nil
                  status-code)))))

(defclass wechat-official-accounts-client ()
  ((protocol
    :initarg :protocol
    :initform +wechat-api-protocol+
    :reader protocol)
   (host
    :initarg :host
    :initform +wechat-api-host+
    :reader host)
   (app-id
    :initarg :app-id
    :initform (error "Must pass a string as value of slot app-id")
    :reader app-id)
   (app-secret
    :initarg :app-secret
    :initform (error "Must pass a string as value of slot app-secret")
    :reader app-secret)
   (access-token
    :initarg :access-token
    :initform nil
    :reader access-token)
   (auto-refresh-access-token-p
    :initarg :auto-refresh-access-token-p
    :initform t
    :reader auto-refresh-access-token-p)
   (encoding-aes-key
    :initarg :encoding-aes-key
    :initform nil
    :accessor encoding-aes-key)
   (server-token
    :initarg :server-token
    :initform nil
    :accessor server-token)))

(defmethod initialize-instance :after ((client wechat-official-accounts-client) &rest args)
  (declare (ignore args))
  (flet ((init-access-token ()
           (setf (slot-value client 'access-token)
                 (wechat-api-get-access-token (app-id client)
                                              (app-secret client)))))
    (init-access-token)
    (when (slot-value client 'auto-refresh-access-token-p)
      ;; refresh access-token every hour
      (cron:make-cron-job
       #'init-access-token
       :minute 0
       :hash-key
       (intern
        (format nil
                "~:@(wechat-official-accounts-client-access-token-refresh-cron-job-~A~)"
                (app-id client))))
      (cron:start-cron))))

(defun make-wechat-official-accounts-client (app-id app-secret
                                             &key
                                               (auto-refresh-access-token-p t)
                                               encoding-aes-key
                                               server-token)
  (make-instance 'wechat-official-accounts-client
                 :app-id app-id
                 :app-secret app-secret
                 :auto-refresh-access-token-p auto-refresh-access-token-p
                 :encoding-aes-key encoding-aes-key
                 :server-token server-token))

(defmethod print-object ((client wechat-official-accounts-client) stream)
  (print-unreadable-object (client stream :type t :identity t)
    (format stream ":APP-ID ~S" (app-id client))))

(defun message-body-signature (list &key (digest :sha1) (sort-p t))
  "Generate message body signature.
  Parameters:
    list: '\(string\), list of timestamp, nonce, token and encrypted-message
  Return string\(hex string\)
  API doc of Message body signature: https://developers.weixin.qq.com/doc/oplatform/en/Third-party_Platforms/Message_Encryption/Technical_Plan.html#Message-body-signature"
  (->> (if sort-p
           (sort list #'string<)
           list)
       (apply #'concatenate 'string)
       (babel:string-to-octets)
       (ironclad:digest-sequence digest)
       (ironclad:byte-array-to-hex-string)))

(defun text-message-dom-value (dom field)
  "Extract value of field in dom."
  (some-> (dom:get-elements-by-tag-name dom field)
          (elt 0)
          (dom:child-nodes)
          (elt 0)
          (dom:data)))

(defmacro expand-text-message-dom-value (dom field)
  `(dom:data (elt (dom:child-nodes (elt (dom:get-elements-by-tag-name ,dom ,field)
                                        0))
                  0)))

(defun text-message-dom->alist (dom)
  "Convert text message dom to alist.
  API doc of Message body encryption: https://developers.weixin.qq.com/doc/oplatform/en/Third-party_Platforms/Message_Encryption/Technical_Plan.html#Message-body-encryption"
  (list (cons :to-user-name (text-message-dom-value dom "ToUserName"))
        (cons :from-user-name (text-message-dom-value dom "FromUserName"))
        (cons :create-time (some-> (text-message-dom-value dom "CreateTime")
                                   (parse-integer)
                                   (dt:unix-time->universal-time)))
        (cons :msg-type (text-message-dom-value dom "MsgType"))
        (cons :content (text-message-dom-value dom "Content"))
        (cons :message-id (text-message-dom-value dom "MsgId"))))

(defun message-length (message)
  "Extract message length from message byte-vector.
  Parameters:
    message: byte-vector
  Return integer
  API doc of Message body encryption: https://developers.weixin.qq.com/doc/oplatform/en/Third-party_Platforms/Message_Encryption/Technical_Plan.html#Message-body-encryption"
  (loop
     for byte across message and i from 3 downto 0
     sum (ash byte (* i 8))))

(defun message-length-byte-vector (message-length)
  "Generate message length byte-vector\(4-bytes\) like socket.htonl\(x\).
  Parameters:
    message-length: integer, message length
  Return byte-vector"
  (loop
     with result-vector = (make-array 4 :element-type '(unsigned-byte 8))
     for i from 3 downto 0
     for j from 0 to 3
     do (setf (aref result-vector j)
              (coerce (ldb (byte 8 (* i 8)) message-length) '(unsigned-byte 8)))
     finally (return result-vector)))

(defun decrypt-message-inner (encoding-aes-key encrypted-message)
  "Decrypt encrypted message body from WeChat client by user.
  Parameters:
    client: wechat-official-accounts-client
    encrypted-message: string\(base64 string\)
  Return \(values decrypted-message decrypted-app-id\)
  API doc of Decryption method: https://developers.weixin.qq.com/doc/oplatform/en/Third-party_Platforms/Message_Encryption/Technical_Plan.html#Decryption-method"
  (let* ((key-vector (base64:base64-string-to-usb8-array
                      (concatenate 'string encoding-aes-key "=")))
         (decrypt-cipher
          (ironclad:make-cipher 'ironclad:aes
                                :key key-vector
                                :mode 'ironclad:cbc
                                :initialization-vector
                                ;; iv is 0-15 bytes of key-vector
                                (make-array 16
                                            :element-type '(unsigned-byte 8)
                                            :initial-contents
                                            (subseq key-vector 0 16))))
         (encrypted-message-byte-vector (base64:base64-string-to-usb8-array
                                         encrypted-message)))
    ;; AES decrypt
    (ironclad:decrypt-in-place decrypt-cipher encrypted-message-byte-vector)
    (let* ((decrypted-message-byte-vector encrypted-message-byte-vector)
           (message-vector-length (message-length
                                   (subseq decrypted-message-byte-vector 16 20)))
           ;; length of encrypted message and decrypted message is same in AES
           (message-byte-vector-length (length encrypted-message-byte-vector))
           (padding-length (elt decrypted-message-byte-vector
                                (1- message-byte-vector-length))))
      (values
       ;; decrypted message
       (babel:octets-to-string (subseq decrypted-message-byte-vector
                                       ;; ignore random-salt and message-vector-length
                                       (+ 16 4)
                                       ;; decode message-vector-length from 4-bytes data
                                       (+ 16 4 message-vector-length))
                               :encoding :utf-8)
       ;; decrypted app-id
       (babel:octets-to-string (subseq decrypted-message-byte-vector
                                       (+ 16 4 message-vector-length)
                                       (- message-byte-vector-length padding-length))
                               :encoding :utf-8)))))

(defmethod decrypt-message ((client wechat-official-accounts-client) encrypted-message)
  "Decrypt encrypted message body from WeChat client by user.
  Parameters:
    client: wechat-official-accounts-client
    encrypted-message: string\(base64 string\), Encrypt of request body\(XML\) from WeChat client by user
  Return \(values decrypted-message decrypted-app-id\)
  API doc of Decryption method: https://developers.weixin.qq.com/doc/oplatform/en/Third-party_Platforms/Message_Encryption/Technical_Plan.html#Decryption-method"
  (assert (encoding-aes-key client))
  (decrypt-message-inner (encoding-aes-key client) encrypted-message))

(defun pkcs7-padding-count (vector-length block-size)
  "Caculate PKCS #7 padding count."
  (let ((remainder (mod vector-length block-size)))
    (coerce (if (> remainder 0)
                (- block-size remainder)
                block-size)
            '(unsigned-byte 8))))

(defun pkcs7-padding-vector (pkcs7-padding-count)
  "Generate PKCS #7 padding vector."
  (make-array pkcs7-padding-count
              :element-type '(unsigned-byte 8)
              :initial-element pkcs7-padding-count))

(defun encrypt-message-inner (app-id encoding-aes-key message
                              &key
                                (block-size 32)
                                (random-salt nil))
  "Encrypt message body.
  Parameters:
    encoding-aes-key: `EncodingAESKey` setting in page https://mp.weixin.qq.com/advanced/advanced?action=dev&t=advanced/dev
    message: string\(XML\)
  Return string\(base64 string\)
  API doc of Message body encryption: https://developers.weixin.qq.com/doc/oplatform/en/Third-party_Platforms/Message_Encryption/Technical_Plan.html#Message-body-encryption"
  (let* ((message-vector (babel:string-to-octets message :encoding :utf-8))
         (data-vector-without-padding (concatenate 'vector
                                                   ;; random salt
                                                   (or random-salt                ;; 16 bytes
                                                       (ironclad:make-random-salt 16))
                                                   ;; length of message
                                                   (message-length-byte-vector    ;;  4 bytes
                                                    (length message-vector))
                                                   ;; message
                                                   message-vector                 ;;  n bytes
                                                   ;; app-id
                                                   (babel:string-to-octets app-id ;; 15 bytes
                                                                           :encoding :utf-8)))
         ;; length of data without padding
         (data-vector-without-padding-length (length data-vector-without-padding))
         (pkcs7-padding-count (pkcs7-padding-count data-vector-without-padding-length
                                                   block-size))
         (data-vector-with-padding (make-array (+ data-vector-without-padding-length
                                                  pkcs7-padding-count)
                                               :element-type '(unsigned-byte 8)
                                               :initial-contents
                                               (concatenate 'vector
                                                            data-vector-without-padding
                                                            (pkcs7-padding-vector
                                                             pkcs7-padding-count))))
         (key-vector (base64:base64-string-to-usb8-array
                      (concatenate 'string encoding-aes-key "=")))
         (iv (make-array (ironclad:block-length 'ironclad:aes)
                         :element-type '(unsigned-byte 8)
                         :initial-contents (subseq key-vector
                                                   0
                                                   ;; 16
                                                   (ironclad:block-length 'ironclad:aes))))
         (encrypt-cipher (ironclad:make-cipher 'ironclad:aes
                                               :key key-vector
                                               :mode 'ironclad:cbc
                                               :initialization-vector iv)))
    ;; AES encrypt
    (ironclad:encrypt-in-place encrypt-cipher data-vector-with-padding)
    (base64:usb8-array-to-base64-string data-vector-with-padding)))

(defmethod encrypt-message ((client wechat-official-accounts-client) message
                            &key (block-size 32))
  "Encrypt message body.
  Parameters:
    client: wechat-official-accounts-client, require encoding-aes-key and app-id
    message: string\(XML\)
  Return string\(base64 string\)
  API doc of Message body encryption: https://developers.weixin.qq.com/doc/oplatform/en/Third-party_Platforms/Message_Encryption/Technical_Plan.html#Message-body-encryption"
  (assert (encoding-aes-key client))
  (encrypt-message-inner (app-id client)
                         (encoding-aes-key client)
                         message
                         :block-size block-size))

(defun generate-text-message (to-user-name from-user-name content
                              &key (timestamp (write-to-string (dt:get-unix-time))))
  (let ((dom (cxml:parse "<xml>
  <ToUserName><![CDATA[toUser]]></ToUserName>
  <FromUserName><![CDATA[fromUser]]></FromUserName>
  <CreateTime>12345678</CreateTime>
  <MsgType><![CDATA[text]]></MsgType>
  <Content><![CDATA[content]]></Content>
</xml>"
                         (cxml-dom:make-dom-builder))))
    (setf (expand-text-message-dom-value dom "ToUserName")
          to-user-name
          (expand-text-message-dom-value dom "FromUserName")
          from-user-name
          (expand-text-message-dom-value dom "CreateTime")
          timestamp
          (expand-text-message-dom-value dom "Content")
          content)
    (dom:map-document (cxml:make-string-sink :omit-xml-declaration-p t) dom)))

(defmethod generate-encrypted-message ((client wechat-official-accounts-client) message nonce)
  (assert (server-token client))
  "API doc: https://developers.weixin.qq.com/doc/oplatform/Third-party_Platforms/Message_Encryption/Technical_Plan.html"
  (let ((dom (cxml:parse "<xml>
  <Encrypt><![CDATA[encryptedMessage]]></Encrypt>
  <MsgSignature><![CDATA[msgSignature]]></MsgSignature>
  <TimeStamp>12345678</TimeStamp>
  <Nonce><![CDATA[nonce]]></Nonce>
</xml>"
                         (cxml-dom:make-dom-builder)))
        (timestamp (write-to-string (dt:get-unix-time)))
        (encrypted-message (encrypt client message)))
    (setf (expand-text-message-dom-value dom "Encrypt")
          encrypted-message
          (expand-text-message-dom-value dom "MsgSignature")
          (generate-signature (list timestamp
                                    nonce
                                    (server-token client)
                                    encrypted-message))
          (expand-text-message-dom-value dom "TimeStamp")
          timestamp
          (expand-text-message-dom-value dom "Nonce")
          nonce)
    (dom:map-document (cxml:make-string-sink :omit-xml-declaration-p t) dom)))

(defmethod material-batch-get-material ((client wechat-official-accounts-client)
                                        &key
                                          (type "image")
                                          (offset 0)
                                          (count 20)
                                          (uri "/cgi-bin/material/batchget_material"))
  "API doc: https://developers.weixin.qq.com/doc/offiaccount/en/Asset_Management/Get_materials_list.html"
  (assert (< count 20))
  (jsown:parse
   (babel:octets-to-string
    (drakma:http-request (format nil (format nil "~A://~A~A?access_token=~A"
                                             (protocol client)
                                             (host client)
                                             uri
                                             (access-token client)))
                         :method :post
                         :content (jsown:to-json
                                   (jsown:new-js
                                     ("type" type)
                                     ("offset" offset)
                                     ("count" count)))
                         :force-binary t)
    :encoding :utf-8)))

(defmethod message-custom-send ((client wechat-official-accounts-client) to-user message
                                &key
                                  (msg-type "text")
                                  (uri "/cgi-bin/message/custom/send"))
  "API doc: https://developers.weixin.qq.com/doc/offiaccount/en/Message_Management/Service_Center_messages.html#7"
  (jsown:parse
   (babel:octets-to-string
    (drakma:http-request (format nil (format nil "~A://~A~A?access_token=~A"
                                             (protocol client)
                                             (host client)
                                             uri
                                             (access-token client)))
                         :method :post
                         :content (jsown:to-json*
                                   (jsown:new-js
                                     ("touser" to-user)
                                     ("msgtype" msg-type)
                                     (msg-type message)))
                         :force-binary t
                         :external-format-out :utf-8)
    :encoding :utf-8)))

(defmethod menu-create ((client wechat-official-accounts-client) button-list
                        &key (uri "/cgi-bin/menu/create"))
  "API doc: https://developers.weixin.qq.com/doc/offiaccount/en/Custom_Menus/Creating_Custom-Defined_Menu.html"
  (jsown:parse
   (babel:octets-to-string
    (drakma:http-request (format nil (format nil "~A://~A~A?access_token=~A"
                                             (protocol client)
                                             (host client)
                                             uri
                                             (access-token client)))
                         :method :post
                         :content (jsown:to-json*
                                   (jsown:new-js
                                     ("button" button-list)))
                         :force-binary t
                         :external-format-out :utf-8)
    :encoding :utf-8)))

