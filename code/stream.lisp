(in-package :cl-user)
(defpackage #:clos-streams
  (:nicknames #:cs)
  (:use :cl)
  (:shadow #:input-stream-p
           #:output-stream-p
           #:force-output
           #:finish-output
           #:close
           #:stream-element-type
           #:read-char
           #:write-char
           #:read-byte
           #:write-byte))

(in-package #:clos-streams)

(proclaim '(declaration values))

;;;; stream 类

;;; 这个基础类必须被所有的流继承
(defclass stream ()
  ((state :initform 'open :accessor stream-state))
  (:documentation "Foundation of all streams."))

;;;; 所有流都支持的外部协议

;;; Common LISP: The Language 第 332 页
(defgeneric input-stream-p (stream)
  ;; 输入流应该覆盖这个默认方法
  (:method ((stream stream)) nil))

;;; Common LISP： The Language 第 332 页
(defgeneric output-stream-p (stream)
  ;; 输出流应该覆盖这个默认方法
  (:method ((stream stream)) nil))

;;; Common LISP： The Language 第 332 页
(defgeneric close (stream &key abort)
  (:documentation "Rrevents further I/O operations on stream")
  (:method ((stream stream) &key abort)
    (declare (ignore abort))
    (setf (stream-state stream) 'closed)))

;;; Common LISP： The Language 第 332 页
;;; 必须为 元素类型流 提供实现
(defgeneric stream-element-type (stream)
  (:documentation "Returns the type of elements of stream"))

;;;; 所有流均支持的内部协议

(defun ensure-open-stream (stream)
  "Prevents access to a stream if it is not open."
  (let ((state (stream-state stream)))
    (unless (eq state 'open)
      (error "Attempt to use stream ~A which is -A"
             stream state))))

;;; 必须为元素类型流实现 bytes-per-element
(defgeneric bytes-per-element (stream)
  (declare (values n-bytes))
  (:documentation "Returns length of one element, in 8-bit bytes."))

;;; 必须为设备流实现 storage-unit-size
(defgeneric storage-unit-size (stream)
  (declare (values n-bytes))
  (:documentation "Returns size of i/o buffer, in 8-bit bytes."))

(defun make-element-array (stream)
  "Returns array of correct size and element type for stream."
  (make-array (/ (storage-unit-size stream)
                 (bytes-per-element stream))
              :element-type (stream-element-type stream)))

;;;; input-stream 类和它的方法

;;; 这个基础类必须被所有的输入流继承
(defclass input-stream (stream) ()
  (:documentation "Foundation of all input streams."))

;;; 覆盖默认的 primary 以返回 T
(defmethod input-stream-p ((stream input-stream))
  t)

;;;; 所有输入流都支持的内部协议

;;; 确保流在允许输入之前是打开的
(defgeneric read-next-element (input-stream)
  (declare (values element eof-p))
  (:method :before ((stream input-stream))
    ;; 此方法确保在 read 之前流是打开的
    ;; 它被所有元素类型的输入流继承，所以它保存了每一个方法，避免复制这些代码
    (ensure-open-stream stream)))

;;; 这个 stream 的默认方法会被 input-stream 覆盖。
;;; 定义它只是为了在这种情况发生时提供一个可理解的错误消息
;;; 并使所有外部函数不需要检查参数 stream 的类型
(defmethod read-next-element ((stream stream))
  (error "Cannot get input from stream ~A of type ~A."
         stream (type-of stream)))

;;;; output-stream 类和它的方法

;;; 这个基础类必须被所有的输出类继承
(defclass output-stream (stream) ()
  (:documentation "Foundation of all output streams."))

;;; 覆盖默认的方法以返回 T
(defmethod output-stream-p ((stream output-stream))
  t)

;;;; 所有输出流均支持的外部协议

;;; 尽管 COMMON LISP 暗示force-output和finish-output仅由字符流支持
;;; 但它们应该适用于所有输出输出流，下面我们就是这样做的。

;;; 此外，因为COMMON LISP指定force-output和finish-output的stream参数
;;; 是可选的，所以我们不能直接将这些操作作为泛型函数实现。因此，我们将
;;; force-output-internal和finish-output-internal定义为泛型函数，
;;; 它们都属于内部协议。

;;; 对流变量进行标准化（如果给定了 t 或 nil）
(defmacro standardize-output-stream-var (stream)
  `(setf ,stream (cond ((eq ,stream t) *terminal-io*)
                       ((null ,stream) *standard-output*)
                       (t ,stream))))

;;; Common LISP: The Language 第 384 页
(defun force-output (&optional (stream *standard-output*))
  (standardize-output-stream-var stream)
  (force-output-internal stream))

;;; Common LISP: The Language 第 384 页
(defun finish-output (&optional (stream *standard-output*))
  (standardize-output-stream-var stream)
  (finish-output-internal stream))

;;;; 所有输出流均支持的内部协议

(defgeneric force-output-internal (output-stream)
  (:method :before ((stream output-stream))
    ;; 流必须是打开的，否则就生成一个错误
    (ensure-open-stream stream)))

(defgeneric finish-output-internal (output-stream)
  (:method :before ((stream output-stream))
    ;; 流必须是打开的，否则就生成一个错误
    (ensure-open-stream stream)))

(defgeneric write-next-element (output-stream element)
  (:method :before ((stream output-stream) element)
    (declare (ignore element))
    ;; 默认方法确保在写入之前流是打开的。这个方法被所有元素类型的输出流继承
    ;; 因此它保存了每个方法，避免了复制这段代码
    (ensure-open-stream stream)))

;;; stream 的这个默认方法会被输出流覆盖。定义它只是为了在这种情况发生时给出一个
;;; 可理解的错误消息，并使所有的用户接口函数不必检查 stream 参数的类型
(defmethod write-next-element ((stream stream) element)
  (declare (ignore element))
  (error "Cannot do output to stream ~A of type ~A."
         stream (type-of stream)))

;;;; bidirectional-stream 类

;;; 这个类继承所有需要的方法，而不提供自己的方法。
(defclass bidirectional-stream
    (input-stream output-stream)
  ()
  (:documentation "A combinded input and output stream."))





;;;; tape-stream 类及它的方法

(defclass tape-stream (stream)
  ((unit :accessor tape-unit :initform 0 :initarg :unit)
   (tape-handle :accessor tape-handle :initform nil)
   (tape-record-size :allocation :class :initform *tape-record-byte-size*
                     :reader storge-unit-size)
   (element-buffer :accessor element-buffer)
   (buffer-index :accessor buffer-index))
  (:documentation "A stream for accessing a tape device."))


(defmethod initialize-instance :after ((stream tape-stream) &key)
  (with-accessors ((tape-handle tape-handle)
                   (unit tape-unit)
                   (element-buffer element-buffer))
      stream
    (setf tape-handle (open-tape-unit unit))
    (setf element-buffer (make-element-array stream))))

;;; 关闭 tape 单元，然后清除 tape-handle
(defmethod close ((stream tape-stream) &key abort)
  (declare (ignore abort))
  (with-accessors ((tape-handle tape-handle))
      stream
    (when tape-handle
      (close-tape-unit tape-handle)
      (setf tape-handle nil))))

;;;; tape-input-stream 类及其方法

(defclass tape-input-stream (tape-stream input-stream)
  ;; EOF 在缓冲区中的位置
  ;; 如果还未到达 EOF 则为 nil
  ((eof-offset :initform nil :accessor eof-offset)
   ;; 为这个继承的槽提供一个默认值
   (buffer-index :initform nil))
  (:documentation "A stream for getting input from a tape device."))

(defmethod read-next-element ((stream tape-input-stream))
  (with-accessors ((element-buffer element-buffer)
                   (buffer-index buffer-index)
                   (tape-handle tape-handle)
                   (eof-offset eof-offset))
      stream
    ;; 确保输入缓冲区包含了想要的数据
    (unless (and buffer-index
                 (< buffer-index (length element-buffer)))
      ;; The current buffer does not contain the desired element.
      ;; Read the next record,
      (multiple-value-bind (eof byte-offset)
          (read-record tape-handle element-buffer
                       (stream-element-type stream))
        (if eof
            (setf eof-offset (/ byte-offset (bytes-per-element stream))))
        (setf buffer-index 0)))
    ;; Return nil t if EOF is reached
    (if (and eof-offset (>= buffer-index eof-offset))
        (values nil t)
        ;; Otherwise return data element and update buffer index
        (progl (aref element-buffer buffer-index)
               (incf buffer-index)))))

;;;; tape-output-stream 类及其方法

(defclass tape-output-stream (tape-stream output-stream)
  ((buffer-index :initform 0))
  (:documentation "A stream for writing output to a tape device."))

(defmethod write-next-element ((stream tape-output-stream) element)
  (with-accessors ((element-buffer element-buffer)
                   (buffer-index buffer-index)
                   (tape-handle tape-handle))
      stream
    (unless (< buffer-index (length element-buffer))
      ;; index is past the end of the buffer, so we
      ;; need to write out the buffer and update index
      (write-record tape-handle element-buffer
                    (stream-element-type stream))
      (setf buffer-index 0))
    (setf (aref element-buffer buffer-index) element)
    (incf buffer-index)))

;;; For close :abort, rewind tape immediately and write EOF.
;;; For normal close, write out remaining buffered data (if
;;; necessary) and then write EOF.
(defmethod close :before ((stream tape-output-stream) &key abort)
  (with-accessors ((tape-handle tape-handle))
      stream
    (if abort
        (rewind tape-handle)
        ;; No need to write buffer out if there is no buffer-index
        ;; because that implies that no writing has begun
        (unless (zerop buffer-index)
          (write-record tape-handle element-buffer
                        (stream-element-type stream)
                        ;; include the size argument
                        (* buffer-index
                           (bytes-per-element stream)))))
    (write-eof-mark tape-handle)))

;;; Neither force-output-internal nor finish-output-internal
;;; should write an incomplete tape record to the tape device,
;;; because it would then be impossible to continue to do
;;; output at the correct tape position. Therefore the two
;;; methods below don't do anything.

(defmethod force-output-internal ((stream tape-output-stream))
  nil) ;nil is the documented returned value.

(defmethod finish-output-internal ((stream tape-output-stream))
  nil) ;nil is the documented returned value.



;;;; THE DISK-STREAM CLASS

;; Disk streams are built on the basic class stream
(defclass disk-stream (stream)
  ;; Handle to file returned by OS
  ((file-handle :initform nil
                :accessor file-handle)
   ;; Name of the file for the OS
   (pathname :initarg :pathname
             :accessor disk-pathname)
   ;; Size of a disk sector, expressed in 8-bit bytes
   (disk-sector-size :allocation :class
                                 :initform *disk-sector-byte-size*
                                 :reader storage-unit-size)
   ;; Position of current element within file
   (element-number :initform 0
                   :accessor element-number)
   ;; Total number of elements in disk file
   (element-length :accessor element-length)
   ;; Element buffer, used for I/O
   (element-buffer :accessor element-buffer)
   ;; Index into element buffer or NIL if uninitialized
   (buffer-index :initform nil
                 :accessor buffer-index)
   ;; Disk block number of buffer,
   (block-number :initform 0
                 :accessor block-number))
  (:documentation "A stream for accessing a disk file."))

;;; This method does a lot of initialization, and some of
;;; it depends on happening in a certain order. Hence an
;;; initialization method is preferable to initforms,
;;; whose execution order is not defined,
(defmethod initialize-instance :after ((stream disk-stream) &key)
  ;; we use with-accessors for convenient access to the slots
  (with-accessors ((file-handle file-handle)
                   (element-length element-length)
                   (pathname disk-pathname)
                   (element-buffer element-buffer))
      stream
    (setf file-handle (open-disk-file (namestring pathname)))
    (setf element-buffer (make-element-array stream))
    (setf element-length (/ (byte-length file-handle)
                            (bytes-per-element stream)))))

;;;; EXTERNAL PROTOCOL SUPPORTED BY ALL DISK STREAMS

;;; set-position is an extension to COMMON LISP, which
;;; we include in the External Interface. It allows
;;; greater control when accessing disk streams.

;;; This sets the current position to the desired element
;;; position. If the element position is beyond the end
;;; of the file, eof-error-p and eof-value define whether
;;; an error or a value is returned. The element position
;;; is the element number in the file, where the first
;;; element is number 0, the next is 1, and so on.
(defgeneric set-position (disk-stream new-position
                          &optional eof-error-p eof-value)
  (:method ((stream disk-stream) new-position
            &optional eof-error-p eof-value)
    (with-accessors ((element-buffer element-buffer)
                     (buffer-index buffer-index)
                     (element-length element-length)
                     (file-handle file-handle)
                     (block-number block-number))
        stream
      ;; Don't allow setting position past end of file
      (if (> new-position element-length)
          (if eof-error-p
              (error "End of file in ~A" stream)
              eof-value)
          ;; Here, new-position is OK
          (multiple-value-bind (block-no offset)
              (truncate new-position (length element-buffer))
            ;; Unless current buffer is valid
            (unless (and buffer-index
                         ;; And contains the same block
                         (= block-number block-no))
              ;; Have to read in the desired block
              (setf block-number block-no)
              (disk-read file-handle element-buffer block-number
                         (stream-element-type stream)))
            (setf buffer-index offset)
            (setf element-number new-position))))))

(defmethod close ((stream disk-stream) &key abort)
  (declare (ignore abort))
  (with-accessors ((file-handle file-handle))
      stream
    ;; Checking and clearing the file-handle isn't essential
    ;; but is good practice in case CLOSE is called multiple
    ;; times, especially CLOSE then CLOSE :ABORT T
    (when file-handle
      (close-disk-file file-handle)
      (setf file-handle nil))))

;;;; THE CLASS DISK-INPUT-STREAM AND ITS METHODS

(defclass disk-input-stream (disk-stream input-stream) ()
  (:documentation "A stream for getting input from a disk."))

(defmethod read-next-element ((stream disk-input-stream))
  (with-accessors ((element-number element-number)
                   (element-length element-length)
                   (buffer-index buffer-index)
                   (element-buffer element-buffer)
                   (block-number block-number)
                   (file-handle file-handle))
      stream
    (cond ((< element-number element-length)
           ;; Make sure the input buffer contains the desired data.
           (unless (and buffer-index
                        (< buffer-index (length element-buffer)))
             ;; Unless at beginning, advance to next block,
             (if buffer-index (incf block-number))
             (disk-read file-handle element-buffer block-number
                        (stream-element-type stream))
             (setf buffer-index 0))
           ;; Update pointers and return data element,
           (incf element-number)
           (progl (aref element-buffer buffer-index)
                  (incf buffer-index)))
          (t ;; At EOF
           (values nil t)))))

;;;; THE CLASS DISK-OUTPUT-STREAM AND ITS METHODS

(defclass disk-output-stream (disk-stream output-stream)
  ;; disk-id is used to store the identifier returned
  ;; by disk-write, which we will use to find out if
  ;; the disk-write is still in progress,
  ((disk-id :initform nil
            :accessor disk-id))
  (:documentation "A stream for writing output to a disk."))

;;; This comes in useful when we need to ensure that
;;; there is no disk-write currently in progress,
(defmethod wait-for-disk ((stream disk-output-stream))
  (with-accessors ((disk-id disk-id))
      stream
    (unless (null disk-id)
      ;; process-wait is not part of COMMON LISP, but we
      ;; defined it earlier, in "Locks and Processes"
      (process-wait "Disk wait" #'disk-finished-p disk-id)
      (setf disk-id nil))))

(defmethod write-next-element
    ((stream disk-output-stream) element)
  (with-accessors ((element-buffer element-buffer)
                   (buffer-index buffer-index)
                   (block-number block-number)
                   (element-number element-number)
                   (element-length element-length)
                   (file-handle file-handle))
      stream
    ;; Ensure that no disk write is happening,
    (wait-for-disk stream)
    (unless (and buffer-index (< buffer-index (length element-buffer)))
      ;; Current buffer does not contain the desired element,
      (when buffer-index
        ;; Write out the old buffer and update the pointers,
        (disk-write file-handle element-buffer block-number
                    (stream-element-type stream))
        (incf block-number))
      ;; Need to read in next block in case we are
      ;; overwriting an existing file,
      (when (< element-number element-length)
        (disk-read file-handle element-buffer block-number
                   (stream-element-type stream)))
      (setf buffer-index 0))
    (setf (aref element-buffer buffer-index) element)
    (incf buffer-index)
    (incf element-number)
    ;; Update the EOF pointer as well, but defer the actual
    ;; setting of the EOF pointer on the disk until CLOSE (or
    ;; FINISH-OUTPUT) time to reduce overhead. FINISH-OUTPUT
    ;; should be called anyway when dealing with files which
    ;; are being read by other processes,
    (when (>= element-number element-length)
      (setf element-length element-number))))

;;; The methods for force-output-internal and finish-output-internal
;;; check whether a force-output-internal is already in progress.
;;; If so, they don't do an additional, unnecessary disk-write.
(defmethod force-output-internal ((stream disk-output-stream))
  (with-accessors ((buffer-index buffer-index)
                   (file-handle file-handle)
                   (element-buffer element-buffer)
                   (disk-id disk-id)
                   (block-number block-number))
      stream
    (unless disk-id
      ;; A force-output-internal is not already in
      ;; progress, so we start one.
      (when buffer-index
        ;; The current buffer contents are valid. Write them
        ;; out. Don't change any of the pointers in case
        ;; output is simply continued.
        (setf disk-id (disk-write file-handle element-buffer
                                  block-number
                                  (stream-element-type stream)
                                  :wait nil)))))
  nil) ;; nil is the documented returned value.

(defmethod finish-output-internal ((stream disk-output-stream))
  (with-accessors ((file-handle file-handle)
                   (buffer-index buffer-index)
                   (element-length element-length)
                   (disk-id disk-id))
      stream
    ;; Don't do anything if buffer is invalid
    (when buffer-index
      (unless disk-id
        ;; A force-output-internal is not already in
        ;; progress, so we start one.
        (force-output-internal stream))
      ;; Also, update the EOF pointer on the disk. It's OK
      ;; if the operating system causes this to hang until
      ;; the disk is updated. Note: this could be optimized
      ;; to do this only if the value has changed,
      (setf (byte-length file-handle)
            (* element-length (bytes-per-element stream)))
      ;; And then wait for it to finish,
      (wait-for-disk stream))))

(defmethod set-position :before ((stream disk-output-stream)
                                 new-position
                                 Soptional eof-error-p eof-value)
  (declare (ignore new-position eof-error-p eof-value))
  ;; Before a new disk block can be read in containing the
  ;; new position, we have to write out the old block if it
  ;; has been modified. Note: this could be improved by
  ;; seeing whether the new-position argument is still in
  ;; the same buffer, and not doing finish-output-internal
  ;; in that case,
  (finish-output-internal stream))

;;; This needs to be done before the primary methods are
;;; called, to prepare the file to be closed by first sending
;;; out any buffered output.
(defmethod close :before ((stream disk-output-stream) &key abort)
  (unless abort
    (finish-output-internal stream)))

;;;; THE CLASS DISK-BIDIRECTIONAL-STREAM

(defclass disk-bidirectional-stream
    (disk-input-stream disk-output-stream bidirectional-stream)
  ()
  (:documentation "A combined input and output disk stream."))


;;; Bidirectional streams do both reading and writing.
;;; Before reading, ensure that no disk-write is happening.
(defmethod read-next-element :before ((stream disk-bidirectional-stream))
  (wait-for-disk stream))




;;;; byte-stream

(defclass byte-stream (stream) ()
  (:documentation "A stream for transmitting bytes of data."))

(defclass 8-bit-byte-stream (byte-stream)
  ((bytes-per-element :allocation :class
                      :initform 1
                      :reader bytes-per-element)
   (element-type :allocation :class
                 :initform '(unsigned-byte 8)
                 :reader stream-element-type))
  (:documentation "A stream for transmitting 8-bit bytes of data."))

(defclass 32-bit-word-stream (byte-stream)
  ((bytes-per-element :allocation :class
                      :initform 4
                      :reader bytes-per-element)
   (element-type :allocation :class
                 :initform '(signed-byte 32)
                 :reader stream-element-type))
  (:documentation "A stream for transmitting 32-bit words of data."))

;;;; byte-input-stream 类和方法

(defclass byte-input-stream (byte-stream input-stream) ())

;;; Common LISP: The Language 第 382 页
(defgeneric read-byte (byte-input-stream
                       &optional eof-error-p eof-value)
  (:method ((stream byte-input-stream)
            &optional eof-error-p eof-value)
    (multiple-value-bind (element eof-p)
        (read-next-element stream)
      (cond (eof-p
             (if eof-error-p
                 (error "End of file while reading from ~A" stream)
                 eof-value))
            (t element)))))

;;;; byte-output-stream 类和方法

(defclass byte-output-stream (byte-stream output-stream) ())

;;; Common LISP: The Language 第 385 页
(defgeneric write-byte (output-stream byte)
  (:method ((stream byte-output-stream) byte)
    (write-next-element stream byte)))


;;;; 其它 byte-stream 类

(defclass 8-bit-byte-input-stream (8-bit-byte-stream byte-input-stream)
  ())

(defclass 8-bit-byte-output-stream (8-bit-byte-stream byte-output-stream)
  ())

(defclass 8-bit-byte-bidirectional-stream
    (8-bit-byte-input-stream
     8-bit-byte-output-stream
     bidirectional-stream)
  ())

(defclass 32-bit-word-input-stream
    (32-bit-word-stream byte-input-stream)
  ())

(defclass 32-bit-word-output-stream
    (32-bit-word-stream byte-output-stream)
  ())

(defclass 32-bit-word-bidirectional-stream
    (32-bit-word-input-stream
     32-bit-word-output-stream
     bidirectional-stream)
  ())


;;;; 每一个可实例化的流需要三个组件：
;;;; 元素类型，方向，设备类型

;;;; 可实例化的 字符磁盘流

(defclass character-disk-input-stream
    (character-input-stream disk-input-stream)
  ())

(defclass character-disk-output-stream
    (character-output-stream disk-output-stream)
  ())

(defclass character-disk-bidirectional-stream
    (character-bidirectional-stream disk-bidirectional-stream)
  ())

;;;; 可实例化的 字符磁带流

(defclass character-tape-input-stream
    (character-input-stream tape-input-stream)
  ())

(defclass character-tape-output-stream
    (character-output-stream tape-output-stream)
  ())

;;;; 可实例化的 8-bit-byte 磁盘流

(defclass 8-bit-byte-disk-input-stream
    (8-bit-byte-input-stream disk-input-stream)
  ())

(defclass 8-bit-byte-disk-output-stream
    (8-bit-byte-output-stream disk-output-stream)
  ())

(defclass 8-bit-byte-disk-bidirectional-stream
    (8-bit-byte-bidirectional-stream
     disk-bidirectional-stream)
  ())

;;;; 可实例化的 8-bit 磁带流

(defclass 8-bit-byte-tape-input-stream
    (8-bit-byte-input-stream tape-input-stream)
  ())

(defclass 8-bit-byte-tape-output-stream
    (8-bit-byte-output-stream tape-output-stream)
  ())

;;;; 可实例化的 32-bit-word 磁盘流

(defclass 32-bit-word-disk-input-stream
    (32-bit-word-input-stream disk-input-stream)
  ())

(defclass 32-bit-word-disk-output-stream
    (32-bit-word-output-stream disk-output-stream)
  ())

(defclass 32-bit-word-disk-bidirectional-stream
    (32-bit-word-bidirectional-stream
     disk-bidirectional-stream)
  ())

;;;; 可实例化的 32-bit-word 磁带流不提供

(defclass 32-bit-word-tape-input-stream
    (32-bit-word-input-stream tape-input-stream)
  ())

(defclass 32-bit-word-tape-output-stream
    (32-bit-word-output-stream tape-output-stream)
  ())


;;; 被任何一个需要创建流的函数调用
;;; make-stream 创建，打开并返回一个流
(defun make-stream (device-type direction element-type name)
  (let* ((stream-class (select-stream-class
                        direction element-type device-type))
         (stream (make-device-stream device-type stream-class
                                     name)))
    (setf (stream-state stream) 'open)
    stream))


;;; 维护 select-stream-class 所需要的关联
(defvar *stream-selector* nil)

;;; 建立从前 3 个参数到类的关联
(defun add-stream-class (direction element-type device-type class)
  (setq *stream-selector*
        (acons (list direction element-type device-type) class
               *stream-selector*)))

;;; 根据参数选择合适的类
;;; 如果没有匹配的类，抛出一个错误
(defun select-stream-class (direction element-type device-type)
  (let* ((entry (assoc (list direction element-type device-type)
                       *stream-selector*
                       :test #'compare-stream-lists))
         (class (cdr entry)))
    (if (null entry)
        (error "Cannot create a ~A ~A stream for device-type ~A."
               element-type direction device-type)
        class)))

;;; 比较两个流列表是否相等
;;; 用于在 *stream-selector* 里比较 plist keys
(defun compare-stream-lists (list1 list2)
  (and (eql (first list1) (first list2))
       ;; 比较 element-type
       (equal-typep (second list1) (second list2))
       (eql (third list1) (third list2))))

;;; 测试两个类型是否相等
(defun equal-typep (t1 t2)
  (and (subtypep t1 t2) (subtypep t2 t1)))


;;;; 泛型函数 make-device-stream

(defgeneric make-device-stream (device-type class name)
  (:documentation "Create an instance with correct initargs."))

;;;; make-device-stream 的方法

(defmethod make-device-stream ((device-type (eql 'tape))
                               class name)
  (make-instance class :unit (parse-integer name)))

(defmethod make-device-stream ((device-type (eql 'disk))
                               class name)
  (make-instance class :pathname name))

;;;; 添加 字符磁盘流

(add-stream-class :input 'character 'disk
                  (find-class 'character-disk-input-stream))

(add-stream-class :output 'character 'disk
                  (find-class 'character-disk-output-stream))

(add-stream-class :bidirectional 'character 'disk
                  (find-class 'character-disk-bidirectional-stream))

;;;; 添加 字符磁带流

(add-stream-class :input 'character 'tape
                  (find-class 'character-tape-input-stream))

(add-stream-class :output 'character 'tape
                  (find-class 'character-tape-output-stream))

;;;; 添加 8-bit-byte 磁盘流

(add-stream-class :input '(unsigned-byte 8) 'disk
                  (find-class '8-bit-byte-disk-input-stream))

(add-stream-class :output '(signed-byte 8) 'disk
                  (find-class '8-bit-byte-disk-output-stream))

(add-stream-class :bidirectional '(unsigned-byte 8) 'disk
                  (find-class '8-bit-byte-disk-bidirectional-stream))

;;;; 添加 8-bit-byte 磁带流

(add-stream-class :input '(unsigned-byte 8) 'tape
                  (find-class '8-bit-byte-tape-input-stream))

(add-stream-class :output '(unsigned-byte 8) 'tape
                  (find-class '8-bit-byte-tape-output-stream))

;;;; 添加 32-bit-word 磁盘流

(add-stream-class :input '(signed-byte 32) 'disk
                  (find-class '32-bit-word-disk-input-stream))

(add-stream-class :output '(signed-byte 32) 'disk
                  (find-class '32-bit-word-disk-output-stream))

(add-stream-class :bidirectional '(signed-byte 32) 'disk
                  (find-class '32-bit-word-disk-bidirectional-stream))

;;;; 添加 32-bit-word 磁带流

(add-stream-class :input '(signed-byte 32) 'tape
                  (find-class '32-bit-word-tape-input-stream))

(add-stream-class :output '(signed-byte 32) 'tape
                  (find-class '32-bit-word-tape-output-stream))
