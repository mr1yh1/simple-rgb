;;; -*- mode: lisp; syntax: common-lisp; encoding: utf-8 -*-
;;;
;;; $Id$
;;;
;;; Copyright (c) 2008 William S. Annis.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.


(in-package :asdf)

(defsystem :simple-rgb
  :name "SIMPLE-RGB"
  :author "William S. Annis <wm.annis@gmail.com>"
  :version "0.1"
  :maintainer "William S. Annis <wm.annis@gmail.com>"
  :licence "MIT"
  :description "simple manipulation of {X|HT}ML RGB values"

  :components ((:file "package")
               (:file "rgb" :depends-on ("package"))))

(defsystem :simple-rgb-test
  :depends-on (:simple-rgb :lift)
  :components ((:file "test-rgb")))

(defmethod perform ((o test-op) (c (eql (find-system :simple-rgb))))
  (operate 'load-op :simple-rgb-test)
  (funcall (intern (symbol-name :run-tests) (find-package :simple-rgb-test))))
