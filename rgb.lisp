;;; -*- mode: lisp; syntax: common-lisp; package: simple-rgb encoding: utf-8 -*-
;;; Author: William S. Annis
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

(in-package :simple-rgb)

(defstruct (rgb (:conc-name nil))
  (r 0 :type (unsigned-byte 8))
  (g 0 :type (unsigned-byte 8))
  (b 0 :type (unsigned-byte 8)))

(declaim (ftype (function ((real 0 255) (real 0 255) (real 0 255)) rgb) rgb))
(defun rgb (r g b)
  "Accepts numbers between 0 255. Rounds floats."
  (make-rgb :r (round r) :g (round g) :b (round b)))

(declaim (ftype (function (rgb rgb) boolean) rgb=))
(defun rgb= (a b)
  (and (= (r a) (r b))
       (= (g a) (g b))
       (= (b a) (b b))))

(defstruct (hsv (:conc-name nil))
  (h 0 :type (float 0.0e0 1.0e0))
  (s 0 :type (float 0.0e0 1.0e0))
  (v 0 :type (float 0.0e0 1.0e0)))

(declaim (ftype (function ((real 0 1) (real 0 1) (real 0 1)) hsv) hsv))
(defun hsv (h s v)
  (make-hsv :h (float h) :s (float s) :v (float v)))

(defparameter +rgb-black+ (rgb 0 0 0))
(defparameter +rgb-white+ (rgb 255 255 255))

(declaim (ftype (function (rgb rgb &optional (real 0 1)) rgb)
                mix-rgb
                mix-rgb!))

(defun mix-rgb! (a b &optional (alpha 0.5))
  "Overwrites the first argument with the mixed color."
  (flet ((mix (x y alpha)
           (round (+ x (* alpha (- y x))))))
    (setf (r a) (mix (r a) (r b) alpha)
          (g a) (mix (g a) (g b) alpha)
          (b a) (mix (b a) (b b) alpha))
    a))

(defun mix-rgb (a b &optional (alpha 0.5))
  "ALPHA weights the mix, 0.0 favoring the first color, 1.0 the second."
  (let ((c (copy-rgb a)))
    (mix-rgb! c b alpha)))

(declaim (ftype (function (rgb) rgb)
                greyscale-rgb
                lighten-rgb lighten-rgb!
                darken-rgb darken-rgb!
                invert-rgb complement-rgb))

;;; http://en.wikipedia.org/wiki/Grayscale
(defun greyscale-rgb (a)
  (let ((gs (+ (* .3  (r a))
               (* .59 (g a))
               (* .11 (b a)))))
    (rgb gs gs gs)))

(defun lighten-rgb (a)
  (mix-rgb a +rgb-white+))

(defun lighten-rgb! (a)
  (mix-rgb! a +rgb-white+))

(defun darken-rgb (a)
  (mix-rgb a +rgb-black+))

(defun darken-rgb! (a)
  (mix-rgb! a +rgb-black+))

(defun invert-rgb (a)
  (rgb (- 255 (r a))
       (- 255 (g a))
       (- 255 (b a))))

;;; http://livedocs.adobe.com/en_US/Illustrator/13.0/help.html?content=WS714a382cdf7d304e7e07d0100196cbc5f-6288.html
;;; This does nothing interesting to greys.
(defun complement-rgb (a)
  (let* ((r (r a))
         (g (g a))
         (b (b a))
         (min+max (+ (min r g b) (max r g b))))
    (rgb (- min+max r)
         (- min+max g)
         (- min+max b))))

(declaim (ftype (function (rgb &optional (real 0 1)) rgb) contrast-rgb))
(defun contrast-rgb (a &optional (cut 0.5))
  (let ((cutoff (round (* cut 255))))
    (labels ((contrastify (color-component)
               (if (>= cutoff color-component) 0 255)))
      (rgb (contrastify (r a))
           (contrastify (g a))
           (contrastify (b a))))))

(defun xmlify-rgb (a &optional (stream nil))
  (declare (type rgb a))
  (format stream "#~2,'0X~2,'0X~2,'0X" (r a) (g a) (b a)))

(declaim (ftype (function (rgb) hsv) rgb->hsv))
(defun rgb->hsv (a)
  (let* ((r (/ (r a) 255.0))
         (g (/ (g a) 255.0))
         (b (/ (b a) 255.0))
         (max (max r g b))
         (min (min r g b))
         (v max))
    (if (= max min)
        (hsv 0.0 0.0 v)
        (let ((s (/ (- max min) max))
              (h 0))
          (cond ((= r max)
                 (setf h (- (/ (- max b) (- max min))
                            (/ (- max g) (- max min)))))
                ((= g max)
                 (setf h (+ 2.0 (- (/ (- max r) (- max min))
                                   (/ (- max b) (- max min))))))
                (t (setf h (+ 4.0 (- (/ (- max g) (- max min))
                                     (/ (- max r) (- max min)))))))
          (setf h (mod (/ h 6.0) 1))
          (hsv h s v)))))

(declaim (ftype (function (hsv) rgb) hsv->rgb))
(defun hsv->rgb (a)
  (let ((h (h a))
        (s (s a))
        (v (v a)))
    (if (= s 0.0)
        (rgb v v v)
        (multiple-value-bind (i f) (truncate (* h 6.0))
          (let* ((p (* v (- 1.0 s)))
                 (q (* v (- 1.0 (* s f))))
                 (tv (* v (- 1.0 (* s (- 1.0 f))))))
            (ecase i
              (0 (rgb v tv p))
              (1 (rgb q  v p))
              (2 (rgb p  v tv))
              (3 (rgb p  q v))
              (4 (rgb tv p v))
              (5 (rgb v  p q))))))))

;;; (hsv->rgb (rotate-hsv (rgb->hsv color) 180)) == (complement-rgb color)
(declaim (ftype (function (hsv real) hsv) rotate-hsv))
(defun rotate-hsv (a rotation)
  (let ((scaled-rotation (/ rotation 360.0)))
    (hsv (mod (+ (h a) scaled-rotation) 1.0) (s a) (v a))))

(declaim (ftype (function (rgb real) rgb) rotate-rgb))
(defun rotate-rgb (a rotation)
  (hsv->rgb (rotate-hsv (rgb->hsv a) rotation)))

;;; rgb.lisp ends here
