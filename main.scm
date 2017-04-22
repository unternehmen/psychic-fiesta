; Copyright © 2017 Chris Murphy and Charlie Murphy.
;
; This file is part of psychic-fiesta.
;
; psychic-fiesta is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; psychic-fiesta is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with psychic-fiesta. If not, see <http://www.gnu.org/licenses/>.

(use-modules (system foreign)
             (sdl2)
             (sdl2 render)
             (sdl2 surface)
             (sdl2 image)
             (sdl2 video)
             (sdl2 events)
             ((sdl2 bindings) #:prefix ffi:))

(define (rect x y width height)
  "Create an SDL2 rectangle."
  (make-c-struct (list int32 int32
                       int32 int32)
                 (list x y width height)))

;; Ripped and modified from the Guile-SDL2 source code.
(define (render-copy renderer texture srcrect destrect)
  "Copy TEXTURE to the rendering target of RENDERER at (X, Y)."
  (let ((result (ffi:sdl-render-copy
                  ((@@ (sdl2 render) unwrap-renderer) renderer)
                  ((@@ (sdl2 render) unwrap-texture) texture)
                  srcrect
                  destrect)))
    (unless (zero? result)
      (sdl-error "render-copy" "failed to copy texture"))))

(define up-state #f)
(define left-state #f)
(define right-state #f)
(define down-state #f)

(define (main-loop ren x y)
  (begin
    (let* ((done #f)
           (surface (load-image "image.png"))
           (texture (surface->texture ren surface)))
      (let loop ((event (poll-event)))
        (if event
          (begin
            (cond
              ((quit-event? event)
               (set! done #t))
              ((and (keyboard-down-event? event))
               (case (keyboard-event-key event)
                 ((right) (set! right-state #t))
                 ((up)    (set! up-state #t))
                 ((down)  (set! down-state #t))
                 ((left)  (set! left-state #t))))
              ((and (keyboard-up-event? event))
               (case (keyboard-event-key event)
                 ((right) (set! right-state #f))
                 ((up)    (set! up-state #f))
                 ((down)  (set! down-state #f))
                 ((left)  (set! left-state #f)))))
            (loop (poll-event)))))
      (if right-state (set! x (+ x 1)))
      (if left-state (set! x (- x 1)))
      (if down-state (set! y (+ y 1)))
      (if up-state (set! y (- y 1)))
      (clear-renderer ren)
      (render-copy ren texture (rect 0 0 32 32)
                               (rect x y 32 32))
      (present-renderer ren)
      (usleep 36000)
      (if (not done) (main-loop ren x y)))))

(begin
  (sdl-init)
  (image-init)
  (call-with-window (make-window #:size '(300 300))
    (lambda (window)
      (begin
        (call-with-renderer (make-renderer window)
          (lambda (ren) (main-loop ren 0 0))))))
  (image-quit)
  (sdl-quit))
