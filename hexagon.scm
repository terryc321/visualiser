#|

initialise SDL at startup
iniitilaise SDL-TTF true type font at startup

add condition handler in case anything goes wrong to call finish or sdl2:quit!

besides keyboard handling being weak , only one key down event processed

how draw a hexagon ? six sided polygon

how draw a circle ?  infinite sided polygon



|#

(import scheme)
(import simple-exceptions)
(import (chicken string))
(import (chicken pretty-print))
;;(define pp pretty-print)
(import (chicken io))
(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day15")
;; (current-directory)
(import procedural-macros)
(import regex)
(import simple-md5)
(import simple-loops)
(import srfi-69)
;; hash-table-ref  hash key thunk
;; hash-table-set! hash key val
;; sudo chicken-install srfi-178
(import srfi-178)
;; srfi-178 provides bit-vectors
;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))
(import sequences)
(import srfi-1)
(import matchable)
;; Compatible with both CHICKEN 4 and CHICKEN 5.
;; sdl2 egg
;; sdl2-image egg
;; sdl2-ttf egg

;; (import coops)
;; (import coops-primitive-objects)


(cond-expand
  (chicken-4 (use (prefix sdl2 "sdl2:")
                  (prefix sdl2-ttf "ttf:")))
  (chicken-5 (import (prefix sdl2 "sdl2:")
                     (prefix sdl2-ttf "ttf:")
		     miscmacros
		     queues)))

;; -------------------------------------------------------
(define window #f)
(define renderer #f)

(define font #f)
(define text #f)

(define color-red-1 #f)
(define color-red-2 #f)

;; --------- a simple red box --------------------------
(define red-box #(0 0 50 50))
(define arr1-get (lambda (v) (vector-ref v 0)))
(define arr2-get (lambda (v) (vector-ref v 1)))
(define arr3-get (lambda (v) (vector-ref v 2)))
(define arr4-get (lambda (v) (vector-ref v 3)))

(define arr1-set! (lambda (v a) (vector-set! v 0 a)))
(define arr2-set! (lambda (v a) (vector-set! v 1 a)))
(define arr3-set! (lambda (v a) (vector-set! v 2 a)))
(define arr4-set! (lambda (v a) (vector-set! v 3 a)))

(define (red-left v)
  (arr1-set! v (- (arr1-get v) 1)))

(define (red-right v)
  (arr1-set! v (+ (arr1-get v) 1)))

(define (red-up v)
  (arr2-set! v (- (arr2-get v) 1)))

(define (red-down v)
  (arr2-set! v (+ (arr2-get v) 1)))


(define (red-shift-left v)
  (arr1-set! v (- (arr1-get v) 10)))

(define (red-shift-right v)
  (arr1-set! v (+ (arr1-get v) 10)))

(define (red-shift-up v)
  (arr2-set! v (- (arr2-get v) 10)))

(define (red-shift-down v)
  (arr2-set! v (+ (arr2-get v) 10)))


;; -------------------------------------------------------


;; let/cc = (call/cc ...
(define (main-loop)
  ;;(full-rerender!)
  (call/cc (lambda (exit-main-loop!)
    ;; Loop forever (until exit-main-loop! is called).
    (while #t
      ;; Handle all pending events.
      (let ((ev (sdl2:poll-event!)))
        (while ev
          (handle-event ev exit-main-loop!)
          (set! ev (sdl2:poll-event!))))))))
  

(define (handle-event ev exit-main-loop!)
  (case (sdl2:event-type ev)
    
    ;; Window exposed, resized, etc.
    ((window)
     ;;(format #t "window exposed~%")
     (redraw)
     #t)

    ;; User requested app quit (e.g. clicked the close button).
    ((quit)
     (exit-main-loop! #t))

    ;; Keyboard key pressed
    ((key-down)
     (let ((shift? (memq 'shift (sdl2:keyboard-event-mod ev)))
           (alt?   (memq 'alt (sdl2:keyboard-event-mod ev))))
       (case (sdl2:keyboard-event-sym ev)
         ;; Plus or Equals zooms in
         ((plus equals)
	  (format #t "plus or equals pressed~%")
	  )
	 
         ;; Minus zooms out
         ((minus)
	  (format #t "minux pressed~%")
	  )
         
         ;; Arrow keys move the view
         ((up)
          (format #t "up key pressed~%")
	  (cond
	   (shift? (red-shift-up red-box))
	   (#t (red-up red-box)))
          ;; (move! 0 -1 shift? alt?)
          ;; (full-rerender!)
	  )
         ((down)
          (format #t "down key pressed~%")	  
	  (cond
	   (shift? (red-shift-down red-box))
	   (#t (red-down red-box)))
          ;; (move! 0 1 shift? alt?)
          ;; (full-rerender!)
	  )
         ((left)
          (format #t "left key pressed~%")
	  (cond
	   (shift? (red-shift-left red-box))
	   (#t (red-left red-box)))
	  
          ;; (move! -1 0 shift? alt?)
          ;; (full-rerender!)
	  )
         ((right)
	  (format #t "right key pressed~%")
	  (cond
	   (shift? (red-shift-right red-box))
	   (#t (red-right red-box)))
          ;;(move! 1 0 shift? alt?)
          ;;(full-rerender!)
	  )

         ;; ;; Number row keys switch palettes
         ;; ((n-1)  (when (switch-palette! 0) (refresh!)))
         ;; ((n-2)  (when (switch-palette! 1) (refresh!)))
         ;; ((n-3)  (when (switch-palette! 2) (refresh!)))
         ;; ((n-4)  (when (switch-palette! 3) (refresh!)))
         ;; ((n-5)  (when (switch-palette! 4) (refresh!)))
         ;; ((n-6)  (when (switch-palette! 5) (refresh!)))
         ;; ((n-7)  (when (switch-palette! 6) (refresh!)))
         ;; ((n-8)  (when (switch-palette! 7) (refresh!)))
         ;; ((n-9)  (when (switch-palette! 8) (refresh!)))
         ;; ((n-0)  (when (switch-palette! 9) (refresh!)))

         ;; R resets to the original view
         ((r)
	  ;;(format #t "r key pressed~%")
          ;;(reset-view!)
          ;;(full-rerender!)
	  (redraw)
	  )

         ;; S saves a screenshot, tagged with the current time in
         ;; seconds since 1970-01-01T00:00Z.
         ((s)
	  ;;(format #t "s key pressed~%")	            
          ;;(save-screenshot!)
	  )

         ;; Escape quits the program
         ((escape)
	  (sdl2:quit!)
          (exit-main-loop! #t)
	  )
	 );; case

       ;; any time key pressed do a redraw 
       (redraw)
       ))))




(define (create-window)
  (let ((width 1024) ;;1920)
	(height 960) ;;1080)
	;;(flags '(resizable fullscreen))
	(flags '(resizable))
	)
    (set! window (sdl2:create-window! "Hello, World!" 0 0 width height flags))
    (set! renderer (sdl2:create-renderer! window -1 '(accelerated target-texture)))
    ;;(set! renderer (sdl2:create-renderer! window -1 '(target-texture)))
    ;;(set! renderer (sdl2:create-renderer! window -1 '()))
    ;;(format #t "renderer = ~a ~%" renderer)

    ;; (format #t "number render drivers ~a ~%" (sdl2:num-render-drivers))
    ;; (do-for (i 0 (sdl2:num-render-drivers))
    ;; 	    (receive info (sdl2:render-driver-info i)
    ;; 	      (format #t "render info ~a ~%" info)))
    
    (set! color-red-1 (sdl2:make-color 0 128 255))
    (set! color-red-2 (sdl2:make-color 255 0 0))
    ;;(set! surface (sdl2:window-surface window))
    ))

(define (finish!)
  ;;(destroy-renderer! renderer)
  (sdl2:quit!))


(define (redraw)  
  (sdl2:render-draw-color-set! renderer (sdl2:make-color 0 0 0))
  (sdl2:render-clear! renderer)
  (sdl2:render-draw-color-set! renderer (sdl2:make-color 255 0 0))
  (sdl2:render-fill-rect! renderer (sdl2:make-rect 0 0 255 255))
  (sdl2:render-draw-rect! renderer (sdl2:make-rect 255 0 255 255))
  (sdl2:render-draw-line! renderer 255 0 (+ 255 255) 255)  
  (sdl2:render-draw-color-set! renderer (sdl2:make-color 0 0 255))
  (sdl2:render-fill-rect! renderer (sdl2:make-rect (arr1-get red-box) (arr2-get red-box) 50 50))

  (set! text (format #f "hello : ~a , ~a" (arr1-get red-box) (arr2-get red-box)))
  (define-values (w h) (ttf:size-utf8 font text))
  (let* ((fore (sdl2:make-color 255 0 0))
	 (back (sdl2:make-color 255 255 255))
	 (text-surface (ttf:render-utf8-shaded
			font
			text
			fore
			back))
	 (text-texture (sdl2:create-texture-from-surface renderer text-surface)))
    ;; copy all of the text texture onto the renderer
    (let ((src #f)
	  (dest (sdl2:make-rect 0 0 w h)))
      (sdl2:render-copy! renderer text-texture src dest))
    (let ((src (sdl2:make-rect 0 0 w h))
	  (dest (sdl2:make-rect (arr1-get red-box) (arr2-get red-box) w h)))
      (sdl2:render-copy! renderer text-texture src dest))
    )

  
  (sdl2:render-present! renderer)
  )






;; !!!!!!!!!!!!  REDRAW 2 IS NEVER CALLED !!!!!!!!!
;; ONLY HERE FOR HISTORICAL REASONS ...........
;; anything to do with surface is CPU
;; anything to do with renderer is GPU 
(define (redraw2)  
  ;; (set! (sdl2:render-draw-color renderer) (sdl2:make-color 0 125 255 255))
  ;; (sdl2:render-color-set! renderer (sdl2:make-color 0 125 255))
  (sdl2:render-clear! renderer)
  (sdl2:render-fill-rect! renderer (sdl2:make-rect 0 0 255 255))
  
  ;; (sdl2:render-draw-line! renderer 0 0 (arr1-get red-box) (arr2-get red-box))
  
  ;;(sdl2:fill-rect! (sdl2:window-surface window)  #f (sdl2:make-color 0 128 255))
  
  ;; (sdl2:fill-rect! (sdl2:window-surface window)  #f color-red-1)
  ;; (sdl2:fill-rect! (sdl2:window-surface window)  (sdl2:make-rect (arr1-get red-box)
  ;; 								(arr2-get red-box)
  ;; 								(arr3-get red-box)
  ;; 								(arr4-get red-box))
  ;; 		   color-red-2)

  
  (sdl2:render-draw-color-set! renderer (sdl2:make-color 0 255 0))

  (sdl2:render-draw-line! renderer 0 0 (arr1-get red-box) (arr2-get red-box))

  (format #t "render draw color : ~a ~%" (receive col (sdl2:render-draw-color renderer) col))
  (format #t "renderer : ~a ~%" renderer)
  
  
  ;;(sdl2:create-texture renderer format access w h)
  ;; rgba8888 ? what should we put there ??
  ;; is accelerated rendering supported ?
  ;;                                                static target
  ;;                                   renderer 'rgba32 'target 1024 960)))
  (let ((texture (sdl2:create-texture-from-surface renderer (sdl2:window-surface window))))
    (sdl2:render-copy! renderer texture #f #f)
    (sdl2:render-copy! renderer texture)    
    )
    
  ;;   (format #t "texture = ~a ~%" texture)
  ;;   ;;(format #t "texture format from software renderer = ~a ~%" (sdl2:texture-format 
    
  ;; ;;(sdl2:update-window-surface! window)
  ;;   (sdl2:render-copy! renderer texture #f #f) ;;(sdl2:make-rect 0 0 1024 960))
  ;;   (sdl2:render-present! renderer))
    
  ;;   (sdl2:blit-surface! text-surf #f
  ;; 			(sdl2:window-surface window) #f))

  ;;(sdl2:render-present! renderer)
  ;;(sdl2:update-window-surface! window)
  
   )






(define (setup)
  ;; Initialize SDL
  (sdl2:set-main-ready!)
  ;; https://gitlab.com/chicken-sdl2/chicken-sdl2/-/blob/main/docs/enums.md#init-flags
  (sdl2:init! '(video events))
  ;; init true type fonts
  (ttf:init!)

  ;;(set! font (ttf:open-font "/usr/share/fonts/truetype/font-awesome/fontawesome-webfont.ttf" 40))
  (set! font (ttf:open-font "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf" 20))
  (set! text "Hello, World!")
  (format #t "font = ~A ~%" font)
    
  ;; Schedule quit! to be automatically called when your program exits normally.
  (on-exit finish!)
  ;; Install a custom exception handler that will call quit! and then
  ;; call the original exception handler. This ensures that quit! will
  ;; be called even if an unhandled exception reaches the top level.
  (current-exception-handler
   (let ((original-handler (current-exception-handler)))
     (lambda (exception)
       (finish!)
       (original-handler exception)))))



(define (entry)
  (set! window #f)
  ;;(sdl2:quit!)
  (setup)
  (create-window)
  (main-loop))

;;(entry)




