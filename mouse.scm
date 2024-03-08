

#|

initialise SDL at startup
iniitilaise SDL-TTF true type font at startup
add condition handler in case anything goes wrong to call finish or sdl2:quit!
besides keyboard handling being weak , only one key down event processed
how draw a hexagon ? six sided polygon
how draw a circle ?  infinite sided polygon

how do we fill polygon with fill colours ?? 
how do we check if mouse pointer is inside polygon - highlight it - move it -

mouse pointer position

|#

(import scheme)
(import simple-exceptions)
(import (chicken keyword))
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

;;------------------------------------------------------
(define mouse-x 0)
(define mouse-y 0)


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

;; ------- a bigger step ------------------------
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

    ;; mouse button pressed
    ((mouse-button-down)
     (let ((x (sdl2:mouse-button-event-x ev))
	   (y (sdl2:mouse-button-event-y ev))
	   (button (sdl2:mouse-button-event-button ev))
	   )
       (format #t "mouse button down (~a ,~a) : button ~a ~%" x y button))
     #t)

    ;; mouse button released
    ((mouse-button-up)
     (let ((x (sdl2:mouse-button-event-x ev))
	   (y (sdl2:mouse-button-event-y ev))
	   (button (sdl2:mouse-button-event-button ev)))
       (format #t "mouse button up (~a ,~a) : button ~a ~%" x y button))     
     #t)

    
    ;; mouse motion
    ((mouse-motion)
     (let ((x (sdl2:mouse-motion-event-x ev))
	   (y (sdl2:mouse-motion-event-y ev)))
       (set! mouse-x x)
       (set! mouse-y y)
       ;;(format #t "mouse position (~a ,~a) ~%" x y))
       (redraw)
     #t))

    ((mouse-wheel)
     (let ((x (sdl2:mouse-wheel-event-x ev))
	   (y (sdl2:mouse-wheel-event-y ev)))
       (format #t "mouse wheel scroll (~a ,~a) ~%" x y))
     #t)

    

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
  (let ((width 1920) ;;1920)
	(height 1080) ;;1080)
	;;(flags '(resizable ))
	(flags '(resizable fullscreen))
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

#|
hexagon = polygon 6 sides
circle of radius R , six points , centred on point x y 

need the 

|#
(define pi 3.1415926535898)

(define (degree->radians v)
  (* 2 pi (/ v 360)))


;; absolute coordinates
(define (line x y x2 y2)
  (sdl2:render-draw-line! renderer x y x2 y2))
;;(- x2 x) (- y2 y)))
  
(define (spiro r x y)
  (let* ((points '())
	 (vertices 4)
	 (vertice-angle (/ 360 vertices))
	 (first-point '()))
    (do-for (i 0 vertice-angle)
	    ;; angle is a defined word ?
	    (let ((angle (* i vertice-angle)))
	      ;;(format #t "angle [~a] = ~a ~%" i angle)
	      (let* ((dx (+ x (* r (cos angle))))
		     (dy (+ y (* r (sin angle))))
		     (new-point (list dx dy)))
		(when (null? first-point) (set! first-point new-point))
		(set! points (cons new-point points))
		)))
    ;;
    ;;(set! points (cons first-point points))
    ;; integer values only
    (set! points (map (lambda (x) (list (floor (car x)) (floor (cadr x)))) points))
    ;; iterate over points until no next point
    
    ;;(format #t "points = ~a ~%" points)
    ;; iterate 
    (letrec ((iter (lambda (ys)
		     (cond
		      ((null? ys) #t)
		      ((null? (cdr ys)) #t)
		      (#t (let ((a (car ys))
				(b (cadr ys)))
			    (let ((ax (car a))
				  (ay (cadr a))
				  (bx (car b))
				  (by (cadr b)))
			      (line ax ay bx by)
			      (iter (cdr ys)))))))))
      (iter points)
      #t
      )))


(define (cos-degree a)
  (cos (degree->radians a)))

(define (sin-degree a)
  (sin (degree->radians a)))

(define points-polygon
  (lambda (r rot n x y)    
  (let* ((points '())
	 (vertices n)
	 (vertice-angle (/ 360 n))
	 (initial-angle rot) 
	 (first-point '()))
    (do-for (i 0 vertices)
	    ;; angle is a defined word ?
	    (let ((angle (+ initial-angle (* i vertice-angle))))
	      ;;(format #t "angle [~a] = ~a ~%" i angle)
	      (let* ((dx (+ x (* r (cos-degree angle))))
		     (dy (+ y (* r (sin-degree angle))))
		     (new-point (list dx dy)))
		(when (null? first-point) (set! first-point new-point))
		(set! points (cons new-point points))
		)))
    ;;
    (set! points (cons first-point points))
    ;; integer values only
    (set! points (map (lambda (x) (list (floor (car x)) (floor (cadr x)))) points))
    ;; iterate over points until no next point
    points)))


(define (points-square r rot x y)
  (let ((sides 4))
    (points-polygon r rot sides x y)))
    

(define (points-triangle r rot x y)
  (let ((sides 3))
    (points-polygon r rot sides x y)))


(define (line-iter ys)
  (cond
   ((null? ys) #t)
   ((null? (cdr ys)) #t)
   (#t (let ((a (car ys))
	     (b (cadr ys)))
	 (let ((ax (car a))
	       (ay (cadr a))
	       (bx (car b))
	       (by (cadr b)))
	   (line ax ay bx by)
	   (line-iter (cdr ys)))))))


(define (triangle r rot x y)
  (let ((sides 3))
    (line-iter (points-polygon r rot sides x y))))

(define (square r rot x y)
  (let ((sides 4))
    (line-iter (points-polygon r rot sides x y))))

(define (pentagon r rot x y)
  (let ((sides 5))
    (line-iter (points-polygon r rot sides x y))))

;; fill hexagon by iterating over radius from zero to r 
(define (hexagon r rot x y #!optional (fill #f))
  (cond
   (fill 
    (do-for (ri 0 r (/ r 20))
	    (let ((sides 6))
	      (line-iter (points-polygon ri rot sides x y)))))
   (#t (let ((sides 6))
	 (line-iter (points-polygon r rot sides x y))))))


(define (septagon r rot x y)
  (let ((sides 7))
    (line-iter (points-polygon r rot sides x y))))

(define heptagon septagon) ;; same 

(define (octagon r rot x y)
  (let ((sides 8))
    (line-iter (points-polygon r rot sides x y))))



;; optional arguments ?

;; (let ((points (points-square r x y))
;; 	(points2 (points-polygon r 45 4 x y)))
;;   ;;
;;   (format #t "square points = [~a]~%" points)
;;   (format #t "square points2 = [~a]~%" points2)

;;   (cond
;;    ((equal? points points2) (format #t "same square !!~%"))
;;    (#t (format #t "not same square ... booo ~%")))
	

#|
(define (hexagon r x y)
  (let ((points '())
	(first-point '()))
    (do-for (i 0 6)
	    ;; angle is a defined word ?
	    (let ((angle (* i 60)))
	      (format #t "angle [~a] = ~a ~%" i angle)
	      (let* ((dx (+ x (* r (cos angle))))
		     (dy (+ y (* r (sin angle))))
		     (new-point (list dx dy)))
		(when (null? first-point) (set! first-point new-point))
		(set! points (cons new-point points))
		)))
    (set! points (cons first-point points))
    ;; integer values only
    (set! points (map (lambda (x) (list (floor (car x)) (floor (cadr x)))) points))
    ;; iterate over points until no next point
    (format #t "points = ~a ~%" points)
    ;; iterate 
    (letrec ((iter (lambda (ys)
		     (cond
		      ((null? ys) #t)
		      ((null? (cdr ys)) #t)
		      (#t (let ((a (car ys))
				(b (cadr ys)))
			    (let ((ax (floor (car a)))
				  (ay (floor (cadr a)))
				  (bx (floor (car b)))
				  (by (floor (cadr b))))
			      (line ax ay (- bx ax) (- by ay))
			      (iter (cdr ys)))))))))
      ;;(iter points)
      #t
      )))
|#


#|
system to indicate what words mean
where they are imported from , where defined , what line , what file
|#

#|

viewport ? or just a view onto math description of problem

window could be resized
orientation could be 

|#

(define rot 0.0)

(define (renderer-size)
  (receive (w h) (sdl2:renderer-output-size renderer)
    (list w h)))


;; optional arguments in chicken scheme
(define (set-color! r g b #!optional (a 255))
  (set! color-current (list r g b a))
  (sdl2:render-draw-color-set! renderer (sdl2:make-color r g b a)))

(define (clear-screen!)
  (sdl2:render-clear! renderer))

(define color-current '(0 0 0 0))
(define color-stack '())
(define (push-color!)
  (set! color-stack (cons color-current color-stack)))
(define (pop-color!)
  (cond
   ((null? color-stack) #f)
   (#t (let ((top (car color-stack)))
	 (set! color-stack (cdr color-stack))
	 (set! current-color top)
	 (set-color! (first top) (second top) (third top) (fourth top))))))

  
(define (redraw)  

  (set-color! 0 125 255) 
  (clear-screen!)
  (set-color! 255 0 0)

    
  ;; (sdl2:render-fill-rect! renderer (sdl2:make-rect 0 0 255 255))
  ;; (sdl2:render-draw-rect! renderer (sdl2:make-rect 255 0 255 255))
  ;; (sdl2:render-draw-line! renderer 255 0 (+ 255 255) 255)  
  ;; (sdl2:render-draw-color-set! renderer (sdl2:make-color 0 0 255))
  ;; (sdl2:render-fill-rect! renderer (sdl2:make-rect (arr1-get red-box) (arr2-get red-box) 50 50))

  ;;(set! text (format #f "hello : ~a , ~a" (arr1-get red-box) (arr2-get red-box)))
  (set! text (format #f "mouse position ~a , ~a " mouse-x mouse-y))
  
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

  ;; ;;
  
  ;; (line 500 500 600 600)
  ;; (line 500 500 400 600)
  ;; (line 500 500 600 400)
  ;; (line 500 500 400 400)

  (set! rot (+ rot 5))
  (when (> rot 360)
    (set! rot (- rot 360)))
  
  (let ((radius 50))
    (square radius rot 100 650)
    (triangle radius rot 200 650)
    (square radius rot 300 650)
    (pentagon radius rot 400 650)

    (push-color!)
    (set-color! 0 255 0)
    (hexagon radius rot 500 650)
    (hexagon radius rot 500 500 #t)
    (pop-color!)
    
    (septagon radius rot 600 650)
    (octagon radius rot 700 650)
    )

  (line 0 0 mouse-x mouse-y)
  
  ;; finished with renderer - put it to the window
  (sdl2:render-present! renderer)
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




