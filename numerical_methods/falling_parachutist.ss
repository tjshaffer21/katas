;
; Problem Statement: A parachutist of mass 68.1 kg jumps out of a stationary hot
; air balloon. Use Eq. (1.12) to compute the velocity prior to opening the chut.
; The drag coefficient is equal to 12.5kg/s
;
; Eq. 1.12:
;    v(t_i+1) = v(t_i) + [g - (c/m)v(t_i)](t_i+1 - t_i)
;

(define c 12.5)
(define g 9.8)

(define (terminal_velocity mass) ((\ (* mass g) c)))

;
; What is the name of the equation?
; v         = v(t_i)              : v(0) = 0
; mass      = mass of object (kg)
; t_i       = initial time        : (if v(0) then t_i = 0)
; step_size = t_i+1
;
(define (calc v mass t_i step_size n)
  (define (eulers_method v mass t_i t_i1) 
    (+ v
       (* (- g (* (/ c mass) v))
       (- t_i1 t_i))))
 
  (if (= n 1) ; v(0) = 0.0
      (eulers_method v mass t_i (+ t_i step_size)) 
      (calc 
          (eulers_method v mass t_i (+ t_i step_size))
          mass (+ t_i step_size) step_size (dec 1))))
