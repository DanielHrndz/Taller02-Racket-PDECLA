#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 1 – Contar elementos positivos en una lista
;; Entrada: '(3 -2 7 0 -5 9)  =>  3
(define (ej1-contar-positivos xs)
  (length (filter (lambda (x) (> x 0)) xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 2 – Generar lista de cuadrados pares
;; Entrada: '(1 2 3 4 5 6 7 8)  =>  '(4 16 36 64)
(define (ej2-cuadrados-pares xs)
  (filter even? (map (lambda (x) (* x x)) xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 3 – Calcular el factorial de un número (recursión)
;; Entrada: 5  => 120
(define (ej3-factorial n)
  (cond [(zero? n) 1]
        [else (* n (ej3-factorial (sub1 n)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4 – Elevar cada número al cubo (map + lambda)
;; Entrada: '(2 3 4)  =>  '(8 27 64)
(define (ej4-cubos xs)
  (map (lambda (x) (* x x x)) xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5 – Sumar todos los elementos impares
;; Entrada: '(1 2 3 4 5 6 7)  =>  16
(define (ej5-suma-impares xs)
  (foldl + 0 (filter odd? xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 6 – ¿La lista contiene números negativos? (ormap)
;; Entrada: '(5 9 -3 2)  =>  #t
(define (ej6-contiene-negativos? xs)
  (ormap (lambda (x) (< x 0)) xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 7 – Suma acumulada de una lista (foldl con acumulador)
;; Entrada: '(1 2 3 4)  =>  '(1 3 6 10)
(define (ej7-suma-acumulada xs)
  (define result
    (foldl (lambda (x acc)            ; acc = (cons suma-actual lista-reversa)
             (define suma (+ (car acc) x))
             (cons suma (cons suma (cdr acc))))
           (cons 0 '())
           xs))
  (reverse (cdr result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 8 – Concatenar cadenas de texto en una lista
;; Entrada: '("Hola" " " "Mundo")  =>  "Hola Mundo"
(define (ej8-concat cadenas)
  (foldl (lambda (x acc) (string-append acc x)) "" cadenas))
  ;; alternativa equivalente:
  ;; (foldr string-append "" cadenas)

(module+ main
  (define entrada '("Hola" " " "Mundo"))
  (displayln (format "Ejercicio 8: ~a => ~a"
                     entrada
                     (ej8-concat entrada)))) ; => "Hola Mundo"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 9 – Doble de los números > 5 (filter + map)
;; Entrada: '(3 6 8 2 10)  =>  '(12 16 20)
(define (ej9-dobles-mayores-5 xs)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 10 – Invertir el orden de una lista (foldl)
;; Entrada: '(1 2 3 4)  =>  '(4 3 2 1)
(define (ej10-invertir xs)
  (foldl (lambda (x acc) (cons x acc)) '() xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 11 – Función que recibe una función como parámetro
;; Ejemplo con cuadrado y '(1 2 3 4)  =>  '(1 4 9 16)
(define (ej11-aplicar f xs) (map f xs))
(define (cuadrado x) (* x x)) ; para el ejemplo que pide la guía

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 12 – Reto: promedio de los números > 5 (map+filter+foldl)
;; Entrada: '(3 8 10 4 9 2 7)  =>  8.5
(define (ej12-promedio-mayores-5 xs)
  (let* ([mayores (filter (lambda (x) (> x 5)) xs)]
         [suma    (foldl + 0 mayores)]
         [n       (length mayores)])
    (if (zero? n)
        (error 'ej12 "no hay elementos > 5")
        (/ (exact->inexact suma) n))))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (displayln "=== TALLER 2 — Declarativa - 00015322 ===")

  ;; Ejercicio 1
  (displayln
   (format "E1: ~a => ~a   (Salida esperada: 3)"
           '(3 -2 7 0 -5 9)
           (ej1-contar-positivos '(3 -2 7 0 -5 9))))

  ;; Ejercicio 2
  (displayln
   (format "E2: ~a => ~a   (Salida esperada: '(4 16 36 64))"
           '(1 2 3 4 5 6 7 8)
           (ej2-cuadrados-pares '(1 2 3 4 5 6 7 8))))

  ;; Ejercicio 3
  (displayln
   (format "E3: 5 => ~a   (Salida esperada: 120)"
           (ej3-factorial 5)))

  ;; Ejercicio 4
  (displayln
   (format "E4: ~a => ~a   (Salida esperada: '(8 27 64))"
           '(2 3 4)
           (ej4-cubos '(2 3 4))))

  ;; Ejercicio 5
  (displayln
   (format "E5: ~a => ~a   (Salida esperada: 16)"
           '(1 2 3 4 5 6 7)
           (ej5-suma-impares '(1 2 3 4 5 6 7))))

  ;; Ejercicio 6
  (displayln
   (format "E6: ~a => ~a   (Salida esperada: #t)"
           '(5 9 -3 2)
           (ej6-contiene-negativos? '(5 9 -3 2))))

  ;; Ejercicio 7
  (displayln
   (format "E7: ~a => ~a   (Salida esperada: '(1 3 6 10))"
           '(1 2 3 4)
           (ej7-suma-acumulada '(1 2 3 4))))

  ;; Ejercicio 8
  (displayln
   (format "E8: ~a => ~a   (Salida esperada: \"Hola Mundo\")"
           '("Hola" " " "Mundo")
           (ej8-concat '("Hola" " " "Mundo"))))

  ;; Ejercicio 9
  (displayln
   (format "E9: ~a => ~a   (Salida esperada: '(12 16 20))"
           '(3 6 8 2 10)
           (ej9-dobles-mayores-5 '(3 6 8 2 10))))

  ;; Ejercicio 10
  (displayln
   (format "E10: ~a => ~a   (Salida esperada: '(4 3 2 1))"
           '(1 2 3 4)
           (ej10-invertir '(1 2 3 4))))

  ;; Ejercicio 11
  (displayln
   (format "E11: cuadrado en ~a => ~a   (Salida esperada: '(1 4 9 16))"
           '(1 2 3 4)
           (ej11-aplicar cuadrado '(1 2 3 4))))

  ;; Ejercicio 12
  (displayln
   (format "E12: ~a => ~a   (Salida esperada: 8.5)"
           '(3 8 10 4 9 2 7)
           (ej12-promedio-mayores-5 '(3 8 10 4 9 2 7)))))
