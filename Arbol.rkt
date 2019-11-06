#lang racket
;------------------------------------------------------------------------------------------------
;DEFINICION DE LOS STRUCTS A USAR

(define-struct nodos (Id Nombre [sequence #:mutable])
  #:transparent)
(define-struct aristas (Padre Hijo Peso)
  #:transparent)


;---------------------------------------------------------------------------------------------------
;CONSTRUCTORES DEL ARBOL

;Árbol
(define (arbol listaNodos listaAristas)
  (list listaNodos listaAristas))

;Nodos
(define A(make-nodos 1 'A '(2 3)))
(define B(make-nodos 2 'B '(4 5 6)))
(define C(make-nodos 3 'C '(9)))
(define D(make-nodos 4 'D '()))
(define E(make-nodos 5 'E '(7 8)))
(define F(make-nodos 6 'F '()))
(define G(make-nodos 7 'G '()))
(define H(make-nodos 8 'H '()))
(define I(make-nodos 9 'I '()))

;Aristas
(define a(make-aristas 1 2 20))
(define b(make-aristas 1 3 30))
(define c(make-aristas 2 4 50))
(define d(make-aristas 2 5 70))
(define e(make-aristas 2 6 15))
(define f(make-aristas 5 7 18))
(define g(make-aristas 5 8 16))
(define h(make-aristas 3 9 31))


;---------------------------------------------------------------------------------------------
;LISTAS DEFINIDAS

;Aristas
(define listaAristas (list a b c d e f g h))

;Nodos
(define listaNodos (list A B C D E F G H I))

;----------------------------------------------------------------------------------------------
;FUNCIONES AUXILIARES

;Llamada al agregar un nodo 
(define (actualizarSequence padre nodo)
  (actualizarSequenceAux (first(filter (lambda(x) (equal? (nodos-Id x) padre)) listaNodos))nodo))
;***************************************************
;Llamada al agregar un nodo 
(define (actualizarSequenceAux padre nodo)
  (set-nodos-sequence! padre (append(nodos-sequence padre) (list nodo)))
  )
;**************************************************
;Funcion usada en la funcion buscarNodo, se compara con el padre del nodo sacado con las demas aristas
(define (hermanosNodoAux id)
  (if (null? listaAristas) ""
      (filter(lambda (elemento) (= (aristas-Padre (hermanosNodosAux2 id)) (aristas-Padre elemento)))listaAristas)) 
  )
;*************************************************
;Funcion usada en la funcion buscarNodo, se busca el la arista del nodo elegido
(define (hermanosNodosAux2 node)
  (if (null? listaAristas) ""
      (car(filter(lambda (elemento) (= node (aristas-Hijo elemento)))listaAristas))) 
  )

;--------------------------------------------------------------------------------------------------
;FUNCIONES DEFINIDAS SOBRE EL ARBOL

;---------------------------------------------------------------------------------------------------
;Busca un nodo determinado a partir del id del nodo.
(define (buscarNodo idNodo)
    (filter (lambda(x) (equal? (nodos-Id x) idNodo)) listaNodos))

;---------------------------------------------------------------------------------------------------
;Busca un nodo padre a partir del id del hijo
(define (buscarNodoPadre idNodo)
    (buscarNodo (aristas-Padre (first(filter (lambda(x) (equal? (aristas-Hijo x) idNodo)) listaAristas)))))

;----------------------------------------------------------------------------------------------------
;Funcion que se encarga de encontrar un nodo aprtir de un id dado
;el remove es para eliminar el mismo y que no se muestre, solo los hermanos
(define (hermanosNodo id)
  (remove (hermanosNodosAux2 id) (hermanosNodoAux id))
  )

;----------------------------------------------------------------------------------------------------------------------
;Seccion en donde se agrega un nodo, se recibe idPadre, idHijo, nombre, peso
(define (agregarNodo idPadre idHijo nombre peso)
  (define aux(make-nodos idHijo nombre '()))
  (define a(append listaNodos (list aux)))
  (define auxb( make-aristas idPadre idHijo peso))
  (define b(append listaAristas (list auxb)))

  (set! listaNodos a);Setea la lista nueva 
  (set! listaAristas b);Setea la lista nueva 
  (actualizarSequence idPadre idHijo)

  )
;-----------------------------------------------------------------------------------------------------------


