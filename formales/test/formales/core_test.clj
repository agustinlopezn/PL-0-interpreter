(ns formales.core-test
  (:require [clojure.test :refer :all]
            [formales.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

; POP: Saca un valor de la pila de datos, lo coloca en una direccion de memoria que forma parte de la instruccion (direccionamiento directo) e incrementa el contador de programa
(deftest test-interpretar-pop
  (testing "Funcionalidad de POP en interpretar"
    (is (= [[['POP 2] ['TEST]] [6 7 5 9] 1 [4] []] 
           (interpretar [['POP 2] ['TEST]] [6 7 8 9] 0 [4 5] [])
        )
    )
  )
)

; PFM: Coloca en la pila de datos un valor proveniente de una direccion de memoria que forma parte de la instruccion (PUSH FROM MEMORY: direccionamiento directo) e incrementa el contador de programa 
(deftest test-interpretar-pfm
  (testing "Funcionalidad de PFM en interpretar"
    (is (= [[['PFM 1] ['TEST]] [6 7 8 9] 1 [4 5 7] []] 
           (interpretar [['PFM 1] ['TEST]] [6 7 8 9] 0 [4 5] [])
        )
    )
  )
)

; PFI: Coloca en la pila de datos un valor que forma parte de la instruccion (PUSH FROM INSTRUCTION: direccionamiento inmediato) e incrementa el contador de programa 
(deftest test-interpretar-pfi
  (testing "Funcionalidad de PFI en interpretar"
    (is (= [[['PFI 3] ['TEST]] [6 7 8 9] 1 [4 5 3] []] 
           (interpretar [['PFI 3] ['TEST]] [6 7 8 9] 0 [4 5] [])
        )
    )
  )
)
; ADD: Reemplaza los dos valores ubicados en el tope de la pila de datos por su suma e incrementa el contador de programa  
(deftest test-interpretar-add
  (testing "Funcionalidad de ADD en interpretar"
    (is (= [[['ADD] ['TEST]] [6 7 8 9] 1 [9] []] 
           (interpretar [['ADD] ['TEST]] [6 7 8 9] 0 [4 5] [])
        )
    )
  )
)


; SUB: Reemplaza los dos valores ubicados en el tope de la pila de datos por su resta e incrementa el contador de programa  
(deftest test-interpretar-sub
  (testing "Funcionalidad de SUB en interpretar"
    (is (= [[['SUB] ['TEST]] [6 7 8 9] 1 [1] []] 
           (interpretar [['SUB] ['TEST]] [6 7 8 9] 0 [4 5] [])
        )
    )
  )
)


; MUL: Reemplaza los dos valores ubicados en el tope de la pila de datos por su producto e incrementa el contador de programa  
(deftest test-interpretar-mul
  (testing "Funcionalidad de MUL en interpretar"
    (is (= [[['MUL] ['TEST]] [6 7 8 9] 1 [20] []] 
           (interpretar [['MUL] ['TEST]] [6 7 8 9] 0 [4 5] [])
        )
    )
  )
)


; DIV: Reemplaza los dos valores ubicados en el tope de la pila de datos por su cociente entero e incrementa el contador de programa  
(deftest test-interpretar-div
  (testing "Funcionalidad de DIV en interpretar"
    (is (= [[['DIV] ['TEST]] [6 7 8 9] 1 [2] []] 
           (interpretar [['DIV] ['TEST]] [6 7 8 9] 0 [2 4] [])
        )
    )
  )
)


; EQ : Reemplaza los dos valores ubicados en el tope de la pila de datos por 1 si son iguales (si no, por 0) e incrementa el contador de programa
(deftest test-interpretar-eq-iguales
  (testing "Funcionalidad de EQ en interpretar"
    (is (= [[['EQ] ['TEST]] [6 7 8 9] 1 [1] []] 
           (interpretar [['EQ] ['TEST]] [6 7 8 9] 0 [4 4] [])
        )
    )
  )
)
(deftest test-interpretar-eq-no-iguales
  (testing "Funcionalidad de EQ en interpretar"
    (is (= [[['EQ] ['TEST]] [6 7 8 9] 1 [0] []] 
           (interpretar [['EQ] ['TEST]] [6 7 8 9] 0 [2 4] [])
        )
    )
  )
)
; NEQ: Reemplaza los dos valores ubicados en el tope de la pila de datos por 1 si son distintos (si no, por 0) e incrementa el contador de programa
(deftest test-interpretar-neq-iguales
  (testing "Funcionalidad de NEQ en interpretar"
    (is (= [[['NEQ] ['TEST]] [6 7 8 9] 1 [0] []] 
           (interpretar [['NEQ] ['TEST]] [6 7 8 9] 0 [4 4] [])
        )
    )
  )
)
(deftest test-interpretar-neq-no-iguales
  (testing "Funcionalidad de NEQ en interpretar"
    (is (= [[['NEQ] ['TEST]] [6 7 8 9] 1 [1] []] 
           (interpretar [['NEQ] ['TEST]] [6 7 8 9] 0 [2 4] [])
        )
    )
  )
)
; GT : Reemplaza los dos valores ubicados en el tope de la pila de datos por 1 si el valor ubicado debajo del tope es mayor que el ubicado en el tope (si no, por 0) e incrementa el contador de programa
(deftest test-interpretar-gt-mayor
  (testing "Funcionalidad de GT en interpretar"
    (is (= [[['GT] ['TEST]] [6 7 8 9] 1 [1] []] 
           (interpretar [['GT] ['TEST]] [6 7 8 9] 0 [5 4] [])
        )
    )
  )
)
(deftest test-interpretar-gt-no-mayor
  (testing "Funcionalidad de GT en interpretar"
    (is (= [[['GT] ['TEST]] [6 7 8 9] 1 [0] []] 
           (interpretar [['GT] ['TEST]] [6 7 8 9] 0 [2 4] [])
        )
    )
  )
)
; GTE: Reemplaza los dos valores ubicados en el tope de la pila de datos por 1 si el valor ubicado debajo del tope es mayor o igual que el ubicado en el tope (si no, por 0) e incrementa el contador de programa
(deftest test-interpretar-gte-igual
  (testing "Funcionalidad de EQ en interpretar"
    (is (= [[['EQ] ['TEST]] [6 7 8 9] 1 [1] []] 
           (interpretar [['EQ] ['TEST]] [6 7 8 9] 0 [4 4] [])
        )
    )
  )
)

(deftest test-interpretar-gte-no-mayor
  (testing "Funcionalidad de EQ en interpretar"
    (is (= [[['EQ] ['TEST]] [6 7 8 9] 1 [0] []] 
           (interpretar [['EQ] ['TEST]] [6 7 8 9] 0 [2 4] [])
        )
    )
  )
)
; LT : Reemplaza los dos valores ubicados en el tope de la pila de datos por 1 si el valor ubicado debajo del tope es menor que el ubicado en el tope (si no, por 0) e incrementa el contador de programa
(deftest test-interpretar-lt-menor
  (testing "Funcionalidad de LT en interpretar"
    (is (= [[['LT] ['TEST]] [6 7 8 9] 1 [1] []] 
           (interpretar [['LT] ['TEST]] [6 7 8 9] 0 [2 4] [])
        )
    )
  )
)
(deftest test-interpretar-lt-no-menor
  (testing "Funcionalidad de LT en interpretar"
    (is (= [[['LT] ['TEST]] [6 7 8 9] 1 [0] []] 
           (interpretar [['LT] ['TEST]] [6 7 8 9] 0 [4 4] [])
        )
    )
  )
)
; LTE: Reemplaza los dos valores ubicados en el tope de la pila de datos por 1 si el valor ubicado debajo del tope es menor o igual que el ubicado en el tope (si no, por 0) e incrementa el contador de programa
(deftest test-interpretar-lte-menor
  (testing "Funcionalidad de LTE en interpretar"
    (is (= [[['LTE] ['TEST]] [6 7 8 9] 1 [1] []] 
           (interpretar [['LTE] ['TEST]] [6 7 8 9] 0 [4 4] [])
        )
    )
  )
)
(deftest test-interpretar-lte-no-menor
  (testing "Funcionalidad de LTE en interpretar"
    (is (= [[['LTE] ['TEST]] [6 7 8 9] 1 [0] []] 
           (interpretar [['LTE] ['TEST]] [6 7 8 9] 0 [4 3] [])
        )
    )
  )
)
; NEG: Le cambia el signo al valor ubicado en el tope de la pila de datos e incrementa el contador de programa
(deftest test-interpretar-neg
  (testing "Funcionalidad de NEG en interpretar"
    (is (= [[['NEG] ['TEST]] [6 7 8 9] 1 [4 -4] []] 
           (interpretar [['NEG] ['TEST]] [6 7 8 9] 0 [4 4] [])
        )
    )
  )
)

; ODD: Reemplaza el valor ubicado en el tope de la pila de datos por 1 si este es impar (si no, por 0) e incrementa el contador de programa
(deftest test-interpretar-odd-impar
  (testing "Funcionalidad de ODD en interpretar"
    (is (= [[['ODD] ['TEST]] [6 7 8 9] 1 [2 1] []] 
           (interpretar [['ODD] ['TEST]] [6 7 8 9] 0 [2 3] [])
        )
    )
  )
)
(deftest test-interpretar-odd-no-impar
  (testing "Funcionalidad de ODD en interpretar"
    (is (= [[['ODD] ['TEST]] [6 7 8 9] 1 [3 0] []] 
           (interpretar [['ODD] ['TEST]] [6 7 8 9] 0 [3 4] [])
        )
    )
  )
)
; JMP: Reemplaza el contador de programa por la direccion que forma parte de la instruccion
;(deftest test-interpretar-jmp
;  (testing "Funcionalidad de JMP en interpretar"
;    (is (= [[['JMP 1] ['TEST]] [6 7 8 9] 1 [2 3] []] 
;           (interpretar [['JMP 3] ['TEST]] [6 7 8 9] 0 [2 3] [])
;        )
;    )
;  )
;)
; JC : Saca un valor de la pila de datos y si es 0 incrementa el contador de programa (si no, reemplaza el contador de programa por la direccion que forma parte de la instruccion)
(deftest test-interpretar-jc-cero
  (testing "Funcionalidad de JC en interpretar"
    (is (= [[['JC 3] ['TEST]] [6 7 8 9] 1 [2] []] 
           (interpretar [['JC 3] ['TEST]] [6 7 8 9] 0 [2 0] [])
        )
    )
  )
)

;(deftest test-interpretar-jc-no-cero
;  (testing "Funcionalidad de JC en interpretar"
;    (is (= [[['JC 3] ['TEST]] [6 7 8 9] 3 [2] []] 
;           (interpretar [['JC 3] ['TEST]] [6 7 8 9] 0 [2 3] [])
;        )
;    )
;  )
;)
; CAL: Coloca en la pila de llamadas el valor del contador de programa incrementado en 1 y reemplaza el contador de programa por la direccion que forma parte de la instruccion
;(deftest test-interpretar-cal
;  (testing "Funcionalidad de CAL en interpretar"
;    (is (= [[['CAL 3] ['TEST]] [6 7 8 9] 3 [2 3] [1]] 
;           (interpretar [['CAL 3] ['TEST]] [6 7 8 9] 0 [2 3] [])
;        )
;    )
;  )
;)
;; RET: Saca una direccion de la pila de llamadas y la coloca en el contador de programa
;(deftest test-interpretar-ret
;  (testing "Funcionalidad de RET en interpretar"
;    (is (= [[['RET] ['TEST]] [6 7 8 9] 7 [2 3] [5]] 
;           (interpretar [['RET] ['TEST]] [6 7 8 9] 0 [2 3] [5 7])
;        )
;    )
;  )
;)


(deftest test-a-mayusculas-salvo-strings 
  (testing "Mayusculas salvo strings con diferentes llamadas"
    (is (= "  CONST Y = 2;"
      (a-mayusculas-salvo-strings "  const Y = 2;"))
    )
    (is (= "  WRITELN ('Se ingresa un valor, se muestra su doble.');"
      (a-mayusculas-salvo-strings "  writeln ('Se ingresa un valor, se muestra su doble.');"))
    )
  )
)

(deftest test-palabra-reservada
  (testing "Palabra reservada con diferentes llamadas"
    (is (= true 
           (palabra-reservada? 'CALL)
        )
    )
    (is (= true 
           (palabra-reservada? "CALL")
        )
    )
    (is (= false 
           (palabra-reservada? 'ASIGNAR)
        )
    )
    (is (= false 
           (palabra-reservada? "ASIGNAR")
        )
    )
  )
)


(deftest test-identificador
  (testing "Identificador con diferentes llamadas"
    (is (= false 
           (identificador? 2)
        )
    )
    (is (= true 
           (identificador? 'V2)
        )
    )
    (is (= true 
           (identificador? "V2")
        )
    )
    (is (= false 
           (identificador? 'CALL)
        )
    )
  )
)


(deftest test-cadena
  (testing "Es cadena? con diferentes llamadas"
    (is (= true 
           (cadena? "'Hola'")
        )
    )
    (is (= false 
           (cadena? "Hola")
        )
    )
    (is (= false 
           (cadena? "'Hola")
        )
    )
    (is (= false 
           (cadena? 'Hola)
        )
    )
  )
)


(deftest test-ya-declarado-localmente? 
  (testing "Ya declarado localmente con diferentes llamadas"
    (is (= true
      (ya-declarado-localmente? 'Y '[[0] [[X VAR 0] [Y VAR 1]]]))
    )
    (is (= false
      (ya-declarado-localmente? 'Z '[[0] [[X VAR 0] [Y VAR 1]]]))
    )
    (is (= false
      (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2]]]))
    )
    (is (= true
      (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2] [Y CONST 6]]]))
    )
  )
)


(deftest test-cargar-var-en-tabla 
  (testing "Cargar var en tabla con diferentes llamadas"
    (is (= '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]
      (cargar-var-en-tabla '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]))
    )
    (is (= '[nil () [VAR X] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]]
      (cargar-var-en-tabla '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]]))
    )
    (is (= '[nil () [VAR X Y] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]]
      (cargar-var-en-tabla '[nil () [VAR X , Y] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]]))
    )
  )
)


; user=> (inicializar-contexto-local '[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])
; [nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]
; user=> (inicializar-contexto-local '[nil () [] :sin-errores [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])
; [nil () [] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]
(deftest test-inicializar-contexto-local 
  (testing "Inicializar contexto local con diferentes llamadas"
    (is (= "[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]"
      (str (inicializar-contexto-local '[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])))
    )
    (is (= "[nil () [] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]"
      (str (inicializar-contexto-local '[nil () [] :sin-errores [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])))
    )
  )
)


; user=> (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :error [[0] []] 0 '[[JMP ?]]])
; [VAR (X , Y ; BEGIN X := 7 ; Y := 12 ; END .) [] :error [[0] []] 0 [[JMP ?]]]
; user=> (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :sin-errores [[0] []] 0 '[[JMP ?]]])
; [BEGIN (X := 7 (symbol ";") Y := 12 (symbol ";") END .) [VAR X , Y (symbol ";")] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]]
(deftest test-declaracion-var 
  (testing "Declaracion var con diferentes llamadas"
    (is (= "[VAR (X , Y ; BEGIN X := 7 ; Y := 12 ; END .) [] :error [[0] []] 0 [[JMP ?]]]"
      (str (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :error [[0] []] 0 '[[JMP ?]]])))
    )
    (is (= "[BEGIN (X := 7 ; Y := 12 ; END .) [VAR X , Y ;] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]]"
      (str (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :sin-errores [[0] []] 0 '[[JMP ?]]])))
    )
  )
)


; user=> (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])
; [+ (7 ; Y := - 12 ; END .) [VAR X , Y ; BEGIN X :=] :error [[0] [[X VAR 0] [Y VAR 1]]] 2 []]
; user=> (procesar-signo-unario [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])
; [7 (; Y := - 12 ; END .) [VAR X , Y ; BEGIN X :=] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 []]
; user=> (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])
; [7 (; Y := - 12 ; END .) [VAR X , Y ; BEGIN X := +] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 []]
; user=> (procesar-signo-unario ['- (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])
; [7 (; Y := - 12 ; END .) [VAR X , Y ; BEGIN X := -] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 []]
(deftest test-procesar-signo-unario 
  (testing "Procesar signo unario con diferentes llamadas"
    (is (= "[+ (7 ; Y := - 12 ; END .) [VAR X , Y ; BEGIN X :=] :error [[0] [[X VAR 0] [Y VAR 1]]] 2 []]"
      (str (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])))
    )
    (is (= "[7 (; Y := - 12 ; END .) [VAR X , Y ; BEGIN X :=] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 []]"
      (str (procesar-signo-unario [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])))
    )
    (is (= "[7 (; Y := - 12 ; END .) [VAR X , Y ; BEGIN X := +] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 []]"
      (str (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])))
    )
    (is (= "[7 (; Y := - 12 ; END .) [VAR X , Y ; BEGIN X := -] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 []]"
      (str (procesar-signo-unario ['- (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])))
    )
  )
)

; user=> (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []])
; [X (* 2 END .) [VAR X ; BEGIN X :=] :error [[0] [[X VAR 0]]] 1 []]
; user=> (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []])
; [END (.) [VAR X ; BEGIN X := X * 2] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL]]
(deftest test-termino 
  (testing "Termino con diferentes llamadas"
    (is (= "[X (* 2 END .) [VAR X ; BEGIN X :=] :error [[0] [[X VAR 0]]] 1 []]"
      (str (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []])))
    )
    (is (= "[END (.) [VAR X ; BEGIN X := X * 2] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL]]"
      (str (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []])))
    )
  )
)

; user=> (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []])
; [- (( X * 2 + 1 ) END .) [VAR X ; BEGIN X :=] :error [[0] [[X VAR 0]]] 1 []]
; user=> (expresion ['+ (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []])
; [END (.) [VAR X ; BEGIN X := + ( X * 2 + 1 )] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL [PFI 1] ADD]]
; user=> (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []])
; [END (.) [VAR X ; BEGIN X := - ( X * 2 + 1 )] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG]]
(deftest test-expresion 
  (testing "Expresion con diferentes llamadas"
    (is (= "[- (( X * 2 + 1 ) END .) [VAR X ; BEGIN X :=] :error [[0] [[X VAR 0]]] 1 []]"
      (str (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []])))
    )
    (is (= "[END (.) [VAR X ; BEGIN X := + ( X * 2 + 1 )] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL [PFI 1] ADD]]"
      (str (expresion ['+ (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []])))
    )
    (is (= "[END (.) [VAR X ; BEGIN X := - ( X * 2 + 1 )] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG]]"
      (str (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []])))
    )
  )
)

(deftest test-aplicar-aritmetico
  (testing "Aritmetico con diferentes llamadas"
    (is (= [3] 
           (aplicar-aritmetico + [1 2])
        )
    )
    (is (= [1 3] 
           (aplicar-aritmetico - [1 1 4])
        )
    )
    (is (= [1 8] 
           (aplicar-aritmetico * [1 2 4])
        )
    )
    (is (= [1 2] 
           (aplicar-aritmetico / [1 2 4])
        )
    )
    (is (= nil 
           (aplicar-aritmetico + nil)
        )
    )
    (is (= [] 
           (aplicar-aritmetico + [])
        )
    )
    (is (= [1] 
           (aplicar-aritmetico + [1])
        )
    )
    (is (= [1 2 4] 
           (aplicar-aritmetico 'hola [1 2 4])
        )
    )
    (is (= [1 2 4] 
           (aplicar-aritmetico count [1 2 4])
        )
    )
  )
)


(deftest test-aplicar-relacional
  (testing "Relacional con diferentes llamadas"
    (is (= [1] 
           (aplicar-relacional > [7 5])
        )
    )
    (is (= [4 1] 
           (aplicar-relacional > [4 7 5])
        )
    )
    (is (= [4 0] 
           (aplicar-relacional = [4 7 5])
        )
    )
    (is (= [4 1] 
           (aplicar-relacional not= [4 7 5])
        )
    )
    (is (= [4 0] 
           (aplicar-relacional < [4 7 5])
        )
    )
    (is (= [4 1] 
           (aplicar-relacional <= [4 6 6])
        )
    )
    (is (= '[a b c] 
           (aplicar-relacional <= '[a b c])
        )
    )
  )
)


; user=> (dump '[[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG])
; 0 [PFM 0]
; 1 [PFI 2]
; 2 MUL
; 3 [PFI 1]
; 4 ADD
; 5 NEG
; nil
; user=> (dump '[HLT])
; 0 HLT
; nil
; user=> (dump nil)
; 0 nil
; nil
;(deftest test-dump 
;  (testing "Dump con diferentes llamadas"
;    (is (= '
;      (with-out-str (dump '[[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG])))
;    )
;    (is (= '
;      (with-out-str (dump '[[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG])))
;    )
;    (is (= '
;      (with-out-str (dump '[[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG])))
;    )
;  )
;)


; user=> (generar '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]] 'HLT)
; [nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?] HLT]]
; user=> (generar '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]] 'PFM 0)
; [nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?] [PFM 0]]]
; user=> (generar '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] 'HLT)
; [nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]
; user=> (generar '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] 'PFM 0)
; [nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]
(deftest test-generar 
  (testing "Generar con diferentes llamadas"
    (is (= '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?] HLT]]
      (generar '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]] 'HLT))
    )
    (is (= '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?] [PFM 0]]]
      (generar '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]] 'PFM 0))
    )
    (is (= '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]
      (generar '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] 'HLT))
    )
    (is (= '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]
      (generar '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] 'PFM 0))
    )
  )
)


; user=> (buscar-coincidencias '[nil () [CALL X] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [A PROCEDURE 1] [X VAR 2] [Y VAR 3] [B PROCEDURE 2]]] 6 [[JMP ?] [JMP 4] [CAL 1] RET]])
; ([X VAR 0] [X VAR 2])
(deftest test-buscar-coincidencias
  (testing "Buscar coincidencias con diferentes llamadas"
    (is (= '([X VAR 0] [X VAR 2])
      (buscar-coincidencias '[nil () [CALL X] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [A PROCEDURE 1] [X VAR 2] [Y VAR 3] [B PROCEDURE 2]]] 6 [[JMP ?] [JMP 4] [CAL 1] RET]]))
    )
  )
)


; user=> (fixup ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] 1)
; [WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]
; user=> (fixup ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] 1)
; [WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP 4] [CAL 1] RET]]
; user=> (fixup ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP 4] [CAL 1] RET [PFM 2] OUT NL RET]] 0)
; [WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP 8] [JMP 4] [CAL 1] RET [PFM 2] OUT NL RET]]
(deftest test-fixup 
  (testing "Fixup con diferentes llamadas"
    (is (= '[WRITELN (END .) [] :error [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]
      (fixup ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] 1))
    )
    (is (= '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP 4] [CAL 1] RET]]
      (fixup ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] 1))
    )
    (is (= '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP 8] [JMP 4] [CAL 1] RET [PFM 2] OUT NL RET]]
      (fixup ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP 4] [CAL 1] RET [PFM 2] OUT NL RET]] 0))
    )
  )
)


; user=> (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=)
; [WRITELN (END .) [] :error [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]
; user=> (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '+)
; [WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]
; user=> (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=)
; [WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET EQ]]
; user=> (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '>=)
; [WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET GTE]]
(deftest test-generar-operador-relacional 
  (testing "Palabra reservada con diferentes llamadas"
    (is (= '[WRITELN (END .) [] :error [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]
      (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=))
    )
    (is (= '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]
      (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '+))
    )
    (is (= '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET EQ]]
      (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=))
    )
    (is (= '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET GTE]]
      (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '>=))
    )
  )
)


; user=> (generar-signo [nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-)
; [nil () [] :error [[0] [[X VAR 0]]] 1 [MUL ADD]]
; user=> (generar-signo [nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+)
; [nil () [] :error [[0] [[X VAR 0]]] 1 [MUL ADD]]
; user=> (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+)
; [nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD]]
; user=> (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '*)
; [nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD]]
; user=> (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-)
; [nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD NEG]]
(deftest test-generar-signo 
  (testing "Palabra reservada con diferentes llamadas"
    (is (= '[nil () [] :error [[0] [[X VAR 0]]] 1 [MUL ADD]]
      (generar-signo [nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-))
    )
    (is (= '[nil () [] :error [[0] [[X VAR 0]]] 1 [MUL ADD]]
      (generar-signo [nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+))
    )
    (is (= '[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD]]
      (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+))
    )
    (is (= '[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD]]
      (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '*))
    )
    (is (= '[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD NEG]]
      (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-))
    )
  )
)
