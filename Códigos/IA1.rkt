#lang racket/gui
(require (lib "graphics.ss" "graphics")); biblioteca simples de graficso
(open-graphics); abre a biblioteca
(require racket/gui/base)

(define aux 50)

;nome do jogador atual
(define nome "Maquina")
(define (salva-nome str)
  (set! nome str))


(define janela (open-viewport "galaxia" 1000 500));janela do tipo viewport
(define oculta (open-pixmap "galaxia" 1000 500))

;base grafica da janela do jogo
((draw-solid-rectangle oculta) (make-posn 0 0) 800 500 "black")
((draw-solid-rectangle oculta) (make-posn 800 0) 200 500 "darkgray")
((draw-string oculta) (make-posn 810 20) "Pontuação:");string pontuacao
((draw-string oculta) (make-posn 810 110) "Posição X:");string posicao
((draw-string oculta) (make-posn 810 180) "Dificultade:");string dificuldade

;label do rank
((draw-string oculta) (make-posn 810 260) "Rank 1:")
((draw-string oculta) (make-posn 810 285) "Rank 2:")
((draw-string oculta) (make-posn 810 315) "Rank 3:")
((draw-string oculta) (make-posn 810 340) "Rank 4:")
((draw-string oculta) (make-posn 810 365) "Rank 5:")
((draw-string oculta) (make-posn 810 390) "Rank 6:")
((draw-string oculta) (make-posn 810 415) "Rank 7:")
((draw-string oculta) (make-posn 810 440) "Rank 8:")
((draw-string oculta) (make-posn 810 465) "Rank 9:")
((draw-string oculta) (make-posn 810 490) "Rank 10:")

;simulador das caxinhas
((clear-solid-rectangle oculta) (make-posn 800 0) 4 500); R1: simula caxinha
((clear-solid-rectangle oculta) (make-posn 800 80) 200 4); R1
((clear-solid-rectangle oculta) (make-posn 800 160) 200 4); R1
((clear-solid-rectangle oculta) (make-posn 800 240) 200 4); R1

(copy-viewport oculta janela); copia de oculta para janela


;tela inicio, adicionar nome
(define inicio (instantiate dialog% ("Bem vindo capitão")))
(define jogador (new text-field% [parent inicio] [label "Nome:"]))
(define panel (new horizontal-panel% [parent inicio]
                                     [alignment '(center center)]))
(new button% [parent panel] [label "Proximo"]
    [callback (lambda (button event)  (send inicio show #f) (salva-nome (send jogador get-value)) (sleep 1))])
(send inicio show #t)


;definicao de comos os processos e imagens que se carregam ao movimento do mouse
(define (coso x y)
  ((draw-pixmap oculta) "C:/Users/Tatsunori/Documents/Racket/cometa.bmp" (make-posn x (+ y 50)))


  ((draw-pixmap oculta) "C:/Users/Tatsunori/Documents/Racket/escuro.bmp" (make-posn x y));aqui
)

(define (quadro x pos)
  (if (equal? pos 'R)
      (begin
        ((draw-pixmap oculta) "C:/Users/Tatsunori/Documents/Racket/foguete.bmp" (make-posn x 450))) ;aqui

      (if (equal? pos 'L)
          (begin
            ((draw-pixmap oculta) "C:/Users/Tatsunori/Documents/Racket/foguete.bmp" (make-posn x 450)))
          ((draw-pixmap oculta) "C:/Users/Tatsunori/Documents/Racket/foguete.bmp" (make-posn x 450))
      )
  )
  (copy-viewport oculta janela)
  ((draw-solid-rectangle oculta) (make-posn x 450) 50 50 "black") ;apaga a nave do quadro anterior, simula o movimento da nave
)

(define (posi cadeia)
  (begin
    ((draw-solid-rectangle oculta) (make-posn 830 120) 50 30 "darkgray"); quadro para evitar a sobreposicao dos numeros
    ((draw-string oculta) (make-posn 840 140) (number->string cadeia)) ;coordenadas da nave
    )
)

(define (teclado posx y x t esp cont cont2 cont3);t=tempo esp=espera
  (posi posx); chama a func posi para atualizar a posicao da nave
  ((draw-solid-rectangle oculta) (make-posn 830 40) 50 30 "darkgray"); quadra para att do contador2
  ((draw-string oculta) (make-posn 840 60) (number->string cont2));convert cont2 para string para imprimir na tela

  ((draw-solid-rectangle oculta) (make-posn 830 200) 50 30 "darkgray")
  ((draw-string oculta) (make-posn 840 220) (number->string cont3))

  (if (= t esp) ; relogio controlador da velocidade dos meteoros
      (begin
        (set! t 0);se tempo igual a espera, reinicia t a 0
        (if (= y 450) ; se y chegar a posicao 450 entao:
            (begin
              (if (and (> x (- posx 50)) (< x (+ posx 50))); evalua a batida
                  (begin
                    (close-viewport janela)
                    ((draw-string (open-viewport "Pontuação:" 250 30)) (make-posn 20 15) (string-append (string-append nome "  ") (number->string cont2)))
                    (sleep 2);para o programa por 3 sg
                    
                    (exit);fecha o game
                    )
                  ;caso nao bata
                  (begin
                    (set! cont (+ cont 1));pontuacao, meteoros desviados
                    (set! cont2 (+ cont2 1));contador do nivel

                    (begin
                      (if (= cont 9) ;sobe de nivel a cada 9
                          (begin
                            (set! cont3 (+ cont3 1)); atualiza cont3 simulando cont1

                            (if (> esp 0);se espera for maior que zero
                                (begin
                                  (set! esp (- esp 100)) ;atualiza
                                  (set! cont 0) ; atualiza o valor de cont a zero
                                  )
                                (void)
                                )
                            )
                          (void)
                          )
                      (set! y 0)
                      (set! x (random 740))
                      (coso x y)
                      )
                    )
                  )
              )

            (if (>= y 0)
                (begin
                  (set! y (+ y 15)) ;pixels em que o metoro se mexe
                  (coso x y))
                (void)
                )
            )
        )
      ;se t nao eh igual a espera
      (set! t (+ t 100))
      )
  ;delimitador a esquerda e direita
  (if (< posx 0)
      (begin
        (quadro 0 'L)
        (teclado 0 y x t esp cont cont2 cont3)
        )

      (if (> posx 740)
          (begin
            (quadro 740 'R)
            (teclado (posn-x (query-mouse-posn janela)) y x t esp cont cont2 cont3)
            )
          (void)
          )
      )


  (if ( > (+ x 50) 740) (set! aux (- x 50)) (set! aux (+ x 50)) )

  
  (if (equal? (query-mouse-posn janela) #t)
      (teclado aux y x t esp cont cont2 cont3)
      (begin
        (quadro aux 'L) ;definicao da posiicao da nave
        (teclado aux y x t esp cont cont2 cont3);logica do jogo
      
       )
    )
)

(define (main)
  (quadro 200 'R)
  (teclado 200 0 200 100 1600 0 0 1)
)

(main)


                  

  