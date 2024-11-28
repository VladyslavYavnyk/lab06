#lang racket

(provide (all-defined-out)) ;; Експортуємо всі визначення для можливості тестування в іншому файлі

; ІПЗ-42/4 Явник Владислав

; 1. Функція створення послідовності чисел
(define (sequence low high stride)
  ;; Якщо поточне число більше за верхню межу, повертаємо порожній список
  (if (> low high)
      null
      ;; Інакше додаємо поточне число і викликаємо функцію рекурсивно
      (cons low (sequence (+ low stride) high stride))))

; 2. Функція для додавання суфікса до кожного рядка в списку
(define (string-append-map xs suffix)
  (map (lambda (i) (string-append i suffix)) xs))

; 3. Функція для отримання елемента списку за індексом (модульний доступ)
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")] ;; Помилка, якщо індекс від'ємний
        [(null? xs) (error "list-nth-mod: empty list")]   ;; Помилка, якщо список порожній
        [#t (car (list-tail xs (remainder n (length xs))))])) ;; Дістаємо елемент за модульним індексом

; 4. Функція для отримання перших n елементів із потоку
(define (stream-for-n-steps s n)
  (if (= n 0)
      null ;; Якщо елементи закінчились, повертаємо порожній список
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1))))) ;; Рекурсивно додаємо n елементів

; 5. Потік чисел, де кожне 5-те число є від'ємним
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (zero? (remainder x 5))
                    (cons (* -1 x) (lambda () (f (+ x 1)))) ;; Від'ємні числа, кратні 5
                    (cons x (lambda () (f (+ x 1))))))])    ;; Інші числа залишаються позитивними
    (lambda () (f 1))))

; 6. Потік, який чергує між "dan.jpg" і "dog.jpg"
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (string=? x "dan.jpg")
                    (cons "dan.jpg" (lambda () (f "dog.jpg"))) ;; Якщо "dan.jpg", наступний "dog.jpg"
                    (cons "dog.jpg" (lambda () (f "dan.jpg")))))]) ;; І навпаки
    (lambda () (f "dan.jpg"))))

; 7. Функція додавання 0 як першого елемента кожного елемента потоку
(define (stream-add-zero s)
  (lambda ()
    (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

; 8. Потік, що циклічно комбінує елементи з двох списків
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n1 n2)
                (cons (cons (list-nth-mod xs n1) (list-nth-mod ys n2)) ;; Беремо елементи списків циклічно
                      (lambda () (f (+ n1 1) (+ n2 1)))))])             ;; Інкрементуємо індекси
    (lambda () (f 0 0))))

; 9. Функція для пошуку асоціації у векторі
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (cond [(= n (vector-length vec)) #f]                  ;; Якщо обійшли весь вектор, повертаємо #f
                      [(not (pair? (vector-ref vec n))) (f (+ n 1))] ;; Пропускаємо не пари
                      [(equal? (car (vector-ref vec n)) v) (vector-ref vec n)] ;; Знайшли пару
                      [#t (f (+ n 1))]))])                           ;; Рекурсія для наступного елемента
    (f 0)))

; 10. Кешований пошук асоціації
(define (cached-assoc xs n)
  (letrec ([track-next 0]                                         ;; Індекс для запису нового значення в кеш
           [cache-vector (make-vector n #f)]                      ;; Створення вектора кешу
           [f (lambda (v)
                (let ([ans (vector-assoc v cache-vector)])         ;; Пошук у кеші
                  (if ans
                      ans                                         ;; Якщо знайдено в кеші
                      (let ([new-ans (assoc v xs)])               ;; Пошук у списку
                        (if new-ans
                            (begin
                              (vector-set! cache-vector track-next new-ans) ;; Додаємо до кешу
                              (set! track-next (remainder (+ track-next 1) n)) ;; Оновлюємо індекс кешу
                              new-ans)                           ;; Повертаємо результат
                            #f)))))])                           ;; Повертаємо #f, якщо нічого не знайдено
    f))
