(defvar operator '(#\+ #\- #\/ #\* #\( #\) "**"))

(setf Keywords '("and" "or" "not" "equal" "append" "concat" "set" "deffun" "for" "while" "if" "exit"))

(setf BinaryValue '("true" "false"))

; Dosyada ifade sonuna kadar okuma yapilir. Ifade sonu space, newline, tab karakterleri ile birlikte operatorler ile belirlenir.
; Eger okunan son karakter operator ise dosyada bir geri gidilir.
(defun read-str (file c)

  (setf temp (read-char file nil 'eof))
  
  (if (or (eql temp 'eof) (eql temp #\space) (eql temp #\newline) (eql temp #\tab))

    (string-downcase c)
    (if (not (eql nil (find temp operator :test #'equal)))
        (progn
            (file-position file (- (file-position file) 1))
            (string-downcase c)
        )
        (concatenate 'string (string-downcase c) (read-str file temp))
    )
  ) 
)
; Okunan karakterlerin identifier kurallarina uyup uymadigi kontrol edilir.
; Eger [az] karakterleri disinda bir sey gelirse hata verilir.
(defun IDControl (str key)

  (setf temp (read-char str nil 'the-end))
  (cond
    ( (eql temp 'the-end)
      "Identifier"
    )
    (  (or (< (char-code temp ) (char-code #\a) ) (> (char-code temp ) (char-code #\z) ))
      (error "~s is unknown" key)
    )
    (t
      (IDControl str key)
    )
  )
)
; Okunan karakterlerin integer kurallarina uyup uymadigi kontrol edilir.
; Eger [09] karakterleri disinda bir sey gelirse hata verilir.
(defun IntegerControl (str key)

  (setf temp (read-char str nil 'the-end))
  
  (cond
    ( (eql temp 'the-end)
      "Integer"
    )
    (  (or (< (char-code temp) (char-code #\0)) (> (char-code temp) (char-code #\9)))
      (error " ~s is unknown" key)
    )
    (t
      (IntegerControl str key)
    )
  )
)
; Verilen dosya acilarak helper fonksiyonu cagirilir.
(defun lexer (filename)
  (defvar result nil)
  (with-open-file (infile filename :direction :input)
    (setf result (lexer_helper infile result))
  )
  (reverse result)
)

(defun lexer_helper (file result)

  (setf c (read-char file nil 'eof))
  (setf tempR nil)

  (cond
    ; Eger dosya sonuna gelinmis ise result listesi return edilir.
    ( (eql c 'eof)
      result
    )
    ; space, newline ve tab karakterleri yok sayilir.
    ( (or (eql c #\space) (eql c #\newline) (eql c #\tab))
      (lexer_helper file result)
    )
    ; Eger girilen karakter sayi ise bosluga kadar okunur. Eger integer sinifina uygunsa resulta atilir, degil ise hata verir.
    ( (and (>= (char-code c) (char-code #\0)) (<= (char-code c) (char-code #\9)))
      (setf key (read-str file c))
      (with-input-from-string (str key)
        (push key tempR)
        (push (IntegerControl str key) tempR)
        (push tempR result)
      )
      (lexer_helper file result)
    )
    ; Karakterin operator olup olmadigi kontrol edilir.
    ( (not (eql nil (find c operator :test #'equal)))
      (cond
        ; Eger karakter * ise * operatoru mu ** operatoru mu ona bakalir.
        ( (eql c #\*)
          (setf temp (read-char file nil 'eof))
          (cond
            ((eql temp #\*)
              (push (concatenate 'string (string c) (string temp)) tempR)  
            )
            ; Eger ikinci okunan karakter * degil ise dosyada bir onceki elemana donulerek tekrar okuma yapilir.
            (t
              (push (string c) tempR)
              (if (not (eql temp 'eof)) 
                (file-position file (- (file-position file) 1))
              )
            )
          )
          (push "operator" tempR)
          (push tempR result)
        )
        ; Eger okunan karakter - ise bunun operator mu yoksa sayi mi olduguna bakilir. Bunun icin bir sonraki karakter okunur.
        ( (eql c #\-)
          (setf temp (read-char file nil 'eof))

          (cond
            ( (eql temp 'eof)
              (push (string c) tempR)
              (push "Operator" tempR)
              (push tempR result)
            )
            ; ikinci okunan deger sayi ise sonraki bosluga kadar okunur. Uygun bir sayi ise listeye atilir.
            ( (and (>= (char-code temp) (char-code #\0)) (<= (char-code temp) (char-code #\9)))
              (setf key (read-str file temp))
              (with-input-from-string (str key)
                (push (concatenate 'string (string-downcase c) key) tempR)
                (push (IntegerControl str key) tempR)
              )
              (push tempR result)
            )
            ; Eger okunan deger sayi degil ise operator oldugu anlasilir. Sonuc listeye atilir ve dosyada bir onceki elemana donulur.
            ( t
              (push (string c) tempR)
              (push "Operator" tempR)
              (push tempR result)
              (file-position file (- (file-position file) 1))
            )
          )
        )
        (t
          (push (string c) tempR)
          (push "Operator" tempR)
          (push tempR result)
        )
      )
      (lexer_helper file result)
    )
    (t
      ; Okunan karakterin id, keyword veya binary value olma durumlari kontrol edilir.
      (setf key (read-str file c))
      (cond
        ( (not (eql nil (find key Keywords :test #'equal)))
          (push key tempR)
          (push "Keyword" tempR)
        )
        ( (not (eql nil (find key BinaryValue :test #'equal)))
          (push key tempR)
          (push "BinaryValue" tempR)
        )
        ; Eger okunana string listelerden birinde bulunmaz ise id olma ihtimali degerlendirilir.
        ; ID kurallarina uygun ise listeye atilir, degil ise hata verilir.
        (t
          (with-input-from-string (str key)
            (push key tempR)
            (push (IDControl str key) tempR)
          )
        )
      )
      (push tempR result)
      (lexer_helper file result) 
    )
  )
)

(write (lexer "deneme.txt"))