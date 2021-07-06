;;**************************************************************************************************
;;シーケンス用３線、２線を描く Izawaさんのsq3wを変更;dimscaleに関係なくピッチを50に変更
;;    ポリラインに変更
;;**************************************************************************************************

(defun C:SQ3W() (while (sqww 3 1.0))(princ))

(defun C:SQ2W() (while (sqww 2 1.0))(princ))

(defun sqww (sqn ww / dimsc sqw ss cnt i ename ent k kelist kelistang n keisuu w p1n p2n pst s j POS3 POS4)
   (setvar "CMDECHO" 0)
   (setq dimsc 50 sqw (* ww dimsc))
   (setq U5 (getvar "USERS5"))
   (if (= U5 "M")
       (progn
          (setq p1 (getpoint "\n始点を指示 <Enter=終了> : "))
          (setq p2 (getpoint p1 "\n終点を指示 : "))
       )
   )
   (if (= U5 "A")(princ "自動モード"))
   (if p2
       (progn
             (if (setq ss (ssget "F" (list p1 p2) '((0 . "LWPOLYLINE"))))
                 (progn
                    (setq cnt (sslength ss) i 0)
                      (repeat cnt
                         (setq ename (ssname ss i) ent (entget ename))
                           (EPL2);(princ "\nKELIST=")(princ KELIST)
                           (if (setq k (inters p1 p2 POS3 POS4 nil));;(cdr (assoc 10 ent))(cdr (assoc 11 ent)) nil))
                               (setq kelist (append kelist (list (list k ename))))
                           )
                           ;(princ "\nPOS3=")(princ POS3)(princ "\nPOS4=")(princ POS4)
                           (setq i (1+ i))
                      )
                   ;;enameを始点からの近い順にソート
                     (setq kelist (pesort p1 kelist));  (princ "\nK=")(princ K)
                     (setq ang (angle p1 p2))
                     (if (or (< ang (* pi 0.5))(> ang (* pi 1.5)))
                         (setq ang (- ang (/ pi 2.0)))
                         (setq ang (+ ang (/ pi 2.0)))
                     )
                   ;;線分が３より多いときは３にする
                     (if (> (setq n (length kelist)) 3)(setq n 3))
                   ;;２線の時
                     (if (and (= n 3)(= sqn 2))
                         (progn
                             (if (= U5 "A")(setq kw "RT"))
                             (if (= U5 "M")
                                 (progn
                                     (initget "RS TS RT")
                                     (if (not (setq kw (getkword "\n相を選択 /RS:R-S /TS:T-S /RT:R-T <Enter=RS>:")))
                                         (setq kw "RS")
                                     )
                                 )
                             )
                             (cond
                                 ((= kw "RS")(setq s 2))
                                 ((= kw "TS")(setq s 0))
                                 ((= kw "RT")(setq s 1))
                             )
                         )
                     )
                     (if (or (= n 2) s)
                         (if (= s 0)
                             (setq keisuu '(0.5 -0.5));;2002.8.1
                             (setq keisuu '(-0.5 0.5))
                         )
                         (setq keisuu '(-1.0 0.0 1.0))
                     )
                     (setq i 0 j 0)
                        (repeat n
                          (if (not (and s (= i s)))
                              (progn
                                 (setq w (* sqw (nth j keisuu)))
                                 (setq p1n (polar p1 ang w) p2n (polar p2 ang w))
                                 (setq ent (entget (cadr (nth i kelist))));;; (princ "\nENT=")(princ ENT)
                                 ;;交点を求め直す
                                   (EPL2)
                                   (setq pst (inters POS3 POS4 p1n p2n nil))
                                   (princ "\nPST=")(princ PST)
                                   (princ "\nJ = ")(princ J)
                                   ;(setq pst (inters (cdr (assoc 10 ent))(cdr (assoc 11 ent)) p1n p2n nil))
                                 ;;線分を作成
                                   (entmake (list '(0 . "LWPOLYLINE")(cons 10 pst)(cons 10 p2n)))
                                   (command "INSERT" "kouten" pst "1" "1" "0")
                                   (setq j (1+ j))
                              )
                          )
                          (setq i (1+ i))
                        );end repeat
                 );end progn 21
                 (progn
                   (if (= sqn 2)(setq keisuu '(-0.5 0.5))
                                (setq keisuu '(-1.0 0.0 1.0))
                   )
                   (setq ang (+ (angle p1 p2)(* pi 0.5)) i 0)
                       (repeat sqn
                          (setq w (* sqw (nth i keisuu)))
                          (EPL2)
                          (setq POS3 (polar p1 ang w) POS4 (polar p2 ang w))
                          (entmake (list '(0 . "LWPOLYLINE")(cons 10 POS3)(cons 10 POS4)))
                          (setq i (1+ i))
                       )
                 );end progn 79
             );end if    20
       );end progn 19
   );end if    18
 (redraw)
);end defun
;;--------------------------------------------------------------------------------------------------
(defun peswap(plist a b / reslist i j )
	(setq i 0)
	(repeat (length pelist)
		(if (= i a)(setq j b)(if (= i b)(setq j a)(setq j i)))
		(setq reslist (append reslist (list (nth j pelist))) i (1+ i))
	)
	reslist;
)
(defun pesort(p pelist / n i j d1 d2) 
	(setq n (length pelist) i 0)
	(repeat (1- n)
		(setq d1 (distance p (car (nth i pelist))) j (1+ i))
		(repeat (- n i 1)
			(setq d2 (distance p (car (nth j pelist))))
			(if (> d1 d2) (setq pelist (peswap pelist i j) d1 d2))
			(setq j (1+ j))
		) 
		(setq i (1+ i))
	)
	pelist;
)
;*************************************************
;PLINEの始点・終点を取得する
;*************************************************
(defun EPL2( / j n ENo P1L P2L PS PE sum atm)
  (prompt "\n==> EPL2")
  (setq sum 0.0)
    (progn
      (setq j 0)(setq n 0)
      (repeat (length ent)
        (setq atm (nth j ent) ENo (car atm))
        (if (= ENo 10)
          (progn
            (setq n (1+ n))
            (if (> n 1)
              (progn;                    終点:n > 1
                (setq P2L (cdr atm))
                (setq sum (+ sum (distance P1L P2L)))
                (setq P1L P2L)
              );progn
              (progn 
                  (setq P1L (cdr atm))
                  (setq Pos3 P1L)        ;始点:n = 1
                  (setq PS P1L)
              )
            );if
          );progn
        );if
        (setq j (1+ j))
      );repeat
      (setq Pos4 P2L) (setq PE P2L);始点 = P1L,POS3 終点 = P2L,POS4
    );progn
)