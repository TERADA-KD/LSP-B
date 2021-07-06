;;2002.8.3 Update
;;ｼｰｹﾝｽ回路用LISP
;;SQYA:矢印線を作成．SQ1Y:片矢．SQ2Y:両矢．SQ3Y:線ｻｲｽﾞ用．
;;SQSC:ｼｰﾙﾄﾞﾏｰｸを作成．SQ0S:ｼﾝﾎﾞﾙ無し SQ1S:横線．SQ2S:斜線を作成．
;;SQNW:複線を作成．SQ2W:２線 SQ3W:３線．
;;SQTW:２線をﾂｲｽﾄﾍﾟｱに変える．
(setvar "CMDECHO" 1)

;;**********************************************************
;;矢印付き線分を作成する（引出線）
;;**********************************************************
(defun c:sq1y()
	(ifs_make_sqya 20 30.0 1 0.5)
	(princ)
)
;;両矢印付き線分を作成する.
;;の時は、線分の中心に文字列を描く。
;(defun c:sq2ya()
;	(ifs_make_sqya 20 30.0 2 nil)
;	(princ)
;)
;;両矢印付き線分+文字列を作成する.
(defun c:sq2y()
	(ifs_make_sqya 20 30.0 3 0.5)
	(princ)
)
(defun c:sqya(/ dimsc l kakudo n txth temp rad w p1 p2 px ang ename txt etext tbox dist)
	(setq dimsc 50)
	(setq l 20 kakudo 30.0 n 3 txth 0.5)
	(if (setq temp (getreal (strcat "\n矢印の長さ <" (rtos l) "> : ")))
		(setq l temp)
	)
	(if (setq temp (getreal "\n矢印の開き角度 <30.0> : "))
		(setq kakudo temp)
	)
	(initget "1 2 3")
	(if (setq temp (getkword "\n1:片矢印 /2:両矢印 /3:両矢印+文字 <Enter=3> : "))
		(setq n (atoi temp))
	)
	(if (and (= n 3)(setq temp (getreal (strcat "\n文字高さ <" (rtos txth) "> : "))))
		(setq txth temp)
	)
	(ifs_make_sqya l kakudo n txth)
	(princ)
)
;;PLINEで矢印を作る
(defun ifs_make_plya(
	l		;;長さ
	kakudo	;;開き角度
	px		;;先端座標
	ang		;;回転角度
	/ dimsc rad)
	(setq dimsc 50)
;	(setq dimsc (getvar "DIMSCALE") l (* l dimsc))
	(setq rad (/ (* kakudo pi) 180.0 2.0) w (* (abs l) (/ (sin rad)(cos rad)) 2.0))
	(entmake (list 
		'(0 . "LWPOLYLINE")'(90 . 2)
		(cons 10 px)(cons 40 0.0)(cons 41 w)(cons 42 0.0)
		(cons 10 (polar px ang l))(cons 40 w)(cons 41 w)(cons 42 0.0)
	))
)
;;矢印付き線分を作成する
;;両矢印の時は、線分の中心に文字列を描く。
(defun ifs_make_sqya(l kakudo n txth / mes tsw dimsc temp rad w p1 p2 px ang ename txt etext tbox dist)
	(setq dimsc 50)
	(if txth (setq txth (* txth dimsc)))	;;文字高さ
	(if (= n 3)(setq n 2 tsw 2))			;;矢印の作成個数
	(if (= n 1)
		(setq mes "\n通過点または終点 : " tsw 0)
		(setq mes "\n終点 : ")
	)
	(if (setq p1 (getpoint "\n始点 : "))
		(setq p2 (getpoint p1 mes))
	)
	(if p2
		(progn
			(setq px p1 ang (angle p1 p2))
			(entmake (list '(0 ."LINE")(cons 10 p1)(cons 11 p2)))
			(setq ename (entlast));;線分を保持
			(repeat n
				(ifs_make_plya l kakudo px ang)
				(setq px p2 l (* l -1.0))
			)
			(if (= n 1)
				(if (setq p3 (getpoint p2 "\n終点を指示 <Enter=終了 > : "))
					(progn
						(entmake (list '(0 ."LINE")(cons 10 p2)(cons 11 p3)))
						(setq ename (entlast))
						(setq p1 p2 p2 p3 ang (angle p1 p2) tsw 1)
					)
					(setq tsw 0)
				)
			)
		)
	)
	(if (and p2 tsw (setq txt (getstring T "\n記入文字を入力 <Enter=文字なし> : ")))
		(progn
			(if (< tsw 2)
				(if (equal ang pi (* pi 0.5))
					;;線が左方向の時
					(progn
						(setq ang (- ang pi))
						(cond
							((= tsw 0)(setq px (polar p2 ang txth) code72 0 code73 1));;BL
							((= tsw 1)(setq px (polar p1 ang (* txth -1.0)) code72 2 code73 1));;BR
						)
					)
					;;線が右方向の時
					(progn
						(cond
							((= tsw 0)(setq px (polar p2 ang (* txth -1.0)) code72 2 code73 1));;BR
							((= tsw 1)(setq px (polar p1 ang txth) code72 0 code73 1));;BL
						)
					)
				)
				;;両矢印
				(setq px (list (/ (+ (car p1)(car p2)) 2.0)(/ (+ (cadr p1)(cadr p2)) 2.0))
						code72 1 code73 2)
			)
			(entmake (list '(0 . "TEXT")(cons 10 px)(cons 72 code72)(cons 73 code73)
				(cons 1 txt)(cons 11 px)(cons 40 txth)(cons 50 ang)(cons 41 0.8);;文字幅係数
			))
			(setq	etext (entlast);;文字列を保持
					tbox (cadr (textbox (entget etext)))
					dist (+ (/ (car tbox) 2.0)(cadr tbox))
			)
			(if (= tsw 1)
				;;下線の長さを調整
				(progn
					(setq ent (entget ename)
						p1 (cdr (assoc 10 ent)) p2 (cdr (assoc 11 ent)) ang (angle p1 p2)) 
					(setq p2 (polar p1 ang (* dist 2.0)))
					(entmod (subst (cons 11 p2)(assoc 11 ent) ent))
				)
			)
			(if (= tsw 2)
				;;両矢印で文字付きの時、線分の中央をカット
				(progn
					(setvar "CMDECHO" 0)
					(command "BREAK" ename (polar px ang dist)(polar px ang (* dist -1.0)))
					(redraw etext)
				)
			)
		)
	)
)
;;**********************************************************
;;シールド線を描く
;;**********************************************************
(defun c:sq0s()
	(ifs_make_sqsc 0)
	(princ)
)
(defun c:sq1s()
	(ifs_make_sqsc 1)
	(princ)
)
(defun c:sq2s()
	(ifs_make_sqsc 2)
	(princ)
)
(defun c:sqsc( / typ kw)
	(setq dimsc 50)
	(setq typ 3)
	(initget "0 1 2")
	(if (setq kw (getkword "\nｱｰｽのｼﾝﾎﾞﾙを選択 /0:ｼﾝﾎﾞﾙ無し /1:横線3本 /2:斜線3本 <Enter=横線> : "))
		(setq typ (atoi kw))
		(setq typ 1)
	)
	(ifs_make_sqsc typ)
	(princ)
)
;;シールド線を描く
(defun ifs_make_sqsc(typ / dimsc p1 p2 ang pa pb pc pd pe kw typ)
	(setq dimsc 50)(setq rad (* 0.2 dimsc));;半径0.2
	(if (setq p1 (getpoint "\n始点を指示 : "))
		(setq p2 (getpoint p1 "\n終点を指示 : "))
	)
	(if p2
		(progn
			;;線種がロードされていなければロードする
			(load_line "DASHED" "ICADISO.LIN")
			(setq ang (angle p1 p2) pe (polar p2 ang rad))
			(entmake (list '(0 . "LWPOLYLINE")'(90 . 4)'(70 . 1)'(6 . "DASHED")
				(cons 10 (polar p1 (+ ang (/ pi 2.0)) rad))'(40 . 0)'(41 . 0)'(42 . 1.0)
				(cons 10 (polar p1 (- ang (/ pi 2.0)) rad))'(40 . 0)'(41 . 0)'(42 . 0.0)
				(cons 10 (polar p2 (- ang (/ pi 2.0)) rad))'(40 . 0)'(41 . 0)'(42 . 1.0)
				(cons 10 (polar p2 (+ ang (/ pi 2.0)) rad))'(40 . 0)'(41 . 0)'(42 . 0.0)
			))
		)
	)
	(if (setq pf pe)
		(progn
			;;交点を挿入する
			(insert_kouten "I_KOUTEN" pf dimsc)
			(while pf
				(if (setq pf (getpoint pe "\n通過点を指示 <Enter=終了> :"))
					(progn
						(entmake (list '(0 . "LINE");;'(6 . "DASHED")
							(cons 10 pe)(cons 11 pf)))
						(setq pe pf)
					)
				)
			)
			(if ( > typ 0)
				;;アースを挿入する
				(insert_earth typ pe dimsc)
			)
		)
	)
	(princ)
)
;;**********************************************************
;;交点マークを挿入する。レイヤ名は『KOUTEN』固定。
;;**********************************************************
(defun insert_kouten(blkname pos sc / clay)
	(setq clay (getvar "CLAYER"))
	;;ﾌﾞﾛｯｸが無ければぶろっくを作る
;	(if (not (tblsearch "BLOCK" blkname))(ifs_make_kouten blkname))
	(setq BLKNAME "KOUTEN")
	(setvar "CMDECHO" 0)
	(command "-LAYER" "M" "KOUTEN" "")
	(command "INSERT" blkname POS "1" "1" "0")
	(setvar "CLAYER" clay)
)
(defun insert_earth(typ pos sc / clay blkname)
	;;ﾌﾞﾛｯｸが無ければぶろっくを作る
	(setq blkname (strcat "I_SQE" (itoa typ)))
;	(if (not (tblsearch "BLOCK" blkname))
;		(if (= typ 1)
;			(ifs_make_sqe1)
;			(ifs_make_sqe2)
;		)
;	)
	(setq BLKNAME "SET")
	(setvar "CMDECHO" 0)
	(command "INSERT" blkname pos "1" "1" "0")
)
;;画層０で色はByBlock(62 . 0)で作る
(defun ifs_make_kouten(blkname)
	;;62:線色0=ByBlock 256=ByLayer
	;;8:画層
	(entmake (list '(0 . "BLOCK")(cons 2 blkname)'(10 0.0 0.0 0.0)'(70 . 0)))
	(entmake '((0 . "CIRCLE")(8 . "0")(10 0.0 0.0 0.0)(40 . 0.10)(62 . 0)))
	(entmake '((0 . "CIRCLE")(8 . "0")(10 0.0 0.0 0.0)(40 . 0.20)(62 . 0)))
	(entmake '((0 . "CIRCLE")(8 . "0")(10 0.0 0.0 0.0)(40 . 0.30)(62 . 0)))
	(entmake '((0 . "CIRCLE")(8 . "0")(10 0.0 0.0 0.0)(40 . 0.40)(62 . 0)))
	(entmake '((0 . "CIRCLE")(8 . "0")(10 0.0 0.0 0.0)(40 . 0.50)(62 . 0)))
	(entmake '((0 . "ENDBLK")))
)
(defun IFS_MAKE_SQE2();;斜線３本のアース
	(entmake '((0 . "BLOCK")(2 . "I_SQE2")(10 0.0 0.0 0.0)(70 . 0)))
	(entmake '((0 . "LINE")(8 . "0")(62 . 0)(6 . "BYBLOCK")(10 -1.0 0.0 0.0)(11 -2.5 -1.5 0.0)))
	(entmake '((0 . "LINE")(8 . "0")(62 . 0)(6 . "BYBLOCK")(10 -2.5 0.0 0.0)(11 2.5 0.0 0.0)))
	;;(entmake '((0 . "LINE")(8 . "0")(62 . 0)(6 . "BYBLOCK")(10 0.0 2.5 0.0)(11 0.0 0.0 0.0)))
	(entmake '((0 . "LINE")(8 . "0")(62 . 0)(6 . "BYBLOCK")(10 0.5 0.0 0.0)(11 -1.0 -1.5 0.0)))
	(entmake '((0 . "LINE")(8 . "0")(62 . 0)(6 . "BYBLOCK")(10 2.0 0.0 0.0)(11 0.5 -1.5 0.0)))
	(entmake '((0 . "ENDBLK")))
)
(defun IFS_MAKE_SQE1();;横線３本のアース
	(entmake '((0 . "BLOCK")(2 . "I_SQE1")(10 0.0 0.0 0.0)(70 . 0)))
	(entmake '((0 . "LINE")(8 . "0")(62 . 0)(6 . "BYBLOCK")(10 -0.833333333333 -2.0 0.0)(11 0.833333333333 -2.0 0.0)))
	(entmake '((0 . "LINE")(8 . "0")(62 . 0)(6 . "BYBLOCK")(10 2.5 0.0 0.0)(11 -2.5 0.0 0.0)))
	(entmake '((0 . "LINE")(8 . "0")(62 . 0)(6 . "BYBLOCK")(10 -1.666666666667 -1.0 0.0)(11 1.666666666667 -1.0 0.0)))
	;;(entmake '((0 . "LINE")(8 . "0")(62 . 0)(6 . "BYBLOCK")(10 0.0 0.0 0.0)(11 0.0 2.5 0.0)))
	(entmake '((0 . "ENDBLK")))
)
;;線種がロードされていなければロードする
(defun load_line(lin_name lin_file)
	(if (not (tblsearch "LTYPE" lin_name))
		(progn
			(setvar "CMDECHO" 0)
			(setvar "FILEDIA" 0)
			(command "LINETYPE" "L" lin_name lin_file "")
			(setvar "FILEDIA" 1)
		)
	)
)
;;**********************************************************
;;シーケンス用矢印（電線サイズ記入）
;;**********************************************************

(defun c:sq3y( / dimsc p1 p2 ss cnt ename ent k kelist i ang item p3 str)
	(setq dimsc 50)(seyq txth (* 2.0 dimsc))
	(if (setq p1 (getpoint "\n始点を指示 <Enter=終了> : "))
		(setq p2 (getpoint p1 "\n通過点、または終点を指示 : "))
	)
	(if p2
		(progn
			(if (setq ss (ssget "F" (list p1 p2) '((0 . "LINE"))))
				(progn
					(setq cnt (sslength ss) i 0)
					(repeat cnt
						(setq ename (ssname ss i) ent (entget ename))
						(if (setq k (inters p1 p2 (cdr (assoc 10 ent))(cdr (assoc 11 ent)) nil))
							(setq kelist (append kelist (list (list k ename))))
						)
						(setq i (1+ i))
					)
					;;enameを始点からの近い順にソート
					(setq kelist (pesort p1 kelist))
					;;線分の角度
					(setq ang (angle p1 p2))
					;;線分を書く
					(entmake (list '(0 . "LINE")(cons 10 (car (nth 0 kelist)))(cons 11 p2)))
					(setq ename (entlast))
					;;矢印を書く
					(foreach item kelist
						(ifs_make_plya 1.5 20.0 (car item) ang)
					)
					;;水平でなければ
					(if (not (or (< ang 0.01)(> ang (- (* pi 2.0) 0.01))(equal ang pi 0.01)))
						(if (setq p3 (getpoint p2 "\n終点を指示 : "))
							(progn
								(entmake (list '(0 . "LINE")(cons 10 p2)(cons 11 p3)))
								(setq ename (entlast) p1 p2 p2 p3 tsw 1)
							)
						)
					)
					(if (equal ang pi (/ pi 2.0))
						;;左
						(if (< tsw 1)
							(setq p2 (polar p2 0.0 txth) code72 0)
							(setq p2 (polar p1 pi txth) code72 2)
						)
						(if (< tsw 1)
							(setq p2 (polar p2 pi txth) code72 2)
							(setq p2 (polar p1 0.0 txth) code72 0)
						)
					)
					(if (setq str (getstring "\n記入文字を入力 : " T))
						(progn
							(entmake (list 
								'(0 . "TEXT")(cons 10 p2)(cons 11 p2)(cons 40 txth)(cons 1 str)
								'(50 . 0.0)'(41 . 1.0)(cons 72 code72)'(73 . 1)
							))
							(if (= tsw 1)
								(progn
									(setq	etext (entlast);;文字列を保持
											tbox (cadr (textbox (entget etext)))
											dist (+ (car tbox)(* (cadr tbox) 2.0))
									)
									(setq ent (entget ename) p1 (cdr (assoc 10 ent)) p2 (cdr (assoc 11 ent)))
									(entmod (subst (cons 11 (polar p1 (angle p1 p2) dist))(assoc 11 ent) ent))
								)
							)
						)
					)
				)
			)
		)
	)
	(princ)
)
;;----------------------------------------------------------
;;２線分をツイストペアに変える
;;----------------------------------------------------------
(defun c:sqtw( / my_error r rdo p1 p2 ss cnt i ename1 ename2 ent1 ent2 k k1 k2 cen p1x p2x p1y p2y ename)
	(defun my_error(msg)
		(setvar "FILLETRAD" frad)
		(setvar "OSMODE" osm)
		(princ)
	)
	(setq
		r 0.5;;丸め半径
		rdo (* pi 0.1);;片側開き角度ﾗｼﾞｱﾝ
	)
	(setq osm (getvar "OSMODE"))
	(setvar "CMDECHO" 0)
	(setvar "OSMODE" 0)
	(setq old_error *error*)
	(setq *error* my_error)
	
	(setq frad (getvar "FILLETRAD"))
	(setq dimsc 50)
	(setvar "FILLETRAD" (* r dimsc));;丸め半径は0.5

	(if (setq p1 (getpoint "\n線分２本をﾌｪﾝｽで選択 ﾌｪﾝｽの始点を指示 : "))
		(setq p2 (getpoint p1 "\nﾌｪﾝｽの終点を指示 : "))
	)
	(if (setq ss (ssget "F" (list p1 p2) '((0 . "LINE"))))
		(if (= (sslength ss) 2)
			(progn
				(setq ename1 (ssname ss 0) ent1 (entget ename1)
					  p1a (cdr (assoc 10 ent1)) p2a (cdr (assoc 11 ent1))
					  k1 (inters p1 p2 p1a p2a  nil) ang1 (angle p1a p2a))
				(setq ename2 (ssname ss 1) ent2 (entget ename2)
					  p1b (cdr (assoc 10 ent2)) p2b (cdr (assoc 11 ent2))
					  k2 (inters p1 p2 p1b p2b  nil) ang2 (angle p1b p2b))
				;;ﾌｪﾝｽの中心
				(setq cen (list (/ (+ (car k1)(car k2)) 2.0)(/ (+ (cadr k1)(cadr k2)) 2.0)))
				(setq i 0)
				(repeat 2
					(if (= i 0)
						(setq p1x p1a p2x p2a p1y p1b p2y p2b ang ang1 ename ename2)
						(setq p1x p1b p2x p2b p1y p1a p2y p2a ang ang2 ename ename1)
					)
					(setq k (inters p1x p2x cen (polar cen (+ ang (* pi 0.5)) 100.0) nil))
					(setq ang (angle cen k) k (polar k ang (- (/ r (sin rdo)) r)))
					
					(setq k1 (inters p1y p2y k (polar k (+ ang rdo) 100.0) nil)
						  k2 (inters p1y p2y k (polar k (- ang rdo) 100.0) nil))
					(entmake (list '(0 . "LINE")(cons 10 k1)(cons 11 k)))
					(setq elast1 (entlast))
					(entmake (list '(0 . "LINE")(cons 10 k)(cons 11 k2)))
					(setq elast2 (entlast))
					(command "FILLET" elast1 elast2)
					(command "BREAK" ename k1 k2)
					(setq elast3 (entlast))
					(setq ent3 (entget elast3) p1c (cdr (assoc 10 ent3)) p2c (cdr (assoc 11 ent3)))
					(if (or (< (distance k1 p1c) 0.01)(< (distance k1 p2c) 0.01))
						(progn
							(command "FILLET" elast3 elast1)
							(command "FILLET" ename elast2)
						)
						(progn
							(command "FILLET" elast3 elast2)
							(command "FILLET" ename elast1)
						)
					)
					(setq i (1+ i))
				)
				(redraw)
			)
			(princ "\n線分が２本ではない.")
		)
	)
	(setq *error* old_error)
	(setvar "OSMODE" osm)
	(setvar "FILLETRAD" frad)
	(princ)
)
;(princ "\nSQYA:矢印線を作成．SQ1Y:片矢．SQ2Y:両矢．SQ3Y:線ｻｲｽﾞ用．")
;(princ "\nSQSC:ｼｰﾙﾄﾞﾏｰｸを作成．SQ0S:ｼﾝﾎﾞﾙ無し SQ1S:横線．SQ2S:斜線を作成．")
;(princ "\nSQNW:複線を作成．SQ2W:２線 SQ3W:３線．")
;(princ "\nSQTW:２線をﾂｲｽﾄﾍﾟｱに変える．")
(princ)
