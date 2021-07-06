;;----------------------------------------------------------
;;AddNum2:数字混じりの文字列(TEXT)の数字部分を加減算する。
;;                                                  2001.3.9
;;AddNCopy:文字列の数字部分を＋１しながら指定回数コピーする
;;                                                 2001.3.11
;;TCopyEd:文字列をコピーしながら編集、または数字部を＋１する
;;                                                 2001.3.12
;;                    URL http://plaza29.mbn.or.jp/~f_izawa/
;;-----------------------------------------------------------
(defun ifs_error(msg)
  (setq *error* old_error)
  (princ (strcat "\n" msg))
  (if (getvar "ICADVER")(icad_esc))
  (princ)
)

;;**********************************************************
;;AddNum2:数字混じりの文字列(TEXT)の数字部分を加減算する。
;;**********************************************************
(defun c:AddNum2( / kw_res kw iti rflag addnum i ss ent lstcnt cnt j rflag res item txt anstxt addtxt)
        (princ "\nAddNum2 :数字混じりの文字の数字部分を加減算します．")
	(setq i 0 iti 1)
	(setq kw_res (get_kw_001))
	(setq iti (car kw_res) rflag (cadr kw_res))
	(if (/= iti 0)
		(setq addnum (getint "\n加算する数値（整数）を入力: "))
		(princ "\n位置番号が異常です．")
	)
	(if (and (/= addnum nil)(/= addnum 0))
		(progn
			(prompt "\n対象文字(TEXT)を選んで下さい : ")
			(if (= (setq ss (ssget (list (cons 0 "TEXT")))) nil)
				(princ "\n何も選択されませんでした。")
				(repeat (sslength ss)
					(setq ent (entget (ssname ss i)))
					(setq txt (cdr (assoc 1 ent)))
					;;文字列を数字と文字に分離
					(setq lst (ifs_txt_bunri txt))
					;;後ろから数える時は、リストを反転
					(if rflag (setq lst (reverse lst)))
					(setq j 0 cnt 0 ansiti 0)
					;;文字列リスト内の位置を取得
					(foreach item lst
						(if (ifs_is_suuji item)
							(progn
								(setq cnt (1+ cnt))
								(if (= cnt iti)(setq ansiti (1+ j) anstxt item))
							)
						)
						(setq j (1+ j))
					)
					(if (> ansiti 0)
						(progn
							;;０を付加し最初の桁数に戻す
							(setq addtxt (itoa (+ (atoi anstxt) addnum)))
							(if (> (strlen anstxt)(strlen addtxt))
								(setq addtxt (ifs_add_zero addtxt (strlen anstxt)))
							)
							;;対象部分の文字列リストを入れ替える
							(setq lst (ifs_txtlst_conv lst ansiti addtxt))
							;;後ろから数える時は、リストを反転し、元に戻す
							(if rflag (setq lst (reverse lst)))
							;;文字列リストを１つに結合
							(setq restxt "")
							(foreach item lst (setq restxt (strcat restxt item)))
							;;結果を表示
							(princ (strcat "\n" txt " -> " restxt))
							;;文字列を書き換え
							(setq ent (subst (cons 1 restxt )(assoc 1 ent) ent))
							(entmod ent)
						)
					)
					(setq i (1+ i))
				)
			)
		)
		(princ "\n加算する数値が異常か０です．")
	)
	(princ)	
)
;;**********************************************************
;;AddNcopy:文字列の数字部分を＋１しながら指定回数コピーする
;;**********************************************************
(defun c:AddNCopy( / kw_res kw i sel ent txt pos10 pos11 iti kai posa posb sax say cnt rflag ansiti
                     anstxt txtlst restxt maetxt atotxt new10 new11 txtcnt cnt kai keta item lstans )
        (princ "\nAddNCopy:文字列の数字部分を＋１しながら指定回数コピーします．")
	(setq old_error *error*)
	(setq *error* ifs_error)

	(setq iti 1 i 0)
	(if (setq sel (entsel "\n基準文字を選択: "))
		(progn
			(setq ent (entget (car sel)))
			(if (= (cdr (assoc 0 ent)) "TEXT")
				(progn
					(setq txt (cdr (assoc 1 ent)) pos10 (cdr (assoc 10 ent)) pos11 (cdr (assoc 11 ent)))
					(setq txtlst (ifs_txt_bunri txt) txtcnt (length txtlst))
					(if (> (get_suuji_cnt txtlst) 1)
						(progn
							(setq kw_res (get_kw_001))
							(setq iti (car kw_res) rflag (cadr kw_res))
						)
					)
					(setq kai (getint "\nコピー回数を入力; "))
				)
				(princ "\n文字列(TEXT)ではありません．")
			)
		)
	)
;	(if kai
;		(if (setq posa (getpoint "\n基準点を指示 :"))
;			(if (setq posb (getpoint "\nコピー先を指示 :"))
;				(setq sax (- (car posb)(car posa)) say (- (cadr posb)(cadr posa)))
;				(princ "\n指示点エラー")
;			)
;			(princ "\n指示点エラー")
;		)
;		(princ "\n文字列ではないか、入力数値のエラーです．")
;	)
	(if kai
		(if (setq posa (getpoint "\n基準点を指示 :"))
			(progn
				;;文字のイメージを見せるためにCOPYコマンドを使う
				(prompt "\nコピー先を指示: ")
				(setvar "CMDECHO" 0)
				(command "COPY" "single" (car sel) posa pause)
				(if (not (equal posa (setq posb (getvar "LASTPOINT"))))
					(setq sax (- (car posb)(car posa)) say (- (cadr posb)(cadr posa)))
					(progn
						(princ "\n指示点エラー")
					)
				)
				;;最後を削除、念のために文字列で比較
				(setq ent (entget (entlast)))
				(if (= txt (cdr (assoc 1 ent))) 
					(command "ERASE" "single" "last")
				)
				(setvar "CMDECHO" 1)
			)
			(princ "\n指示点エラー")
		)
		(princ "\n文字列ではないか、入力数値のエラーです．")
	)
	(if sax
		(progn
			(setq lstans (get_txtlst_itians txtlst iti rflag))
			(setq ansiti (car lstans) anstxt (cadr lstans))
			(setq keta (strlen anstxt))
			(if rflag (setq ansiti (1+ (- txtcnt ansiti))))
			(setq maeato (get_maeato_txt txtlst ansiti))
			(setq maetxt (car maeato) atotxt (cadr maeato))
			(setq i 1)
			(repeat kai
				(setq restxt (itoa (+ (atoi anstxt) i)))
				;;最初の桁数に満たない時は文字列の前に０を付ける。
				(if (> keta (strlen restxt))(setq restxt (ifs_add_zero restxt keta)))
				;;新しく文字を作成する
				(setq ent (subst (cons 1 (strcat maetxt restxt atotxt))(assoc 1 ent) ent))
				(setq new10 (list (+ (car pos10)(* sax i))(+ (cadr pos10)(* say i)) 0.0)
					  ent  (subst (cons 10 new10)(assoc 10 ent) ent))
				(setq new11 (list (+ (car pos11)(* sax i))(+ (cadr pos11)(* say i)) 0.0)
					  ent  (subst (cons 11 new11)(assoc 11 ent) ent))
				(entmake ent)
				(setq i (1+ i))
			)
		)
	)
	(setq *error* old_error)
	(princ)	
)
;;**********************************************************
;;TCopyEd:文字列をコピーしながら編集、または数字部を＋１する
;;**********************************************************
(defun c:TCopyEd( / kw2 iti lstans ansiti anstxt keta maeato maetxt atotxt sel ent txt 
					code72 code73 tg loop basepos txtlst rflag)
	(setq old_error *error*)
	(setq *error* ifs_error)
        (princ "\nTCopyEd :文字列をコピーしながら編集、または数字部分を＋１します．")
	(setq iti 1)
	(prompt "\nコピー後の処理を選択: ")
	(initget 0 "Edit Inc")
	(setq kw2 (getkword "\nE=編集する /I=＋１する <Enter=＋１する>: "))
	(if (= kw2 nil)(setq kw2 "Inc"))
	(if (setq sel (entsel "\n基準文字を選択: "))
		(progn
			(setq ent (entget (car sel)))
			(if (= (cdr (assoc 0 ent)) "TEXT")
				(progn
					(setq txt (cdr (assoc 1 ent)) code72 (cdr (assoc 72 ent)) code73 (cdr (assoc 73 ent)))
					(if (/= kw2 "Edit")
						(progn
							(setq txtlst (ifs_txt_bunri txt) txtcnt (length txtlst))
							(if (> (get_suuji_cnt txtlst) 1)
								(progn
									(setq kw_res (get_kw_001))
									(setq iti (car kw_res) rflag (cadr kw_res))
								)
							)
							(setq lstans (get_txtlst_itians txtlst iti rflag))
							(setq ansiti (car lstans) anstxt (cadr lstans))
							(setq keta (strlen anstxt))
							(if rflag (setq ansiti (1+ (- txtcnt ansiti))))
							(setq maeato (get_maeato_txt txtlst ansiti))
							(setq maetxt (car maeato) atotxt (cadr maeato))
						)
					)
					;;基点を得る
					(if (and (= code72 0) (= code73 0))
						(setq basepos (cdr (assoc 10 ent)))
						(setq basepos (cdr (assoc 11 ent)))
					)
					(setq tg (car sel))
					(setvar "CMDECHO" 0)
					(setq loop T i 1)
					(while loop
						(prompt "\n複写先を指示 <ESC=終了> : ")
						(command "COPY" "single" tg basepos pause)
						(if(setq loop (not (equal (getvar "LASTPOINT") basepos)))
							(progn
								;;DDEDITで編集する
								(if (= kw2 "Edit")
									(progn
										(prompt "\n文字列編集: ")
										(command "DDEDIT" "last" "")
									)
								)
								;;次のループのために最後の文字列を得る
								(setq tg (entlast) ent (entget tg))
								(setq txt (cdr (assoc 1 ent)) code72 (cdr (assoc 72 ent)) code73 (cdr (assoc 73 ent)))
								(if (and (= code72 0) (= code73 0))
									(setq basepos (cdr (assoc 10 ent)))
									(setq basepos (cdr (assoc 11 ent)))
								)
								;;数字部分を＋１する
								(if (/= kw2 "Edit")
									(progn
										(setq restxt (itoa (+ (atoi anstxt) i)))
										;;最初の桁数に満たない時は文字列の前に０を付ける。
										(if (> keta (strlen restxt))(setq restxt (ifs_add_zero restxt keta)))
										;;新しく文字を作成する
										(setq txt (strcat maetxt restxt atotxt))
										(setq ent (subst (cons 1 txt)(assoc 1 ent) ent))
										(entmod ent)
										(setq i (1+ i)) 
									)
								)
							)
							;;コマンドをEnterで終了させるための逃げ
							;(progn
								;;Enterで終了したときは、最後を削除
							;	;;念のために文字列で比較
							;	(setq ent (entget (entlast)))
							;	(setvar "CMDECHO" 1)
							;	;(if (= txt (cdr (assoc 1 ent))) 
							;	;	(command "ERASE" "single" "last")
							;	;)
							;)
						)
					)	
					(setvar "CMDECHO" 1)
				
				)
				(princ "\n文字列(TEXT)ではありません．")
			)
		)
		(princ "\n何も選択されませんでした．")
	)
	(setq *error* old_error)
	(princ)
)
;;文字列リストの指定位置より前後の文字列を得る
(defun get_maeato_txt(txtlst iti / i maetxt atotxt item)
	(setq maetxt "" atotxt "" i 1)
	(foreach item txtlst
		(if (< i iti)(setq maetxt (strcat maetxt item)))
		(if (> i iti)(setq atotxt (strcat atotxt item)))
		(setq i (1+ i))
	)
	(list maetxt atotxt)
)

;;数字群の位置番号から文字列リスト全体の位置とその文字列を得る
(defun get_txtlst_itians( txtlst iti rflag / cnt i item ansiti anstxt)
	(if rflag (setq txtlst (reverse txtlst)))
	;;文字列リスト内の位置を取得
	(setq cnt 0 i 0)
	(foreach item txtlst
		(if (ifs_is_suuji item)
			(progn
				(setq cnt (1+ cnt))
				(if (= cnt iti)(setq ansiti (1+ i) anstxt item))
			)
		)
		(setq i (1+ i))
	)
	(list ansiti anstxt)
)
;;数字群の番号とフラグを返す
(defun get_kw_001( / kw rflag)
	(setq kw (getint "\n加算する数字群の位置を入力（Enter=1,マイナス=後ろからの位置）: "))
	(if kw (if (< kw 0) (setq kw (abs kw) rflag T)) (setq kw 1))
	(list kw rflag)
)

;;数字の前に０を付加する
(defun ifs_add_zero( str keta / res)
	(setq res "")
	(repeat (- keta (strlen str))(setq res (strcat res "0")))
	(strcat res str)
)
;;文字列リストのn番目をstrに入れ替える
(defun ifs_txtlst_conv(txtlst iti str / i res)
	(setq i 0 iti (1- iti))
	(repeat (length txtlst)
		(if (/= i iti)
			(setq res (append res (list (nth i txtlst))))
			(setq res (append res (list str)))
		)
		(setq i (1+ i))
	)
	res;
)
;;文字列を数字とそれ以外に分離しLISTにする
(defun ifs_txt_bunri(str / len st ed i flag1 flag2 lst)
	(setq i 1 st 1)
	(setq len (strlen str))
	(repeat (1- len)
		(setq flag1 (ifs_is_suuji (substr str i 1)))
		(setq flag2 (ifs_is_suuji (substr str (1+ i) 1)))
		(if (or (and flag1 (not flag2 ))(and (not flag1) flag2))
			(setq ed (+ i 1)
				  lst (append lst (list (substr str st (- ed st))))
				  st ed
			)
		)
		(setq i (1+ i))
	)
	(append lst (list (substr str st (+ (- len st)1))))
)
;;数字（1234567890）か？
(defun ifs_is_suuji( str / asc res)
	(setq asc (ascii (substr str 1 1)))
	(and (>= asc 48)(<= asc 57))
)
;;文字列リスト内の数字群の個数を得る
(defun get_suuji_cnt (txtlist / item cnt)
	(setq cnt 0)
	(foreach item txtlist
		(if (ifs_is_suuji item)(setq cnt (1+ cnt)))
	)
	cnt;
)
;***************************************************
; Icadでの残像対策（暫定）
;***************************************************
(defun icad_esc()
  (setvar "CMDECHO" 0)
  (command "LINE" "0,0" "0,0" "")
  (command "ERASE" "SINGLE" "LAST")
  (setvar "CMDECHO" 1)
  (redraw)
)

;(princ "\nAddNum2 :数字混じりの文字の数字部分を加減算します．")
;(princ "\nAddNCopy:文字列の数字部分を＋１しながら指定回数コピーします．")
;(princ "\nTCopyEd :文字列をコピーしながら編集、または数字部分を＋１します．")
(princ)