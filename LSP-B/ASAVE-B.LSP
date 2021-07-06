;; ASAVE-B.LSP       1998/Dec/11変更     2021/MAR/30改       By T.Terada
;; 開いている図面の部品表・銘板・ＣＨのデーターを書き出します。
;; DRV-ME  = C: <========== ICAD.LSPで定義されてます。
;; DRV-LAN = X: <========== ICAD.LSPで定義されてます。
(vl-load-com)
;;====================    銘板用ﾌｧｲﾙ出力    ========================================================
(defun NP( / LEN01 NAME02 FP01 EN01 ENT01 COM01 DAT01)
         (setvar "cmdecho" 1)
             (setq W1 nil) (setq EN1 nil) (setq IND1 0)
             (princ "銘板用ﾌｧｲﾙを出力します")
             (princ "\n銘板一覧に渡したい文字の")
             (setq W1 (ssget '((1 . "TEXT"))))(princ "\nW1=")(princ W1)
;             (setq W1 (ssget))
             (setq ENAM (ssname W1 0))   (princ "ENAM=")(princ ENAM)(princ "\n")
             (setq EDAT (entget ENAM))(princ "EDAT=")(princ EDAT)(princ "\n");選択した図形の内容を見る
             
             (cond (/= W01 nil)
                   (setq LEN01 (sslength W01))(princ "\nLEN01=")(princ LEN01)
;                   (setq NAME01 (vla-get-Name doc)) 
                   (setq NAME02 (strcat "X:/NP/" (substr NAME01 1 8) ".NP1"))(princ "\nNP-NAME02=")(princ NAME02)(princ "\n")
;                   (vl-file-delete NAME02)
                   (setq FP01 (open NAME02 "a"))
                      (princ "\n1:")(princ NAME01)
                      (princ "\n2:")(princ NAME02)
                         (repeat LEN01
                            (setq EN01 (ssname W01 IND01))
                            (cond (/= EN01 nil)
                                  (setq ENT01 (entget EN01))(princ "\nENT01=")(princ ENT01)
                                  (setq COM01 (cdr (assoc 0 ENT01)))
                               (cond 
                                   (= COM01 "TEXT")(setq DAT01 (cdr (assoc 1 ENT01)))(princ "\nDAT01=")(princ DAT01)
                                   (write-line DAT01 FP01)
                               )
                            )
                            (setq EN01 nil) (setq IND01 (+ IND01 1))
                         ); repeat
                      (close FP01)
                    (princ "\n  N P 書出完了...")(princ "\n")
             );Cond
)
;;=======================    ＣＨ用ﾌｧｲﾙ出力    =====================================================
(defun CH( / LEN01 NAME02 FP01 EN01 ENT01 COM01 DAT01)
             (setq W01 nil) (setq en01 nil) (setq ind01 0)
             (princ "CH用ﾌｧｲﾙを出力します")
             (princ "\n出力したいＣＨ名称の")
             (setq W01 (ssget))
             (cond (/= W01 nil)
                   (setq LEN01 (sslength W01))
                   (setq NAME01 (vla-get-Name doc)) (princ "\nNAME01=")(princ NAME01)(princ "\n")
                   (setq NAME02 (strcat "X:/CH/" (substr NAME01 1 8) ".CH1"))
                   (vl-file-delete NAME02)
                   (setq FP01 (open NAME02 "a"))
                      (princ "\n1:")(princ NAME01)
                      (princ "\n2:")(princ NAME02)
                        (repeat LEN01 (princ "\nLEN01=")(princ LEN01)
                           (setq EN01 (ssname W01 ind01))
                           (cond (/= EN01 nil)
                                 (setq ENT01 (entget EN01))
                                 (setq COM01  (cdr (assoc 0 ENT01)))
                              (cond (= COM01 "TEXT")
                                    (setq DAT01 (cdr (assoc 1 ENT01)))(write-line DAT01 FP01)
                              )
                           )
                           (setq EN01 nil)(setq IND01 (+ IND01 1))
                      ); repeat
                    (close FP01)
                  (princ "\n  C H 書出完了...")(princ "\n")
             );Cond
)
;;==================================================================================================
(defun C:ASAVE(/ n Rn xdoc LN NEN)
	(setvar "cmdecho" 0)
	(setq acad (vlax-get-acad-object))

	;; アクティブな図面名の取得
	(setq curdoc (vla-get-ActiveDocument acad))
	(princ "\n現在の図面")(princ (vla-get-Name curdoc))(princ "\n")

	;; すべての図面名の取得
	(setq docs (vla-get-Documents acad))
	;; 開いているの数
	(setq n (vla-get-Count docs))

	;; 図面名を順に取得
	(setq Rn n)
    (repeat n 
        ;; 現在アクティブな図面の名前
         (setq Rn (- Rn 1));(princ "\n Rn = ")(princ Rn)(princ "\n")
         (setq xdoc (vla-get-ActiveDocument (vlax-get-acad-Object)))
         (vl-propagate 'xdoc)
         (vla-put-ActiveDocument (vlax-get-acad-Object) xdoc)
         (vla-put-ActiveDocument (vlax-get-acad-Object)(vla-Item docs Rn))
         (setq doc (vla-Item docs Rn))
            (setq NAME01 (vla-get-Name doc)) (princ "\nNAME01=")(princ NAME01)(princ "\n")
            (setq NAME02 (substr NAME01 2 1))
            (setq NAME03 (substr NAME01 6 1))
	        (if (and (= NAME02 "-")(= NAME03 "-"));Drawing等は対象外（2と6文字目が－ﾊｲﾌﾝか調べる）
 	           (progn
	             ;;=================    部品表属性ﾌｧｲﾙ出力    ==================================
	             ;;    << X:\\LSP-I\\ATT-OUT.TXTは属性書出しの定義ファイルです。>>
    	         ;;       (DOSで使用するので \\ は / に置換えない事！)
        	     (setq LN (strlen NAME01))
            	 (setq NEN (substr NAME01 1 1))
	             (setq NAME01 (substr NAME01 3 (- LN 6)))
    	         (setq NAME02 (strcat "Z:\\BUHIN-I\\" NEN "\\" NAME01 ".TXT"))
        	     (setq NAME03 (strcat "Z:\\BUHIN-I\\" NEN "\\" NAME01 ".IPL"))
	             ;;部品表ファイルの保管      ********.IPL IPL=Icad Parts List
    	         (command "ATTEXT" "c" "X:\\LSP-i\\ATT-OUT.TXT" NAME02)(princ "ATT-OUTPUT")
        	     (vl-file-delete NAME03)
            	 (vl-file-copy NAME02 NAME03)                                   ; DOS操作
  	             (vl-file-delete NAME02)
                 (princ "\n 部品表 書出完了...")(princ "\n")
                (NP);NPを出力
;               (CH);CHを出力
              );progn
           );if
    );rep
;; 0番目の図面名をアクティブに
;    (vla-put-ActiveDocument (vlax-get-acad-Object) (vla-Item docs 0))
    (setvar "cmdecho" 1)
    (princ)
)
