;;回路図面から機器外形情報をファイルに書き出す。 D_OUT.LSP   2003/JUL   By T.Terada
;;* END-I.LSPの中でも使用

(defun C:D_OUT(/ DAT AA BB CC DD n Z1D Z2D F NAME01 NAME04 LN NEN)
  (setq *error* *err1*)
  (setq DAT nil)(setq n 0)
  (setvar "CMDECHO" 0)
   
;;-----------------------------------------------------------------------------
;;選択したオブジェクトをDATに収納
   (setq AA (ssget "x" (list '(66 . 1))))      ;属性の付いた図形だけ。"INSERT"ではダメ。
   (while (setq BB (ssname AA n))
       (setq CC (entnext BB)  DD (entget CC))
       (setq Z1D nil  Z2D nil)
              (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
                 (if (and (= (cdr (assoc 0 DD)) "ATTRIB")(= (cdr (assoc 2 DD)) "Z1"))
                     (if (and (/= (cdr (assoc 2 DD)) "")(/= (cdr (assoc 1 DD)) ""))
                         (setq Z1D (cdr (assoc 1 DD)))
                     )
                 )
                 (if (and (= (cdr (assoc 0 DD)) "ATTRIB")(= (cdr (assoc 2 DD)) "PNO"))
                     (if (and (/= (cdr (assoc 2 DD)) "")(/= (cdr (assoc 1 DD)) ""))
                         (setq Z2D (cdr (assoc 1 DD)))
                     )
                 )
                 (setq CC (entnext CC) DD (entget CC))
              );while
              (if (or (= Z1D nil)(= Z2D nil))
                  (princ)
                  (setq DAT (append DAT (list (list Z1D Z2D))))
              )
      (setq n (1+ n))
   );while
;;-----------------------------------------------------------------------------
;;保存するファイル名の取得と設定 
   (setq Name1 (getvar "dwgname"))
   (setq Name11 (substr Name1 1 7))
   (if (= Name11 "Drawing")
       (progn                              ;図番が "Drawing"の時は X:\DDat\ に出力。
          (setq Name2 (substr Name1 1 8))
          (setq Name4 (strcat "X:/DDAT/" Name2 ".DAT"))  
          (setq  F (open Name4 "w"))
          (print DAT F)
          (close F)
          (princ "\nX:\DDat\ に Drawingx.DATで出力しました！")
       )
   )
   (if (/= Name11 "Drawing") ;else
       (progn
          (setq LN (strlen Name1))
          (setq NEN (substr Name1 1 1))
          (setq Name2 (substr Name1 3 (- LN 6)))
          (setq Name4 (strcat "X:/DDAT/" NEN "/" Name2 ".DAT"))  ;  外形情報ファイル
          (setq  F (open Name4 "w"))
          (print DAT F)
          (close F)
       )
   )
   (command "REDRAW")
   (setvar  "CMDECHO" 1)
   (prompt  "\n  外形情報、書出完了...")
)
