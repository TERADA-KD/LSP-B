;;高圧機器　単線図の入力                  HV1K.LSP  2004/JAN/4  By T.Terada
;;
(defun C:HV1K (/ GYO RETU NAM PC SYM LG MN Rasc DCL_ID VCBA VCBKA VCBT DAT SEL1 SEL
                   PNA PNo T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 MK QT Z1 Z2)
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)
      (setq DCL_ID (load_dialog "/LSP-I/KD.DCL"))
      (if (not (new_dialog "HV1K" DCL_ID) )(EXIT))
       
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "1" "(setq NAM \"VCB\")")
      (action_tile "2" "(setq NAM \"LBS\")")
      (action_tile "3" "(setq NAM \"PF\")")
      (action_tile "4" "(setq NAM \"DS\")")
      (action_tile "5" "(setq NAM \"VCS\")")
      (action_tile "6" "(setq NAM \"VSPF\")")
      (action_tile "7" "(setq NAM \"LA\")")
      (action_tile "8" "(setq NAM \"CT\")")
      (action_tile "9" "(setq NAM \"VT\")")
      (action_tile "10" "(setq NAM \"ZCTH\")")
      (action_tile "11" "(setq NAM \"DRW\")")
    (start_dialog)
    (unload_dialog DCL_ID)(princ)

;;初期設定
    (C:MKR)               ;; 基本メーカーを調べる
    (setvar "USERS5" "A") ;; C:SQ3Wを自動モードで使う
    (setq LG "N")         ;; LGR無しの設定（Ｃのダイアログには設定無し）
;;メーカー番号の決定
    (if (= MAKER "0")(setq MN "1")) (if (= MAKER "1")(setq MN "4"))
    (if (= MAKER "2")(setq MN "1")) (if (= MAKER "3")(setq MN "4"))

;;EXCELの読込み開始"列"を決定
;     (setq RETU "E")
    (if (or (= MAKER "0")(= MAKER "2"))(progn (setq RETU "E") (setq Rasc (ascii "E")))) ;三菱の場合
    (if (or (= MAKER "1")(= MAKER "3"))(progn (setq RETU "AE")(setq Rasc (ascii "AE"))));富士の場合

;;VCB
     (if (= NAM "VCB")
       (progn
         (setq DCL_ID (load_dialog "X:/LSP-I/KD.DCL"))
         (if (not (new_dialog "VCB1K" DCL_ID) )(EXIT))
          
         (action_tile "cancel" "(done_dialog)(exit)")
        ;定格電流 
         (action_tile "1" "(setq VCBA \"400A\")")
         (action_tile "2" "(setq VCBA \"600A\")")
         (action_tile "3" "(setq VCBA \"1200A\")")
         (action_tile "4" "(setq VCBA \"2000A\")")
         (action_tile "5" "(setq VCBA \"3000A\")")
        ;遮断容量 
         (action_tile "11" "(setq VCBKA \"8KA\")")
         (action_tile "12" "(setq VCBKA \"12.5KA\")")
         (action_tile "13" "(setq VCBKA \"20KA\")")
         (action_tile "14" "(setq VCBKA \"32KA\")")
         (action_tile "15" "(setq VCBKA \"40KA\")")
        ;タイプ 
         (action_tile "21" "(setq VCBT \"手動\")")
         (action_tile "22" "(setq VCBT \"固定\")")
         (action_tile "23" "(setq VCBT \"引出\")")
         (start_dialog)
;ダイアログチェック忘れ時のエラー処理
         (if (= VCBA nil) (progn (ALERT "定格電流が選択されていません")(unload_dialog DCL_ID)(exit)))
         (if (= VCBKA nil)(progn (ALERT "遮断容量が選択されていません")(unload_dialog DCL_ID)(exit)))
         (if (= VCBT nil) (progn (ALERT "タイプが選択されていません")  (unload_dialog DCL_ID)(exit)))
         (if (and (= VCBA "400A")(or (= VCBKA "12.5KA")(= VCBKA "20KA")(= VCBKA "32KA")(= VCBKA "40KA")))
             (progn (ALERT "400Aには、8KAしか選択できません。")  (unload_dialog DCL_ID)(exit))
         )
         (unload_dialog DCL_ID)(princ)
       )
     )
       (if (and (= VCBT "手動")(= VCBKA "8KA")   (= VCBA "400A"))  (setq GYO "181"))
       (if (and (= VCBT "固定")(= VCBKA "8KA")   (= VCBA "400A"))  (setq GYO "182"))
       (if (and (= VCBT "手動")(= VCBKA "12.5KA")(= VCBA "600A"))  (setq GYO "183"))
       (if (and (= VCBT "固定")(= VCBKA "12.5KA")(= VCBA "600A"))  (setq GYO "184"))
       (if (and (= VCBT "引出")(= VCBKA "12.5KA")(= VCBA "600A"))  (setq GYO "185"))
       (if (and (= VCBT "固定")(= VCBKA "20KA")  (= VCBA "600A"))  (setq GYO "186"))
       (if (and (= VCBT "引出")(= VCBKA "20KA")  (= VCBA "600A"))  (setq GYO "187"))
       (if (and (= VCBT "固定")(= VCBKA "20KA")  (= VCBA "1200A")) (setq GYO "188"))
       (if (and (= VCBT "引出")(= VCBKA "20KA")  (= VCBA "1200A")) (setq GYO "189"))
       (if (and (= VCBT "固定")(= VCBKA "25KA")  (= VCBA "600A"))  (setq GYO "190"))
       (if (and (= VCBT "引出")(= VCBKA "25KA")  (= VCBA "600A"))  (setq GYO "191"))
       (if (and (= VCBT "固定")(= VCBKA "25KA")  (= VCBA "1200A")) (setq GYO "192"))
       (if (and (= VCBT "引出")(= VCBKA "25KA")  (= VCBA "1200A")) (setq GYO "193"))
       (if (and (= VCBT "固定")(= VCBKA "32KA")  (= VCBA "1200A")) (setq GYO "194"))
       (if (and (= VCBT "引出")(= VCBKA "32KA")  (= VCBA "1200A")) (setq GYO "195"))
       (if (and (= VCBT "固定")(= VCBKA "32KA")  (= VCBA "2000A")) (setq GYO "196"))
       (if (and (= VCBT "引出")(= VCBKA "32KA")  (= VCBA "2200A")) (setq GYO "197"))
       (if (and (= VCBT "固定")(= VCBKA "32KA")  (= VCBA "3000A")) (setq GYO "198"))
       (if (and (= VCBT "引出")(= VCBKA "32KA")  (= VCBA "3000A")) (setq GYO "199"))
       (if (and (= VCBT "固定")(= VCBKA "40KA")  (= VCBA "1200A")) (setq GYO "200"))
       (if (and (= VCBT "引出")(= VCBKA "40KA")  (= VCBA "1200A")) (setq GYO "201"))
       (if (and (= VCBT "固定")(= VCBKA "40KA")  (= VCBA "2000A")) (setq GYO "202"))
       (if (and (= VCBT "引出")(= VCBKA "40KA")  (= VCBA "2000A")) (setq GYO "203"))
       (if (and (= VCBT "固定")(= VCBKA "40KA")  (= VCBA "3000A")) (setq GYO "204"))
       (if (and (= VCBT "引出")(= VCBKA "40KA")  (= VCBA "3000A")) (setq GYO "205"))
       (princ "\n列=")(princ RETU)
       (princ "\n行=")(princ GYO)
       (xls_open "X:/XLS/高圧機器一覧表" "r")
          (repeat 15 
              (setq SEL1 (strcat RETU GYO))
              (setq SEL (xls_getval SEL1));;;(princ "\nCEL=")(princ SEL1)
              (setq DAT (append DAT (list SEL)))
              (if (= (strlen RETU) 2)
                  (progn
                      (setq RETU (substr RETU 2 1))
                      (setq RETU (strcat "A" (chr (+ (ascii RETU) 1))))
                  )
              )
              (if (= (strlen RETU) 1)(setq RETU (chr (+ (ascii RETU) 1))))
          )
       (xls_close)
           (setq PNA (nth 0 DAT))(setq SYM (nth 1 DAT))(setq T1 (nth 2 DAT)) (setq T2 (nth 3 DAT))
           (setq T3 (nth 4 DAT)) (setq T4 (nth 5 DAT)) (setq T5 (nth 6 DAT)) (setq T6 (nth 7 DAT))
           (setq T7 (nth 8 DAT)) (setq T8 (nth 9 DAT)) (setq T9 (nth 10 DAT))(setq MK (nth 11 DAT))
           (setq QT (nth 12 DAT))(setq Z1 (nth 13 DAT))(setq Z2 (nth 14 DAT))
        (setq PC (getpoint "\n挿入点を指示... "))
        (setq PNo (getstring "\nVCB番号を指示:(例：-1)"))
                              (cond ((= PNo "") (setq PNo "-1")))
       (setq PNO (strcat PNA PNO))
       
       (command "INSERT" SYM PC "1" "1" "0" PNA PNO T1 T2 T3 T4 T5 T6 T7 T8 T9 MK QT Z1 Z2)


;;======================================================================================================
   (command "REGEN");再作図をしないと画面上に変更が反映されない
   (redraw)
   (setvar "CMDECHO" 1)
   (setvar "USERI5" 0)
   (setvar "USERS5" "A");;C:SQ3Wを手動モードに戻す
   (prompt "\n作図完了！！")
)

;;==================================================================================================
;;VCB　単線図の入力         HV1K ==> S1VCB.LSP  2004/JAN/4  By T.Terada
;;==================================================================================================
(defun C:S1VCB (/ GYO RETU NAM PC SYM LG MN Rasc DCL_ID VCBA VCBKA VCBT DAT SEL1 SEL
                   PNA PNo T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 MK QT Z1 Z2)
;      (setq *error* *err1*)
;      (setvar "CMDECHO" 0)
;;初期設定
    (prompt "X:\XLS\高圧機器一覧表のデーターを参照しています。")
    (C:MKR)               ;; 基本メーカーを調べる
    (setvar "USERS5" "A") ;; C:SQ3Wを自動モードで使う
    (setq LG "N")         ;; LGR無しの設定（Ｃのダイアログには設定無し）
;;メーカー番号の決定
    (if (= MAKER "0")(setq MN "1")) (if (= MAKER "1")(setq MN "4"))
    (if (= MAKER "2")(setq MN "1")) (if (= MAKER "3")(setq MN "4"))

;;EXCELの読込み開始"列"を決定
;     (setq RETU "E")
    (if (or (= MAKER "0")(= MAKER "2"))(progn (setq RETU "E") (setq Rasc (ascii "E")))) ;三菱の場合
    (if (or (= MAKER "1")(= MAKER "3"))(progn (setq RETU "AE")(setq Rasc (ascii "AE"))));富士の場合

;;VCB
         (setq DCL_ID (load_dialog "X:/LSP-I/KD.DCL"))
         (if (not (new_dialog "VCB1K" DCL_ID) )(EXIT))
          
         (action_tile "cancel" "(done_dialog)(exit)")
        ;定格電流 
         (action_tile "1" "(setq VCBA \"400A\")")
         (action_tile "2" "(setq VCBA \"600A\")")
         (action_tile "3" "(setq VCBA \"1200A\")")
         (action_tile "4" "(setq VCBA \"2000A\")")
         (action_tile "5" "(setq VCBA \"3000A\")")
        ;遮断容量 
         (action_tile "11" "(setq VCBKA \"8KA\")")
         (action_tile "12" "(setq VCBKA \"12.5KA\")")
         (action_tile "13" "(setq VCBKA \"20KA\")")
         (action_tile "14" "(setq VCBKA \"32KA\")")
         (action_tile "15" "(setq VCBKA \"40KA\")")
        ;タイプ 
         (action_tile "21" "(setq VCBT \"手動\")")
         (action_tile "22" "(setq VCBT \"固定\")")
         (action_tile "23" "(setq VCBT \"引出\")")
         (start_dialog)
;ダイアログチェック忘れ時のエラー処理
         (if (= VCBA nil) (progn (ALERT "定格電流が選択されていません")(unload_dialog DCL_ID)(exit)))
         (if (= VCBKA nil)(progn (ALERT "遮断容量が選択されていません")(unload_dialog DCL_ID)(exit)))
         (if (= VCBT nil) (progn (ALERT "タイプが選択されていません")  (unload_dialog DCL_ID)(exit)))
         (if (and (= VCBA "400A")(or (= VCBKA "12.5KA")(= VCBKA "20KA")(= VCBKA "32KA")(= VCBKA "40KA")))
             (progn (ALERT "400Aには、8KAしか選択できません。")  (unload_dialog DCL_ID)(exit))
         )
         (unload_dialog DCL_ID)(princ)

       (if (and (= VCBT "手動")(= VCBKA "8KA")   (= VCBA "400A"))  (setq GYO "181"))
       (if (and (= VCBT "固定")(= VCBKA "8KA")   (= VCBA "400A"))  (setq GYO "182"))
       (if (and (= VCBT "手動")(= VCBKA "12.5KA")(= VCBA "600A"))  (setq GYO "183"))
       (if (and (= VCBT "固定")(= VCBKA "12.5KA")(= VCBA "600A"))  (setq GYO "184"))
       (if (and (= VCBT "引出")(= VCBKA "12.5KA")(= VCBA "600A"))  (setq GYO "185"))
       (if (and (= VCBT "固定")(= VCBKA "20KA")  (= VCBA "600A"))  (setq GYO "186"))
       (if (and (= VCBT "引出")(= VCBKA "20KA")  (= VCBA "600A"))  (setq GYO "187"))
       (if (and (= VCBT "固定")(= VCBKA "20KA")  (= VCBA "1200A")) (setq GYO "188"))
       (if (and (= VCBT "引出")(= VCBKA "20KA")  (= VCBA "1200A")) (setq GYO "189"))
       (if (and (= VCBT "固定")(= VCBKA "25KA")  (= VCBA "600A"))  (setq GYO "190"))
       (if (and (= VCBT "引出")(= VCBKA "25KA")  (= VCBA "600A"))  (setq GYO "191"))
       (if (and (= VCBT "固定")(= VCBKA "25KA")  (= VCBA "1200A")) (setq GYO "192"))
       (if (and (= VCBT "引出")(= VCBKA "25KA")  (= VCBA "1200A")) (setq GYO "193"))
       (if (and (= VCBT "固定")(= VCBKA "32KA")  (= VCBA "1200A")) (setq GYO "194"))
       (if (and (= VCBT "引出")(= VCBKA "32KA")  (= VCBA "1200A")) (setq GYO "195"))
       (if (and (= VCBT "固定")(= VCBKA "32KA")  (= VCBA "2000A")) (setq GYO "196"))
       (if (and (= VCBT "引出")(= VCBKA "32KA")  (= VCBA "2200A")) (setq GYO "197"))
       (if (and (= VCBT "固定")(= VCBKA "32KA")  (= VCBA "3000A")) (setq GYO "198"))
       (if (and (= VCBT "引出")(= VCBKA "32KA")  (= VCBA "3000A")) (setq GYO "199"))
       (if (and (= VCBT "固定")(= VCBKA "40KA")  (= VCBA "1200A")) (setq GYO "200"))
       (if (and (= VCBT "引出")(= VCBKA "40KA")  (= VCBA "1200A")) (setq GYO "201"))
       (if (and (= VCBT "固定")(= VCBKA "40KA")  (= VCBA "2000A")) (setq GYO "202"))
       (if (and (= VCBT "引出")(= VCBKA "40KA")  (= VCBA "2000A")) (setq GYO "203"))
       (if (and (= VCBT "固定")(= VCBKA "40KA")  (= VCBA "3000A")) (setq GYO "204"))
       (if (and (= VCBT "引出")(= VCBKA "40KA")  (= VCBA "3000A")) (setq GYO "205"))
       (princ "\n列=")(princ RETU)
       (princ "\n行=")(princ GYO)
       (xls_open "X:/XLS/高圧機器一覧表" "r")
          (repeat 15 
              (setq SEL1 (strcat RETU GYO))
              (setq SEL (xls_getval SEL1));;;(princ "\nCEL=")(princ SEL1)
              (setq DAT (append DAT (list SEL)))
              (if (= (strlen RETU) 2)
                  (progn
                      (setq RETU (substr RETU 2 1))
                      (setq RETU (strcat "A" (chr (+ (ascii RETU) 1))))
                  )
              )
              (if (= (strlen RETU) 1)(setq RETU (chr (+ (ascii RETU) 1))))
          )
       (xls_close)
           (setq PNA (nth 0 DAT))(setq SYM (nth 1 DAT))(setq T1 (nth 2 DAT)) (setq T2 (nth 3 DAT))
           (setq T3 (nth 4 DAT)) (setq T4 (nth 5 DAT)) (setq T5 (nth 6 DAT)) (setq T6 (nth 7 DAT))
           (setq T7 (nth 8 DAT)) (setq T8 (nth 9 DAT)) (setq T9 (nth 10 DAT))(setq MK (nth 11 DAT))
           (setq QT (nth 12 DAT))(setq Z1 (nth 13 DAT))(setq Z2 (nth 14 DAT))
        (setq PC (getpoint "\n挿入点を指示... "))
        (setq PNo (getstring "\nVCB番号を指示:(例：-1)"))
                              (cond ((= PNo "") (setq PNo "-1")))
       (setq PNO (strcat PNA PNO))
       
       (command "INSERT" SYM PC "1" "1" "0" PNA PNO T1 T2 T3 T4 T5 T6 T7 T8 T9 MK QT Z1 Z2)


;;======================================================================================================
   (command "REGEN");再作図をしないと画面上に変更が反映されない
   (redraw)
   (setvar "CMDECHO" 1)
   (setvar "USERI5" 0)
   (setvar "USERS5" "A");;C:SQ3Wを手動モードに戻す
   (prompt "\n作図完了！！")
)
