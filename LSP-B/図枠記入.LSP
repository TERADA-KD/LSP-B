;;図枠入力                       図枠入力.LSP           2003/Apr/5   By T.Terada
(defun C:ZIN()
   (setq *error* *err1*)
   (setvar "CMDECHO" 0)

   (setq CNAM "")(setq DNAM "")(setq PNAM "")
   
   (setq SUGT "杉本(忠)")
   (setq SATO "里平")
   (setq OOTA "大田")
   (setq HIRO "廣田")
   (setq OOGU "大口")
   (setq MATU "松田")
   (setq YOKO "横路")
   (setq MIAK "三明")
   (setq TERA "寺田")
   (setq SUEM "末政")
   (setq NISI "西岡")
   (setq YUKA "油川")

   ;;基準図枠サイズの取得
   (C:SZ)

   ;;件名・盤名・図番テキストファイルの読み込み
   (setq DWG (GETVAR "DWGNAME"))
   (setq DWG (substr DWG 1 (- (strlen DWG) 4)))
   ;;(princ "\nDWG=")(princ DWG)
   (setq EDA01 (substr DWG 7 2))
   (setq FN01 (strcat "Z:" "\\LIST-TXT\\" (substr DWG 1 5) ".TXT"))
   ;;(princ "\nFN01=")(princ FN01)

   (setq F01 (open  FN01 "r"))
   (if (= *error* nil)
      (progn
         (setvar "CMDECHO" 0) 
         (alert "図面番号が正しくありません。図枠記入は中止します。")
         (princ "\n図番に誤りがあります")
         (setenv "ST" "OLD")
         (exit)
      )
      (progn
         (if (/= F01 nil)
            (progn
               (setq L01 (read-line F01))
               (setq EDA02 (substr L01 61 2)):;１行目の枝番
               (setq KEN  (substr L01 1 30)) ;;物件名
               (setq ZDWG (substr DWG 1 16)) ;;図番

               ;;(princ "\nEDA01=")(princ EDA01)

               (if (/= EDA01 "SP")
                  (progn
                     ;;該当する盤記号がない時はデータがなくなっても読み込もうとするのでエラーおこす
                     (while (and (/= L01 nil) (/= EDA01 EDA02))
                        (setq L01 (read-line F01))
                        (setq EDA02 (substr L01 61 2))
                     )
                     ;;盤名称
                     (setq BAN (substr L01 31 30))
                  )
                  (progn
                     ;;既存の盤名称を読み取る
                     ;;(setq BAN " ");;初期化
                     (setq SZW (strcat SZ "W"))
        
                     (setq n 0)
                     (setq AA (ssget "X" (list '(66 . 1))))
                     (while (setq BB (ssname AA n))
                        (setq ENT (entget BB))
                        (if (= (cdr (assoc 2 ENT)) SZW);;属性の書込対象となるシンボル
                           (progn
                              (setq N2 1)
                              (setq CC (entnext BB) DD (entget CC))
                              (while (/= (cdr (assoc 0 DD)) "SEQEND");;SEQENDまで繰り返し
                                 (if (= (cdr (assoc 0 DD)) "ATTRIB")
                                    (progn
                                       (if (= (cdr (assoc 2 DD)) "ZW2")
                                          (setq BAN (cdr (assoc 1 DD)))
                                       )
                                    )
                                 )
                                 (setq N2 (1+ N2))
                                 (setq CC (entnext CC) DD (entget CC))
                              )
                           );progn
                        );if
                        (setq n (1+ n)) 
                     );while
                  );progn
               )
            )
            (progn
               (setvar "CMDECHO" 0)
               (alert "図面番号が正しくありません。図枠記入は中止します。2")
               (princ "\n図番に誤りがあります")
               (setenv "ST" "OLD")
               (exit)
            )
         );if
      )
   )
   (close F01)
      
   (if (= (getvar "USERI5") 0)
      (progn
         ;;DCLファイルの読み込み
         (startapp "VBeep" "")
         (setq dcl_id (load_dialog "KD.DCL"))
         (if (not (new_dialog "ZIN" dcl_id) )(exit))
         ;;(mode_tile "accept" 1)  ;;OKﾎﾞﾀﾝのﾌｫｰｶｽをOFF
         ;;検図・設計の選択用ダイアログボックス
         (action_tile "cancel" "(done_dialog)(exit)")

         ;;検図者
         ;;(action_tile "11" "(setq CNAM SUGJ)")
         (action_tile "12" "(setq CNAM SUGT)")
         (action_tile "13" "(setq CNAM TERA)")
         (action_tile "14" "(setq CNAM SUEM)")
         (action_tile "15" "(setq CNAM NISI)")
         (action_tile "16" "(setq CNAM YUKA)")

         ;;設計者
         ;;(action_tile "21" "(setq PNAM SUGJ)")
         (action_tile "22" "(setq PNAM SUGT)")
         (action_tile "23" "(setq PNAM SATO)")
         (action_tile "24" "(setq PNAM OOTA)")
         (action_tile "25" "(setq PNAM HIRO)")
         (action_tile "26" "(setq PNAM OOGU)")
         (action_tile "27" "(setq PNAM MATU)")
         (action_tile "28" "(setq PNAM YOKO)")
         (action_tile "33" "(setq PNAM MIAK)")
         (action_tile "29" "(setq PNAM TERA)")
         (action_tile "30" "(setq PNAM SUEM)")
         (action_tile "31" "(setq PNAM NISI)")
	 (action_tile "32" "(setq PNAM YUKA)")

         ;;製図者
         ;;(action_tile "41" "(setq DNAM SUGJ)")
         (action_tile "42" "(setq DNAM SUGT)")
         (action_tile "43" "(setq DNAM SATO)")
         (action_tile "44" "(setq DNAM OOTA)")
         (action_tile "45" "(setq DNAM HIRO)")
         (action_tile "46" "(setq DNAM OOGU)")
         (action_tile "47" "(setq DNAM MATU)")
         (action_tile "48" "(setq DNAM YOKO)")
         (action_tile "53" "(setq DNAM MIAK)")
         (action_tile "49" "(setq DNAM TERA)")
         (action_tile "50" "(setq DNAM SUEM)")
         (action_tile "51" "(setq DNAM NISI)")
	 (action_tile "52" "(setq DNAM YUKA)")

  ;尺度の選択用ダイアログボックスに1：5を追加　20130205　大田。

         ;;尺度の選択用ダイアログボックス
         (setq SCAL "")
         (action_tile "81" "(setq SCAL \"１：１０\")")
         (action_tile "82" "(setq SCAL \"NoScale\")")
         (action_tile "83" "(setq SCAL \"１：５\")")

         ;;連番の取得
         (setq REN1 "")(setq REN2 "")
         (mode_tile "REN1" 3)(mode_tile "REN2" 3)
         (action_tile "REN1" "(setq REN1 (get_tile \"REN1\"))");;何枚の中の
         (action_tile "REN2" "(setq REN2 (get_tile \"REN2\"))");;何枚目
         (start_dialog)
         (unload_dialog dcl_id)
         (princ)
      )
   )

   (if (= (getvar "USERI5") 1)
      (progn
         (princ "\nSNAME1：")(princ SNAME1)
         (princ "\nSNAME2：")(princ SNAME2)
         (princ "\nSYAKU：") (princ SYAKU)
         (princ "\nSNAME3：")(princ SNAME3)

         (setq PNAM SNAME1);;設計者
         (setq DNAM SNAME2);;製図者
         (setq SCAL SYAKU) ;;尺度
         (setq CNAM SNAME3);;検図者
         (setq REN1 "")    ;;連番
         (setq REN2 "")    ;;連番
      )
   )

   ;;日付の設定
   (setq DATE01 (rtos (getvar "CDATE") 2 0))
   (setq DATE01 (strcat "'" (substr DATE01 3 2) "/" (substr DATE01 5 2) "/" (substr DATE01 7 2)))

   ;;(princ "\nSZW：")(princ SZW)
   ;;(princ "\nP1：")(princ P1)
   ;;(princ "\nKEN：")(princ KEN)
   ;;(princ "\nBAN：")(princ BAN)
   ;;(princ "\nZDWG：")(princ ZDWG)
   ;;(princ "\nCNAM：")(princ CNAM)
   ;;(princ "\nPNAM：")(princ PNAM)
   ;;(princ "\nDNAM：")(princ DNAM)
   ;;(princ "\nSCAL：")(princ SCAL)
   ;;(princ "\nSZ：")(princ SZ)
   ;;(princ "\nDATE01：")(princ DATE01)
   ;;(princ "\nREN2：")(princ REN2)
   ;;(princ "\nREN1：")(princ REN1)
   ;;(princ "\n")

   ;;ブロック AnW.DWGの有無：有りで一旦AnWを削除する
   (setq SZW (strcat SZ "W"))
   (setq i 0)
   (setq SS (ssget "X"))
   (setq SN (sslength SS))
   (while (< i SN)
      (setq ENAME (ssname SS i))
      (setq EDATA (entget ENAME))
         ;;(princ "EDATA=")(princ EDATA)(princ "\n")
         (if (= (cdr(assoc 2 EDATA)) SZW)
            (progn
               (princ "\n既存の図枠覧を消去します。")(princ "\n")
               (entdel ENAME)
            )
         )
      (setq i(+ i 1))
   )

   (if (= BAN "")(setq BAN " "))
   (startapp "VBeep" "") (setq P1 (list 0 0))
   (setq SZW (strcat "X:\\ZUWAKU\\" SZW)) ;;09/06/09 杉本(純)(忠)に対応させるとA1図面時だけ枠内をはみだすので、A1Wで文字の高さを50→45に変更した
   (command "INSERT" SZW P1 "1" "1" "0" KEN BAN ZDWG CNAM PNAM DNAM SCAL SZ DATE01 REN2 REN1 "")

   ;;キャンセルボタンの処理
   (action_tile "cancel" "(done_dialog 0)(exit)")

   (setvar "USERI5" 0)
   (setvar "CMDECHO" 1)
);end
;(prompt "\nZIN.....ok")
;
;=====================================================================================================
;;物件名書換え     VBのICADファイル名変更.exeで使用
;=====================================================================================================
(defun C:KENUP ( / DWG EDA01 FN01 F01 L01 EDA02 EDA03 KEN BAN ZDWG SZW FP1 FP2 FP3 FP4 AA n BB ENT N2 CC DD)

   (setq *error* *err1*)
   (setvar "CMDECHO" 0)

   ;;基準図枠サイズの取得
   (C:SZ)

   ;;件名・盤名・図番テキストファイルの読み込み
   (setq DWG (GETVAR "DWGNAME"))
   (setq DWG (substr DWG 1 (- (strlen DWG) 4)))
   ;;(princ "\nDWG=")(princ DWG)
   (setq EDA01 (substr DWG 7 2))
   (setq FN01 (strcat "Z:" "\\LIST-TXT\\" (substr DWG 1 5) ".TXT"))
   ;;(princ "\nFN01=")(princ FN01)
   (setq F01 (open  FN01 "r"))
   (if (= *error* nil)
      (progn
          
         (alert "図面番号が正しくありません。図枠記入は中止します。")
         (princ "\n図番に誤りがあります")
         (setenv "ST" "OLD")
         (exit)
      )
      (progn
         (if (/= F01 nil)
            (progn
               (setq L01 (read-line F01))
               (setq EDA02 (substr L01 61 2))
                  
               (if (/= EDA01 "SP")
                  (progn
                     ;;該当する盤記号がない時はデータがなくなっても読み込もうとするのでエラーおこす
                     (while (and (/= L01 nil) (/= EDA01 EDA02))
                        (setq L01 (read-line F01))
                        (setq EDA02 (substr L01 61 2))
                     )
                  )
               )
                  
               ;;盤名称
               (if (/= EDA01 "SP")(setq BAN (substr L01 31 30)))
                  
               (if (= EDA01 "SP")
                  (progn
                     (setq EDA03 (substr DWG 7 5))
                          
                     (setq BAN " ") ;;初期値
                     (if (= EDA03 "SP1A1")(setq BAN "制御盤・分電盤・端子盤等　共通仕様書"))
                     (if (= EDA03 "SP1A2")(setq BAN "使用機器一覧表"))
                     (if (= EDA03 "SP1A3")(setq BAN "製作盤一覧表"))
                     (if (= EDA03 "SP1A4")(setq BAN "使用機器・製作盤一覧表"))
                     (if (= EDA03 "SP2A1")(setq BAN "制御盤・分電盤・端子盤等　国土交通省仕様書"))
                     (if (= EDA03 "SP4A1")(setq BAN "共通仕様書"))
                     (if (= EDA03 "SP4A4")(setq BAN "使用機器・製作盤一覧表"))
                     (if (= EDA03 "SP5A1")(setq BAN "国土交通省仕様書"))
                     ;;(princ "\nBAN=")(princ BAN)
                  )
               )
                  
               (setq KEN  (substr L01 1 30)) ;物件名
               (setq ZDWG (substr DWG 1 16)) ;図番
            )
            (progn
                
               (alert "図面番号が正しくありません。図枠記入は中止します。2")
               (princ "\n図番に誤りがあります")
               (setenv "ST" "OLD")
               (exit)
            )
         );if
      )
   )
   (close F01)

   ;;シンボルの属性を読み込んで書き換える  ATT-RW.LSP
   (setq SZW (strcat SZ "W"))
      (setq FP1 (list 0 8400))    ;;フェンス左上
      (setq FP2 (list 0 0))       ;;フェンス左下
      (setq FP3 (list 11880 0))   ;;フェンス右下
      (setq FP4 (list 11880 8400));;フェンス右上
   
   (setq AA (ssget "WP" (list FP1 FP2 FP3 FP4) (list '(66 . 1))))
   (if (/= AA nil)
      (progn
         (setq n 0)
         (setq AA (ssget "WP" (list FP1 FP2 FP3 FP4) (list '(66 . 1))))
         (while (setq BB (ssname AA n))
            (setq ENT (entget BB))
            (if (= (cdr (assoc 2 ENT)) SZW);;属性の書込対象となるシンボル
               (progn
                  (setq N2 1)
                  (setq CC (entnext BB)  DD (entget CC))
                  (while (/= (cdr (assoc 0 DD)) "SEQEND");;SEQENDまで繰り返し
                     (if (= (cdr (assoc 0 DD)) "ATTRIB")
                        (progn
                           (if (= (cdr (assoc 2 DD)) "ZW1")(entmod (subst (cons 1 KEN)(assoc 1 DD) DD)))
                           (if (= (cdr (assoc 2 DD)) "ZW2")(entmod (subst (cons 1 BAN)(assoc 1 DD) DD)))
                           (if (= (cdr (assoc 2 DD)) "ZW3")(entmod (subst (cons 1 ZDWG)(assoc 1 DD) DD)))
                        )
                     )
                     (setq N2 (1+ N2))
                     (setq CC (entnext CC) DD (entget CC))
                  )
               );progn
            );if
            (setq n (1+ n)) 
         );while
      );progn
   )

   (command "regen")
   (command "QSAVE")
   (setvar "CMDECHO" 1)
);end
