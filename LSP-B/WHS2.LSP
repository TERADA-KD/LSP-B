;;===================================================================================================
;;ＷＨＭの回路入力                                            WHS2.LSP  2004/NOV/17  By T.Terada
;;  参照ファイル：X:\XLSX\SWH-SEL.xlsx
;;  単線図にも対応 （CT回路付きを除く）2005/NOV/1
;;===================================================================================================
  
  (defun C:WHS (/ DPU DPU_BAK DTY DTY_BAK DCL_ID WHAT_NEXT GYO RETU MN KEN SOU TA PU TY Rasc CTYN DAT
                  PC SYM SEL1 SEL PNA PNO T1 T2 T3 T4 T5 T6 T8 T9 QT Z1 Z2 DAT MK STY STY_BAK PFX
                  DST DST_BAK FILE CELL)
;      (setq *error* *err1*)
      (setvar "CMDECHO" 0)
;;ﾒｰｶｰの設定
;      (C:MKR) ;基本メーカーを調べる
      (princ "\n※ /5AのWHMでエラーがでますが正常に動作します！！")
      (if (= MAKER "0")(progn (setq MK "M")(setq MN "1"))) ;三菱
      (if (= MAKER "1")(progn (setq MK "F")(setq MN "4"))) ;富士
      (if (or (= MAKER "2")(= MAKER "3"))(progn (setq MK "T")(setq MN "7"))) ;寺崎
      (DCL_WH)
     ;ダイアログチェック忘れ時のエラー処理
      (if (= KEN nil)(progn (ALERT "検定の有無が選択されていません")(DCL_WH)))
      (if (= SOU nil)(progn (ALERT "相・線が選択されていません")    (DCL_WH)))
      (if (= TA nil) (progn (ALERT "定格電流が選択されていません")  (DCL_WH)))
      (if (and (or (= TA "30A")(= TA "120A"))(= DTY "62"))
          (progn (ALERT "普通型ダイレクトには埋込型はありません")(DCL_WH)))
      (unload_dialog DCL_ID)
      (cond ((= WHAT_NEXT 0)(setq DPU DPU_BAK)(princ))((= WHAT_NEXT 1) DPU))
      (cond ((= WHAT_NEXT 0)(setq DTY DTY_BAK)(princ))((= WHAT_NEXT 1) DTY))
      (cond ((= WHAT_NEXT 0)(setq DST DST_BAK)(princ))((= WHAT_NEXT 1) DST))
      (cond ((= DPU "1") (setq PU "N"))
            ((= DPU "51")(setq PU "N"))
            ((= DPU "52")(setq PU "Y"))
      )
      (cond ((= DTY "1") (setq TY "N"))
            ((= DTY "61")(setq TY "N"))
            ((= DTY "62")(setq TY "U"))
            ((= DTY "63")(setq TY "E"))
      )
      (cond ((= DST "1") (setq ST "3"))
            ((= DST "S") (setq ST "3"))
            ((= DST "T") (setq ST "1"))
      );    (princ "\nST=")(princ ST)  (princ "\nDST=")(princ DST)

;;EXCELの読込み開始"列"を決定
    (if (= MN "1") (progn (setq RETU "E") (setq Rasc (ascii "E")))) ;三菱の場合
    (if (= MN "4") (progn (setq RETU "AE")(setq Rasc (ascii "AE"))));富士の場合
    (if (= MN "45")(progn (setq RETU "BE")(setq Rasc (ascii "BE"))));大崎の場合

;;機種別の読込み開始"行"を決定
   ;1φ2W ﾊﾟﾙｽの有無 ダイレクト
    (if (and (= TY "N")(= SOU "1P2W")(= TA "30A") (= PU "N"))(setq GYO "11"));普通
    (if (and (= TY "N")(= SOU "1P2W")(= TA "30A") (= PU "Y"))(setq GYO "12"));普通・ﾊﾟﾙｽ
    (if (and (= TY "N")(= SOU "1P2W")(= TA "120A")(= PU "N"))(setq GYO "21"));普通
    (if (and (= TY "N")(= SOU "1P2W")(= TA "120A")(= PU "Y"))(setq GYO "22"));普通・ﾊﾟﾙｽ
    (if (and (= TY "E")(= SOU "1P2W")(= TA "30A"))           (setq GYO "13"));電子式
    (if (and (= TY "E")(= SOU "1P2W")(= TA "120A"))          (setq GYO "23"));電子式
   ;1φ3W ﾊﾟﾙｽの有無 ダイレクト
    (if (and (= TY "N")(= SOU "1P3W")(= TA "30A") (= PU "N"))(setq GYO "31"));普通
    (if (and (= TY "N")(= SOU "1P3W")(= TA "30A") (= PU "Y"))(setq GYO "32"));普通・ﾊﾟﾙｽ
    (if (and (= TY "N")(= SOU "1P3W")(= TA "120A")(= PU "N"))(setq GYO "41"));普通
    (if (and (= TY "N")(= SOU "1P3W")(= TA "120A")(= PU "Y"))(setq GYO "42"));普通・ﾊﾟﾙｽ
    (if (and (= TY "E")(= SOU "1P3W")(= TA "30A"))           (setq GYO "33"));電子式
    (if (and (= TY "E")(= SOU "1P3W")(= TA "120A"))          (setq GYO "43"));電子式
   ;1φ3W ﾊﾟﾙｽの有無 ／５Ａ
    (if (and (= TY "N")(= SOU "1P3W")(= TA "R5A") (= PU "N"))(setq GYO "51"));普通
    (if (and (= TY "N")(= SOU "1P3W")(= TA "R5A") (= PU "Y"))(setq GYO "52"));普通・ﾊﾟﾙｽ
    (if (and (= TY "E")(= SOU "1P3W")(= TA "R5A"))           (setq GYO "53"));電子式
    (if (and (= TY "U")(= SOU "1P3W")(= TA "R5A") (= PU "N"))(setq GYO "54"));埋込
    (if (and (= TY "U")(= SOU "1P3W")(= TA "R5A") (= PU "Y"))(setq GYO "55"));埋込・ﾊﾟﾙｽ
   ;3φ3W ﾊﾟﾙｽの有無 ダイレクト
    (if (and (= TY "N")(= SOU "3P3W")(= TA "30A") (= PU "N"))(setq GYO "61"));普通
    (if (and (= TY "N")(= SOU "3P3W")(= TA "30A") (= PU "Y"))(setq GYO "62"));普通・ﾊﾟﾙｽ
    (if (and (= TY "N")(= SOU "3P3W")(= TA "120A")(= PU "N"))(setq GYO "71"));普通
    (if (and (= TY "N")(= SOU "3P3W")(= TA "120A")(= PU "Y"))(setq GYO "72"));普通・ﾊﾟﾙｽ
    (if (and (= TY "E")(= SOU "3P3W")(= TA "30A"))           (setq GYO "63"));電子式
    (if (and (= TY "E")(= SOU "3P3W")(= TA "120A"))          (setq GYO "73"));電子式
   ;1φ3W ﾊﾟﾙｽの有無 ／５Ａ
    (if (and (= TY "N")(= SOU "3P3W")(= TA "R5A") (= PU "N"))(setq GYO "81"));普通
    (if (and (= TY "N")(= SOU "3P3W")(= TA "R5A") (= PU "Y"))(setq GYO "82"));普通・ﾊﾟﾙｽ
    (if (and (= TY "E")(= SOU "3P3W")(= TA "R5A"))           (setq GYO "83"));電子式
    (if (and (= TY "U")(= SOU "3P3W")(= TA "R5A") (= PU "N"))(setq GYO "84"));埋込
    (if (and (= TY "U")(= SOU "3P3W")(= TA "R5A") (= PU "Y"))(setq GYO "85"));埋込・ﾊﾟﾙｽ
;   (princ "\n列=")(princ RETU) (princ "\n行=")(princ GYO)
    (if (= (strlen RETU) 2)(setq PFX  (substr RETU 1 1)));(princ "\nPFX=")(princ PFX)
;--------------------------------------------------------------------------------------------------
:EXCELの読込み
    (vl-load-com)
    (setq FILE "X:/XLSX/SWH-SEL.xlsx")
   	(setq excelApplication (vlax-get-or-create-object "Excel.Application"))
    (vlax-put-property excelApplication 'Visible :vlax-false);excelを表示しない

;既存のブックを開きます。
	(setq workbooks (vlax-get-property excelApplication 'Workbooks));アクティブなブックを取得
	(setq workbook (vlax-invoke-method workbooks 'Open FILE))
	(setq workbook (vlax-get-property excelApplication 'ActiveWorkbook))
	(vlax-invoke-method workbook 'Activate)                         ;ブックをアクティブにする
	(setq window (vlax-get-property (vlax-get-property workbook 'Windows) 'Item 1))
;(princ FILE)
;ワークシートの読込み。
	(setq sheets (vlax-get-property workbook 'Worksheets))
	(vlax-invoke-method (vlax-get-property sheets 'Item "Sheet1") 'Activate)
	(setq sheet (vlax-get-object excelApplication 'ActiveSheet))  ；アクティブなシートを得る
;;;	(setq sheet (vlax-get-property excelApplication 'ActiveSheet))  ；アクティブなシートを得る
	(princ "\nsheet = ")(princ sheet)

;--------------------------------------------------------------------------------------------------
          (repeat 15 
              (setq CELL (strcat RETU GYO))
              ;セルの読込み。
			  (setq rangeObj (vlax-get-property sheet 'Range CELL))
			  (setq value (vlax-get-property rangeObj 'Value2))
			  (if (= value nil)(setq value ""));(princ "\nvalue =")(princ value)
              (setq DAT (append DAT (list value)))
              (if (= (strlen RETU) 2);;          
                  (progn 
                      (setq RETU (substr RETU 2 1))
                      (setq RETU (strcat PFX (chr (+ (ascii RETU) 1))))
                  )
              )
              (if (= (strlen RETU) 1)(setq RETU (chr (+ (ascii RETU) 1))))
          )
          (princ "\nDAT =")(princ DAT)
;EXCEL終了処理	
	(vlax-invoke-method workbook 'Close);workbook終了
	(vlax-invoke-method excelApplication 'Quit);EXCEL終了
	(vlax-release-object excelApplication);VLAの開放
	(setq excelApplication nil)
;	(princ "\nEXCEL-Finish")(princ)
;---------------------------------------------------------------------------------------------------
       (if (= KEN "Y")(setq T7 "検付")(setq T7 "未検"));検定付きの場合
       (setq PNA (nth 0 DAT)) (setq SYM (nth 1 DAT)) (setq T1 (nth 2 DAT)) (setq T2 (nth 3 DAT))
       (setq T3  (nth 4 DAT)) (setq T4  (nth 5 DAT)) (setq T5 (nth 6 DAT)) (setq T6 (nth 7 DAT))
;;       (setq T7 (nth 8 DAT))
       (setq T8  (nth 9 DAT)) (setq T9  (nth 10 DAT))(setq MN (nth 11 DAT))
       (setq QT  (nth 12 DAT))(setq Z1  (nth 13 DAT))(setq Z2 (nth 14 DAT))

       ;;091218 ＣＴ付きにするとＤＡＴデータを使うのでＴ７のデータを含めて再セットしておく
       ;;下記の方で(setq DAT (subst T7 (nth 8 DAT) DAT))を使う方法もあるが、空白全てが検付になってしまう
       (setq DAT (list PNA SYM T1 T2 T3 T4 T5 T6 T7 T8 T9 MN QT Z1 Z2))
;;======================================================================================================
;;/5Aの場合
     (if (= TA "R5A")
         (progn
            (setq STY_BAK STY)(if (not STY)(setq STY "1"))
            (setq DCL_ID (load_dialog "X:/DCL/WHS.DCL"))
            (setq WHAT_NEXT 30)
            (while (>= WHAT_NEXT 2)
               (if (not (new_dialog "WHT5" DCL_ID) )(exit))  
               (action_tile "cancel" "(done_dialog)(exit)")
               (action_tile "T5A"  "(setq T2 \"5/5A\")")
               (action_tile "T10A" "(setq T2 \"10/5A/\")")
               (action_tile "T15A" "(setq T2 \"15/5A\")")
               (action_tile "T20A" "(setq T2 \"20/5A\")")
               (action_tile "T30A" "(setq T2 \"30/5A\")")
               (action_tile "T40A" "(setq T2 \"40/5A\")")
               (action_tile "T50A" "(setq T2 \"50/5A\")")
               (action_tile "T60A" "(setq T2 \"60/5A\")")
               (action_tile "T75A" "(setq T2 \"75/5A\")")
               (action_tile "T80A" "(setq T2 \"80/5A\")")
               (action_tile "T100A" "(setq T2 \"100/5A\")")
               (action_tile "T120A" "(setq T2 \"120/5A\")")
               (action_tile "T150A" "(setq T2 \"150/5A\")")
               (action_tile "T200A" "(setq T2 \"200/5A\")")
               (action_tile "T250A" "(setq T2 \"250/5A\")")
               (action_tile "T300A" "(setq T2 \"300/5A\")")
               (action_tile "T400A" "(setq T2 \"400/5A\")")
               (action_tile "T500A" "(setq T2 \"500/5A\")")
               (action_tile "T600A" "(setq T2 \"600/5A\")")
               (action_tile "T750A" "(setq T2 \"750/5A\")")
               (action_tile "T800A" "(setq T2 \"800/5A\")")
               (action_tile "T1000A" "(setq T2 \"1000/5A\")")
               (action_tile "T1200A" "(setq T2 \"1200/5A\")")
               (action_tile "T1500A" "(setq T2 \"1500/5A\")")
               (action_tile "T2000A" "(setq T2 \"2000/5A\")")
               (action_tile "T2500A" "(setq T2 \"2500/5A\")")
               (action_tile "T3000A" "(setq T2 \"3000/5A\")")
               (action_tile "T4000A" "(setq T2 \"4000/5A\")")
               (action_tile "T5000A" "(setq T2 \"5000/5A\")")
            (act_radioset '("CTY" "CTN") STY)
               (action_tile "CTY" "(setq STY $key)")
               (action_tile "CTN" "(setq STY $key)")
               (setq WHAT_NEXT (start_dialog))
            );while
            (if (= T2 nil) (progn (ALERT "\nＣＴの比率が選択されていません。")  (unload_dialog DCL_ID)(exit)))
            (unload_dialog DCL_ID)
            (cond ((= WHAT_NEXT 0)(setq STY STY_BAK)(princ))((= WHAT_NEXT 1) STY))
            (cond ((= STY "1")  (setq CTYN "Y"))
                  ((= STY "CTY")(setq CTYN "Y"))
                  ((= STY "CTN")(setq CTYN "N"))
            )
            (princ)
           ;挿入図形の選択
            (if (= CTYN "Y")(R5CT))
            (if (= CTYN "N")(R5WH))
         )
         (DWH);ダイレクトの図形挿入へ(ELSE)
    )
;;------------------------------------------------------------------------------------------------------
;;終了処理
   (command "REGEN")(setvar "CMDECHO" 1)(setvar "USERI5" 0)
; )
;;======================================================================================================
;;ダイレクトの図形挿入サブルーチン
  (defun DWH (/)(setvar "CMDECHO" 1)
        (setq PNo (getstring "\nWHM番号を指示:(例：-1)"))
                  (cond ((= PNo "") (setq PNo "-1")))
       (setq PNO (strcat PNA PNO))
       (if (= ST "1")(setq SYM "QWHM"));; <=== 単線図の場合（CT回路付きを除く）
       (princ "\nX:/XLS/SWH-SEL.xlsxを参照しています。")(princ "\n挿入点を指示")
       (command "INSERT" SYM pause "1" "1" "0" PNA PNO T1 T2 T3 T4 T5 T6 T7 T8 T9 MN QT Z1 Z2)
  )
;;======================================================================================================
;;／５Ａの図形挿入（ＣＴ付き）サブルーチン
  (defun R5CT ( / PC PC1 PC2 PNo PNO2 AA BB CC DD ENT n N2 ZOKU)(setvar "CMDECHO" 1)
       (setq PNo (getstring "\nWHM番号を指示:(例：-1)"))
                       (cond ((= PNo "") (setq PNo "-1")))
       (setq PNO2 (strcat "CT" PNO));CT
       (setq PNO  (strcat PNA  PNO));WH
       (princ "\nX:/XLS/SWH-SEL.xlsxを参照しています。")(princ "\n挿入点を指示... /5CT：")
       (command "INSERT" Z2 pause "1" "0")
       (setq PC (getvar "LASTPOINT"));(princ "\nPC = ")(princ PC)
       (setq DAT (subst PNO (nth 1 DAT) DAT));PNOの設定
       (setq DAT (subst T2  (nth 3 DAT) DAT));/5Aの設定
       (setq DAT (subst "" (nth 14 DAT) DAT));Z2 の設定
;       (princ "\nDAT=")(princ DAT)
  ;;属性の編集 WHM
       (setq n 0)
       (setq PC1 (list (+ (car PC) 700)(+ (cadr PC) 275)));(princ "\nPC1 = ")(princ PC1)
       (setq AA (ssget PC1 (list '(66 . 1))));(princ "\nAA = ")(princ AA)
       (setq BB (ssname AA n));(princ "\nBB = ")(princ BB)
       (setq ENT (entget BB)); (princ "\nENT = ")(princ ENT)
;       (princ "\nSYM = ")(princ (cdr (assoc 2 ENT)))
        ;;属性の書込対象となるシンボル
        (if (or (= (cdr (assoc 2 ENT)) "MWH5")(= (cdr (assoc 2 ENT)) "MWH5P"))
            (progn
               (setq N2 1);(princ "\nN2 =")(princ N2)
               (setq CC (entnext BB));(princ "\nCC =")(princ CC)
               (setq DD (entget CC)) ;(princ "\nDD =")(princ DD)
               (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
                  (if (= (cdr (assoc 0 DD)) "ATTRIB")
                      (progn
;                         (princ "\nATT =")(princ (cdr (assoc 0 DD)))
                         (if (= N2 1) (setq ZOKU "PNA"))(if (= N2 2) (setq ZOKU "PNO"))
                         (if (= N2 3) (setq ZOKU "T1")) (if (= N2 4) (setq ZOKU "T2"))
                         (if (= N2 5) (setq ZOKU "T3")) (if (= N2 6) (setq ZOKU "T4"))
                         (if (= N2 7) (setq ZOKU "T5")) (if (= N2 8) (setq ZOKU "T6"))
                         (if (= N2 9) (setq ZOKU "T7")) (if (= N2 10)(setq ZOKU "T8"))
                         (if (= N2 11)(setq ZOKU "T9")) (if (= N2 12)(setq ZOKU "MN"))
                         (if (= N2 13)(setq ZOKU "QT")) (if (= N2 14)(setq ZOKU "Z1"))
                         (if (= N2 15)(setq ZOKU "Z2"));(princ "\nZOKU =")(princ ZOKU)
                         (if (= (cdr (assoc 2 DD)) ZOKU)
                             (entmod (subst (cons 1 (nth (1- N2) DAT))(assoc 1 DD) DD));属性を変更
                         );if
                      );progn
                  );if
                  (setq N2 (1+ N2))
                  (setq CC (entnext CC) DD (entget CC))
               );while
            );progn
        );if
(princ "\nSSGET-END")
;;シンボルの属性を読み込んで書き換える   ATT-RW.LSP
        (setq n 0 DAT nil AA nil BB nil CC nil DD nil ENT nil)
        (setq PNA "CT" T1 "" QT "2")
        (setq PC2 (list (- (car PC) 225)(- (cadr PC) 5)))
        (setq AA (ssget PC2 (list '(66 . 1))))
        (while (setq BB (ssname AA n))
           (setq ENT (entget BB))
           ;;属性の書込対象となるシンボル
             (if (= (cdr (assoc 2 ENT)) "MCTWHM")
                (progn
                   (setq N2 1)
                   (setq CC (entnext BB)  DD (entget CC))
                   (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
                      (if (= (cdr (assoc 0 DD)) "ATTRIB")
                          (progn
                             (if (= (cdr (assoc 2 DD)) "PNA")(entmod (subst (cons 1 PNA) (assoc 1 DD) DD)))
                             (if (= (cdr (assoc 2 DD)) "PNO")(entmod (subst (cons 1 PNO2)(assoc 1 DD) DD)))
                             (if (= (cdr (assoc 2 DD)) "T1" )(entmod (subst (cons 1 T1)  (assoc 1 DD) DD)))
                             (if (= (cdr (assoc 2 DD)) "T3" )(entmod (subst (cons 1 T2)  (assoc 1 DD) DD)))
                             (if (= (cdr (assoc 2 DD)) "QT" )(entmod (subst (cons 1 QT)  (assoc 1 DD) DD)))
                          )
                      )
                      (setq N2 (1+ N2))
                      (setq CC (entnext CC) DD (entget CC))
                      (if *error* (EXIT))
                   )
                );progn
            );if
            (setq n (1+ n)) 
       );while
     ;;CTを選択してALLで属性編集する
       (command "REGEN")
       (setq PC3 (list (- (car PC) 200)(cadr PC)))
       (setq PC4 (list (- (car PC) 225)(cadr PC)))
       (setq ALL_AA (ssget "W" PC3 PC4))
       (C:ALL-ATT)
  );defun
;;======================================================================================================
;;／５Ａの図形挿入（ＷＨＭのみ）
  (defun R5WH (/ PC PNo)(setvar "CMDECHO" 0)
       (setq PNo (getstring "\nWHM番号を指示:(例：-1)"))
           (cond ((= PNo "") (setq PNo "-1")))
       (setq PNO (strcat PNA PNO))
       (if (= ST "1")(setq SYM "QWHM"));; <=== 単線図の場合（CT回路付きを除く）
           (princ "\nX:/XLSX/SWH-SEL.xlsxを参照しています。")
           (princ "\nエラーがでますが正常に動作します！！")
           (princ "\n挿入点を指示")
       (command "INSERT" SYM pause "1" "1" "0" PNA PNO T1 T2 T3 T4 T5 T6 T7 T8 T9 MN QT Z1 "")
  )
;;======================================================================================================
;;DCL_WH
   (defun DCL_WH ()
      (setq DPU_BAK DPU)(setq DTY_BAK DTY)(setq DST_BAK DST)
      (if (not DPU)(setq DPU "1"))(if (not DTY)(setq DTY "1"))(if (not DST)(setq DST "1"))
      (setq DCL_ID (load_dialog "X:/DCL/WHS.DCL"))
      (setq WHAT_NEXT 15)
      (while (>= WHAT_NEXT 2)
         (if (null (new_dialog "WHS" DCL_ID) )(EXIT))
         (if (= MK "M")(set_tile "1" "1"))
         (if (= MK "F")(set_tile "2" "1"))
         (action_tile "cancel" "(done_dialog)(exit)")
        ;３線図 or 単線図
         (act_radioset '("S" "T") DST)
         (action_tile "S" "(setq DST $key)")
         (action_tile "T" "(setq DST $key)")
        ;メーカー番号
         (action_tile "1" "(setq MN \"1\")")
         (action_tile "2" "(setq MN \"4\")")
         (action_tile "3" "(setq MN \"45\")")
        ;検定の有無
         (action_tile "21" "(setq KEN \"N\")")
         (action_tile "22" "(setq KEN \"Y\")")
        ;相・線
         (action_tile "31" "(setq SOU \"1P3W\")")
         (action_tile "32" "(setq SOU \"3P3W\")")
         (action_tile "33" "(setq SOU \"1P2W\")")
        ;定格電流
         (action_tile "41" "(setq TA \"30A\")")
         (action_tile "42" "(setq TA \"120A\")")
         (action_tile "43" "(setq TA \"R5A\")")
        ;パルスの有無
         (act_radioset '("51" "52") DPU)
         (action_tile "51" "(setq DPU $key)")
         (action_tile "52" "(setq DPU $key)")
        ;タイプ
         (act_radioset '("61" "62" "63") DTY)
         (action_tile "61" "(setq DTY $key)")
         (action_tile "62" "(setq DTY $key)")
         (action_tile "63" "(setq DTY $key)")
         (setq WHAT_NEXT (start_dialog))
      );while
   )
  )
