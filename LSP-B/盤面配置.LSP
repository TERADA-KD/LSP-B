;;**************************************************************************************************
;;盤面配置ツールバー用       盤面配置.LSP        2004/May   By T.Terada
;;**************************************************************************************************
;;1.ＮＰ付きＰＬ
;;2.ＰＬ
;;3.ＣＳ
;;4.ＰＢ
;;5.ＳＳ
;;6.ＡＳ・ＶＳ
;;7.ＣＯＳ
;;8.ＳＬＣ  １窓
;;9.ＳＬＣ  ２窓
;;10.ＳＬＣ  ３窓
;;11.ブザー−１
;;12.ブザー−２
;;13.ガラス管ヒューズ
;;14.ＣＴ
;;15.メーター外形入力
;;16.ＥＬＢアース端子の外形入力 

;;==================================================================================================
;;1.ＮＰ付きＰＬ
;;==================================================================================================
  (defun C:DPLNP (/ PC PC1)
     (C:ST1)(prompt "\n挿入点を指示...")
     (command "INSERT" "DPL30F004" pause "1" "1" "0" "PL" "" "" "" "" "")
     (setq PC (getvar "LASTPOINT"))
     (setq PC1 (list (car PC)(+ (cadr PC) 50)))
     (command "INSERT" "*NP1-1" PC1 "1" "0")
     (C:END1)
  )
;;==================================================================================================
;;2.ＰＬ
;;==================================================================================================
  (defun C:DPL (/ PC)
     (C:ST1)(prompt "\n挿入点を指示...")
     (command "INSERT" "DPL30F004" pause  "1" "1" "0" "PL" "" "" "" "" "")(C:END1)
  )
;;==================================================================================================
;;3.ＣＳ
;;==================================================================================================
  (defun C:DCS (/ PC)
     (C:ST1)(prompt "\n挿入点を指示...")
     (command "INSERT" "DCS30F004" pause  "1" "1" "0" "CS" "" "" "" "" "")(C:END1)
  )
;;==================================================================================================
;;4.ＰＢ
;;==================================================================================================
  (defun C:DPB (/ PC)
     (C:ST1)(prompt "\n挿入点を指示...")
     (command "INSERT" "DPB30F004" pause  "1" "1" "0" "PB" "" "" "" "" "")(C:END1)
  )
;;==================================================================================================
;;5.ＳＳ
;;==================================================================================================
  (defun C:DSS (/)
     (C:ST1)(prompt "\n挿入点を指示...")
     (command "INSERT" "DSS" pause  "1" "1" "0" "" "" "" "" "" "")(C:END1)
  )
;;==================================================================================================
;;6.ＡＳ・ＶＳ
;;==================================================================================================
  (defun C:DAVS (/)
     (C:ST1)(prompt "\n挿入点を指示...")
     ;(command "INSERT" "DAVS2F014" pause  "1" "1" "0" "AVS")(C:END1)
     (command "INSERT" "DAVS2F014" pause  "1" "1" "0" "AVS" "" "" "" "" "")(C:END1)
  )
;;==================================================================================================
;;7.ＣＯＳ
;;==================================================================================================
  (defun C:DCOS (/)
     (C:ST1)(prompt "\n挿入点を指示...")
     (command "INSERT" "DCOSF014" pause  "1" "1" "0" "COS" "" "" "" "" "")(C:END1)
  )
;;======================================================================================================
;;8.ＳＬＣ  １窓
;;==================================================================================================
  (defun C:DAP1 (/ PC PNA PNO T1 T2 T3 T4 T7 T8 SLN COL VV)
     (setq *error* *err1*)
     (setvar "CMDECHO" 0)
     (setq SLN 1)(DCL_DSLC1)
     (setq PNA "PLC" PNO "PLC-")
     (setq T1 (strcat "AP30F-0101-" VV "-S-" COL))
      (prompt "\n挿入点を指示... ")
     (command "INSERT" "DAP301F004" pause "1" "1" "0" PNA PNO T1 T2 T3 T4 "" "" T7 T8 "" "2" "1" "" "")
     (setq PC (getvar "LASTPOINT"))
     (command "COLOR" 2)(command "TEXT" "S" "M0" "M" PC "20" "0" COL)(command "COLOR" 4)
     (redraw)(setvar "CMDECHO" 1)
     )
;;==================================================================================================
;;9.ＳＬＣ  ２窓
;;==================================================================================================
  (defun C:DAP2 (/ PC PNA PNO T1 T2 T3 T4 T7 T8 SLN COL VV PCL PCR)
     (setq *error* *err1*)(setvar "CMDECHO" 1)
     (setq SLN 2)(DCL_DSLC1)
     (setq PNA "PLC" PNO "PLC-" T2 "赤緑")
     (setq T1 (strcat "AP30F-0102-" VV "-S-RG"))
      (prompt "\n挿入点を指示... ")
     (command "INSERT" "DAP302F004" pause "1" "1" "0" PNA PNO T1 T2 T3 T4 "" "" T7 T8 "" "2" "1" "" "")
     ;(command "INSERT" "DAP302F004" pause "1" "1" "0" PNA PNO T1 T2 T3 T4)
     (setq PC (getvar "LASTPOINT"))
     (setq PCL (list (- (car PC) 15)(cadr PC)))
     (setq PCR (list (+ (car PC) 15)(cadr PC)))
     (command "COLOR" 2)(command "TEXT" "S" "M0" "M" PCL "20" "0" "R")
                        (command "TEXT" "S" "M0" "M" PCR "20" "0" "G")(command "COLOR" 4)
     (redraw)(setvar "CMDECHO" 1)
  )
;;==================================================================================================
;;10.ＳＬＣ  ３窓
;;==================================================================================================
  (defun C:DAP3 (/ PC PNA PNO T1 T2 T3 T4 T7 T8 COL VV PCL PCR)
     (setq *error* *err1*)(setvar "CMDECHO" 0)
     (setq SLN 3)(DCL_DSLC1)
     (setq PNA "PLC" PNO "PLC-" T2 "赤橙緑")
     (setq T1 (strcat "AP30F-0103-" VV "-S-ROG"))
      (prompt "\n挿入点を指示... ")
     (command "INSERT" "DAP303F004" pause "1" "1" "0" PNA PNO T1 T2 T3 T4 "" "" T7 T8 "" "2" "1" "" "")
     ;(command "INSERT" "DAP303F004" pause "1" "1" "0" PNA PNO T1 T2 T3 T4)
     (setq PC (getvar "LASTPOINT"))
     (setq PCL (list (- (car PC) 30)(cadr PC)))
     (setq PCR (list (+ (car PC) 30)(cadr PC)))
     (command "COLOR" 2)(command "TEXT" "S" "M0" "M" PCL "20" "0" "R")
                        (command "TEXT" "S" "M0" "M" PC  "20" "0" "O")
                        (command "TEXT" "S" "M0" "M" PCR "20" "0" "G")(command "COLOR" 4)
     (redraw)(setvar "CMDECHO" 1)
  )
;;--------------------------------------------------------------------------------------------------
(defun DCL_DSLC1(/)
      (setq DCL_ID (load_dialog "X:/LSP-I/KD.DCL"))
          (if (not (new_dialog "DSLC1" DCL_ID) )(exit)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "E3" "(setq VV \"E3\")(setq T3  \"AC/DV24V\")     (setq T8 \"全電圧式\")")
          (action_tile "H3" "(setq VV \"H3\")(setq T3  \"AC/DC100-110V\")(setq T8 \"R分圧式\")")
          (action_tile "M3" "(setq VV \"M3\")(setq T3  \"AC200-220V\")   (setq T8 \"CR分圧式\")")
          (if (= SLN 1)   
              (progn
                    (action_tile "CR" "(setq COL \"R\")(setq T2 \"赤\")")
                    (action_tile "CG" "(setq COL \"G\")(setq T2 \"緑\")")
                    (action_tile "CO" "(setq COL \"O\")(setq T2 \"橙\")")
                    (action_tile "CY" "(setq COL \"Y\")(setq T2 \"黄\")")
                    (action_tile "CS" "(setq COL \"S\")(setq T2 \"青\")")
                    (action_tile "CW" "(setq COL \"W\")(setq T2 \"白\")")
              )
          )
          (if (or (= SLN 2)(= SLN 3))
              (progn
                    (mode_tile "CR" 1)(mode_tile "CG" 1)(mode_tile "CO" 1)
                    (mode_tile "CY" 1)(mode_tile "CS" 1)(mode_tile "CW" 1)
              )
          )
          (start_dialog) (unload_dialog DCL_ID) (princ)
      (setq T4 "LED球" T7 "角30×30mm")
)

;;ＳＬＣ  ４窓
;;集合表示灯−富士.LSPで対応
;;======================================================================================================
;;11.ブザー−１
;;==================================================================================================
  (defun C:DBZ1 (/ PC)
     (C:ST1)(prompt "\n挿入点を指示... ")
     (command "INSERT" "DBZ30F004" pause  "1" "1" "0" "BZ" "" "" "" "" "")(C:END1)
  )
;;==================================================================================================
;;12.ブザー−２
;;==================================================================================================
  (defun C:DBZ2 (/ PC)
     (C:ST1)(prompt "\n挿入点を指示... ")
     (command "INSERT" "DBZ30F009" pause  "1" "1" "0" "BZ" "" "" "" "" "")(C:END1)
  )
;;==================================================================================================
;;13.ガラス管ヒューズ
;;==================================================================================================
  (defun C:DF (/ SYM PC)
      (setvar "CMDECHO" 0)
      (setq SYM "DF") ;;既定値
      (setq DCL_ID (load_dialog "X:/LSP-I/KD.DCL"))
      (if (not (new_dialog "JT" DCL_ID) )(EXIT))
       
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "J" "(setq SYM \"DF\")")  ;実線
      (action_tile "T" "(setq SYM \"DF-T\")");点線
         (start_dialog)(unload_dialog DCL_ID)(princ)
     (C:ST1)(prompt "\n挿入点を指示... ")
     (command "INSERT" SYM pause  "1" "1" "0" "F" "" "" "" "" "")
     (setvar "CMDECHO" 1)
  )
;;==================================================================================================
;;14.ＣＴ
;;==================================================================================================
  (defun C:DCT (/ PC SYM)
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)
      (setq SYM "DCTMARUT001") ;;既定値
      (setq DCL_ID (load_dialog "X:/LSP-I/KD.DCL"))
      (if (not (new_dialog "JT" DCL_ID) )(EXIT))
       
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "J" "(setq SYM \"DCTMARUT001\")");実線
      (action_tile "T" "(setq SYM \"DCTMARUF001\")");点線
         (start_dialog)(unload_dialog DCL_ID)(princ)
      (prompt "\n挿入点を指示... ")
     (command "INSERT" SYM pause  "1" "1" "0" "CT" "" "" "" "" "")
     (setvar "CMDECHO" 1)
  )
;;==================================================================================================
;;15.メーター外形入力   DBr_IN.LSP / DBr.DCL   2004/Aug/31 By T.Terada
;;==================================================================================================
  (defun C:DAM ( / SYM P1 TYP KAKU)
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)
;;  DCLﾌｧｲﾙの読み込み
      (setq DCL_ID (load_dialog "X:/LSP-I/KD.DCL"))
      (if (not (new_dialog "DAM" DCL_ID) )(EXIT))
       
      (setq DMC (getvar "dimscale"))

     ;;メーターの選択用ダイアログボックス
     ;サイズ
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "1" "(setq KAKU \"1\") (setq SYM \"DY8NF001\")");80角
      (action_tile "2" "(setq KAKU \"2\") (setq SYM \"DY10NF001\")");100角
      (action_tile "3" "(setq KAKU \"3\") (setq SYM \"DY12NF001\")");120角
      (action_tile "4" "(setq KAKU \"4\") (setq SYM \"DY15NF001\")");150角
      (action_tile "5" "(setq KAKU \"5\") (setq SYM \"DL8NF001\")");広角80
      (action_tile "6" "(setq KAKU \"6\") (setq SYM \"DL10NF001\")");広角110
      (action_tile "7" "(setq KAKU \"7\") (setq SYM \"DY8UNF001\")");切替80
      (action_tile "8" "(setq KAKU \"8\") (setq SYM \"DY10UNF001\")");切替100
      (action_tile "9" "(setq KAKU \"9\") (setq SYM \"DY12UNF001\")");切替120
     ;種類
      (action_tile "A" "(setq TYP \"AM\")")
      (action_tile "B" "(setq TYP \"VM\")")
      (action_tile "C" "(setq TYP \"KW\")")
      (action_tile "D" "(setq TYP \"COS\")")
      (action_tile "E" "(setq TYP \"Hz\")")
         (start_dialog)(unload_dialog DCL_ID)(princ)
       (prompt "\n挿入点を指示... ")
      (command "INSERT" SYM pause "1" "1" "0" TYP "" "" "" "" "")
      (setvar "CMDECHO" 1)
  )
;;==================================================================================================
;;16.ＥＬＢアース端子の外形入力   DET_IN.LSP / DET.DCL   2004/Aug/31 By T.Terada
;;==================================================================================================
  (defun C:DET ( / SYM WHAT_NEXT DCL_ID JT VH DJT DVH DJT_BAK DVH_BAK PN SH DSH DSH_BAK)
      (setvar "CMDECHO" 0)
      (DCL_ET)
      (if (not PN)(progn  (alert "タイプが選択されていません")(DCL_ET)))
      (cond ((= WHAT_NEXT 0)(setq DJT DJT_BAK)(princ))((= WHAT_NEXT 1) DJT))
      (cond ((= WHAT_NEXT 0)(setq DVH DVH_BAK)(princ))((= WHAT_NEXT 1) DVH))
      (cond ((= WHAT_NEXT 0)(setq DSH DSH_BAK)(princ))((= WHAT_NEXT 1) DSH))
      (cond ((= DJT "1") (setq JT "J"))
            ((= DJT "21")(setq JT "J"))
            ((= DJT "22")(setq JT "T"))
      )
      (cond ((= DVH "1") (setq VH "V"))
            ((= DVH "31")(setq VH "V"))
            ((= DVH "32")(setq VH "H"))
      )
      (cond ((= DSH "1") (setq SH "ELB"))
            ((= DSH "41")(setq SH "ELB"))
            ((= DSH "42")(setq SH "MCB"))
      )
      
      (setq SYM (strcat SH PN VH JT))
      (princ "\nSYM=")(princ SYM);(princ "\nJT=")(princ JT)  (princ "\nVH=")(princ VH)
       (prompt "\n挿入点を指示... ")
      (command "INSERT" SYM pause "1" "1" "0")
      
      (setq PC (getvar "LASTPOINT"))
      (setq KATA (strcat "EL-" PN))
      (if (= VH "H")   
          (progn
              (setq PC1 (list (+ (car PC) 0)(+ (cadr PC) 5)))
              (command "INSERT" "ZOKUS" PC1 "1" "1" "0" "Cu" "" KATA "" "" "" "" "" "" "" "" "100" "1" "" "")
              (setq PC1 (list (+ (car PC) 80)(+ (cadr PC) 5)))
              (command "INSERT" "ZOKUS" PC1 "1" "1" "0" "KANA" "" "SB-25" "" "" "" "" "" "" "" "" "18" "2" "" "")
          )
      )
      (if (= VH "V")   
          (progn
              (setq PC1 (list (+ (car PC) 5)(+ (cadr PC) 0)))
              (command "INSERT" "ZOKUS-T" PC1 "1" "1" "0" "Cu" "" KATA "" "" "" "" "" "" "" "" "100" "1" "" "")
              (setq PC1 (list (+ (car PC) 5)(- (cadr PC) 80)))
              (command "INSERT" "ZOKUS-T" PC1 "1" "1" "0" "KANA" "" "SB-25" "" "" "" "" "" "" "" "" "18" "2" "" "")
          )
      )
      (setvar "CMDECHO" 1)
  )
;---------------------------------------------------------------------------------------------------
  (defun DCL_ET() ;;  DCLﾌｧｲﾙの読み込み
      (setq DCL_ID (load_dialog "X:/LSP-I/KD.DCL"))
      (setq DJT_BAK DJT)
      (setq DVH_BAK DVH)
      (setq DSH_BAK DSH)
      (if (not DJT)(setq DJT "1"))
      (if (not DVH)(setq DVH "1"))
      (if (not DSH)(setq DSH "1"))
       
      (setq DCL_ID (load_dialog "X:/LSP-I/KD.DCL"))
      (setq WHAT_NEXT 2)
      (while (>= WHAT_NEXT 2)
         (princ "\nWHAT_NEXT=")(princ WHAT_NEXT)
         (if (null (new_dialog "DET" DCL_ID) )(EXIT))
;;         (act_radioset '("E1" "E2") DEF)
         (action_tile "cancel" "(done_dialog)(exit)")
         (action_tile "1"  "(setq PN \"2\")")
         (action_tile "2"  "(setq PN \"3\")")
         (action_tile "3"  "(setq PN \"5\")")
         (action_tile "4"  "(setq PN \"7\")")
         (action_tile "5"  "(setq PN \"9\")")
         (action_tile "6"  "(setq PN \"11\")")
         (action_tile "7"  "(setq PN \"13\")")
         (action_tile "8"  "(setq PN \"15\")")
         (action_tile "9"  "(setq PN \"17\")")
         (action_tile "10" "(setq PN \"19\")")
         (action_tile "11" "(setq PN \"21\")")
         (action_tile "21" "(setq DJT $key)")
         (action_tile "22" "(setq DJT $key)")
         (action_tile "31" "(setq DVH $key)")
         (action_tile "32" "(setq DVH $key)")
         (action_tile "41" "(setq DSH $key)")
         (action_tile "42" "(setq DSH $key)")
         (setq WHAT_NEXT (start_dialog))
      )
      (unload_dialog DCL_ID)(princ)
  )

