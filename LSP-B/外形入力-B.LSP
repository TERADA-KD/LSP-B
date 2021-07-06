;;====================================================================
;; 外形入力.LSP   KD.DCL                      2004/Aug/31 By T.Terada
;;
;; 1.ＷＨＭの外形入力         C:DWH      DWH.DCL
;; 2.任意数の端子台の外形入力 C:DTB_M    DTB.DCL  
;; 3.吊り環 正面              C:DRINGS  RING.DCL
;; 4.吊り環 側面              C:DRINGF  RING.DCL
;; 5.鎧型押し                 C:YOROI  YOROI.DCL
;; 6.カードホルダー           C:CHN      CHN.DCL
;; 7.コンセント               C:DCON    DCON.DCL
;; 8.ＷＨＭ枠の入力           C:WHW  WH-WAKU.DCL
;;==================================================================================================
;;1.ＷＨＭの外形入力         C:DWH  DWH.DCL
;;==================================================================================================
(defun C:DWH ( / SYM D1 D2 D5 D6 DEF DEF_BAK WHAT_NEXT LN SOKU FS DCL_ID)
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
      (DCL_DWH)
      (if (not SYM)(progn  (alert "タイプが選択されていません")(DCL_DWH)))
      (cond ((= WHAT_NEXT 0)(setq DEF DEF_BAK)(princ))((= WHAT_NEXT 1) DEF))
      (cond ((= DEF "1") (setq FS "F"))
            ((= DEF "E1")(setq FS "F"))
            ((= DEF "E2")(setq FS "S"))
      )
      (setq LN (strlen SYM))
      (if (= SYM "DWH135PF045")(setq SOKU "S045")(setq SOKU "S001"))
      (if (and (= FS "S")(= LN 11)) (progn (setq SYM (strcat (substr SYM 1 7) SOKU))))
      (if (and (= FS "S")(= LN 12)) (progn (setq SYM (strcat (substr SYM 1 8) SOKU))))
      (princ "\nSYM = ")(princ SYM);(princ "\nFS = ")(princ FS)
      (setq D1 "WH")
      (if (= FS "F")
          (progn
              (setq D2 (getstring "\nWHM番号を指示:(例：-1)"))
             (cond ((= D2 "") (setq D2 "-1")))
          )
      )
       (prompt "\n挿入点を指示")
      (if (= FS "F")
          (command "INSERT" SYM pause "1" "1" "0" D1 D2 "" "" D5 D6)
          (command "INSERT" SYM pause "1" "1" "0")
      )
    (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)
    (REDRAW)
)
;---------------------------------------------------------------------------------------------------
(defun DCL_DWH()
      (setq DEF_BAK DEF)
      (if (not DEF)(setq DEF "1"))
      (setq DCL_ID (load_dialog "X:/DCL/DWH.DCL"))
      (setq WHAT_NEXT 2)
      (while (>= WHAT_NEXT 2)
         (if (null (new_dialog "DWH" DCL_ID) )(exit)) 
         (act_radioset '("E1" "E2") DEF)
         (action_tile "cancel" "(done_dialog)(exit)")
         (action_tile "A1" "(setq SYM \"DWH135FF001\") (setq D5 \"1φ3W\")(setq D6 \"/5A\")")
         (action_tile "A2" "(setq SYM \"DWH1330FF001\") (setq D5 \"1φ3W\")(setq D6 \"30A\")")
         (action_tile "A3" "(setq SYM \"DWH1312FF001\") (setq D5 \"1φ3W\")(setq D6 \"120A\")")
         (action_tile "A4" "(setq SYM \"DWH135PF001\") (setq D5 \"1φ3W\")(setq D6 \"/5A\")")
         (action_tile "A5" "(setq SYM \"DWH135PF045\") (setq D5 \"1φ3W\")(setq D6 \"/5A\")")
         (action_tile "A6" "(setq SYM \"DWH1312KF001\")(setq D5 \"1φ3W\")(setq D6 \"\")")
         (action_tile "A7" "(setq SYM \"DWH1312KT001\")(setq D5 \"1φ3W\")(setq D6 \"\")")
         
         (action_tile "B1" "(setq SYM \"DWH135FF001\") (setq D5 \"3φ3W\")(setq D6 \"/5A\")")
         (action_tile "B2" "(setq SYM \"DWH1330FF001\") (setq D5 \"3φ3W\")(setq D6 \"30A\")")
         (action_tile "B3" "(setq SYM \"DWH1312FF001\") (setq D5 \"3φ3W\")(setq D6 \"120A\")")
         (action_tile "B4" "(setq SYM \"DWH135PF001\") (setq D5 \"3φ3W\")(setq D6 \"/5A\")")
         (action_tile "B5" "(setq SYM \"DWH135PF045\") (setq D5 \"3φ3W\")(setq D6 \"/5A\")")
         (action_tile "B6" "(setq SYM \"DWH1312KF001\")(setq D5 \"3φ3W\")(setq D6 \"\")")
         (action_tile "B7" "(setq SYM \"DWH1312KT001\")(setq D5 \"1φ3W\")(setq D6 \"\")")
         
         (action_tile "C1" "(setq SYM \"DWH125FF001\") (setq D5 \"1φ2W\")(setq D6 \"/5A\")")
         (action_tile "C2" "(setq SYM \"DWH1230FF001\")(setq D5 \"1φ2W\")(setq D6 \"30A\")")
         (action_tile "C3" "(setq SYM \"DWH1212FF001\")(setq D5 \"1φ2W\")(setq D6 \"120A\")")
         
         (action_tile "D1" "(setq SYM \"DWH345FF001\") (setq D5 \"3φ4W\")(setq D6 \"/5A\")")
         (action_tile "D2" "(setq SYM \"DWH3430FF001\")(setq D5 \"3φ4W\")(setq D6 \"30A\")")
         (action_tile "D3" "(setq SYM \"DWH3412FF001\")(setq D5 \"3φ4W\")(setq D6 \"120A\")")
         
         (action_tile "E1" "(setq DEF $key)")
         (action_tile "E2" "(setq DEF $key)")
         (setq WHAT_NEXT (start_dialog))
      )
      (unload_dialog DCL_ID)
)
;;==================================================================================================
;;2. 任意数の端子台の外形入力   DTB_INN.LSP DTB.DCL   2004/Aug/31 By T.Terada
;;==================================================================================================
(defun C:DTB_M ( / D5 W1 W2 W3 W H TORI KI PC PC2 PN WW HH LU RD TX MKSU AMARI)
     (setq D5 "")(setq W1 "")(setq W2 "")(setq W3 "")(setq W "")(setq H "")(setq MKSU "")(setq TORI "")
     (setq KI "")(setq PC "")(setq PN "")(setq WW "")(setq HH "")(setq AMARI "")(setq LU "")(setq RD "")
     (setq TX "")          ;;変数の初期化
     (setq *error* *err1*)
     (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
     (setq DCL_ID (load_dialog "X:/DCL/DTB.DCL"))
     (if (not (new_dialog "DTB" DCL_ID) )(EXIT))
      (setq LB1 "端子台外形の入力")(set_tile "LB1" LB1)
        ;;図枠の選択用ダイアログボックス
        (action_tile "cancel" "(done_dialog)(exit)")

        ;;東洋技研の端子台
        ;(action_tile "1"  "(setq D5 \"PT-20\")(setq W1 8)(setq W2 24)(setq W3 6)(setq H 42)(setq MKSU 1)")    
        ;(action_tile "2"  "(setq D5 \"PT-30\")(setq W1 10)(setq W2 24)(setq W3 6)(setq H 42)(setq MKSU 1)")   
        ;(action_tile "3"  "(setq D5 \"PT-40\")(setq W1 12)(setq W2 24)(setq W3 6)(setq H 42)(setq MKSU 1)")   
        ;(action_tile "4"  "(setq D5 \"PT-80\")(setq W1 14)(setq W2 24)(setq W3 6)(setq H 46)(setq MKSU 1)")   
        ;(action_tile "5"  "(setq D5 \"PT-90\")(setq W1 20)(setq W2 24)(setq W3 6)(setq H 64)(setq MKSU 1)")   
        ;(action_tile "6"  "(setq D5 \"PT-150\")(setq W1 26)(setq W2 24)(setq W3 6)(setq H 74)(setq MKSU 1)")  
        ;(action_tile "7"  "(setq D5 \"PT-200\")(setq W1 40)(setq W2 30)(setq W3 10)(setq H 86)(setq MKSU 1)") 
        ;(action_tile "8"  "(setq D5 \"PT-300\")(setq W1 48)(setq W2 30)(setq W3 10)(setq H 98)(setq MKSU 1)") 
        ;(action_tile "9"  "(setq D5 \"PT-400\")(setq W1 58)(setq W2 30)(setq W3 10)(setq H 110)(setq MKSU 1)")
        ;(action_tile "X"  "(setq D5 \"PT-600\")(setq W1 66)(setq W2 30)(setq W3 10)(setq H 124)(setq MKSU 1)")

        (action_tile "1"  "(setq D5 \"PT-20\")(setq W1 8)(setq W2 24)(setq W3 6)(setq H 42)(setq MKSU 1)")    ;;PT-20を選択
        (action_tile "2"  "(setq D5 \"PT-30\")(setq W1 10)(setq W2 24)(setq W3 6)(setq H 42)(setq MKSU 1)")   ;;PT-30を選択
        (action_tile "3"  "(setq D5 \"PT-40\")(setq W1 11)(setq W2 24)(setq W3 6)(setq H 42)(setq MKSU 1)")   ;;PT-40を選択
        (action_tile "4"  "(setq D5 \"PT-80\")(setq W1 14)(setq W2 24)(setq W3 6)(setq H 46)(setq MKSU 1)")   ;;PT-80を選択
        (action_tile "5"  "(setq D5 \"PT-90\")(setq W1 19)(setq W2 24)(setq W3 6)(setq H 64)(setq MKSU 1)")   ;;PT-90を選択
        (action_tile "6"  "(setq D5 \"PT-150\")(setq W1 25)(setq W2 24)(setq W3 6)(setq H 74)(setq MKSU 1)")  ;;PT-150を選択
        (action_tile "7"  "(setq D5 \"PT-200\")(setq W1 39)(setq W2 30)(setq W3 10)(setq H 86)(setq MKSU 1)") ;;PT-200を選択
        (action_tile "8"  "(setq D5 \"PT-300\")(setq W1 47)(setq W2 30)(setq W3 10)(setq H 98)(setq MKSU 1)") ;;PT-300を選択
        (action_tile "9"  "(setq D5 \"PT-400\")(setq W1 58)(setq W2 30)(setq W3 10)(setq H 110)(setq MKSU 1)");;PT-400を選択
        (action_tile "X"  "(setq D5 \"PT-600\")(setq W1 66)(setq W2 30)(setq W3 10)(setq H 124)(setq MKSU 1)");;PT-600を選択   

        ;;キムデンの端子台     
        (action_tile "11"  "(setq D5 \"TS-802RK2\")(setq W1  9)(setq W2 43)(setq W3 19)(setq H 38)(setq MKSU 2)") ;;TS-802RK2を選択
        (action_tile "12"  "(setq D5 \"TS-803RK2\")(setq W1 11)(setq W2 43)(setq W3 19)(setq H 38)(setq MKSU 2)") ;;TS-803RK2を選択
        (action_tile "13"  "(setq D5 \"TS-804RK2\")(setq W1 12)(setq W2 43)(setq W3 19)(setq H 38)(setq MKSU 2)") ;;TS-804RK2を選択
        (action_tile "14"  "(setq D5 \"TS-805RK2\")(setq W1 15)(setq W2 43)(setq W3 19)(setq H 44)(setq MKSU 2)") ;;TS-805RK2を選択
        (action_tile "15"  "(setq D5 \"TS-806RK2\")(setq W1 16)(setq W2 43)(setq W3 19)(setq H 48)(setq MKSU 2)") ;;TS-806RK2を選択
        (action_tile "16"  "(setq D5 \"TS-807RK2\")(setq W1 20)(setq W2 43)(setq W3 19)(setq H 52)(setq MKSU 2)") ;;TS-807RK2を選択
        (action_tile "17"  "(setq D5 \"TS-808RK2\")(setq W1 26)(setq W2 43)(setq W3 19)(setq H 62)(setq MKSU 2)") ;;TS-808RK2を選択
        (action_tile "18"  "(setq D5 \"TS-809RK2\")(setq W1 36)(setq W2 43)(setq W3 19)(setq H 68)(setq MKSU 2)") ;;TS-809RK2を選択
        (action_tile "19"  "(setq D5 \"TS-810RK2\")(setq W1 53)(setq W2 43)(setq W3 19)(setq H 86)(setq MKSU 2)") ;;TS-810RK2を選択
        (action_tile "1X"  "(setq D5 \"TS-811RK2\")(setq W1 60)(setq W2 43)(setq W3 19)(setq H 112)(setq MKSU 2)");;TS-811RK2を選択

        ;;マルヤスの端子台
        (action_tile "40" "(setq D5 \"TM-150-S\")(setq W1 32)(setq W2 50)(setq W3 0)(setq H 72)(setq MKSU 3)")  ;;TM-150-Sを選択
        (action_tile "41" "(setq D5 \"TM-200-S\")(setq W1 40)(setq W2 50)(setq W3 0)(setq H 82)(setq MKSU 3)")  ;;TM-200-Sを選択
        (action_tile "42" "(setq D5 \"TM-300-S\")(setq W1 47)(setq W2 50)(setq W3 0)(setq H 90)(setq MKSU 3)")  ;;TM-300-Sを選択
        (action_tile "43" "(setq D5 \"TM-400-S\")(setq W1 55)(setq W2 50)(setq W3 0)(setq H 100)(setq MKSU 3)") ;;TM-400-Sを選択
        (action_tile "44" "(setq D5 \"TMD-500-S\")(setq W1 65)(setq W2 20)(setq W3 0)(setq H 110)(setq MKSU 3)");;TMD-500-Sを選択
        (action_tile "45" "(setq D5 \"TMD-600-S\")(setq W1 65)(setq W2 20)(setq W3 0)(setq H 110)(setq MKSU 3)");;TMD-600-Sを選択
        ;;取付
        (action_tile "21" "(setq TORI \"T\")") ;;留め金具を選択
        (action_tile "22" "(setq TORI \"P\")") ;;プレートを選択
        (action_tile "23" "(setq TORI \"N\")") ;;無しを選択
        ;;基点
        (action_tile "31" "(setq KI   \"U\")") ;;上を選択
        (action_tile "32" "(setq KI   \"C\")") ;;中心を選択
        (action_tile "33" "(setq KI   \"D\")") ;;下を選択

    (start_dialog)(unload_dialog DCL_ID)(princ)
        (if (= MKSU 3)(setq TORI "T")) ;*マルヤスの場合は、留め金具選択になる。
    ;(princ "\nW1=")(princ W1)
    ;(princ "\nW2=")(princ W2)
    ;(princ "\nW3=")(princ W3)
    ;(princ "\nH=")(princ H)    

     (setq PC (getpoint "\n挿入点を指示..."))
     (setq PN (getint   "\n何Ｐですか (ﾘﾀｰﾝ = 3)："))
    (if (= PN nil)(setq PN 3))           ;;Ｐ数が未入力の場合は3
    (setq WW (* W1 PN))                  ;;端子台の横幅＝横*Ｐ数
    (setq HH (/ H 2))                    ;;端子台の中心＝高さ/2

    ;(princ "\nWW = ")(princ WW)
    (if (= TORI "T")(setq WW (+ WW W2))) ;;留め金具を選んだ場合 横幅に留め金具の変数をたす
    (if (= TORI "P")(setq WW (+ WW W3))) ;;プレートを選んだ場合 横幅にプレートの変数をたす
    ;(princ "\nWW = ")(princ WW)

    (setq AMARI (rem WW 2))              ;;奇数か偶数かを判別させる為に余りを計算する
    ;(princ "\nAMARI=")(princ AMARI)

    ;(setq aaa  (zerop (logand WW 1)))   ;;1KD.LSP 2-4 参照  偶数ならT 奇数ならnilが返ってくる
    ;(princ "\naaa=")(princ aaa)
    
    ;(if (and (= MKSU 2)(= AMARI 1))     ;;横幅を計算して余りがでた場合は横幅に+1する
        ;(setq WW (+ WW 1))
    ;);if
    
    (if (= AMARI 1)                     ;;横幅を計算して余りがでた場合は横幅に+1する
        (setq WW (+ WW 1))
    );if


    ;(princ "\nWW2=")(princ WW)
    
    (if (= KI "U")(setq PC2 (list (car PC)(- (cadr PC) HH)))) ;;基点を上にした時の図形の挿入位置
    (if (= KI "C")(setq PC2 PC))                              ;;基点を中心にした時の図形の挿入位置
    (if (= KI "D")(setq PC2 (list (car PC)(+ (cadr PC) HH)))) ;;基点を下にした時の図形の挿入位置
    
    ;(princ "\nPC2=")(princ PC2)
    
    (setq LU (list (+ (car PC2) (/ WW 2))(+ (cadr PC2) HH)))  ;;端子台の外形図の左上
    (setq RD (list (- (car PC2) (/ WW 2))(- (cadr PC2) HH)))  ;;端子台の外形図の右下
    (command "COLOR" "4")(command "rectangle" LU RD)

    (setq TX (list (car PC2)(- (- (cadr PC2) HH) 25)))        ;;型番をテキストで挿入する位置
    (command "COLOR" "2")(command "TEXT" "S" "M0" "C" TX "15" "0" D5)
                         (command "TEXT" "S" "M0" "M" PC2 "20" "0" "TB")(command "COLOR" "4")
    (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)(redraw)
)
;;==================================================================================================
;;3.吊り環 正面図の入力
;;==================================================================================================
  (defun C:DRINGS (/ PC SYM)
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)
      (setq DCL_ID (load_dialog "X:/DCL/RING.DCL"))
      (if (not (new_dialog "RING" DCL_ID) )(EXIT))
       
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "1" "(setq SYM \"RM16-F\")");RM-16F
      (action_tile "2" "(setq SYM \"RM20-F\")");RM-20F
         (start_dialog)
         (unload_dialog DCL_ID)
         (princ)
      (prompt "\n挿入点を指示")
     (command "INSERT" SYM pause "1" "1" "0")
     (setvar "CMDECHO" 1)
  )
;;==================================================================================================
;;4.吊り環 側面図の入力
;;==================================================================================================
  (defun C:DRINGF (/ PC SYM)
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)
      (setq DCL_ID (load_dialog "X:/DCL/RING.DCL"))
      (if (not (new_dialog "RING" DCL_ID) )(EXIT))
       
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "1" "(setq SYM \"RM16-S\")");RM-16S
      (action_tile "2" "(setq SYM \"RM20-S\")");RM-20S
         (start_dialog)
         (unload_dialog DCL_ID)
         (princ)
      (prompt "\n挿入点を指示")
     (command "INSERT" SYM pause "1" "1" "0")
     (setvar "CMDECHO" 1)
  )
;;==================================================================================================
;;5.鎧型押しの入力
;;==================================================================================================
  (defun C:YOROI (/ DEF DEF_BAK DCL_ID WHAT_NEXT PC SYM)
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)
      (setq DEF_BAK DEF)
      (if (not DEF)(setq DEF "1"))
      (setq DCL_ID (load_dialog "X:/DCL/YOROI.DCL"))
      (setq WHAT_NEXT 2)
      (while (>= WHAT_NEXT 2)
         (if (null (new_dialog "YOROI" DCL_ID) )(exit))
          
         (act_radioset '("1" "2" "3") DEF)
         (action_tile "cancel" "(done_dialog)(exit)")
         (action_tile "1" "(setq DEF $key)")
         (action_tile "2" "(setq DEF $key)")
         (action_tile "3" "(setq DEF $key)")
         (setq WHAT_NEXT (start_dialog))
      )
      (unload_dialog DCL_ID)
      (cond ((= WHAT_NEXT 0)(setq DEF DEF_BAK)(princ))
            ((= WHAT_NEXT 1) DEF)
      )
      (cond ((= DEF "1") (setq SYM "YOROI1"))
            ((= DEF "2") (setq SYM "YOROI3"))
            ((= DEF "3") (setq SYM "YOROI4"))
      )
       (prompt "\n挿入点を指示")
      (command "INSERT" SYM pause "1" "1" "0")
      (setvar "CMDECHO" 1)
  )
;;==================================================================================================
;;6.カードホルダーの入力
;;==================================================================================================
  (defun C:CHN (/ PC SYM)
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)
      (setq DCL_ID (load_dialog "X:/DCL/CHN.DCL"))
      (if (not (new_dialog "CHN" DCL_ID) )(EXIT))
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "1" "(setq SYM \"CHKA4\")")
      (action_tile "2" "(setq SYM \"CHKA5S\")")
      (action_tile "3" "(setq SYM \"CHKA6S\")")
      (action_tile "4" "(setq SYM \"CHKA7S\")")
      (action_tile "5" "(setq SYM \"CHA4\")")
      (action_tile "6" "(setq SYM \"CH\")")
      
      (start_dialog)
      (unload_dialog DCL_ID)
      
      (if (= SYM "CHKA4") (progn (setq KATA "DHP-A4")   (setq TEI "")  (setq MEIKA 36) ))
      (if (= SYM "CHKA5S")(progn (setq KATA "C-26-C-A5")(setq TEI "A5")(setq MEIKA 34) ))
      (if (= SYM "CHKA6S")(progn (setq KATA "C-26-C-A6")(setq TEI "A6")(setq MEIKA 34) ))
      (if (= SYM "CHKA7S")(progn (setq KATA "C-26-C-A7")(setq TEI "A7")(setq MEIKA 34) ))
      (if (= SYM "CHA4")  (progn (setq KATA "CHA4")     (setq TEI "")  (setq MEIKA 100)))
      (if (= SYM "CH")    (progn (setq KATA "CH")       (setq TEI "")  (setq MEIKA 100)))
      (princ "\nSYM = ")  (princ SYM)
      (princ "\nKATA = ") (princ KATA)
      (princ "\nMEIKA = ")(princ MEIKA)
      
       (prompt "\n挿入点を指示")
      (command "INSERT" SYM pause "1" "1" "0" "CH" "" KATA "" TEI "" "" "" "" "" "" MEIKA "1" "" "")
      (setvar "CMDECHO" 1)
  )
;;==================================================================================================
;;7.コンセントの外形入力
;;==================================================================================================
  (defun C:DCON (/ PC SYM)
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)
      (setq DCL_ID (load_dialog "X:/DCL/DCON.DCL"))
      (if (not (new_dialog "DCON" DCL_ID) )(EXIT))
       
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "1" "(setq SYM \"CON-E1D\")")
      (action_tile "2" "(setq SYM \"CON-E2D\")")
      (action_tile "3" "(setq SYM \"CON-E1D\")")
      (action_tile "4" "(setq SYM \"CON-E1D\")")
      (action_tile "5" "(setq SYM \"CON-E1D\")")
         (start_dialog)
         (unload_dialog DCL_ID)
         (princ)
      (prompt "\n挿入点を指示")
     (command "INSERT" SYM pause "1" "1" "0")
     (setvar "CMDECHO" 1)
  )
;;==================================================================================================
;;8.ＷＨＭ枠の入力         C:WHW  WH-WAKU.DCL
;;==================================================================================================
(defun C:WHW ( / SYM PC D1 D2 D5 D6 No)
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
      (setq DCL_ID (load_dialog "X:/DCL/WH-WAKU.DCL"))
      (if (not (new_dialog "WHW" DCL_ID) )(EXIT)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "1" "(setq K1 \"1\")(setq MK \"39\")")
          (action_tile "2" "(setq K1 \"2\")(setq MK \"39\")")
          (action_tile "3" "(setq K1 \"3\")(setq MK \"39\")")
          (action_tile "4" "(setq K1 \"4\")(setq MK \"39\")")
          (action_tile "11" "(setq K1 \"11\")(setq MK \"18\")")
          (action_tile "12" "(setq K1 \"12\")(setq MK \"18\")")
          (action_tile "13" "(setq K1 \"13\")(setq MK \"18\")")
          (action_tile "14" "(setq K1 \"14\")(setq MK \"18\")")
          (action_tile "15" "(setq K1 \"15\")(setq MK \"18\")")
          (action_tile "16" "(setq K1 \"16\")(setq MK \"18\")")
          (action_tile "17" "(setq K1 \"17\")(setq MK \"18\")")
          (action_tile "18" "(setq K1 \"18\")(setq MK \"18\")")
          (action_tile "19" "(setq K1 \"19\")(setq MK \"18\")")
          (action_tile "20" "(setq K1 \"20\")(setq MK \"18\")")
          (action_tile "21" "(setq K1 \"21\")(setq MK \"18\")")
          (action_tile "22" "(setq K1 \"22\")(setq MK \"18\")")
       (start_dialog)
       (unload_dialog DCL_ID)
    (princ)
    (cond
      ((= K1 "1") (setq K1 "VP16-1"  K2 ""      K3 "1ヶ単位" K5 "強化ガラス" SK ""              SYM "VP16-1"))
      ((= K1 "2") (setq K1 "VP16-2"  K2 ""      K3 "1ヶ単位" K5 "強化ガラス" SK ""              SYM "VP16-2"))
      ((= K1 "3") (setq K1 "VP16-3"  K2 ""      K3 "1ヶ単位" K5 "網入ガラス" SK ""              SYM "VP16-3"))
      ((= K1 "4") (setq K1 "VP16-4"  K2 ""      K3 "1ヶ単位" K5 "網入ガラス" SK ""              SYM "VP16-4"))
      ((= K1 "11")(setq K1 "PMY1010" K2 "-K3P1" K3 ""        K5 "強化ガラス" SK "PMY1010用ｸｻﾋﾞ" SYM "PMY1010"))
      ((= K1 "12")(setq K1 "PMY1010" K2 "-K3P2" K3 ""        K5 "強化ガラス" SK "PMY1010用ｸｻﾋﾞ" SYM "PMY1010"))
      ((= K1 "13")(setq K1 "PMY1010" K2 "-K3P3" K3 ""        K5 "強化ガラス" SK "PMY1010用ｸｻﾋﾞ" SYM "PMY1010"))
      ((= K1 "14")(setq K1 "PMY1010" K2 "-A2P1" K3 ""        K5 "ｱｸﾘﾗｲﾄ"     SK "PMY1010用ｸｻﾋﾞ" SYM "PMY1010"))
      ((= K1 "15")(setq K1 "PMY1010" K2 "-A2P2" K3 ""        K5 "ｱｸﾘﾗｲﾄ"     SK "PMY1010用ｸｻﾋﾞ" SYM "PMY1010"))
      ((= K1 "16")(setq K1 "PMY1010" K2 "-A2P3" K3 ""        K5 "ｱｸﾘﾗｲﾄ"     SK "PMY1010用ｸｻﾋﾞ" SYM "PMY1010"))
      ((= K1 "17")(setq K1 "PMY1015" K2 "-K3P1" K3 ""        K5 "強化ガラス" SK "PMY1015用ｸｻﾋﾞ" SYM "PMY1015"))
      ((= K1 "18")(setq K1 "PMY1015" K2 "-K3P2" K3 ""        K5 "強化ガラス" SK "PMY1015用ｸｻﾋﾞ" SYM "PMY1015"))
      ((= K1 "19")(setq K1 "PMY1015" K2 "-K3P3" K3 ""        K5 "強化ガラス" SK "PMY1015用ｸｻﾋﾞ" SYM "PMY1015"))
      ((= K1 "20")(setq K1 "PMY1015" K2 "-A2P1" K3 ""        K5 "ｱｸﾘﾗｲﾄ"     SK "PMY1015用ｸｻﾋﾞ" SYM "PMY1015"))
      ((= K1 "21")(setq K1 "PMY1015" K2 "-A2P2" K3 ""        K5 "ｱｸﾘﾗｲﾄ"     SK "PMY1015用ｸｻﾋﾞ" SYM "PMY1015"))
      ((= K1 "22")(setq K1 "PMY1015" K2 "-A2P3" K3 ""        K5 "ｱｸﾘﾗｲﾄ"     SK "PMY1015用ｸｻﾋﾞ" SYM "PMY1015"))
    )
       (prompt "\n挿入点を指示")
      (command "INSERT" SYM pause "1" "1" "0" "WW" "" K1 K2 K3 "" K5 "" "" "" "" MK "1" "" "")
      (setq PC (getvar "LASTPOINT"))
      (if (/= SK "")(command "INSERT" "クサビ" PC "1" "1" "0" "WW" "" SK "" "" "" "" "" "" "" "" MK "1" "" ""))
      (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)
      (REDRAW)
)
;;==================================================================================================
;;(act_radioset 引数 引数) はラジオボタン用の set_tile 関数。
;;(act_radioset 引数1 引数2)
;;引数1　dcl の radio_button の key のリスト
;;引数2　action_tile で得た値
  (defun act_radioset(KEY_LIST DATA /)
      (foreach n KEY_LIST (if (= n DATA)(set_tile n "1")))
  )

