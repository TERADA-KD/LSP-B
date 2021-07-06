;;==================================================================================================
;;壁掛型BOX 正面・側面図    GetBox.LSP + CGetBox.CCP  2002/NOV ==> DCL化 2020/Dec     BY T.Terada
;;==================================================================================================
;;手動モードでは盤寸法や屋内・屋外や内扉等の仕様をダイアログで入力して正面・側面図を作図する。
;;自動モードでは内部配置図の寸法を取得して正面・側面図を自動作図する。＊２枚扉の窓付きは出来ない。
;;既製BOXは河村電器産業のみです。内外は取り止め。
;;==================================================================================================
(defun DCL_GBOX(/);手動・自動選択のダイアログ
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
          (if (not (new_dialog "GBOX" DCL_ID) )(exit))
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "A" "(setvar \"USERS5\" \"A\")")
          (action_tile "M" "(setvar \"USERS5\" \"M\")")
      (start_dialog) (unload_dialog DCL_ID) (princ)
)
;;==================================================================================================
(defun DCL_GetBox (/);手動事の入力用DCLの起動
	  (setq DWP nil DKJ nil DKT nil DLM nil DDN nil DDR nil DCS nil DCH nil DBC nil)
	  (setq DOP1 nil DOP2 nil)
      (if (not DWP)(setq DWP "1"));初期値の設定
      (if (not DKJ)(setq DKJ "1"))
      (if (not DKT)(setq DKT "1"))
      (if (not DLM)(setq DLM "1"))
      (if (not DDN)(setq DDN "1"))
      (if (not DDR)(setq DDR "1"))
      (if (not DCS)(setq DCS "1"))
      (if (not DCH)(setq DCH "1"))
      (if (not DBC)(setq DBC "1"))
;      (if (not DOP)(setq DOP "1"))
      (setq DCL_ID (load_dialog "X:/LSP-I/GetBox.DCL"))
      (setq WHAT_NEXT 2)
      (while (>= WHAT_NEXT 2);;;ステータスが非表示を示している場合
         (if (not (new_dialog "GetBox" DCL_ID) )(EXIT))
         (action_tile "cancel" "(done_dialog)(exit)")
         
         ;mode_tile: "0"= 使用可能 / "1"= 禁止
         (action_tile "WN" "(setq DWP $key)   ;ＢＯＸタイプ  
                  (mode_tile \"WN\" 0)(mode_tile \"M\" 0)(mode_tile \"L\" 0)")
         (action_tile "WP" "(setq DWP $key)(mode_tile \"M\" 0)(mode_tile \"L\" 1)")
                  
         (action_tile "KK" "(setq DKJ $key)");壁掛/自立
         (action_tile "JJ" "(setq DKJ $key)")
         
         (action_tile "KB" "(setq DKT $key)");既製/特注
         (action_tile "TB" "(setq DKT $key)")
         
         (action_tile "L" "(setq DLM $key)")   ;額縁/突合せ
                        
         (action_tile "M" "(setq DLM $key)")
         
         (action_tile "D1" "(setq DDN $key)");扉の数
         (action_tile "D2" "(setq DDN $key)")
         
         (action_tile "Y1" "(setq DDR $key)") ;内扉の有無
         (action_tile "N1" "(setq DDR $key)")

         (action_tile "Y2" "(setq DCS $key)") ;ケーブルサポートの有無
         (action_tile "N2" "(setq DCS $key)")
         
         (action_tile "ST" "(setq DCH $key)") ;カードホルダーの種類
         (action_tile "A4" "(setq DCH $key)")
         
         (action_tile "U" "(setq DOP1 $key)") ;オプション
         (action_tile "W" "(setq DOP2 $key)")

         (action_tile "C5" "(setq DBC $key)(mode_tile \"C5\"0)(mode_tile \"IR\" 1)") ;塗装色
         (action_tile "C2" "(setq DBC $key)(mode_tile \"C2\"0)(mode_tile \"IR\" 1)")
         (action_tile "C9" "(setq DBC $key)(mode_tile \"C9\"0)(mode_tile \"IR\" 0)")

         (action_tile "IR" "(setq IR (get_tile \"IR\"))");指定色
;         (princ "\nT or nil")(princ (numberp BD))(princ "\nモード=")(princ U5)
         (if (= U5 "A")
         	(progn
         	    (setq BW (rtos BW))(set_tile "BW" BW)
         	    (setq BH (rtos BH))(set_tile "BH" BH)
         	    (set_tile "BD" BD);BDは文字列
            )
         )
         
         (if (= U5 "M")
         	(progn
         		(action_tile "BW" "(setq BW (get_tile \"BW\"))");箱体寸法
         		(action_tile "BH" "(setq BH (get_tile \"BH\"))")
         		(action_tile "BD" "(setq BD (get_tile \"BD\"))")
            );progn
         );if
         (setq WHAT_NEXT (start_dialog))
	  )
      (unload_dialog DCL_ID)(princ)
)
;;==================================================================================================
;;メインプログラム
(defun C:GetBox(/ SS ENAM EDAT ERR AA BB CC1 DD1 n ENT P1 P2 LU1 LD1 RU1 RD1 PDIM PDIM1 PDIM2
       PDIM PDIM4 PDIM6 SDIM SDIM1 PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 BD1 BW1 BH1 PC PFX PZ PZ1 COL
       BKATA CKATA SPD KEY KEYL B1 B1C B2 B2C B3 B3C IP Bt Dt FRH LSU ULU1 URU1 ULD1 FU1 FD1 OP1 OP2
       PCCU PCCD D2T D4T LCD RCD CCL CCR WPC 2WPC WLU WRD WPC2 WLU2 WPT LU2D LD2D LD2B RP1 RP2 RP3
       RP4 RP5 NU ND DNR DNL RU22 LU22 SLCD SRCD SCCL SCCR LU2 RD2 RU2 IUR IUL IDR IDL ZB1 ZB2 ZB3
       ZB4 ZB5 ZB6 ZB7 ZB8 ZB9 ZB10 CAH DIMH1 DIMH2 HH PDY PDYL PDYR STL DSTR DSTL CHAN RINL RINR
       RINS FDO FDI WW WH WC PU1 PU2 PU3 PU4 PD1 PD2 PD3 PD4 DD2 FU2 FD2 SP SZ WS OFFS ID LL LD11
       RD11 LD22 RD22 5DIML 5DIMR LDLD LDLDR PDY1 PDY2 DSTRR DSTLL DSTRR1 DSTLL1 PDY5 PUYR1 SUNLD2
       SUNRD2 PUYL1 ET1 ET2 M5MJ ETMJ PDY6 MANSUN0 MANSUNR MANSUNL ZOOM1L ZOOM1R ZOOM2L ZOOM2R
       WP WN KK JJ KB TB L M D1 D2 N1 Y1 N2 Y2 ST A4 U W C5 C2 C9 BW BH BD)

;初期設定                                           DCL からもらう変数
;  (setq *error* *err1*)                            ; WP   "WN"= 屋内型  "WP"= 屋外型
   (setvar "CMDECHO" 1)                             ; KJ   "KK"= 壁掛型  "JJ"= 自立型
   (command "COLOR" 4)    ;水色                     ; KT   "KB"= 既製BOX "TB"= 特注BOX
   (setvar "PLINEWID" 0)  ;線幅を0                  ; LM   "L" = 落込    "M" = 突合せ
                                                    ; DN   "D1"= １枚扉  "D2"= ２枚扉
   (command "LINETYPE" "S" "CONTINUOUS" "")         ; DR   "Y1"= 内扉有  "N1"= 内扉無
;  (command "LAYER" "S" "BOX" "");画層 BOXに変更    ; OP   "U" = 埋込み  "W" = 窓付き
                                                    ; CS   "Y2"= 有り    "N2"=なし ケーブルサポート
                                                    ; BC   "C5"= 5Y7/1   "C2"= 2.5Y9/1  "C9"= 指定色
                                                    ; BMK  "K"=河村
   (DCL_GBOX);手動・自動選択のダイアログ
   (setq U5 (getvar "USERS5"))
   (if (= U5 "M");手動モード
       (progn
          (setq BW 0)(setq BH 0)(setq BD 0)
       )
;;自動モード----------------------------------------------------------------------------------------
       (progn 
          (if (= U5 "A")(princ "自動モード"))(princ)
          ;;箱体の横幅と高さを取得
          (setq BW 0)(setq BH 0)(setq BD "0")
          (princ "\nオブジェクトを選択で内部配置図の外枠をクリック")
          (setq SS (ssget))
          (setq ENAM (ssname SS 0));   (princ "ENAM=")(princ ENAM)(princ "\n")
          (setq EDAT (entget ENAM))(princ "EDAT=")(princ EDAT)(princ "\n");選択した図形の内容を見る
          (princ "\nLWP = ")(princ (cdr (assoc 0 EDAT)))
          (if (= (cdr (assoc 0 EDAT)) "LWPOLYLINE")
              (progn (setq P1 (cdr (assoc 10 EDAT)))
                     (setq P2 (cdr (nth 26 EDAT))));ICAD4では nth 23
          )
          (setq BW (- (car P2) (car P1))) (setq BH (- (cadr P1) (cadr P2)))
          (princ "\nP1=")(princ P1)(princ "\nP2=")(princ P2)
          (princ "\nBW=")(princ BW)(princ "\nBH=")(princ BH)

          ;;箱の奥行を取得
          (setq n 0)
          (setq AA (ssget "X" (list '(66 . 1))))
          (while (setq BB (ssname AA n))
             (setq ENT (entget BB))
             (if (= (cdr (assoc 2 ENT)) "BOXD")
                (progn   ;(princ "ENT=")(princ ENT)(princ "\n")
                   (setq CC1 (entnext BB))  ; 次の図形名
                    (setq DD1 (entget CC1))  ; 図形リスト
                   (while (/= (cdr (assoc 0 DD1)) "SEQEND");SEQENDまで繰り返し
                      (if (= (cdr (assoc 0 DD1)) "ATTRIB")(setq BD (cdr (assoc 1 DD1))))
                      (setq CC1 (entnext CC1)  DD1 (entget CC1))
                   );while
                );progn
             );if
             (setq n (1+ n))
          );while
          (princ "\nBD=")(princ BD)(princ)
       );progn
   );if;手動・自動モード
   (princ "\nEnd SSget")
;;-------------------------------------------------------------------------------------------------
   (setq BMK "K");標準BOX：河村電器に設定
   (DCL_GetBox)  ;箱体仕様、確認ダイアログ

   (if (= BMK "")(exit));;Cancel時は終了
   (cond ((= DWP "1") (setq WP "WP"))
         ((= DWP "WP")(setq WP "WP"))
         ((= DWP "WN")(setq WP "WN"))
   )
   (cond ((= DKJ "1") (setq KJ "KK"))
         ((= DKJ "KK")(setq KJ "KK"))
         ((= DKJ "JJ")(setq KJ "JJ"))
   )
   (cond ((= DKT "1") (setq KT "KB"))
         ((= DKT "KB")(setq KT "KB"))
         ((= DKT "TB")(setq KT "TB"))
   )
   (cond ((= DLM "1")(setq LM "M"))
         ((= DLM "M")(setq LM "M"))
         ((= DLM "L")(setq LM "L"))
   )
   (cond ((= DDN "1") (setq DN "D1"))
         ((= DDN "D1")(setq DN "D1"))
         ((= DDN "D2")(setq DN "D2"))
   )
   (cond ((= DDR "1") (setq DR "N1"))
         ((= DDR "N1")(setq DR "N1"))
         ((= DDR "Y1")(setq DR "Y1"))
   )
   (cond ((= DCS "1") (setq CS "Y2"))
         ((= DCS "Y2")(setq CS "Y2"))
         ((= DCS "N2")(setq CS "N2"))
   )
   (cond ((= DCH "1") (setq CH "ST"))
         ((= DCH "ST")(setq CH "ST"))
         ((= DCH "A4")(setq CH "A4"))
   )
   (cond ((= DOP1 "U")(setq OP1 "U")))
   (cond ((= DOP2 "W")(setq OP2 "W")))
   
   (cond ((= DBC "1") (setq BC "C2"))
         ((= DBC "C2")(setq BC "C2"))
         ((= DBC "C5")(setq BC "C5"))
         ((= DBC "C9")(setq BC "C9"))
   )
;(princ "\nWP=")(princ WP)(princ "\nKJ=")(princ KJ)(princ "\nKT=")(princ KT)
;(princ "\nLM=")(princ LM)(princ "\nDN=")(princ DN)(princ "\nDR=")(princ DR)
;(princ "\nCS=")(princ CS)(princ "\nCH=")(princ CH)(princ "\nBC=")(princ BC)
;(princ "\nOP1=")(princ OP1)(princ "\nOP2=")(princ OP2)(princ "\nIR=")(princ IR)
;(princ "\nBW=")(princ BW)(princ "\nBH=")(princ BH)(princ "\nBD=")(princ BD)
;;-----------------------------------------------------------------------------

;;箱体のサイズの判定 DCLでは値は文字列なので単精度の数値に変換する
   (setq BW (atoi BW))(setq BH (atoi BH))(setq BD (atoi BD));文字を数値に変換
;   (princ "\nBW= T or nil")(princ (numberp BW))(princ "\nBH= T or nil")(princ (numberp BH))
;   (princ "\nBD= T or nil")(princ (numberp BD))
   (setq BD1 BD)
   (if (= KT "KB")
        (progn
             (if (or (= BD1 100)(= BD1 120)(= BD1 140)(= BD1 160)(= BD1 180)
                                (= BD1 200)(= BD1 250)(= BD1 300)(= BD1 350))
                 (princ "\n奥行きﾁｪｯｸ OK")
                 (progn
                    (princ "\n奥行きﾁｪｯｸエラー :")(princ BD1)
                    (alert "既製BOXには その奥行サイズは ありません！\n\nまたは小数点表示になっています！")
                    (princ "\n終了します。もう一度やり直してください！！")
                    (exit)
                 );progn
             );if
        );progn
    );if

    ;挿入点
     (setq PC (getpoint "\n挿入点を指示して下さい．(上端の中心点):"))
;;-----------------------------------------------------------------------------
;;箱体条件の設定と判定 
;    (setq PFX nil)
  ;屋内・既成・落込みの場合
    (if (and (= WP "WN")(= KT "KB")(= LM "L"))(setq PFX "FX"))                 ;FX BOX
    (if (and (= WP "WN")(= KT "KB")(= LM "L")(= OP1 "U")) (setq PFX "FXU"))     ;FXU BOX 
    
  ;屋内・既成・突合せの場合
    (if (and (= WP "WN")(= KT "KB")(= LM "M"))           (setq PFX "RX"))      ;RX BOX
    (if (and (= WP "WN")(= KT "KB")(= LM "M")(= OP2 "W")) (setq PFX "RXM"))     ;RXM BOX 
   
  ;屋外・既成（屋外の場合は突合になる）
    (if (and (= WP "WP")(= KT "KB")(= LM "M"))           (setq PFX "PO"))      ;PO BOX
    (if (and (= WP "WP")(= KT "KB")(= LM "M")(>= BH 1700))(setq PFX "POT"))    ;POT BOX
    (if (and (= WP "WP")(= KT "KB")(= LM "M")(= OP2 "W"))     (setq PFX "POM")) ;POM BOX 
    
  ;特注BOXの場合
    (if (= KT "TB") (setq PFX "KD"))                           ;箱体の型番"特注BOX=KD"
  (princ "\n型式=")(princ PFX)(princ "\n")
  (if (= PFX nil)(alert "この箱体は描けません！"))
;  (princ "\n型式=")(princ PFX)(princ "\n")

;;箱体の型式番号を合成
   (if (= KT "KB") (setq PZ (list (- (car PC) 100)(- (cadr PC) (+ BH 250)))))
   (if (= KT "TB") (setq PZ (list (car PC)(- (cadr PC) (+ BH 350)))))
   (setq B1 (substr (rtos BW) 1 2))
   (setq B2 (substr (rtos BH) 1 2))
   (setq B3 (substr (rtos BD) 1 2))
   
   (if (= BC "C2") (setq COL "2.5Y9/1"));2.5Y9/1
   (if (= BC "C5") (setq COL "5Y7/1"))  ;5Y7/1
   (if (= BC "C9") (setq COL IR))       ;指定色
   
      ;;(setq BKATA (strcat PFX " " B2 B1 "-" B3))
      ;;(if (and (= PFX "PO")(>= BH 1700))(setq PFX (strcat PFX "T")));POのH=1700以上の時POTにする
         
         (setq BKATA (strcat PFX B2 B1 "-" B3))
         
         (if (or (= PFX "FX")(= PFX "FXU"))
             (progn
                 (setq BKATA (strcat PFX B2 B1 "-" B3))
                 (if (= BH 1350) (setq BKATA (strcat BKATA "A")))
                 (if (and (< BW 750) (= DN "D2")) (setq BKATA (strcat BKATA "片扉のみ")))
                 (if (and (= BW 750) (= DN "D1")) (setq BKATA (strcat BKATA "両扉のみ")))
                 (if (and (= BW 800) (= DN "D1")) (setq BKATA (strcat BKATA "S")))
                 (if (and (> BW 800) (= DN "D1")) (setq BKATA (strcat BKATA "両扉のみ")))
             )
         )
         
         (if (or (= PFX "RX")(= PFX "RXM"))
             (progn
                 (setq BKATA (strcat PFX B2 B1 "-" B3))
                 (if (and (< BW 700) (= DN "D2")) (setq BKATA (strcat BKATA "片扉のみ")))
                 (if (and (= BW 700) (= DN "D2")) (setq BKATA (strcat BKATA "W")))
                 (if (and (= BW 800) (= DN "D1")) (setq BKATA (strcat BKATA "S")))
                 (if (and (> BW 800) (= DN "D1")) (setq BKATA (strcat BKATA "両扉のみ")))
             )
         )
         
         (if (or (= PFX "PO")(= PFX "POT")(= PFX "POM"))
             (progn
                 (setq B2 (substr (rtos (+ BH 30)) 1 2))
                 (setq BKATA (strcat PFX B2 B1 "-" B3))
                 (if (>= BH 1000.00) (setq BKATA (strcat BKATA "A")))
                 (if (and (< BW 700) (= DN "D2")) (setq BKATA (strcat BKATA "片扉のみ")))
                 (if (and (= BW 700) (= DN "D2")) (setq BKATA (strcat BKATA "W")))
                 (if (and (= BW 800) (= DN "D1")) (setq BKATA (strcat BKATA "S")))
                 (if (and (> BW 800) (= DN "D1")) (setq BKATA (strcat BKATA "両扉のみ")))
                 
             )
         )
         
         ;;(princ "\nBKATA=")(princ BKATA)
         
         (if (= BC "C2") (setq BKATA (strcat BKATA "K"))) ;2.5Y9/1の場合
         (command "COLOR" 2)
         
;         (if (= KT "KB") ;;既製BOXの場合
;             (progn
;                 (setq Bkflg "")
;                 (Bkensaku) ;;BCBで箱体の型式番号が台帳に存在するか調べる
;                 (setq PZ (list (+ (car PZ) 100) (- (cadr PZ) 150)))
;                 (if (= Bkflg "1") (command "TEXT" "S" "M0" "C" PZ "30" "0" "台帳に該当 "))
;                 (if (= Bkflg "0") (command "TEXT" "S" "M0" "C" PZ "30" "0" "台帳に非該当"))
;             )
;         )
;         (if (and (= KT "KB")(= Bkflg "0"))(progn (setq BKIKI "BOX-O")(setq BMEIKA "39")))
;         (if (and (= KT "KB")(= Bkflg "1"))(progn (setq BKIKI "KB")   (setq BMEIKA "39")))
         (if (= KT "TB")(progn (setq BKIKI "BB")(setq BMEIKA "100")))
         (setq PZ (list (- (car PZ) 100) (+ (cadr PZ) 150)))
         ;;(command "TEXT" "S" "M0" "C" PZ "30" "0" BKATA)
         (command "INSERT" "ZOKU" PZ "1" "1" "0" BKIKI "" BKATA "" "" "" "" "" "" "" "" BMEIKA "1" "" "")
         ;*(princ "\nPZ=")(princ PZ)*(princ "\nBMEIKA=")(princ BMEIKA)
         ;*(princ "\nBKIKI=")(princ BKIKI);*(princ "\nBKATA=")(princ BKATA)

;;既製ﾁｬﾝﾈﾙﾍﾞｰｽ 型式番号の合成と型番ﾁｪｯｸ
   (if (and (= KT "KB")(= KJ "JJ")) ;既製自立の場合
       (progn;1
          (setq BD1 (rtos BD)) (setq BW1 (rtos BW))
          (if (or
                 (and (= BD1 "120")
                      (or (= BW1 "400")(= BW1 "450")(= BW1 "500")
                          (= BW1 "600")(= BW1 "800")(= BW1 "900")))
                 (and (= BD1 "140")
                      (or (= BW1 "400")(= BW1 "450")(= BW1 "500")(= BW1 "600")
                          (= BW1 "700")(= BW1 "800")(= BW1 "900")))
                 (and (= BD1 "160")
                      (or (= BW1 "400")(= BW1 "450")(= BW1 "500")(= BW1 "600")
                          (= BW1 "700")(= BW1 "800")(= BW1 "900")
                          (= BW1 "1000")(= BW1 "1200")))
                 (and (= BD1 "180")
                      (or (= BW1 "500")(= BW1 "1000")))
                 (and (= BD1 "200")
                      (or (= BW1 "400")(= BW1 "450")(= BW1 "500")(= BW1 "600")
                          (= BW1 "700")(= BW1 "800")(= BW1 "900")
                          (= BW1 "1000")(= BW1 "1200")(= BW1 "1400")))
                 (and (= BD1 "250")
                      (or (= BW1 "500")(= BW1 "600")(= BW1 "700")
                          (= BW1 "800")(= BW1 "1000")(= BW1 "1200")))
                 (and (= BD1 "300")
                      (or (= BW1 "500")(= BW1 "600")(= BW1 "700")
                          (= BW1 "800")(= BW1 "1000")(= BW1 "1200")))
                 (and (= BD1 "350")( BW1 "700"))
              );or
              (progn;2
                 (princ "\nﾁｬﾝﾈﾙ OK")
                 ;;(setq PZ1 (list (car PC) (- (cadr PC) (+ BH 300))))
                 (setq PZ1 (list (- (car PC) 100) (- (cadr PC) (+ BH 300))))
                 (setq B1C (rtos (/ BW 10)))
                 (setq B2C "10")
                 (setq B3C (rtos (/ BD 10)))
                 (if (= (strlen B1C) 3) (setq B1C (substr B1C 1 2)))
                 (if (= (strlen B2C) 1) (setq B2C (strcat B2C "0")))
                 
                 ;;(setq CKATA (strcat "TE" B2C B1C "-" B3C "N"))
                 (if (= BC "C2") (setq CKATA (strcat "TE" B2C B1C "-" B3C "K")));2.5Y9/1ではKが付く
                 (if (/= BC "C2") (setq CKATA (strcat "TE" B2C B1C "-" B3C)));それ以外は何も付かない
                                  
                 ;;(command "TEXT" "S" "M0" "C" PZ1 "30" "0" CKATA)
                 (command "INSERT" "ZOKU" PZ1 "1" "1" "0" BKIKI "" CKATA 
                                                          "" "" "" "" "" "" "" "" BMEIKA "1" "" "")
;                (princ "CKATA=")(princ CKATA)
              );2
              (progn;3 else
                 (alert "既製ﾁｬﾝﾈﾙﾍﾞｰｽには そのサイズは ありません！")
                 (princ "\n終了します。もう一度やり直してください！！ CH")
                 (exit)
              );3
          );if
       );1
   );if

;KEY・IP・鉄板の厚みの設定
    (if (and (or (= PFX "FX")(= PFX "FXU")) (> BH 900))
        (progn (setq KEY  "DKH117FF039")                       ;KH-117F
               (setq KEYL "DKH117FFL039")                      ;2枚扉の左key
               (setq IP "IP2XD")                               ;IPの設定
               (setq Bt "鋼鈑1.6")                                 ;箱の鉄板
               (setq Dt "鋼鈑1.6")                                 ;扉の鉄板
        )
    )
    (if (and (or (= PFX "FX")(= PFX "FXU")) (<= BH 900))
        (progn (setq KEY  "DKH84FF039R")                       ;KH-84F
               (setq KEYL "DKH84FF039L")                       ;2枚扉の左key
               (setq IP "IP2XD")                               ;IPの設定
               (setq Bt "鋼鈑1.6")                                 ;箱の鉄板
               (setq Dt "鋼鈑1.6")                                 ;扉の鉄板
        )
    )
    (if (or (= PFX "RX")(= PFX "RXM")(= PFX "PO")(= PFX "POT")(= PFX "POM"))
        (progn (setq KEY  "DKH110AF039R")                      ;KH-110A
               (setq KEYL "DKH110AF039L")                      ;2枚扉の左key
               (setq IP "IP44")                                ;IPの設定
               (setq Bt "鋼鈑1.6")                                 ;箱の鉄板
               (setq Dt "鋼鈑1.6")                                 ;扉の鉄板
        )
    )
    (if (and (= PFX "RX")(< BH 800)(= DN "D1"))
             (setq IP "IP54")                                  ;IPの設定
    )
    (if (and (or (= PFX "PO")(= PFX "POT"))(>= BH 1700))
        (progn (setq Bt "鋼鈑2.3")                                 ;箱の鉄板
               (setq Dt "鋼鈑2.3")                                 ;扉の鉄板
        )
    )
    (if (and (= PFX "KD")(= KJ "KK")(<= BH 1200))
        (progn (setq KEY  "DA4643S034R")                       ;A-464-3-1
               (setq KEYL "DA4643S034L")                       ;2枚扉の左key
               (setq KNAM "A-464-3-1")                         ;keyの形式
               (setq KNO  "KEY(No.0200)付")                    ;key番号
               (setq IP "")                                    ;IPの設定
               (setq Bt "鋼鈑1.6")                                 ;箱の鉄板
               (setq Dt "鋼鈑1.6")                                 ;扉の鉄板
        )
    )
    (if (and (= PFX "KD")(= KJ "KK")(> BH 1200))
        (progn (setq KEY  "DA4642S034R")                       ;A-464-2-1
               (setq KEYL "DA4642S034L")                       ;2枚扉の左key
               (setq KNAM "A-464-2-1")                         ;keyの形式
               (setq KNO  "KEY(No.0200)付")                    ;key番号
               (setq IP "")                                    ;IPの設定
               (setq Bt "鋼鈑1.6")                                 ;箱の鉄板
               (setq Dt "鋼鈑1.6")                                 ;扉の鉄板
        )
    )
    (if (and (= PFX "KD")(= KJ "JJ"))
        (progn (setq KEY  "DA4642S034R")                       ;A-464-2-1
               (setq KEYL "DA4642S034L")                       ;2枚扉の左key
               (setq KNAM "A-464-2-1")                         ;keyの形式
               (setq KNO  "KEY(No.0200)付")                    ;key番号
               (setq IP "")                                    ;IPの設定
               (setq Bt "鋼鈑2.3")                                 ;箱の鉄板
               (setq Dt "鋼鈑2.3")                                 ;扉の鉄板
        )
    )
    (if (>= BH 1700.00)(setq Dt "鋼鈑2.3"))                        ;高さ1700以上の時は扉だけ2.3
;;-----------------------------------------------------------------------------
;箱体の外枠と寸法
    (setq LU1 (list (- (car PC) (/ BW 2)) (cadr PC)))        ;左上端
    ;(princ "\nLU1=")(princ LU1)
    (setq RU1 (list (+ (car PC) (/ BW 2)) (cadr PC)))        ;右上端
    ;(princ "\nRU1=")(princ RU1)
    (setq LD1 (list (- (car PC) (/ BW 2)) (- (cadr PC) BH))) ;左下端
    ;(princ "\nLD1=")(princ LD1)
    (setq RD1 (list (+ (car PC) (/ BW 2)) (- (cadr PC) BH))) ;右下端
    ;(princ "\nRD1=")(princ RD1)
    (if (= WP "WP")(setq DIMH1 200)(setq DIMH1 150))         ;PDIM  屋根付きの場合の寸法位置
    (if (= WP "WP")(setq DIMH2 150)(setq DIMH2 100))         ;PDIM1 屋根付きの場合の寸法位置
    ;(princ "\nDIMH1=")(princ DIMH1)
    ;(princ "\nDIMH2=")(princ DIMH2)
    (setq PDIM (list (car PC) (+ (cadr PC) DIMH1))) ;(princ "\nPDIM=") (princ PDIM) ;寸法線のY位置
    (setq PDIM1 (list (car PC) (+ (cadr PC) DIMH2)));(princ "\nPDIM1=")(princ PDIM1);寸法線のY位置1
    (setq PDIM2 (list (car LD1) (- (cadr LD1) 50))) ;(princ "\nPDIM2=")(princ PDIM2);寸法線のY位置2
    (setq PDIM3 (list (car LD1) (- (cadr LD1) 175)));(princ "\nPDIM3=")(princ PDIM3);寸法線のY位置3
    (setq PDIM4 (list (car PC) (- (cadr PC) 100)))  ;(princ "\nPDIM4=")(princ PDIM4);寸法線のY位置4
    (setq SDIM (list (- (car LU1) 175) (cadr LU1))) ;(princ "\nSDIM=") (princ SDIM) ;寸法線のX位置
    (setq SDIM1 (list (- (car LU1) 100) (cadr LU1)));(princ "\nSDIM1=")(princ SDIM1);寸法線のX位置1
    (setq PC1 (list (car PC) (- (cadr PC) 20)))     ;(princ "\nPC1=")(princ PC1)    ;OFFSET値:20
    (setq PC2 (list (car PC) (- (cadr PC) 50)))     ;(princ "\nPC2=")(princ PC2)    ;NPの挿入点
    (setq PC3 (list (car LU1)(- (cadr LU1)(/ BH 2))));(princ "\nPC3=")(princ PC3)   ;KEYの挿入点
    (setq PC5 (list (car PC) (+ (cadr LD1) 150)))   ;(princ "\nPC5=")(princ PC5)    ;CHの挿入点
    (setq PC6 (list (car PC) (- (cadr PC) 5)))      ;(princ "\nPC6=")(princ PC6) ;変更後OFFSET値:5
    (setq PC7 (list (car PC) (+ (cadr PC) 15)))     ;(princ "\nPC7=")(princ PC7) ;OFFSET値:15  埋込枠
    (setq PC8 (list (car PC) (- (cadr PC) 20)))     ;(princ "\nPC8=")(princ PC8) ;OFFSET値:-20 扉
    (setq PC4 (list (car PC) (- (cadr PC) (/ BH 4))))        ;下記以外のNP-Jの挿入点
    (if (and (= OP2 "W")(= KT "KB"))
        (setq PC4 (list (car PC) (- (- (cadr PC) (/ BH 4)) 100)));既製BOX 窓付きのNP-Jの挿入点
    )
    (if (and (= OP2 "W")(= KT "TB"))
        (setq PC4 (list (car PC) (- (- (cadr PC) (/ BH 4)) 200)));特注BOX 窓付きのNP-Jの挿入点
    )
    ;*(princ "\nPC4=")(princ PC4)
    (command "COLOR" 4)
    (command "RECTANGLE" "F" 0 "W" 0 LU1 RD1)                            ;箱体の外枠
;;---------------------------------------------------------------------------------------------------
        (if (= LM "M");突合せの場合
            (progn
               (if (= KT "KB")(setq FRH 30));FRH=屋根の高さ
               (if (= KT "TB")(setq FRH 50))
               (setq LSU (list (car LU1) (+ (cadr LU1) FRH)))
               (command "OFFSET" "T" PC PC6 "");扉
               ;(princ "\nRU1=")(princ RU1)(princ "\nLSU=")(princ LSU)
               (if (= WP "WP")                                ;屋根付きの場合
                   (progn
                      (command "RECTANGLE" "F" 0 "W" 0  RU1 LSU)   ;屋根
                      (command "COLOR" 2)
                      (command "DIM" "VER" LU1 LSU SDIM1 "" "EXIT") ;屋根の高さ寸法
                      (if (= KJ "KK")(command "DIM" "VER" LSU LD1 SDIM "" "EXIT"))
                      (if (= KT "KB")
                          (progn
                            (command "DIM" "VER" LU1 LSU SDIM1 "" "EXIT");屋根の高さ寸法
                            (command "COLOR" 4)
                            (if (and (= KJ "KK")(< BW 600))
                                     (command "INSERT" "MIZUN-16" LD1 "1" "1" "0")
                            )
                            (if (and (= KJ "KK")(>= BW 600))
                                     (command "INSERT" "MIZUN-25" LD1 "1" "1" "0");水抜
                            )
                            (if (= OP2 "W")
                                (setq PC2 (list (car PC) (- (cadr PC) 10)));NPの挿入点 窓付き
                                (setq PC2 (list (car PC) (- (cadr PC) 50)));NPの挿入点 else
                            )
                          )
                      )
                   );progn
               );if
            (if (= DN "D1")(command "INSERT" "*NP2" PC2 "1" "0"))                ;NP挿入
            );progn
        );if
        (if (and (= LM "L")(= OP1 "U"));落込で埋込の場合
            (progn           ; 埋込BOX
               (setq ULU1 (list (- (car LU1) 15) (+ (cadr LU1) 15)))
               (setq URU1 (list (+ (car RU1) 15) (+ (cadr RU1) 15)))
               (setq ULD1 (list (- (car LD1) 15) (- (cadr LD1) 15)))
           ;(princ "\nULU1=")(princ ULU1)(princ "\nURU1=")(princ URU1)(princ "\nULD1=")(princ ULD1)
               (command "OFFSET" "T" PC PC7 "");埋込枠
               (command "OFFSET" "T" PC PC8 "");扉
               (command "CHANGE" "C" PC PC6 "" "P" "LT" "DASHED" "")
               (command "COLOR" 2)                                  ;黄色
               (command "DIM" "HOR" URU1 ULU1 PDIM ""
                              "HOR" LU1  ULU1 PDIM1 ""
                              "HOR" RU1  URU1 PDIM1 ""
                              "VER" ULU1 ULD1 SDIM ""
                              "VER" LU1  ULU1  SDIM1 "" 
                              "VER" LD1  ULD1  SDIM1 "" "EXIT")
               (command "COLOR" 4)                                  ;水色
               (setq PDIM PDIM1);
               (if (= DN "D1")(command "INSERT" "*NP2" PC2 "1" "0"))     ;NP挿入
            )
            (if (and (= LM "L")(= WP "WN"))
                (progn       ;else 条件  落込BOX
                   (command "OFFSET" "T" PC PC1 "");扉
                   ;*(command "PEDIT" PC "W" "4" "") ;*太線を表示させない
                )
            )
        )
        (if (= DN "D2");２枚扉
            (progn
               (if (= LM "L") (setq WS 20));２枚扉縦線の上下基点
               (if (= LM "M") (setq WS 5))
               (setq PCCU (list (car PC)(- (cadr PC) WS)))
               (setq PCCD (list (car PC)(- (cadr PC) (- BH WS))))
               (setq D2T (/ BW 2))
               (setq D4T (/ BW 4))
               (if (= OP2 "W")
                   (setq PC2 (list (- (car PC) D4T) (- (cadr PC) 10)));NPの挿入点 2枚扉で窓付き
                   (setq PC2 (list (- (car PC) D4T) (- (cadr PC) 50)));NPの挿入点
               )
               (setq PC3 (list (+ (car LU1) D2T) (- (cadr LU1) (/ BH 2))))  ;KEY
               (setq PC4 (list (+ (car PC) D4T) (- (cadr PC) (/ BH 4))))    ;NP-J
               (setq PC5 (list (+ (car PC) D4T) (+ (cadr LD1) 150)))        ;CH
               (command "PLINE" PCCU PCCD "")
               ;(princ "\nPCCU=")(princ PCCU)(princ "\nPCCD=")(princ PCCD)
               (command "INSERT" "*NP2" PC2 "1" "0")                        ;NP挿入
               (if (= KT "KB")
                   (command "INSERT" KEYL PC3 "1" "1" "0")                      ;KEY左側挿入 既製
                   (command "INSERT" KEYL PC3 "1" "1" "0" 
                   "BH" "KEY-L" KNAM KNO "" "" "" "" "" "" "" "34" "1" "" "");特注
               )
            )
        )
        (if (= KT "KB")
            (command "INSERT" KEY PC3 "1" "1" "0")                      ;KEY挿入 既製
            (command "INSERT" KEY PC3 "1" "1" "0" "BH" "KEY" KNAM KNO 
                                                       "" "" "" "" "" "" "" "34" "1" "" "");特注
        )
        (command "INSERT" "*NP2" PC2 "1" "0")                   ;NP挿入
        (command "INSERT" "NP-J" PC4 "1" "1" "0")               ;NP-J挿入
        
        ;;カードホルダー(CH)の挿入
        (if (and (= KT "KB")(= CH "HYO"))(progn (setq CHSYM "CHKA6S")
                                                (setq CHKATA "C-26-C-A6")
                                                (setq CHTEI "A6")(setq CHMEIKA 34))
        )
        (if (and (= KT "KB")(= CH "A4")) (progn (setq CHSYM "CHKA4")
                                                (setq CHKATA "DHP-A4")(setq CHTEI "")
                                                (setq CHMEIKA 36))
        )
        (if (and (= KT "TB")(= CH "HYO"))(progn (setq CHSYM "CHA4")
                                                (setq CHKATA "CHA4")(setq CHTEI "")
                                                (setq CHMEIKA 100));;変更分 シンボルCHをCH4へ
        )
        (if (and (= KT "TB")(= CH "A4"))(progn (setq CHSYM "CHA4")
                                                (setq CHKATA "CHA4")(setq CHTEI "")
                                                (setq CHMEIKA 100))
        )
        (setq PC5 (list (car PC5) (- (cadr PC5) 80))) ;;底より50高い位置にする
        (command "INSERT" CHSYM PC5 "1" "1" "0" "CH" "" CHKATA "" 
                                                        CHTEI "" "" "" "" "" "" CHMEIKA "1" "" "")
        (princ "\nCH=")(princ CH)
        
        ;;KDロゴシールの追加
        (setq MENSEKI (* BH BW))
        ;;(princ "\nMENSEKI=")(princ MENSEKI)
        (if (and (= LOGO "Y")(>= MENSEKI 500000))
            (progn
                (setq PCLGX1 (+ (car PC)(/ BW 2)))
                (setq PCLGY1 (cadr PC))
                                
                (if (= LM "L") (progn (setq PCLGX1 (- PCLGX1 30)) (setq PCLGY1 (- PCLGY1 30)) ))
                (if (= LM "M") (progn (setq PCLGX1 (- PCLGX1 13)) (setq PCLGY1 (- PCLGY1 13)) ))
                
                ;(princ "\nPCLGX1=")(princ PCLGX1)
                ;(princ "\nPCLGY1=")(princ PCLGY1)
                
                (setq PCLG (list PCLGX1 PCLGY1))
                ;(princ "\nPCLG=")(princ PCLG)
                (command "INSERT" "KD-LOGO" PCLG "1" "1" "0")
            )
        )
        ;*(princ "\nLU1@1=")(princ LU1);*(princ "\nRU1@1=")(princ RD1)
        ;*(princ "\nLD1@1=")(princ LD1);*(princ "\nRD1@1=")(princ RD1)
        (setq ET1 (list (- (car RD1) 50) (+ (cadr RD1) 50)))    ;アース端子シンボルを挿入する位置
        (setq ET2 (list (+ (car LD1) 50) (+ (cadr RD1) 50)))    ;上記、２枚扉の場合
        (setq ZOOM1L (list (- (car ET1) 50) (+ (cadr ET1) 50))) ;再作図用ズーム範囲 右上
        (setq ZOOM1R (list (+ (car ET1) 50) (- (cadr ET1) 50))) ;再作図用ズーム範囲 左下
        (setq ZOOM2L (list (- (car ET2) 50) (+ (cadr ET2) 50))) ;再作図用ズーム範囲 右上
        (setq ZOOM2R (list (+ (car ET2) 50) (- (cadr ET2) 50))) ;再作図用ズーム範囲 左下

        (command "INSERT" "*M5ET4" ET1 "1" "0")            ;点線尺度を先に変更してあるシンボルを挿入
        ;(princ "\n@ET1=")(princ ET1)

        (if (= DN "D2")                                    ;2枚扉を選択
            (progn
                (command "INSERT" "*M5ET4" ET2 "1" "0")    ;2枚扉を選択した時挿入するｱ-ｽ端子ｼﾝﾎﾞﾙ
            );progn
        );if
        (command "COLOR" 2)                                     ;黄色
        (command "DIM" "HOR" RU1 LU1 PDIM ""
                       "VER" LU1 LD1 SDIM1 """EXIT")
        (command "COLOR" 4)                                     ;水色
        ;(command REGEN)
;;-----------------------------------------------------------------------------
;正面チャンネル図
    (if (and (= KT "KB")(= KJ "JJ"));既製BOXで自立の場合
        (progn
           (setq HH 100)                                       ;既製BOXの場合のﾁｬﾝﾈﾙ高さ
           (setq LCD (list (car LD1) (- (cadr LD1) HH)))       ;ﾁｬﾝﾈﾙ左下
           (setq RCD (list (car RD1) (- (cadr RD1) HH)))       ;ﾁｬﾝﾈﾙ右下
           (setq CCL (list (+ (car LCD) 30) (+ (cadr LCD) 30)));ﾁｬﾝﾈﾙｱﾝｶｰ穴L
           (setq CCR (list (- (car RCD) 30) (+ (cadr RCD) 30)));ﾁｬﾝﾈﾙｱﾝｶｰ穴R
           (setq CAH (list (+ (car LCD) 30) (cadr LCD)))       ;ﾁｬﾝﾈﾙｱﾝｶｰ穴の寸法ｼﾝﾎﾞﾙ挿入点
           ;(princ "\nLCD=")(princ LCD);(princ "\nRCD=")(princ RCD)
           ;(princ "\nCCL=")(princ CCL);(princ "\nCCR=")(princ CCR);(princ "\nCAH=")(princ CAH)
           (command "RECTANGLE" "F" 0 "W" 0  LD1 RCD)          ;ﾁｬﾝﾈﾙの描画
           (command "INSERT" "4-16" CAH "1" "1" "0")           ;ﾁｬﾝﾈﾙｱﾝｶｰ穴の寸法ｼﾝﾎﾞﾙ挿入
           (command "COLOR" 2);黄色
           (command "DIM" "HOR" CCL LCD PDIM3 ""
                          "HOR" CCL CCR PDIM3 ""
                          "HOR" CCR RCD PDIM3 ""
                          "VER" LD1 LCD SDIM1 "" "EXIT");ﾁｬﾝﾈﾙの高さ
           ;(princ "\nPDIM3=")(princ PDIM3)
           ;(princ "\nSDIM1=")(princ SDIM1) 
        );progn
    )
    (if (and (= KT "KB")(= KJ "JJ")(= WP "WP"));既製BOXで自立・屋根付きの場合
        (progn
           (setq HH 100)
           (setq LCD (list (car LD1) (- (cadr LD1) HH)))       ;ﾁｬﾝﾈﾙ左下
           (setq LSU (list (car LU1) (+ (cadr LU1) FRH)))
           (command "DIM" "VER" LSU LCD SDIM "" "EXIT")        ;全体の高さ
        )
        (if (and (= KT "KB")(= KJ "JJ")(= WP "WN"))
            (progn 
              (setq LCD (list (car LD1) (- (cadr LD1) HH)))       ;ﾁｬﾝﾈﾙ左下
              (command "DIM" "VER" LU1 LCD SDIM "" "EXIT")        ;全体の高さ else if
            )
        )
    );if
    (if (and (= KT "TB")(= KJ "JJ"));特注BOXで自立の場合
        (progn
           (setq HH 50)
           (setq LCD (list (car LD1) (- (cadr LD1) HH)))                  ;ﾁｬﾝﾈﾙ左下
           (setq PDY  (list (car PC)(+ (cadr PC) 8000)))                  ;ﾁｬﾝﾈﾙの仮挿入点
           (setq PDYL (list (- (car PDY) 1000)(- (cadr PDY) 400)))        ;ｽﾄﾚｯﾁ左ﾌｪﾝｽ点
           (setq PDYR (list (+ (car PDY) 1000)(- (cadr PDY) 400)))        ;ｽﾄﾚｯﾁ右ﾌｪﾝｽ点
           (setq STL  (/ (- BW 400.00) 2))                                ;ｽﾄﾚｯﾁ距離 400=BLOCKの幅
           (setq DSTR (list (+ (car PDY) STL)(cadr PDY)))                 ;ｽﾄﾚｯﾁ目標点
           (setq DSTL (list (- (car PDY) STL)(cadr PDY)))                 ;ｽﾄﾚｯﾁ目標点
           (setq LD11 (list (+ (car LD1) 5) (cadr LD1)))       ;変更 寸法線 ﾁｬﾝﾈﾙと箱体隙間左
           (setq RD11 (list (- (car RD1) 5) (cadr RD1)))       ;変更 寸法線 ﾁｬﾝﾈﾙと箱体隙間右
           (setq 5DIML (list (car LD1) (- (cadr LD11) 20)))          ;変更 寸法線 箱体とﾁｬﾝﾈﾙ間
           ;(princ "\n5DIML=")(princ 5DIML)
           (setq 5DIMR (list (car RD1) (- (cadr RD11) 20)))          ;変更 寸法線 箱体とﾁｬﾝﾈﾙ間
           ;(princ "\n5DIML=")(princ 5DIML)
           ;(princ "\nLCD=")(princ LCD)
           ;(princ "\nPDY=")(princ PDY)
           ;(princ "\nPDYL=")(princ PDYL)
           ;(princ "\nPDYR=")(princ PDYR)
           ;(princ "\nSTL=")(princ STL)
           ;(princ "\nDSTR=")(princ DSTR)
           ;(princ "\nDSTL=")(princ DSTL)
           ;(princ "\nWP=")(princ WP)
           ;(command "DIM" "VER" LD1 LD11 5DIML "" "EXIT")
           (if (= WP "WN");屋根無しの場合
               (progn
                  (setq CHAN "*CHAN-IN")
                  (setq LCD (list (car LD1) (- (cadr LD1) HH))) ;ﾁｬﾝﾈﾙ左下
                  ;(princ "\nLCD=")(princ LCD)
                  (command "DIM" "VER" LU1 LCD SDIM "" "EXIT")  ;全体の高さ
                  ;(setq RINL (list (+ (car LU1) 50)(cadr LU1)));変更前 吊り環の挿入 L
                  ;(setq RINR (list (- (car RU1) 50)(cadr RU1)));変更前 吊り環の挿入 R
                  (setq RINL (list (+ (car LU1) 25)(cadr LU1))) ;変更後 吊り環の挿入 L 左へ25移動
                  (setq RINR (list (- (car RU1) 25)(cadr RU1))) ;変更後 吊り環の挿入 R 右へ25移動
                  ;(princ "\nRINL=")(princ RINL)
                  ;(princ "\nRINR=")(princ RINR)
                  (command "INSERT" "RING-S2" RINL "1" "1" "0") ;吊り環の挿入
                  (command "INSERT" "RING-S2" RINR "1" "1" "0")
               )
           )
           (if (= WP "WP");屋根付きの場合
               (progn
                  ;*(princ "\nDSTR=")(princ DSTR)
                  (setq HH 100)
                  (setq CHAN "*CHAN-WP")
                  (setq LCD (list (car LD1) (- (cadr LD1) HH)))    ;ﾁｬﾝﾈﾙ左下
                  (setq LSU (list (car LU1) (+ (cadr LU1) FRH)))
                  (setq PDY1 (list (+ (car PDY) 25) (cadr PDY)))   ;ﾁｬﾝﾈﾙをｽﾄﾚｯﾁする基点 右側
                  (setq PDY2 (list (- (car PDY) 25) (cadr PDY)))   ;ﾁｬﾝﾈﾙをｽﾄﾚｯﾁする基点 左側
                  (setq DSTRR (list (+ (car DSTR) 25) (cadr DSTR)));ﾁｬﾝﾈﾙをｽﾄﾚｯﾁする範囲 右側                  
                  (setq DSTLL (list (- (car DSTL) 25) (cadr DSTL)));ﾁｬﾝﾈﾙをｽﾄﾚｯﾁする範囲 左側

                  ;(princ "\nPDY1@=")(princ PDY1)
                  ;(princ "\nPDY2@=")(princ PDY2)
                  ;(princ "\nDSTRR@=")(princ DSTRR)
                  ;(princ "\nDSTLL@=")(princ DSTLL)
                  ;(princ "\nLCD@=")(princ LCD)
                  ;(princ "\nLSU@=")(princ LSU)
                  (command "DIM" "VER" LSU LCD SDIM "" "EXIT")   ;全体の高さ
                  ;(setq RINL (list (+ (car LU1) 50)(cadr LSU))) ;変更前 吊り環の挿入 L
                  ;(setq RINR (list (- (car RU1) 50)(cadr LSU))) ;変更前 吊り環の挿入 R
                  (setq RINL (list (+ (car LU1) 25)(cadr LSU)))  ;変更後 吊り環の挿入 L 左へ25移動
                  (setq RINR (list (- (car RU1) 25)(cadr LSU)))  ;変更後 吊り環の挿入 R 右へ25移動
                  ;(princ "\nRINL@=")(princ RINL)
                  ;(princ "\nRINR@=")(princ RINR)
                  (command "INSERT" "RING-S2" RINL "1" "1" "0")       ;吊り環の挿入 左
                  (command "INSERT" "RING-S2" RINR "1" "1" "0")       ;吊り環の挿入 右
                  (command "INSERT" CHAN PDY "1" "0")                 ;ﾁｬﾝﾈﾙの挿入
                  (command "STRETCH" "C" PDY1 PDYR "" PDY1 DSTRR)     ;変更後 ﾁｬﾝﾈﾙ右側ｽﾄﾚｯﾁ
                  (command "STRETCH" "C" PDY2 PDYL "" PDY2 DSTLL)     ;変更後 ﾁｬﾝﾈﾙ左側ｽﾄﾚｯﾁ
                  (command "MOVE" "C" (list (car PDYL)(cadr PDY))
                                            PDYR "" PDY (list (car PDY)(cadr LD1)))  ;図形移動
               );progn
           );if
           (if (/= WP "WP")
               (progn
                   (command "INSERT" CHAN PDY "1" "0")
                   (setq PDY1 (list (+ (car PDY) 25)(cadr PDY)))
                   (setq PDY2 (list (- (car PDY) 25)(cadr PDY)))
                   (setq DSTRR (list (+ (car DSTR) 25)(cadr DSTR)))
                   (setq DSTLL (list (- (car DSTL) 25)(cadr DSTL)))
                   (command "STRETCH" "C" PDY1 PDYR "" PDY1 DSTRR)    ;変更後 ストレッチR
                   (command "STRETCH" "C" PDY2 PDYL "" PDY2 DSTLL)    ;変更後 ストレッチL
                   (command "MOVE" "C" (list (car PDYL)(cadr PDY)) 
                                              PDYR "" PDY (list (car PDY)(cadr LD1)))  ;図形移動
               );progn
           );if
           (if (= WP "WP")
               (progn
                  (command "COLOR" 2)
                  (command "DIM" "HOR" LD11 LD1 5DIML "" "EXIT")   ;変更 ﾁｬﾝﾈﾙと箱体間 寸法線 左
                  (command "DIM" "HOR" RD11 RD1 5DIMR "" "EXIT")   ;変更 ﾁｬﾝﾈﾙと箱体間 寸法線 右
                  (command "COLOR" 4)
               );progn
           );if
        );progn
    );if
    (command "COLOR" 4)
;------------------------------------------------------------------------------
;正面窓付き
    (if (and (= LM "M")(= OP2 "W"));突合せ and 窓付きの場合
        (progn
           (if (= KT "KB");既製BOXの窓
               (progn 
                  (setq FDO 35) (setq FDI 25) (setq OFFS 20)
                  (if (>= BW 700) ;;窓の横幅
                      (setq WW (- BW 174));盤幅が700以上の窓の横幅
                      (setq WW (- BW 154));盤幅が700未満の窓の横幅 
                  )
                  (if (and (< BW 600) (= BH 500)) (setq WH 136) ;;窓の高さ
                      (if (or (and (= BW 600)(= BH 500)) (and (= BW 500)(<= BH 900)))
                          (setq WH 176) ;then
                          (setq WH 216) ;else
                      )
                  )
                  (if (and (< BW 600) (= BH 500)) (setq WC 165) ;;PC～窓中心までの距離
                      (if (or (and (= BW 600)(= BH 500)) (and (= BW 500)(<= BH 900)))
                          (setq WC 165) ;then
                          (setq WC 175) ;else
                      )
                  )
               );progn
           )
           (if (= KT "TB");特注BOXの窓
               (progn
                  (setq FDO 60)(setq FDI 50)(setq OFFS 30)
                  (setq WW (- BW 250))
                  (setq WH 300)
                  (setq WC 300);窓の縦の位置
               )
           )
           (setq WPC (list (car PC)(- (cadr PC) WC)))
           (setq 2WPC (list (+ (car PC) 5)(- (cadr PC) WC)));2枚扉選択時に窓をHATCHさせる為の変数
           (setq WLU (list (- (car WPC)(/ WW 2))(+ (cadr WPC)(/ WH 2))))
           (setq WRD (list (+ (car WPC)(/ WW 2))(- (cadr WPC)(/ WH 2))))
           (setq WPC2 (list (- (car WLU) 20)(+ (cadr WLU) 20)))
           (setq WLU2 (list (+ (car WLU)(/ WW 2))(cadr WLU)))
           
           ;(princ "\nWPC=") (princ WPC) (princ "\nWLU=") (princ WLU) (princ "\nWRD=")(princ WRD)
           ;(princ "\nWPC2=")(princ WPC2)(princ "\nWLU2=")(princ WLU2)
           ;(princ "\nWW=")(princ WW)(princ "\nBW=")(princ BW)(princ "\nWC=")(princ WC)
           
               (command "RECTANGLE" "F" 50 "W" 15 WLU WRD)          ;窓枠の内側
               (command "COLOR" 2) ;黄色
               (command "TEXT" "S" "M0" "C" WPC "30" "0" "3t 強化ガラス")
               (command "COLOR" 4) ;水色
               (command "-BHATCH" "P" "ANSI31" "20" "0" WPC "")              ;窓枠をハッチングする
;      (exit)
           (if (= DN "D2")                         ;2枚扉を選択した場合
                   (progn
                      (command "-BHATCH" "P" "ANSI31" "20" "0" 2WPC "")      ;窓の右半分をハッチングさせる
                   );progn
           );if
        );progn
    );EndIf
;;=============================================================================
;;側面図－右側
        (setq LU2 (list (+ (car PC) (+ (/ BW 2) 500)) (cadr PC)));;左上端
        (setq RU2 (list (+ (car LU2) BD) (cadr PC)))             ;;右上端
        (setq LD2 (list (car LU2) (- (cadr LU2) BH)))            ;;左下端
        (setq RD2 (list (+ (car LU2) BD) (- (cadr LU2) BH)))     ;;右下端
        (setq LD22 (list (+ (car LD2) 5) (cadr LD2)))    ;;変更 側面図 寸法線 ﾁｬﾝﾈﾙと箱体隙間左
        (setq RD22 (list (- (car RD2) 5) (cadr RD2)))    ;;変更 側面図 寸法線 ﾁｬﾝﾈﾙと箱体隙間右
        ;(princ "\nLU2=")(princ LU2)(princ "\nRU2=")(princ RU2)
        ;(princ "\nLD2=")(princ LD2)(princ "\nRD2=")(princ RD2)
    ;中板の厚み
        (if (and (= KT "KB")(>= BH 1700.00))(setq ID 35)(setq ID 15))  ;既製BOXの場合
        (if (= KT "TB")(setq ID 20))                                   ;特注BOXの場合
;        (setq PDIM (list (car LU2) (+ (cadr LU2) 150)))
        (setq IUR (list (car RU2) (- (cadr RU2) 40)))           ;中板右上
        (setq IUL (list (- (car RU2) ID) (- (cadr RU2) 40)))    ;中板左上
        (setq IDR (list (car RD2) (+ (cadr RD2) 40)))           ;中板右下
        (setq IDL (list (- (car RD2) ID) (+ (cadr RD2) 40)))    ;中板左下
        ;(princ "\nIUR=")(princ IUR)(princ "\nIUL=")(princ IUL) 
        ;(princ "\nIDR=")(princ IDR)(princ "\nIDL=")(princ IDL)
            (command "COLOR" 2)                                 ;黄色
            (command "LINETYPE" "S" "DASHED" "")
            (command "PLINE" IUR IUL IDL IDR "")                ;中板の描画
            (command "LINETYPE" "S" "CONTINUOUS" "")
            (if (/= WP "WP")(command "DIM" "HOR"
                                     (list (+ (car LU2) BD) (cadr LU2)) LU2 PDIM "" "EXIT"));D寸法
            (command "COLOR" 4);水色
    (if (= LM "L");落込み
        (progn
           (command "RECTANGLE" "F" 0 "W" 0 LU2 RD2)
           ;*(command "PEDIT" LU2 "W" "4" "") ;*太線で表示させない為
          ;側面額縁の点線
           (setq PU1 (list (car LU2) (- (cadr LU2) 20)));額縁上
           (setq PU2 (list (+ (car LU2) 25) (cadr PU1)))
           (setq PU3 (list (car PU2) (- (cadr PU2) 25)))
           (setq PU4 (list (+ (car PU3) 20) (cadr PU3)))
           (setq PD1 (list (car LD2) (+ (cadr LD2) 20)));額縁下
           (setq PD2 (list (+ (car PD1) 25) (cadr PD1)))
           (setq PD3 (list (car PD2) (+ (cadr PD2) 25)))
           (setq PD4 (list (+ (car PD3) 20) (cadr PD3)))
              (command "COLOR" 2)                                  ;黄色
              (command "LINETYPE" "S" "DASHED" "")
              (command "PLINE" PU1 PU2 PU3 PU4 "")                 ;側面額縁上
              (command "PLINE" PD1 PD2 PD3 PD4 "")                 ;側面額縁下
              (command "LINETYPE" "S" "CONTINUOUS" "")
              (command "DIM" "HOR" 
                       (list (+ (car LU2) BD)(cadr LU2)) LU2 PDIM "" "EXIT")
        )
    )
    (if (= LM "M");突合せ
        (progn                                        ;DD1=扉の厚み DD2=扉～本体までの寸法
          (if (= KT "KB")                 (progn (setq DD1 20)(setq DD2 24)));既製BOX
          (if (and (= KT "TB")(= KJ "KK"))(progn (setq DD1 25)(setq DD2 30)));特注BOX 壁掛
          (if (and (= KT "TB")(= KJ "JJ"))(progn (setq DD1 35)(setq DD2 40)));特注BOX 自立
          (if (= PFX "POT")               (progn (setq DD1 30)(setq DD2 35)));POTの場合
          (setq LU2D (list (+ (car PC)(+ (/ BW 2) 500))(- (cadr PC) 5)))     ;扉左上端
          (setq LD2D (list (+ (car LU2) DD1) (- (cadr LU2D)(- BH 10))))      ;扉左下端
          (setq LD2B (list (+ (car LD2) DD2) (cadr LU2)))                    ;箱左下端
          ;(setq LD22 (list (+ (car LD2B) 5) (cadr LU2)))                     ;;;;;;;;;;
          (setq PDIM6 (list (car LD2) (+ (cadr LD2) 50)))                    ;扉寸法線のY位置2
            (command "RECTANGLE" "W" 0 "F" 0 LU2D LD2D)
            (command "RECTANGLE" "W" 0 "F" 0 LD2B RD2)
            (command "COLOR" 2) ;黄色
            (command "DIM" "HOR"(list (+ (car LD2) DD2)(cadr LD2)) LD2 PDIM6 "" "EXIT");扉の厚み寸法
              ;(princ "\nLU2D@M=")(princ LU2D)(princ "\nLD2D@M=")(princ LD2D)
              ;(princ "\nLD2B@M=")(princ LD2B)(princ "\nPDIM6@M=")(princ PDIM6)
              ;(princ "\nRD2@M=")(princ RD2)         
           (if (and (= KT "KB")(= WP "WP"));既製BOXの屋根
               (progn
                  (setq LU2 (list (- (car RU2) BD) (cadr RU2)))
                  (setq RP1 (list (car RU2) (+ (cadr RU2) 30)))
                  (setq RP2 (list (car LU2) (cadr RP1)))
                  (setq RP3 (list (- (car RP2) 15) (- (cadr RP2) 15)))
                  (setq RP4 (list (car RP3) (- (cadr RP3) 15)))
                  (setq RP5 (list (+ (car LU2) BD) (cadr RU2)))
                      (command "COLOR" 4)
                      (command "PLINE" RU2 RP1 RP2 "")
                      (command "ARC" RP2 RP3 RP4)
                      (command "PLINE" RP3 RP4 RP5 "")
                      (command "COLOR" 2)
                      (command "DIM" "HOR" RP1 RP2 PDIM1 "" "HOR" RP1 RP4 PDIM "" "EXIT")
                      ;(princ "\nLU2@BW=")(princ LU2)(princ "\nRP1@BW=")(princ RP1)
                      ;(princ "\nRP2@BW=")(princ RP2)(princ "\nRP3@BW=")(princ RP3)
                      ;(princ "\nRP4@BW=")(princ RP4)(princ "\nRP5@BW=")(princ RP5)
               );progn
           )
           (if (and (= KT "TB")(= WP "WP"));特注BOXの屋根
               (progn
                  (setq LU2 (list (- (car RU2) BD) (cadr RU2)))
                  (setq RP1 (list (car RU2) (+ (cadr RU2) 50)))
                  (setq RP2 (list (- (car LU2) 20) (cadr RU2)))
                  (setq RP3 (list (car RP2) (+ (cadr RP2) 20)))
                  (setq RP4 (list (car LU2) (+ (cadr LU2) 20)))
                  (setq RP5 (list (+ (car RP2) 50) (cadr RP1)))
                      (command "COLOR" 4)
                      (command "PLINE" RU2 RP1 RP5 RP3 RP2 "C")
                      (command "COLOR" 2)
                      (command "DIM" "HOR" RP1 RP4 PDIM1 "" "HOR" RP1 RP3 PDIM "" "EXIT")
                      ;(princ "\nLU2@TW=")(princ LU2)(princ "\nRP1@TW=")(princ RP1)
                      ;(princ "\nRP2@TW=")(princ RP2)(princ "\nRP3@TW=")(princ RP3)
                      ;(princ "\nRP4@TW=")(princ RP4)(princ "\nRP5@TW=")(princ RP5)
               );progn
            );if
        );progn
    );if

;-------------20130207(THU) 大田追加（屋外・特注・の水抜き穴）--始まり------------
    ;;(princ "\nしおり3=")(princ "しおり3")
    (if (and (= WP "WP")(= KJ "KK")(= KT "TB")(= LM "M"));もしKTが"KB"= 既製BOX "TB"= 特注BOXなら
        (progn;; progn 関数を使用すると、1 つの式しか指定できない箇所で複数の式を評価できます
        ;水抜き；壁掛かつ幅600未満
            (if (and (= KJ "KK")(< BW 600)) (command "INSERT" "MIZUN-16" LD1 "1" "1" "0"))
        ;水抜き；壁掛かつ幅600以上
            (if (and (= KJ "KK")(>= BW 600))(command "INSERT" "MIZUN-25" LD1 "1" "1" "0"))
            ;Ｘ方向尺度"1" Ｘ方向尺度"1" 回転角度"0"　　MIZUN-16,25はブロック図の名前。
            ;LD1は　指定した１点左下端。
            ;PCの座標をリストにして使用している。
            ;LD1(setq LD1 (list (- (car PC)(/ BW 2))(- (cadr PC) BH))
        );progn
    );if
    ;;(princ "\nしおり4=")(princ "しおり4")
;-------------20130207(THU) 大田追加（屋外・特注・の水抜き穴）---終わり-----------

    (if (= OP1 "U"); 埋込の縁
        (progn
            (if (>= BH 1700)(setq LL 15))       ;LL=埋込の縁 上下の折曲がり長さ
            (if (<= BH 1600)(setq LL 12))
            (command "COLOR" 4)
            (setq FU1 (list (car LU2) (+ (cadr LU2) 15)))
            (setq FU2 (list (+ (car LU2) LL) (+ (cadr LU2) 15)))
            (setq FD1 (list (car LD2) (- (cadr LD2) 15)))
            (setq FD2 (list (+ (car LD2) LL) (- (cadr LD2) 15)))
            (command "PLINE" LU2 "W" "4" "4" FU1 FU2 "")        ;埋込の縁 上
            (command "PLINE" LD2 "W" "4" "4" FD1 FD2 "")        ;埋込の縁 下
            (command "PLINE" FU2 "W" "0" "0" FD2 "")            ;埋込の縁 縦細線
            ;(princ "\nFU1@U=")(princ FU1)(princ "\nFU2@U=")(princ FU2)
            ;(princ "\nFD1@U=")(princ FD1)(princ "\nFD2@U=")(princ FD2)
         )
     )
;;-----------------------------------------------------------------------------
;側面図のチャンネル図
    (if (and (= KT "KB")(= KJ "JJ"));自立の場合
        (progn
           (setq SLCD (list (car LD2) (- (cadr LD2) HH)))         ;ﾁｬﾝﾈﾙ左下
           (setq SRCD (list (car RD2) (- (cadr RD2) HH)))         ;ﾁｬﾝﾈﾙ右下
           (setq SCCL (list (+ (car SLCD) 30) (+ (cadr SLCD) 30)));ﾁｬﾝﾈﾙｱﾝｶｰ穴L
           (setq SCCR (list (- (car SRCD) 30) (+ (cadr SRCD) 30)));ﾁｬﾝﾈﾙｱﾝｶｰ穴R
           (command "COLOR" 4)
           (command "RECTANGLE" "W" 0 "F" 0 LD2 SRCD)                         ;ﾁｬﾝﾈﾙ
           (command "COLOR" 2)                                    ;黄色
           (command "DIM" "HOR" SCCL SLCD PDIM3 ""
                          "HOR" SCCL SCCR PDIM3 ""
                          "HOR" SCCR SRCD PDIM3 "" "EXIT")
           ;(princ "\nSLCD@BJ=")(princ SLCD)
           ;(princ "\nSRCD@BJ=")(princ SRCD)
           ;(princ "\nSCCL@BJ=")(princ SCCL)
           ;(princ "\nSCCR@BJ=")(princ SCCR)
        );progn
    );if
    (if (and (= KT "TB")(= KJ "JJ"));特注BOXで自立の場合
        (progn (princ "\n特注BOXで自立の場合")
           (setq HH 50)
           (setq PDY  (list (- (car RU2) (/ BD 2))(+ (cadr RU2) 8000)))   ;ﾁｬﾝﾈﾙの仮挿入点
           (setq PDYL (list (- (car PDY) 500)(- (cadr PDY) 350)))         ;ｽﾄﾚｯﾁ左ﾌｪﾝｽ点
           (setq PDYR (list (+ (car PDY) 500)(- (cadr PDY) 350)))         ;ｽﾄﾚｯﾁ右ﾌｪﾝｽ点
           (setq STL  (/ (- BD 160) 2))                                   ;ｽﾄﾚｯﾁ距離 160=BLOCKの幅
           (setq DSTR (list (+ (car PDY) STL)(cadr PDY)))                 ;ｽﾄﾚｯﾁ目標点 R
           ;(princ "\nPDY@TJ=")(princ PDY)
           ;(princ "\nPDYL@TJ=")(princ PDYL)
           ;(princ "\nPDYR@TJ=")(princ PDYR)
           ;(princ "\nSTL@TJ=")(princ STL)
           ;(princ "\nDSTR@TJ=")(princ DSTR)
           (if (= LM "L") ;落込みの場合
               (setq DSTL (list (- (car PDY) STL)(cadr PDY)))             ;ｽﾄﾚｯﾁ目標点 L
           );if
           (if (= LM "M");突合せの場合
               (setq DSTL (list (- (car PDY) (- STL DD2))(cadr PDY)))     ;ｽﾄﾚｯﾁ目標点 R
           );if
           (if (= WP "WN");屋根無しの場合
               (progn 
                  (setq CHAN "*CHAN-INS")
                  (setq LCD (list (car LD1) (- (cadr LD1) HH)))                ;ﾁｬﾝﾈﾙ左下
                  (command "DIM" "VER" LU1 LCD SDIM "" "EXIT")                 ;全体の高さ
                  (setq RINS (list (- (car RU2) (/ BD 2))(cadr RU2)))          ;吊り環の挿入 S
                  (command "INSERT" "RING-F2" RINS "1" "1" "0")
                  ;(princ "\nLCD@WN=")(princ LCD)
                  ;(princ "\nRINS@WN=")(princ RINS)
               );progn
           );if
           (if (= WP "WP");屋根付きの場合
               (progn 
                  (setq HH 100)
                  (setq CHAN "*CHAN-WPS")
                  (setq LCD (list (car LD1) (- (cadr LD1) HH)))                ;ﾁｬﾝﾈﾙ左下
                  (setq LSU (list (car LU1) (+ (cadr LU1) FRH)))
                  (command "DIM" "VER" LSU LCD SDIM "" "EXIT")                 ;全体の高さ
                  (setq RINS (list (- (car RU2) (/ BD 2))(+ (cadr RU2) 50)))   ;吊り環の挿入 S
                  (command "INSERT" "RING-F2" RINS "1" "1" "0")
                  ;(princ "\nLCD@WP=")(princ LCD)
                  ;(princ "\nLSU@WP=")(princ LSU)
                  ;(princ "\nRINS@WP=")(princ RINS)
                  (setq SUNM (/ (- BD 200) 2))                                 ;ｽﾄﾚｯﾁする距離を計算
                  ;(princ "\nSUNM=")(princ SUNM)
                  (setq DSTR (list (+ (car PDY) SUNM) (cadr DSTR)))     ;
                  (setq DSTL (list (- (car PDY) SUNM) (cadr DSTL)))     ;
                  (setq PDY1 (list (+ (car PDY) 0) (cadr PDY)))         ;ﾁｬﾝﾈﾙをｽﾄﾚｯﾁする基点 右側
                  (setq PDY2 (list (- (car PDY) 0) (cadr PDY)))         ;ﾁｬﾝﾈﾙをｽﾄﾚｯﾁする基点 左側
                  (setq DSTRR (list (+ (car DSTR) 0) (+ (cadr DSTR) 0)));ﾁｬﾝﾈﾙをｽﾄﾚｯﾁする 距離 右側
                  (setq DSTLL (list (- (car DSTL) 0) (+ (cadr DSTL) 0)));ﾁｬﾝﾈﾙをｽﾄﾚｯﾁする 距離 左側
                  (setq PDY5 (list (car PDY) (+ (cadr PDY) 0)))                ;ｽﾄﾚｯﾁの基点 上部
                  (setq PDY6 (list (- (car PDY) 18) (cadr PDY)))
                  (setq PUYR1 (list (+ (car PDY) 80) (- (cadr PDY) 105)))      ;ｽﾄﾚｯﾁの終点 上部
                  (setq SUNRD2 (list (+ (car PDY) 26) (- (cadr PDY) 107)))     ;ｽﾄﾚｯﾁの始点 下部
                  (setq PUYL1 (list (- (car PDY) 80) (- (cadr PDY) 105)))      ;ｽﾄﾚｯﾁの終点 上部
                  (setq SUNLD2 (list (- (car PDY) 26) (- (cadr PDY) 107)))     ;ｽﾄﾚｯﾁの始点 下部
                  (setq DSTRR1 (list (+ (car DSTR) 26) (cadr SUNRD2)))   ;ﾁｬﾝﾈﾙをｽﾄﾚｯﾁする距離 右側
                  (setq DSTLL1 (list (- (car DSTL) 26) (cadr SUNLD2)))   ;ﾁｬﾝﾈﾙをｽﾄﾚｯﾁする距離 左側
                  ;;;;;;;;(setq PDYX (car PDY))
                  (setq MANSUN0 (- BD 50))              ;奥行から、扉とﾁｬﾝﾈﾙと箱体の隙間分を引く
                  (setq MANSUN0 (- MANSUN0 40))         ;ﾁｬﾝﾈﾙの両端分の合計 40を引く
                  (setq MANSUN0 (/ MANSUN0 2))          ;中心から、寸法線を引く距離を出す
        ;ﾁｬﾝﾈﾙ 中央 寸法線 右位置
                  (setq MANSUNR (list (+ (car PDY) MANSUN0) (- (cadr PDY) 183))) 
        ;ﾁｬﾝﾈﾙ 中央 寸法線 左位置
                  (setq MANSUNL (list (- (car PDY) MANSUN0) (- (cadr PDY) 183))) 
        ;ﾁｬﾝﾈﾙ 真中寸法線を消す 点
                  (setq KESU (list (car PDY) (- (cadr PDY) 183)))
                  ;(princ "\nMANSUN0=")(princ MANSUN0)
                  ;(princ "\nPDY@WP=")(princ PDY)
                  ;(princ "\nDSTRR@@=")(princ DSTRR)
                  ;(princ "\nDSTLL@@=")(princ DSTLL)
                  ;(princ "\nPDY1@WP=")(princ PDY1)
                  ;(princ "\nPDY2@WP=")(princ PDY2)
                  (command "INSERT" CHAN PDY "1" "0")                     ;ﾁｬﾝﾈﾙを挿入する
                  (command "DELETE" KESU "")
                  (command "STRETCH" "C" PDY5 PUYR1 "" PDY5 DSTRR)        ;ﾁｬﾝﾈﾙ 右側 図形 ｽﾄﾚｯﾁ
                  (command "STRETCH" "C" SUNRD2 PDYR "" SUNRD2 DSTRR1)    ;ﾁｬﾝﾈﾙ 右側 寸法 ｽﾄﾚｯﾁ
                  (command "STRETCH" "C" PDY6 PUYL1 "" PDY5 DSTLL)        ;ﾁｬﾝﾈﾙ 左側 図形 ｽﾄﾚｯﾁ
                  (command "STRETCH" "C" SUNLD2 PDYL "" SUNLD2 DSTLL1)    ;ﾁｬﾝﾈﾙ 左側 寸法 ｽﾄﾚｯﾁ
                  (command "DIM" "HOR" MANSUNR MANSUNL MANSUNL "" "EXIT") ;ﾁｬﾝﾈﾙ 真中 寸法線
                  (command "MOVE" "C" (list (car PDYL)(cadr PDY)) PDYR "" PDY 
                                      (list (+ (car PDY) 20)(cadr LD2)))
               );progn
           );if
           ;(princ "\nLD2B@1@=")(princ LD2B)(princ "\nRD22@1@=")(princ RD)
           (if (and (= LM "M")(= WP "WP"))
               (progn
                  (setq LDLD (list (+ (car LD2B) 5) (cadr RD22)))      ;変更 扉を除いた箱体左下端
                  (setq LDLDR (list (car LD2B) (cadr LDLD)))           ;変更 寸法線 箱体とﾁｬﾝﾈﾙ間
                  (setq 55DIML (list (car LDLDR) (- (cadr LD22) 20)))  ;変更 寸法線 箱体とﾁｬﾝﾈﾙ間
                  (setq 55DIMR (list (car RD2) (- (cadr RD22) 20)))    ;変更 寸法線 箱体とﾁｬﾝﾈﾙ間
               );progn
           );if
           (if (/= WP "WP")
               (progn
                   (princ "\nPDY@00=")(princ PDY)
                   ;(princ "\nPDYR@00=")(princ PDYR)
                   ;(princ "\nDSTR@00=")(princ DSTR)
                   ;(princ "\nPDYL@00=")(princ PDYL)
                   ;(princ "\nDSTL@00=")(princ DSTL)
                   (setq KESU (list (car PDY) (- (cadr PDY) 150)))   ;ﾁｬﾝﾈﾙ 真中 寸法線を消す位置
                   (setq PDY1 (list (+ (car PDY) 29) (cadr PDY)))    ;ｽﾄﾚｯﾁする基点 右側
                   (setq PDY2 (list (- (car PDY) 29) (cadr PDY)))    ;ｽﾄﾚｯﾁする基点 左側
                   (setq DSTRR (list (+ (car DSTR) 29) (cadr DSTR))) ;ｽﾄﾚｯﾁする目的点 右側 
                   (setq DSTLL (list (- (car DSTL) 29) (cadr DSTL))) ;ｽﾄﾚｯﾁする目的点 左側

                      (if (= LM "M")
                          (progn
                              (setq PDY1 (list (+ (car PDY) 28)(cadr PDY)))
                              (setq PDY2 (list (- (car PDY) 28)(cadr PDY)))
                              (setq SUNM (/ (- BD 200) 2))                     ;ｽﾄﾚｯﾁする距離を計算
                              (setq DSTR (list (+ (car PDY) SUNM) (cadr DSTR)));
                              (setq DSTL (list (- (car PDY) SUNM) (cadr DSTL)));
                              (setq DSTRR (list (+ (car DSTR) 28)(cadr DSTR)))
                              (setq DSTLL (list (- (car DSTL) 28)(cadr DSTL)))
                              ;(princ "\nPDY1@@00=")(princ PDY1)
                              ;(princ "\nPDY2@@00=")(princ PDY2)
                              ;(princ "\nDSTRR@@00=")(princ DSTRR)
                              ;(princ "\nDSTLL@@00=")(princ DSTLL)
                          );progn
                      );if
                   (command "INSERT" CHAN PDY "1" "0")                          ;ﾁｬﾝﾈﾙを挿入する
                      (if (= LM "M")
                        (progn
                          (command "DELETE" KESU "")                ;ﾁｬﾝﾈﾙ 真中 寸法線を消す
                          (setq MANSUN0 (- BD 40))                  ;奥行から扉分の厚さを引く                        
                          (setq MANSUN0 (- MANSUN0 60))             ;ﾁｬﾝﾈﾙ両端の合計を引く
                          (setq MANSUN0 (/ MANSUN0 2))              ;中心から寸法線をひく距離を出す
                          (setq MANSUNR (list (+ (car PDY) MANSUN0) (cadr KESU)));ﾁｬﾝﾈﾙ寸法線右位置
                          (setq MANSUNL (list (- (car PDY) MANSUN0) (cadr KESU)));ﾁｬﾝﾈﾙ寸法線左位置
                        );progn
                      );if
                   (command "STRETCH" "C" PDY1 PDYR "" PDY1 DSTRR) ;変更後 ﾁｬﾝﾈﾙをｽﾄﾚｯﾁする 右側
                   (command "STRETCH" "C" PDY2 PDYL "" PDY2 DSTLL) ;変更後 ﾁｬﾝﾈﾙをｽﾄﾚｯﾁする 左側
                       (if (/= LM "M")
                           (progn
                               (command "MOVE" "C" (list (car PDYL)(cadr PDY)) PDYR "" PDY 
                                                   (list (car PDY)(cadr LD2))) ;ﾁｬﾝﾈﾙ図形を移動する
                           );progn
                           (progn  ;ﾁｬﾝﾈﾙ 真中 寸法線を引く
                                (command "DIM" "HOR" MANSUNR MANSUNL MANSUNL "" "EXIT")        
                                (command "MOVE" "C" (list (car PDYL)(cadr PDY)) PDYR "" PDY 
                                         (list (+ (car PDY) 20)(cadr LD2))) ;ﾁｬﾝﾈﾙ図形を移動する
                           );progn
                       );if
               );progn
           );if
           (if (and (= LM "M")(= WP "WP"))
               (progn
                   (command "COLOR" 2)
                   (command "DIM" "HOR" LDLD LDLDR 55DIML "" "EXIT");変更 寸法線 箱体とﾁｬﾝﾈﾙ間 左側
                   (command "DIM" "HOR" RD22 RD2 55DIMR "" "EXIT")  ;変更 寸法線 箱体とﾁｬﾝﾈﾙ間 右側
                   (command "COLOR" 4)
               );progn
           );if 
        );progn
    );if
    (command "COLOR" 4)
;;-----------------------------------------------------------------------------
;;分電盤の場合（内扉付き）--- 側面図
   (if (= DR "Y1")
       (progn
          (if (>= BD 250.00)(setq SP 146))                     ;;BOX D=250以上
          (if (and (>= BD 200.00) (< BD 250.00))(setq SP 106)) ;;BOX D=250未満 200まで
          (if (< BD 200.00)(setq SP 82))                       ;;BOX D=200未満
          (setq SPD (+ ID SP))
          (setq NU (list (- (car RU2) SPD)(- (cadr RU2) 55)))
          (setq ND (list (- (car RU2) (- SPD 15))(+ (cadr RD2) 55)))
          (setq DNR (list (car IUL)(- (cadr RU2) 100)))
          (setq DNL (list (car NU)(- (cadr RU2) 100)))
          (setq RU22 (list (car RU2)(- (cadr RU2) 100)))
          (setq LU22 (list (car LU2)(- (cadr LU2) 100)))
             (command "LINETYPE" "S" "DASHED" "")
             (command "RECTANGLE" "W" 0 "F" 0 NU ND)                        ;内扉
             (command "COLOR" 2)
             (command "LINETYPE" "S" "CONTINUOUS" "")
             (command "DIM" "HOR" DNR RU22 PDIM4 ""             ;扉～内扉～中板の寸法図
                            "HOR" DNR DNL PDIM4 ""
                            "HOR" DNL LU22 PDIM4 "" "EXIT")
             (command "COLOR" 4)
            ;(princ "\nID@Y=")(princ ID)(princ "\nSPD@Y=")(princ SPD)(princ "\nNU@Y=")(princ NU)
            ;(princ "\nND@Y=")(princ ND)(princ "\nDNR@Y=")(princ DNR)(princ "\nDNL@Y=")(princ DNL)
            ;(princ "\nRU22@Y=")(princ RU22)(princ "\nLU22@Y=")(princ LU22)
       )
   )
;;-----------------------------------------------------------------------------
;;板金記入欄と、特注で自立の場合、箱体内部図の取付穴を消す。
;ﾌﾞﾛｯｸA3Cが 挿入されているかをﾁｪｯｸ
    (setq n 0)
    (setq AA (ssget "X" (list '(0 . "INSERT")))) ;  (list '(66 . 1))))は属性は属性付きのみ有効
    (while (setq BB (ssname AA n))
           (setq ENT (entget BB));;;;;;(princ "\nENT=")(princ (cdr (assoc 2 ENT)))
           (if (or (= (cdr (assoc 2 ENT)) "A3C")(= (cdr (assoc 2 ENT)) "A2C")
                   (= (cdr (assoc 2 ENT)) "A1C")(= (cdr (assoc 2 ENT)) "A0C"))
                   (entdel BB)  ;A3C,A2C,A1C,A0Cを削除
           )
           ;;特注で自立の場合、箱体内部図の取付穴を消す。
           (if (and (= KT "TB")(= KJ "JJ")(= (cdr (assoc 2 ENT)) "D_EN7"))(entdel BB))
           (if (and (= KT "TB")(= KJ "JJ")(= (cdr (assoc 2 ENT)) "6-16P"))(entdel BB))
           (setq n (1+ n))
    );while
;ﾌﾞﾛｯｸA3C 挿入
    (C:SZ)      ;;図面サイズを取得
    (setq SZ (strcat SZ "C"))
    (setq ZB1 Bt ZB2 Dt ZB3 "標準色" ZB4 "無" ZB5 COL ZB6 "7分艶")
                  (setq ZB7 "標準色" ZB8 "無" ZB9 COL ZB10 "7分艶")
    (command "INSERT" SZ "0,0" "1" "1" "0" ZB1 ZB2 ZB3 ZB4 ZB5 ZB6 ZB7 ZB8 ZB9 ZB10)
;;-----------------------------------------------------------------------------
;;ケーブルサポートの追加
    (if (= CS "Y2")
      (progn
        (setq PCCAX1 0)(setq CAH 0)
        (setq PCCAY1 0)(setq PCCAY2 0)(setq PCCAY3 0)
        
        (if (= BH 400) (progn (setq PCCAY1 200)(setq PCCAY2 0)   (setq PCCAY3 0)    ))
        (if (= BH 500) (progn (setq PCCAY1 250)(setq PCCAY2 0)   (setq PCCAY3 0)    ))
        (if (= BH 600) (progn (setq PCCAY1 300)(setq PCCAY2 0)   (setq PCCAY3 0)    ))
        (if (= BH 700) (progn (setq PCCAY1 350)(setq PCCAY2 0)   (setq PCCAY3 0)    ))
        (if (= BH 800) (progn (setq PCCAY1 200)(setq PCCAY2 600) (setq PCCAY3 0)    ))
        (if (= BH 900) (progn (setq PCCAY1 200)(setq PCCAY2 700) (setq PCCAY3 0)    ))
        (if (= BH 1000)(progn (setq PCCAY1 200)(setq PCCAY2 800) (setq PCCAY3 0)    ))
        (if (= BH 1100)(progn (setq PCCAY1 200)(setq PCCAY2 900) (setq PCCAY3 0)    ))
        (if (= BH 1200)(progn (setq PCCAY1 200)(setq PCCAY2 1000)(setq PCCAY3 0)    ))
        (if (= BH 1300)(progn (setq PCCAY1 200)(setq PCCAY2 620) (setq PCCAY3 1100) ))
        (if (= BH 1400)(progn (setq PCCAY1 200)(setq PCCAY2 670) (setq PCCAY3 1200) ))
        (if (= BH 1500)(progn (setq PCCAY1 200)(setq PCCAY2 720) (setq PCCAY3 1300) ))
        (if (= BH 1600)(progn (setq PCCAY1 200)(setq PCCAY2 770) (setq PCCAY3 1400) ))
        (if (= BH 1700)(progn (setq PCCAY1 200)(setq PCCAY2 820) (setq PCCAY3 1500) ))
        (if (= BH 1800)(progn (setq PCCAY1 200)(setq PCCAY2 870) (setq PCCAY3 1600) ))
        (if (= BH 1900)(progn (setq PCCAY1 200)(setq PCCAY2 920) (setq PCCAY3 1700) ))
        (if (= BH 2000)(progn (setq PCCAY1 200)(setq PCCAY2 970) (setq PCCAY3 1800) ))
        
        (setq PCCAX1 (+ (car PC)(/ BW 2)(+ BD 500)))
        (setq CAH (/ (- BD 50) 10))
        
        ;(princ "\nPC=")(princ PC)        (princ "\nPCCAY1=")(princ PCCAY1)
        ;(princ "\nPCCAY2=")(princ PCCAY2)(princ "\nPCCAY3=")(princ PCCAY3)
        ;(princ "\nPCCAX1=")(princ PCCAX1)
        (command "LINETYPE" "S" "DASHED" "")
        
        (if (/= PCCAY1 0)
            (progn
                (setq PCCA (list (- PCCAX1 10)(- (cadr PC) PCCAY1)))
                (command "INSERT" "CASUPPORT" PCCA CAH "1" "0")
                ;(princ "\nPCCA@1=")(princ PCCA)
            )
        )
        (if (/= PCCAY2 0)
            (progn
                (setq PCCA (list (- PCCAX1 10)(- (cadr PC) PCCAY2)))
                (command "INSERT" "CASUPPORT" PCCA CAH "1" "0")
                ;(princ "\nPCCA@2=")(princ PCCA)
            )
        )
        (if (/= PCCAY3 0)
            (progn
                (setq PCCA (list (- PCCAX1 10)(- (cadr PC) PCCAY3)))
                (command "INSERT" "CASUPPORT" PCCA CAH "1" "0")
                ;(princ "\nPCCA@3=")(princ PCCA)
            )
        )
        
        (if (and (= PCCAY1 0)(= PCCAY2 0)(= PCCAY3 0))
                             (alert "この高さはケーブルサポートを追加できません！"))
        
        (command "LINETYPE" "S" "CONTINUOUS" "")
      )
    )
;;-----------------------------------------------------------------------------
;;終了処理
   (princ "\nPDIM=")(princ PDIM)
   (command "LAYER" "S" "S1" "")(setvar "PLINEWID" 0)(command "COLOR" 4)(setvar "USERS5" "M")
   (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)(princ "\n箱体外形図、作画終了！")
   (redraw)
   (command "ZOOM" "W" ZOOM1L ZOOM1R)           ;アース端子シンボルの点線に再作図を効かすズーム
   (command "REGEN")                            ;再作図
   (command "ZOOM" "E")                         ;図枠範囲内にもどる

   (if (= DN "D2")                              ;2枚扉を選択した場合
       (progn
            (command "ZOOM" "W" ZOOM2L ZOOM2R)  ;2枚扉用のｱ-ｽ端子ｼﾝﾎﾞﾙの点線に再作図を効かすズーム
            (command "REGEN")                   ;再作図
            (command "ZOOM" "E")                ;図枠範囲内にもどる
       );progn
   );if
 )

