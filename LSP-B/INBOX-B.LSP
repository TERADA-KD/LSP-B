;;==================================================================================================
;;箱体内部配置図  INBOX-I J.LSP + INBOX.DCL  1988/MAY/4 => 2003/JUL/2 =>2020/Nov   By T.Terada
;; 1) 05/Sep/20 水切り構造に対応
;; 2) 05/Sep/25 既製ＢＯＸにホシモトの金具、対応。
;; 3) 06/Dec/25 ケーブルサポートの追加。
;===================================================================================================
;;DCLの起動
(defun DCL_INBOX (/)
	  (setq DBT nil DWP nil DNT nil DPSH nil DCBL nil DDG nil DHA nil);忘れないように！
      (if (not DBT)(setq DBT "1"));初期値の設定
      (if (not DWP)(setq DWP "1"))
      (if (not DNT)(setq DNT "1"))
      (if (not DPSH)(setq DPSH "1"))
      (if (not DCBL)(setq DCBL "1"))
      (if (not DDG)(setq DDG "1"))
      (setq DCL_ID (load_dialog "X:/LSP-I/INBOX.DCL"))
      (setq WHAT_NEXT 2)
      (while (>= WHAT_NEXT 2);;;ステータスが非表示を示している場合
         (if (not (new_dialog "INBOX" DCL_ID) )(EXIT))
         (action_tile "cancel" "(done_dialog)(exit)")
          
         (action_tile "KK" "(setq DBT $key)      ;ＢＯＸタイプ
                            (mode_tile \"42\" 1)
                            (mode_tile \"60\" 1)
                            (mode_tile \"HA\" 1)");  mode_tile: "0"= 使用可能 / "1"= 禁止
                             
;;         (action_tile "KJ" "(setq DBT $key)")
         (action_tile "TT" "(setq DBT $key)
                            (mode_tile \"42\" 0)
                            (mode_tile \"60\" 0)
                            (mode_tile \"HA\" 0)")
                             
         (action_tile "Y1" "(setq DWP $key)");屋内外
         (action_tile "N1" "(setq DWP $key)")
         
         (action_tile "N2" "(setq DNT $key);内扉の有無
                           (mode_tile \"NG\" 1)(mode_tile \"HO\" 1)")
         (action_tile "Y2" "(setq DNT $key)
                           (mode_tile \"NG\" 0)(mode_tile \"HO\" 0)")
         
         (action_tile "NG" "(setq DPSH $key)");支柱の有無
         (action_tile "HO" "(setq DPSH $key)")
         
         (action_tile "N3" "(setq DCBL $key)");ケーブルサポート
         (action_tile "Y3" "(setq DCBL $key)")
         
         (action_tile "42" "(setq DDG $key) ;扉の厚み
                            (mode_tile \"LG\" 1)(mode_tile \"RG\" 1)(mode_tile \"UG\" 1)(mode_tile \"DG\" 1)")
         (action_tile "60" "(setq DDG $key)
                            (mode_tile \"LG\" 1)(mode_tile \"RG\" 1)(mode_tile \"UG\" 1)(mode_tile \"DG\" 1)")
         (action_tile "HA" "(setq DHA $value)
                            (mode_tile \"42\" 1)(mode_tile \"60\" 1)
                            (mode_tile \"LG\" 0)(mode_tile \"RG\" 0)(mode_tile \"UG\" 0)(mode_tile \"DG\" 0)")
         ;;; "HA"はedit_boxのフォーカス切替えのみに機能させ$valueでボタンのON-OFFを検出する。

         		(action_tile "LG" "(setq LG (get_tile \"LG\"))");特注のガータースペース
         		(action_tile "RG" "(setq RG (get_tile \"RG\"))")
         		(action_tile "UG" "(setq UG (get_tile \"UG\"))")
         		(action_tile "DG" "(setq DG (get_tile \"DG\"))")
         (action_tile "PW" "(setq PW (get_tile \"PW\"))");箱体寸法
         (action_tile "PH" "(setq PH (get_tile \"PH\"))")
         (action_tile "PD" "(setq PD (get_tile \"PD\"))")
		 (start_dialog)
         (setq WHAT_NEXT (start_dialog))
	  )
      (unload_dialog DCL_ID)(princ)
)
;===================================================================================================
(defun C:INBOX ( / PC1 PC2 PC3 LU1 LU2 LU3 LU4 LD1 LD2 LD3 LD4 RD1 RD2 RD3 RD4 RU1 RU2 RU3 RU4 LG RG UG DG
                   NW NW1 N1 N2 K1 K2 K3 K4 K5 TH1 TH11 TW1 PDD ZH3 ZH4 LC3 LCU LCD NH NH1 NH11 RDIM PDIM SDIM
                   LDIM HDIM VDIM DRU DRD RCU RCD DRC DUL DUR DSU DSD RC3 n HIN UKD UKU ROL AA BB ENT SLI SLI2
                   ZI1 ZI2 ZI3 ZI4 ZI5 SSLU SSLD SSR RLT SSLUV SSLDV SSRV RLTV 2PW 2LU3 2LD3 2RU3 2RD3 NLR NTW
                   LU4CR RD4CL DRCU DRCD 2LC3 2RC3 KMK LU3N LU4N NPN NPN2 NDT TOTP TOTP2 KL RDP DRDP DLDP GT
                   ETRR BTYP WP NAKA PSH CBL PW PH DBT DWT DNT DPSH DCBL)
;(setq *error* *err1*)                     ;DCL からもらう変数
(setvar "CMDECHO" 1)(setvar "BLIPMODE" 0) ;BOXﾀｲﾌﾟ      BTYP  既製壁掛="KK" 既製自立="KJ" 特注="TT"
                                          ;屋内or屋外   WP    "Y" or "N"
                                          ;中扉の有無   NAKA  "Y" or "N"
                                          ;支柱のﾒｰｶｰ   PSH   "NG" or "HO"  内外 or ホシモト
                                          ;ｹｰﾌﾞﾙｻﾎﾟｰﾄ   CBL   "Y" or "N"
                                          ;ガーター     左:LLG 右:RRG 上:UUG 下:DDG  
                                          ;箱体の外形   横幅="PW" 高さ="PH" 奥行="PD"
;;                                        ;ﾒｰｶｰ         BMKR "K" or "N"    河村 or 内外

    (if (= (getvar "USERI5") 0)
       (progn
;          (setq BMKR "")
          (DCL_INBOX);DCLのﾀﾞｲｱﾛｸﾞ呼び出し
          (if (and (= DNT "Y2")(= DPSH "1"))(progn (alert "支柱メーカーが選択されていません")(DCL_INBOX)))
          (setq PC (getpoint "\n挿入点を指示して下さい．(上端の中心点):"))
       )
    )
;   	(princ "\nDBT = ")(princ DBT)(princ "\nDWP = ")(princ DWP)(princ "\nDNT = ")(princ DNT)
;   	(princ "\nDPSH = ")(princ DPSH)(princ "\nDCBL = ")(princ DCBL)
;   	(princ "\nDDG = ")(princ DDG)(princ "\nDHA = ")(princ DHA)(princ "\n")
	(cond ((= DBT "1") (setq BTYP "KK"))
	      ((= DBT "KK")(setq BTYP "KK"))
	      ((= DBT "TT")(setq BTYP "TT"))
	)
    (cond ((= DWP "1")(setq WP "Y"))
	      ((= DWP "Y1")(setq WP "Y"))
	      ((= DWP "N1")(setq WP "N"))
	)
    (cond ((= DNT "1")(setq NAKA "N"))
	      ((= DNT "N2")(setq NAKA "N"))
	      ((= DNT "Y2")(setq NAKA "Y"))
	)
    (cond ((= DPSH "1") (setq PSH "NG"))
	      ((= DPSH "NG")(setq PSH "NG"))
	      ((= DPSH "HO")(setq PSH "HO"))
	)
    (cond ((= DCBL "1")(setq CBL "N"))
	      ((= DCBL "N3")(setq CBL "N"))
	      ((= DCBL "Y3")(setq CBL "Y"))
	)
   	(cond ((and (= DDG "1")(/= DHA "1")) (setq LG 42 RG 42 UG 42 DG 42))
	      ((and (= DDG "42")(/= DHA "1"))(setq LG 42 RG 42 UG 42 DG 42))
	      ((and (= DDG "60")(/= DHA "1"))(setq LG 60 RG 60 UG 60 DG 60))
	)
;   	(princ "\nBTYP = ")(princ BTYP)(princ "\nWP = ")(princ WP)(princ "\nNAKA = ")(princ NAKA)
;   	(princ "\nPSH = ") (princ PSH)(princ "\nCBL = ") (princ CBL)
;   	(princ "\nHA = ") (princ "HA")
  	(princ "\nLG = ")  (princ LG)(princ "\nRG = ")(princ RG)(princ "\nUG = ")(princ UG)(princ "\nDG = ")(princ DG);
	(princ "\nPW = ")  (princ PW)(princ "\nPH = ")(princ PH)(princ "\nPD = ")(princ PD)(princ)
;    (textscr)(princ)
;===================================================================================================
    (command "FILLET" "R" "25");FILLETの半径を25に戻す。ﾊﾞｰｼﾞｮﾝによって振る舞いが異なる。
    
    (setq PW (atoi PW))(setq PH (atoi PH))(setq PD (atoi PD));数値に変換
;    (setq LG (atoi LG))(setq RG (atoi RG))(setq UG (atoi UG))(setq DG (atoi DG))
    (setq PDD PD)
    (command "LINETYPE" "S" "CONTINUOUS" "") ;実線
    (command "COLOR" 4)                      ;水色
    (setvar  "PLINEWID" 0)                   ;線幅を0
;    (command "LAYER" "S" "BOX" "")           ;画層 BOXに変更
;２枚扉の条件
    (if (and (/= BTYP "TT")(>= PW  800)(= NAKA "Y"))(setq NAKA "Y2"));既製BOXの場合
    (if (and (=  BTYP "TT")(>= PW 1000)(= NAKA "Y"))(setq NAKA "Y2"));特注BOXの場合
;ガーター値設定
    (if (= BTYP "KK");既製BOX
        (progn
           (setq LG 40)  ;左側のガーター
           (setq RG 40)  ;右側のガーター
           (setq UG 40)  ;上部のガーター
           (setq DG 40)  ;下部のガーター
           (setq KL 95)  ;内扉上端からの支柱の位置 盤上端から150に変更 05/10/29
           (setq NLR 55) ;内扉寸法算出 (W-NLR-NLR=内扉上下寸法)
           (setq NTW 110);盤幅から内扉の幅を求める為に引く値  内扉の幅 = W - NTW
        )
    )
    (if (and (= BTYP "KK")(<= PH 1600)(= WP "Y"));既製BOX 水切構造 1600以下
        (progn
           (setq LG 40)  ;左側のガーター
           (setq RG 40)  ;右側のガーター
           (setq UG 40)  ;上部のガーター
           (setq DG 40)  ;下部のガーター
           (setq KL 95)  ;内扉上端からの支柱の位置 盤上端から150に変更 05/10/29
           (setq NLR 40) ;内扉寸法算出 (W-NLR-NLR=内扉上下寸法)
           (setq NTW 80) ;盤幅から内扉の幅を求める為に引く値  内扉の幅 = W - NTW
        )
    )
    (if (and (= BTYP "KK")(>= PH 1700)(= WP "Y"));既製BOX 水切構造 1600以上
        (progn
           (setq LG 50)  ;左側のガーター
           (setq RG 50)  ;右側のガーター
           (setq UG 45)  ;上部のガーター
           (setq DG 45)  ;下部のガーター
           (setq KL 85)  ;内扉上端からの支柱の位置 盤上端から150に変更 05/10/29
           (setq NLR 55) ;内扉寸法算出 (W-NLR-NLR=内扉上下寸法)
           (setq NTW 110) ;盤幅から内扉の幅を求める為に引く値  内扉の幅 = W - NTW
        )
    )
    (if (= BTYP "TT");特注BOX
        (progn 
               (if (and (= WP "Y")(or (= NAKA "Y")(= NAKA "Y2")))
                   (progn (setq LG 60)(setq RG 60)(setq UG 60)(setq DG 60))
               )
               (setq KL 95)  ;内扉上端からの支柱の位置
               (setq NLR 55) ;内扉寸法算出 (W-NLR-NLR=上下内扉寸法)
               (if (= WP "Y")(setq NLR 60))
               (setq NTW 110);盤幅から内扉の幅を求める為に引く値  内扉の幅 = W - NTW
        );progn
    )
;箱体の外枠と四角
    (setq LU1 (list (- (car PC) (/ PW 2)) (cadr PC)))          ;;左上端
(princ "\nLU1=")(princ LU1)
    (setq RU1 (list (+ (car PC) (/ PW 2)) (cadr PC)))          ;;右上端
    ;;(princ "\nRU1=")(princ RU1) 
    (setq LD1 (list (- (car PC) (/ PW 2)) (- (cadr PC) PH)))   ;;左下端
    ;;(princ "\nPW@=")(princ PW)
    ;;(princ "\nPH@=")(princ PH)
    ;;(princ "\nPC@=")(princ PC)
    ;;(princ "\nLD1=")(princ LD1)
    (setq RD1 (list (+ (car PC) (/ PW 2)) (- (cadr PC) PH)))   ;;右下端
(princ "\nRD1=")(princ RD1)
    (command "RECTANGLE" LU1 RD1)
    (princ "\nLU1=")(princ LU1)
    (princ "\nRD1=")(princ RD1)
;アース端子
    (if (= BTYP "KK")(command "INSERT" "ET-K" (list (- (car RU1) 20) (+ (cadr RD1) 75)) "1" "1" "0"))
    ;(if (= BTYP "TT")(command "INSERT" "ET1" LD1 "1" "1" "0")) ;ET1
    (setq WARI (/ 25 2))
    (setq WARI (+ 25 WARI))
    (setq ETRR (list (- (car RD1) WARI) (cadr RD1)))
    (if (= BTYP "TT")(command "INSERT" "ET1RR" ETRR "1" "1" "0"))
    (princ "\nETRR@=")(princ ETRR)
    (princ "\nLD1@=")(princ LD1)
    (princ "\nRD1@=")(princ RD1)
;OFFSETの設定 屋内－屋外
    (princ "\nWP =")(princ WP)
    (if (and (= WP "N")(= BTYP "KK"))(progn (setq OFF1 20)(setq OFF2 45)))
    (if (and (= WP "Y")(= BTYP "KK")(<= PH 1600))(progn (setq OFF1 17)(setq OFF2 31)))
    (if (and (= WP "Y")(= BTYP "KK")(>= PH 1700))(progn (setq OFF1 30)(setq OFF2 45)))
    (if (and (= WP "Y")(= BTYP "TT"))(progn (setq OFF1 30)(setq OFF2 50)))
    (if (and (= WP "N")(= BTYP "TT"))(progn (setq OFF1 20)(setq OFF2 45)))
;箱体の額縁
    (setq PC1 (list (car PC) (- (cadr PC) OFF1)))             ;;OFFSET値:IN-20 WP-17 30
    (princ "\nPC1=")(princ PC1)
    (command "OFFSET" "T" PC PC1 "")
;額縁の折り曲げ
    (setq PC2 (list (car PC) (- (cadr PC) OFF2)))             ;;OFFSET値:IN-45 WP-31 45
    ;;(princ "\nPC2=")(princ PC2)
    (command "OFFSET" "T" PC PC2 "")
;(setvar "CMDECHO" 0) 
;WPの水切りFILLET処理
    (if (= WP "Y")
        (progn
            (setq FIL1 (list (car PC)(- (cadr PC) OFF1)))
;            (princ "\nFIL1=")(princ FIL1)  
            (setq FIL2 (list (car PC)(- (cadr PC) OFF2)))
;            (princ "\nFIL2=")(princ FIL2)
            (command "FILLET" "P" "R" "20" FIL1)                   ;IJCAD5 2020の時
            (command "FILLET" "P" "R" "14" FIL2)                   ;IJCAD5 2020の時
;            (command "FILLET" "R" "20")(command "FILLET" "P" FIL1);ICAD5 Var.050617の時
;            (command "FILLET" "R" "14")(command "FILLET" "P" FIL2);ICAD5 Var.050617の時
            (command "FILLET" "R" "25");FILLETの半径を25に戻す。ﾊﾞｰｼﾞｮﾝによって振る舞いが異なる。
        )
    )
(princ "\nFILLET終わり")
;基板の作図
    (setq LU2 (list (+ (car LU1) LG) (- (cadr LU1) UG)))    ;;基板の左上端
    ;;(princ "\nLU2=")(princ LU2)
    (setq RD2 (list (- (car RD1) RG) (+ (cadr RD1) DG)))    ;;基板の右下端
    ;;(princ "\nRD2=")(princ RD2)
    (setq LD2 (list (+ (car LD1) LG) (+ (cadr LD1) UG)))    ;;基板の左下端
    ;;(princ "\nLD2=")(princ LD2)
    (setq RU2 (list (- (car RU1) RG) (- (cadr RU1) DG)))    ;;基板の右上端
    ;;(princ "\nRU2=")(princ RU2)
       (command "COLOR" 2);黄色
       (command "LINETYPE" "S" "CONTINUOUS" "")
       (if (/= NAKA "N")(command "LINETYPE" "S" "DASHED" ""))
       (command "RECTANGLE" LU2 RD2)
    (setq NW (- PW (+ LG RG)))                              ;;基板の横幅
    ;;(princ "\nNW=")(princ NW)
    (setq NH (- PH (+ UG DG)))                              ;;基板の高さ
    ;;(princ "\nNH=")(princ NH)

;取付穴の条件判定
    (if (> PH 1100)(setq N1 3) (setq N1 2));;高さ 1100以上
    (if (> PH 1100)(setq N2 2) (setq N2 1));;高さ 1100以上
;基板の取付穴 （基板より20入った所）
    (setq LU3N (list (+ (car LU2) 20) (- (cadr LU2) 20)))
    (setq NW1 (- NW 40))    (setq NH11 (- (- NH 40)))    (setq NH1 (/ NH11 N2))
    ;;(princ "\nLU3N=")(princ LU3N)
    ;;(princ "\nNW1=")(princ NW1)
    ;;(princ "\nNH11=")(princ NH11)
    ;;(princ "\nNH1=")(princ NH1)
       (command "LINETYPE" "S" "CONTINUOUS" "")
       (command "CIRCLE" LU3N "4")
       (command "ARRAY" "L" "" "R" N1 "2" NH1 NW1)
;函体の取付穴（特注BOX）
    (if (and (= BTYP "TT")(= WP "N"));WPでは取付穴を明けない
        (progn
           (setq LU4N (list (+ (car LU1) 25) (cadr LU3N)))
           (setq TW1 (- PW 50))
           (setq TH11 (- (- PH UG)))
           (setq TH1 (/ NH11 N2))
           ;;(princ "\nTH1=")(princ TH1)
           ;;(princ "\nTH11=")(princ TH11)
           ;;(princ "\nTW1=")(princ TW1)
           (command "INSERT" "D_EN7" LU4N "1" "1" "0");     TH1
           (command "ARRAY" "L" "" "R" N1 "2" TH1 TW1)
           ;(command "ARRAY" "PRO" "N" "D_EN7" "" "R" N1 "2" TH1 TW1)
           (setq RDP (list (- (car RD1) 25)(+ (cadr RD2) 20)))         ;取付穴 右下
           (if (>= PH 1200)(command "INSERT" "6-16P" RDP "1" "1" "0")) ;取付穴サイズ
           (if (<  PH 1200)(command "INSERT" "4-16P" RDP "1" "1" "0")) ;取付穴サイズ
           (setq DRDP (list (- (car RD1) 25)(cadr RD1)))
           (setq DLDP (list (+ (car LD1) 25)(cadr LD1)))
           (setq DDIM (list (car RD1)(- (cadr RD1) 100)))              ;寸法記入位置 盤下 100
           ;(command "DIM" "HOR" DLDP LD1 DDIM ""                       ;取付穴横寸法 左
                          ;"HOR" DRDP RD1 DDIM ""                       ;取付穴横寸法 右
                          ;"HOR" DLDP DRDP DDIM "" "EXIT")              ;取付穴横寸法 中
        );progn
    )
;;-----------------------------------------------------------------------------
  ;;支柱・ﾛｰﾚｯﾄの選定
    (if (= PSH "HO");ホシモト
        (progn  (setq SSLU  "DHUPT102")     ;受支柱上
                (setq SSLD  "DHUPT102")     ;受支柱下
                (setq SSR   "DHTPT102")     ;ヒンジ支柱
                (setq RLT   "DC100A2T034")  ;ﾛｰﾚｯﾄ
                (setq SSLUV "DHUPT102V")    ;受支柱上   内扉2枚用
                (setq SSLDV "DHUPT102V")    ;受支柱下   内扉2枚用
                (setq SSRV  "DHTPT102V")    ;ヒンジ支柱 内扉2枚用
                (setq RLTV  "DC100A2T034V") ;ﾛｰﾚｯﾄ      内扉2枚用
                (setq TOT   "D1832S034R")   ;取手
                (setq TOTV  "D1832S034L")   ;取手       内扉2枚用
         )
    )
    (if (= PSH "NG");内外
        (progn  (setq SSLU  "DMSZRT036")    ;受支柱上  R=上
                (setq SSLD  "DMSZLT036")    ;受支柱下  L=下
                (setq SSR   "DBSZT036")     ;ヒンジ支柱
                (setq RLT   "DRL1F036")     ;ﾛｰﾚｯﾄ
                (setq SSLUV "DMSZRT036V")   ;受支柱上  R=上 内扉2枚用
                (setq SSLDV "DMSZLT036V")   ;受支柱下  L=下 内扉2枚用
                (setq SSRV  "DBSZT036V")    ;ヒンジ支柱 内扉2枚用
                (setq RLTV  "DRL1F036V")    ;ﾛｰﾚｯﾄ      内扉2枚用
         )
    )
;;内扉の作図
    (setq RDIM (list (+ (car RD1) 150) (cadr RD1)))          ;;寸法記入位置
    (setq PDIM (list (car PC) (+ (cadr PC) 150)))
    (setq LDIM (list (- (car LD1) 150) (cadr LD1)))
    ;;(setq ZH3 (- (/ (- PH (+ UG DG 85 85)) 2) 75))           ;;支柱３個の位置
    ;;(setq ZH4 (+ (/ (- PH (+ UG DG 65 65)) 3) 0.5))          ;;支柱４個の位置
    ;;(princ "\nRDIM=")(princ RDIM)
    ;;(princ "\nPDIM=")(princ PDIM)
    ;;(princ "\nLDIM=")(princ LDIM)
    
    ;;支柱３個の位置
    (setq ZH3 (fix (/ (- PH 150 150) 2)))     ;;高さから上下の150を引いた数字を２で割る。それを切り捨て処理する
    ;;(princ "\nZH3=")(princ ZH3)
    (setq ZH3 (- ZH3 50))                     ;;中心に支柱はダメなので50ずらす
    ;;(princ "\nZH3＋50=")(princ ZH3)
    (setq AMARI (rem ZH3 25))                 ;;基準ピッチ25の倍数になるように数字を処理する。remは余り計算
    ;;(princ "\nAMARI=")(princ AMARI)
    (if (/= AMARI 0)(setq ZH3 (- ZH3 AMARI))) ;;余りがあるなら３個の場合は、中心より50以上ずらしたいので余りを引いて25の倍数にする
    ;;(princ "\nZH3rem25=")(princ ZH3)
    
    ;;支柱４個の位置
    (setq ZH4 (fix (/ (- PH 150 150) 3)))     ;;高さから上下の150を引いた数字を３で割る。それを切り捨て処理する
    ;;(princ "\nZH4=")(princ ZH4)
    (setq AMARI (rem ZH4 25))                 ;;基準ピッチ25の倍数になるように数字を処理する。remは余り計算
    ;;(princ "\nAMARI=")(princ AMARI)
    (if (/= AMARI 0)                          ;;余りがあるなら４個の場合は、１番真ん中の幅を小さくしたいので(25-余り)を足して25の倍数にする
        (progn
            (setq AMARI (- 25 AMARI))
            (setq ZH4 (+ ZH4 AMARI))
        )
    )
    ;;(princ "\nZH4rem25=")(princ ZH4)
    
    (setq DUL (list (+ (car LU1) LG) (cadr PC)))             ;;寸法基板の左上端
    ;;(princ "\nDUL=")(princ DUL)
    (setq DUR (list (- (car RU1) RG) (cadr PC)))             ;;寸法基板の右上端
    ;;(princ "\nDUR=")(princ DUR)
    (setq LU4 (list (+ (car LU1) NLR) (- (cadr LU1) NLR)))   ;;内扉の左上端
    ;;(princ "\nLU4=")(princ LU4)
    (setq RU4 (list (- (car RU1) NLR) (- (cadr RU1) NLR)))   ;;内扉の右上端
    ;;(princ "\nRU4=")(princ RU4)
    (setq LD4 (list (+ (car LD1) NLR) (+ (cadr LD1) NLR)))   ;;内扉の左下端
    ;;(princ "\nLD4=")(princ LD4)
    (setq RD4 (list (- (car RD1) NLR) (+ (cadr RD1) NLR)))   ;;内扉の右下端
    ;;(princ "\nRD4=")(princ RD4)
    (setq LU3 (list (car LU4) (- (cadr LU4) KL)))            ;;支柱の左上端
    ;;(princ "\nLU3=")(princ LU3)
    (setq LD3 (list (car LD4) (+ (cadr LD4) KL)))            ;;支柱の左下端
    ;;(princ "\nLD3=")(princ LD3)
    (setq RU3 (list (car RU4) (- (cadr RU4) KL)))            ;;支柱の右上端
    ;;(princ "\nRU3=")(princ RU3)
    (setq RD3 (list (car RD4) (+ (cadr RD4) KL)))            ;;支柱の右下端
    ;;(princ "\nRD3=")(princ RD3)
    (setq RCU (list (car RU4) (- (cadr RU3) ZH4)))           ;;４個支柱の右上中
    ;;(princ "\nRCU=")(princ RCU)
    (setq RCD (list (car RD4) (+ (cadr RD3) ZH4)))           ;;４個支柱の右下中
    ;;(princ "\nRCD=")(princ RCD)
    (setq DRU (list (car RU1) (cadr RU3)))                   ;;寸法支柱の右上
    ;;(princ "\nDRU=")(princ DRU)
    (setq DRD (list (car RD1) (cadr RD3)))                   ;;寸法支柱の右下
    ;;(princ "\nDRD=")(princ DRD)
    (setq UDT (/ (- (cadr LU4)(cadr LD4)) 2))                ;;内扉の高さ/2
    ;;(princ "\nUDT=")(princ UDT)
    (setq TOTP(list (car LU4) (- (cadr LU4) UDT)))           ;;取手の挿入点
    ;;(princ "\nTOTP=")(princ DTOTP)
    (setq NDT (/ (- (car RU2)(car LU2)) 2))                  ;;内板の幅/2
    ;;(princ "\nNDT=")(princ NDT)
    (setq NPN (list (+ (car LU2) NDT)(- (cadr LU2) 30)))     ;;NP-Nの挿入点
    ;;(princ "\nNPN=")(princ NPN)
    (setq NPK (list (+ (car LU3) 30) (- (cadr LU3) 50)))     ;;NP-Kの挿入点
    ;;(princ "\nNPK=")(princ NPK)

    ;２枚扉の条件                内扉の真ん中の隙間 6mm
    (setq 2PW (- (/ (- PW NTW) 2) 2))                        ;;内扉2枚の内扉横幅
    ;;(princ "\n2PW=")(princ 2PW)
    (setq 2LU3 (list (+ (+ (car LU4) 2PW) 5) (cadr LU3)))    ;;支柱の左上端
    ;;(princ "\n2LU3=")(princ 2LU3)
    (setq 2LD3 (list (+ (+ (car LD4) 2PW) 5) (cadr LD3)))    ;;支柱の左下端
    ;;(princ "\n2LD3=")(princ 2LD3)
    (setq 2RU3 (list (- (- (car RU3) 2PW) 5) (cadr RU3)))    ;;支柱の右上端
    ;;(princ "\n2RU3=")(princ 2RU3)
    (setq 2RD3 (list (- (- (car RD3) 2PW) 5) (cadr RD3)))    ;;支柱の右下端
    ;;(princ "\n2RD3=")(princ 2RD3)
    (setq LU4CR (list (+ (+ (car LU4) 2PW) 5) (cadr LU4)))   ;;内扉2枚 右の左上端
    ;;(princ "\nLU4CR=")(princ LU4CR)
    (setq RD4CL (list (- (+ (car LD4) 2PW) 1) (cadr LD4)))   ;;内扉2枚 左の右下端
    ;;(princ "\nRD4CL=")(princ RD4CL)
    (setq TOTP2 (list (car PC) (- (cadr LU4) UDT)))          ;;取手の挿入点
    ;;(princ "\nTOTP2=")(princ TOTP2)
    (setq NPN2  (list (- (car RD4CL) (/ 2PW 2))(- (cadr LU4) 30))) ;;NP-Nの挿入点
    ;;(princ "\nNPN2=")(princ NPN2)
    (setq NPKL  (list (- (car 2RU3) 30) (- (cadr 2RU3) 50))) ;;NP-Kの挿入点 L
    ;;(princ "\nNPKL=")(princ NPKL)
    (setq NPKR  (list (+ (car 2LU3) 30) (- (cadr 2LU3) 50))) ;;NP-Kの挿入点 R
    ;;(princ "\nNPKR=")(princ NPKR)

;;=================================================================================================
  ;;中扉が有る場合
    (if (/= NAKA "N");中扉が1枚 or 2枚の場合
        (progn
           ;;板金用基板の作図
           (setq HDIM (list (car PC) (+ (cadr PC) 100)))
           ;;(princ "\nHDIM=")(princ HDIM)
           (setq VDIM (list (- (car LD1) 100) (cadr LD1)))
           ;;(princ "\nVDIM")(princ VDIM)

           (command "LAYER" "ON" "BANKIN" "")               ;画層 BANKINをON
           (command "LAYER" "S" "BANKIN" "")                ;画層 BANKINに変更
           (command "COLOR" 4)                              ;水色
           (command "RECTANGLE" LU4 RD4)                    ;もう一度 画層BANKINに基板を画く
           (command "DIM" "HOR" LU4 RU4 HDIM ""             ;基板横寸法 左
                          "VER" LU4 LD4 VDIM "" "EXIT")     ;基板横寸法 上
           (command "LAYER" "S" "BOX" "")                   ;画層 BOXに変更
           (command "LAYER" "OFF" "BANKIN" "")              ;画層 BANKINをOFF
           
         ;;ヒンジが４個の場合
           (if (>= PH 1600.00) 
               (progn
                  (setq DRCU (list (car RD1) (cadr RCU)))        ;寸法支柱の右中上
                  ;;(princ "\nDRCU=")(princ DRCU)
                  (setq DRCD (list (car RD1) (cadr RCD)))        ;寸法支柱の右中下
                  ;;(princ "\nDRCD=")(princ DRCD)
                  (command "INSERT" SSR RCU "1" "1" "0")         ;ヒンジ支柱（中上）
                  (command "INSERT" SSR RCD "1" "1" "0")         ;ヒンジ支柱（中下）
                  ;;(setvar "DIMDEC" 1)                          ;寸法値を正確にする為に小数点表示にする（支柱３個・４個の位置計算を変更したのでコメントアウトしておく）
                  (command "DIM" "VER" DRU RU1 RDIM ""           ;ヒンジ縦寸法 上ガター
                           "VER" DRU DRCU RDIM ""                ;ヒンジ縦寸法 上
                           "VER" DRCU DRCD RDIM ""               ;ヒンジ縦寸法 中
                           "VER" DRCD DRD RDIM ""                ;ヒンジ縦寸法 下
                           "VER" DRD RD1 RDIM "" "EXIT")         ;ヒンジ縦寸法 下ガター
                  ;;(setvar "DIMDEC" 0)
               )
           );End If
         ;;ヒンジが３個の場合
           (if (and (>= PH 1200) (< PH 1600)) 
               (progn
                  (setq RC3 (list (car RU4) (- (cadr RU3) ZH3))) ;支柱の右中心
                  ;;(princ "\nRC3=")(princ RC3)
                  (setq DRC (list (car RD1) (cadr RC3)))         ;寸法支柱の右中
                  ;;(princ "\nDRC=")(princ DRC)
                  (command "INSERT" SSR RC3 "1" "1" "0")         ;ヒンジ支柱（中）
                  (command "DIM" "VER" DRU RU1 RDIM ""           ;ヒンジ横寸法 上ガター
                           "VER" DRU DRC RDIM ""                 ;ヒンジ横寸法 上中
                           "VER" DRD DRC RDIM ""                 ;ヒンジ横寸法 中下
                           "VER" DRD RD1 RDIM "" "EXIT")         ;ヒンジ横寸法 下ガター
               )
            );End If
         ;;ヒンジが２個の場合
            (if (< PH 1200.00) (command "DIM" "VER" DRU RU1 RDIM ""    ;基板横寸法 上
                                        "VER" DRU DRD RDIM ""          ;基板横寸法 中
                                        "VER" DRD RD1 RDIM "" "EXIT")  ;基板横寸法 下
            );End If
       ;-----------------------
         ;;内扉が１枚の場合 
              (if (= NAKA "Y")
                  (progn
                     (command "COLOR" 40);オレンジ
                     (command "RECTANGLE" LU4 RD4)                 ;内扉の作図 内扉１枚
                     (command "INSERT" SSLU LU3 "1" "1" "0")       ;受支柱（上）
                     (command "INSERT" SSLD LD3 "1" "1" "0")       ;受支柱（下）
                     (command "INSERT" SSR RU3 "1" "1" "0")        ;ヒンジ支柱（上）
                     (command "INSERT" SSR RD3 "1" "1" "0")        ;ヒンジ支柱（下）
                     (command "INSERT" RLT LU3 "1" "1" "0")        ;ローレット（上）
                     (command "INSERT" RLT LD3 "1" "1" "0")        ;ローレット（下）
                     (command "INSERT" "NP-N" NPN "1" "1" "0")     ;NP-N
                     (command "INSERT" "NP-K" NPK "1" "1" "0")     ;NP-K
                   ;;ヒンジが３個以上の場合
                     (if (>= PH 1200)
                         (progn (setq LC3 (list (car LU4) (- (cadr RU3) ZH3)));;受支柱の左中心
                                ;;(princ "\nLC3=")(princ LC3)
                                (command "INSERT" SSLU LC3 "1" "1" "0")       ;;受  支  柱（中）
                                (command "INSERT" RLT LC3 "1" "1" "0")        ;;ローレット（中）
                         )
                     )
                  );progn
              );if
        ;----------------------
           ;;内扉が２枚の場合 
              (if (= NAKA "Y2")
                  (progn
                     (command "COLOR" 40);オレンジ
                     (command "RECTANGLE" LU4 RD4CL)                ;内扉の作図 内扉 2枚 左
                     (command "RECTANGLE" LU4CR RD4)                ;内扉の作図 内扉 2枚 右
                     (command "INSERT" "NP-N" NPN2 "1" "1" "0")     ;NP-N
                     (command "INSERT" "NP-K" NPKL "1" "1" "0")     ;NP-N L
                     (command "INSERT" "NP-K" NPKR "1" "1" "0")     ;NP-N R
                  ;----------------
                     ;内扉 右側
                     (command "INSERT" SSR RU3 "1" "1" "0")         ;ヒンジ支柱（上）
                     (command "INSERT" SSR RD3 "1" "1" "0")         ;ヒンジ支柱（下）
                     (command "INSERT" SSLU 2LU3 "1" "1" "0")       ;受支柱（上）
                     (command "INSERT" SSLD 2LD3 "1" "1" "0")       ;受支柱（下）
                     (command "INSERT" RLT 2LU3 "1" "1" "0")        ;ローレット（上）
                     (command "INSERT" RLT 2LD3 "1" "1" "0")        ;ローレット（下）
                     ;;ヒンジが３個の場合 （真ん中の受支柱とローレットのみを描く）
                       (if (>= PH 1200)
                           (progn (setq LC3 (list (car LU4) (- (cadr RU3) ZH3)))        ;受支柱の左中心
                                  (setq 2LC3 (list (+ (+ (car LC3) 2PW) 5) (cadr LC3))) ;受支柱の右中心
                                  ;;(princ "\nLC3=")(princ LC3)
                                  ;;(princ "\n2LC3=")(princ 2LC3)
                                  (command "INSERT" SSLD 2LC3 "1" "1" "0")              ;受支柱（中）
                                  (command "INSERT" RLT 2LC3 "1" "1" "0")               ;ローレット（中）
                           )
                       )
                   ;---------------
                     ;内扉 左側
                     (command "INSERT" SSRV LU3 "1" "1" "0")        ;ヒンジ支柱（上）V
                     (command "INSERT" SSRV LD3 "1" "1" "0")        ;ヒンジ支柱（下）V
                     (command "INSERT" SSLDV 2RU3 "1" "1" "0")      ;受支柱（上）
                     (command "INSERT" SSLUV 2RD3 "1" "1" "0")      ;受支柱（下）
                     (command "INSERT" RLTV 2RU3 "1" "1" "0")       ;ローレット（上）V
                     (command "INSERT" RLTV 2RD3 "1" "1" "0")       ;ローレット（下）V
                     ;;ヒンジが４個の場合
                       (if (>= PH 1600)
                           (progn (setq LC3 (list (car LU4) (- (cadr RU3) ZH3))) ;受支柱の左中心*
                                  (setq 2RC3 (list (- (+ (car LC3) 2PW) 1) (cadr LC3)));受支柱の右中心*
                                  (setq LCU (list (- (car RCU) (+ (* 2PW 2) 4))(cadr RCU)))
                                  (setq LCD (list (- (car RCD) (+ (* 2PW 2) 4))(cadr RCD)))
                                  ;;(princ "\nLC3=")(princ LC3)
                                  ;;(princ "\n2RC3=")(princ 2RC3)
                                  ;;(princ "\nLCU=")(princ LCU)                                  
                                  (command "INSERT" SSRV LCU "1" "1" "0")        ;;ヒンジ（中）
                                  (command "INSERT" SSRV LCD "1" "1" "0")        ;;ヒンジ（中）
                                  (command "INSERT" SSLUV 2RC3 "1" "1" "0")      ;;受支柱（中）*
                                  (command "INSERT" RLTV 2RC3 "1" "1" "0")       ;ローレット（中）
                            )
                        )
                     ;;ヒンジが３個の場合
                       (if (and (>= PH 1200)(< PH 1600))
                           (progn (setq LC3 (list (car LU4) (- (cadr RU3) ZH3))) ;受支柱の左中心
                                  (setq 2RC3 (list (- (+ (car LC3) 2PW) 1) (cadr LC3)));受支柱の右中心
                                  ;;(princ "\nLC3=")(princ LC3)
                                  ;;(princ "\n2RC3=")(princ 2RC3)
                                  (command "INSERT" SSRV LC3 "1" "1" "0")        ;ヒンジ（中）
                                  (command "INSERT" SSLUV 2RC3 "1" "1" "0")      ;受支柱（中）
                                  (command "INSERT" RLTV 2RC3 "1" "1" "0")       ;ローレット（中）
                           )
                       )
                                  ;;(princ "\nLC3=")(princ LC3)
                                  ;;(princ "\n2RC3=")(princ 2RC3)
                                  ;;(princ "\nLCU=")(princ LCU)
                                  ;;(princ "\nLCD=")(princ LCD)
                     ;;真ん中には何もいらない
                     ;;;;ヒンジが２個の場合 （真ん中の受支柱とローレットのみを描く）
                     ;;  (if (< PH 1200)
                     ;;      (progn (setq LC3 (list (car LU4) (- (cadr RU3) ZH3))) ;受支柱の左中心
                     ;;             (setq 2RC3 (list (+ (car LC3) 2PW) (cadr LC3)));受支柱の右中心
                     ;;             (command "INSERT" SSLUV 2RC3 "1" "1" "0")      ;受支柱（中）
                     ;;             (command "INSERT" RLTV  2RC3 "1" "1" "0")      ;ローレット（中）
                     ;;      )
                     ;;  )
                  );progn
              );if
        );progn
    );End If
    ;*(command "PEDIT" PC "W" "4" "")    ;;箱体外枠の線を太くする
    ;取手の挿入
    (if (and (= BTYP "TT")(= NAKA "Y")(= PSH "HO"))
        (command "INSERT" TOT TOTP "1" "1" "0"
                 "ZX" "ZX-1" "AP183-2" "" "" "" "" "" "" "" "" "34" "1" "" "")
    )
    (if (and (= BTYP "TT")(= NAKA "Y2")(= PSH "HO"))
        (progn
           (command "INSERT" TOT  TOTP2 "1" "1" "0"
                    "ZX" "ZX-1" "AP183-2" "" "" "" "" "" "" "" "" "34" "1" "" "")
           (command "INSERT" TOTV TOTP2 "1" "1" "0"
                    "ZX" "ZX-2" "AP183-2" "" "" "" "" "" "" "" "" "34" "1" "" "")
        )
     )
; 寸法記入
    (command "COLOR" 2)
    (setq DSU (list (car LU1) (+ (cadr RD2) (- PH (+ UG DG)))));寸法基板の右上端
    ;;(princ "\nDSU=")(princ DSU)
    (setq DSD (list (car LD1) (+ (cadr RD1) DG)))              ;寸法基板の右下端
    ;;(princ "\nDSD=")(princ DSD)

    (command "DIM" "HOR" DUL LU1 PDIM ""           ;基板横寸法 左
                   "HOR" DUL DUR PDIM ""           ;基板横寸法 中
                   "HOR" DUR RU1 PDIM ""           ;基板横寸法 右
                   "VER" DSU LU1 LDIM ""           ;基板横寸法 上
                   "VER" DSU DSD LDIM ""           ;基板横寸法 中
                   "VER" DSD LD1 LDIM "" "EXIT")   ;基板横寸法 下

    ;(princ "\nDUL@=")(princ DUL)
    ;(princ "\nLU1@=")(princ LU1)
    ;(princ "\nPDIM@=")(princ PDIM)
    ;(princ "\nDUR@=")(princ DUR)
    ;(princ "\nRU1@=")(princ RU1)
    ;(princ "\nDSU@=")(princ DSU)
    ;(princ "\nLDIM@=")(princ LDIM)
    ;(princ "\nDSD@=")(princ DSD)
    ;(princ "\nLD1@=")(princ LD1)



    (command "LAYER" "S" "S1" "");画層 S1に変更
    (command "COLOR" 4);水色
    
;--------------------------------------------------------------------------------------------------
;支柱等の属性書き出し
    (setq n 0)(setq HIN 0)(setq UKD 0)(setq UKU 0)(setq ROL 0) ;初期化
    (setq AA (ssget "X"))
    ;(setq AA (ssget "W" LU1 RD1))
    (while (setq BB (ssname AA n))
           (setq ENT (entget BB))
           (if (and (= (cdr (assoc 0 ENT)) "INSERT")
                    (or (= (cdr (assoc 2 ENT)) SSR)(= (cdr (assoc 2 ENT)) SSRV))  ;ヒンジの数量検出
               )
               (setq HIN (1+ HIN));個数
           )
           (if (and (= (cdr (assoc 0 ENT)) "INSERT")
                    (or (= (cdr (assoc 2 ENT)) SSLU)(= (cdr (assoc 2 ENT)) SSLUV));受金具Rの数量検出
               (setq ENTSS (cdr (assoc 2 ENT)))
               ;;(princ "\nENTSS=")(princ ENTSS)
               )
               (setq UKU (1+ UKU));個数
           )
           (if (and (= (cdr (assoc 0 ENT)) "INSERT")
                    (or (= (cdr (assoc 2 ENT)) SSLD)(= (cdr (assoc 2 ENT)) SSLDV));受金具Lの数量検出
               )
               (setq UKD (1+ UKD));個数
           )
           (if (and (= (cdr (assoc 0 ENT)) "INSERT")
                    (or (= (cdr (assoc 2 ENT)) RLT)(= (cdr (assoc 2 ENT)) RLTV))  ;ﾛｰﾚｯﾄの数量検出
               )
               (setq ROL (1+ ROL));個数 
           )
           (setq n (+ n 1))
           ;;(princ "\nn=")(princ n)
           ;;(princ "\nBB=")(princ BB)
           ;;(princ "\nENT=")(princ ENT)
    );while
   
;;金具の選定（既製BOXの場合）
   ;;(princ "\nBB=")(princ BB)
   ;;(princ "\nENT=")(princ ENT)
   ;;(princ "\nHIN=")(princ HIN)
   ;;(princ "\nUKU=")(princ UKU)
   ;;(princ "\nUKD=")(princ UKD)
   ;;(princ "\nROL=")(princ ROL)
   (if (and (= PSH "NG")(>= PD 250))                                      ;;既製BOX D=250以上
       (progn (setq K1 "BSZ-141") (setq K2 "MSZ-144R") (setq K3 "MSZ-144L")))
   (if (and (= PSH "NG")(> PH 160) (< PD 250))                         ;;既製BOX D=200未満 161以上
       (progn (setq K1 "BSZ-103") (setq K2 "MSZ-104R") (setq K3 "MSZ-104L")))
   (if (and (= PSH "NG")(<= PD 160))                                      ;;既製BOX D=160以下 120,140
       (progn (setq K1 "BSZ-79") (setq K2 "MSZ-80R") (setq K3 "MSZ-80L")))
;;特注BOXの場合（ホシモトの金具を使用の場合）D=140が最低奥行き
   (if (and (= PSH "HO")(>= PD 250))                                          ;;特注BOX D=250以上
       (progn (setq K1 "HTP-200-2B") (setq K2 "HUP-200-2B") (setq K3 "HUP-200-2B")))
   (if (and (= PSH "HO")(> PH 140) (< PD 250))                             ;;特注BOX D=250未満 140以上
       (progn (setq K1 "HTP-115-2B") (setq K2 "HUP-115-2B") (setq K3 "HUP-115-2B")))

;                          金具ﾒｰｶｰ                        ﾛｰﾚｯﾄ
   (if (= PSH "NG")(progn (setq KMK "36")(setq KMK2 "36") (setq K4 "RL-1")))                       
   (if (= PSH "HO")(progn (setq KMK "34")(setq KMK2 "102")(setq K4 "A-176-14"))) ;;090406ホシモト選択時のヒンジ・受側メーカーはタキゲンではなくホシモトにする

   (C:SZ)
   (if (= SZ "A3")(progn (setq SLI (list 4000 2625)) (setq SLI2 (list (- (car SLI) 450)(- (cadr SLI) 250)))))
   (if (= SZ "A2")(progn (setq SLI (list 5620 3690)) (setq SLI2 (list (- (car SLI) 450)(- (cadr SLI) 250)))))
   (if (= SZ "A1")(progn (setq SLI (list 8045 5275)) (setq SLI2 (list (- (car SLI) 450)(- (cadr SLI) 250)))))
   (if (= SZ "A0")(progn (setq SLI (list 11235 7385))(setq SLI2 (list (- (car SLI) 450)(- (cadr SLI) 250)))))

;; ICAD5ではcommand "ERASE" "W"で"W"内に削除図形が無いとエラーが出るので、仮に"KOUTEN"を挿入してから削除を行う
   (setq TMP1 (list (- (car SLI) 50)(- (cadr SLI) 50)))
   (command "INSERT" "KOUTEN" TMP1  "1" "1" "0")     
   ;;(princ "\nSLI = ")(princ SLI)
   ;;(princ "\nSLI2=")(princ SLI2)
   (command "ERASE" "W" SLI SLI2 "")
   (setq ZI1 (list (- (car SLI) 440)(- (cadr SLI) 75))) ;ヒンジの挿入点 
   ;;(princ "\nZI1=")(princ ZI1)
   (setq ZI2 (list (car ZI1) (- (cadr ZI1) 40)))        ;受Lの挿入点
   ;;(princ "\nZI2=")(princ ZI2)
   (setq ZI3 (list (car ZI1) (- (cadr ZI2) 40)))        ;受Rの挿入点
   ;;(princ "\nZI3=")(princ ZI3)   
   (setq ZI4 (list (car ZI1) (- (cadr ZI3) 40)))        ;ﾛｰﾚｯﾄの挿入点
   ;;(princ "\nZI4=")(princ ZI4)
   (setq ZI5 (list (car ZI1) (- (cadr ZI4) 40)))        ;BOXDの挿入点
   ;;(princ "\nZI5=")(princ ZI5)
   (command "INSERT" "S-LIST" SLI "1" "1" "0")
   (command "INSERT" "BOXD" ZI5 "1" "1" "0" "" PDD)     ;箱の奥行を表示するﾌﾞﾛｯｸ
   (if (= PSH "NG")
       (progn
          (command "INSERT" "支柱金具" ZI1 "1" "1" "0"
                            "KANA" "ヒンジ側" K1 "" "" "" "" "" "" "" "" KMK2 HIN "" "")
          (command "INSERT" "支柱金具" ZI2 "1" "1" "0"
                            "KANA" "受  側" K2 "" "" "" "" "" "" "" "" KMK2 UKU "" "")
          (command "INSERT" "支柱金具" ZI3 "1" "1" "0"
                            "KANA" "受  側" K3 "" "" "" "" "" "" "" "" KMK2 UKD "" "")
          (command "INSERT" "支柱金具" ZI4 "1" "1" "0"
                            "KANA" "ローレット" K4 "" "" "" "" "" "" "" "" KMK ROL "" "")
        )
   )
   (if (= PSH "HO")
       (progn
          (command "INSERT" "支柱金具" ZI1 "1" "1" "0"
                            "KANA" "ヒンジ側" K1 "" "" "" "" "" "" "" "" KMK2 HIN "" "")
          (command "INSERT" "支柱金具" ZI2 "1" "1" "0"
                            "KANA" "受  側"   K2 "" "" "" "" "" "" "" "" KMK2 UKU "" "")
          (command "INSERT" "支柱金具" ZI4 "1" "1" "0"
                            "KANA" "ローレット" K4 "" "" "" "" "" "" "" "" KMK ROL "" "")
        )
   )
;;------------------------------------------------------------------------------
;;ケーブルサポートの追加
    (if (= CBL "Y")
      (progn
        
        (setq PCCAX1 0)(setq PCCAX2 0)
        (setq PCCAY1 0)(setq PCCAY2 0)(setq PCCAY3 0)
        
        (if (= PH 400) (progn (setq PCCAY1 200)(setq PCCAY2 0)   (setq PCCAY3 0)    ))
        (if (= PH 500) (progn (setq PCCAY1 250)(setq PCCAY2 0)   (setq PCCAY3 0)    ))
        (if (= PH 600) (progn (setq PCCAY1 300)(setq PCCAY2 0)   (setq PCCAY3 0)    ))
        (if (= PH 700) (progn (setq PCCAY1 350)(setq PCCAY2 0)   (setq PCCAY3 0)    ))
        (if (= PH 800) (progn (setq PCCAY1 200)(setq PCCAY2 600) (setq PCCAY3 0)    ))
        (if (= PH 900) (progn (setq PCCAY1 200)(setq PCCAY2 700) (setq PCCAY3 0)    ))
        (if (= PH 1000)(progn (setq PCCAY1 200)(setq PCCAY2 800) (setq PCCAY3 0)    ))
        (if (= PH 1100)(progn (setq PCCAY1 200)(setq PCCAY2 900) (setq PCCAY3 0)    ))
        (if (= PH 1200)(progn (setq PCCAY1 200)(setq PCCAY2 1000)(setq PCCAY3 0)    ))
        (if (= PH 1300)(progn (setq PCCAY1 200)(setq PCCAY2 620) (setq PCCAY3 1100) ))
        (if (= PH 1400)(progn (setq PCCAY1 200)(setq PCCAY2 670) (setq PCCAY3 1200) ))
        (if (= PH 1500)(progn (setq PCCAY1 200)(setq PCCAY2 720) (setq PCCAY3 1300) ))
        (if (= PH 1600)(progn (setq PCCAY1 200)(setq PCCAY2 770) (setq PCCAY3 1400) ))
        (if (= PH 1700)(progn (setq PCCAY1 200)(setq PCCAY2 820) (setq PCCAY3 1500) ))
        (if (= PH 1800)(progn (setq PCCAY1 200)(setq PCCAY2 870) (setq PCCAY3 1600) ))
        (if (= PH 1900)(progn (setq PCCAY1 200)(setq PCCAY2 920) (setq PCCAY3 1700) ))
        (if (= PH 2000)(progn (setq PCCAY1 200)(setq PCCAY2 970) (setq PCCAY3 1800) ))
        
        (setq PCCAX1 (- (car PC)(/ PW 2)))
        (setq PCCAX2 (+ (car PC)(/ PW 2)))
        
        ;;(princ "\nPC=")    (princ PC)
        ;;(princ "\nPCCAY1=")(princ PCCAY1)
        ;;(princ "\nPCCAY2=")(princ PCCAY2)
        ;;(princ "\nPCCAY3=")(princ PCCAY3)
        ;;(princ "\nPCCAX1=")(princ PCCAX1)
        ;;(princ "\nPCCAX2=")(princ PCCAX2)
        
        (command "LINETYPE" "S" "DASHED" "")
        
        (if (/= PCCAY1 0)
            (progn
                (setq PCCA (list (+ PCCAX1 20)(- (cadr PC) PCCAY1)))
                (command "INSERT" "CASUPPORT" PCCA "2" "1" "0")
                (setq PCCA (list (+ PCCAX2 0) (- (cadr PC) PCCAY1)))
                (command "INSERT" "CASUPPORT" PCCA "2" "1" "0")
            )
        )
        (if (/= PCCAY2 0)
            (progn
                (setq PCCA (list (+ PCCAX1 20)(- (cadr PC) PCCAY2)))
                (command "INSERT" "CASUPPORT" PCCA "2" "1" "0")
                (setq PCCA (list (+ PCCAX2 0) (- (cadr PC) PCCAY2)))
                (command "INSERT" "CASUPPORT" PCCA "2" "1" "0")
            )
        )
        (if (/= PCCAY3 0)
            (progn
                (setq PCCA (list (+ PCCAX1 20)(- (cadr PC) PCCAY3)))
                (command "INSERT" "CASUPPORT" PCCA "2" "1" "0")
                (setq PCCA (list (+ PCCAX2 0) (- (cadr PC) PCCAY3)))
                (command "INSERT" "CASUPPORT" PCCA "2" "1" "0")
            )
        )
                
        (command "LINETYPE" "S" "CONTINUOUS" "")
      )
    )
;------------------------------------------------------------------------------
;終了ルーチン
  (command "ZOOM" "E")   ;*内外用に変更 図枠範囲内にもどる
  (setvar "USERI5" 0);Access自動作図のフラグ
  (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)(redraw)
  (princ "\n作図終了")(princ)
)

