;;==================================================================================================
;;チャンネル図の作成    CHAN_B.LSP        2005/Apr/16     BY T.Terada
;;==================================================================================================
(defun C:CHAN_B(/ SS ENAM EDAT ERR AA BB CC1 DD1 n ENT P1 P2 LU1 LD1 RU1 RD1 PDIM PDIM1 PDIM2 PDIM PDIM4 PDIM6
                  SDIM SDIM1 PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 BD1 BW1 BH1 PC PFX PZ PZ1 COL BKATA CKATA SPD
                  KEY KEYL B1 B1C B2 B2C B3 B3C IP Bt Dt FRH LSU ULU1 URU1 ULD1 FU1 FD1 PCCU PCCD D2T D4T
                  LCD RCD CCL CCR WPC WLU WRD WPC2 WLU2 WPT LU2D LD2D LD2B RP1 RP2 RP3 RP4 RP5 NU ND DNR DNL
                  RU22 LU22 SLCD SRCD SCCL SCCR LU2 RD2 RU2 IUR IUL IDR IDL ZB1 ZB2 ZB3 ZB4 ZB5 ZB6 ZB7 ZB8
                  ZB9 ZB10 CAH DIMH1 DIMH2 HH PDY PDYL PDYR STL DSTR DSTL CHAN RINL RINR RINS FDO FDI WW WH WC
                  PU1 PU2 PU3 PU4 PD1 PD2 PD3 PD4 DD2 FU2 FD2 SP SZ WS OFFS ID LL BH
               )
;初期設定
;   (setq *error* *err1*)
   (setvar "CMDECHO" 1) 
   (command "COLOR" 4)(setvar "PLINEWID" 0)  ;線幅を0
   (command "LINETYPE" "S" "CONTINUOUS" "")(command "LAYER" "S" "BOX" "");画層 BOXに変更
;;---------------------------------------------------------------------------------------------------
    (DCL_CHANB)
     (setq BW (getreal  "\n横幅を指示してください．(MAX=2500) :"))
     (setq BD (getreal  "\n奥行を指示してください．(MAX=2200) :"))
     (setq PC (getpoint "\n挿入点を指示して下さい．(上端の中心点):"))

;;チャンネルのサイズの判定
    (setq BH 0)(setq FRH 30) (setq BD1 (rtos BD) BW1 (rtos BW))
    (if (= KT "KB")
        (progn
             (if (or (= BD1 "160")(= BD1 "180")(= BD1 "200")(= BD1 "250")(= BD1 "300")(= BD1 "350"))
                 (princ "\n奥行きﾁｪｯｸ OK")
                 (progn
                    (alert "既製TEチャンネルには その奥行サイズは ありません！")
                    (princ "\n終了します。もう一度やり直してください！！")
                    (exit)
                 )
             )
        )
    )
;;--------------------------------------------------------------------------------------------------
;;既製ﾁｬﾝﾈﾙﾍﾞｰｽ 型式番号の合成と型番ﾁｪｯｸ
   (if (and (= KT "KB")(= FU "F")) ;既製BOXの場合
       (progn;1
          (setq BD1 (rtos BD)) (setq BW1 (rtos BW))
          (if (or
                 (and (= BD1 "120")
                      (or (= BW1 "400")(= BW1 "450")(= BW1 "500")(= BW1 "600")(= BW1 "800")(= BW1 "900")))
                 (and (= BD1 "140")
                      (or (= BW1 "400")(= BW1 "450")(= BW1 "500")(= BW1 "600")
                          (= BW1 "700")(= BW1 "800")(= BW1 "900")))
                 (and (= BD1 "160")
                      (or (= BW1 "400")(= BW1 "450")(= BW1 "500")(= BW1 "600")(= BW1 "700")
                          (= BW1 "800")(= BW1 "900")(= BW1 "1000")(= BW1 "1200")))
                 (and (= BD1 "180")
                      (or (= BW1 "500")(= BW1 "1000")))
                 (and (= BD1 "200")
                      (or (= BW1 "400")(= BW1 "450")(= BW1 "500")(= BW1 "600")(= BW1 "700")
                      (= BW1 "800")(= BW1 "900")(= BW1 "1000")(= BW1 "1200")(= BW1 "1400")))
                 (and (= BD1 "250")
                      (or (= BW1 "500")(= BW1 "600")(= BW1 "700")(= BW1 "800")(= BW1 "1000")(= BW1 "1200")))
                 (and (= BD1 "300")
                      (or (= BW1 "500")(= BW1 "600")(= BW1 "700")(= BW1 "800")(= BW1 "1000")(= BW1 "1200")))
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
                 (setq CKATA (strcat "TE" B2C B1C "-" B3C "N"))
                 
                 (if (= KT "KB")(progn (setq BKIKI "KB")(setq BMEIKA "39")))
                 (if (= KT "TB")(progn (setq BKIKI "BB")(setq BMEIKA "100")))
                 ;;(command "TEXT" "S" "M0" "C" PZ1 "30" "0" CKATA)
                 (command "INSERT" "ZOKU" PZ1 "1" "1" "0" BKIKI "" CKATA "" "" "" "" "" "" "" "" BMEIKA "1" "" "")
              );2
              (progn;3 else
                 (alert "既製ﾁｬﾝﾈﾙﾍﾞｰｽには そのサイズは ありません！")
                 (princ "\n終了します。もう一度やり直してください！！ CH")
                 (exit)
              );3
          );if
       );1
   );if
;;-----------------------------------------------------------------------------
;箱体の外枠と寸法
    (setq LU1 (list (- (car PC) (/ BW 2)) (cadr PC)))        ;左上端
    (setq RU1 (list (+ (car PC) (/ BW 2)) (cadr PC)))        ;右上端
    (setq LD1 (list (- (car PC) (/ BW 2)) (- (cadr PC) BH))) ;左下端
    (setq RD1 (list (+ (car PC) (/ BW 2)) (- (cadr PC) BH))) ;右下端
    (if (= WP "WP")(setq DIMH1 200)(setq DIMH1 150))         ;PDIM  屋根付きの場合の寸法位置
    (if (= WP "WP")(setq DIMH2 150)(setq DIMH2 100))         ;PDIM1 屋根付きの場合の寸法位置
    (setq PDIM (list (car PC) (+ (cadr PC) DIMH1)))          ;寸法線のY位置
    (setq PDIM1 (list (car PC) (+ (cadr PC) DIMH2)))         ;寸法線のY位置1
    (setq PDIM2 (list (car LD1) (- (cadr LD1) 50)))          ;寸法線のY位置2
    (setq PDIM3 (list (car LD1) (- (cadr LD1) 175)))         ;寸法線のY位置3
    (setq PDIM4 (list (car PC) (- (cadr PC) 100)))           ;寸法線のY位置4
    (setq SDIM (list (- (car LU1) 175) (cadr LU1)))          ;寸法線のX位置
    (setq SDIM1 (list (- (car LU1) 100) (cadr LU1)))         ;寸法線のX位置1

;;------------------------------------------------------------------------------------------------------------
;正面チャンネル図
    (if (and (= KT "KB")(= FU "F"));既製BOXの場合
        (progn
           (setq HH 100)                                       ;既製BOXの場合のﾁｬﾝﾈﾙ高さ
           (setq LCD (list (car LD1) (- (cadr LD1) HH)))       ;ﾁｬﾝﾈﾙ左下
           (setq RCD (list (car RD1) (- (cadr RD1) HH)))       ;ﾁｬﾝﾈﾙ右下
           (setq CCL (list (+ (car LCD) 30) (+ (cadr LCD) 30)));ﾁｬﾝﾈﾙｱﾝｶｰ穴L
           (setq CCR (list (- (car RCD) 30) (+ (cadr RCD) 30)));ﾁｬﾝﾈﾙｱﾝｶｰ穴R
           (setq CAH (list (+ (car LCD) 30) (cadr LCD)))       ;ﾁｬﾝﾈﾙｱﾝｶｰ穴の寸法ｼﾝﾎﾞﾙ挿入点
           (command "RECTANGLE" LD1 RCD)                       ;ﾁｬﾝﾈﾙの描画
           (command "INSERT" "4-16" CAH "1" "1" "0")           ;ﾁｬﾝﾈﾙｱﾝｶｰ穴の寸法ｼﾝﾎﾞﾙ挿入
           (command "COLOR" 2);黄色
           (command "DIM" "HOR" CCL LCD PDIM3 ""
                          "HOR" CCL CCR PDIM3 ""
                          "HOR" CCR RCD PDIM3 ""
                          "VER" LD1 LCD SDIM1 "" "EXIT");ﾁｬﾝﾈﾙの高さ
        );progn
    )

    (if (and (= KT "TB")(= FU "F"));特注BOXの場合
        (progn
           (setq HH 50)
           (setq LCD (list (car LD1) (- (cadr LD1) HH)))                 ;ﾁｬﾝﾈﾙ左下
           (setq PDY  (list (car PC)(+ (cadr PC) 8000)))                 ;ﾁｬﾝﾈﾙの仮挿入点
           (setq PDYL (list (- (car PDY) 1550)(- (cadr PDY) 350)))       ;ｽﾄﾚｯﾁ左ﾌｪﾝｽ点
           (setq PDYR (list (+ (car PDY) 1550)(- (cadr PDY) 350)))       ;ｽﾄﾚｯﾁ右ﾌｪﾝｽ点
           (setq STL  (/ (- BW 400.00) 2))                               ;ｽﾄﾚｯﾁ距離 400=BLOCKの幅
           (setq DSTR (list (+ (car PDY) STL)(cadr PDY)))                ;ｽﾄﾚｯﾁ目標点
           (setq DSTL (list (- (car PDY) STL)(cadr PDY)))                ;ｽﾄﾚｯﾁ目標点
           
           (if (= WP "WN");屋根無しの場合
               (progn
                  (setq CHAN "*CHAN-IN")
;                  (setq LCD (list (car LD1) (- (cadr LD1) HH)))                ;ﾁｬﾝﾈﾙ左下
;                  (command "DIM" "VER" LU1 LCD SDIM "" "EXIT")                 ;全体の高さ
               )
           )
           (if (= WP "WP");屋根付きの場合
               (progn
                  (setq HH 100)
                  (setq CHAN "*CHAN-WP")
                  (setq LCD (list (car LD1) (- (cadr LD1) HH)))                ;ﾁｬﾝﾈﾙ左下
               )
           )
           (command "INSERT" CHAN PDY "1" "0")
           (command "STRETCH" "C" PDY PDYR "" PDY DSTR)
           (command "STRETCH" "C" PDY PDYL "" PDY DSTL)
           (command "MOVE" "C" (list (car PDYL)(cadr PDY)) PDYR "" PDY (list (car PDY)(cadr LD1)))
        );progn
    );if
    (command "COLOR" 4)

;;------------------------------------------------------------------------------------------------------------
;;側面図のチャンネル図－右側
    (setq LU2 (list (+ (car PC) (+ (/ BW 2) 500)) (cadr PC)));;左上端
    (setq RU2 (list (+ (car LU2) BD) (cadr PC)))             ;;右上端
    (setq LD2 (list (car LU2) (- (cadr LU2) BH)))            ;;左下端
    (setq RD2 (list (+ (car LU2) BD) (- (cadr LU2) BH)))     ;;右下端

    (if (and (= KT "KB")(= FU "F"));既製BOXの場合
        (progn
           (setq SLCD (list (car LD2) (- (cadr LD2) HH)))         ;ﾁｬﾝﾈﾙ左下
           (setq SRCD (list (car RD2) (- (cadr RD2) HH)))         ;ﾁｬﾝﾈﾙ右下
           (setq SCCL (list (+ (car SLCD) 30) (+ (cadr SLCD) 30)));ﾁｬﾝﾈﾙｱﾝｶｰ穴L
           (setq SCCR (list (- (car SRCD) 30) (+ (cadr SRCD) 30)));ﾁｬﾝﾈﾙｱﾝｶｰ穴R
           (command "COLOR" 4)
           (command "RECTANGLE" LD2 SRCD)                         ;ﾁｬﾝﾈﾙ
           (command "COLOR" 2)                                    ;黄色
           (command "DIM" "HOR" SCCL SLCD PDIM3 ""
                          "HOR" SCCL SCCR PDIM3 ""
                          "HOR" SCCR SRCD PDIM3 "" "EXIT")
        );progn
    );if
    (if (and (= KT "TB")(= FU "F"));特注BOXの場合
        (progn (princ "\n特注BOXの場合")
           (setq HH 50)
           (setq PDY  (list (- (car RU2) (/ BD 2))(+ (cadr RU2) 8000)))   ;ﾁｬﾝﾈﾙの仮挿入点
           (setq PDYL (list (- (car PDY) 1200)(- (cadr PDY) 350)))        ;ｽﾄﾚｯﾁ左ﾌｪﾝｽ点
           (setq PDYR (list (+ (car PDY) 1200)(- (cadr PDY) 350)))        ;ｽﾄﾚｯﾁ右ﾌｪﾝｽ点
           (setq STL  (/ (- BD 160) 2))                                   ;ｽﾄﾚｯﾁ距離 160=BLOCKの幅
           (setq DSTR (list (+ (car PDY) STL)(cadr PDY)))                 ;ｽﾄﾚｯﾁ目標点 R
           (if (= LM "L") ;落込みの場合
               (setq DSTL (list (- (car PDY) STL)(cadr PDY)))             ;ｽﾄﾚｯﾁ目標点 L
           )
           (if (= LM "M");突合せの場合
               (progn                                        ;DD1=扉の厚み DD2=扉～本体までの寸法
                  (if (= KT "KB")                 (progn (setq DD1 20)(setq DD2 24)));既製BOX
                  (if (and (= KT "TB")(= KJ "KK"))(progn (setq DD1 25)(setq DD2 30)));特注BOX 壁掛
                  (if (and (= KT "TB")(= KJ "JJ"))(progn (setq DD1 35)(setq DD2 40)));特注BOX 壁掛
                  (setq DSTL (list (- (car PDY) (- STL DD2))(cadr PDY)))             ;ｽﾄﾚｯﾁ目標点 R
               )
           )
           (if (= WP "WN");屋内の場合
               (progn 
                  (setq CHAN "*CHAN-INS")
                  (setq LCD (list (car LD1) (- (cadr LD1) HH)))                ;ﾁｬﾝﾈﾙ左下
               )
           )
           (if (= WP "WP");屋外の場合
               (progn 
                  (setq HH 100)
                  (setq CHAN "*CHAN-WPS")
                  (setq LCD (list (car LD1) (- (cadr LD1) HH)))                ;ﾁｬﾝﾈﾙ左下
                  (setq LSU (list (car LU1) (+ (cadr LU1) FRH)))
               )
           )
           (command "INSERT" CHAN PDY "1" "0")
           (command "STRETCH" "C" PDY PDYR "" PDY DSTR)
           (command "STRETCH" "C" PDY PDYL "" PDY DSTL)
           (command "MOVE" "C" (list (car PDYL)(cadr PDY)) PDYR "" PDY (list (car PDY)(cadr LD2)))
        );progn
    );if
    (command "COLOR" 4)
;;------------------------------------------------------------------------------------------------------------
;;上面チャンネル図
    (setq LU2 (list (- (car PC) (/ BW 2)) (cadr PC)))        ;左上端
    (setq RU2 (list (+ (car PC) (/ BW 2)) (cadr PC)))        ;右上端
    (setq LD2 (list (- (car PC) (/ BW 2)) (- (cadr PC) BD))) ;左下端
    (setq RD2 (list (+ (car PC) (/ BW 2)) (- (cadr PC) BD))) ;右下端
;    (if (= WP "WP")(setq DIMH1 200)(setq DIMH1 150))         ;PDIM  屋根付きの場合の寸法位置
;    (if (= WP "WP")(setq DIMH2 150)(setq DIMH2 100))         ;PDIM1 屋根付きの場合の寸法位置
;    (setq PDIM (list (car PC) (+ (cadr PC) DIMH1)))          ;寸法線のY位置
;    (setq PDIM1 (list (car PC) (+ (cadr PC) DIMH2)))         ;寸法線のY位置1
;    (setq PDIM2 (list (car LD1) (- (cadr LD1) 50)))          ;寸法線のY位置2
;    (setq PDIM3 (list (car LD1) (- (cadr LD1) 175)))         ;寸法線のY位置3
;    (setq PDIM4 (list (car PC) (- (cadr PC) 100)))           ;寸法線のY位置4
;    (setq SDIM (list (- (car LU1) 175) (cadr LU1)))          ;寸法線のX位置
;    (setq SDIM1 (list (- (car LU1) 100) (cadr LU1)))         ;寸法線のX位置1
    (if (and (= KT "KB")(= FU "U"));既製BOXの場合
        (progn
           (command "RECTANGLE" LU2 RD2)                         ;ﾁｬﾝﾈﾙ
;ここを製作する事！！！！！！！！！！！！！！！！


        )
    )
;;--------------------------------------------------------------------------------------------------
;;終了処理
   (command "LAYER" "S" "S1" "")(setvar "PLINEWID" 0)(command "COLOR" 4)(setvar "USERS5" "M")
   (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)(princ "\n箱体外形図、作画終了！")(redraw)
)
;;--------------------------------------------------------------------------------------------------
(defun DCL_CHANB(/)
      (setq DCL_ID (load_dialog "X:/LSP-I/KD.DCL"))
          (if (not (new_dialog "CHAN_B" DCL_ID) )(exit))
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "F"  "(setq FU \"F\")") (action_tile "U"  "(setq FU \"U\")")
          (action_tile "KB" "(setq KT \"KB\")")(action_tile "TB" "(setq KT \"TB\")")
          (action_tile "L"  "(setq LM \"L\")") (action_tile "M"  "(setq LM \"M\")")
          (action_tile "WN" "(setq WP \"WN\")")(action_tile "WP" "(setq WP \"WP\")")
      (start_dialog) (unload_dialog DCL_ID) (princ)
)
