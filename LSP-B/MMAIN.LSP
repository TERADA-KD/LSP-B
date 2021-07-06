;;=============================================================================================================
;;動力盤・分電盤の主幹入力    MMAIN.LSP      2004/Feb/04   By T.Terada
;;NF-50HRW,NV50-HRWには、対応しません。
;;主幹接続バー無しの電線サイズを検証の事。
;;主端子台・主受けバーの外形図をINSERT時に登録する事。行487 501
;;以下のEXCELを参照しています。
;;   X:/XLS/ブレーカー外形図対応表.XLS
;;   X:/XLS/分電盤－サポート台選定
;;=============================================================================================================
(defun C:MMAIN( / KN SOU VV PE KAN MTB MCu SQE Pn MYN BTY AF AT AL AX BCu WH PL AM 2Pn BrB BKH WW P1 P2 BrP BrT
                  PC PC1 PC2 PC2 DP1 DP2 DAT1 MK PE2 STD GYO RETU RETUG KATA DMCB ALX mA BMN PLK PLP PLT FK FMN
                  DF SCu Sq XS TBRNo TBSNo TBTNo MCBNo FNo PLNo XR XY FP-1 FP-2 FP-3 FP-4 n N2 N3 AA BB CC DD ENT
                  Br1No Br2No Br3No Br4No Cu1No Cu2No Cu3No NFZ2 TBZ2 PLZ2 BrZ2 CuZ2 F-Z2 D-MCB D-F D-PC D-PL
                  D-TBR D-TBS D-TBT D-Br1 D-Br2 D-Br3 D-Br4 D-Cu1 D-Cu2 D-Cu3 D-RST ZOKU ATT SEN LN AF2 CuZ PNo
                  BSP1 BSP2 BSP3 D-CuR D-CuN D-CuT KCuR KCuN KCuT APC WPC WPC2 M-TBAMC AMC2 TBZ DATT MKK
                  RETU1 RETU2 RETU3 RETU4 CEL1 CEL2 CEL3 CEL4 MUMA SH SHT AT2 PEAL PCM AFB T1CuR T1CuN T1CuT
              )

;;変数の初期化
(setq KN "")(setq SOU "")(setq VV "")(setq PE "")(setq KAN "")(setq MTB "")(setq MCu "")(setq SQE "")(setq Pn "")
(setq MYN "")(setq BTY "")(setq AF "")(setq AT "")(setq AL "")(setq AX "")(setq BCu "")(setq WH "")(setq PL "")
(setq AM "")(setq 2Pn "")(setq BrB "")(setq BKH "")(setq WW "")(setq P1 "")(setq P2 "")(setq BrP "")(setq BrT "")
(setq PC "")(setq PC1 "")(setq PC2 "")(setq DP1 "")(setq DP2 "")(setq DAT1 "")(setq MK "")(setq PE2 "")(setq STD "")    
(setq GYO "")(setq RETU "")(setq RETUG "")(setq KATA "")(setq DMCB "")(setq ALX "")(setq mA "")(setq BMN "")
(setq PLK "")(setq PLP "")(setq PLT "")(setq FK "")(setq FMN "")(setq DF "")(setq SCu "")(setq Sq "")(setq XS "")
(setq TBRNo "")(setq TBSNo "")(setq TBTNo "")(setq MCBNo "")(setq FNo "")(setq PLNo "")(setq XR "")(setq XY "")
(setq FP-1 "")(setq FP-2 "")(setq FP-3 "")(setq FP-4 "")(setq n "")(setq N2 "")(setq N3 "")(setq AA "")(setq BB "")
(setq CC "")(setq DD "")(setq ENT "")(setq Br1No "")(setq Br2No "")(setq Br3No "")(setq Br4No "")(setq Cu1No "")
(setq Cu2No "")(setq Cu3No "")(setq NFZ2 "")(setq TBZ2 "")(setq PLZ2 "")(setq BrZ2 "")(setq CuZ2 "")(setq F-Z2 "")
(setq D-MCB "")(setq D-F "")(setq D-PC "")(setq D-PL "")(setq D-TBR "")(setq D-TBS "")(setq D-TBT "")(setq D-Br1 "")
(setq D-Br2 "")(setq D-Br3 "")(setq D-Br4 "")(setq D-Cu1 "")(setq D-Cu2 "")(setq D-Cu3 "")(setq D-RST "")
(setq D-RST "")(setq ZOKU "")(setq ATT "")(setq SEN "")(setq LN "")(setq AF2 "")(setq CuZ "")(setq PNo "")
(setq BSP1 "")(setq BSP2 "")(setq BSP3 "")(setq D-CuR "")(setq D-CuN "")(setq D-Cut "")(setq KCuR "")(setq KCuN "")
(setq KCuT "")(setq APC "")(setq WPC "")(setq WPC2 "")(setq M-TBAMC "")(setq AMC2 "")(setq TBZ "")(setq DATT "")
(setq RETU1 "")(setq RETU2 "")(setq RETU3 "")(setq RETU4 "")(setq CEL1 "")(setq CEL2 "")(setq CEL3 "")(setq CEL4 "")
(setq MUMA "")(setq SH "")(setq SHT "")(setq AT2 "")(setq PEAL "")(setq PCM "")(setq AFB "")(setq T1CuR "")(setq MKK "")
(setq T1CuN "")(setq T1CuT "")

    (setq *error* *err1*)
    (setvar "CMDECHO" 0)
    (command "COLOR" 4) ;水色
    (C:MKR)                                 ;基本メーカーを調べる

;;C++ ダイアログの読み込み
    (if (= (getvar "USERI5") 0)(progn (MCB_IN) (if (= CAN "Y")(exit))))
    (setq DAT1 (read MDAT1));; MDAT1の読み込み；MDAT1はACCES or C++ MCB_INで生成されるLIST

;;ﾒｰｶｰの設定
    (if (= MAKER "0")(progn (setq MK "M")(setq MN "1"))) ;三菱
    (if (= MAKER "1")(progn (setq MK "F")(setq MN "4"))) ;富士
    (if (or (= MAKER "2")(= MAKER "3"))(progn (setq MK "T")(setq MN "7"))) ;寺崎

;;ACCES or MCB_INのMDATからもらうﾃﾞｰﾀｰ
    (setq KN  (nth 0 DAT1)) ; 幹線の順番１～４ :"1"
    (setq PE  (nth 1 DAT1)) ; 幹線の相・電圧   :コンボから選択された値
                            ; "1P 2W 105V""1P 3W 210V""3P 3W 210V""3P 3W 440V"
    (setq KAN (nth 2 DAT1)) ; 幹線番号         :Text値
    (setq MTB (nth 3 DAT1)) ; 主端子台         :"Y"or"N"
    (setq MCu (nth 4 DAT1)) ; 主バー受け       :"Y"or"N"
    (setq SQE (nth 5 DAT1)) ; 幹線ｻｲｽﾞ         :Text値
    (setq Pn  (nth 6 DAT1)) ; 極数             :"2P"or"3P"or"4P"or""
    (setq MYN (nth 7 DAT1)) ; 主幹ﾌﾞﾚｰｶｰの有無 :"N"or"Y"
    (setq BTY (nth 8 DAT1)) ; 主幹ﾌﾞﾚｰｶｰタイプ :"MCB"or"ELB"or"AMCB"
    (if (= BTY "EAL") (setq BTY "AMCB"))
    
    (setq AF  (nth 9 DAT1)) ; AF ﾌﾚｰﾑ 値       :リストから選択されたText値
    (setq AT  (nth 10 DAT1)); AT ﾄﾘｯﾌﾟ値       :リストから選択されたText値
    (setq AL  (nth 11 DAT1)); AL               :"Y"or"N"
    (setq AX  (nth 12 DAT1)); AX               :"Y"or"N"
    (setq BCu (nth 13 DAT1)); 接続バーの種類   :コンボから選択された値("N""BL""BC""BR""DN""MB")
    (setq WH  (nth 14 DAT1)); WHの有無         :"Y"or"N"
    (setq PL  (nth 15 DAT1)); PLの有無         :コンボから選択された値
                            ; "N" :PL無し       "D10" :電球 100V        "L10" :LED  100V
                            ;                   "D20" :電球 200V        "L20" :LED  200V
                            ;                   "D40" :電球 400V Tr付き "L40" :LED  400V Tr付き
    (setq AM  (nth 16 DAT1)); AMの有無         :"Y"or"N"
    (setq 2Pn (nth 17 DAT1)); １次分岐２Ｐの数 :コンボから選択された値("1""2""3""4")
    (setq BrB (nth 18 DAT1)); １次分岐ブレーカ :"SM"or"SB"or"BH"or""
    (setq BKH (nth 19 DAT1)); 主幹ﾌﾞﾚｰｶｰ       :"K" :経済型   "H" :汎用型
    (setq SH  (nth 20 DAT1)); 支柱の高さ       :"0" :支柱無し,"1":支柱=82, "2":支柱=106, "3":支柱=146
    (setq T6  (nth 21 DAT1)); ALAXの形式       :"N" :無し,"L":リード線, "T":端子台
    (if (= T6 "N")(setq T6 ""))
;    (princ "\nBrB=")(princ BrB)
;    (princ "\nBCu=")(princ BCu)
;---------------------------------------------------------------------------------------------------
;;主幹無しの場合の仮設定
    (if (and (= MYN "N")(= AF "30AF"))
        (progn (setq Pn "3P")(setq AT "30AT")(setq AMC "")(setq ALX "")(setq AMC2 "")))
    (if (and (= MYN "N")(= AF "50AF"))
        (progn (setq Pn "3P")(setq AT "50AT")(setq AMC "")(setq ALX "")(setq AMC2 "")))
    (if (and (= MYN "N")(= AF "100AF"))
        (progn (setq Pn "3P")(setq AT "100AT")(setq AMC "")(setq ALX "")(setq AMC2 "")))
    (if (and (= MYN "N")(= AF "225AF"))
        (progn (setq Pn "3P")(setq AT "225AT")(setq AMC "")(setq ALX "")(setq AMC2 "")))
    (if (and (= MYN "N")(= AF "400AF"))
        (progn (setq Pn "3P")(setq AT "400AT")(setq AMC "")(setq ALX "")(setq AMC2 "")))
    (if (and (= MYN "N")(= AF "600AF"))
        (progn (setq Pn "3P")(setq AT "600AT")(setq AMC "")(setq ALX "")(setq AMC2 "")))
    (if (and (= MYN "N")(= AF "800AF"))
        (progn (setq Pn "3P")(setq AT "800AT")(setq AMC "")(setq ALX "")(setq AMC2 "")))
;;幹線の相・電圧
    (setq SOU (substr PE 1 1))
    (setq WW  (substr PE 4 1))
    (setq VV  (substr PE 7))
;   (princ "\nSOU=")(princ SOU)(princ "\nVV =")(princ VV)
    (if (and (= SOU "1")(= VV "105V"))           (setq PE2 "1φ2W 105V"))
    (if (and (= SOU "1")(= VV "105/210V"))       (setq PE2 "1φ3W 105/210V"))
    (if (and (= SOU "1")(= VV "210V"))           (setq PE2 "1φ2W 210V"))
    (if (and (= SOU "3")(= VV "210V"))           (setq PE2 "3φ3W 210V"))
    (if (and (= SOU "3")(= WW "3")(= VV "440V")) (setq PE2 "3φ3W 440V"))
    (if (and (= SOU "3")(= WW "4")(= VV "440V")) (setq PE2 "3φ4W 440V"))
    (if (and (= SOU "D")(= VV "105V"))           (setq PE2 "DC 105V"))

    (if 
        (and 
            (= SOU "3")(= 2Pn "1")
        );and
            (progn 
               (alert "3φ3W選択時に、１次分岐ブレーカーは使用できません。処理を中止します")(exit)
            );progn
    );if
    
    (if
        (and
            (= SOU "3")(= 2Pn "2")
        );and
            (progn
               (alert "3φ3W選択時に、１次分岐ブレーカーは使用できません。処理を中止いします")(exit)
            );progn
    );if

   (if
        (and
            (= SOU "3")(= 2Pn "3")
        );and
            (progn
               (alert "3φ3W選択時に、１次分岐ブレーカーは使用できません。処理を中止いします")(exit)
            );progn
   );if
   
   (if
        (and
            (= SOU "3")(= 2Pn "4")
        );and
            (progn
               (alert "3φ3W選択時に、１次分岐ブレーカーは使用できません。処理を中止いします")(exit)
            );progn
   );if




;;内扉付きの条件
   (cond ((and (= SH "1")(or (= AF "225AF")(= AF "400AF")(= AF "600AF")(= AF "800AF")))
         (alert "支柱高さ：８２では、225AF以上は、使用出来ません。処理を中止します")(exit)))
;;ここから1φ2W
   (if (and (= PE2 "1φ2W 105V")(= Pn "4P"))
            (progn (alert "正しくない組み合わせが選ばれた為、処理を中止します")(exit))
   )
   (if (and (or (= PE2 "1φ2W 105V")(= PE2 "1φ2W 210V"))(= BTY "ELB")(= Pn "2P")(/= AF "50AF")(= MK "M"))
        (progn(alert "正しくない組み合わせが選ばれた為、処理を中止します")(exit))
    )
   (if (and (or (= PE2 "1φ2W 105V")(= PE2 "1φ2W 210V"))(= BTY "ELB")(= Pn "2P")(or (= MK "F")(= MK "T")))
       (if (or (= AF "60AF")(= AF "100AF")(= AF "225AF")(= AF "400AF")(= AF "600AF")(= AF "800AF"))
           (progn (alert "正しくない組み合わせが選ばれた為、処理を中止します")(exit))
       )
   )
   (if (and (or (= PE2 "1φ2W 105V")(= PE2 "1φ2W 210V"))(= BTY "AMCB")(= Pn "2P"))
       (progn (alert "正しくない組み合わせが選ばれた為、処理を中止します")(exit))
   )
   (if (and (= PE2 "1φ2W 105V")(or (= PL "D20")(= PL "L20")(= PL "D40")(= PL "L40")))
       (progn (alert "正しくない組み合わせが選ばれた為、処理を中止します")(exit))
   )
   (if (and (= PE2 "1φ2W 210V")(or (= PL "D10")(= PL "L10")(= PL "D40")(= PL "L40")))
       (progn (alert "正しくない組み合わせが選ばれた為、処理を中止します")(exit))
   )
;;ここから1φ3W
   (if (and (= PE2 "1φ3W 105/210V")(or (= Pn "4P")(= Pn "2P")))
       (progn (alert "正しくない組み合わせが選ばれた為、処理を中止します")(exit)))
  
   (if (and (= PE2 "1φ3W 105/210V")(or (= PL "D40")(= PL "L40")))
       (progn (alert "正しくない組み合わせが選ばれた為、処理を中止します")(exit)))
   
;;ここから3φ3W
   (if (and (or (= PE2 "3φ3W 210V")(= PE2 "3φ3W 440V"))(or (= Pn "4P")(= Pn "2P")))
       (progn (alert "正しくない組み合わせが選ばれた為、処理を中止します")(exit)))
   
   (if (and (= PE2 "3φ3W 210V")(or (= PL "D10")(= PL "L10")(= PL "D40")(= PL "L40")))
       (progn (alert "正しくない組み合わせが選ばれた為、処理を中止します")(exit)))
   
   (if (and (or (= PE2 "3φ3W 440V")(= PE2 "3φ4W 440V"))(or (= PL "D10")(= PL "L10")(= PL "D20")(= PL "L20")))
       (progn (alert "正しくない組み合わせが選ばれた為、処理を中止します")(exit)))
   


;;標準図の選択
    (if (and (= SOU "1")(= VV "105V"))(setq STD "*MAIN-LRN"))
    (if (and (= SOU "1")(= VV "105/210V")(= 2Pn "0")(or (= BrB "")(= BrB "SB")(= BrB "SM")))(setq STD "*MAIN-L1"));2P1E
    (if (and (= SOU "1")(= VV "105/210V")(= 2Pn "1")(or (= BrB "")(= BrB "SB")(= BrB "SM")))(setq STD "*MAIN-L1"));2P1E
    (if (and (= SOU "1")(= VV "105/210V")(= 2Pn "2")(or (= BrB "")(= BrB "SB")(= BrB "SM")))(setq STD "*MAIN-L2"));2P1E
    (if (and (= SOU "1")(= VV "105/210V")(= 2Pn "3")(or (= BrB "")(= BrB "SB")(= BrB "SM")))(setq STD "*MAIN-L4"));2P1E
    (if (and (= SOU "1")(= VV "105/210V")(= 2Pn "4")(or (= BrB "")(= BrB "SB")(= BrB "SM")))(setq STD "*MAIN-L4"));2P1E
    (if (and (= SOU "1")(= VV "105/210V")(= 2Pn "0")(or (= BrB "BH")))(setq STD "*MAIN-LL1"));Br2P2E
    (if (and (= SOU "1")(= VV "105/210V")(= 2Pn "1")(or (= BrB "BH")))(setq STD "*MAIN-LL1"));Br2P2E
    (if (and (= SOU "1")(= VV "105/210V")(= 2Pn "2")(or (= BrB "BH")))(setq STD "*MAIN-LL2"));Br2P2E
    (if (and (= SOU "1")(= VV "105/210V")(= 2Pn "3")(or (= BrB "BH")))(setq STD "*MAIN-LL4"));Br2P2E
    (if (and (= SOU "1")(= VV "105/210V")(= 2Pn "4")(or (= BrB "BH")))(setq STD "*MAIN-LL4"));Br2P2E
    (if (and (= SOU "3")(= VV "210V"))              (setq STD "*MAIN-L1")) ;1P3W
    (if (and (= SOU "3")(= WW "3")(= VV "440V"))    (setq STD "*MAIN-L1")) ;3P3W 440V
;    (if (and (= SOU "1")(= VV "210V"))             (setq STD "*MAIN-LRT"));1P2W 210V
;    (if (and (= SOU "3")(= WW "4")(= VV "440V"))   (setq STD "*MAIN-L6")) ;3P4W 440V
;    (if (and (= SOU "D")(= VV "105V"))             (setq STD "*MAIN-LDC"));2P DC105V
    (if (= STD nil)(progn (alert "そのファイル名が見つかりませんでした！処理を中止します")(exit)))

;;主幹ブレーカーの形式と外形シンボルの読込
    (setq GYO 51);ダミー
    (if (and (= BTY "MCB")(= Pn "2P")(= BKH "K")) (setq GYO 51))
    (if (and (= BTY "MCB")(= Pn "2P")(= BKH "H")) (setq GYO 71))
    (if (and (= BTY "MCB")(= Pn "3P")(= BKH "K")) (setq GYO 11))
    (if (and (= BTY "MCB")(= Pn "3P")(= BKH "H")) (setq GYO 31))
    (if (and (= BTY "ELB")(= Pn "2P")(= BKH "K")) (setq GYO 131))
    (if (and (= BTY "ELB")(= Pn "2P")(= BKH "H")) (setq GYO 151))
    (if (and (= BTY "ELB")(= Pn "3P")(= BKH "K")) (setq GYO 91))
    (if (and (= BTY "ELB")(= Pn "3P")(= BKH "H")) (setq GYO 111))
    (if (and (= BTY "AMCB")(= Pn "2P")(= BKH "K"))(setq GYO 171))
    (if (and (= BTY "AMCB")(= Pn "2P")(= BKH "H"))(setq GYO 171))
    (if (and (= BTY "AMCB")(= Pn "3P")(= BKH "K"))(setq GYO 171))
    (if (and (= BTY "AMCB")(= Pn "3P")(= BKH "H"))(setq GYO 171))
    
    (if (= AF  "50AF")(setq GYO (+ GYO 1)))
    (if (= AF  "60AF")(setq GYO (+ GYO 2)))
    (if (= AF "100AF")(setq GYO (+ GYO 3)))
    (if (= AF "225AF")(setq GYO (+ GYO 4)))
    (if (= AF "400AF")(setq GYO (+ GYO 5)))
    (if (= AF "600AF")(setq GYO (+ GYO 6)))
    (if (= AF "800AF")(setq GYO (+ GYO 7)))
    
    (if (= MK "M")(setq RETU  ""))
    (if (= MK "F")(setq RETU "A"))
    (if (= MK "T")(setq RETU "B"))
    
    (if (= SOU "1")(setq RETUG (strcat RETU "J"))); 1φで、点線の外形図を選択
    (if (= SOU "3")(setq RETUG (strcat RETU "G"))); 3φで、実線の外形図を選択
    
    (xls_open "X:/XLS/ブレーカー外形図対応表" "r")
        (setq KATA (xls_getval (strcat RETU "F" (itoa GYO))))
        (setq DMCB (xls_getval (strcat RETUG    (itoa GYO))))
        (if (= KATA "")(progn (alert "このブレーカーの外形図が登録されていません！！ 中断します。")(exit)))
    (xls_close)
    ; AL,AX
    (if (and (= AL "N")(= AX "N"))(setq ALX ""))
    (if (and (= AL "Y")(= AX "N"))(setq ALX "AL"))
    (if (and (= AL "N")(= AX "Y"))(setq ALX "AX"))
    (if (and (= AL "Y")(= AX "Y"))(setq ALX "AL・AX"))

;;ＥＬＢの感度電流を選定
    (if                               (or (= AF "30AF") (= AF "50AF") (= AF "60AF") (= AF "100AF")) (setq mA "30mA"))
    (if (and (or (= MK "M")(= MK "T"))(or (= AF "225AF")(= AF "400AF")(= AF "600AF")(= AF "800AF")))(setq mA "100-200-500mA"))
    (if (and (= MK "F")               (or (= AF "225AF")(= AF "400AF")(= AF "600AF")(= AF "800AF")))(setq mA "100-200-500-1000mA"))

;;AMCBの感度電流を選定
    (if (and (= BTY "AMCB")                             (= AF "50AF")                                                          )(setq mA "30mA"))
    (if (and (= BTY "AMCB")(or (= MK "M")(= MK "T"))(or (= AF "100AF")(= AF "225AF")(= AF "400AF")(= AF "600AF")(= AF "800AF")))(setq mA "100-200-500mA"))
    (if (and (= BTY "AMCB")    (= MK "F")           (or (= AF "100AF")(= AF "225AF")(= AF "400AF")(= AF "600AF")(= AF "800AF")))(setq mA "100-200-500-1000mA"))

;;分岐ブレーカーの選定
    (setq AT2 "20AT")
    (if (= BrB "")  ;選択されていない時
        (progn (setq BrP "2P")
               (if (or (= MK "M")(= MK "F")(= MK "T"))(progn (setq BrT "")(setq DBr "")(setq BMN "")))
        )
    )
    (if (= BrB "SM") ;スリム
        (progn (setq BrP "2P1E")
               (if (= MK "M")(progn (setq BrT "BH-C1") (setq DBr "DMBHK1PF001")(setq BMN "1")))
               (if (= MK "F")(progn (setq BrT "F52CN")(setq DBr "DMBHK1PF001")(setq BMN "4")))
               (if (= MK "T")(progn (setq BrT "TB-51C")(setq DBr "DMBHK1PF001")(setq BMN "7")))
        )
    )
    (if (= BrB "BH") ;BHK
        (progn (setq BrP "2P")
               (if (= MK "M") (progn (setq BrT "BH-K")  (setq DBr "DMBHK2PF001")(setq BMN "1")))
               (if (= MK "F") (progn (setq BrT "EA32FC")(setq DBr "DMBHK2PF001")(setq BMN "4")))
               (if (= MK "T") (progn (setq BrT "TB-5S") (setq DBr "DMBHK2PF001")(setq BMN "7")))
        )
    )
    (if (= BrB "SB") ;SB
        (progn (setq BrP "2P1E")(setq BrT "SE2P1E20S")(setq DBr "SMCB01")(setq BMN "39"))
    )
;;ＰＬの選定
    (if (= PL "N")  (progn (setq PLK "DR30D0L-M4W")(setq PLP "AC100-110V")(setq PLT "白熱球")));PL無しの場合
    (if (= PL "D10")(progn (setq PLK "DR30D0L-H4W")(setq PLP "AC100-110V")(setq PLT "白熱球")));白熱球 100V
    (if (= PL "D20")(progn (setq PLK "DR30D0L-M4W")(setq PLP "AC200-220V")(setq PLT "白熱球")));白熱球 200V
    (if (= PL "D40")(progn (setq PLK "DR30D0L-M4W")(setq PLP "AC/DC5.5V" )(setq PLT "白熱球")));白熱球 440V
    
    (if (= PL "L10")(progn (setq PLK "DR30D0L-H3W")(setq PLP "AC100-110V")(setq PLT "LED球"))) ;LED  100V
    (if (= PL "L20")(progn (setq PLK "DR30D0L-M3W")(setq PLP "AC200-220V")(setq PLT "LED球"))) ;LED  200V
    (if (= PL "L40")(progn (setq PLK "DR30D0L-A3W")(setq PLP "AC6V")      (setq PLT "LED球"))) ;LED  440V
;;ヒューズの選定
    (if (or (= PL "N")(= PL "D10")(= PL "D20")(= PL "L10")(= PL "L20"))
        (progn (setq FK "FHC-15N")(setq FMN "22")(setq DF "外形"))
    )
    (if (or (= PL "D40")(= PL "L40"))
        (progn (setq FK "栓型の型番を記入の事")(setq FMN "4")(setq DF "栓型外形"))
    )
;;ＡＦ別、電線サイズの選定
    (if (= AF  "50AF")(setq SEN "14Sqe"))
    (if (= AF  "60AF")(setq SEN "14Sqe"))
    (if (= AF "100AF")(setq SEN "38Sqe"))
    (if (= AF "225AF")(setq SEN "60Sqe"))
    (if (= AF "400AF")(setq SEN "250Sqe"))
    (if (= AF "600AF")(setq SEN "150SqeX2"))
    (if (= AF "800AF")(setq SEN "250SqeX2"))
;;３φ ＡＴ別、電線サイズの選定
    (if (= SOU "3")
        (progn
           (if (or (= AT "3AT")(= AT "5AT")(= AT "10AT")(= AT "15AT"))(setq SEN "2Sqe"))
           (if (= AT "20AT") (setq SEN "3.5Sqe"))
           (if (= AT "30AT") (setq SEN "5.5Sqe"))
           (if (= AT "40AT") (setq SEN "8 Sqe"))
           (if (= AT "50AT") (setq SEN "14Sqe"))
           (if (= AT "60AT") (setq SEN "14Sqe"))
           (if (= AT "75AT") (setq SEN "22Sqe"))
           (if (= AT "100AT")(setq SEN "38Sqe"))
           (if (= AT "125AT")(setq SEN "38Sqe"))
           (if (= AT "150AT")(setq SEN "60Sqe"))
           (if (= AT "175AT")(setq SEN "60Sqe"))
           (if (= AT "200AT")(setq SEN "100Sqe"))
           (if (= AT "225AT")(setq SEN "100Sqe"))
           (if (= AT "250AT")(setq SEN "150Sqe"))
           (if (= AT "300AT")(setq SEN "150Sqe"))
           (if (= AT "350AT")(setq SEN "200Sqe"))
           (if (= AT "400AT")(setq SEN "250Sqe"))
           (if (= AT "500AT")(setq SEN "100SqeX2"))
           (if (= AT "600AT")(setq SEN "150SqeX2"))
           (if (= AT "700AT")(setq SEN "200SqeX2"))
           (if (= AT "800AT")(setq SEN "250SqeX2"))
         )
    )
;;支柱の設定
    (cond ((= SH "0")(setq SH "4")));支柱無しの場合、仮にXCELの４行目を読まして空データーを作る。
    (if (= MYN "N")(setq BTY "MCB"));主幹無しの場合に仮のブレーカータイプを設定。
    (setq GYO (itoa GYO))           ;支柱に対応出来ない600AF,800AFのGYOを認識させる。
    (if (= BTY "MCB") (setq RETU1 "E" RETU2 "F" RETU3 "G" RETU4 "H"))
    (if (= BTY "ELB") (setq RETU1 "L" RETU2 "M" RETU3 "N" RETU4 "O"))
    (if (= BTY "AMCB")(setq RETU1 "L" RETU2 "M" RETU3 "N" RETU4 "O"));漏電アラームはELBと同じ条件
    (cond
       ((and (= MK "M")(or (= AF "50AF")(= AF "60AF")))(setq GYO (itoa (+ (atoi SH) 10))));50,60AF 三菱
       ((and (= MK "F")(or (= AF "50AF")(= AF "60AF")))(setq GYO (itoa (+ (atoi SH) 35))));50,60AF 富士
       ((and (= MK "T")(or (= AF "50AF")(= AF "60AF")))(setq GYO (itoa (+ (atoi SH) 60))));50,60AF 寺崎
       ((and (= MK "M")(= AF "100AF"))(setq GYO (itoa (+ (atoi SH) 15))));100AF 三菱
       ((and (= MK "F")(= AF "100AF"))(setq GYO (itoa (+ (atoi SH) 40))));100AF 富士
       ((and (= MK "T")(= AF "100AF"))(setq GYO (itoa (+ (atoi SH) 65))));100AF 寺崎
       ((and (= MK "M")(= AF "225AF"))(setq GYO (itoa (+ (atoi SH) 20))));225AF 三菱
       ((and (= MK "F")(= AF "225AF"))(setq GYO (itoa (+ (atoi SH) 45))));225AF 富士
       ((and (= MK "T")(= AF "225AF"))(setq GYO (itoa (+ (atoi SH) 70))));225AF 寺崎
       ((and (= MK "M")(= AF "400AF"))(setq GYO (itoa (+ (atoi SH) 25))));400AF 三菱
       ((and (= MK "F")(= AF "400AF"))(setq GYO (itoa (+ (atoi SH) 50))));400AF 富士
       ((and (= MK "T")(= AF "400AF"))(setq GYO (itoa (+ (atoi SH) 75))));400AF 寺崎
       ((and (= MK "T")(= AF "100AF")(= BKH "H"))(setq GYO (itoa (+ (atoi SH) 80))));100AF 寺崎 汎用
    )
;    (princ "\nRETU1=")(princ RETU1)(princ "\nGYO=")(princ GYO)
    (setq CEL1 (strcat RETU1 GYO)  CEL2 (strcat RETU2 GYO)  CEL3 (strcat RETU3 GYO)  CEL4 (strcat RETU4 GYO))
    (cond 
       ((= SH "0")(setq SHT (strcat "SH = " "無" )))
       ((= SH "1")(setq SHT (strcat "SH = " "82" )))
       ((= SH "2")(setq SHT (strcat "SH = " "106")))
       ((= SH "3")(setq SHT (strcat "SH = " "146")))
    )

;;サポートの読み込み
    (xls_open "X:/XLS/分電盤－サポート台選定" "r")
    (cond
       ((or (= AF "50AF")(= AF "60AF"))
             (setq MUMA (xls_getval CEL1))(setq BSP1 (xls_getval CEL2))
             (setq BSP2 (xls_getval CEL3))(setq BSP3 (xls_getval CEL4)))
       ((= AF "100AF")
             (setq MUMA (xls_getval CEL1))(setq BSP1 (xls_getval CEL2))
             (setq BSP2 (xls_getval CEL3))(setq BSP3 (xls_getval CEL4)))
       ((= AF "225AF")
             (setq MUMA (xls_getval CEL1))(setq BSP1 (xls_getval CEL2))
             (setq BSP2 (xls_getval CEL3))(setq BSP3 (xls_getval CEL4)))
       ((= AF "400AF")
             (setq MUMA (xls_getval CEL1))(setq BSP1 (xls_getval CEL2))
             (setq BSP2 (xls_getval CEL3))(setq BSP3 (xls_getval CEL4)))
       ((and (= AF "100AF")(= BKH "H"))
             (setq MUMA (xls_getval CEL1))(setq BSP1 (xls_getval CEL2))
             (setq BSP2 (xls_getval CEL3))(setq BSP3 (xls_getval CEL4)))
    )
    (xls_close)
       (cond ((= BSP1 nil)(setq BSP1 "")));;(alert "適合するバーサポートがありません！！ 中断します。")(exit)))
       (cond ((= MUMA nil)(setq MUMA "")))
       (cond ((= BSP2 nil)(setq BSP2 "")))
       (cond ((= BSP3 nil)(setq BSP3 "")))

  ;;主幹－分岐導体の選定
    (if (= BCu "N") ;;ダミー
        (progn (setq KCuR "BMC064")(setq KCuN "BMC065")(setq KCuT "BMC066")(setq SCu "Cu 3X15"))
    )
  ;;50AF 60AFの場合
    (cond
        ((and (= MK "M")(= BCu "BL")(or (= AF "50AF")(= AF "60AF")))                 ;;三菱 左寄せ
                                    (setq KCuR "BMC60L" KCuN "" KCuT "" SCu "Cu 3X15"))
        ((and (= MK "M")(= BCu "BC")(or (= AF "50AF")(= AF "60AF")))                 ;;三菱 センター
                                    (setq KCuR "BMC60C" KCuN "" KCuT "" SCu "Cu 3X15"))
        ((and (= MK "M")(= BCu "N") (or (= AF "50AF")(= AF "60AF")))                 ;;三菱 接続バーが無い時
                                    (setq KCuR ""       KCuN "" KCuT "" SCu "Cu 3X15"))
        ((and (or (= MK "F")(= MK "T"))(= BCu "BL")(or (= AF "50AF")(= AF "60AF")))  ;;富士・寺崎 左寄せ
                                       (setq KCuR "" KCuN "" KCuT "" SCu "Cu 3X15"))
        ((and (or (= MK "F")(= MK "T"))(= BCu "BC")(or (= AF "50AF")(= AF "60AF")))  ;;富士・寺崎 センター
                                       (setq KCuR "" KCuN "" KCuT "" SCu "Cu 3X15"))
        ((and (or (= MK "F")(= MK "T"))(= BCu "N")(or (= AF "50AF")(= AF "60AF")))   ;;富士・寺崎 接続バーが無い時
                                       (setq KCuR "" KCuN "" KCuT "" SCu "Cu 3X15"))
    )
  ;;100AFの場合
    (cond
        ((and (= MK "M")(= BCu "BL")(= AF "100AF"))(setq KCuR "BMC100L" KCuN "" KCuT "" SCu "Cu 3X15"));;三菱 左寄せ
        ((and (= MK "M")(= BCu "BC")(= AF "100AF"))(setq KCuR "BMC100C" KCuN "" KCuT "" SCu "Cu 3X15"));;三菱 センター
        ((and (= MK "M")(= BCu "N") (= AF "100AF"))(setq KCuR ""        KCuN "" KCuT "" SCu "Cu 3X15"));;三菱 接続バーが無い時

        ((and (or (= MK "F")(= MK "T"))(= BCu "BL")(= AF "100AF")(= BKH "K"))(setq KCuR ""        KCuN "" KCuT "" SCu "Cu 3X15"));;富士・寺崎 左寄せ
        ((and (or (= MK "F")(= MK "T"))(= BCu "BC")(= AF "100AF")(= BKH "K"))(setq KCuR ""        KCuN "" KCuT "" SCu "Cu 3X15"));;富士・寺崎 センター
        ((and (or (= MK "F")(= MK "T"))(= BCu "BL")(= AF "100AF")(= BKH "H"))(setq KCuR "BMC100L" KCuN "" KCuT "" SCu "Cu 3X15"));;富士・寺崎(汎用型) 左寄せ   100NB
        ((and (or (= MK "F")(= MK "T"))(= BCu "BC")(= AF "100AF")(= BKH "H"))(setq KCuR "BMC100C" KCuN "" KCuT "" SCu "Cu 3X15"));;富士・寺崎(汎用型) センター 100NB
        ((and (or (= MK "F")(= MK "T"))(= BCu "N") (= AF "100AF")           )(setq KCuR ""        KCuN "" KCuT "" SCu "Cu 3X15"));;富士・寺崎 接続バーが無い時
    )
  ;;225AFの場合
    (cond ((and (or (= MK "M")(= MK "T")(= MK "F"))(= BCu "BL")(= AF "225AF")(or (= SH "2")(= SH "3")));;三菱・寺崎・富士 左寄せ
                (setq KCuR "BMC225L" KCuN "" KCuT "" SCu "Cu 6X20"))
          ((and (or (= MK "M")(= MK "T")(= MK "F"))(= BCu "BC")(= AF "225AF")(or (= SH "2")(= SH "3")));;三菱・寺崎・富士 センター
                (setq KCuR "BMC225C" KCuN "" KCuT "" SCu "Cu 6X20"))
          ((and (or (= MK "M")(= MK "T")(= MK "F"))(= BCu "N")(= AF "225AF"))                          ;;三菱・寺崎・富士 接続バーが無い時
                (setq KCuR ""        KCuN "" KCuT "" SCu "Cu 6X20"))
    )
  ;;400AFの場合
    (cond
        ((and (or (= MK "M")(= MK "T")(= MK "F"))(= BCu "BC")(= AF "400AF")) ;;三菱・寺崎・富士 センター
              (setq KCuR "BMC400C" KCuN "" KCuT "" SCu "Cu 8X28"))
        ((and (or (= MK "M")(= MK "T")(= MK "F"))(= BCu "N") (= AF "400AF")) ;;三菱・寺崎・富士 接続バーが無い時
              (setq KCuR ""        KCuN "" KCuT "" SCu "Cu 8X28"))
    )

    (cond ((= KCuR nil)(setq KCuR "")))(cond ((= KCuN nil)(setq KCuN "")))(cond ((= KCuT nil)(setq KCuT "")))

;;回路データー
    (setq Sq (strcat SQE "Sqe"))
    (if (= SOU "1")(setq XS "N")) (if (= SOU "3")(setq XS "S"));１φ、３φでのＳ相の表示
    (setq TBRNo (strcat "TB-R" KN))
    (setq TBSNo (strcat "TB-" XS KN))
    (setq TBTNo (strcat "TB-T" KN))
    (setq MCBNo (strcat BTY "-M" KN))
    (setq FNo   (strcat "F-W" KN))
    (setq PLNo  (strcat "WL-" KN))
    (setq WHNo  (strcat "WH-" KN))
    
    (if (= BrB "")  (setq BrPNA ""))
    (if (= BrB "SM")(setq BrPNA "CMCB"))
    (if (= BrB "BH")(setq BrPNA "MCB"))
    (if (= BrB "SB")(setq BrPNA "SMCB"))
    
    (if (= BrB "")(setq Br1No ""))
    (if (= BrB "SM")(setq Br1No "CMCB-01"))
    (if (= BrB "BH")(setq Br1No "MCB-01"))
    (if (= BrB "SB")(setq Br1No "SMCB-01"))
    
    (if (= BrB "")(setq Br2No ""))
    (if (= BrB "SM")(setq Br2No "CMCB-02"))
    (if (= BrB "BH")(setq Br2No "MCB-02"))
    (if (= BrB "SB")(setq Br2No "SMCB-02"))
    
    (if (= BrB "")(setq Br3No ""))
    (if (= BrB "SM")(setq Br3No "CMCB-03"))
    (if (= BrB "BH")(setq Br3No "MCB-03"))
    (if (= BrB "SB")(setq Br3No "SMCB-03"))
    
    (if (= BrB "")(setq Br4N ""))
    (if (= BrB "SM")(setq Br4No "CMCB-04"))
    (if (= BrB "BH")(setq Br4No "MCB-04"))
    (if (= BrB "SB")(setq Br4No "SMCB-04"))
    
    (setq Cu1No (strcat "CuZ1-" KN))
    (setq Cu2No (strcat "CuZ2-" KN))
    (setq Cu3No (strcat "CuZ3-" KN))
    
    (if (or (= BTY "MCB")(= BTY "ELB"))(setq AMC ""))
    (if (or (= BTY "MCB")(= BTY "ELB"))(setq AMC2 ""))
;;;    (if (= BTY "AMCB")  (setq AMC  "自動リセット式"));;;;;;;;;;;;T7orT8に持ってゆけば？
;;;    (if (= BTY "AMCB")  (setq AMC2 "ECA-SLT付"))

    (if 
       (or (= MK "F")(= MK "T"))
          (setq MKK "FT")
    );if
    
    (if
       (or (= AF "30AF")(= AF "50AF")(= AF "60AF")(= AF 75AF)(= AF "100AF"))
          (setq AFB "Y")
    );if

;;外形データー Z2
    (setq NFZ2 (strcat "NF0" KN))
    (setq TBZ2 (strcat "TB"  KN))
    (setq PLZ2 (strcat "PL"  KN))
    (setq BrZ2 (strcat "Br"  KN))
    (setq CuZ2 (strcat "CuZ" KN))
    (setq F-Z2 (strcat "F-" KN))
;---------------------------------------------------------------------------------------------------
;;属性の書込データーをセット （ブレーカー送り）;書込みたい属性値を一旦D-XXXに書き込む。
;;                                                                                               D_IN.LSP参照
;;                  Pna   Pno   T1       T2   T3     T4  T5  T6  T7 T8 T9 MN   QT  Z1(外形SYM)   Z2(外形番号)
;;                  ---   ----- -------- ---- -----  --  --  --- -- -- -- ---- --  ------------- ----
  (setq D-MCB (list "MCB" MCBNo KATA     Pn   AT    AMC ALX  T6  "" "" "" MN   "1" DMCB          ""))
  (setq D-ELB (list "ELB" MCBNo KATA     Pn   AT     mA ALX  T6  "" "" "" MN   "1" DMCB          ""))
  (setq D-AMCB(list "AMCB" MCBNo KATA    Pn   AT     mA ALX  T6  "" "" "" MN   "1" DMCB          ""))
  (setq D-F   (list "F"   FNo   FK       ""   "3A"   ""  ""  ""  "" "" "" FMN  "1" "DF"          ""))
  (setq D-PL  (list "PL"  PLNo  PLK      "白" PLP    PLT ""  ""  "φ30mm ﾄﾞｰﾑ形" "トランス付" "" "4" "1" "DPL30F004" ""))
  (setq D-TBR (list "TB"  TBRNo "PT-30"  ""   ""     ""  ""  ""  "" "" "" "76" "1" "DTBF353F026" ""))
  (setq D-TBS (list "TB"  TBSNo "PT-30"  ""   ""     ""  ""  ""  "" "" "" "76" "1" ""            ""))
  (setq D-TBT (list "TB"  TBTNo "PT-30"  ""   ""     ""  ""  ""  "" "" "" "76" "1" ""            ""))
  (setq D-Br1 (list BrPNA Br1No BrT      BrP  AT2    ""  ""  ""  "" "" "" BMN  "1" DBr           ""))
  (setq D-Br2 (list BrPNA Br2No BrT      BrP  AT2    ""  ""  ""  "" "" "" BMN  "1" DBr           ""))
  (setq D-Br3 (list BrPNA Br3No BrT      BrP  AT2    ""  ""  ""  "" "" "" BMN  "1" DBr           ""))
  (setq D-Br4 (list BrPNA Br4No BrT      BrP  AT2    ""  ""  ""  "" "" "" BMN  "1" DBr           ""))

  (if (= KCuR "")
      (progn
          (setq D-CuR (list "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""))
      )
      (progn
          (setq D-CuR (list "Cu" "CuRNT" KCuR "" "" "" "" "" "" "" "" "36" "1" "" ""))
      )
  )
  
  (if (= KCuN "")
      (progn
          (setq D-CuN (list "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""))
      )
      (progn
          (setq D-CuN (list "Cu" "CuRNT" KCuN "" "" "" "" "" "" "" "" "36" "1" "" ""))
      )
  )

  (if (= KCuT "")
      (progn
          (setq D-CuT (list "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""))
      )
      (progn
          (setq D-CuT (list "Cu" "CuRNT" KCuT "" "" "" "" "" "" "" "" "36" "1" "" ""))
      )
  )

  (setq D-Cu1 (list "CuZ" Cu1No BSP1 "" "" "" "" "" "" "" "" "36" "1" "BBM1" ""))
  
  (if (/= BSP2 "")
      (progn     
          (setq D-Cu2 (list "CuZ" Cu2No BSP2 "" "" "" "" "" "" "" "" "36" "1" "" ""))
      )
      (progn
          (setq D-Cu2 (list "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""))
      ) 
  )

  (if (/= BSP3 "")
      (progn
          (setq D-Cu3 (list "CuZ" Cu3No BSP3 "" "" "" "" "" "" "" "" "36" "1" "" ""))
      )
      (progn
          (setq D-Cu3 (list "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""))
      )
  )
  
;;                  1   2   3  4  5   6  7
  (setq D-RST (list KAN PE2 KN Sq "R" XS "T"));SYM RSTの属性の書込データー
;==================================================================================================
;;標準図の図形選択範囲
  (if (or (= STD "*MAIN-L0")(= STD "*MAIN-LRN"))(progn (setq XL 225)(setq XR  400)(setq YY 700)));;09/06/26修正
  (if (or (= STD "*MAIN-L1")(= STD "*MAIN-LL1"))(progn (setq XL 225)(setq XR  900)(setq YY 850)))
  (if (or (= STD "*MAIN-L2")(= STD "*MAIN-LL2"))(progn (setq XL 225)(setq XR 1200)(setq YY 850)))
  (if (or (= STD "*MAIN-L4")(= STD "*MAIN-LL4"))(progn (setq XL 225)(setq XR 2000)(setq YY 850)))
  (if (= STD "*MAIN-L5")(progn (setq XL 225)(setq XR  400)(setq YY 700)))
  (if (= STD "*MAIN-L6")(progn (setq XL 225)(setq XR  400)(setq YY 700)))
  (if (= STD "*MAIN-L7")(progn (setq XL 225)(setq XR  400)(setq YY 700)))
  (if (= STD "*MAIN-L8")(progn (setq XL 225)(setq XR  400)(setq YY 700)))
  (if (= STD "*MAIN-M1")(progn (setq XL 225)(setq XR  700)(setq YY 700)))
  (setvar "CMDECHO" 0) 
  (prompt "\nX:/XLS/ブレーカー外形図対応表.XLSを参照しています。  ")
  
  (if (= (getvar "USERI5") 0) ;;手動で挿入する場合
      (progn
          (prompt "\n挿入点を指示")
          (command "INSERT" STD pause "1" "0")
          (setq PC (getvar "LASTPOINT"))
          ;(princ "\nPC1=")(princ PC)
          ;(princ "\nSTD1=")(princ STD)
      )
  )
  (if (= (getvar "USERI5") 1) ;;自動で挿入する場合
      (progn
          (setq PC (list PCX PCY))
          (command "INSERT" STD PC "1" "0")
          ;(princ "\nPC2=")(princ PC)
          ;(princ "\nSTD2=")(princ STD) 
      )
  )
      (setq FP1 (list (- (car PC) XL) (+ (cadr PC) YY ))) ;フェンス左上
      (setq FP2 (list (- (car PC) XL) (- (cadr PC) 100))) ;フェンス左下
      (setq FP3 (list (+ (car PC) XR) (- (cadr PC) 100))) ;フェンス右下
      (setq FP4 (list (+ (car PC) XR) (+ (cadr PC) YY ))) ;フェンス右上
      (setq PCS (list (- (car PC) 205)(+ (cadr PC) 50)))  ;支柱高さの文字位置
      (setq PCM (list (+ (car PC) 100)(+ (cadr PC) 175))) ;主幹の文字位置
      (setq PEAL(list (+ (car PC) 170)(+ (cadr PC) 175))) ;漏電アラームの文字位置
      (cond ((or (= SH "1")(= SH "2")(= SH "3"))
      (command "COLOR" 2)
      (if (= MYN "Y")   (command "TEXT" "S" "M0" PCM  "25" "0" "主幹"))
      (if (= BTY "AMCB")(command "TEXT" "S" "M0" PEAL "20" "0" "漏電アラーム"))
      (command "TEXT"  PCS "20" "0" SHT)))(command "COLOR" 4)
;     (command "PLINE" FP1 FP2 FP3 FP4 "C")
;;属性の編集
  (setq n 0)
  (setq AA (ssget "WP" (list FP1 FP2 FP3 FP4) (list '(66 . 1))))
  (while (setq BB (ssname AA n))
     (setq ENT (entget BB))
        ;;属性の書込対象となるシンボル
        (if (or (= (cdr (assoc 2 ENT)) "MMCB2P-3")
                (= (cdr (assoc 2 ENT)) "MMCB3P")
                (= (cdr (assoc 2 ENT)) "SPL")
                (= (cdr (assoc 2 ENT)) "SFH")
            
            ;;１次分岐ブレーカーが""かSM,SBの時 100V 2P1E
               (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))(= (cdr (assoc 2 ENT)) "BRMCB2P1E1"))
               (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))(= (cdr (assoc 2 ENT)) "BRMCB2P1E21"))
               (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))(= (cdr (assoc 2 ENT)) "BRMCB2P1E22"))
               (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))(= (cdr (assoc 2 ENT)) "BRMCB2P1E31"))
               (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))(= (cdr (assoc 2 ENT)) "BRMCB2P1E32"))
               (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))(= (cdr (assoc 2 ENT)) "BRMCB2P1E33"))
               (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))(= (cdr (assoc 2 ENT)) "BRMCB2P1E34"))
                
            ;;１次分岐ブレーカーがBHの時 2P
                (if (= BrB "BH")(= (cdr (assoc 2 ENT)) "BRMCB2P1"))
                (if (= BrB "BH")(= (cdr (assoc 2 ENT)) "BRMCB2P21"))
                (if (= BrB "BH")(= (cdr (assoc 2 ENT)) "BRMCB2P22"))
                (if (= BrB "BH")(= (cdr (assoc 2 ENT)) "BRMCB2P31"))
                (if (= BrB "BH")(= (cdr (assoc 2 ENT)) "BRMCB2P32"))
                (if (= BrB "BH")(= (cdr (assoc 2 ENT)) "BRMCB2P33"))
                (if (= BrB "BH")(= (cdr (assoc 2 ENT)) "BRMCB2P34"))
                (= (cdr (assoc 2 ENT)) "TB1PR")
                (= (cdr (assoc 2 ENT)) "TB1PS")
                (= (cdr (assoc 2 ENT)) "TB1PT")
                (= (cdr (assoc 2 ENT)) "MCUR")
                (= (cdr (assoc 2 ENT)) "MCUN")
                (= (cdr (assoc 2 ENT)) "MCUT")
                (= (cdr (assoc 2 ENT)) "MCUZ182")
                (= (cdr (assoc 2 ENT)) "MCUZ286")
                (= (cdr (assoc 2 ENT)) "MCUZ4106")
            )
            (progn                        ;ｼﾝﾎﾞﾙ名
               (if (= (cdr (assoc 2 ENT)) "MMCB2P-3")    (setq DATT D-MCB))
               (if (= (cdr (assoc 2 ENT)) "MMCB3P")      (setq DATT D-MCB))
               (if (and (= (cdr (assoc 2 ENT)) "MMCB3P")(= BTY "AMCB"))(setq DATT D-AMCB));漏電アラームの場合
               (if (= (cdr (assoc 2 ENT)) "SPL")         (setq DATT D-PL))
               (if (= (cdr (assoc 2 ENT)) "SFH")         (setq DATT D-F))
              
               ;;１次分岐ブレーカーが""かSMかSBの時
               (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))(if (= (cdr (assoc 2 ENT)) "BRMCB2P1E1")  (setq DATT D-Br1)))
               (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))(if (= (cdr (assoc 2 ENT)) "BRMCB2P1E21") (setq DATT D-Br1)))
               (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))(if (= (cdr (assoc 2 ENT)) "BRMCB2P1E22") (setq DATT D-Br2)))
               (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))(if (= (cdr (assoc 2 ENT)) "BRMCB2P1E31") (setq DATT D-Br1)))
               (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))(if (= (cdr (assoc 2 ENT)) "BRMCB2P1E32") (setq DATT D-Br2)))
               (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))(if (= (cdr (assoc 2 ENT)) "BRMCB2P1E33") (setq DATT D-Br3)))
               (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))(if (= (cdr (assoc 2 ENT)) "BRMCB2P1E34") (setq DATT D-Br4)))
              
               ;;１次分岐ブレーカーがBHの時
               (if (= BrB "BH")(if (= (cdr (assoc 2 ENT)) "BRMCB2P1")  (setq DATT D-Br1)))
               (if (= BrB "BH")(if (= (cdr (assoc 2 ENT)) "BRMCB2P21") (setq DATT D-Br1)))
               (if (= BrB "BH")(if (= (cdr (assoc 2 ENT)) "BRMCB2P22") (setq DATT D-Br2)))
               (if (= BrB "BH")(if (= (cdr (assoc 2 ENT)) "BRMCB2P31") (setq DATT D-Br1)))
               (if (= BrB "BH")(if (= (cdr (assoc 2 ENT)) "BRMCB2P32") (setq DATT D-Br2)))
               (if (= BrB "BH")(if (= (cdr (assoc 2 ENT)) "BRMCB2P33") (setq DATT D-Br3)))
               (if (= BrB "BH")(if (= (cdr (assoc 2 ENT)) "BRMCB2P34") (setq DATT D-Br4)))

               (if (= (cdr (assoc 2 ENT)) "TB1PR")   (setq DATT D-TBR))
               (if (= (cdr (assoc 2 ENT)) "TB1PS")   (setq DATT D-TBS))
               (if (= (cdr (assoc 2 ENT)) "TB1PT")   (setq DATT D-TBT))
               (if (= (cdr (assoc 2 ENT)) "MCUR")    (setq DATT D-CuR))
               (if (= (cdr (assoc 2 ENT)) "MCUN")    (setq DATT D-CuN))
               (if (= (cdr (assoc 2 ENT)) "MCUT")    (setq DATT D-CuT))
               (if (= (cdr (assoc 2 ENT)) "MCUZ182") (setq DATT D-Cu1))
               (if (= (cdr (assoc 2 ENT)) "MCUZ286") (setq DATT D-Cu2))
               (if (= (cdr (assoc 2 ENT)) "MCUZ4106")(setq DATT D-Cu3))
              
              (setq N2 1)
              (setq CC (entnext BB)  DD (entget CC))
                (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
                   (if (= (cdr (assoc 0 DD)) "ATTRIB")
                       (progn
                          (if (= N2 1) (setq ZOKU "PNA"))(if (= N2 2) (setq ZOKU "PNO"))
                          (if (= N2 3) (setq ZOKU "T1")) (if (= N2 4) (setq ZOKU "T2"))
                          (if (= N2 5) (setq ZOKU "T3")) (if (= N2 6) (setq ZOKU "T4"))
                          (if (= N2 7) (setq ZOKU "T5")) (if (= N2 8) (setq ZOKU "T6"))
                          (if (= N2 9) (setq ZOKU "T7")) (if (= N2 10)(setq ZOKU "T8"))
                          (if (= N2 11)(setq ZOKU "T9")) (if (= N2 12)(setq ZOKU "MN"))
                          (if (= N2 13)(setq ZOKU "QT")) (if (= N2 14)(setq ZOKU "Z1"))
                          (if (= N2 15)(setq ZOKU "Z2"))
                          (if (= (cdr (assoc 2 DD)) ZOKU)
                              (entmod (subst (cons 1 (nth (1- N2) DATT))(assoc 1 DD) DD));属性を変更
                          );if
                       );progn
                   );if
                   (setq N2 (1+ N2))
                   (setq CC (entnext CC) DD (entget CC))
                );while
            );progn
        );if
        
        ;;線サイズの変更
        (if (= (cdr (assoc 2 ENT)) "RST")
            (progn
               (setq N3 1)
               (setq CC (entnext BB) DD (entget CC))
               (while (/= (cdr (assoc 0 DD)) "SEQEND") ;SEQENDまで繰り返し
                  (if (= (cdr (assoc 0 DD)) "ATTRIB")
                      (progn
                         (if (= N3 1) (progn (setq ZOKU "SHU1")(setq ATT KAN)))
                         (if (= N3 2) (progn (setq ZOKU "SHU2")(setq ATT PE2)))
                         (if (= N3 3) (progn (setq ZOKU "SHU3")(setq ATT KN )))
                         (if (= N3 4) (progn (setq ZOKU "SHU4")(setq ATT Sq )))
                         (if (= N3 5) (progn (setq ZOKU "SHU5")(setq ATT "R")))
                         (if (= N3 6) (progn (setq ZOKU "SHU6")(setq ATT XS )))
                         (if (= N3 7) (progn (setq ZOKU "SHU7")(setq ATT "T")))
                         (if (= (cdr (assoc 2 DD)) ZOKU)
                         (entmod (subst (cons 1 ATT)(assoc 1 DD) DD));属性を変更
                         );if
                      );progn
                  );if
                  (setq N3 (1+ N3))
                  (setq CC (entnext CC) DD (entget CC))
               );while
            );progn
        );if
        
        
        ;;主幹を消す
        (if (and (= MYN "N")(= (cdr (assoc 2 ENT)) "MMCB3P"))(entdel BB))
        (if (and (= MYN "N")(= VV "105V")(= (cdr (assoc 2 ENT)) "MMCB2P-3"))(entdel BB))
        (if (and (or (= BTY "MCB")(= BTY "AMCB"))(= VV "105V")(= Pn "3P")(= (cdr (assoc 2 ENT)) "MMCB2P-3"))
            (entdel BB))
        ;(princ "\nENT=")(princ ENT)
        
        ;;ＥＬＢの場合一旦、主幹を消す
        (if (and (= BTY "ELB")(= (cdr (assoc 2 ENT)) "MMCB2P-3"))(entdel BB))
        (if (and (= BTY "ELB")(= (cdr (assoc 2 ENT)) "MMCB3P"))(entdel BB))
        
        ;;分岐が 0の場合
        (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))
            (if (and (= 2Pn "0")(= (cdr (assoc 2 ENT)) "BRMCB2P1E1"))(entdel BB))
        )
        (if (= BrB "BH")(if (and (= 2Pn "0")(= (cdr (assoc 2 ENT)) "BRMCB2P1"))(entdel BB)))
        (if (and (= 2Pn "0")(= (cdr (assoc 2 ENT)) "YA11"))        (entdel BB))
        
        ;;分岐が 3の場合
        (if (or (= BrB "")(= BrB "SM")(= BrB "SB"))
            (if (and (= 2Pn "3")(= (cdr (assoc 2 ENT)) "BRMCB2P1E34"))(entdel BB))
        )
        (if (= BrB "BH")(if (and (= 2Pn "3")(= (cdr (assoc 2 ENT)) "BRMCB2P34"))(entdel BB)))
        
        ;;バーに型番の属性がない場合、図形を消去
        ;(if (and (= T1CuR "")(= (cdr (assoc 2 ENT)) "MCUR"))(entdel BB))
        ;(if (and (= T1CuN "")(= (cdr (assoc 2 ENT)) "MCUN"))(entdel BB))    
        ;(if (and (= T1CuT "")(= (cdr (assoc 2 ENT)) "MCUT"))(entdel BB))

        
        ;;主幹－分岐が 電線接続の場合
;        (cond
;        ((and (or (= MK "F")(= MK "T")) (or (= AF "50AF")(= AF "60AF")(= AF "100AF"))(= BKH "K"))
;                (cond ((= (cdr (assoc 2 ENT)) "MCUR")(entdel BB)))
;                (cond ((= (cdr (assoc 2 ENT)) "MCUN")(entdel BB)))
;                (cond ((= (cdr (assoc 2 ENT)) "MCUT")(entdel BB))))
;        )

        ;;主幹－分岐が 不要の場合
        ;(if (or (and (= BCu "N")(= (cdr (assoc 2 ENT)) "MCUR")) (or (= AF "600AF")(= AF "800AF")))(entdel BB))
        ;(if (or (and (= BCu "N")(= (cdr (assoc 2 ENT)) "MCUN")) (or (= AF "600AF")(= AF "800AF")))(entdel BB))
        ;(if (or (and (= BCu "N")(= (cdr (assoc 2 ENT)) "MCUT")) (or (= AF "600AF")(= AF "800AF")))(entdel BB))
        
        ;;主幹が６００ＡＦ・８００ＡＦ場合、内外のブスバーサポートと主幹分岐バーを消す
        (if (and (or (= AF "600AF")(= AF "800AF"))(= (cdr (assoc 2 ENT)) "MCUZ182")) (entdel BB))
        (if (and (or (= AF "600AF")(= AF "800AF"))(= (cdr (assoc 2 ENT)) "MCUR"))    (entdel BB))
        (if (and (or (= AF "600AF")(= AF "800AF"))(= (cdr (assoc 2 ENT)) "MCUN"))    (entdel BB))
        (if (and (or (= AF "600AF")(= AF "800AF"))(= (cdr (assoc 2 ENT)) "MCUT"))    (entdel BB))
;        (princ "\nSYM = ")(princ (cdr (assoc 2 ENT)))
;        (princ "\nBCu = ")(princ BCu)
        (setq n (1+ n))
   );while
;(princ "\nBSP3=")(princ BSP3)
;;線番の書換え
  (if (or (= KAN nil)(= KAN ""))(setq KAN "1"))
  (if (= SOU "3")(progn (setq 1R1 (strcat "1" "R2"))(setq 1N1 (strcat "1" "S2"))(setq 1R1F (strcat "1" "R2F"))))
  (if (= SOU "1")(progn (setq 1R1 (strcat "1" "R1"))(setq 1N1 (strcat "1" "N1"))(setq 1R1F (strcat "1" "R1F"))))
  (setq n 0)
  (setq AA (ssget "WP" (list FP1 FP2 FP3 FP4) (list '(0 . "TEXT"))))
     (while (setq BB (ssname AA n))
            (setq ENT (entget BB))
            (if (= (cdr (assoc 1 ENT)) "1R1") (entmod (subst (cons 1 1R1) (assoc 1 ENT) ENT)))
            (if (= (cdr (assoc 1 ENT)) "1N1") (entmod (subst (cons 1 1N1) (assoc 1 ENT) ENT)))
            (if (= (cdr (assoc 1 ENT)) "1R1F")(entmod (subst (cons 1 1R1F)(assoc 1 ENT) ENT)))
         (setq n (1+ n))
     )
  (command "REGEN")
;==================================================================================================
;;MCBの挿入
;;105Vの3P
  (cond ((and (= BTY "MCB")(= VV "105V")(= Pn "3P"))
              (setq PC2 (list (car PC)(+ (cadr PC) 200)))
              (command "INSERT" "MMCB3P" PC2 "1" "1" "0" "MCB" MCBNo KATA Pn AT "" "" ALX "" "" "" MN "1" DMCB ""))
  )
;;AMCBの挿入
;;105Vの3P
   (if (and (= BTY "AMCB")(= VV "105V")(= Pn "3P"))
      (progn
          (setq PC2 (list (car PC)(+ (cadr PC) 200)))
          (command "INSERT" "MMCB3P" PC2 "1" "1" "0" "MCB" MCBNo KATA Pn AT AMC ALX AMC2 "" "" "" MN "1" DMCB "")
      )
  )
;;ELBの挿入
 ;;2P
  (if (and (= BTY "ELB")(= Pn "2P"))
      (progn
         (setq PC2 (list (car PC)(+ (cadr PC) 200)))
         (command "INSERT" "MELB2P-3" PC2 "1" "1" "0" "ELB" MCBNo KATA Pn AT mA "" ALX "" "" "" MN "1" DMCB "")
      )
  )
 ;;3P
  (if (and (= BTY "ELB")(= Pn "3P"))
      (progn
         (setq PC2 (list (car PC)(+ (cadr PC) 200)))
         (command "INSERT" "MELB3P" PC2 "1" "1" "0" "ELB" MCBNo KATA Pn AT mA "" ALX "" "" "" MN "1" DMCB "")
      )
  )
;;600AF・800AFの場合に、バーサポートの代わりにバー接続記号を挿入(BU-3P.DWG)
  (if (and (or (= VV "105/210V")(= VV "210V")(= VV "440V"))(= MCU "N")(or (= AF "600AF")(= AF "800AF")))
      (progn
          (command "INSERT" "BU-3P" PC "1" "1" "0")
          (setq CuP (list (+ (car PC) 100)(- (cadr PC) 25)))  ;バーサイズの文字位置
          (if (= AF "600AF")(setq CuTXT "Cu 8X50"))
          (if (= AF "800AF")(setq CuTXT "Cu 10X50"))
          (if (or (= SOU "1")(= SOU "3"))(progn (command "COLOR" 2)(command "TEXT" CuP "20" "0" CuTXT)(command "COLOR" 4)))
          ;(if (and (= T1CuR "")(= (cdr (assoc 2 ENT)) "MCUR"))(entdel BB))
          ;(if (and (= T1CuN "")(= (cdr (assoc 2 ENT)) "MCUN"))(entdel BB))    
          ;(if (and (= T1CuT "")(= (cdr (assoc 2 ENT)) "MCUT"))(entdel BB))
      )
  )
;;PL回路の削除
  (if (= PL "N")
      (progn
         (if (or (= STD "*MAIN-L1") (= STD "*MAIN-L2") (= STD "*MAIN-L4")
                 (= STD "*MAIN-LL1")(= STD "*MAIN-LL2")(= STD "*MAIN-LL4"))
             (progn
                (setq DP1 (list (- (car PC)  75) (+ (cadr PC) 525))) ;左上
                (setq DP2 (list (+ (car PC) 700) (+ (cadr PC) 375))) ;右下
             )
         )
         (if (= STD "*MAIN-M1")
             (progn
                (setq DP1 (list (- (car PC)  75) (+ (cadr PC) 325))) ;左上
                (setq DP2 (list (+ (car PC) 700) (+ (cadr PC) 225))) ;右下
             )
         )
         (command "ERASE" "W" DP1 DP2 "")
      )
  )
;;2Pnが0の時
   (if (= 2Pn "0")
       (if (or (= STD "*MAIN-L1")(= STD "*MAIN-L2")(= STD "*MAIN-L4")(= STD "*MAIN-M1"))
           (progn (setq P1 (list (car PC)(+ (cadr PC) 200)))(setq P2 (list (car PC)(+ (cadr PC) 325)))(3PL))
       )
   )
;;主幹接続の電線 or バーサイズ
   (if (and (or (= VV "105/210V")(= VV "210V")(= "440V"))(/= BCu "N")(or (= AF "50AF")(= AF "60AF")(= AF "100AF")(= AF "225AF")(= AF "400AF")))
       (progn
          (command "COLOR" 2)
          (command "TEXT" "S" "M0" (list (+ (car PC) 100)(- (cadr PC) 25)) "20" "0" SCu)
          (command "INSERT" "BU-3P" PC "1" "1" "0")
          (command "COLOR" 4)
       );progn
   );if
;;主幹接続バー無しの時に電線サイズを記入する
   (if (and (= VV "105/210V")(= BCu "N"))(progn (princ "YAYAYA")(command "INSERT" "YA1" PC "1" "1" "0" SEN)))
   
;;主幹－メインバーが電線の場合
    (if (and (= MCU "N")(= BCu "N")(= SOU "1")(/= VV "105/210V"))
        (progn
           (if (or (= AF "50AF")(= AF "60AF")(= AF "100AF")(= AF "225AF")(= AF "400AF")(= AF "600AF")(= AF "800AF"))
              (progn (if (or (and (= BTY "AMCB")(= AX "N")(= AL "N"))(= BTY "MCB")(= BTY "ELB"))
                         (command "INSERT" "YA1" PC "1" "1" "0" SEN)
                     );if
                  ;;AMCBにAXかALがついた時はバーの位置を下げる
                  (if (and (or (= AX "Y")(= AL "Y"))(= BTY "AMCB"))
                      (progn
                          (setq PC1 (list (car PC)(+ (cadr PC) -30)))
                          (command "INSERT" "YA1" PC1 "1" "1" "0" SEN)
                          ;(princ "\nYA1=")(princ YA1)
                      );progn
                  );if
              );progn
           );if
        );progn
    );if
    ;適合するバーが無い場合（寺崎・富士）
    (if (or (and (or (= MK "T")(= MK "F"))(= AT "20AT")(/= VV "105/210V"))
            (and (= BCu "N")(= AT "20AT")(/= VV "105/210V")))
            (progn (setq SEN "3.5Sqe")(command "INSERT" "YA1" PC "1" "1" "0" SEN))
    )
    (if (or (and (or (= MK "T")(= MK "F"))(/= AF "50AF")(= AT "30AT")(/= VV "105/210V"))
            (and (= BCu "N")(/= AF "50AF")(= AT "30AT")(/= VV "105/210V")))
            (progn (setq SEN "5.5Sqe")(command "INSERT" "YA1" PC "1" "1" "0" SEN))
    )
    (if (or (and (or (= MK "T")(= MK "F"))(/= AF "50AF")(= AT "40AT")(/= VV "105/210V"))
            (and (= BCu "N")(/= AF "50AF")(= AT "40AT")(/= VV "105/210V")))
            (progn (setq SEN "8Sqe")(command "INSERT" "YA1" PC "1" "1" "0" SEN))
    )
    (if (or (and (or (= MK "T")(= MK "F"))(/= AF "50AF")(= AT "50AT")(/= VV "105/210V"))
            (and (= BCu "N")(/= AF "50AF")(= AT "50AT")(/= VV "105/210V")))
            (progn (setq SEN "14Sqe")(command "INSERT" "YA1" PC "1" "1" "0" SEN))
    )
    (if (or (and (or (= MK "T")(= MK "F"))(/= AF "60AF")(= AT "60AT")(/= VV "105/210V"))
            (and (= BCu "N")(/= AF "60AF")(= AT "60AT")(/= VV "105/210V")))
            (progn (setq SEN "14Sqe")(command "INSERT" "YA1" PC "1" "1" "0" SEN))
    )
    (if (or (and (or (= MK "T")(= MK "F"))(/= AF "100AF")(= AT "60AT")(/= VV "105/210V"))
            (and (= BCu "N")(/= AF "100AF")(= AT "60AT")(/= VV "105/210V")))
            (progn (setq SEN "14Sqe")(command "INSERT" "YA1" PC "1" "1" "0" SEN))
    )
    (if (or (and (or (= MK "T")(= MK "F"))(/= AF "100AF")(= AT "75AT")(/= VV "105/210V"))
            (and (= BCu "N")(/= AF "100AF")(= AT "75AT")(/= VV "105/210V")))
            (progn (setq SEN "22Sqe")(command "INSERT" "YA1" PC "1" "1" "0" SEN))
    )
    (if (or (and (or (= MK "T")(= MK "F"))(/= AF "100AF")(= AT "100AT")(/= VV "105/210V"))
            (and (= BCu "N")(/= AF "100AF")(= AT "100AT")(/= VV "105/210V")))
            (progn (setq SEN "38Sqe")(command "INSERT" "YA1" PC "1" "1" "0" SEN))
    )
;;主幹ブレーカー無しの場合
  (if (= MYN "N")
      (progn (setq P2 (list (car PC)(cadr PC))) (setq P1 (list (car PC)(+ (cadr PC) 200))) (3PL))
  )
;;主幹ブレーカーが無し・ＰＬ有りの場合
;  (if (and (= MYN "N")(/= PL "N")(= VV "105/210V")(= VV "210V"))
;      (progn (setq P1 (list (car PC)(cadr PC)))(setq P2 (list (car PC)(+ (cadr PC) 400)))(3PL))
;  )
;;主幹ブレーカー有り・ＰＬ無しの場合
;  (if (and (= MYN "Y")(= PL "N")(= VV "105/210V")(= VV "210V"))
;      (progn (setq P1 (list (car PC)(+ (cadr PC) 200)))(setq P2 (list (car PC)(+ (cadr PC) 500)))(3PL))
;  )
;;主幹ブレーカーが有り・ＰＬ有りの場合
  (if (and (= MYN "Y")(/= PL "N")(= VV "105/210V")(= VV "210V"))
      (progn (setq P1 (list (car PC)(cadr PC)))(setq P2 (list (car PC)(+ (cadr PC) 400)))(3PL))
  )
;;主幹バー・主幹端子台が有る場合
    (if (or (= STD "*MAIN-L1") (= STD "*MAIN-L2") (= STD "*MAIN-L4")) (setq PC1 (list (car PC)(+ (cadr PC) 525))))
    (if (or (= STD "*MAIN-LL1")(= STD "*MAIN-LL2")(= STD "*MAIN-LL4"))(setq PC1 (list (car PC)(+ (cadr PC) 525))))
       
    (if (or (= STD "*MAIN-M1")(= STD "*MAIN-L0")(= STD "*MAIN-L5")(= STD "*MAIN-L6")(= STD "*MAIN-L7")
            (= STD "*MAIN-L8")) 
       (setq PC1 (list (car PC)(+ (cadr PC) 350)))
    )
;;M-TBかM-TB2かの判定
    (if (= STD  "*MAIN-L0")(setq M-TB "M-TB2"))
    (if (/= STD "*MAIN-L0")(setq M-TB "M-TB"))
;;主幹ブレーカーが有って、主幹受けバーが有る場合
    (if (= MCu "Y")
      (progn
           (setq LN (strlen AF))
           (if (= LN 4)(setq AF2 (substr AF 1 2)))
           (if (= LN 5)(setq AF2 (substr AF 1 3)))
           (if (<= (atoi AF2) 100)(setq CuZ "Cu4X18"))
           (if (and (<= (atoi AF2) 225)(> (atoi AF2) 100))(setq CuZ "Cu6X20"))
           (if (and (<= (atoi AF2) 400)(> (atoi AF2) 225))(setq CuZ "Cu8X25"))
           (if (and (<= (atoi AF2) 600)(> (atoi AF2) 400))(setq CuZ "Cu8X50"))
           (if (and (<= (atoi AF2) 800)(> (atoi AF2) 600))(setq CuZ "Cu10X50"))
           (setq PNo (strcat "Cu-0" KN))
           (command "INSERT" M-TB PC1 "1" "1" "0" "Cu" PNo CuZ "" "" "" "" "" "" "" "" "100" "0" "" "")
        )
    )
    (if (and (= MCu "Y")(= STD "*MAIN-L0"))
        (progn
             (setq PC1 (list (car PC)(+ (cadr PC) 200)))
             (command "INSERT" "YA1" PC1 "1" "1" "0" SEN)
        )
    )
    (if (and (= MCu "Y")(= VV "105/210V")(= 2Pn "0"))
        (progn
             (setq PC1 (list (car PC)(+ (cadr PC) 200)))
             (if (or (= AF "50AF")(= AF "60AF")(= AF "100AF")(= AF "225AF")(= AF "400AF")(= AF "600AF")(= AF "800AF"))
                 (command "INSERT" "YA1" PC1 "1" "1" "0" SEN)
             )
        )
    )

;主幹ブレーカーが有って、主幹端子台が有る場合
    (if (= MTB "Y") 
        (progn
           (setq PNO (strcat "TB-0" KN))
           (setq LN (strlen AF))
           (if (= LN 4)(setq AF2 (substr AF 1 2)))
           (if (= LN 5)(setq AF2 (substr AF 1 3)))
           (if (<= (atoi AF2) 100)(setq TBZ "PT-150"))
           (if (and (<= (atoi AF2) 200)(> (atoi AF2) 100))(setq TBZ "PT-200"))
           (if (and (<= (atoi AF2) 300)(> (atoi AF2) 200))(setq TBZ "PT-300"))
           (if (and (<= (atoi AF2) 400)(> (atoi AF2) 300))(setq TBZ "PT-400"))
           (if (and (<= (atoi AF2) 600)(> (atoi AF2) 400))(setq TBZ "PT-600"))
           (if (and (<= (atoi AF2) 800)(> (atoi AF2) 600))(setq TBZ "CTKC-800-3P"))
           (command "INSERT" M-TB PC1 "1" "1" "0" "TB" PNO TBZ "" "" "" "" "" "" "" "" "76" "3" "" "")
       )
    )
    (if (and (= MTB "Y")(= STD "*MAIN-L0"))
        (progn
            (setq PC1 (list (car PC)(+ (cadr PC) 200)))
            (command "INSERT" "YA1" PC1 "1" "1" "0" SEN)
        )
    )
    (if (and (= MTB "Y")(= VV "105/210V")(= 2Pn "0"))
        (progn
            (if (or (= AF "50AF")(= AF "60AF")(= AF "100AF")(= AF "225AF")(= AF "400AF")(= AF "600AF")(= AF "800AF"))
                (progn
                    (setq PC1 (list (car PC)(+ (cadr PC) 200)))
                    (command "INSERT" "YA1" PC1 "1" "1" "0" SEN)
                );progn
            );if
        );progn
    );if

;３φ 主幹ブレーカーが有って、主幹端子台が有る場合の電線サイズ記入
    ;(if (and (= SOU "3")(= MYN "Y")(= MCu "N"))(command "INSERT" "YA1" PC "1" "1" "0" SEN))

     (if (and (= SOU "3")(= BCu "N"))(command "INSERT" "YA1" PC "1" "1" "0" SEN))
     ;(princ "\nBCu=")(princ BCu)
     ;(princ "\nSEN=")(princ SEN)
    
;３φ　主幹ブレーカーが有って、主バー受けが有る場合の電線サイズ記入
;    (if (and (= SOU "3")(or (= MTB "Y")(= MCu "N")))
;        (progn
;           (setq PC1 (list (car PC)(+ (cadr PC) 200))) ;主幹ブレーカーと端子台が無い場合はYA1を描かない
;           (command "INSERT" "YA1" PC1 "1" "1" "0" SEN)
;        )
;    )
;;ＡＭが有る場合
    (if (and (= AM "Y")(or (= STD "*MAIN-L0")(= STD "*MAIN-L5")(= STD "*MAIN-L6")
                           (= STD "*MAIN-L7")(= STD "*MAIN-L8")(= STD "*MAIN-M1"))
        )
        (progn (setq APC (list (+ (car PC) 525) (+ (cadr PC) 100)))(command "INSERT" "*ST-ACT" APC "1" "0"))
    )
    (if (and (or (= STD "*MAIN-L1")(= STD "*MAIN-LL1"))(= AM "Y")(= 2Pn "0"))
        (progn (setq APC (list (+ (car PC) 525) (+ (cadr PC) 100)))(command "INSERT" "*ST-ACT" APC "1" "0"))
    )
    (if (and (or (= STD "*MAIN-L1")(= STD "*MAIN-LL1"))(= AM "Y")(= 2Pn "1"))
        (progn (setq APC (list (+ (car PC) 925) (+ (cadr PC) 100)))(command "INSERT" "*ST-ACT" APC "1" "0"))
    )
    (if (and (or (= STD "*MAIN-L2")(= STD "*MAIN-LL2"))(= AM "Y")(= 2Pn "2"))
        (progn (setq APC (list (+ (car PC) 1325) (+ (cadr PC) 100)))(command "INSERT" "*ST-ACT" APC "1" "0"))
    )
    (if (and (or (= STD "*MAIN-L4")(= STD "*MAIN-LL4"))(= AM "Y")(= 2Pn "3"))
        (progn (setq APC (list (+ (car PC) 1725) (+ (cadr PC) 100)))(command "INSERT" "*ST-ACT" APC "1" "0"))
    )
    (if (and (or (= STD "*MAIN-L4")(= STD "*MAIN-LL4"))(= AM "Y")(= 2Pn "4"))
        (progn (setq APC (list (+ (car PC) 2125) (+ (cadr PC) 100)))(command "INSERT" "*ST-ACT" APC "1" "0"))
    )
;;WHMが有る場合
    (setq WPC  (list (+ (car PC) 825)(+ (cadr PC) 700)))
    (setq WPC2 (list (+ (car PC) 825)(+ (cadr PC) 500)))
   ;ﾌﾞﾚｰｶｰのﾄﾘｯﾌﾟ値でCT比を選定
    (if (or (= AT "125AT")(= AT "150AT"))(setq T5A "150/5A"))
    (if (or (= AT "175AT")(= AT "200AT"))(setq T5A "200/5A"))
    (if (or (= AT "225AT")(= AT "250AT"))(setq T5A "250/5A"))
    (if (or (= AT "350AT")(= AT "400AT"))(setq T5A "400/5A"))
    (if (= AT "300AT")(setq T5A "300/5A"))(if (= AT "500AT")(setq T5A "500/5A"))
    (if (= AT "600AT")(setq T5A "600/5A"))(if (= AT "800AT")(setq T5A "800/5A"))
    (cond ;1P ﾀﾞｲﾚｸﾄ- 30A
       ((and (= WH "Y")(= PE "1P 3W 105/210V")(= AF "30AF"))
       (command "INSERT" "MWHD" WPC "1" "1" "0"
                         "WH" WHNo "M2LM" "30A" "1φ3W" "100V" "60Hz" "" "" "" "" "1" "1" "" "")))
    (cond ;1P ﾀﾞｲﾚｸﾄ- 120A
       ((and (= WH "Y")(= PE "1P 3W 105/210V")(or (= AF "50AF")(= AF "60AF")(= AF "100AF")))
       (command "INSERT" "MWHD" WPC "1" "1" "0"
                         "WH" WHNo "M2LM" "120A" "1φ3W" "100V" "60Hz" "" "" "" "" "1" "1" "" "")))
    (cond ;3P ﾀﾞｲﾚｸﾄ 30A
       ((and (= WH "Y")(= PE "3P 3W 210V")(= AF "30AF"))
       (command "INSERT" "MWHD" WPC "1" "1" "0"
                         "WH" WHNo "M2LM" "120A" "3φ3W" "200V" "60Hz" "" "" "" "" "1" "1" "" "")))
    (cond ;3P ﾀﾞｲﾚｸﾄ 120A
       ((and (= WH "Y")(= PE "3P 3W 210V")(or (= AF "50AF")(= AF "60AF")(= AF "100AF")))
       (command "INSERT" "MWHD" WPC "1" "1" "0"
                         "WH" WHNo "M2LM" "120A" "3φ3W" "200V" "60Hz" "" "" "" "" "1" "1" "" "")))
    (cond ;1P /5A
       ((and (= WH "Y")(= PE "1P 3W 105/210V")(or (= AF "225AF")(= AF "400AF")(= AF "600AF")(= AF "800AF")))
       (command "INSERT" "MWH5" WPC "1" "1" "0"
                         "WH" WHNo "M2LM" T5A "1φ3W" "100V" "60Hz" "" "" "" "" "1" "1" "" "")))
    (cond ;3P /5A
       ((and (= WH "Y")(= PE "3P 3W 210V")(or (= AF "225AF")(= AF "400AF")(= AF "600AF")(= AF "800AF")))
       (command "INSERT" "MWH5" WPC "1" "1" "0"
                         "WH" WHNo "M2LM" T5A "3φ3W" "200V" "60Hz" "" "" "" "" "1" "1" "" "")))



;==================================================================================================
   (command "REGEN") (setvar "BLIPMODE" 1)  (setvar "CMDECHO" 1)  (setvar "USERI5" 0)  (GC)
);end
;==================================================================================================
;;３線引き  SQ3Wとは別です
  (defun 3PL (/ );P1R P1T P2R P2T)
      (setq P1R (list (- (car P1) 50)(cadr P1)))  (setq P2R (list (- (car P2) 50)(cadr P2)))
      (setq P1T (list (+ (car P1) 50)(cadr P1)))  (setq P2T (list (+ (car P2) 50)(cadr P2)))
      (command "PLINE" P1 P2 "")
      (command "PLINE" P1R P2R "")
      (command "PLINE" P1T P2T "")
  )
