;;高圧配電盤用、受電回路入力（単線図） 標準図の属性を書換え   HV_IN.LSP  2004/JAN/4  By T.Terada
;;
;; C++ KD1 Form1 ＶＣＢ受電盤の入力ダイアログから取得する変数      ＊X:/XLS/高圧機器一覧表、使用。
;; 設 定 項 目         TAGNo 変数  変 数 の 値
;; =================== ===== ===== ===============================================================
;; 設備容量            :Ed1   KVA
;; 選定したＣＴ比      :Ed2   CTR
;; 使用電圧            :G6    HV    B22="6600V" B23="3300V"
;; 周波数              :G10   HZ    B55="60Hz"  B56="50Hz"
;; ＶＣＴの有無        :G21   VCT   B48="N"     B49="Y"
;; ＤＳ定格電流        :G1    DSA   B1 ="200A"  B2 ="400A" B3 ="600A"  B4="1200A" B50="2000A"
;; ＤＳ操作方式        :G8    DST   B5 ="1P"    B6 ="3P"   B7 ="EN"
;; ＶＣＢ定格電流      :G13   VCBA  B8 ="400A"  B9="600A"  B10="1200A" B11="2000A"
;; ＶＣＢ遮断容量      :G12   VCBKA B12="8KA"   B13="12.5KA" B14="20KA" B15="25KA" B16="32KA" B17="40KA"
;; ＶＣＢタイプ        :G14   VCBT  B18="手動"  B19="電動" B20="引出"
;; 端子配列 手動       :G2    TBT   B21="N"     B24="R"    B25="P"
;; 端子配列 電動・引出 :G15   TBD   B26="CW1"   B27="CW2"  B28="PW"    B29="MW"
;; ＡＰＦＣの有無      :G16   APFC  B34="N"     B35="Y"
;; 避雷器のタイプ      :G11   LA    B58="N"     B59="1P"   B60="3P"
;; 電流計のタイプ      :G3    AMT   B30="AM"    B31="DMA"  B32="MLT"
;; オプションメーター  :G4    M1-4  M1 ="VM"    M2 ="KW"   M3="PF"     M4="VAR"   M5="HZ"
;; オプションＴ／Ｄ    :G5    TD1-6 TD1 ="AMT"  TD2 ="VWT" TD3 ="KWT"  TD4 ="PFT" TD5 ="VART" TD6="HZT"
;; ＷＨＭのタイプ      :G7    WHT   B33="N"     B44="M"    B45="P "    B46="E"    B47="C"
;; 地絡リレー支給      :G9    SHI   C7 ="Y" or "N"
;; 地絡リレーのタイプ  :G9    GRT   B53="HGR"   B54="DGR"   B57="N"
;; ＵＶＲの有無        :G19   UV    B40="N"     B41="Y"
;; ＯＶＲの有無        :G20   OV    B42="N"     B43="Y"
;; VTT・CTTのタイプ    :G18   VCTT  B38="N"     B39="P"
;; 操作用ＶＴの有無    :G17   OPVT  B36="N"     B37="Y"
;; 操作用ＶＴの容量    :CBox1 VTVA  
;; ＺＣＴの内径        :CBox2 ZKEI  

(defun C:HV_IN ( / KVA CTR HV HZ VCT DSA DST VCBA VCBKA VCBT TBT TBD APFC LA AMT M1 M2 M3 M4 CTNAM
                   TD1 TD2 TD3 TD4 TD5 TD6 WHT SHI GRT UV OV VCTT OPVT VTVA ZKEI MN STD MS CTR5 ICTR
                   K12 K3 K4 K5 K67 K89 KVCB1 KVCB2 IHV ICT sAM vAM vKW sKW HV2 Z1 Z2 Z3 Z4
                   SEL SEL1 Rasc PC1 PC2 PC21 PC22 PC23 PC24 PC25 PC26 PC27 PC28 PC3 PC4 PC5 PC6
                   FP1 FP2 FP3 FP4  n N2 AA BB CC DD ENT DATT ZOKU Hsq Sq STD MN GYO RETU RETU1
                   D-OCR D-GR D-UV D-OV D-ZPD D-APFC D-CTT D-VTT D-ZCTT D-ATD D-VTD D-KWTD D-COSTD
                   D-VARTD D-HZTD D-VT D-OPVT D-WHM D-WHM2 D-AM D-AM2 D-VM D-KW D-COS
                   D-VAR D-HZ D-ZCT D-OPVT D-MCB D-LA D-DS D-VCB D-VCB2
               )
;;出来ていない事: ＊マルチメーター 未対応
                  ＊ＶＣＢの型番合成が三菱のみになっている！！！！！！！！！！！
;;初期設定
    (setq *error* *err1*)
    (setvar "CMDECHO" 0)
     
    (C:MKR)               ;; 基本メーカーを調べる
    (CVCB1)               ;; C++ 入力用ダイアログを呼びだし
    (setvar "USERS5" "A") ;; C:SQ3Wを自動モードで使う
    (setq LG "N")         ;; LGR無しの設定（Ｃのダイアログには設定無し）

;;メーカー番号の決定
    (if (= MAKER "0")(setq MN "1")) (if (= MAKER "1")(setq MN "4"))
    (if (= MAKER "2")(setq MN "1")) (if (= MAKER "3")(setq MN "4"))

;;標準図シンボルの決定
    (setq STD "*ST-HJ1")
;;メーターの形式（別ダイアログで設定の事！）
;    (setq MS "S")  ;１２０角
    (setq MS "W") ;広角
;    (setq MS "K") ;切替式

;;ＣＴ比による設定
    (setq D-CTH nil CTNAM nil)
    (setq CTR5 (strcat CTR "/5A"))
    (setq ICTR (ATOI CTR))
    
    ;VCB 8KA
    (if (and (= VCBKA "8KA")(<= ICTR 800))(setq CTNAM "CD-40K"))
    
    ;VCB 12.5KA
    (if (and (= VCBKA "12.5KA")(>= ICTR 20)(<= ICTR 600))(setq CTNAM "CD-25ANA"))
        
        ;(alert "12.5KA この設定は出来ません")
    ;VCB 20KA
    (if (= VCBKA "20KA")
        (progn
           (if (or (> ICTR 80)(<= ICTR 500))            (setq CTNAM nil))
           (if (or (= ICTR 150)(= ICTR 200)(= ICTR 250))(setq CTNAM "CD40-GNA"))
           (if (or (= ICTR 300)(= ICTR 400))            (setq CTNAM "CD40-ENA"))
           (if (= ICTR 100)(setq CTNAM "CD-40LN"))
           (if (= ICTR 120)(setq CTNAM "BN-0"))
        )
    )
    ;VCB 25KA
    (if (and (= VCBKA "25KA")(= HV "3300V"))
        (progn
           (if (or (= ICTR  75)(= ICTR  80)) (setq CTNAM "BN-0"))
           (if (or (= ICTR 120)(= ICTR 150)) (setq CTNAM "BN-0"))
           (if (= ICTR  100) (setq CTNAM "CD-40LN"))
           (if (= ICTR  200) (setq CTNAM "CD-40GNA"))
           (if (>= ICTR 250) (setq CTNAM "BN-0"))
        )
    )
    ;VCB 32KA
    (if (and (= VCBKA "32KA")(> ICTR 100))(setq CTNAM "BN-0"))
    
    ;VCB 40KA
    (if (and (= VCBKA "40KA")(>= ICTR 150))(setq CTNAM "BN-0"))
    
    (if (= CTNAM nil)(progn (alert "この設定は出来ません")(exit)))
    (princ "\nCTNAM =")(princ CTNAM) (prompt "\n")
    (setq D-CTH (list "CT" "CT-1" CTNAM "" CTR5 "" "" "" "" "" "" MN "2" "Z1" "Z2" ))

;;ＶＣＢの型番合成
    (if (= VCBKA "8KA")   (setq K12 "VF08"))       ;型名・定格遮断電流 ①②
    (if (= VCBKA "12.5KA")(setq K12 "VF13"))       ;型名・定格遮断電流 ①②
    (if (= VCBKA "20KA")  (setq K12 "VF20"))       ;型名・定格遮断電流 ①②
    (if (= VCBKA "25KA")  (setq K12 "VF25"))       ;型名・定格遮断電流 ①②
    (if (= VCBKA "32KA")  (setq K12 "VF32"))       ;型名・定格遮断電流 ①②
    (if (= VCBKA "40KA")  (setq K12 "VF40"))       ;型名・定格遮断電流 ①②
    
    (if (= VCBT "手動")(setq K3 TBT))              ;据付方式 ③
    (if (or (= VCBKA "8KA")(= VCBKA "12.5KA"))
        (if (= VCBT "固定")(setq K3 TBT))          ;据付方式 ③
        (setq K3 "L") ; ELSE
    )
    (if (= VCBT "引出")(setq K3 TBD))              ;据付方式 ③
    (if (= VCBT "手動")(setq K4 "H")(setq K4 "M")) ;閉路操作方式 ④
    (if (= VCBKA "20KA")(setq K5 "H")(setq K5 "-"));低サージ区分 ⑤
    (if (and (or (= VCBKA "20KA")(= VCBKA "25KA")(= VCBKA "32KA")(= VCBKA "40KA"))
             (or (= VCBT "固定")(= VCBT "引出"))
        )
        (progn
           (if (= VCBA  "600A")(setq K67 "06"))    ;20KA以上、定格電流 ⑥⑦
           (if (= VCBA "1200A")(setq K67 "12"))
           (if (= VCBA "2000A")(setq K67 "20"))
           (if (= VCBA "3000A")(setq K67 "30"))
           (setq K89 "11")                         ;閉路操作電圧 & 電圧引外し装置 ⑧⑨
        )
        (setq K89 "00")                            ;UVT & OTC                     ⑧⑨
    )
    (if (or (= VCBKA "8KA")(= VCBKA "12.5KA"))     ;閉路操作電圧 & STC ⑥⑦
        (progn
           (princ "\n8KAヨ～")
           (if (= VCBT "手動")(setq K67 "00"))
           (if (or (= VCBT "固定")(= VCBT "引出"))(setq K67 "11"))
        )
    )
    ;型番合成
    (setq KVCB1 (strcat K12 K3 K4 "C"))
    ;(setq KVCB (strcat K12 K3 K4 "C" K5 K67 K89 "00"))
    (if (or (= VCBKA "8KA")(= VCBKA "12.5KA"))
            (setq KVCB2 (strcat K5 K67 K89 "00"))
    )
    (if (or (= VCBKA "20KA")(= VCBKA "25KA")(= VCBKA "32KA")(= VCBKA "40KA"));20KA以上
            (setq KVCB2 (strcat K5 K67 K89 "0000"))
    )
    (princ "\nＶＣＢ型番 =")(princ KVCB)
    
;;ＡＭ・ＫＷ・Ｖａｒのメータースケール設定 
    (setq IHV (atoi HV)) (setq ICT (atoi CTR))
    ;ＡＭ
       (setq sAM (strcat "0～" CTR "/5A"))
    
    ;ＫＷの計算式  計器目盛り P(KW) = (受電電圧 / 110) X (CT比 /5) X P0=1
       (setq vKW (* (/ IHV 110) (/ ICT 5) 1))
       (setq sKW (strcat "0～" (rtos vKW) "KW"))
    
    ;Ｖａｒの計算式  計器目盛り P(KVar) = (受電電圧 / 110) X (CT比 /5) X P0=1 X 0.5
       (setq vVAR (* vKW 0.5))
       (setq sVAR (strcat (rtos vVAR) "Kvar～0～" (rtos vVAR) "Kvar"))
       (princ "\nVKW =")(princ sKW)
       (princ "\nVVAR =")(princ sVAR)

;;１次幹線と開閉器の変更
    (if (= HV "6600V")(setq HV2 "6.6KV"))
    (if (= HV "3300V")(setq HV2 "3.3KV"))
    (setq Z1 (strcat "3φ3W " HV " " HZ))
    (setq Z2 "PAS")
    (setq Z3 "400A")
    (setq Z4 (strcat HV2 "CV-T ???Sqe"))
    
;;電線サイズ
    (if (<= ICT 100)                 (setq Hsq  "38sqe"))    ;CT比  100/5A 以下の場合
    (if (or (= ICT 120)(= ICT 150))  (setq Hsq  "60sqe"))
    (if (or (= ICT 200)(= ICT 250))  (setq Hsq "100sqe"))
    (if (= ICT 300)                  (setq Hsq "150sqe"))
    (if (= ICT 400)                  (setq Hsq "200sqe"))
    (if (or (= ICT 500)(= ICT 600))  (setq Hsq "Cu 6-50"))
    (if (or (= ICT 750)(= ICT 800))  (setq Hsq "Cu 8-50"))
    (if (= ICT 1000)                 (setq Hsq "Cu 10-50"))
    (if (or (= ICT 1200)(= ICT 1500))(setq Hsq "Cu 10-75"))
    
;;EXCELの読込み開始"列"を決定
    (if (or (= MAKER "0")(= MAKER "2"))(progn (setq RETU "E") (setq Rasc (ascii "E")))) ;三菱の場合
    (if (or (= MAKER "1")(= MAKER "3"))(progn (setq RETU "AE")(setq Rasc (ascii "AE"))));富士の場合

 ;EXCELデーターの読込
    (setq D-GR nil D-UV nil D-DS nil D-ZPD nil D-CTT nil D-VTT nil D-HZTD nil D-VARTD nil D-OCR nil 
          D-VT nil D-OV nil D-LA nil D-CAP nil D-ATD nil D-VTD nil D-KWTD nil D-COSTD nil
          D-AM nil D-VM nil D-KW nil D-COS nil D-VAR nil D-LBS nil D-APFC nil D-OPMCB nil
          D-PF nil D-VG nil D-HZ nil D-ZCT nil D-VCB nil D-WHM nil D-ZCTT nil D-OPVT nil
    )
    (xls_open "X:/XLS/高圧機器一覧表" "r")
       ;;OCR 
       (setq RETU1 RETU)
       (if (= VCBT "手動")
           (setq GYO "12") ;手動 = 電流引はずし
           (setq GYO "11") ;電動 = 電圧引はずし ;else
       )
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-OCR (append D-OCR (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;GR 
       (setq RETU1 RETU)
       (if (= SHI "Y")(setq GRT5 "*"));支給品の場合
       (if (and (= GRT "HGR")(= VCBT "手動")) (setq GYO "14"))
       (if (and (= GRT "HGR")(= VCBT "固定")) (setq GYO "13"))
       (if (= GRT "DGR")(setq GYO "15"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-GR (append D-GR (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;UV 
       (setq RETU1 RETU)
       (if (= UV "Y") (setq GYO "16"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-UV (append D-UV (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;OV 
       (setq RETU1 RETU)
       (if (= OV "Y") (setq GYO "17"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-OV (append D-OV (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;ZPD 
       (setq RETU1 RETU)
       (if (= GRT "DGR") (setq GYO "19"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-ZPD (append D-ZPD (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;APFC 
       (setq RETU1 RETU)
       (if (= APFC "Y") (setq GYO "20"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-APFC (append D-APFC (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;CTT 
       (setq RETU1 RETU)
       (if (= VCTT "N")(setq GYO "24"))
       (if (= VCTT "P")(setq GYO "21"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-CTT (append D-CTT (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;VTT 
       (setq RETU1 RETU)
       (if (= VCTT "N")(setq GYO "25"))
       (if (= VCTT "P")(setq GYO "22"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-VTT (append D-VTT (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;ZCTT 
       (setq RETU1 RETU)
       (if (= VCTT "N")(setq GYO "26"))
       (if (= VCTT "P")(setq GYO "23"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-ZCTT (append D-ZCTT (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;AM T/D 
       (setq RETU1 RETU)
       (if (= TD1 "AM")(setq GYO "31"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-ATD (append D-ATD (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;VM T/D
       (setq RETU1 RETU)
       (if (= TD2 "VM")(setq GYO "32"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-VTD (append D-VTD (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;KW T/D
       (setq RETU1 RETU)
       (if (= TD3 "KW")(setq GYO "33"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-KWTD (append D-KWTD (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;COS T/D
       (setq RETU1 RETU)
       (if (= TD4 "COS")(setq GYO "34"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-COSTD (append D-COSTD (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;VAR T/D
       (setq RETU1 RETU)
       (if (= TD5 "VAR")(setq GYO "35"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-VARTD (append D-VARTD (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;HZ T/D
       (setq RETU1 RETU)
       (if (= TD6 "HZ")(setq GYO "36"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-HZTD (append D-HZTD (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;VT
       (setq GYO "37")(setq RETU1 RETU)
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-VT (append D-VT (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;VT-OP
       (setq RETU1 RETU)
       (if (and (= OPVT "Y")(= VTVA "300VA"))(setq GYO "38"))
       (if (and (= OPVT "Y")(= VTVA "600VA"))(setq GYO "39"))
       (if (and (= OPVT "Y")(= VTVA "1KVA")) (setq GYO "40"))
       (if (and (= OPVT "Y")(= VTVA "2KVA")) (setq GYO "41"))
       (if (and (= OPVT "Y")(= VTVA "3KVA")) (setq GYO "42"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-OPVT (append D-OPVT (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;WHM
       (setq RETU1 RETU)
           (if (= WHT "M")(setq GYO "51"))
           (if (= WHT "P")(setq GYO "52"))
           (if (= WHT "E")(setq GYO "53"))
           (if (= WHT "C")(setq GYO "54"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-WHM (append D-WHM (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       (setq D-WHM2 (nth 3 D-WHM));選定ＣＴ比に対応したレシオに書換
       (setq D-WHM (subst CTR5 D-WHM2 D-WHM))
       ;;AM
       (setq RETU1 RETU)
       (if (and (= MS "S")(= AMT "AM")) (setq GYO "61"));１２０角
       (if (and (= MS "W")(= AMT "AM")) (setq GYO "62"));広角
       (if (and (= MS "K")(= AMT "AM")) (setq GYO "63"));切替式
       (if (and (= MS "W")(= AMT "DMA"))(setq GYO "64"))
       (if (= AMT "MLT")(setq GYO "65"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-AM (append D-AM (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       (setq D-AM2 (nth 4 D-AM))(setq D-AM (subst sAM D-AM2 D-AM)) ;選定スケールに書換
       ;;VM
       (setq RETU1 RETU)
       (if (and (= MS "S")(= M1 "VM"))(setq GYO "71"));１２０角
       (if (and (= MS "W")(= M1 "VM"))(setq GYO "72"));広角
       (if (and (= MS "K")(= M1 "VM"))(setq GYO "73"));切替式
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-VM (append D-VM (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;KW
       (setq RETU1 RETU)
       (if (and (= MS "S")(= M2 "KW"))(setq GYO "81"));１２０角
       (if (and (= MS "W")(= M2 "KW"))(setq GYO "82"));広角
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-KW (append D-KW (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       (setq D-KW2 (nth 4 D-KW))(setq D-KW (subst sKW D-KW2 D-KW)) ;選定スケールに書換
       ;;COS
       (setq RETU1 RETU)
       (if (and (= MS "S")(= M3 "COS"))(setq GYO "91"));１２０角
       (if (and (= MS "W")(= M3 "COS"))(setq GYO "92"));広角
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-COS (append D-COS (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;VAR
       (setq RETU1 RETU)
       (if (and (= MS "S")(= M4 "VAR"))(setq GYO "101"));１２０角
       (if (and (= MS "W")(= M4 "VAR"))(setq GYO "102"));広角
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-VAR (append D-VAR (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       (setq D-VAR2 (nth 4 D-VAR))(setq D-VAR (subst sVAR D-VAR2 D-VAR)) ;選定スケールに書換
       ;;HZ
       (setq RETU1 RETU)
       (if (and (= MS "S")(= M5 "HZ"))(setq GYO "111"));１２０角
       (if (and (= MS "W")(= M5 "HZ"))(setq GYO "112"));広角
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-HZ (append D-HZ (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;ZCT
       (setq RETU1 RETU)
       (if (or (= GRT "HGR")(= GRT "DGR"))
           (progn 
              (if (= ZKEI "53")     (setq GYO "141"))
              (if (= ZKEI "68")     (setq GYO "142"))
              (if (= ZKEI "90")     (setq GYO "143"))
              (if (= ZKEI "110")    (setq GYO "144"))
              (if (= ZKEI "160")    (setq GYO "145"))
              (if (= ZKEI "250")    (setq GYO "146"))
              (if (= ZKEI "分割52") (setq GYO "147"))
              (if (= ZKEI "分割77") (setq GYO "148"))
              (if (= ZKEI "分割112")(setq GYO "149"))
           )
       )
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-ZCT (append D-ZCT (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;OP-VT
       (setq RETU1 RETU)
       (if (and (= OPVT "Y")(= VTVA "300VA"))(setq GYO "38"))
       (if (and (= OPVT "Y")(= VTVA "600VA"))(setq GYO "39"))
       (if (and (= OPVT "Y")(= VTVA "1KVA" ))(setq GYO "40"))
       (if (and (= OPVT "Y")(= VTVA "2KVA" ))(setq GYO "41"))
       (if (and (= OPVT "Y")(= VTVA "3KVA" ))(setq GYO "42"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-OPVT (append D-OPVT (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;OP-MCB
       (setq RETU1 RETU)
       (if (= OPVT "Y")(setq GYO "46"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-MCB (append D-MCB (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;LA
       (setq RETU1 RETU)
       (if (= LA "1P")(setq GYO "44"))
       (if (= LA "3P")(setq GYO "45"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-LA (append D-LA (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;DS
       (setq RETU1 RETU)
       (if (and (= DST "1P")(= DSA  "200A"))(setq GYO "161"))
       (if (and (= DST "1P")(= DSA  "400A"))(setq GYO "162"))
       (if (and (= DST "1P")(= DSA  "600A"))(setq GYO "163"))
       (if (and (= DST "1P")(= DSA "1200A"))(setq GYO "164"))
       (if (and (= DST "3P")(= DSA  "200A"))(setq GYO "165"))
       (if (and (= DST "3P")(= DSA  "400A"))(setq GYO "166"))
       (if (and (= DST "3P")(= DSA  "600A"))(setq GYO "167"))
       (if (and (= DST "3P")(= DSA "1200A"))(setq GYO "168"))
       (if (and (= DST "EN")(= DSA  "200A"))(setq GYO "169"))
       (if (and (= DST "EN")(= DSA  "400A"))(setq GYO "170"))
       (if (and (= DST "EN")(= DSA  "600A"))(setq GYO "171"))
       (if (and (= DST "EN")(= DSA "1200A"))(setq GYO "172"))
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-DS (append D-DS (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn ;;(princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU1) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       ;;VCB
       (setq RETU1 RETU)
       (if (and (= VCBT "手動")(= VCBKA "8KA")   (= VCBA "400A"))  (setq GYO "181"))
       (if (and (= VCBT "固定")(= VCBKA "8KA")   (= VCBA "400A"))  (setq GYO "182"))
       (if (and (= VCBT "手動")(= VCBKA "12.5KA")(= VCBA "600A"))  (setq GYO "183"))
       (if (and (= VCBT "固定")(= VCBKA "12.5KA")(= VCBA "600A"))  (setq GYO "184"))
       (if (and (= VCBT "引出")(= VCBKA "12.5KA")(= VCBA "600A"))  (setq GYO "185"))
       (if (and (= VCBT "固定")(= VCBKA "20KA")  (= VCBA "600A"))  (setq GYO "186"))
       (if (and (= VCBT "引出")(= VCBKA "20KA")  (= VCBA "600A"))  (setq GYO "187"))
       (if (and (= VCBT "固定")(= VCBKA "20KA")  (= VCBA "1200A")) (setq GYO "188"))
       (if (and (= VCBT "引出")(= VCBKA "20KA")  (= VCBA "1200A")) (setq GYO "189"))
       (if (and (= VCBT "固定")(= VCBKA "25KA")  (= VCBA "600A") (= HV "3300V"))  (setq GYO "190"))
       (if (and (= VCBT "引出")(= VCBKA "25KA")  (= VCBA "600A") (= HV "3300V"))  (setq GYO "191"))
       (if (and (= VCBT "固定")(= VCBKA "25KA")  (= VCBA "1200A")(= HV "3300V"))  (setq GYO "192"))
       (if (and (= VCBT "引出")(= VCBKA "25KA")  (= VCBA "1200A")(= HV "3300V"))  (setq GYO "193"))
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
       (repeat 15 
         (setq SEL1 (strcat RETU1 GYO))(setq SEL (xls_getval SEL1))(setq D-VCB (append D-VCB (list SEL)))
         (if (= (strlen RETU1) 2)
             (progn (princ "\n列=")(princ RETU1)
                 (setq RETU1 (substr RETU1 2 1))(setq RETU1 (strcat PFX (chr (+ (ascii RETU1) 1))))
             )
         )
         (if (= (strlen RETU) 1)(setq RETU1 (chr (+ (ascii RETU1) 1))))
       )
       (setq D-VCBT2 (nth 2 D-VCB))(setq D-VCB (subst KVCB1 D-VCBT2 D-VCB)) ;選定VCBに書換
       (setq D-VCBT2 nil)
       (setq D-VCBT2 (nth 3 D-VCB))(setq D-VCB (subst KVCB2 D-VCBT2 D-VCB)) ;選定VCBに書換
    (xls_close)

;;挿入図形の位置計算
    (ST1)(prompt "\n挿入点を指示... ")
    (command "INSERT" STD pause "1" "0")
    (setq PC1 (getvar "LASTPOINT"))
    (setq PC2 (list (car PC1) (+ (cadr PC1) 300)))           ;CTの挿入点
       (setq PC21 (list (+ (car PC1) 600)  (cadr PC2)))      ;OCRの終点
       (setq PC22 (list (+ (car PC1) 850)  (cadr PC2)))      ;APFCの終点
       (setq PC23 (list (+ (car PC1) 1100) (cadr PC2)))      ;KWの終点
       (setq PC24 (list (+ (car PC1) 1450) (cadr PC2)))      ;VARの終点
       (setq PC25 (list (+ (car PC1) 1800) (cadr PC2)))      ;COSの終点
       (setq PC26 (list (+ (car PC1) 2040) (cadr PC2)))      ;WHMの終点
       (setq PC27 (list (+ (car PC1) 2290) (cadr PC2)))      ;W/TDの終点
       (setq PC28 (list (+ (car PC1) 2590) (cadr PC2)))      ;A/TDの終点
    (setq PC3 (list (car PC1) (+ (cadr PC1) 1475)))          ;VCTの始点
    (setq PC4 (list (car PC1) (+ (cadr PC1) 1375)))          ;VCTの終点
    (setq PC5 (list (car PC1) (+ (cadr PC1) 1600)))          ;PASの挿入点
    (setq PC6 (list (- (car PC1) 500) (cadr PC1)))           ;ZPDの挿入点
    (setq PC7 (list (- (car PC1) 650) (+ (cadr PC1) 725)))   ;OP-VTの削除点A
    (setq PC8 (list (- (car PC1) 1200)(+ (cadr PC1) 950)))   ;OP-VTの削除点B
    (setq PC9 (list (car PC1) (+ (cadr PC1) 650)))           ;VCB引出記号の挿入点
;;======================================================================================================
;;標準図の図形選択範囲
  (setq FP1 (list (- (car PC1) 1500) (+ (cadr PC1) 2500)))  ;フェンス左上
  (setq FP2 (list (- (car PC1) 1500) (- (cadr PC1) 0)))     ;フェンス左下
  (setq FP3 (list (+ (car PC1) 3400) (- (cadr PC1) 0)))     ;フェンス右下
  (setq FP4 (list (+ (car PC1) 3400) (+ (cadr PC1) 2500)))  ;フェンス右上
  

;;属性の編集
  (setq n 0)
  (setq AA (ssget "WP" (list FP1 FP2 FP3 FP4) (list '(0 . "INSERT"))))
  (while (setq BB (ssname AA n))
     (setq ENT (entget BB))
;     (princ "ENT=")(princ (cdr (assoc 2 ENT)))
        ;;属性の書込対象となるシンボル
        (if (or (= (cdr (assoc 2 ENT)) "QDS2")       (= (cdr (assoc 2 ENT)) "QVCB1")
                (= (cdr (assoc 2 ENT)) "QCTH")       (= (cdr (assoc 2 ENT)) "QVTF")
                (= (cdr (assoc 2 ENT)) "QVTT")       (= (cdr (assoc 2 ENT)) "QCTT")
                (= (cdr (assoc 2 ENT)) "QOCR")      ;(= (cdr (assoc 2 ENT)) "QPFL")
                (= (cdr (assoc 2 ENT)) "QVM")        (= (cdr (assoc 2 ENT)) "QAM")
                (= (cdr (assoc 2 ENT)) "QUVR-1")     (= (cdr (assoc 2 ENT)) "QOVR-1")
                (= (cdr (assoc 2 ENT)) "QFM-1")      (= (cdr (assoc 2 ENT)) "QWM-1")
                (= (cdr (assoc 2 ENT)) "QVAR-1")     (= (cdr (assoc 2 ENT)) "QPFM-1")
                (= (cdr (assoc 2 ENT)) "QWHM-1")
                (= (cdr (assoc 2 ENT)) "QVTD-1")     (= (cdr (assoc 2 ENT)) "QATD-1")
               ;(= (cdr (assoc 2 ENT)) "QVARTD")     (= (cdr (assoc 2 ENT)) "QPFTD")
                (= (cdr (assoc 2 ENT)) "QWTD-1")     (= (cdr (assoc 2 ENT)) "QAPFC-1")
                (= (cdr (assoc 2 ENT)) "QTRH")       (= (cdr (assoc 2 ENT)) "QMCB2")
                (= (cdr (assoc 2 ENT)) "QLA1-1")
            );or
            (progn
;              (princ "ENT=")(princ (cdr (assoc 2 ENT)))
               (setq DATT nil)
               (if (= (cdr (assoc 2 ENT)) "QDS2")   (setq DATT D-DS))
               (if (= (cdr (assoc 2 ENT)) "QVCB1")  (setq DATT D-VCB))
               (if (= (cdr (assoc 2 ENT)) "QCTH")   (setq DATT D-CTH))
               (if (= (cdr (assoc 2 ENT)) "QVTF")   (setq DATT D-VT))
               (if (= (cdr (assoc 2 ENT)) "QVTT")   (setq DATT D-VTT))
               (if (= (cdr (assoc 2 ENT)) "QCTT")   (setq DATT D-CTT))
               (if (= (cdr (assoc 2 ENT)) "QOCR")   (setq DATT D-OCR))
               (if (= (cdr (assoc 2 ENT)) "QAM")    (setq DATT D-AM))
               (if (= (cdr (assoc 2 ENT)) "QVM")    (setq DATT D-VM))
               (if (= (cdr (assoc 2 ENT)) "QUVR-1") (setq DATT D-UV))
               (if (= (cdr (assoc 2 ENT)) "QOVR-1") (setq DATT D-OV))
               (if (= (cdr (assoc 2 ENT)) "QFM-1")  (setq DATT D-HZ))
               (if (= (cdr (assoc 2 ENT)) "QWM-1")  (setq DATT D-KW))
               (if (= (cdr (assoc 2 ENT)) "QVAR-1") (setq DATT D-VAR))
               (if (= (cdr (assoc 2 ENT)) "QPFM-1") (setq DATT D-COS))
               (if (= (cdr (assoc 2 ENT)) "QWHM-1") (setq DATT D-WHM))
               (if (= (cdr (assoc 2 ENT)) "QVTD-1") (setq DATT D-VTD))
               (if (= (cdr (assoc 2 ENT)) "QATD-1") (setq DATT D-ATD))
               (if (= (cdr (assoc 2 ENT)) "QWTD-1") (setq DATT D-KWTD))
               (if (= (cdr (assoc 2 ENT)) "QAPFC-1")(setq DATT D-APFC))
               (if (= (cdr (assoc 2 ENT)) "QTRH")   (setq DATT D-OPVT))
               (if (= (cdr (assoc 2 ENT)) "QMCB2")  (setq DATT D-MCB))
               (if (= (cdr (assoc 2 ENT)) "QLA1-1") (setq DATT D-LA))
;               (princ "\nDATT=")(princ DATT)
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
                         (if (= N2 15)(setq ZOKU "Z1")) (if (= N2 16)(setq ZOKU "Z2"))
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
      ;;必要に応じて不要なシンボルを消去する
        ;;VCTが無い場合
        (if (and (= VCT "N")(= (cdr (assoc 2 ENT)) "QVCT-1"))
            (progn
;               (command "PLINE" PC3 "W" "10" "" PC4 "")
               (entdel BB) ; VCTを消す
            )
        )
      ;;オプションメーター
        (if (and (/= M1 "VM") (= (cdr (assoc 2 ENT)) "QVM"))   (entdel BB)); VMを消す
        
        ;;KWが無い場合
        (if (and (/= M2 "KW") (= (cdr (assoc 2 ENT)) "QWM-1"))
            (progn
;               (command "PLINE" PC22 "W" "0" "" PC23 "")
               (entdel BB) ; KWを消す
            )
        )
        ;;VARが無い場合
        (if (and (/= M4 "VAR")(= (cdr (assoc 2 ENT)) "QVAR-1"))
            (progn
;               (command "PLINE" PC23 "W" "0" "" PC24 "")
               (entdel BB) ; VARを消す
            )
        )
        ;;COSが無い場合
        (if (and (/= M3 "COS")(= (cdr (assoc 2 ENT)) "QPFM-1"))
            (progn
;               (command "PLINE" PC24 "W" "0" "" PC25 "")
               (entdel BB) ; COSを消す
            )
        )
        
        (if (and (/= M5 "HZ") (= (cdr (assoc 2 ENT)) "QFM-1")) (entdel BB)); HZを消す
        
      ;;オプションT/D
        (if (and (/= TD2 "VM")(= (cdr (assoc 2 ENT)) "QVTD-1"))(entdel BB)); VTDを消す
        
        ;;A/TDが無い場合
        (if (and (/= TD1 "AM")(= (cdr (assoc 2 ENT)) "QATD-1"))
            (progn
;               (command "PLINE" PC27 "W" "0" "" PC28 "")
               (entdel BB) ; ATDを消す
            )
        )
        ;;KW/TDが無い場合
        (if (and (/= TD3 "KW")(= (cdr (assoc 2 ENT)) "QWTD-1"))
            (progn
;               (command "PLINE" PC26 "W" "0" "" PC27 "")
               (entdel BB) ; WTDを消す
            )
        )
        ;;UV,OVが無い場合
        (if (and (= UV "N")(= (cdr (assoc 2 ENT)) "QUVR-1"))(entdel BB)); UVを消す
        (if (and (= OV "N")(= (cdr (assoc 2 ENT)) "QOVR-1"))(entdel BB)); OVを消す
        
        ;;ＷＨＭが無い場合
        (if (and (= WHT "N")(= (cdr (assoc 2 ENT)) "QWHM-1"))
            (progn
;               (command "PLINE" PC25 "W" "0" "" PC26 "")
               (entdel BB) ; WHMを消す
            )
        )
        ;;APFCが無い場合
        (if (and (= APFC "N")(= (cdr (assoc 2 ENT)) "QAPFC-1"))
            (progn
               (command "PLINE" PC21 "W" "0" "" PC22 "")
               (entdel BB) ; APFCを消す
            )
        )
        ;;操作用VTが無い場合
        (if (and (= OPVT "N")(= (cdr (assoc 2 ENT)) "QTRH" ))(entdel BB)); OPVTを消す
        (if (and (= OPVT "N")(= (cdr (assoc 2 ENT)) "QMCB2"))(entdel BB)); OP-MCBを消す
        
;; ICAD5ではcommand "ERASE" "W"で"W"内に削除図形が無いとエラーが出るので、仮に"KOUTEN"を挿入してから削除を行う
     (setq TMP1 (list (+ (car PC8) 50)(- (cadr PC8) 50)))
     (command "INSERT" "KOUTEN" TMP1  "1" "1" "0")
        (command "ERASE" PC7 PC8 "")                                     ; 余分な線を消す
        
        ;;LAが無い場合
        (if (and (= LA "N")(= (cdr (assoc 2 ENT)) "QLA1-1"))(entdel BB)); LAを消す
        
        ;;GRがKD手配の場合
        (if (and (= SHI "N")(= (cdr (assoc 2 ENT)) "ST-PGS1"))(entdel BB)); PASを消す
        (if (and (= SHI "N")(= (cdr (assoc 2 ENT)) "QZCTT"))  (entdel BB)); ZCTを消す
        
        ;;マルチメーターの場合
;        (if (and (= G14 "KM")(= (cdr (assoc 2 ENT)) "MVM"))(entdel BB)); VMを消す
;        (if (and (= G14 "KM")(= (cdr (assoc 2 ENT)) "MVS"))(entdel BB)); VSを消す
;        (if (and (= G14 "KM")(= (cdr (assoc 2 ENT)) "MAM"))(entdel BB)); AMを消す
;        (if (and (= G14 "KM")(= (cdr (assoc 2 ENT)) "MAS"))(entdel BB)); ASを消す
        
        ;;線サイズの変更
        (if (= (cdr (assoc 2 ENT)) "QYAH")
            (progn
               (setq CC (entnext BB)  DD (entget CC))
               (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
                  (if (= (cdr (assoc 0 DD)) "ATTRIB")
                      (progn
                         (setq ZOKU "N1")
                         (if (= (cdr (assoc 2 DD)) ZOKU)
                             (entmod (subst (cons 1 HSq)(assoc 1 DD) DD));属性を変更
                         );if
                      );progn
                  );if
                  (setq CC (entnext CC) DD (entget CC))
               );while
            );progn
        );if
        
        ;;１次幹線と開閉器の変更
        (if (= (cdr (assoc 2 ENT)) "ST-PGS1")
            (progn
               (setq CC (entnext BB)  DD (entget CC))
               (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
                  (if (= (cdr (assoc 0 DD)) "ATTRIB")
                      (progn
                        (if (= (cdr (assoc 2 DD)) "SHU1")(entmod (subst (cons 1 Z1)(assoc 1 DD) DD)))
                        (if (= (cdr (assoc 2 DD)) "SHU2")(entmod (subst (cons 1 Z2)(assoc 1 DD) DD)))
                        (if (= (cdr (assoc 2 DD)) "SHU3")(entmod (subst (cons 1 Z3)(assoc 1 DD) DD)))
                        (if (= (cdr (assoc 2 DD)) "SHU4")(entmod (subst (cons 1 Z4)(assoc 1 DD) DD)))
                      );progn
                  );if
                  (setq CC (entnext CC) DD (entget CC))
               );while
            );progn
        );if
        (setq n (1+ n)) 
   );while
;;-------------------------------------------------------------------------------------------------
;;ＧＲがＫＤ手配の場合
   (if (= SHI "N")(command "INSERT" "*ST-PGS2" PC5 "1" "0"))
   (if (and (= SHI "N")(= GRT "DGR"))
       (command "INSERT" "QZPD" PC6 "1" "1" "0"
                 (nth 0 D-ZPD) (nth 1 D-ZPD) (nth 2 D-ZPD) "" "" "" "" "" "" "" "" MN "1" "" "")
   )
;;ＤＧＲ・ＺＣＴ・ＺＣＴＴの属性編集
  (setq n 0)
  (setq AA (ssget "WP" (list FP1 FP2 FP3 FP4) (list '(0 . "INSERT"))))
  (while (setq BB (ssname AA n))
     (setq ENT (entget BB))
;     (princ "ENT=")(princ (cdr (assoc 2 ENT)))
        ;;属性の書込対象となるシンボル
        (if (or (= (cdr (assoc 2 ENT)) "QDGR")  (= (cdr (assoc 2 ENT)) "QZCTH")
                (= (cdr (assoc 2 ENT)) "QZCTT")
            );or
            (progn
;              (princ "ENT=")(princ (cdr (assoc 2 ENT)))
               (setq DATT nil)
               (if (= (cdr (assoc 2 ENT)) "QDGR")   (setq DATT D-GR))
               (if (= (cdr (assoc 2 ENT)) "QZCTH")  (setq DATT D-ZCT))
               (if (= (cdr (assoc 2 ENT)) "QZCTT")  (setq DATT D-ZCTT))
;               (princ "\nDATT=")(princ DATT)
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
                         (if (= N2 15)(setq ZOKU "Z1")) (if (= N2 16)(setq ZOKU "Z2"))
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
        (setq n (1+ n)) 
   );while
;;VCB引出記号の挿入
  (if (= VCBT "引出")(command "INSERT" "Q-HIKIDASI" PC9 "1" "1" "0"))

;;サーマルとＷＨＭが無くて、ﾏﾙﾁﾒｰﾀｰじゃない場合
;        (if (and (= G3 "WH0")(= G5 "N"))
;            (command "PLINE" PC95 "W" "0" "" PC92 "")
;            (progn ; else ;ＷＨＭが無い場合
;              (if (and (= G3 "WH0")(/= G5 "N"))(command "PLINE" PC94 "W" "0" "" PC92 "")) 
;                          ;サーマルが無い場合
;              (if (and (/= G3 "WH0")(= G5 "N"))(command "PLINE" PC95 "W" "0" "" PC94 "")) 
;            )
;        )

;;マルチメーターの場合
;     (if (= G14 "KM")
;         (progn
;            (command "PLINE" PC82 "W" "0" "" PC83 PC84 "")      ; 補助線
;            (command "INSERT" "MMM"  PC92 "1" "1" "0" "MM" MMPNo KAM "電圧・電流" "" "" "" "" "" "" "")
;         )
;     )

;;ＬＧＲへの矢印挿入
;     (if (= LG "N") (command "INSERT" "MZCTL1" PC5 "1" "1" "0"))
     

;;======================================================================================================
   (command "REGEN");再作図をしないと画面上に変更が反映されない
   (redraw)
   (setvar "CMDECHO" 1)
   (setvar "USERI5" 0)
   (setvar "USERS5" "A");;C:SQ3Wを手動モードに戻す
   (prompt "\n作図完了！！")
)