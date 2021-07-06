;;***************************************************************************************************
;;制御回路用LISP    制御回路-B.LSP
;;***************************************************************************************************
;;  機  能                     関  数  名   旧LISP名
;;===========================  ==========   ============
;;1.制御回路で母線に縦線を引く C:TATESEN    TATESEN.LSP
;;2.シーケンス回路用 PLINE作成 C:SLI        SLine.LSP
;;3.ブロック移動               C:BMV        BMOVE.LSP
;;4.ブロックコピー             C:BCP        BCOPY.LSP
;;5.ブロック削除               C:BDEL       BDEL.LSP
;;6.制御回路母線作成           C:BOSEN      BOSEN.LSP
;;7.制御回路の母線と縦線を書く C:BOSEN-A    BOSEN-A.LSP
;;8.作図を実線に設定           C:J-LINE
;;9.作図を点線に設定           C:T-LINE
;;10.作図を破線に設定          C:H-LINE
;;=================================================================================================
;;8,9,10.線種の設定                           2020/7/8 By T.Terada
===================================================================================================
(defun C:J-LINE (/ )
  (setq *error* *err1*)(setvar "CMDECHO" 0)
  (command "LINETYPE" "S" "CONTINUOUS" "")
  (princ "線種を <実線>に変更しました。")(princ)
)
;;-------------------------------------------------------------------------------------------------
(defun C:T-LINE (/ )
  (setq *error* *err1*)(setvar "CMDECHO" 0)
  (command "LINETYPE" "S" "DASHED" "")
  (princ "線種を <点線>に変更しました。")(princ)
)
;;-------------------------------------------------------------------------------------------------
(defun C:H-LINE (/ )
  (setq *error* *err1*)(setvar "CMDECHO" 0)
  (command "LINETYPE" "S" "CENTER2" "")
  (princ "線種を <破線>に変更しました。")(princ)
)


;;=================================================================================================
;;1.制御回路で母線に縦線を引く。 TATESEN.LSP  2004/2/26 By T.Terada  ＊制御回路.LSPのC:SLIを利用
;;=================================================================================================
(defun C:TATESEN (/ )
  (setq *error* *err1*)
  (setvar "CMDECHO" 0)
  (command "COLOR" 4);(command "OSNAP" "A")
  (setq POS1 nil POS2 nil)
  (setvar "USERS5" "A") ;SLIを自動モードで使う
  (setq BW (getint"\n母線の縦幅を入力 <例:1000>：")) (if (= BW nil)(setq BW 1000))
  (while (= POS1 nil)
             
            (setq POS1 (getpoint "\nタテ線を引く母線の位置上をピック /終了=ﾘﾀｰﾝ："))
            (if (= POS1 nil) ;SLIを手動モードにもどす 
                (progn 
                   (setvar "USERS5" "M")(setq U5 "M")
                   (setvar "CMDECHO" 1)(setq POS1 "" POS2 nil)
                   (princ "\nUSERS5=")(princ (getvar "USERS5"))
                   (exit)
                )
            )
            (if (/= POS1 nil);SLI自動モード
                (progn (setq POS2 (list (car POS1)(- (cadr POS1) BW)))(C:SLI)(setq POS1 nil))
            )
  )
  (princ)
  (setq POS1 nil POS2 nil)
  (setvar "USERS5" "M")(setq U5 "M");SLIを手動モードに戻す
  (setvar "CMDECHO" 1)
  (gc)(princ "\nUSERS5=")(princ USERS5)
)
;;=================================================================================================
;;2.シーケンス回路用 PLINE作成 LISP    SLine.LSP    2002/NOV     By T.Terada
;;=================================================================================================
;;iLine2.LSP  by izawa 2000.3.12を改造  
;;   * LINEをPLINEに変更
;;   * 属性付きBLOCKに対応
;;   * TRIMを用いずにBREAKを使用
;;未解決点
;;  1)ブロックの挿入回転角、縦方向のみにしか対応出来ない。
;;  2)交点のDIMSCALEを図面サイズで調整する事に未対応。
;;-------------------------------------------------------------------------
;; ※線分作成後、交点にシンボルを挿入する。＋ シンボル背景削除。
;; ※交点にすでに何らかのブロックが存在する場合は挿入しない。
;; ※端点同士の交点には挿入しない。
;; ※KOUTENという名前のブロックは削除対象とはならない。
;********************************************************************
(defun C:SLI( / SS ENAME EDATA LEN i POS3 POS4 BLK_LIST BLK_ELIST INT_POS DIMSC); POS1 POS2)
  (setq *error* *err1*)
  (setvar "CMDECHO" 0)
  (setvar "orthomode" 1)
;  (command "OSNAP" "OFF")
  ;交点のシンボル名
    (setq $KOUTEN "KOUTEN")
    (setq LEN 0)
  ;交点用のブロックが存在するか
  (setq U5 (getvar "USERS5")) ; C:SLIを自動モードで使う。
  (if (/= (findfile (strcat $KOUTEN ".dwg")) nil )
    (progn
       ;２点取得
       (if (= U5 "M")
           (progn
              (setq POS2 nil) 
              (setq POS1 (getpoint "\n始点を指示: "))
              (if (/= POS1 nil)
                  (setq POS2 (getpoint POS1 "\n終点を指示 : "))
              )
            )
       )
    )
    (princ "\nﾌﾞﾛｯｸ(kouten.dwg)が見つかりません.Icadと同じﾌｫﾙﾀﾞに必要です.")
  )
  ;PLINE作成、SELECT F
  (if (/= POS2 nil)
      (progn
         (setvar "CMDECHO" 0)
         (command "COLOR" 4)
         (command "PLINE" POS1 POS2 "")
         (setq ss (ssget "F" (list POS1 POS2)))
         (if (/= ss nil)
             (setq LEN (sslength ss));自分を含めての数
             (setq LEN 0);else
         )
      )
  )
  (if (> len 1)
      (progn
         (setq DIMSC (/ (getvar "DIMSCALE") 80));;交点のサイズ調整:80で直径10
        ;挿入されたブロックを取得し座標を比較------------------------------------
         (setq BLK_LIST nil)
         (setq BLK_ELIST nil)
         (setq i 0)
         (repeat (sslength ss )
             (setq ENAME (ssname ss i))
             (setq EDATA (entget ENAME ))
             (if (= (cdr(assoc 0 edata)) "INSERT")
                 (progn
                    (setq BLK_LIST (cons (cdr(assoc 10 edata)) BLK_LIST))
                    (setq BLK_ELIST (cons ENAME BLK_ELIST))
                 )
             )
             (setq i (1+ i))
         )
;        (princ "BLK_ELIST=")(princ BLK_ELIST)(prompt "\n")
;        (princ "LENGTH BLK_ELIST=")(princ (length BLK_ELIST))(prompt "\n")
      
;;PLINEを取得し、交点を求め、KOUTEN.DWGを挿入-------------------------------------
      (setq i 0)
      (repeat (sslength ss )
          (setq edata (entget (ssname ss i)))
          (if (= (cdr(assoc 0 edata)) "LWPOLYLINE")
              (progn
                (EPL1)
                (setq int_pos (inters POS1 POS2 POS3 POS4));;plineの交点を求める
;               (princ "int_pos=")(princ int_pos)(prompt "\n")
                (if (/= int_pos nil)
              ;端点同士でなければ．．．
                   (if (not (int_hasi pos1 pos2 pos3 pos4))
                ;交点にブロックが存在していなければ．．
                       (if (= (in_poslist BLK_LIST int_pos ) nil)
                           (progn
                    ;内部に交点があるかの確認
                              (if (/= (tblsearch "BLOCK" $KOUTEN ) nil )
                                 (command "INSERT" $KOUTEN int_pos dimsc dimsc "0")
                                 (command "INSERT" (strcat $KOUTEN ".dwg") int_pos dimsc dimsc "0")
                             )
                    ;同じところにマークを打たないようにLISTに追加
                             (setq blk_list (cons int_pos blk_list))
                          )
                       )
                   )
              )
           )
        )
        (setq i (1+ i))
      )
      ;ブロックの線、切取り-------------------------------------------------
      (setq i 0)
      (repeat (length blk_elist)
        (setq ename (nth i blk_elist))
        (if (/= (cdr(assoc 2 (entget ename))) $KOUTEN )
            (P-BRK ename)
        )
        (setq i (+ i 1))
      )
    )
  )
;  (setvar "USERS5" "M") ;SLIを自動モードに戻す
  (redraw)
  (if (= (getvar "USERS5") "A")(prompt "\n自動モードに成っています。手動には「手」アイコンを押して下さい！"))
);End Def
;*********************************************************
;リストb_listにi_posが含まれているか（誤差を考慮）
;*********************************************************
(defun in_poslist( b_list i_pos / b_list i_pos res flag i posn)
  (setq res nil)
  (if (and (/= b_list nil)(/= i_pos nil))
    (progn
      (setq flag T)
      (setq i 0)
      (while flag
        (setq posn (nth i b_list))
        (if (/= posn nil)
          (progn
            (if (equal posn i_pos 0.01)
              (progn
                (setq res T)
                (setq flag nil)
              )
            )
          )
          (setq flag nil)
        )
        (setq i (+ i 1))
      )
    )
  )  
  res
) 
;********************************************************
;線分の両端同士がつながっているか
;********************************************************
(defun int_hasi( pos1a pos1b pos2a pos2b / pos1a pos1b res)
;  (princ "in_hasiに入ります")
  (setq res nil)
  (if (equal pos1a pos2a 0.1)(setq res T))
  (if (equal pos1a pos2b 0.1)(setq res T))
  (if (equal pos1b pos2a 0.1)(setq res T))
  (if (equal pos1b pos2b 0.1)(setq res T))
  res
;;(princ "res=")(princ res)(prompt "\n")
)
;*************************************************
;PLINEの始点・終点を取得する
;*************************************************
(defun EPL1( / j n ENo P1L P2L PS PE sum atm)
;;  (prompt "\n==> EPL1")
  (setq sum 0.0)
    (progn
      (setq j 0)
      (setq n 0)
      (repeat (length edata)
        (setq atm (nth j edata) ENo (car atm))
        (if (= ENo 10)
          (progn
            (setq n (1+ n))
            (if (> n 1)
              (progn;                    終点:n > 1
                (setq P2L (cdr atm))
                (setq sum (+ sum (distance P1L P2L)))
                (setq P1L P2L)
              );progn
              (progn 
                  (setq P1L (cdr atm))
                  (setq Pos3 P1L)        ;始点:n = 1
                  (setq PS P1L)
              )
            );if
          );progn
        );if
        (setq j (1+ j))
      );repeat
      (setq Pos4 P2L) (setq PE P2L);始点 = P1L,POS3 終点 = P2L,POS4
    );progn
)
;*************************************************
;PLINEをブロックサイズに合わせてBREAKする
;*************************************************
(defun P-BRK(entname / elist P1 P2 P2Y BLKNAME BLKOBJ ename)
;  (prompt "\n==> P-BRK")(prompt "\n")
  (setq Ymax 0)
  (if (/= entname nil)
    (progn
      (setq elist (entget entname))
      (if (=(cdr (assoc 0 elist)) "INSERT")
        (progn
          (setq P1 (cdr (assoc 10 elist)))
          (setq BLKNAME (cdr (assoc 2 elist)))
;          (princ "BLKNAME =")(princ BLKNAME)(prompt "\n")
 ;ブロック内の図形調査
          (setq ename (cdr (assoc -2 (entget (tblobjname "BLOCK" BLKNAME)))))
          (while ename
            ;ブロックの要素をリストに格納
            (setq BLKOBJ (entget ename))
                 ;エンティティが PLINEの場合
;                 (princ "TYPE =")(princ (cdr (assoc 0 BLKOBJ)))(prompt "\n")
                  (if (= (cdr (assoc 0 BLKOBJ)) "LWPOLYLINE")
                      (progn
                          (Pse)
                          (if (< Ymax Yse)
                              (setq Ymax Yse)
                          )
;                          (princ "Ymax=")(princ Ymax)(prompt "\n")
                      )
                  )
              (setq ename (entnext ename))  ;次を探す。無い時はnil
          );while end
        );progn
      );if 
      ;;PLINEの削除
        (setq P2Y (- (cadr P1) Ymax))
        (setq P2 (list (car P1) P2Y))
        (command "BREAK" P1 P2)
    );progn 
  );if
)

;;=================================================================================================
;;3.ブロック移動       BMOVE.LSP   2002/NOV    by T.Terada
;;=================================================================================================
(defun C:BMV(/ ENT ENTNAME ELIST ENAME BLKNAME BLKOBJ P1 P2 Psel Pd1 Pd2 PH1 PH2 PC1 PC2 Ymax)
  (setq *error* *err1*)(setvar "orthomode" 0)(setvar "CMDECHO" 0)
   (setq ENT (entsel "\n移動するブロックを選択:"))
  (setq ENTNAME (car ENT))
  (if (/= ENTNAME nil);if1
    (progn
       
      (prompt "\n移動先を指定:")
      (setq ELIST (entget ENTNAME))
      (if (= (cdr (assoc 0 ELIST)) "INSERT");if2
        (progn
          (setq P1 (cdr (assoc 10 ELIST)))
          (setq BLKNAME (cdr (assoc 2 ELIST)))

         ;ブロック内の図形調査
          (setq ENAME (cdr (assoc -2 (entget (tblobjname "BLOCK" BLKNAME)))))
               (while ENAME
;                  (princ "ENAME=")(princ ENAME)(prompt "\n")
                 ;ブロックの要素をリストに格納
                  (setq BLKOBJ (entget ENAME))
;                 (princ "BLKOBJ=")(princ BLKOBJ)(prompt "\n")
                 ;エンティティが PLINEの場合
                  (if (= (cdr (assoc 0 BLKOBJ)) "LWPOLYLINE")
                      (progn
                          (Pse)
                          (if (< Ymax Yse)
                              (setq Ymax Yse)
                          )
;                          (princ "Ymax=")(princ Ymax)(prompt "\n")
                      )
                  )
                  (setq ename (entnext ename))  ;次を探す。無い時はnil
               );while end
          ;;ブロックの移動
          (command "MOVE" "single" entname P1 pause)
          (setq P2 (getvar "LASTPOINT"))
         );progn
       );if2
     );progn
  );if1
;;
;;------------------------------------------------------------------------
;;移動先に残った線を削除
       (setq Psel (list (car P2) (+ (cadr P2) 15)))
       (setq Pd1 P2)
       (setq Pd2 (list (car P2) (- (cadr P2) Ymax)))
       (command "BREAK" Psel "F" Pd1 Pd2)
;;------------------------------------------------------------------------
;;移動後に残った線を連結
    (setq P2 (list (car P1) (- (- (cadr P1) Ymax) 10)))
    ;EXTENDの補助線を引く
        (setq PH1 (list (- (car P1) 10) (cadr P1)))
        (setq PH2 (list (+ (car P1) 10) (cadr P1)))
                  (command "PLINE" PH1 PH2 "")
    ;EXTENDの実行
        (command "EXTEND" "L" "" P2 "")
        (command "ERASE" "L" "")
        (princ)

    ;Peditで延長した線を結合する
        (setq PC1 (list (car P1) (+ (cadr P1) 20)))
        (setq PC2 (list (car P1) (- (cadr P1) 20)))
                  (command "PEDIT" PC1 "J" PC2 "" "")

    (redraw)(setvar "CMDECHO" 1)(setvar "orthomode" 1)(princ)
);def
;;=================================================================================================
;;4.ブロックのコピー        BCOPY.LSP        2002/NOV    by T.Terada
;;=================================================================================================
(defun C:BCP(/ ENT ENTNAME ELIST ENAME BLKNAME BLKOBJ P1 P2 Psel Pd1 Pd2 Ymax)
  (setq *error* *err1*)
  (setvar "orthomode" 0)
  (setvar "CMDECHO" 0) 
  (setq ENT (entsel "\nコピーするブロックを選択:"))
  (setq ENTNAME (car ENT))
  (if (/= ENTNAME nil);if1
    (progn
       (prompt "\nコピー先を指定:")
      (setq ELIST (entget ENTNAME))
      (if (= (cdr (assoc 0 elist)) "INSERT");if2
        (progn
          (setq P1 (cdr (assoc 10 ELIST)))
          (setq BLKNAME (cdr (assoc 2 ELIST)))
        ;;ブロック内の図形調査
          (setq ENAME (cdr (assoc -2 (entget (tblobjname "BLOCK" BLKNAME)))))
               (while ENAME
                 ;ブロックの要素をリストに格納
                  (setq BLKOBJ (entget ENAME))
;                 (princ "BLKOBJ=")(princ BLKOBJ)(prompt "\n")
                 ;エンティティが PLINEの場合
                  (if (= (cdr (assoc 0 BLKOBJ)) "LWPOLYLINE")
                      (progn
                          (Pse)
                          (if (< Ymax Yse)
                              (setq Ymax Yse)
                          )
                      )
                  )
                  (setq ENAME (entnext ENAME))  ;次を探す。無い時はnil
               );while end
          ;;ブロックのコピー
          (command "COPY" "single" ENTNAME P1 pause)
          (setq P2 (getvar "LASTPOINT"))
         );progn
       );if2
     );progn
  );if1
;;
;;-----------------------------------------------------------------------------
;;コピー先に残った線を削除
       (setq Psel (list (car P2) (+ (cadr P2) 15)))
       (setq Pd1 P2)
       (setq Pd2 (list (car P2) (- (cadr P2) Ymax)))
       (command "BREAK" Psel "F" Pd1 Pd2)
  (redraw)
  (setvar "orthomode" 1)
  (princ)
);def

;;=================================================================================================
;;5.ブロックの削除                        BDEL.LSP   2002/NOV    by T.Terada
;;=================================================================================================
(defun C:BDEL(/ ENT ENTNAME ELIST ENAME BLKNAME BLKOBJ P1 P2 PH1 PH2 PC1 PC2 Ymax)
  (setq *error* *err1*)
  (setvar "CMDECHO" 0)
   (setq ENT (entsel "\n削除するブロックを選択:"))
  (setq ENTNAME (car ENT))
  (if (/= ENTNAME nil);if1
    (progn
      (setq ELIST (entget ENTNAME))
      (if (= (cdr (assoc 0 ELIST)) "INSERT");if2
        (progn
          (setq P1 (cdr (assoc 10 ELIST)))
          (setq BLKNAME (cdr (assoc 2 ELIST)))

         ;ブロック内の図形調査
          (setq ENAME (cdr (assoc -2 (entget (tblobjname "BLOCK" BLKNAME)))))
               (while ENAME
;                  (princ "ENAME=")(princ ENAME)(prompt "\n")
                 ;ブロックの要素をリストに格納
                  (setq BLKOBJ (entget ENAME))
;                 (princ "BLKOBJ=")(princ BLKOBJ)(prompt "\n")
                 ;エンティティが PLINEの場合
                  (if (= (cdr (assoc 0 BLKOBJ)) "LWPOLYLINE")
                      (progn
                          (Pse)
                          (if (< Ymax Yse)
                              (setq Ymax Yse)
                          )
;                          (princ "Ymax=")(princ Ymax)(prompt "\n")
                      )
                  )
                  (setq ename (entnext ename))  ;次を探す。無い時はnil
               );while end
          ;;ブロックの移動
           (command "ERASE" ENTNAME "")
;          (setq P2 (getvar "LASTPOINT"))
         );progn
       );if2
     );progn
  );if1
;;
;;移動後に残った線を連結
    (setq P2 (list (car P1) (- (- (cadr P1) Ymax) 10)))
    ;EXTENDの補助線を引く
        (setq PH1 (list (- (car P1) 10) (cadr P1)))
        (setq PH2 (list (+ (car P1) 10) (cadr P1)))
                  (command "PLINE" PH1 PH2 "")
    ;EXTENDの実行
        (command "_EXTEND" "L" "" P2 "")
        (command "ERASE" "L" "")
        (princ)

    ;Peditで延長した線を結合する
        (setq PC1 (list (car P1) (+ (cadr P1) 20)))
        (setq PC2 (list (car P1) (- (cadr P1) 20)))
                  (command "PEDIT" PC1 "J" PC2 "" "")

  (redraw)(setvar "CMDECHO" 1)
);def

;;=================================================================================================
;;6.制御回路の母線を書く    BOSEN.LSP         2002/Dec  by T.Terada
;;=================================================================================================
(defun C:BOSEN( / P1 P2 P11 BW PU PUL PUR PU2 PD PDL PDR PD2 PZ RK SK)
;   (setq *error* *err1*)
   (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
   (if (= (getvar "USERI5") 0);Access自動作図のフラグ
     (progn
       (setq BW (getint"\n母線の縦幅を入力 <例:1000>：")) (if (= BW nil)(setq BW 1000))
        (setq P1 (getpoint "\n始点を指示:"))
        (if (/= P1 nil)(setq P2 (getpoint P1 "\n終点を指示:")))
     )
   )
   (if (= (getvar "USERI5") 1)(progn (setq BW 1000)(setq P1 ACP1)(setq P2 ACP2)))
   (if (/= P2 nil)(progn (setvar "CMDECHO" 1)(command "COLOR" 4)(command "PLINE" P1 P2 "")))
   (setq P11 (list (car P1) (- (cadr P1) 50)));offset側
   (setq P12 (list (+ (car P1) 50) (cadr P1)));YUD1の挿入基点
   (command "OFFSET" BW "L" P11 "")
(princ "\n作図完了")

;;形式１の選択
   (setq PU  (list (+ (car P1) 50)(- (cadr P1) 25)))
   (setq PUL (list (- (car PU) 20)(- (cadr PU) 30)))
   (setq PUR (list (+ (car PU) 20)(- (cadr PU) 30)))
   (setq PU2 (list (+ (car P1) 50)(- (cadr P1) (- (/ BW 2) 50))))
   (setq PD  (list (+ (car P1) 50)(- (cadr P1) (- BW 25))))
   (setq PDL (list (- (car PD) 20)(+ (cadr PD) 30)))
   (setq PDR (list (+ (car PD) 20)(+ (cadr PD) 30)))
   (setq PD2 (list (car PU2)(- (cadr PU2) 100)))
   (setq PZ  (list (+ (car P1) 50)(- (cadr P1) (/ BW 2))))
   (command "PLINE" PU PU2 "")(command "PLINE" PUL PU PUR "")
   (command "PLINE" PD PD2 "")(command "PLINE" PDL PD PDR "")
;      (INITGET 1 "AC100 AC200 AC24 DC24 DC100")
       (INITGET 1 "1 2 3 4 5")
        (if (= (getvar "USERI5") 0)(setq K1 (GETKWORD "\n回路電圧を選択：1/AC100 2/AC200 3/AC24 4/DC24 5/DC100")))
        (if (= (getvar "USERI5") 1)(setq K1 ACK1))
	(cond ((= K1 "1")(setq K1 "AC100V")(setq RK "01R1")(setq SK "01N1"))
	      ((= K1 "2")(setq K1 "AC200V")(setq RK "01R2")(setq SK "01S2"))
	      ((= K1 "3")(setq K1 "AC24V") (setq RK "R24") (setq SK "S24"))
	      ((= K1 "4")(setq K1 "DC24V") (setq RK "P24") (setq SK "N24"))
	      ((= K1 "5")(setq K1 "DC100V")(setq RK "P100")(setq SK "N100"))
        ) (princ "\nK1 =")(princ K1)
        (command "COLOR" 2)(command "INSERT" "YUDN" PZ "1" "1" "0" K1)
        (setq PC1 (list (car P1) (+ (cadr P1) 5)))
        (setq PC2 (list (car P1) (- (- (cadr P1) BW) 25)))
        (command "TEXT" "S" "M0" PC1 "20" "0" RK)
        (command "TEXT" "S" "M0" PC2 "20" "0" SK)(command "COLOR" 4)
   (setvar "USERI5" 0) ;Access自動作図のフラグ
   (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)(redraw)
   (prompt "\n属性の編集で電圧を変更出来ます。")
)

;*********************************************************************
;PLINEの大きさを得る
;*********************************************************************
(defun Pse ( / i n sum atm ENo PL1 PL2 Ps Pe Ys Ye)
      (setq i 0)
      (setq n 0)
      (setq Yse 0)
      (setq sum 0.0)
      (repeat (length BLKOBJ)
        (setq atm (nth i BLKOBJ) ENo (car atm))
        (if (= ENo 10)
          (progn
            (setq n (1+ n))
            (if (> n 1)
              (progn
                (setq PL2 (cdr atm))
                (setq sum (+ sum (distance PL1 PL2)))
                (setq PL1 PL2)
              );progn
              (progn (setq PL1 (cdr atm)) (setq ps PL1))
            );if
          );progn
        );if
        (setq i (1+ i))
      );repeat
    (setq pe PL2)

   (if (= (car Ps) 0)(setq Ys (abs (cadr Ps))))
   (if (= (car Pe) 0)(setq Ye (abs (cadr Pe))))
   (if (= Ys nil) (setq Ys 0))
   (if (= Ye nil) (setq Ye 0))
   (setq Yse (max Ys Ye))
)
;;=================================================================================================
;;7.制御回路の母線と縦線を書く    BOSEN-A.LSP         2005/JUL  by T.Terada
;;=================================================================================================
(defun C:BOSEN-A( / P1 P2 P11 BW PU PUL PUR PU2 PD PDL PDR PD2 PZ RK SK LW NN)
   (setq *error* *err1*)
    (setvar "BLIPMODE" 0)
   (if (= (getvar "USERI5") 0)
       (progn
          (setq BW (getint"\n母線の縦幅を入力 <例:1000>：")) (if (= BW nil)(setq BW 1000))
           (setq P1 (getpoint "\n始点を指示:"))
           (if (/= P1 nil)(setq P2 (getpoint P1 "\n終点を指示:")))
       )
   )
   (if (= (getvar "USERI5") 1)(progn (setq BW 1000)(setq P1 ACP1)(setq P2 ACP2)))
   (if (/= P2 nil)(progn (setvar "CMDECHO" 0)(command "COLOR" 4)(command "PLINE" P1 P2 "")))
   (setq P11 (list (car P1) (- (cadr P1) 50)));offset側
   (setq P12 (list (+ (car P1) 50) (cadr P1)));YUD1の挿入基点
   (command "OFFSET" BW "L" P11 "")
(princ "\n作図完了")

;;形式１の選択
   (setq PU  (list (+ (car P1) 50)(- (cadr P1) 25)))
   (setq PUL (list (- (car PU) 20)(- (cadr PU) 30)))
   (setq PUR (list (+ (car PU) 20)(- (cadr PU) 30)))
   (setq PU2 (list (+ (car P1) 50)(- (cadr P1) (- (/ BW 2) 50))))
   (setq PD  (list (+ (car P1) 50)(- (cadr P1) (- BW 25))))
   (setq PDL (list (- (car PD) 20)(+ (cadr PD) 30)))
   (setq PDR (list (+ (car PD) 20)(+ (cadr PD) 30)))
   (setq PD2 (list (car PU2)(- (cadr PU2) 100)))
   (setq PZ  (list (+ (car P1) 50)(- (cadr P1) (/ BW 2))))
   (command "PLINE" PU PU2 "")(command "PLINE" PUL PU PUR "")
   (command "PLINE" PD PD2 "")(command "PLINE" PDL PD PDR "")
       (INITGET 1 "1 2 3 4 5")
        (if (= (getvar "USERI5") 0)(setq K1 (GETKWORD "\n回路電圧を選択：1/AC100 2/AC200 3/AC24 4/DC24 5/DC100")))
        (if (= (getvar "USERI5") 1)(setq K1 ACK1))
	(cond ((= K1 "1")(setq K1 "AC100V")(setq RK "01R1")(setq SK "01N1"))
	      ((= K1 "2")(setq K1 "AC200V")(setq RK "01R2")(setq SK "01S2"))
	      ((= K1 "3")(setq K1 "AC24V") (setq RK "R24") (setq SK "S24"))
	      ((= K1 "4")(setq K1 "DC24V") (setq RK "P24") (setq SK "N24"))
	      ((= K1 "5")(setq K1 "DC100V")(setq RK "P100")(setq SK "N100"))
    )
        (command "COLOR" 2)(command "INSERT" "YUDN" PZ "1" "1" "0" K1)
        (setq PC1 (list (car P1) (+ (cadr P1) 5)))
        (setq PC2 (list (car P1) (- (- (cadr P1) BW) 25)))
        (command "TEXT" "S" "M0" PC1 "20" "0" RK)
        (command "TEXT" "S" "M0" PC2 "20" "0" SK)(command "COLOR" 4)
;縦線の描画：SLI自動
    (setq LW (- (- (car P2) (car P1)) 300))
    (setq NN (/ (fix LW) 300));(princ "\nNN = ")(princ NN)
    (setq POS1 (list (car P1)(cadr P1)))
    (setvar "USERS5" "A") ;SLIを自動モードにする。
    (while (>= NN 0)
       (setq POS1 (list (+ (car POS1) 300)(cadr POS1)))
       (setq POS2 (list (car POS1)(- (cadr POS1) BW)))
       (C:SLI)
       (setq NN (1- NN))
    )
    (setvar "USERS5" "M") ;SLIを手動モードに戻す。
    (setvar "USERI5" 0) ;Access自動作図のフラグ
    (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)(redraw)
    (prompt "\n属性の編集で電圧を変更出来ます。")
)
(princ)
(prompt "\n制御回路.LSP....OK")
