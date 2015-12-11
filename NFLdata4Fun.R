require(data.table)
require(rpart)
require(partykit)
require(dplyr)

# https://github.com/veltman/nflplays より取得
nfl <- fread("data/nflplays.csv", data.table = FALSE)
nfl$RUNorPASS <- with(nfl, ifelse(PLAY_TYPE %like% "RUN.*", "RUN",
                                  ifelse(PLAY_TYPE %like% "PASS.*", "PASS",
                                         ifelse(PLAY_TYPE %like% "PUNT.*", "PUNT", "OTHER"))))
nfl$RUNorPASS <- factor(nfl$RUNorPASS, levels = c("RUN", "PASS", "PUNT", "OTHER"))
nfl$TOGO_LEVEL <- cut(nfl$YARDS_TO_FIRST, breaks = c( -Inf, 2, 5, 10, Inf),
                      labels = c("0-2", "2-5", "5-10", "10-"), right = FALSE)
nfl$REMAIN_SEC <- nfl$MIN * 60 + nfl$SEC
nfl$DIFF <- nfl$OFF_SCORE - nfl$DEF_SCORE
nfl <- subset(nfl, RUNorPASS != "OTHER" & DOWN >= 1 & DOWN <= 4)
nfl$RUNorPASS <- factor(nfl$RUNorPASS, levels = c("RUN", "PASS", "PUNT"))


# ダウン，残りヤード数ごとのプレイ選択
ftable(nfl[c("DOWN", "RUNorPASS", "TOGO_LEVEL")])
ftable(nfl[c("TOGO_LEVEL", "DOWN", "RUNorPASS")])

table(nfl[c("DOWN", "RUNorPASS")])

# ランかパスか予測したいのでロジスティック回帰でやってみる
nfl_RorP <- subset(nfl, RUNorPASS == "RUN" | RUNorPASS == "PASS")
nfl_glm <- glm(RUNorPASS ~ DOWN + YARDS_TO_FIRST, family=binomial(link="logit"), data = nfl_RorP)
nfl_glm
# 1/(1+exp(-(-2.09 +0.73 * DOWN + 0.12 * YARDS_TO_FIRST)))
printRunOrPass <- function(down, yards_to_first) {
  p <- 1/(1+exp(-(-2.09 +0.73 * down + 0.12 * yards_to_first))) * 100
  sprintf("第%iダウン残り%iヤードのときは %4.1f％の確率でパス", down, yards_to_first, p)
}

printRunOrPass(1, 10) # ファーストダウン残り10ヤード  ほぼ半々
printRunOrPass(2, 2)  # セカンドダウン残り2ヤード　　 ややランが増える
printRunOrPass(3, 18) # サードダウン残り18ヤード　　　ほぼパス

# SF49ersでもやってみる
SF_RorP <- subset(nfl_RorP, OFF == "SF")
SF_glm <- glm(RUNorPASS ~ DOWN + YARDS_TO_FIRST, family=binomial(link="logit"), data = SF_RorP)
summary(sf_glm)
# 1/(1+exp(-(-1.55 +0.53 * DOWN + 0.07 * YARDS_TO_FIRST)))
# SF49ersの場合：3rdダウン残り18ヤード
1/(1+exp(-(-1.55+0.53 * 3 + 0.07 * 18))) # 意外に低い。無理しない？


# 決定木分析やってみる
# 参考) http://www.trifields.jp/decision-tree-classification-tree-1012

# nflデータ全体
nfl_RorP %>% 
  with(data.frame(RUNorPASS = as.factor(RUNorPASS),
                  DOWN = as.factor(DOWN),
                  TOGO_LEVEL,
                  YARDS_TO_FIRST,
                  QTR = as.factor(QTR),
                  DIFF)) %>%
  rpart(RUNorPASS ~ DOWN + YARDS_TO_FIRST + QTR + DIFF, data = ., method = "class") %>%
  as.party() %>%
  plot()

# サンフランシスコ49ers
SF_RorP %>% 
  with(data.frame(RUNorPASS = as.factor(RUNorPASS),
                  DOWN = as.factor(DOWN),
                  TOGO_LEVEL,
                  YARDS_TO_FIRST,
                  QTR = as.factor(QTR),
                  DIFF)) %>%
  rpart(RUNorPASS ~ DOWN + YARDS_TO_FIRST + QTR + DIFF, data = ., method = "class") %>%
  as.party() %>%
  plot()

# シアトルシーホークス
SEA_RorP <- subset(nfl_RorP, OFF == "SEA")
SEA_RorP %>% 
  with(data.frame(RUNorPASS = as.factor(RUNorPASS),
                  DOWN = as.factor(DOWN),
                  TOGO_LEVEL,
                  YARDS_TO_FIRST,
                  QTR = as.factor(QTR),
                  DIFF)) %>%
  rpart(RUNorPASS ~ DOWN + YARDS_TO_FIRST + QTR + DIFF, data = ., method = "class") %>%
  as.party() %>%
  plot()



