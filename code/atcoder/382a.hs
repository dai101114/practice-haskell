{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

main :: IO ()
main = do
    [n, d] <- map read . words <$> getLine
    s <- getLine
    let result = min (d + T.count "." (T.pack s)) n
    print result

-- メモ
-- {-# LANGUAGE OverloadedStrings #-}はリテラルの型をStringからTextに変更する
-- 動的に設定したStringはpack関数でTextに変換する
-- qualifiedはモジュールの名前空間を汚染せず、名前の競合を防ぐために使われる便利な仕組み
-- putStrLnは引数はString型である必要がある
