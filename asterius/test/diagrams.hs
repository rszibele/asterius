import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    ["--input-hs", "test/diagrams/Hilbert.hs", "--browser", "--verbose-err", "--binaryen"] <> args
