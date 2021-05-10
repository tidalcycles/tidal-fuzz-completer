import Sound.Tidal.Ngrams
import Sound.Tidal.Types
import Sound.Tidal.Tokeniser

import System.Environment

main = do
          args <- getEnvVars
          aha <- Sound.Tidal.Types.wWalk $ Sig [] $ Pattern Osc
          let tokenised = codeToToken aha
          putStrLn $ show tokenised


-- To do:
-- convert output (#) (speed sine) $ sound $ rev "bd sn"  >> ["(#)", ""]
-- run the main on receiving osc messagge..


sCode :: Code -> String
sCode = show


-- codeToToken :: Code -> [String]
codeToToken inp = head tokenised
                    where reformat = removePunc $ sCode $ inp
                          breakup = breakupCode "\n\n" reformat
                          tokenised = map (tokeniser) $ breakup
