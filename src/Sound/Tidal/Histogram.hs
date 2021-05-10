module Sound.Tidal.Histogram where

import Sound.Tidal.Ngrams
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import Graphics.Histogram

input = [1,0.2,0.23,0.15,0.1,0.88,0.89,0.33,0.05,0.33,0.45,0.99,0.01,0.01,0.5]

simple = do
    let hist = histogram binSturges input
    plot "simple.png" hist

advanced = do
    let hist = histogram binSqrt input
    let opts = Opts.title "I'm a histogram!" $
               Opts.yLabel "Why?" $
               Opts.xLabel "Because!" $
               defOpts hist
    plotAdv "advanced.eps" opts hist

histCalc st = do
                ngram <- ngramOut st
                return (ngram)
