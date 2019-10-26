import Chapter8.Spec as Chapter8
import Chapter8rr.Spec as Chapter8rr

main :: IO ()
main = do
    Chapter8.spec
    Chapter8rr.spec
      