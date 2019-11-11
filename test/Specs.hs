import Chapter8.Spec as Chapter8
import Chapter8rr.Spec as Chapter8rr
import Chapter9.Spec as Chapter9
import Chapter10.Spec as Chapter10

main :: IO ()
main = do
    Chapter8.spec
    Chapter8rr.spec
    Chapter9.spec
    Chapter10.spec

      