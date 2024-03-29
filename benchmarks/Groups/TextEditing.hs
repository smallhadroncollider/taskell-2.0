module Groups.TextEditing
    ( group
    ) where

import RIO

import Criterion.Main (Benchmark, bench, bgroup, whnf)

import qualified Taskell.UI.Text.Editor as E
import qualified Taskell.UI.Text.Split as S

import Taskell.Data.Taskell (Taskell)

group :: Taskell -> IO Benchmark
group _ =
    pure $
    bgroup
        "TextEditing"
        [ bgroup
              "Editor"
              [ bench "up short" $ whnf (E.up <=< E.create 5) "Hello Today Fish"
              , bench "up long" $ whnf (E.up <=< E.create 30) longText
              , bench "down short" $ whnf (E.down <=< E.create 5) "Hello Today Fish"
              , bench "down long" $ whnf (E.down <=< E.create 30) longText
              , bench "begin short" $ whnf (E.begin <=< E.create 5) "Hello Today Fish"
              , bench "begin long" $ whnf (E.begin <=< E.create 30) longText
              , bench "end short" $ whnf (E.end <=< E.create 5) "Hello Today Fish"
              , bench "end long" $ whnf (E.end <=< E.create 30) longText
              , bench "endOfLine short" $ whnf (E.endOfLine <=< E.create 5) "Hello Today Fish"
              , bench "endOfLine long" $ whnf (E.endOfLine <=< E.create 30) longText
              , bench "top short" $ whnf (E.top <=< E.create 5) "Hello Today Fish"
              , bench "top long" $ whnf (E.top <=< E.create 30) longText
              , bench "bottom short" $ whnf (E.bottom <=< E.create 5) "Hello Today Fish"
              , bench "bottom long" $ whnf (E.bottom <=< E.create 30) longText
              , bench "insert short" $ whnf (E.insert "e" <=< E.create 5) "Hello Today Fish"
              , bench "insert long" $ whnf (E.insert "e" <=< E.create 30) longText
              , bench "backspace short" $ whnf (E.backspace <=< E.create 5) "Hello Today Fish"
              , bench "backspace long" $ whnf (E.backspace <=< E.create 30) longText
              ]
        , bgroup
              "Wrapping"
              [ bench "simple" $ whnf (S.withWidth 20) "hello"
              , bench "longer" $
                whnf (S.withWidth 20) "hello how are you today? My name is blah blah"
              , bench "long" $ whnf (S.withWidth 20) longText
              ]
        ]

longText :: Text
longText =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Hendrerit gravida rutrum quisque non tellus. Ut tellus elementum sagittis vitae et leo. Quisque egestas diam in arcu. Cras sed felis eget velit aliquet sagittis. Turpis massa tincidunt dui ut ornare. Ac tortor vitae purus faucibus ornare suspendisse sed nisi. Neque convallis a cras semper. Tempor orci eu lobortis elementum nibh tellus molestie. Amet tellus cras adipiscing enim eu turpis egestas pretium aenean.\nTincidunt eget nullam non nisi est sit amet facilisis magna. Euismod elementum nisi quis eleifend quam. Tortor at risus viverra adipiscing at in tellus integer. Tincidunt vitae semper quis lectus nulla at. Sed odio morbi quis commodo. Duis tristique sollicitudin nibh sit amet commodo nulla. Pharetra magna ac placerat vestibulum. Sagittis purus sit amet volutpat. Cursus risus at ultrices mi tempus imperdiet nulla. A erat nam at lectus urna duis convallis convallis tellus. Urna et pharetra pharetra massa. Mattis vulputate enim nulla aliquet porttitor lacus. Amet mauris commodo quis imperdiet massa tincidunt nunc pulvinar. Nam libero justo laoreet sit amet cursus sit. Sit amet dictum sit amet justo donec. Egestas integer eget aliquet nibh praesent tristique magna sit amet.\nEu nisl nunc mi ipsum faucibus vitae aliquet nec. Amet cursus sit amet dictum. Nibh nisl condimentum id venenatis a condimentum. Commodo ullamcorper a lacus vestibulum sed arcu non odio euismod. Diam sollicitudin tempor id eu nisl. Imperdiet nulla malesuada pellentesque elit. Ullamcorper velit sed ullamcorper morbi tincidunt ornare. Diam phasellus vestibulum lorem sed risus ultricies tristique. Ante in nibh mauris cursus mattis molestie a iaculis. Turpis nunc eget lorem dolor. Convallis a cras semper auctor neque vitae tempus. Adipiscing commodo elit at imperdiet dui accumsan sit amet nulla. Lacinia at quis risus sed vulputate odio ut. Ipsum dolor sit amet consectetur. Amet nulla facilisi morbi tempus iaculis urna. Diam donec adipiscing tristique risus.\nConvallis aenean et tortor at risus viverra adipiscing at. Mattis enim ut tellus elementum sagittis vitae. Integer vitae justo eget magna fermentum. Adipiscing at in tellus integer feugiat scelerisque. Id venenatis a condimentum vitae. Pellentesque massa placerat duis ultricies. Augue eget arcu dictum varius. Habitant morbi tristique senectus et netus et. Amet est placerat in egestas erat. Enim nunc faucibus a pellentesque. Ornare quam viverra orci sagittis eu volutpat odio.\nFacilisis magna etiam tempor orci eu lobortis elementum. Pharetra et ultrices neque ornare aenean euismod elementum nisi quis. Ultrices tincidunt arcu non sodales. Duis ultricies lacus sed turpis tincidunt id aliquet risus feugiat. Consequat semper viverra nam libero justo. Faucibus scelerisque eleifend donec pretium vulputate. Dolor magna eget est lorem ipsum. Mauris nunc congue nisi vitae suscipit tellus. Elit ut aliquam purus sit. Vitae justo eget magna fermentum iaculis eu non diam. Ut tortor pretium viverra suspendisse potenti nullam ac tortor. Orci ac auctor augue mauris augue neque. In vitae turpis massa sed."
