module Script where

import Language.Exec
import Language.Expressions

main = do
   pwd
   ls []
   ls [Str "Language"]
   createDir [Str "Folder"]
   cd [Str "Folder"]
   create [Str "text.txt"]
   move [Str "text.txt", Str "novi.txt"]
   remove [Str "novi.txt"]
   cd [Str ".."]
   removeDir [Str "Folder"]
   cat [Str "Main.hs"]
   
   
