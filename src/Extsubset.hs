module Extsubset where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Types
import Text.XML.HaXml.OneOfN


{-Type decls-}

newtype Multifile = Multifile [File] 		deriving (Eq,Show)
data File = File File_Attrs String
          deriving (Eq,Show)
data File_Attrs = File_Attrs
    { filePath :: String
    } deriving (Eq,Show)


{-Instance decls-}

instance HTypeable Multifile where
    toHType x = Defined "multifile" [] []
instance XmlContent Multifile where
    toContents (Multifile a) =
        [CElem (Elem (N "multifile") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["multifile"]
        ; interior e $ return (Multifile) `apply` many parseContents
        } `adjustErr` ("in <multifile>, "++)

instance HTypeable File where
    toHType x = Defined "file" [] []
instance XmlContent File where
    toContents (File as a) =
        [CElem (Elem (N "file") (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["file"]
        ; interior e $ return (File (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <file>, "++)
instance XmlAttributes File_Attrs where
    fromAttrs as =
        File_Attrs
          { filePath = definiteA fromAttrToStr "file" "path" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "path" (filePath v)
        ]



{-Done-}
