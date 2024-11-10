module EnvparseExtras.Parsers where

import Attoparsec.Time.Text qualified as Attoparsec
import Data.Attoparsec.Text qualified as Attoparsec
import Data.List qualified as List
import Data.Time (DiffTime)
import Env qualified as Envparse
import Relude
import TextBuilderDev qualified

enum ::
  (Eq a) =>
  -- | Name.
  Text ->
  -- | Help.
  Text ->
  -- | Default.
  Maybe a ->
  -- | Mappings.
  [(Text, a)] ->
  Envparse.Parser Envparse.Error a
enum name help def mappings =
  Envparse.var (Envparse.eitherReader reader) (toString name) mods
  where
    reader =
      maybe (Left errorMessage) Right . flip List.lookup mappings . fromString
      where
        errorMessage =
          mappings
            & fmap fst
            & fmap TextBuilderDev.text
            & TextBuilderDev.intercalate ", "
            & mappend ("Is not one of the following: ")
            & TextBuilderDev.fromTextBuilder

    mods =
      (mconcat . catMaybes)
        [ Just (Envparse.help (toString help)),
          Envparse.def <$> def,
          Just (Envparse.helpDef printer)
        ]
      where
        printer value =
          case matches of
            text : _ -> toString text
            _ -> "No mapping found"
          where
            matches = do
              (text, mappingValue) <- mappings
              if mappingValue == value
                then pure text
                else empty

text ::
  -- | Name.
  Text ->
  -- | Help.
  Text ->
  -- | Default.
  Maybe Text ->
  Envparse.Parser Envparse.Error Text
text name help def =
  Envparse.var Envparse.str (toString name) mods
  where
    mods =
      (mconcat . catMaybes)
        [ Just (Envparse.help (toString help)),
          Envparse.def <$> def,
          Just (Envparse.helpDef toString)
        ]

diffTime ::
  -- | Name.
  Text ->
  -- | Help.
  Text ->
  -- | Default.
  Maybe DiffTime ->
  Envparse.Parser Envparse.Error DiffTime
diffTime name help def =
  Envparse.var (Envparse.eitherReader reader) (toString name) mods
  where
    reader =
      Attoparsec.parseOnly parser . fromString
      where
        parser =
          Attoparsec.diffTime <* Attoparsec.endOfInput
    mods =
      (mconcat . catMaybes)
        [ Just (Envparse.help (toString help)),
          Envparse.def <$> def,
          Just (Envparse.helpDef (TextBuilderDev.fromTextBuilder . TextBuilderDev.diffTimeCompact))
        ]

port ::
  -- | Name.
  Text ->
  -- | Help.
  Text ->
  -- | Default.
  Maybe Word16 ->
  Envparse.Parser Envparse.Error Word16
port name help def =
  Envparse.var (Envparse.eitherReader reader) (toString name) mods
  where
    reader =
      Attoparsec.parseOnly parser . fromString
      where
        parser =
          Attoparsec.decimal
    mods =
      (mconcat . catMaybes)
        [ Just (Envparse.help (toString help)),
          Envparse.def <$> def,
          Just (Envparse.helpDef show)
        ]
