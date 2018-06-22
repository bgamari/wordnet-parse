{-# LANGUAGE OverloadedStrings #-}

module NLP.WordNet where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Maybe
import Data.Word
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Map.Lazy as M
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe
import System.FilePath

-- | A WordNet database.
data WNDB = WNDB { dbs :: M.Map POS BS.ByteString }

-- | Parts of speech.
data POS = Adj | Adv | Noun | Verb
         deriving (Show, Eq, Ord, Enum, Bounded)

newtype Offset = Offset Int
               deriving (Show, Eq, Ord)

openWordNet :: FilePath -> IO WNDB
openWordNet dbPath = do
    dbs <- mapM openPosDb $ M.fromList [ (pos, pos)
                                       | pos <- [minBound..maxBound]
                                       ]
    return $ WNDB dbs
  where
    posExt Adj = "adj"
    posExt Adv = "adv"
    posExt Noun = "noun"
    posExt Verb = "verb"

    openPosDb :: POS -> IO BS.ByteString
    openPosDb pos = unsafeInterleaveIO $ do
        BS.readFile (dbPath </> "data" <.> posExt pos)

lookupOffsetPos :: WNDB -> POS -> Offset -> Synset
lookupOffsetPos wndb pos (Offset off)
  | BS.head bs /= '\n' = error "Invalid offset"
  | otherwise =
    case A.parseOnly synset (BS.tail bs) of
      Right ss -> ss
      Left err -> error $ "invalid synset "++show off++": "++err
  where
    bs = BS.drop (off-1) (dbs wndb M.! pos)


synset :: A.Parser Synset
synset = labelled "synset" $ do
    off <- Offset <$> A.decimal
    A.skipSpace
    lex_no <- A.decimal
    A.skipSpace
    synset_ty <- synsetType
    A.skipSpace
    word_cnt <- A.hexadecimal
    A.skipSpace

    -- Words
    words <- replicateM word_cnt $ labelled "word" $ do
        word <- A.takeWhile (not . A.isSpace)
        A.skipSpace
        lex_id <- A.hexadecimal
        A.skipSpace
        return $! SynsetWord word lex_id

    -- Pointers
    ptr_cnt <- A.decimal
    A.skipSpace
    ptrs <- replicateM ptr_cnt $ labelled "pointer" $ do
        pointer <- parsePointerType
        A.skipSpace
        synset_off <- Offset <$> A.decimal
        A.skipSpace
        pos <- synsetType
        A.skipSpace
        src_tgt <- A.hexadecimal
        A.skipSpace
        return $! Pointer pointer synset_off pos src_tgt

    -- Frames
    frames <- fmap (fromMaybe []) $ optional $ A.try $ labelled "frame" $ do
        n_frames <- A.decimal
        A.skipSpace
        replicateM n_frames $ do
            A.char '+'
            A.skipSpace
            frame_num <- A.decimal
            A.skipSpace
            w_num <- WordNum <$> A.hexadecimal
            A.skipSpace
            return $! Frame frame_num w_num

    -- Gloss
    glos <- labelled "gloss" $ do
        A.char '|'
        A.skipSpace
        A.takeWhile (/= '\n')

    return $! Synset off lex_no synset_ty words ptrs frames glos
  where
    labelled lbl = (A.<?> lbl)

    synsetType = labelled "POS" $ A.choice
        [ Adj  <$ A.char 'a'
        , Adv  <$ A.char 'r'
        , Adj  <$ A.char 's'
        , Noun <$ A.char 'n'
        , Verb <$ A.char 'v'
        ]

data Synset
    = Synset { ssOffset   :: !Offset
             , ssLexNum   :: !Int
             , ssPos      :: !POS
             , ssWords    :: [SynsetWord]
             , ssPointers :: [Pointer]
             , ssFrames   :: [Frame]
             , ssGloss    :: !BS.ByteString
             }
    deriving (Show, Eq, Ord)

data SynsetWord
    = SynsetWord { ssWord :: !BS.ByteString, ssLexId :: !Word16 }
    deriving (Show, Eq, Ord)

data Pointer
    = Pointer { pointerType   :: !PointerType
              , pointerOffset :: !Offset
              , pointerPOS    :: !POS
              , pointerSrcTgt :: !Word16
              }
    deriving (Show, Eq, Ord)

data Frame
    = Frame { frameNum :: Int
            , frameWordNum :: WordNum
            }
    deriving (Show, Eq, Ord)

newtype WordNum = WordNum Word8
                deriving (Show, Eq, Ord)

pointerTargetWordNum :: Pointer -> WordNum
pointerTargetWordNum =
    WordNum . fromIntegral . (.&. 0xff) . pointerSrcTgt

pointerSourceWordNum :: Pointer -> WordNum
pointerSourceWordNum =
    WordNum . fromIntegral . (`shiftR` 8) . pointerSrcTgt

data PointerType
    = Antonym
    | Hypernym
    | InstHypernym
    | Hyponym
    | InstHyponym
    | MemberHolonym
    | SubstHolonym
    | PartHolonym
    | MemberMeronym
    | SubstMeronym
    | PartMeronym
    | Attribute
    | DerivRelatedForm -- ^ derivationally related form
    | DomainOfSynset !TopicRegionUsage
    | MemberOfDomain !TopicRegionUsage

      -- verbs only
    | Entailment
    | Cause
    | AlsoSee
    | VerbGroup

      -- adjectives only
    | SimilarTo
    | ParticipleOfVerb
    | Pertainym
    deriving (Eq, Ord, Show)

data TopicRegionUsage = Topic | Region | Usage
                      deriving (Eq, Ord, Show, Bounded, Enum)

parsePointerType :: A.Parser PointerType
parsePointerType = A.choice
    [ Antonym <$ A.char '!'
    , A.char '@' >> A.choice
      [ InstHypernym <$ A.char 'i'
      , pure Hypernym
      ]
    , A.char '~' >> A.choice
      [ InstHyponym <$ A.char 'i'
      , pure Hyponym
      ]
    , A.char '#' >> A.choice
      [ MemberHolonym <$ A.char 'm'
      , SubstHolonym  <$ A.char 's'
      , PartHolonym   <$ A.char 'p'
      ]
    , A.char '%' >> A.choice
      [ MemberMeronym <$ A.char 'm'
      , SubstMeronym  <$ A.char 's'
      , PartMeronym   <$ A.char 'p'
      ]
    , Attribute <$ A.char '='
    , DerivRelatedForm <$ A.char '+'
    , A.char ';' >> A.choice
      [ DomainOfSynset Topic  <$ A.char 'c'
      , DomainOfSynset Region <$ A.char 'r'
      , DomainOfSynset Usage  <$ A.char 'u'
      ]
    , A.char '-' >> A.choice
      [ MemberOfDomain Topic  <$ A.char 'c'
      , MemberOfDomain Region <$ A.char 'r'
      , MemberOfDomain Usage  <$ A.char 'u'
      ]
    , Entailment <$ A.char '*'
    , Cause <$ A.char '>'
    , AlsoSee <$ A.char '^'
    , VerbGroup <$ A.char '$'
    , SimilarTo <$ A.char '&'
    , ParticipleOfVerb <$ A.char '<'
    , Pertainym <$ A.char '\\'
    ]

iterSynsets :: FilePath -> IO [Synset]
iterSynsets path = parse <$> BS.readFile path
  where
    parse = map (either error id . A.parseOnly synset)
          . dropWhile ("  " `BS.isPrefixOf`)
          . BS.lines
