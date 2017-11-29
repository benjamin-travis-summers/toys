{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}

import ClassyPrelude
import Text.Taggy.Lens as Taggy
import Control.Lens    as Lens

import qualified Data.Text.Lazy.IO    as TL
import qualified Data.Text.Lazy       as TL
import qualified Data.Text.IO         as T
import qualified Data.Text            as T
import qualified Data.Attoparsec.Text as A

import Data.Maybe  (fromMaybe)
import Debug.Trace (trace)

-- Turn WikiWords and [explicit] [links] into hrefs ----------------------------------------------------------

explicitLink ∷ A.Parser Text
explicitLink = do
    _        ← A.char '['
    wContent ← A.takeWhile1 (\c → c /= ']')
    _        ← A.char ']'
    pure wContent

wikiWord ∷ A.Parser Text
wikiWord = do
  let upper  = A.inClass "A-ZÖÜĞ"
  let lower  = A.inClass "a-zöüğ"
  let letter = A.inClass "a-zöüğA-ZÖÜĞ"

  w1Head    ← A.takeWhile1 upper
  w1Tail    ← A.takeWhile1 lower
  w2Head    ← A.takeWhile1 upper
  w2Tail    ← A.takeWhile1 lower
  moreWords ← A.takeWhile  letter

  pure (w1Head <> w1Tail <> w2Head <> w2Tail <> moreWords)

data LinkClass = RawText Text
               | WikiWord Text
               | ExplicitLink Text

-- TODO wikiWords must not be procended by lowercase letters.
classifyLinksP ∷ A.Parser [LinkClass]
classifyLinksP = many $ A.choice
  [ WikiWord      <$> wikiWord
  , ExplicitLink  <$> explicitLink
  , RawText       <$> A.takeWhile1 (not . A.inClass "A-ZÖÜĞ[")
  , RawText       <$> (T.singleton <$> A.anyChar)
  ]

classifyLinks ∷ Text → [LinkClass]
classifyLinks txt = A.parseOnly classifyLinksP txt & \case
  Right r  → r
  Left msg → error msg

nodeElt nm attrs subNodes = NodeElement (Element nm (mapFromList attrs) subNodes)

nSpan subNodes = nodeElt "span" [] subNodes

nHref link content = nodeElt "a" [("href", link)] [NodeContent content]

mkNode ∷ LinkClass → Node
mkNode = \case
  RawText txt       → NodeContent txt
  WikiWord word     → nHref (word <> ".html") word
  ExplicitLink link → nSpan [NodeContent "[", nHref (link <> ".html") link, NodeContent "]"]

hrefWikiWords ∷ Node → Node
hrefWikiWords = \case
  node@(NodeElement _) → node
  NodeContent t        → classifyLinks t & \case
    []                  → NodeContent ""
    [RawText plainText] → NodeContent plainText
    parts               → nSpan (map mkNode parts)

-- Lens Helpers for Working with HTML ------------------------------------------------------------------------

enter tag =
  Taggy.children . traverse . Taggy.element . named (only tag)

decend = \case
  [tag]    → enter tag
  tag:tags → enter tag . decend tags

withClass c =
  attributed (ix "class" . only c)

-- Use taggy-lens to update the title and contents of Google Keeps notes. ------------------------------------

keepTitle ∷ Lens.Traversal' Taggy.Element Taggy.Element
keepTitle = decend ["DOCTYPE", "html", "body", "div", "div"] . withClass "title"

keepContent ∷ Lens.Traversal' Taggy.Element Taggy.Element
keepContent = decend ["DOCTYPE", "html", "body", "div", "div"] . withClass "content"

allTextContent ∷ Fold Taggy.Node T.Text
allTextContent = to universe . traverse . content

gKeepTitle ∷ Lens.Fold TL.Text T.Text
gKeepTitle = html . Taggy.element . keepTitle . Taggy.children . traverse . allTextContent

gKeepContent ∷ Lens.Fold TL.Text T.Text
gKeepContent = html . Taggy.element . keepContent . Taggy.children . traverse . allTextContent

gKeepLinkTitle ∷ TL.Text → TL.Text
gKeepLinkTitle noteHtml =
  transformOn (html . Taggy.element . keepTitle . Taggy.children . traverse) hrefWikiWords noteHtml

gKeepLinkContent ∷ TL.Text → TL.Text
gKeepLinkContent noteHtml =
  transformOn (html . Taggy.element . keepContent . Taggy.children . traverse) hrefWikiWords noteHtml

--------------------------------------------------------------------------------------------------------------

main = do
  [infileT, outfileT] ← getArgs

  let infile  = T.unpack infileT
      outfile = T.unpack outfileT

  note ← TL.readFile infile

  TL.writeFile outfile $ gKeepLinkTitle $ gKeepLinkContent note
