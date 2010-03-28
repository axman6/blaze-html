{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
-- |BlazeHTML is a library to produce HTML. It is faster than the @html@ package, it guarantees at compile time that the HTML is syntactically valid, and gives composites of elements and attributes and content the same semantics as primitive elements.  This module contains the general documentation, and the combinators for generating HTML.
--
-- Before beginning a systematic exposition of BlazeHtml, we provide enough rote material to experiment while following the rest of the documentation.
--
-- The simplest value in HTML is a plain string, such as @Hello, world!@, with no tags.  In BlazeHtml it is denoted
--
-- > text "Hello, world!"
--
-- /Remark./ 'text' escapes the characters in its argument.  & becomes @&amp;@, \< becomes @&lt;@, etc.  If you don't wish BlazeHtml to escape your text, use 'unescapedText' instead. /End Remark./
--
-- We postpone the details of rendering HTML, but to experiment with the library as you go use the 'putHtml' function, which writes any HTML generated by BlazeHtml to standard output.  For example, in GHCi,
--
-- > > :m +Text.BlazeHtml.Html
-- > > putHtml $ text "Hello, world!"
-- > Hello, world!
--
-- To use BlazeHtml in a module, import
--
-- > import BlazeHtml.Html
--
-- 'BlazeHtml.Html' includes functions like 'div', 'head', and 'span' which conflict with other functions.  You should adjust what functions you hide from 'BlazeHtml.Html' and 'Prelude' to your particular needs.
--
-- So much for preliminaries.  Let us turn to generating HTML.
--
-- HTML is a markup language.  It wraps chunks of text in a hierarchy of elements.  Aside from element attributes, which we will examine in the next section, encoding 
--
-- > <p>This is a <em>very</em> important paragraph.</p>
--
-- covers all of generating HTML.  Drawn as a hierarchy, it is
--
-- > p --- "This is a "
-- >    |- em - "very"
-- >    |- " important paragraph."
--
-- We need three functions
--
-- > text :: Html h => Text -> h
-- > p :: Html h => h -> h
-- > em :: Html h => h -> h
--
-- /Remark/. Why is 'Html' a typeclass?  One of the design criteria for BlazeHtml was speed, but what makes code fast depends on how it is used.  Making 'Html' a fixed type would restrict us to optimization for one set of conditions.  As a typeclass, we can specialize BlazeHtml for specific scenarios, but all HTML generation remains generic. /End Remark/
--
-- 'text' we saw already.  It makes chunks of text into HTML values.
--
-- 'p' and 'em', and every other element, have type 'Html h => h -> h'.  They take an 'Html' value and wraps it in an element.  So
--
-- > > putHtml $ em $ text "Hello, world!"
-- > <em>Hello, world!</em>
--
-- In our example HTML, 'em' contains only one value, which fits its type 'Html h => h -> h'.  'p' has the same type, but takes three values.  How does this work?
--
-- This situation occurs often enough to Haskell to have a definitive solution.  Instead of writing separate functions that do the same thing, but on inputs @a@ and @[a]@, we make the type into a monoid.  A monoid is any type with a sensible values corresponding to zero (denoted 'mempty'), and a way of \"adding\" values (denoted 'mappend').  The numbers 0, 1, 2, ... are the classical example of a monoid.  Lists are a monoid with @[]@ playing the role of zero and concatenation as addition.
--
-- 'Html' is a monoid with an empty chunk of text as zero and concatenation as addition.  We can pass the three chunks to 'p' by first adding them,
--
-- > p $ text "This is a " <> (em $ text "very") <> text " important paragraph."
-- 
-- but keeping track of the spaces at boundaries between chunks is annoying.  Instead use the '(|.|)' combinator to append 'Html' values with space between.  Then the example becomes
--
-- > p $ text "This is a" |.| (em $ text "very") |.| text "important paragraph."
--
-- or a longer example which gives a feel for really using this notation,
--
-- > p $ text "World of Warcraft™ and Blizzard Entertainment™ are trademarks or \
-- >           \registered trademarks of Blizzard Entertainment, Inc. in the U.S.\
-- >           \and/or other countries.  The original materials contained in the" |-|
-- >           (em $ text "API Reference") |-| text "section of this website are \
-- >           \copyright John Wiley & Sons.  Other copyrights on this page are \
-- >           \owned by their respective owners.  All other content © 2008-2009" |-| 
-- >           (a "/about" $ text "wowprogramming.com") |-| text "."
--
-- '(<>)' and '(|.|)' are convenient for small pieces, but are awkward for nesting large elements.  BlazeHtml defines 'Html' to be a monad so you can concatenate elements with Haskell's do notation.
--
-- > body $ do div $ text "First div..."
-- >           div $ text "Second div..."
-- >           div $ text "Third div..."
--
--  It should be obvious 
--
-- The semantics of elements were chosen to permit defining composite elements that behave exactly like the primitive ones, as in
--
-- > remark :: Html h -> h -> h
-- > remark t = p $ (strong $ text "Remark.") |.| t |.| (strong $ text " End Remark.")
--
-- then in GHCi,
--
-- > > putHtml $ remark $ text "This is a remark."
-- > <p><strong>Remark.</strong> This is a remark. <strong>End Remark.</strong></p>@
--
-- /Remark/. All of HTML's elements retain their name in BlazeHtml.  @\<strong>\<\/strong>@ is 'strong'; @\<div>\<\/div>@ is 'div'; etc.  Most have type 'Html h => h -> h', but not all.  'img', for example, has type @Html h => String -> String -> h -> h@ to set the @src@ and @alt@ attributes.  You will profit from looking over the types of all the standard elements. /End Remark/
--
-- With 'text', the element functions, @do@ notation, '(<>)' and '(|.|)', you can construct the hierarchy of any HTML document.  We must now add attributes to element.
--
-- @p :: Html h => h -> h@ has no attributes.  A paragraph element with a set of attributes has the same time.  How do we turn one into the other?
--
-- First, all attributes in BlazeHtml have type 'Attribute', a synonym for @(Text,Text)@.  The first field is the attribute's key, the second it's value.  The library provides a combinator
--
-- > (!) :: Html h => (h -> h) -> Attribute -> h -> h
--
-- which adds an attribute to an element, as in
--
-- > > putHtml $ p <! ("id","myparagraph") "Hello, world!"
-- > <p id="myparagraph">Hello, world!</p>
--
-- Very often you will want to set a list of attributes.  BlazeHtml provides a combinator '(!:)' that differs from '(!)' only in that it takes a list of attributes instead of a single attribute.  For example,
--
-- > > putHtml $ p !: [("id","myparagraph"), ("style","color: green")] text "Hello, world!"
-- > <p id="myparagraph" style="color: green">Hello, world!</p>
--
-- /Warning./ Attribute values added with '(!)' are added verbatim.  There is no escaping.  To prevent security flaws, you should never, ever manually set attributes with dynamic data.  BlazeHtml provides attribute combinators like 'hrefA' and 'charsetA' (always the attribute name in lowercase, followed by @A@) which take care of escaping data.  Use them instead. /End Warning./
--
-- '(!)' is overloaded to operate on functions @Html h => h -> h@ and directly on values of type @Html h => h@.
--
-- /Warning./ If you add the same attribute twice to the same element, it will appear twice in that element.  BlazeHtml does no checking of uniqueness or merging of elements, since the correct behavior is not obvious.  Should an @id@ attribute be overridden?  Should multiple @class@ attributes be merged?  It is the user's responsibility to ensure their attribute lists are valid. /End Warning./
--
-- '(!)' has several corner cases you should be aware of.
--
-- First, 'Html' is a monoid, so its values may be a sequence of elements.  '(!)' adds attributes to each top level element of such compound values.  That is,
--
-- > > putHtml $ ul $ (li $ text "First item.") `mappend` (li $ text "Second item.") ! ("class","horatio")
-- > <ul><li class="horatio">First item.</li><li class="horatio">Second item.</li></ul>
--
-- Second, the value of @text "Hello, world!"@ is a valid argument for '(!)', but what does it mean to add attributes to a value with no elements?  In BlazeHtml, it does nothing.  The attributes are discarded and the text is unchanged.  Formally, it satisfies @text x ! a == text x@ for all @x :: Text@ and @a :: Attribute@ or @a :: [Attribute]@.
--
-- 'putHtml' let us explore HTML generation.  Now we must use the generated HTML.  To render HTML, we must have an instance of the 'Html' typeclass.  BlazeHtml provides several such renderers for different purposes.
--
-- Each renderer provides a function @render@/ModuleName/ ('renderHtmlText' in 'HtmlText', 'renderHtmlPretty' in 'HtmlPretty', etc.) which takes a value of type 'Html h => h'.  The exact type of the rendering function depends on the renderer since they need different parameters.  The current renderers in BlazeHtml are
--
--   ['HtmlIO'] performs an IO action on the resulting HTML text as it is built.
--
--   ['HtmlText'] produces the HTML as a 'Text' value.
--
--   ['HtmlPrettyText'] is identical to 'HtmlText', but puts newlines and indentation in the stream to make it easier to read.
--
-- See the description of the 'Html' typeclass for the details of implementing an instance of it.
--
-- The BlazeHtml library's modules fall in three categories.  'Text.BlazeHtml.Html', 'Text.BlazeHtml.XHtml', and 'Text.BlazeHtml.Xml' are the libraries users import.  They bear all the combinators and elements for their respective formats.  The renderers are all found in @Text.BlazeHtml.Render@, for example 'Text.BlazeHtml.Render.HtmlIO' and 'Text.BlazeHtml.Render.HtmlText'.  The modules under @Text.BlazeHtml.Internal@ contain the guts of the library.  'Text.BlazeHtml.Internal.Html' contains the 'Html' typeclass, and 'Text.BlazeHtml.Internal.Escaping' implements string escaping for the library.
--
-- Finally, here are some examples of pages encoded in BlazeHtml:
--
-- > html $ head $ do link "stylesheet" "default.css"
-- >                  title $ text "Hello!"
-- >        body $ do h1 $ text "Hello!"
-- >                , p $ text "Welcome to BlazeHtml."
--


module Text.BlazeHtml.Html
    ( -- * Basic combinators
      module Text.BlazeHtml.Internal.Html
    , -- * Do notation.
      module Text.BlazeHtml.Internal.HtmlMonad
      -- * Additional combinators.
    , (!)
    , (<>)
    , (<->)
    , text
    -- * Text chunks
    , emptyText
    -- * Elements
    , a
    , abbr
    , acronym
    , address
    , applet
    , area
    , b
    , base
    , basefont
    , bdo
    , big
    , blockquote
    , body
    , br
    , button
    , caption
    , center
    , cite
    , code
    , col
    , colgroup
    , dd
    , del
    , dfn
    , dir
    , div
    , dl
    , dt
    , em
    , fieldset
    , font
    , form
    , frame
    , frameset
    , h1
    , h2
    , h3
    , h4
    , h5
    , h6
    , head
    , hr
    , html
    , i
    , iframe
    , img
    , input
    , ins
    , isindex
    , kbd
    , label
    , legend
    , li
    , link
    , map
    , menu
    , meta
    , noframes
    , noscript
    , object
    , ol
    , optgroup
    , option
    , p
    , param
    , pre
    , q
    , s
    , samp
    , script
    , select
    , small
    , span
    , strike
    , strong
    , style
    , sub
    , sup
    , table
    , tbody
    , td
    , textarea
    , tfoot
    , th
    , thead
    , title
    , tr
    , tt
    , u
    , ul
    , var
    ) where

import Prelude hiding (div, head, span, map)
import GHC.Exts (IsString(..))
import Data.Monoid

import Text.BlazeHtml.Text (Text)
import Text.BlazeHtml.Internal.Html 
    hiding (modifyAttributeModifier, clearAttributes)
import Text.BlazeHtml.Internal.HtmlMonad
import Text.BlazeHtml.Internal.Escaping

infixl 2 !

-- NOTE: Somehow it makes more sense to define this instance here, because the
-- proper escaping depends on the document format and cannot be decided once an
-- forall. To avoid the orphan instances we should consider the following two
-- options:
--
--   1. Each document format could define its own composition monad using a
--      newtype by wrappin the prototype in Internal.HtmlMonad.
--
--   2. Each document format could define its own composition Monad by copying
--      the monad code from HtmlMonad.
--
-- Simon: I currently don't know which solution is better. For now it seems as
-- if (2) would be simpler. Moreover, switching to the first solution is easily
-- possible afterwards.
--
-- Jasper: If we split encoding and escaping, I think we can move this back to
-- the HtmlMonad module, since escaping is the same for Html/XHtml/Xml.
instance (Html h) => IsString (HtmlMonad h a) where
    fromString = text . fromString

(!) :: (Html h, Attributable a) => h -> a -> h
(!) = flip addAttributable

-- | Concatenate two @Html h => h@ values with no space inbetween.
(<>) :: Monoid h => h -> h -> h
(<>) = mappend

-- | Concatenate two @Html h => h@ values with whitespace inbetween.
(<->) :: Html h => h -> h -> h
(<->) = separate

-- | 'emptyText' is an empty chunk of text with no tags.
emptyText :: (Html h) => h
emptyText = mempty

-- | Escaped text.
text :: Html h => Text -> h
text = unescapedText . escapeHtml

-- | Render an @a@ element.
a :: (Html h) => h -> h
a = nodeElement "a"

-- | Render a @abbr@ element
abbr :: (Html h) => h -> h
abbr = nodeElement "abbr"

-- | Render a @acronym@ element
acronym :: (Html h) => h -> h
acronym = nodeElement "acronym"

-- | Render a @address@ element
address :: (Html h) => h -> h
address = nodeElement "address"

-- | Render a @applet@ element
applet :: (Html h) => h -> h
applet = nodeElement "applet"

-- | Render a @b@ element
b :: (Html h) => h -> h
b = nodeElement "b"

-- | Render a @bdo@ element
bdo :: (Html h) => h -> h
bdo = nodeElement "bdo"

-- | Render a @big@ element
big :: (Html h) => h -> h
big = nodeElement "big"

-- | Render a @blockquote@ element
blockquote :: (Html h) => h -> h
blockquote = nodeElement "blockquote"

-- | Render a @body@ element
body :: (Html h) => h -> h
body = nodeElement "body"

-- | Render a @button@ element
button :: (Html h) => h -> h
button = nodeElement "button"

-- | Render a @caption@ element
caption :: (Html h) => h -> h
caption = nodeElement "caption"

-- | Render a @center@ element
center :: (Html h) => h -> h
center = nodeElement "center"

-- | Render a @cite@ element
cite :: (Html h) => h -> h
cite = nodeElement "cite"

-- | Render a @code@ element
code :: (Html h) => h -> h
code = nodeElement "code"

-- | Render a @colgroup@ element
colgroup :: (Html h) => h -> h
colgroup = nodeElement "colgroup"

-- | Render a @dd@ element
dd :: (Html h) => h -> h
dd = nodeElement "dd"

-- | Render a @del@ element
del :: (Html h) => h -> h
del = nodeElement "del"

-- | Render a @dfn@ element
dfn :: (Html h) => h -> h
dfn = nodeElement "dfn"

-- | Render a @dir@ element
dir :: (Html h) => h -> h
dir = nodeElement "dir"

-- | Render a @div@ element
div :: (Html h) => h -> h
div = nodeElement "div"

-- | Render a @dl@ element
dl :: (Html h) => h -> h
dl = nodeElement "dl"

-- | Render a @dt@ element
dt :: (Html h) => h -> h
dt = nodeElement "dt"

-- | Render a @em@ element
em :: (Html h) => h -> h
em = nodeElement "em"

-- | Render a @fieldset@ element
fieldset :: (Html h) => h -> h
fieldset = nodeElement "fieldset"

-- | Render a @font@ element
font :: (Html h) => h -> h
font = nodeElement "font"

-- | Render a @form@ element
form :: (Html h) => h -> h
form = nodeElement "form"

-- | Render a @frameset@ element
frameset :: (Html h) => h -> h
frameset = nodeElement "frameset"

-- | Render a @h1@ element
h1 :: (Html h) => h -> h
h1 = nodeElement "h1"

-- | Render a @h2@ element
h2 :: (Html h) => h -> h
h2 = nodeElement "h2"

-- | Render a @h3@ element
h3 :: (Html h) => h -> h
h3 = nodeElement "h3"

-- | Render a @h4@ element
h4 :: (Html h) => h -> h
h4 = nodeElement "h4"

-- | Render a @h5@ element
h5 :: (Html h) => h -> h
h5 = nodeElement "h5"

-- | Render a @h6@ element
h6 :: (Html h) => h -> h
h6 = nodeElement "h6"

-- | Render a @head@ element
head :: (Html h) => h -> h
head = nodeElement "head"

-- | Render a @html@ element
html :: (Html h) => h -> h
html = nodeElement "html"

-- | Render a @i@ element
i :: (Html h) => h -> h
i = nodeElement "i"

-- | Render a @iframe@ element
iframe :: (Html h) => h -> h
iframe = nodeElement "iframe"

-- | Render a @ins@ element
ins :: (Html h) => h -> h
ins = nodeElement "ins"

-- | Render a @kbd@ element
kbd :: (Html h) => h -> h
kbd = nodeElement "kbd"

-- | Render a @label@ element
label :: (Html h) => h -> h
label = nodeElement "label"

-- | Render a @legend@ element
legend :: (Html h) => h -> h
legend = nodeElement "legend"

-- | Render a @li@ element
li :: (Html h) => h -> h
li = nodeElement "li"

-- | Render a @map@ element
map :: (Html h) => h -> h
map = nodeElement "map"

-- | Render a @menu@ element
menu :: (Html h) => h -> h
menu = nodeElement "menu"

-- | Render a @noframes@ element
noframes :: (Html h) => h -> h
noframes = nodeElement "noframes"

-- | Render a @noscript@ element
noscript :: (Html h) => h -> h
noscript = nodeElement "noscript"

-- | Render a @object@ element
object :: (Html h) => h -> h
object = nodeElement "object"

-- | Render a @ol@ element
ol :: (Html h) => h -> h
ol = nodeElement "ol"

-- | Render a @optgroup@ element
optgroup :: (Html h) => h -> h
optgroup = nodeElement "optgroup"

-- | Render a @option@ element
option :: (Html h) => h -> h
option = nodeElement "option"

-- | Render a @p@ element
p :: (Html h) => h -> h
p = nodeElement "p"

-- | Render a @pre@ element with the given literal escaped text.
--
-- Carriage returns are added after the begin tag <pre> and before the end tag
-- </pre> in order to allow for correct indenting semantics with 'Indent'.
pre :: (Html h) => Text -> h
pre t = unescapedText "<pre>\n" `mappend` text t `mappend` unescapedText "\n</pre>"

-- | Render a @q@ element
q :: (Html h) => h -> h
q = nodeElement "q"

-- | Render a @s@ element
s :: (Html h) => h -> h
s = nodeElement "s"

-- | Render a @samp@ element
samp :: (Html h) => h -> h
samp = nodeElement "samp"

-- | Render a @script@ element
script :: (Html h) => h -> h
script = nodeElement "script"

-- | Render a @select@ element
select :: (Html h) => h -> h
select = nodeElement "select"

-- | Render a @small@ element
small :: (Html h) => h -> h
small = nodeElement "small"

-- | Render a @span@ element
span :: (Html h) => h -> h
span = nodeElement "span"

-- | Render a @strike@ element
strike :: (Html h) => h -> h
strike = nodeElement "strike"

-- | Render a @strong@ element
strong :: (Html h) => h -> h
strong = nodeElement "strong"

-- | Render a @style@ element
style :: (Html h) => h -> h
style = nodeElement "style"

-- | Render a @sub@ element
sub :: (Html h) => h -> h
sub = nodeElement "sub"

-- | Render a @sup@ element
sup :: (Html h) => h -> h
sup = nodeElement "sup"

-- | Render a @table@ element
table :: (Html h) => h -> h
table = nodeElement "table"

-- | Render a @tbody@ element
tbody :: (Html h) => h -> h
tbody = nodeElement "tbody"

-- | Render a @td@ element
td :: (Html h) => h -> h
td = nodeElement "td"

-- | Render a @textarea@ element
textarea :: (Html h) => h -> h
textarea = nodeElement "textarea"

-- | Render a @tfoot@ element
tfoot :: (Html h) => h -> h
tfoot = nodeElement "tfoot"

-- | Render a @th@ element
th :: (Html h) => h -> h
th = nodeElement "th"

-- | Render a @thead@ element
thead :: (Html h) => h -> h
thead = nodeElement "thead"

-- | Render a @title@ element
title :: (Html h) => h -> h
title = nodeElement "title"

-- | Render a @tr@ element
tr :: (Html h) => h -> h
tr = nodeElement "tr"

-- | Render a @tt@ element
tt :: (Html h) => h -> h
tt = nodeElement "tt"

-- | Render a @u@ element
u :: (Html h) => h -> h
u = nodeElement "u"

-- | Render a @ul@ element
ul :: (Html h) => h -> h
ul = nodeElement "ul"

-- | Render a @var@ element
var :: (Html h) => h -> h
var = nodeElement "var"

-- | Render a @area@ leaf element.
area :: (Html h) => h
area = leafElement "area"

-- | Render a @base@ leaf element.
base :: (Html h) => h
base = leafElement "base"

-- | Render a @basefont@ leaf element.
basefont :: (Html h) => h
basefont = leafElement "basefont"

-- | Render a @br@ leaf element.
br :: (Html h) => h
br = leafElement "br"

-- | Render a @col@ leaf element.
col :: (Html h) => h
col = leafElement "col"

-- | Render a @frame@ leaf element.
frame :: (Html h) => h
frame = leafElement "frame"

-- | Render a @hr@ leaf element.
hr :: (Html h) => h
hr = leafElement "hr"

-- | Render a @img@ leaf element.
img :: (Html h) => h
img = leafElement "img"

-- | Render a @input@ leaf element.
input :: (Html h) => h
input = leafElement "input"

-- | Render a @isindex@ leaf element.
isindex :: (Html h) => h
isindex = leafElement "isindex"

-- | Render a @link@ leaf element.
link :: (Html h) => h
link = leafElement "link"

-- | Render a @meta@ leaf element.
meta :: (Html h) => h
meta = leafElement "meta"

-- | Render a @param@ leaf element.
param :: (Html h) => h
param = leafElement "param"
