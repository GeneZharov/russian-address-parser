-- Строчные компоненты адреса
--
-- Не распознаю:
--
-- Смежные символьные компоненты, если их значения слиплись и не разделены 
-- запятой или ключами. Например: "г. Москва 1-я Дубровская ул". Тут программно 
-- просто невозможно различить границу между значениями компонент, если не 
-- иметь словаря городов/улиц.


module Address.String (constant, standalone, prefix, postfix) where


import Text.Parsec
import Control.Applicative hiding (optional, (<|>), many)
import Data.Char (toLower)
import Control.Monad (when)
import Debug.Trace (trace)
import qualified Data.Set as Set

import Address.Utils
import Address.Types
import Address.Cities
import qualified Address.Number as N


constant :: Parsec String Bool Component
constant = do -- статичные легко узнаваемые компоненты
   watch "symbol constant"
   strings "мо"
      *> lookAhead sep
      *> return (Область "Московская")


-- Компонента по умолчанию, без ключа
standalone :: Parsec String Bool Component
standalone = do
   watch "symbol standalone"
   v <- value <* lookAhead sep
   let v' = toLower `map` v
   if v' `Set.member` russianCities
   then return (Город v')
   else return (Улица v') <* modifyState (const True)


prefix :: Parsec String Bool Component
prefix = do
   watch "symbol prefix"
   choice $ flip map keys $ \ (constr, key) ->
      let fullKey  = fst key *> skipMany1 space
          shortKey = snd key *> (char '.' *> skipMany space
                             <|> skipMany1 space)
      in do
          watch $ "symbol test " ++ show (constr "")
          result <- (try fullKey <|> try shortKey)
                 *> (constr . map toLower) `fmap` value
                 <* lookAhead sep
          when (isRoad result) (modifyState $ const True)
          return result


postfix :: Parsec String Bool Component
postfix = do
   watch "symbol postfix"
   value <- value <* skipMany1 space
   choice $ flip map keys $ \ (constr, key) ->
      let fullKey  = fst key <* lookAhead sep
          shortKey = snd key <* (char '.' <|> lookAhead sep)
      in do
          watch $ "symbol test " ++ show (constr "")
          try fullKey <|> try shortKey
          let result = constr (toLower `map` value)
          when (isRoad result) (modifyState $ const True)
          return result


sep :: Parsec String Bool Char
sep = space
  <|> char ','
  <|> char '.' -- бывают адреса с точкой-разделителем
  <|> eof *> return 'x'


value :: Parsec String Bool String
value = do
   watch "value"
   manyTill1 (alphaNum <|> oneOf " -.") $ lookAhead $
          try (many  space <* eof)
      <|> try (many  space <* char ',')
      <|> try (many1 space <* choice (symbolKey `map` keys))
      <|> try (many  space <* (try N.prefix <|> try N.postfix))
   where symbolKey (constr, key) = do
            watch $ "symbolKey " ++ show (constr "")
            try (fst key <* sep) <|> try (snd key <* (sep <|> char '.'))
         sep = space
           <|> char ','
           <|> eof *> return 'x'


keys = let null = many1 $ satisfy (const False)
       in [

            -- Несмотря на то, что по правилам русского языка после слов, 
            -- сокращённых через '-' не ставится точка, находятся дебилы, 
            -- которые её ставят, поэтому располагаю такие ключи во второй 
            -- группе.

            ( Область
            , ( strings "область"
              , strings "обл"
              )
            )

          , ( Город
            , ( strings "город"
              , try (strings "гор") <|> strings "г"
              )
            )

          , ( Посёлок
            , ( strings "пос" *> oneOf "ёе" *> strings "лок"
              , try (strings "пос") <|> strings "п"
              )
            )

          , ( Село
            , ( strings "село"
              , strings "с" -- сокращение 'с' конфликтует со строением
              )
            )

          , ( Деревня
            , ( strings "деревня"
              , null
              )
            )

          , ( Район
            , ( strings "район"
              , strings "р-н"
              )
            )

          , ( Микрорайон
            , ( strings "микрорайон"
              , try (strings "мкрн") <|> strings "мкр"
              )
            )

          , ( Улица
            , ( strings "улица"
              , strings "ул"
              )
            )

          , ( Шоссе
            , ( strings "шоссе"
              , strings "ш"
              )
            )

          , ( Переулок
            , ( strings "переулок"
              , strings "пер"
              )
            )

          , ( Бульвар
            , ( strings "бульвар"
              , strings "б-р"
              )
            )

          , ( Проспект
            , ( strings "проспект"
              , try (strings "пр-т") <|> strings "пр"
              )
            )

          , ( Набережная
            , ( strings "набережная"
              , strings "наб"
              )
            )

          , ( Проезд
            , ( strings "проезд"
              , strings "пр-д"
              )
            )

          , ( Спуск
            , ( strings "спуск"
              , null
              )
            )

          , ( Тупик
            , ( strings "тупик"
              , null
              )
            )

        ]
