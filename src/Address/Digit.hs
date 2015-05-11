-- Цифровые компоненты адреса
--
-- Не распознаю:
-- • Номер дома без ключа, например: Волгоградский проспект, 0
-- • Слипшиеся ключ и значение:
--    1к1 — так выдаёт адреса карты яндекса и гугла
--    д1
--    1д — так не пишут, и это конфликтует с номером с буквой вроде 1А


module Address.Digit (standalone, prefix, postfix) where


import Text.Parsec
import Control.Applicative hiding (optional, (<|>))
import Data.Char (toLower)
import Control.Monad (when)

import Address.Utils
import Address.Types


-- Компонента по умолчанию, без ключа
standalone :: Parsec String Bool Component
standalone = do
    watch "digit standalone"
    roadParsed <- getState
    if roadParsed
    then fmap Дом number <* lookAhead sep
    else fail "Ещё не была распарсена дорога"


prefix :: Parsec String Bool Component
prefix = do
    watch "digit prefix"
    choice $ flip map keys $ \ (constr, key) ->
        let fullKey  = fst key *> skipMany1 space
            shortKey = snd key *> (char '.' *> skipMany space
                               <|> skipMany1 space)
        in do
            watch $ "digit test " ++ show (emptyNum constr)
            (try fullKey <|> try shortKey)
                *> fmap constr number
                <* lookAhead sep


postfix :: Parsec String Bool Component
postfix = do
    watch "digit postfix"
    value <- number <* skipMany1 space
    choice $ flip map keys $ \ (constr, key) ->
        let fullKey  = fst key <* lookAhead sep
            shortKey = snd key <* (char '.' <|> lookAhead sep)
        in do
            watch $ "digit test " ++ show (emptyNum constr)
            try fullKey <|> try shortKey
            return (constr value)


sep :: Parsec String Bool Char
sep = space
  <|> char ','
  <|> char '.' -- Бывают адреса с точкой-разделителем
  <|> eof *> return 'x'


emptyNum :: (HouseNum -> Component) -> Component
emptyNum constr = constr $ HouseNum (Part 0 Nothing) Nothing
    -- Загоняет в числовой конструктор пустой номер дома, чтобы его потом можно 
    -- было распечатать для отладки, чтобы понять какой конструр использовался.


number :: Parsec String Bool HouseNum
number = do
    -- Полный номер, например "1A/2B"
    -- Встречаются корпуса с буквой. По тем же соображениям, что и номера 
    -- домов, номера корпусов и строений также могут быть с буквой и через 
    -- слэш.
    watch "number"
    optional (char '№') *> (HouseNum <$> part <*> option Nothing suffix)
        where suffix = (char '/' <|> char '-') *> fmap Just part
           -- Разделитель дефисом — это костыль в здравпросвете,
           -- так как в имени файла не может быть слэша.


part :: Parsec String Bool Part
part = do
    -- Половина номера с левой или правой стороны от слэша
    -- Например "2-B" или "2B"
    watch "part"
    Part
        <$> fmap (read :: String -> Int) (many1 digit)
        <*> option Nothing
                (try $ optional (char '-') *> fmap (Just . toLower) letter)


keys = [

        ( Дом, (
            strings "дом",
            strings "д"
        ) ),

        ( Корпус, (
            strings "корпус",
            try (strings "корп") <|> try (strings "кор") <|> strings "к"
        ) ),

        ( Строение, (
            strings "строение",
            try (strings "стр") <|> strings "с"
        ) ),

        ( Владение, (
            strings "владение",
            strings "вл"
        ) )

    ]
