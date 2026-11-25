{-# OPTIONS_GHC -Wno-unused-matches -Wno-orphans #-}

-- | Обязательный уровень: обход структуры и небольшой язык парсеров.
module Level1 where

import Control.Applicative
import Lexer (Token)
import MetaUtils
import MyIO
import Parser


-- 1.1. Обходы и учебная консоль.
-- Реализуйте Traversable Point3 через Applicative: обход слева направо,
-- форма точки сохраняется. Не используйте deriving Traversable.

data Point3 a = Point3 a a a
  deriving (Show, Eq)

instance Functor Point3 where
  fmap f (Point3 x y z) = Point3 (f x) (f y) (f z)

instance Foldable Point3 where
  foldMap f (Point3 x y z) = f x <> f y <> f z

instance Applicative Point3 where
  pure x = Point3 x x x

  Point3 f g h <*> Point3 x y z = Point3 (f x) (g y) (h z)

instance Traversable Point3 where
  traverse f (Point3 x y z) =
    Point3 <$> f x <*> f y <*> f z

-- Через свой traverse и writeInt напишите printSum:
-- печать компонент первой точки, второй и их суммы, слева направо.
-- Функции *UnsafeMyIO используйте только для проверки.

printSum :: Point3 Int -> Point3 Int -> MyIO ()
printSum p q = do
  _ <- traverse writeInt p
  _ <- traverse writeInt q
  _ <- traverse writeInt ((+) <$> p <*> q)
  pure ()

-- 1.2. Прочитайте, прежде чем писать.
-- В Parser.hs даны тип MyParser, примитивы и пример parseAssignmentManual.
-- Запишите результаты значениями, без вызовов парсеров и других функций.
-- a) runMyParser parseAssignmentManual ["count", "=", "42", ";"]
-- b) runMyParser parseAssignmentManual ["count", "=", "oops", ";"]
-- c) Для стандартного Functor MyParser:
--    runMyParser (length <$> anyToken) ["hello", "!"]

answerAssignment :: Maybe ([Token], Assignment)
answerAssignment = Just ([";"], Assignment "count" 42)

answerInvalid :: Maybe ([Token], Assignment)
answerInvalid = Nothing

answerMapped :: Maybe ([Token], Int)
answerMapped = Just (["!"], 5)

-- 1.3. Три операции над парсерами.
-- Заполните по одному пропуску в каждом инстансе:
-- fmap преобразует результат, сохраняя остаток;
-- <*> запускает второй парсер на остатке после первого;
-- <|> при неудаче левой ветки запускает правую на исходном вводе.

instance Functor MyParser where
  fmap f (MyParser p) = MyParser \ts -> case p ts of
    Nothing -> Nothing
    Just (rest, x) -> Just (rest, f x)

instance Applicative MyParser where
  pure x = MyParser \ts -> Just (ts, x)

  MyParser pf <*> MyParser px = MyParser \ts -> case pf ts of
    Nothing -> Nothing
    Just (rest, f) -> case px rest of
      Nothing -> Nothing
      Just (rest', x) -> Just (rest', f x)

instance Alternative MyParser where
  empty = MyParser $ const Nothing

  MyParser p <|> MyParser q = MyParser \ts -> case p ts of
    Nothing -> q ts
    success -> success

-- Через fmap преобразуем числовой токен в натуральное число (включая 0).
natural :: MyParser Int
natural = read <$> satisfy decimal

-- 1.4. Файл настроек.
-- Через комбинаторы напишите parseAssignment для записи name = 42
-- и parseAssignments для списка таких записей, каждая завершается ';'.
-- Список может быть пустым; сохраните порядок и повторы, прочитайте весь ввод.
-- Сравните запись parseAssignment с данным parseAssignmentManual.
-- runMyParser parseAssignments (tokenize "count = 42; limit = 10;")
--   == Just ([], [Assignment "count" 42, Assignment "limit" 10])

parseAssignment :: MyParser Assignment
parseAssignment = Assignment <$> identifier <* token "=" <*> natural

parseAssignments :: MyParser [Assignment]
parseAssignments = many (parseAssignment <* token ";") <* eof
