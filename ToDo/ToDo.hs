module ToDo.ToDo where

data ToDo = ToDo Int String
          | ToDoDate Int String String

instance Show ToDo where
    show (ToDo number name) = show(number) ++ ") " ++ name
    show (ToDoDate number name date) =
      show(number) ++ ") " ++ name ++ " " ++ date

number :: ToDo -> Int
number (ToDo number _) = number
number (ToDoDate number _ _) = number

name :: ToDo -> String
name (ToDo _ name) = name
name (ToDoDate _ name _) = name

date :: ToDo -> String
date (ToDoDate _ _ date) = date

