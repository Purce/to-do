module Types where

import Data.Maybe

type Name = String
type Date = String

data ToDo = ToDo Int String
          | ToDoDate Name (Maybe Date)
          deriving (Eq, Show)

name :: ToDo -> Name
name (ToDo _ name) = name
name (ToDoDate name _) = name

date :: ToDo -> Maybe Date
date (ToDoDate _ date) = if isJust date
                         then date
                         else Nothing


