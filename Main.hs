module Main where

import Library
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "Library Management System"
  putStrLn ""
  putStrLn "Supported Commands: addBook, listBooks, exit"

  processCommands library

processCommands :: Library -> IO ()
processCommands library = do
  putStr ">"

  hFlush stdout
  cmd <- getLine

  case cmd of
    "addBook" -> do
      putStr "Please enter Title: "
      inputTitle <- getLine

      putStr "Please enter Author: "
      inputAuthor <- getLine

      let newLibrary = addBook inputTitle inputAuthor library

      putStrLn "Book added !"

      processCommands newLibrary -- Looping to get next command
    "listBooks" -> do
      putStrLn "Available Books:"
      availableBooks (books library)

      putStrLn "Borrowed Books:"
      borrowedBooks (books library)

      processCommands library -- Looping to get next command
    "exit" -> putStrLn "Thank you !"
    _ -> do
      putStrLn "Invalid command !"
      processCommands library

