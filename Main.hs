module Main where

import Library
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "Library Management System"
  putStrLn ""
  putStrLn "Supported Commands: addBook, removeBook, addUser, removeUser, borrow, return, listBooks, listUsers, exit"
  processCommands library

processCommands :: Library -> IO ()
processCommands currentLibrary = do
  putStr ">"
  hFlush stdout
  cmd <- getLine

  case cmd of
    "addBook" -> do
      putStr "Please enter Title: "
      inputTitle <- getLine

      putStr "Please enter Author: "
      inputAuthor <- getLine

      let newLibrary = addBook inputTitle inputAuthor currentLibrary

      putStrLn "Book added !"
      processCommands newLibrary -- Looping to get next command
    "removeBook" -> do
      putStr "Please enter Book Id: "
      
      inputIDStr <- getLine
      
      let inputBookId = read inputIDStr :: Int
          (newLibrary, message) = removeBook inputBookId currentLibrary
      
      putStrLn message
      
      processCommands newLibrary -- Looping to get next command
    "addUser" -> do
      putStr "Please enter Name: "
      inputName <- getLine
  
      let newLibrary = addUser inputName currentLibrary
  
      putStrLn "User added !"
      processCommands newLibrary -- Looping to get next command
    "removeUser" -> do
      putStr "Please enter User Id: "

      inputIDStr <- getLine

      let inputUserId = read inputIDStr :: Int
          (newLibrary, message) = removeUser inputUserId currentLibrary
      
      putStrLn message
      
      processCommands newLibrary -- Looping to get next command
    "borrow" -> do
      putStr "Please enter Book Id: "
      inputBookIDStr <- getLine

      putStr "Please enter User Id: "
      inputUserIDStr <- getLine
      
      let inputBookId = read inputBookIDStr :: Int
          inputUserId = read inputUserIDStr :: Int
          (newLibrary, message) = borrowBook inputBookId inputUserId currentLibrary

      putStrLn message
      
      processCommands newLibrary -- Looping to get next command
    "return" -> do
      putStr "Please enter Book Id: "
      inputBookIDStr <- getLine
  
      putStr "Please enter User Id: "
      inputUserIDStr <- getLine
  
      let inputBookId = read inputBookIDStr :: Int
          inputUserId = read inputUserIDStr :: Int
          (newLibrary, message) = returnBook inputBookId inputUserId currentLibrary

      putStrLn message
      
      processCommands newLibrary -- Looping to get next command
    "listBooks" -> do
      availableBooks (books currentLibrary)
      borrowedBooks  (books currentLibrary)
      
      processCommands currentLibrary -- Looping to get next command
    "listUsers" -> do
      printUsers (users currentLibrary)
      
      processCommands currentLibrary -- Looping to get next command
    "exit" ->
      putStrLn "Thank you !"

    _ -> do
      putStrLn "Invalid command !"
      processCommands currentLibrary -- Looping to get next command
