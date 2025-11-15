module Library where

data Status
  = Available
  | Borrowed
  deriving (Eq, Show)

data Book = Book
  { bookId :: Int,
    title :: String,
    author :: String,
    status :: Status
  }
  deriving (Show, Eq)

data User = User
  { userId :: Int,
    userName :: String
  }
  deriving (Show, Eq)

data Library = Library
  { books :: [Book],
    users :: [User],
    bookCount :: Int,
    userCount :: Int,
    borrowerName :: Maybe String
  }
  deriving (Show, Eq)

library :: Library
library =
  Library
    { books = [],
      users = [],
      bookCount = 0,
      userCount = 0
    }

-- methods

-- Add a new Book
addBook :: String -> String -> Library -> Library
addBook inputTitle inputAuthor inputLibrary =
  let netBookCount = bookCount inputLibrary + 1

      newBook =
        Book
          { bookId = netBookCount,
            title = inputTitle,
            author = inputAuthor,
            status = Available
          }
   in inputLibrary
        { books = books inputLibrary ++ [newBook],
          bookCount = netBookCount
        }

-- Print Available Books
availableBooks :: [Book] -> IO ()
availableBooks [] = putStrLn ""
availableBooks allBooks = do
  let availableBookList = [book | book <- allBooks, status book == Available]
  printBooks availableBookList

-- Print Available Books
borrowedBooks :: [Book] -> IO ()
borrowedBooks [] = putStrLn ""
borrowedBooks allBooks = do
  let borrowedBookList = [book | book <- allBooks, status book == Borrowed]
  printBooks borrowedBookList

-- Print Books
printBooks :: [Book] -> IO ()
printBooks [] = putStrLn ""
printBooks (book : books) = do
  putStrLn "-----------------------------------------"
  putStrLn ("Book Id     : " ++ show (bookId book))
  putStrLn ("Book Name   : " ++ show (title book))
  putStrLn ("Book Author : " ++ show (author book))
  putStrLn ("Book Status : " ++ show (status book))

  case borrower book of
    Nothing -> putStrLn ""
    Just borrowerName ->
      putStrLn ("Book Borrower: " ++ show (borrowerName))

  putStrLn "-----------------------------------------"
  printBooks books