module Library where

data Status
  = Available
  | Borrowed
  deriving (Eq, Show)

data Book = Book
  { bookId :: Int,
    title :: String,
    author :: String,
    status :: Status,
    borrowerName :: Maybe String
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
    userCount :: Int
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

-- Book Section

-- Add a new Book
addBook :: String -> String -> Library -> Library
addBook inputTitle inputAuthor inputLibrary =
  let newBookCount = bookCount inputLibrary + 1

      newBook =
        Book
          { bookId = newBookCount,
            title = inputTitle,
            author = inputAuthor,
            status = Available,
            borrowerName = Nothing
          }
   in inputLibrary
        { books = books inputLibrary ++ [newBook],
          bookCount = newBookCount
        }

-- Remove a Book
removeBook :: Int -> Library -> (Library, String)
removeBook inputBookId inputLibrary =
  let allBooks = books inputLibrary
      targetBooks = [book | book <- allBooks, bookId book == inputBookId]
      otherBooks = [book | book <- allBooks, bookId book /= inputBookId]
   in case targetBooks of
        [] -> (inputLibrary, "Book ID is invalid!")
        [book] ->
          case status book of
            Borrowed -> (inputLibrary, "Book is borrowed!")
            Available -> (inputLibrary {books = otherBooks}, "Book removed!")

-- Borrow Book
borrowBook :: Int -> Int -> Library -> (Library, String)
borrowBook inputBookId inputUserId inputLibrary =
  let allBooks = books inputLibrary
      allUsers = users inputLibrary
      targetBooks = [book | book <- allBooks, bookId book == inputBookId]
      otherBooks = [book | book <- allBooks, bookId book /= inputBookId]
      targetUsers = [user | user <- allUsers, userId user == inputUserId]
   in case targetBooks of
        [] -> (inputLibrary, "Book ID is invalid!")
        [book] ->
          case status book of
            Borrowed -> (inputLibrary, "Book is borrowed!")
            Available ->
              case targetUsers of
                [] -> (inputLibrary, "User ID is invalid!")
                [user] ->
                  let name = userName user
                      updatedLibrary = inputLibrary {books = [book {status = Borrowed, borrowerName = Just name}] ++ otherBooks}
                   in (updatedLibrary, "Book borrowed!")

-- Return Book
returnBook :: Int -> Int -> Library -> (Library, String)
returnBook inputBookId inputUserId inputLibrary =
  let allBooks = books inputLibrary
      allUsers = users inputLibrary
      targetBooks = [book | book <- allBooks, bookId book == inputBookId]
      otherBooks = [book | book <- allBooks, bookId book /= inputBookId]
      targetUsers = [user | user <- allUsers, userId user == inputUserId]
   in case targetBooks of
        [] -> (inputLibrary, "Book ID is invalid!")
        [book] ->
          case status book of
            Available -> (inputLibrary, "Book is available!")
            Borrowed ->
              case targetUsers of
                [] -> (inputLibrary, "User ID is invalid!")
                [user] ->
                  let name = userName user
                   in case borrowerName book of
                        Nothing -> (inputLibrary, "Book is not borrowed by anyone!")
                        Just borrowerName ->
                          if borrowerName /= name
                            then (inputLibrary, "Book is not borrowed by this user!")
                            else
                              let updatedLibrary = inputLibrary {books = [book {status = Available, borrowerName = Nothing}] ++ otherBooks}
                               in (updatedLibrary, "Book returned!")

-- Print Available Books
availableBooks :: [Book] -> IO ()
availableBooks [] = putStrLn "No available books."
availableBooks allBooks = do
  let availableBookList = [book | book <- allBooks, status book == Available]
  putStrLn "Available Books:"
  printBooks availableBookList

-- Print Borrowed Books
borrowedBooks :: [Book] -> IO ()
borrowedBooks [] = putStrLn "No borrowed books."
borrowedBooks allBooks = do
  let borrowedBookList = [book | book <- allBooks, status book == Borrowed]
  putStrLn "Borrowed Books:"
  printBooks borrowedBookList

-- Print Books
printBooks :: [Book] -> IO ()
printBooks [] = return ()
printBooks (book : otherBooks) = do
  putStrLn "-----------------------------------------"
  putStrLn ("Book Id     : " ++ show (bookId book))
  putStrLn ("Book Name   : " ++ title book)
  putStrLn ("Book Author : " ++ author book)
  putStrLn ("Book Status : " ++ show (status book))

  case borrowerName book of
    Nothing ->
      return ()
    Just name ->
      putStrLn ("Book Borrower: " ++ name)

  putStrLn "-----------------------------------------"
  printBooks otherBooks

-- User Section

-- Add a new User
addUser :: String -> Library -> Library
addUser inputName inputLibrary =
  let newUserCount = userCount inputLibrary + 1

      newUser =
        User
          { userId = newUserCount,
            userName = inputName
          }
   in inputLibrary
        { users = users inputLibrary ++ [newUser],
          userCount = newUserCount
        }

-- Print Users
printUsers :: [User] -> IO ()
printUsers [] = return ()
printUsers (user : otherUsers) = do
  putStrLn "-----------------------------------------"
  putStrLn ("User Id     : " ++ show (userId user))
  putStrLn ("User Name   : " ++ userName user)
  putStrLn "-----------------------------------------"
  printUsers otherUsers

-- Remove a User
removeUser :: Int -> Library -> (Library, String)
removeUser inputUserId inputLibrary =
  let allUsers = users inputLibrary
      allBooks = books inputLibrary

      targetUsers = [user | user <- allUsers, userId user == inputUserId]
      otherUsers = [user | user <- allUsers, userId user /= inputUserId]
   in case targetUsers of
        [] -> (inputLibrary, "User ID is invalid!")
        [user] ->
          let name = userName user
              targetBooks = [book | book <- allBooks, borrowerName book == Just name]
           in case targetBooks of
                [] -> (inputLibrary {users = otherUsers}, "User removed !")
                _ -> (inputLibrary, "User has borrowed one or many books !")