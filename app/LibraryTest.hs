module LibraryTest where

import Test.HUnit
import Library

-- addBook Test - Adding one book and check if it is added properly
testAddOneBook :: Test
testAddOneBook = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addBook "ABC" "XYZ" library
	let bookList   = books updatedlibrary
	assertEqual "Book list should contain 1 item" 1 (length bookList)
	assertEqual "First book ID should be 1"   1 (bookId (head bookList))
	assertEqual "First book title should be ABC" "ABC" (title (head bookList))
	assertEqual "First book author should be XYZ" "XYZ" (author (head bookList))

-- addBook Test - Adding two books and check if they are added properly
testAddTwoBook :: Test
testAddTwoBook = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addBook "ABC" "XYZ" library
	let updatedlibrary2 = addBook "TWO" "TEST" updatedlibrary
	
	let bookList   = books updatedlibrary2
	let secondBook  = bookList !! 1
	
	assertEqual "Book list should contain 2 item" 2 (length bookList)
	assertEqual "Second book ID should be 2"   2 (bookId secondBook)
	assertEqual "Second book title should be TWO" "TWO" (title secondBook)
	assertEqual "Second book author should be TEST" "TEST" (author secondBook)

-- removeBook Test - Removing the added book and check if it is removed properly
testRemoveBook :: Test
testRemoveBook = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addBook "ABC" "XYZ" library
	
	let (updatedlibrary2, message) = removeBook 1 updatedlibrary
		
	let bookList   = books updatedlibrary2
	
	assertEqual "Message should be proper." "Book removed!" message
	assertEqual "Book list should contain 0 item" 0 (length bookList)
	
-- removeBook Test - Removing the added book and check if it is removed properly
testRemoveBookInvalid :: Test
testRemoveBookInvalid = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addBook "ABC" "XYZ" library
	let (updatedlibrary2, message) = removeBook 0 updatedlibrary
		
	let bookList   = books updatedlibrary2
	
	assertEqual "Message should be proper." "Book ID is invalid!" message
	assertEqual "Book list should contain 1 item" 1 (length bookList)
	
	
-- addUser Test - Adding one user and check if it is added properly
testAddOneUser :: Test
testAddOneUser = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addUser "ABC" library
	let userList   = users updatedlibrary
	assertEqual "User list should contain 1 item" 1 (length userList)
	assertEqual "First user ID should be 1"   1 (userId (head userList))
	assertEqual "First user userName should be ABC" "ABC" (userName (head userList))

-- addUser Test - Adding two users and check if they are added properly
testAddTwoUser :: Test
testAddTwoUser = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addUser "ABC" library
	let updatedlibrary2 = addUser "TWO" updatedlibrary
	
	let userList   = users updatedlibrary2
	let secondUser  = userList !! 1
	
	assertEqual "User list should contain 2 item" 2 (length userList)
	assertEqual "Second user ID should be 2"   2 (userId secondUser)
	assertEqual "Second user userName should be TWO" "TWO" (userName secondUser)

-- removeUser Test - Removing the added user and check if it is removed properly
testRemoveUser :: Test
testRemoveUser = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addUser "ABC" library
	
	let (updatedlibrary2, message) = removeUser 1 updatedlibrary
		
	let userList   = users updatedlibrary2
	
	assertEqual "Message should be proper." "User removed !" message
	assertEqual "User list should contain 0 item" 0 (length userList)
	
-- removeUser Test - Removing the added user and check if it is removed properly
testRemoveUserInvalid :: Test
testRemoveUserInvalid = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addUser "ABC" library
	let (updatedlibrary2, message) = removeUser 0 updatedlibrary
		
	let userList   = users updatedlibrary2
	
	assertEqual "Message should be proper." "User ID is invalid!" message
	assertEqual "User list should contain 1 item" 1 (length userList)

-- borrowBook Test - Adding one user and one book, check if it is borrowBook modifies the status of the book
testBorrowBookWithValidUser :: Test
testBorrowBookWithValidUser = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addBook "Book1" "XYZ" library
	let updatedlibrary2 = addUser "User1" updatedlibrary
	let (updatedlibrary3, message) = borrowBook 1 1 updatedlibrary2
	
	let bookList   = books updatedlibrary3
	
	assertEqual "Message should be proper." "Book borrowed!" message
	assertEqual "Book list should contain 1 item" 1 (length bookList)
	assertEqual "Book status should be 1"   Borrowed (status (head bookList))
	assertEqual "Book borrowerName should be User1"   (Just "User1") (borrowerName (head bookList))

-- borrowBook Test - Adding one user and one book, check if it is borrowBook with invalid user ID should return invalid message
testBorrowBookWithInalidUser :: Test
testBorrowBookWithInalidUser = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addBook "Book1" "XYZ" library
	let updatedlibrary2 = addUser "User1" updatedlibrary
	let (updatedlibrary3, message) = borrowBook 1 0 updatedlibrary2
	
	let bookList   = books updatedlibrary3
	
	assertEqual "Message should be proper." "User ID is invalid!" message

-- borrowBook Test - Adding one user and one book, check if it is borrowBook with invalid book ID should return invalid message
testBorrowBookWithInalidBook :: Test
testBorrowBookWithInalidBook = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addBook "Book1" "XYZ" library
	let updatedlibrary2 = addUser "User1" updatedlibrary
	let (updatedlibrary3, message) = borrowBook 0 1 updatedlibrary2
	
	assertEqual "Message should be proper." "Book ID is invalid!" message

-- borrowBook Test - Adding one user and one book, check if it is borrowBook with invalid book ID should return invalid message
testBorrowBookWithAlreadyBorrowedBook :: Test
testBorrowBookWithAlreadyBorrowedBook = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addBook "Book1" "XYZ" library
	let updatedlibrary2 = addUser "User1" updatedlibrary
	let (updatedlibrary3, message) = borrowBook 1 1 updatedlibrary2
	let (updatedlibrary4, message2) = borrowBook 1 1 updatedlibrary3
	
	assertEqual "Message should be proper." "Book is borrowed!" message2

-- returnBook Test - Adding one user and one book, check if it is returnBook with valid user ID should return valid message
testReturnBookWithValidUser :: Test
testReturnBookWithValidUser = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addBook "Book1" "XYZ" library
	let updatedlibrary2 = addUser "User1" updatedlibrary
	let (updatedlibrary3, message) = borrowBook 1 1 updatedlibrary2
	let (updatedlibrary4, message) = returnBook 1 1 updatedlibrary3
	
	let bookList   = books updatedlibrary4
	
	assertEqual "Book list should contain 1 item" 1 (length bookList)
	assertEqual "Book status should be 1"   Available (status (head bookList))
	assertEqual "Message should be proper." "Book returned!" message
	
-- returnBook Test - Adding one user and one book, check if it is returnBook with invalid user ID should return invalid message
testReturnBookWithInvalidUser :: Test
testReturnBookWithInvalidUser = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addBook "Book1" "XYZ" library
	let updatedlibrary2 = addUser "User1" updatedlibrary
	let (updatedlibrary3, message) = borrowBook 1 1 updatedlibrary2
	let (updatedlibrary4, message) = returnBook 1 0 updatedlibrary3
	
	assertEqual "Message should be proper." "User ID is invalid!" message

-- returnBook Test - Adding one user and one book, check if it is returnBook with invalid book ID should return invalid message
testReturnBookWithInvalidBook :: Test
testReturnBookWithInvalidBook = TestCase $ do
	let currentLibrary :: Library =
			Library
				{ books = [],
					users = [],
					bookCount = 0,
					userCount = 0
				}
	let updatedlibrary = addBook "Book1" "XYZ" library
	let updatedlibrary2 = addUser "User1" updatedlibrary
	let (updatedlibrary3, message) = returnBook 0 1 updatedlibrary2
	
	assertEqual "Message should be proper." "Book ID is invalid!" message
	
tests :: Test
tests = TestList
  [ TestLabel "Add one book" testAddOneBook,
	TestLabel "Add two book" testAddTwoBook,
	TestLabel "Remove a book" testRemoveBook,
	TestLabel "Remove an invalid book" testRemoveBookInvalid,
	TestLabel "Add one user" testAddOneUser,
	TestLabel "Add two user" testAddTwoUser,
	TestLabel "Remove a user" testRemoveUser,
	TestLabel "Remove an invalid user" testRemoveUserInvalid,
	TestLabel "Borrow a book with a valid user" testBorrowBookWithValidUser,
	TestLabel "Borrow a book with an invalid user" testBorrowBookWithInalidUser,
	TestLabel "Borrow a book with an invalid book" testBorrowBookWithInalidBook,
	TestLabel "Borrow a book with already borrowed book" testBorrowBookWithAlreadyBorrowedBook,
	TestLabel "Return a book with an valid user" testReturnBookWithValidUser,
	TestLabel "Return a book with an invalid user" testReturnBookWithInvalidUser,
	TestLabel "Return a book with an invalid book" testReturnBookWithInvalidBook
  ]

main :: IO Counts
main = runTestTT tests