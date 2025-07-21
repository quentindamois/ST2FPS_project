# ST2FPS_project - Library Management System

This project is a library management system developed in Scala for the functional programming course.

## Project Structure

```
library-management-system/
├── build.sbt                    # SBT Configuration
├── project/
│   ├── build.properties        # SBT Version
│   └── plugins.sbt            # SBT Plugins 
├── src/
│   ├── main/
│   │   └── scala/
│   │       ├── models/         # Data Models
│   │       │   ├── Book.scala
│   │       │   ├── User.scala
│   │       │   ├── Transaction.scala
│   │       │   └── Catalog.scala
│   │       ├── services/       # Business Services
│   │       │   ├── LibraryService.scala
│   │       │   ├── RecommendationService.scala
│   │       │   └── ValidationService.scala
│   │       ├── utils/          # Utility classes
│   │       │   ├── JsonUtil.scala
│   │       │   └── ErrorHandling.scala
│   │       └── Main.scala      # Entry point
│   └── test/
│       └── scala/
│           ├── models/
│           │   └── BookSpec.scala
│           └── services/
│               └── LibraryServiceSpec.scala
├── data/                       # JSON data (generated)
├── README.md
└── .gitignore
```

## Features

- **Book Management** : Add, search by title/author/genre
- **User Management** : Registration, user types
- **Borrow/return System** : Transaction handling with due dates
- **Recommendations** : Suggestions based on borrowing history
- **Validation** : Data validation with error handling
- **Persistence** : Save/load data in JSON format

## Prerequisites

- Java 11 or higher
- SBT (Scala Build Tool) 1.9.6

## Installation and Usage

### Compile the project
```bash
sbt compile
```

### Run the application
```bash
sbt run
```

### Run the tests
```bash
sbt test
```

### Launch Scala REPL with project classes
```bash
sbt console
```

## Functional Programming Concepts

This project illustrates several key concepts:

- **Immutability** : All data structures are immutable
- **Pure functions** : Functions have no side effects
- **Either for error handling** : Functional error management
- **Case classes and pattern matching** : Data modeling
- **Higher-order functions** : Map, filter, fold, etc.
- **Function composition** : Chaining operations
- **Type safety** : Leverage Scala's type system

## Architecture

The project follows a layered architecture:

1. **Models** (`models/`) : Business entities definition
2. **Services** (`services/`) : Business logic and orchestration
3. **Utilities** (`utils/`) : Cross-cutting helper functions
4. **Main** : Entry point and demonstration

## Example Usage

```scala
import models.*
import services.*

// Create a library service
val libraryService = new LibraryService()

// Add a book
val book = Book(
  id = "1",
  title = "Le Petit Prince",
  author = List["Antoine de Saint-Exupéry"],
  isbn = "978-2-07-040848-4",
  publishedDate = LocalDate.of(1943, 4, 6),
  genre = "Littérature",
  totalCopies = 5,
  availableCopies = 5
)

libraryService.addBook(book)

// Add a user
val user = User(
  id = "1",
  firstName = "Marie",
  lastName = "Dupont",
  email = "marie@email.com",
  membershipDate = LocalDate.now(),
  userType = UserType.Student
)

libraryService.addUser(user)

// Borrow a book
libraryService.borrowBook(user.id, book.id)
```
