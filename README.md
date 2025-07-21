# ST2FPS_project - Système de Gestion de Bibliothèque

Ce projet est un système de gestion de bibliothèque développé en Scala pour le cours de programmation fonctionnelle.

## Structure du projet

```
library-management-system/
├── build.sbt                    # Configuration SBT
├── project/
│   ├── build.properties        # Version SBT
│   └── plugins.sbt            # Plugins SBT
├── src/
│   ├── main/
│   │   └── scala/
│   │       ├── models/         # Modèles de données
│   │       │   ├── Book.scala
│   │       │   ├── User.scala
│   │       │   ├── Transaction.scala
│   │       │   └── Catalog.scala
│   │       ├── services/       # Services métier
│   │       │   ├── LibraryService.scala
│   │       │   ├── RecommendationService.scala
│   │       │   └── ValidationService.scala
│   │       ├── utils/          # Utilitaires
│   │       │   ├── JsonUtil.scala
│   │       │   └── ErrorHandling.scala
│   │       └── Main.scala      # Point d'entrée
│   └── test/
│       └── scala/
│           ├── models/
│           │   └── BookSpec.scala
│           └── services/
│               └── LibraryServiceSpec.scala
├── data/                       # Données JSON (généré)
├── README.md
└── .gitignore
```

## Fonctionnalités

- **Gestion des livres** : Ajout, recherche par titre/auteur/genre
- **Gestion des utilisateurs** : Inscription, types d'utilisateurs
- **Système d'emprunt/retour** : Gestion des transactions avec dates d'échéance
- **Recommandations** : Suggestions basées sur l'historique d'emprunt
- **Validation** : Validation des données avec gestion d'erreurs
- **Persistence** : Sauvegarde/chargement en JSON

## Prérequis

- Java 11 ou supérieur
- SBT (Scala Build Tool) 1.9.6

## Installation et utilisation

### Compiler le projet
```bash
sbt compile
```

### Exécuter l'application
```bash
sbt run
```

### Exécuter les tests
```bash
sbt test
```

### Lancer le REPL Scala avec les classes du projet
```bash
sbt console
```

## Concepts de programmation fonctionnelle

Ce projet illustre plusieurs concepts clés :

- **Immutabilité** : Toutes les structures de données sont immutables
- **Fonctions pures** : Les fonctions n'ont pas d'effets de bord
- **Either pour la gestion d'erreurs** : Gestion fonctionnelle des erreurs
- **Case classes et pattern matching** : Modélisation de données
- **Higher-order functions** : Map, filter, fold, etc.
- **Composition de fonctions** : Chaînage d'opérations
- **Type safety** : Utilisation du système de types de Scala

## Architecture

Le projet suit une architecture en couches :

1. **Modèles** (`models/`) : Définition des entités métier
2. **Services** (`services/`) : Logique métier et orchestration
3. **Utilitaires** (`utils/`) : Fonctions transversales
4. **Main** : Point d'entrée et démonstration

## Exemple d'utilisation

```scala
import models.*
import services.*

// Créer un service de bibliothèque
val libraryService = new LibraryService()

// Ajouter un livre
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

// Ajouter un utilisateur
val user = User(
  id = "1",
  firstName = "Marie",
  lastName = "Dupont",
  email = "marie@email.com",
  membershipDate = LocalDate.now(),
  userType = UserType.Student
)

libraryService.addUser(user)

// Emprunter un livre
libraryService.borrowBook(user.id, book.id)
```
