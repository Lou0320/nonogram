# Nonogram Solver / Résolveur de Nonogrammes

## English

### Overview
This project is an implementation of a **Nonogram puzzle solver** written in OCaml (Objective Caml). A nonogram, also known as a *picross*, *griddlers*, or *pic-a-pix*, is a logic puzzle that combines elements of crosswords and mathematics.

### What is a Nonogram?
A nonogram is a puzzle game where you fill in cells in a grid to reveal a hidden picture. You are given:
- **Row clues**: Numbers indicating consecutive filled cells in each row
- **Column clues**: Numbers indicating consecutive filled cells in each column

The goal is to determine which cells should be filled (marked with "X") and which should be left empty (".") by using logical deduction based on the given clues.

#### Example
For a grid with row clue `1 1 1` and column clue `3`, it means:
- In one dimension, there are 3 groups of consecutive filled cells (with at least one empty cell between groups)
- In the other dimension, there are 3 consecutive filled cells

### Project Goal
This project implements a **solver algorithm** that:
1. Parses nonogram puzzle definitions from a file
2. Uses constraint satisfaction and logical deduction
3. Fills in the solution grid automatically
4. Displays the complete nonogram solution

### Problem-Solving Approach
The implementation uses:
- **Constraint satisfaction**: Eliminating possibilities that violate row/column constraints
- **Logical deduction**: Using clues to determine which cells must be filled or empty
- **Functional programming**: Leveraging OCaml's functional paradigm for elegant problem-solving

### File Structure
- `tp4.ml`: Main OCaml implementation containing the nonogram solver
- `nonogram.txt`: Example puzzle input file with grid dimensions and clues

### Input Format
```
<rows> <columns>
L <row_clue_1> : <row_clue_2> : ... : <row_clue_n>
C <col_clue_1> : <col_clue_2> : ... : <col_clue_m>
```

Example:
```
5 5
L 1 1 1 : 1 1 : 3 : 2 : 1 1
C 1 : 1 : 3 : 3 : 4
```

### Play Nonogram Games Online
For an interactive experience solving nonogram puzzles, visit:
**[Puzzle Nonograms](https://www.puzzle-nonograms.com/)**

### Compilation & Usage
```bash
ocaml tp4.ml
```

---

## Français

### Aperçu
Ce projet est une implémentation d'un **résolveur de puzzles nonogrammes** écrit en OCaml (Objective Caml). Un nonogramme, également connu sous le nom de *picross*, *griddlers* ou *pic-a-pix*, est un puzzle logique qui combine des éléments de mots croisés et des mathématiques.

### Qu'est-ce qu'un Nonogramme ?
Un nonogramme est un puzzle où vous remplissez les cases d'une grille pour révéler une image cachée. Vous êtes donné :
- **Indices de lignes** : Nombres indiquant les cellules remplies consécutives dans chaque ligne
- **Indices de colonnes** : Nombres indiquant les cellules remplies consécutives dans chaque colonne

L'objectif est de déterminer quelles cellules doivent être remplies (marquées avec "X") et lesquelles doivent rester vides (".") en utilisant la déduction logique basée sur les indices fournis.

#### Exemple
Pour une grille avec l'indice de ligne `1 1 1` et l'indice de colonne `3`, cela signifie :
- Dans une dimension, il y a 3 groupes de cellules remplies consécutives (avec au moins une cellule vide entre les groupes)
- Dans l'autre dimension, il y a 3 cellules remplies consécutives

### Objectif du Projet
Ce projet implémente un **algorithme de résolution** qui :
1. Analyse les définitions de puzzles nonogrammes à partir d'un fichier
2. Utilise la satisfaction de contraintes et la déduction logique
3. Remplit automatiquement la grille de solution
4. Affiche la solution complète du nonogramme

### Approche de Résolution des Problèmes
L'implémentation utilise :
- **Satisfaction de contraintes** : Éliminer les possibilités qui violent les contraintes des lignes/colonnes
- **Déduction logique** : Utiliser les indices pour déterminer quelles cellules doivent être remplies ou vides
- **Programmation fonctionnelle** : Exploiter le paradigme fonctionnel d'OCaml pour une résolution élégante des problèmes

### Structure des Fichiers
- `tp4.ml` : Implémentation OCaml principale contenant le résolveur de nonogrammes
- `nonogram.txt` : Fichier d'entrée exemple avec les dimensions de la grille et les indices

### Format d'Entrée
```
<lignes> <colonnes>
L <indice_ligne_1> : <indice_ligne_2> : ... : <indice_ligne_n>
C <indice_col_1> : <indice_col_2> : ... : <indice_col_m>
```

Exemple :
```
5 5
L 1 1 1 : 1 1 : 3 : 2 : 1 1
C 1 : 1 : 3 : 3 : 4
```

### Jouer à des Nonogrammes en Ligne
Pour une expérience interactive de résolution de puzzles nonogrammes, visitez :
**[Puzzle Nonograms](https://www.puzzle-nonograms.com/)**

### Compilation & Utilisation
```bash
ocaml tp4.ml
```

---

## Author / Auteur
Lou-Ann Coquard-Morel

## Course / Cours
Functional Programming - L2 Level / Programmation Fonctionnelle - Niveau L2
