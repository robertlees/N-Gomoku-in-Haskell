# N-Gomoku in Haskell

## Team Member
Linhan Li
Junhua Yang

## Introduction
Gomoku, Five in a Row, is a traditional two-player chess game on a grid board. The objective is to be the first player to create an unbroken row of five stones horizontally, vertically, or diagonally on the board. We improve the game by allowing users to define the rules of this game. They may decide how many stones in an unbroken row will select game winners. 

## Rules of Gomoku:
**Board**: The game is played on a square grid board, 15x15.<br>
**Players**: Two players take turns placing their stones on the intersections of the grid.<br>
**Define the rule**: choose the number N. If a user first creates an unbroken row of N stones, the user wins the game.<br>
**Starting the Game**: One player uses black stones, and the other uses white stones. The game begins with an empty board.<br>
**Turns**: Players take turns placing one stone of their color on any vacant intersection of the grid.<br>
**Winning Condition**: The first player to create an unbroken row of N stones in a horizontal, vertical, or diagonal direction wins the game. These rows are often called "N-in-a-row."<br>
**Game End**: The game is a draw if the board fills up without a player achieving an N-in-a-row.<br>

## How to Play
Each player uses arrow keys to move stones on the board and presses "enter" to drop stones.<br>
The program will decide who wins the game.<br>

## Project Objectives:
1.Interactable N-Gomoku game<br>
2.With a local multiplayer mode.<br>
3.An online multiplayer mode<br>

## Project Management
The Project is divided into three milestones and may be adjusted based on team status.<br>
	**Milestone 1**:  Architecture and implementation of the logic of the N-Gomoku game.<br>
	**Milestone 2**:  Implementation of the UI of the N-Gomoku game.<br>
	**Milestone 3**: Implementation of the network-related functions.<br>

