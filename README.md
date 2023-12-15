# N-Gomoku in Haskell

## Team Member
Linhan Li

Junhua Yang

Mudi Huang
## Introduction
Gomoku, Five in a Row, is a traditional two-player chess game on a grid board. The objective is to be the first player to create an unbroken row of five stones horizontally, vertically, or diagonally on the board. We improve the game by allowing users to define the rules of this game. They may decide how many stones in an unbroken row will select game winners. 

## Rules of Gomoku:
**Board**: The game is played on a square grid board, 15x15.<br>
**Players**: Two players take turns placing their stones on the intersections of the grid.<br>
**Define the rule**: Choose the number N (no less than 4). If a user first creates an unbroken row of N stones, the user wins the game.<br>
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
	**Milestone 3**:  Implementation of the network-related functions.<br>

## Update (12/01/23)
### 1. Architecture
![ga2](./docs/game_architecture_solid.png)

The project is formed by three main parts:<br>
#### 1.1.User Interface
Users can interact with game logic by getting game status from User Interface(UI) and bypassing their actions to change game status via UI.<br>
#### 1.2.Game Logic
The game logic component contains two subparts: Game Board and Game Status Agent. <br>
**Game Board**: record game status <br>
**Status Agent**: to see if any user wins the game <br>
#### 1.3.Connecting Components
The connecting components contain two subparts: Game Renderer and Action Handler. <br>
**Game Renderer**: Rendering game status from game logic into UI, so the user can get game status from UI <br>
**Action Handler**: Processing actions from users' inputs and changing the game status accordingly <br>

### 2. Challenges
1. Development under the Brick Schema. We need to learn and implement Brick Schema in a short time, which is a challenging task. To tackle the Brick schema, we examined different examples listed in the official git repo.<br>
2. Game is a different application compared with other types of applications. It requires more interaction between the user and the system, so we took more time to consider different scenarios. <br>
3. We still need to tackle game AI and multiplayer-related issues.<br>

### 3. Expectations
The priority of the project is the stability of the game. Beyond stability, we will try to implement other game functions. If we cannot catch up with the original schedule, we'll try our best to implement gaming AI or online gaming.


   