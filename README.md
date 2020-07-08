# gamer

Classic Video Games Implemented with R

> The games marked ![](https://img.shields.io/badge/dynamic-yes-critical) are supported by animation effect using the idle event handler in `getGraphicsEvent {grDevices}`.
This feature could be CPU-intensive, and currently does not apply on Windows.

## Installation

```r
devtools::install_github("darrenproj/gamer")
```

## 1. Tetris ![](https://img.shields.io/badge/dynamic-yes-critical)

<img src="man/figures/tetris_demo.gif" width="400" align="right" />

```r
gamer::play_tetris()
```

#### Keyboard Event

- <kbd>Left</kbd> <kbd>Right</kbd> <kbd>Down</kbd> : Move
- <kbd>Up</kbd> : Rotate
- <kbd>Space</kbd> : Fall
- <kbd>A</kbd> : Alter
- <kbd>Q</kbd> : Quit

#### Scoring System

<img src="man/figures/tetris_score.png" width="250" />
