# Barbq

Barbq 🍖 is a simple, no frills, terminal-based status bar for macOS.

## Screenshots

<img width="731" alt="barbq" src="https://user-images.githubusercontent.com/515445/72689328-1d22a700-3ac5-11ea-9ad7-343965242db1.png">

<img width="1792" alt="motivation" src="https://user-images.githubusercontent.com/515445/72689329-1eec6a80-3ac5-11ea-999a-da140f9ac07c.png">

## Installation

### Prerequisites

* You're using the yabai tiling window manager on your system
* You're using a font with PragmataPro-style glyphs (afaik this means the PragmataPro font)
* Nix package manager is installed (optionally with [nix-darwin](https://github.com/LnL7/nix-darwin))
* You have Alacritty installed

### Steps

1. Clone this repo
2. Add barbq to your [nix-darwin environment.systemPrograms array](https://github.com/bkase/life/blob/8c9f03973da0daffb3efc2a53b5e6d3a82644ac1/darwin-configuration.nix#L7..L17). Alternatively, `nix-build release.nix` and add the `./result` directory to your `PATH`
3. Run the following commands (perhaps in a [launchd service](https://github.com/bkase/life/blob/e1fb8c0acf886cf9a65b090f9c797291124c7e54/darwin-configuration.nix#L31..L46))

```
yabai -m rule --add app=Alacritty sticky=on
# You may need to change the path to Alacritty.app for your system
~/Applications/Nix\ Apps/Alacritty.app/Contents/MacOS/alacritty -d 180 1 --position 0 0 -e barbq
```

4. Barbq runs!
