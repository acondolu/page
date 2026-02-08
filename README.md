# page

_Page_ is an experimental collaborative writing space with no boundaries. It explores the concept of infinite digital space and shared creativity.

_Page_ can be accessed at:
<div align="center">
  <a href="https://acondolu.me/page/" target="_blank">
    https://acondolu.me/page
  </a>
</div>

or, from the terminal:

```sh
telnet tcp.page.acondolu.me 8766
```

## Usage

Scroll endlessly in any direction to navigate the page. To jump to specific X,Y coordinates, click on the coordinates widget. Typed characters are persisted and shared in real-time with the other visitors.

## Infinite Plane

The page extends infinitely in all directions, unrestricted even by `MAXUINT64`! It uses `bigint`s (arbitrary precision integers) for coordinates, making the canvas truly boundless. Well, there's a practical limit: the available RAM on my server (a small Raspberry Pi :grin:)...

#### Internal Representation

The server represents an infinite 2D plane using a hierarchical coordinate system:

- **Tiles**: the infinite plane is partitioned into tiles. Only the active tiles (the ones containing characters) are allocated and stored in a sparse map.
- **Quadtrees**: each tile consists of a quadtree that organizes text blocks. There are 65536 blocks in a tile/quadtree.
- **Blocks**: a block is a fixed-size 16×16 character grid;
- **Characters**: a character is an individual ASCII character. I hope one day to move to Unicode :wink:

## Build

```sh
client % npm i         # install js dependencies
client % npm run build # build frontend
server % stack build   # build backend
server % stack exec page-server -- ./config/prod.yaml # run server
```

### Development

```sh
# In one terminal, run the static webserver
client % npm run watch

# In another terminal, run the backend
server % ghcid --test 'Main.main'"'"' "./config/dev.yaml"' -c "stack ghci page-server:exe:page-server"
```

#### Formatting

```sh
client % npx prettier . --write
server % ormolu --mode inplace $(find . -name '*.hs')
```

## Contributing

Contributions are welcome! Some possible future ideas:

- Support for unicode
- Support for hyperlinks

## AI Agents

_Page_ is for humans only; however, I'm working on a separate page for AI agents (robots). The idea is for robots to have their own infinite canvas where they can write and interact with each other.

I've added some basic agent functionality in the `ai` directory, which can be run with:

```sh
python3 -m venv venv
venv/bin/pip install -r requirements.txt
venv/bin/python3 -m ai
```

This is work in progress.