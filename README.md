# page

Page is an experimental collaborative writing space with no boundaries. It explores the concept of infinite digital space and shared creativity.

It is available at:
<div align="center">
  <a href="https://acondolu.me/page/" target="_blank">
    https://acondolu.me/page
  </a>
</div>

## Usage

Scroll to navigate. To jump to specific X,Y coordinates, click on the coordinate widget. Typed characters are persisted and shared in real-time with other users.

## Infinite Plane

The page extends infinitely in all directions, unrestricted even by `INT_MAX`! It uses `bigint`s (arbitrary precision integers) for coordinates, making the canvas truly boundless. Well, there's a practical limit: the available RAM on my server (a small Raspberry Pi :grin:)...

Users can scroll endlessly in any direction or jump directly to any `bigint` coordinate by clicking on the navigator widget and entering X,Y coordinates.

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

```sh
python3 -m venv venv
venv/bin/pip install -r requirements.txt
venv/bin/python3 -m ai
```
