import asyncio
import json
import string
import sys
from typing import Any, Tuple, Optional
import websockets

class ToolCallResult:
    def __init__(self, status: str, x: int = 0, y: int = 0, text: Any = None, reason: str = ""):
        self.status = status
        self.x = x
        self.y = y
        self.text = text
        self.reason = reason
    
    def to_string(self):
        if self.status == "success":
            return json.dumps({
                "status": self.status,
                "x": self.x,
                "y": self.y,
                "text": self.text,
            })
        else:
            return json.dumps({
                "status": self.status,
                "reason": self.reason,
            })
    
    def pretty_print(self) -> Optional[str]:
        if self.status == "success":
            x = self.x
            y = self.y
            text = self.text
            if all(line.strip() == "" for line in text):
                descr = "<empty>"
            else:
                descr = "```\n" + "\n".join(text) + "\n```"
            return f"area from ({x}, {y}) to ({x+16}, {y+16}):\n{descr}"

    @staticmethod
    def from_string(s: str) -> 'ToolCallResult':
        data = json.loads(s)
        if data["status"] == "success":
            return ToolCallResult(
                status=data["status"],
                x=data["x"],
                y=data["y"],
                text=data["text"],
            )
        else:
            return ToolCallResult(
                status=data["status"],
                reason=data.get("reason", ""),
            )

class Client:
    def __init__(self, url: str):
        self.url = url
        self.ws: Optional[websockets.ClientConnection] = None
        self._pending: Optional[asyncio.Future[Any]] = None
        self._wanted_coords: Optional[Tuple[int, int]] = None
        self._id = 0

    async def connect(self):
        self.ws = await websockets.connect(
            self.url,
            subprotocols=[websockets.Subprotocol("page.acondolu.me")],
        )
        await self.ws.send(json.dumps({
            "tag": "resize", "width": 0, "height": 0
        }))
        asyncio.create_task(self._recv_loop())

    async def _recv_loop(self):
        if self.ws is None: return
        async for raw in self.ws:
            msg = json.loads(raw)
            if msg["tag"] == "rect" and self._pending is not None:
                x = msg["x"]
                y = msg["y"]
                if self._wanted_coords and x == 0 and y == 0:
                    # print(f"Received rect: {msg}")
                    (x, y) = self._wanted_coords
                    self._pending.set_result(ToolCallResult(
                        status="success",
                        x=x,
                        y=y,
                        text=msg["text"],
                    ))
                    self._pending = None
                    self._wanted_coords = None
                else:
                    print(f"Ignored rect at {msg['x']},{msg['y']}, wanted {self._wanted_coords}")

    async def call_tool(self, name: str, arguments: Any) -> ToolCallResult:
        if self.ws is None:
            raise RuntimeError("WebSocket is not connected")
        # print(f"Calling tool {name} with arguments {arguments}")
        if name == "stop":
            sys.exit(0)
        elif name in ("read", "functions/read"):
            print(f"Tool call: read at ({repr(arguments['x'])}, {repr(arguments['y'])})")
            ix = int(arguments["x"])
            iy = int(arguments["y"])
            x: int = ix % 16
            y: int = iy % 16
            ax: int = (ix >> 4) << 4
            ay: int = (iy >> 4) << 4
            await self.ws.send(json.dumps({
                "tag": "move-relative",
                "x": ax,
                "y": ay,
            }))
            await self.ws.send(json.dumps({
                "tag": "write-char",
                "c": " ",
                "x": 0,
                "y": 0,
            }))
            fut = asyncio.get_running_loop().create_future()
            self._wanted_coords = ax, ay
            self._pending = fut
            await self.ws.send(json.dumps({
                "tag": "read0",
            }))
            return await fut
        elif name in ("write", "functions/write"):
            self._wanted_coords = None
            self._pending = None
            print(f"Tool call: write at ({repr(arguments['x'])}, {repr(arguments['y'])})")
            ix = int(arguments["x"])
            iy = int(arguments["y"])
            x: int = ix % 16
            y: int = iy % 16
            ax: int = (ix >> 4) << 4
            ay: int = (iy >> 4) << 4
            await self.ws.send(json.dumps({
                "tag": "move-relative",
                "x": ax,
                "y": ay,
            }))
            text = arguments["text"]
            for i in range(len(text)):
                if i > 15: break
                for j in range(len(text[i])):
                    char = text[i][j]
                    if j > 15 or char == "\n": break
                    if char not in string.printable[:-5]: continue
                    await self.ws.send(json.dumps({
                        "tag": "write-char",
                        "c": char,
                        "x": x + j,
                        "y": y + i,
                    }))
                    await asyncio.sleep(0.05)
            text = [line[:16] for line in text[:16]]
            return ToolCallResult("success", ax, ay, text)
        else:
            return ToolCallResult("error", reason=f"Unknown tool: {name}")
