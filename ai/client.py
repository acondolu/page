import asyncio
import json
import math
import string
import sys
from typing import Any, List, Optional
import websockets

class ToolCallResult:
    def __init__(self, status: str, x: int = 0, y: int = 0, w: int = 0, h: int = 0, text: Optional[str] = None, reason: str = ""):
        self.status = status
        self.x = x
        self.y = y
        self.w = w
        self.h = h
        self.text = text
        self.reason = reason
    
    def to_string(self):
        if self.status == "success":
            return json.dumps({
                "status": self.status,
                "x": self.x,
                "y": self.y,
                "w": self.w,
                "h": self.h,
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
            if not text or not text.strip():
                descr = "<empty>"
            else:
                descr = "```\n" + text + "\n```"
            return f"region from ({x}, {y}) to ({x+self.w}, {y+self.h}):\n{descr}"

    @staticmethod
    def from_string(s: str) -> 'ToolCallResult':
        data = json.loads(s)
        if data["status"] == "success":
            return ToolCallResult(
                status=data["status"],
                x=data["x"],
                y=data["y"],
                w=data.get("w", 0),
                h=data.get("h", 0),
                text=data.get("text", ""),
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
                self._pending.set_result(msg["text"])
                self._pending = None

    async def read(self, x: int, y: int, w: int, h: int) -> ToolCallResult:
        if self.ws is None:
            raise RuntimeError("WebSocket is not connected")
        ax = (x // 16) * 16
        ay = (y // 16) * 16
        bx = math.ceil((x + w) / 16) * 16
        by = math.ceil((y + h) / 16) * 16
        kx = x - ax
        ky = y - ay
        ret: List[List[str]] = [[] for _ in range(by - ay)]
        i = 0
        for iy in range(ay, by, 16):
            for ix in range(ax, bx, 16):
                res = await self._read_helper(ix, iy)
                for j in range(16):
                    ret[i + j].append(res[j])
            i += 16
        ret2 = ["".join(row)[kx:kx+w].replace("\n", " ") for row in ret]
        return ToolCallResult("success", x, y, w, h, "\n".join(ret2[ky:ky+h]))
    
    async def write(self, x: int, y: int, text: str) -> ToolCallResult:
        if self.ws is None:
            raise RuntimeError("WebSocket is not connected")
        lines = text.splitlines()
        await self.ws.send(json.dumps({
                "tag": "move-relative",
                "x": 0,
                "y": 0,
            }))
        h = len(lines)
        w = max(len(line) for line in lines)
        for i in range(h):
            for j in range(len(lines[i])):
                char = lines[i][j]
                if char == "\n": break
                if char not in string.printable[:-5]: continue
                await self.ws.send(json.dumps({
                    "tag": "write-char",
                    "c": char,
                    "x": x + j,
                    "y": y + i,
                }))
                await asyncio.sleep(0.05)
        return ToolCallResult("success", x, y, w, h, text)
    
    async def _read_helper(self, ix: int, iy: int) -> str:
        if self.ws is None:
            raise RuntimeError("WebSocket is not connected")
        ax: int = (ix // 16) * 16
        ay: int = (iy // 16) * 16
        await self.ws.send(json.dumps({
            "tag": "move-relative",
            "x": ax,
            "y": ay,
        }))
        fut = asyncio.get_running_loop().create_future()
        self._pending = fut
        await self.ws.send(json.dumps({
            "tag": "read0",
        }))
        return await fut

    async def call_tool(self, name: str, arguments: Any) -> ToolCallResult:
        if self.ws is None:
            raise RuntimeError("WebSocket is not connected")
        # print(f"Calling tool {name} with arguments {arguments}")
        if name == "stop":
            sys.exit(0)
        elif name in ("read", "functions/read"):
            print(f"Tool call: read at ({repr(arguments['x'])}, {repr(arguments['y'])})")
            x = int(arguments["x"])
            y = int(arguments["y"])
            w = arguments.get("w", arguments.get("width"))
            h = arguments.get("h", arguments.get("height"))
            if w is None or h is None:
                return ToolCallResult("error", reason="Missing width or height argument")
            return await self.read(x, y, int(w), int(h))
        elif name in ("write", "functions/write"):
            print(f"Tool call: write at ({repr(arguments['x'])}, {repr(arguments['y'])})")
            x = int(arguments["x"])
            y = int(arguments["y"])
            text = arguments["text"]
            return await self.write(x, y, text)
        else:
            return ToolCallResult("error", reason=f"Unknown tool: {name}")
