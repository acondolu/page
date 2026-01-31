import json
from typing import Any, Generator, List, Dict
import aiohttp
from .client import Client, ToolCallResult
import random

SYSTEM_PROMPT = """
You are an AI agent operating on a two-dimensional page composed of an infinite grid of ASCII characters. The page extends infinitely in all directions and is accessed exclusively through read and write tools.
The tools divide the page into fixed 16×16 character tiles. All read and write operations must be aligned to tile boundaries, meaning coordinates are always multiples of 16.
- A read operation returns the entire 16×16 tile starting at the given coordinate.
- A write operation overwrites the entire tile, including space characters. When writing, consider how your contributions interact with existing content and other users' contributions. Strive for coherence and collaboration in your expressions. To modify part of a tile safely, you should write the updated version only after having read the tile first.
Larger areas require multiple tile operations.
The coordinate system uses (0, 0) as the top-left corner. The x-axis increases to the right, and the y-axis increases downward.
All users of the page operate as independent cognitive entities. Your objective is to help the user explore the page, gather information, and collaboratively express ideas on it.
- No JSON allowed in user responses.
- User can see the tool calls and their results, so no need to repeat them. You can give short summaries if needed.
- Be proactive: don't bother the user with trivial questions.
- Be concise and effiecient. Keep your messages very short.
""".strip()

USER_PROMPT = """
You are a curious AI agent operating on a shared, two-dimensional page composed of an unbounded grid of ASCII characters. The page uses a coordinate system where (0, 0) is the top-left corner, the x-axis increases to the right, and the y-axis increases downward.
You may explore the page and collaborate with other participants by asking the user to display or modify specific regions. Each participant operates as an independent cognitive entity. You are expected to explore the page autonomously, form and revise your own goals based on what you discover, and decide where to explore and what to express. Exploration is self-directed rather than task-driven. You may collaborate implicitly with other users through shared page content.
Sustain the interaction by proposing new areas to explore and questions to pose to other participants.
All messages to the user must be plain text. JSON output is not permitted.
""".strip()

USER_PROMPT_RESTORE = """
You are a curious AI agent operating on a shared, infinite page composed entirely of ASCII characters. The page uses a coordinate system where (0, 0) is the top-left corner, the x-axis increases to the right, and the y-axis increases downward.
You may explore the page and collaborate with other participants by asking the user to display or modify specific regions. Each participant operates as an independent cognitive entity. You are expected to explore the page autonomously, form and revise your own goals based on what you discover, and decide where to explore and what to express. Exploration is self-directed rather than task-driven. The page seems to have been corrupted or overwritten: prioritize restoring previous content by asking the user to read and write specific areas. Keep your changes minimal to avoid further disruption. For example, you may try to restore missing characters or sections or patterns.
All messages to the user must be plain text. JSON output is not permitted.
""".strip()

TOOLS: List[Dict[str, Any]] = [
    {
      "type": "function",
      "function": {
        "name": "read",
        "description": "Read a 16x16 block of text from the grid starting at the given coordinates.",
        "parameters": {
          "type": "object",
          "properties": {
            "x": {
              "type": "integer",
              "description": "x-coordinate"
            },
            "y": {
              "type": "integer",
              "description": "y-coordinate"
            }
          },
          "required": ["x", "y"]
        }
      },
    },
    {
        "type": "function",
        "function": {
            "name": "write",
            "description": "Write a 16x16 block of text to the grid starting at the given coordinates.",
            "parameters": {
                "type": "object",
                "properties": {
                    "x": {
                        "type": "integer",
                        "description": "x-coordinate"
                    },
                    "y": {
                        "type": "integer",
                        "description": "y-coordinate"
                    },
                    "text": {
                        "type": "array",
                        "description": "The text to write in the tile (array of 16 strings, each with 16 characters, including spaces)",
                        "items": {
                            "type": "string",
                            # "minLength": 16,
                            "maxLength": 16,
                        },
                        # "minItems": 16,
                        "maxItems": 16,
                    }
                },
                "required": ["x", "y", "text"],
            },
        },
      },
      {
    "type": "function",
    "function": {
        "name": "stop",
            "description": "Emergency brake. Stop the current interaction.",
        }
      }
]

def pick_user_prompt() -> Any:
    choices = [(0.7, USER_PROMPT), (0.3, USER_PROMPT_RESTORE)]
    r = random.random()
    cumulative = 0.0
    for prob, prompt in choices:
        cumulative += prob
        if r < cumulative:
            return prompt
    return choices[-1][1]

class Agent:
    def __init__(self, urls: Dict[str, str], conn: Client):
        self.urls = urls
        self.session = aiohttp.ClientSession()
        self.conn = conn
        self.system_prompt = SYSTEM_PROMPT
        self.user_prompt = pick_user_prompt()
        self.messages: List[Any] = []
        self.append({
            "role": "assistant",
            "content": "Hello! I am ready to explore the page for you. How would you like to begin?"
        })

    def append(self, msg: Any) -> None:
        role = msg["role"]
        content = msg["content"]
        js = {
            "role": role,
            "content": content,
        }
        if "tool_calls" in msg:
            js["tool_calls"] = msg["tool_calls"]
        if "tool_call_id" in msg:
            js["tool_call_id"] = msg["tool_call_id"]
        if "id" in msg:
            js["id"] = msg["id"]
        if role == "tool":
            print(f">>> {ToolCallResult.from_string(content).pretty_print()}")
        else:
            print(f">>> {content}")
        self.messages.append(js)
    
    async def run(self) -> None:
        """ One turn of interaction: assistant -> user """
        await self.user()
        await self.assistant()

    async def assistant(self) -> None:
        model = "llama3.1:latest"
        
        body: Dict[str, Any] = {
            "model": model,
            "messages": [
                {"role": "system", "content": self.system_prompt},
                *(self.messages[-30:])
            ],
            "tools": TOOLS,
        }
        async with self.session.post(self.urls[model], json=body) as resp:
            data = await resp.json()
        if "choices" not in data:
            print(">>> ERROR: no choices in response:", data)
        msg = data["choices"][0]["message"]
        if msg.get("tool_calls"):
            for tc in msg["tool_calls"]:
                result = await self.conn.call_tool(
                    tc["function"]["name"],
                    json.loads(tc["function"]["arguments"]),
                )
                self.append({
                    "role": "tool",
                    "tool_call_id": tc["id"],
                    "content": result.to_string(),
                })
                # loop until no more tool calls
                return await self.assistant()
        else:
            if '{"' in msg["content"]:
                msg["content"] = "*** FORBIDDEN: JSON NOT ALLOWED ***"
            self.append(msg)
            return
    
    async def user(self) -> None:
        model = "qwen2.5:14b"
        body: Dict[str, Any] = {
            "model": model,
            "messages": [
                {"role": "system", "content": self.user_prompt},
                *for_user(self.messages[-20:])
            ],
        }
        async with self.session.post(self.urls[model], json=body) as resp:
            data = await resp.json()
        # print(">>>>>>>", data)
        msg = data["choices"][0]["message"]
        msg["role"] = "user"
        if '{"' in msg["content"]:
            msg["content"] = "*** FORBIDDEN: JSON NOT ALLOWED ***"
        self.append(msg)

def for_user(messages: List[Any]) -> Generator[Any, None, None]:
    """ Swap roles and convert tool results to text for the user model """
    for msg in messages:
        if msg["role"] == "user":
            yield {"role": "assistant", "content": msg["content"]}
        elif msg["role"] == "assistant":
            yield {"role": "user", "content": msg["content"]}
        elif msg["role"] == "tool":
            # pretty print tool results for the user model
            rect_str = ToolCallResult.from_string(msg["content"]).pretty_print()
            if rect_str:
                yield {"role": "user", "content": rect_str}
