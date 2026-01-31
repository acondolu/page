import json
import os
from typing import Any, Generator, List, Dict, Optional
import aiohttp
from .client import Client, ToolCallResult
from .roles import pick_role, PROMPTS, ROLES

TOOLS: List[Dict[str, Any]] = [
    {
      "type": "function",
      "function": {
        "name": "read",
        "description": "Reads a rectangular block of text from the grid.",
        "parameters": {
          "type": "object",
          "properties": {
            "x": {
              "type": "integer",
              "description": "starting x-coordinate"
            },
            "y": {
              "type": "integer",
              "description": "starting y-coordinate"
            },
            "w": {
              "type": "integer",
              "description": "width of the region to read"
            },
            "h": {
              "type": "integer",
              "description": "height of the region to read"
            }
          },
          "required": ["x", "y", "w", "h"]
        }
      },
    },
    {
        "type": "function",
        "function": {
            "name": "write",
            "description": "Overwrites text on the grid starting at the given coordinates.",
            "parameters": {
                "type": "object",
                "properties": {
                    "x": {
                        "type": "integer",
                        "description": "starting x-coordinate"
                    },
                    "y": {
                        "type": "integer",
                        "description": "starting y-coordinate"
                    },
                    "text": {
                        "type": "string",
                        "description": "text to write; may include newlines",
                        "maxLength": 64,
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
                "description": "Immediately terminates the current interaction.",
            }
        }
]

class Agent:
    def __init__(self, llms: Dict[str, Any], conn: Client, role: Optional[str] = None):
        self.llms = llms
        self.session = aiohttp.ClientSession()
        self.conn = conn
        self.system_prompt = PROMPTS["assistant"]
        self.user_role = role if role else pick_role()
        if self.user_role not in ROLES:
            raise ValueError(f"Invalid role: {self.user_role}")
        self.user_prompt = PROMPTS[self.user_role]
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
        elif content:
            print(f">>> {content}")
        self.messages.append(js)
    
    async def run(self) -> None:
        """ One turn of interaction: assistant -> user """
        await self.user()
        await self.assistant()

    async def assistant(self) -> None:
        llm = self.llms["assistant"]
        model = llm["model"]
        url = llm["url"]
        body: Dict[str, Any] = {
            "model": model,
            "messages": [
                {"role": "system", "content": self.system_prompt},
                *take_only(self.messages, 30)
            ],
            "tools": TOOLS,
        }
        headers = {}
        if "api_key_env" in llm:
            api_key = os.getenv(llm["api_key_env"])
            if api_key:
                headers["Authorization"] = f"Bearer {api_key}"
        async with self.session.post(url, json=body, headers=headers) as resp:
            data = await resp.json()
        if "choices" not in data:
            print(">>> ERROR: no choices in response:", data)
        msg = data["choices"][0]["message"]
        if msg.get("tool_calls"):
            self.append(msg)
            for tc in msg["tool_calls"]:
                function_name = tc["function"]["name"]
                arguments = json.loads(tc["function"]["arguments"])
                result = await self.conn.call_tool(function_name, arguments)
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
        llm = self.llms["user"]
        model = llm["model"]
        url = llm["url"]
        body: Dict[str, Any] = {
            "model": model,
            "messages": [
                {"role": "system", "content": self.user_prompt},
                *take_only(list(for_user(self.messages)), 20)
            ],
        }
        headers = {}
        if "api_key_env" in llm:
            api_key = os.getenv(llm["api_key_env"])
            if api_key:
                headers["Authorization"] = f"Bearer {api_key}"
        async with self.session.post(url, json=body, headers=headers) as resp:
            data = await resp.json()
        if "choices" not in data:
            print(">>> ERROR: no choices in response:", data)
        msg = data["choices"][0]["message"]
        msg["role"] = "user"
        if '{"' in msg["content"]:
            msg["content"] = "*** FORBIDDEN: JSON NOT ALLOWED ***"
        self.append(msg)

def for_user(messages: List[Any]) -> Generator[Any, None, None]:
    """ Swap roles and convert tool results to text for the user model """
    for msg in messages:
        role = msg["role"]
        content = msg["content"]
        if not content: continue
        if role == "user":
            yield {"role": "assistant", "content": content}
        elif role == "assistant":
            yield {"role": "user", "content": content}
        elif role == "tool":
            # pretty print tool results for the user model
            rect_str = ToolCallResult.from_string(content).pretty_print()
            if rect_str:
                yield {"role": "user", "content": rect_str}

def take_only(messages: List[Any], n: int) -> List[Any]:
    """ Take only the last n messages, preserving tool results with their calls """
    result: List[Any] = []
    count = 0
    for msg in reversed(messages):
        result.append(msg)
        if count >= n and msg["role"] != "tool":
            break
        if msg["role"] != "tool":
            count += 1
    result.reverse()
    return result
