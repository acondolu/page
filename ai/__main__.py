import asyncio
import sys
from typing import Optional
from .client import Client
from .agent import Agent

CLIENT_URL = "ws://localhost:8765"
# LLMS = {
#     "assistant": {
#         "model": "llama3.1:latest",
#         "url": "http://192.168.1.176:11434/v1/chat/completions",
#         "api_key_env": "LLAMA_API_KEY",
#     },
#     "user": {
#         "model": "qwen2.5:14b",
#         "url": "http://192.168.1.176:11435/v1/chat/completions",
#         "api_key_env": "LLAMA_API_KEY",
#     },
# }
LLMS = {
    "assistant": {
        "model": "gpt-5.1", # "gpt-5.2-2025-12-11",
        "url": "https://api.openai.com/v1/chat/completions",
        "api_key_env": "OPENAI_API_KEY",
    },
    "user": {
        "model": "gpt-4.1", # "gpt-5.2-2025-12-11",
        "url": "https://api.openai.com/v1/chat/completions",
        "api_key_env": "OPENAI_API_KEY",
    },
}

async def main():
    role: Optional[str] = None
    if len(sys.argv) > 1:
        role = sys.argv[1]
    client = Client(CLIENT_URL)
    await client.connect()
    agent = Agent(llms=LLMS, conn=client, role=role)
    while True:
        await agent.run()

if __name__ == "__main__":
    asyncio.run(main())
