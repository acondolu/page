import asyncio
from .client import Client
from .agent import Agent

CLIENT_URL = "ws://localhost:8765"
COMPLETIONS_URLS = {
    "llama3.1:latest": "http://192.168.1.176:11434/v1/chat/completions",
    "qwen2.5:14b": "http://192.168.1.176:11435/v1/chat/completions",
}

async def main():
    client = Client(CLIENT_URL)
    await client.connect()
    agent = Agent(urls=COMPLETIONS_URLS, conn=client)
    while True:
        await agent.run()

if __name__ == "__main__":
    asyncio.run(main())
