# test_agent.py

from crewai import Agent
from crewai_tools import SerperDevTool

agent = Agent(
    role="Researcher",
    goal="Find NBA news",
    backstory="Sports journalist",
    tools=[SerperDevTool()],
    verbose=True
)

response = agent.kickoff(
    "Find the latest Lakers injury news"
)

print(response)