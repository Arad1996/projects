from pydantic import BaseModel, Field
from agents import Agent

HOW_MANY_SEARCHES = 5

INSTRUCTIONS = f"""
You are an NBA player development planner.

Given a player and a development question,
create 5 web searches that help understand:

1. Current strengths
2. Current weaknesses
3. Recent performance
4. Skill development opportunities
5. Comparable players who improved similarly

Keep it a short answer but accurate

Generate exactly {HOW_MANY_SEARCHES} searches.
"""


class WebSearchItem(BaseModel):
    reason: str = Field(description="Your reasoning for why this search is important to the query.")
    query: str = Field(description="The search term to use for the web search.")


class WebSearchPlan(BaseModel):
    searches: list[WebSearchItem] = Field(description="A list of web searches to perform to best answer the query.")
    
planner_agent = Agent(
    name="PlannerAgent",
    instructions=INSTRUCTIONS,
    model="gpt-4o-mini",
    output_type=WebSearchPlan,
)