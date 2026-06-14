from agents import Agent, WebSearchTool, ModelSettings

INSTRUCTIONS = """
You are an NBA research analyst.

Search the web and summarize only factual information from one the following. Get information from only one of the websites

Prioritize:
- NBA.com
- Basketball Reference
- ESPN
- The Athletic
- StatMuse
- Yahoo Sports

Focus on:

- Statistics
- Skills
- Strengths
- Weaknesses

Avoid speculation.

Output concise research notes.
Maximum 300 words.
Keep your answer short and accurate.
"""

search_agent = Agent(
    name="Search agent",
    instructions=INSTRUCTIONS,
    tools=[WebSearchTool(search_context_size="low")],
    model="gpt-4o-mini",
    model_settings=ModelSettings(tool_choice="required"),
)