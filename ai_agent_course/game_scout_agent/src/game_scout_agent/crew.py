from crewai import Agent, Crew, Process, Task
from crewai.project import CrewBase, agent, crew, task
from crewai.agents.agent_builder.base_agent import BaseAgent
from crewai_tools import SerperDevTool



@CrewBase
class GameScoutAgent():
    """GameScoutAgent crew"""

    agents: list[BaseAgent]
    tasks: list[Task]

    @agent
    def news_researcher(self) -> Agent:
        return Agent(
            config=self.agents_config['news_researcher'],
            tools=[SerperDevTool()],
            allow_delegation=False,
            verbose=True
        )

    @agent
    def stats_analyst(self) -> Agent:
        return Agent(
            config=self.agents_config['stats_analyst'],
            verbose=True
        )

    @agent
    def game_scout(self) -> Agent:
        return Agent(
            config=self.agents_config['game_scout'],
            verbose=True
        )

    @task
    def news_research_task(self) -> Task:
        return Task(
            config=self.tasks_config['news_research_task'],
        )

    @task
    def stats_analysis_task(self) -> Task:
        return Task(
            config=self.tasks_config['stats_analysis_task'],
        )

    @task
    def game_scout_report_task(self) -> Task:
        return Task(
            config=self.tasks_config['game_scout_report_task'],
            context=[
                self.news_research_task(),
                self.stats_analysis_task()
            ],
            output_file='scouting_report.md'
        )

    @crew
    def crew(self) -> Crew:
        return Crew(
            agents=self.agents,
            tasks=self.tasks,
            process=Process.sequential,
            verbose=True,
        )