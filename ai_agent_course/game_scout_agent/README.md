# Game Scout Agent 🏀

A simple multi-agent sports analysis project built with **CrewAI**.

## What It Does

Given a matchup (e.g., **Lakers vs Warriors**), the system:

1. Researches recent news and updates.
2. Analyzes relevant team/player statistics.
3. Generates a scouting report with key insights.

## Agents

* **News Researcher** – Finds the latest news and team updates.
* **Stats Analyst** – Reviews statistics and performance trends.
* **Game Scout** – Combines research and analysis into a final report.

## Tech Stack

* Python
* CrewAI
* SerperDevTool

## Output

The final scouting report is saved as:

```text
scouting_report.md
```

## Run

```bash
crewai run
```

This project is a simple example of using CrewAI to coordinate multiple AI agents for sports research and analysis.
