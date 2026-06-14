from pydantic import BaseModel, Field
from agents import Agent


INSTRUCTIONS = """
You are an elite NBA Player Development Coach.

You will receive:

1. A player development question.
2. Web research summaries about the player.

Your job is to create a realistic development plan.

Use only information found in the research.

Do not invent statistics.

Focus on:

- Current strengths
- Current weaknesses
- Areas for improvement
- Skill development
- Training recommendations
- Future projection

The report should contain:

# Executive Summary

# Player Overview

# Current Strengths

# Current Weaknesses

# Development Priorities

# Recommended Training Plan

# 12 Month Projection

# Long-Term Ceiling

# Final Recommendations
"""


class DevelopmentReport(BaseModel):

    summary: str = Field(
        description="Short summary of the development plan."
    )

    markdown_report: str = Field(
        description="Detailed development report."
    )

    next_focus_areas: list[str] = Field(
        description="Most important skills to improve."
    )


development_writer_agent = Agent(
    name="DevelopmentWriterAgent",
    instructions=INSTRUCTIONS,
    model="gpt-4o-mini",
    output_type=DevelopmentReport,
)