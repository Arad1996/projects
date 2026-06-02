from pathlib import Path

from dotenv import load_dotenv
from openai import OpenAI
import gradio as gr
import json
import os
import requests
import sqlite3
import pandas as pd

load_dotenv(override=True)

APP_DIR = Path(__file__).resolve().parent
DATA_DIR = APP_DIR / "data"
DB_PATH = APP_DIR / "knicks_coach.db"
MODEL = "gpt-4o-mini"

openai = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))

PUSHOVER_USER = os.getenv("PUSHOVER_USER")
PUSHOVER_TOKEN = os.getenv("PUSHOVER_TOKEN")


def push(message: str) -> None:
    print(f"Push: {message}", flush=True)
    if not PUSHOVER_USER or not PUSHOVER_TOKEN:
        print("Pushover credentials not set; skipping notification.", flush=True)
        return
    requests.post(
        "https://api.pushover.net/1/messages.json",
        data={"user": PUSHOVER_USER, "token": PUSHOVER_TOKEN, "message": message},
        timeout=10,
    )


def record_unknown_question(question: str) -> dict:
    push(f"Unknown basketball question:\n{question}")
    return {"recorded": "ok"}


def _clean_columns(df: pd.DataFrame) -> pd.DataFrame:
    df.columns = [
        c.strip().lower()
        .replace(" ", "_")
        .replace("%", "pct")
        .replace("-", "_")
        .replace("(", "")
        .replace(")", "")
        .replace("/", "_")
        for c in df.columns
    ]
    return df


def create_database() -> None:
    conn = sqlite3.connect(DB_PATH)
    for table, csv_name in (
        ("traditional_stats", "traditional_stats_q1.csv"),
        ("advanced_stats", "advanced_stats_q1.csv"),
        ("matchups", "prev_matchups.csv"),
    ):
        df = _clean_columns(pd.read_csv(DATA_DIR / csv_name))
        df.to_sql(table, conn, if_exists="replace", index=False)
    conn.commit()
    conn.close()
    print("Database created successfully", flush=True)


if not DB_PATH.exists():
    create_database()


def _fetch_table(table: str, result_key: str) -> dict:
    conn = sqlite3.connect(DB_PATH)
    try:
        df = pd.read_sql_query(f"SELECT * FROM {table}", conn)
    finally:
        conn.close()
    return {result_key: df.to_dict("records")}


def get_traditional_stats_q1() -> dict:
    return _fetch_table("traditional_stats", "traditional_stats")


def get_advanced_stats_q1() -> dict:
    return _fetch_table("advanced_stats", "advanced_stats")


def get_matchup_history() -> dict:
    return _fetch_table("matchups", "matchups")


def _tool_schema(name: str, description: str, *, properties=None, required=None) -> dict:
    return {
        "name": name,
        "description": description.strip(),
        "parameters": {
            "type": "object",
            "properties": properties or {},
            "required": required or [],
            "additionalProperties": False,
        },
    }


tools = [
    {
        "type": "function",
        "function": _tool_schema(
            "get_traditional_stats_q1",
            """
            Q1 traditional box-score stats per Knicks player.
            Use for lineup, rotation, scoring, and foul-trouble questions.
            """,
        ),
    },
    {
        "type": "function",
        "function": _tool_schema(
            "get_advanced_stats_q1",
            """
            Q1 advanced metrics: ratings, usage, rebound rates, efficiency.
            Use for impact, spacing, and two-way fit questions.
            """,
        ),
    },
    {
        "type": "function",
        "function": _tool_schema(
            "get_matchup_history",
            """
            Historical head-to-head matchup data for Knicks players vs opponents.
            Use for matchup and defensive assignment questions.
            """,
        ),
    },
    {
        "type": "function",
        "function": _tool_schema(
            "record_unknown_question",
            "Use when the question cannot be answered from available stats or matchups.",
            properties={"question": {"type": "string"}},
            required=["question"],
        ),
    },
]

system_prompt = """
You are an NBA assistant coach for the New York Knicks.

Your job is to help the coaching staff make decisions for the beginning of Q2.

You work like a real assistant coach: investigate the situation with data
before recommending lineups or strategy. Do not answer from memory or guess.

Available tools:
- get_traditional_stats_q1 — Q1 box-score stats
- get_advanced_stats_q1 — Q1 advanced impact metrics
- get_matchup_history — historical player matchup data
- record_unknown_question — when the data cannot answer the question

Follow this process for every coaching question:

1. Understand the coaching question (lineup, matchup, offense, defense, rotation, etc.).
2. Decide what evidence you need, then call the appropriate tool(s).
3. Analyze the returned data before recommending anything.
4. If you still lack evidence, call another tool before answering.
5. Make a clear Q2 coaching recommendation.
6. Explain your reasoning with specific numbers and player names from the data.

You may call multiple tools in sequence across several turns. Gather evidence first;
only give your final recommendation once you have enough data to support it.

Rules:
- Do not invent statistics or player names.
- Cite the data that supports each recommendation.
- If a player is not in the tool results, or the question needs data you do not have,
  call record_unknown_question with the user's question, then explain what is missing.
"""

TOOLS_MAP = {
    "get_traditional_stats_q1": get_traditional_stats_q1,
    "get_advanced_stats_q1": get_advanced_stats_q1,
    "get_matchup_history": get_matchup_history,
    "record_unknown_question": record_unknown_question,
}


def handle_tool_calls(tool_calls):
    results = []
    for tool_call in tool_calls:
        name = tool_call.function.name
        args = json.loads(tool_call.function.arguments)
        print(f"Tool called: {name}", flush=True)
        fn = TOOLS_MAP.get(name)
        results.append(
            {
                "role": "tool",
                "content": json.dumps(fn(**args) if fn else {}),
                "tool_call_id": tool_call.id,
            }
        )
    return results


def run_agent_loop(messages):
    while True:
        response = openai.chat.completions.create(
            model=MODEL, messages=messages, tools=tools
        )
        if response.choices[0].finish_reason == "tool_calls":
            message = response.choices[0].message
            messages.append(message)
            messages.extend(handle_tool_calls(message.tool_calls))
        else:
            return messages


def chat(message, history):
    messages = [{"role": "system", "content": system_prompt}] + history + [
        {"role": "user", "content": message}
    ]
    messages = run_agent_loop(messages)

    stream = openai.chat.completions.create(model=MODEL, messages=messages, stream=True)
    result = ""
    for chunk in stream:
        result += chunk.choices[0].delta.content or ""
        yield result


demo = gr.ChatInterface(
    fn=chat,
    type="messages",
    title="🏀 Knicks Q2 Coaching Assistant",
    description="""
    AI assistant coach for Q2 lineup and strategy decisions.

    Investigates with tools before recommending:
    • Q1 traditional stats • Q1 advanced metrics • Historical matchups

    Ask about lineups, matchups, offense, defense, or rotations.
    """,
)

if __name__ == "__main__":
    demo.launch()
