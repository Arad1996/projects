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

openai = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))

pushover_user = os.getenv("PUSHOVER_USER")
pushover_token = os.getenv("PUSHOVER_TOKEN")
pushover_url = "https://api.pushover.net/1/messages.json"


def push(message):
    print(f"Push: {message}", flush=True)
    if not pushover_user or not pushover_token:
        print("Pushover credentials not set; skipping notification.", flush=True)
        return
    payload = {"user": pushover_user, "token": pushover_token, "message": message}
    requests.post(pushover_url, data=payload, timeout=10)


def record_unknown_question(question):
    push(f"Unknown basketball question:\n{question}")
    return {"recorded": "ok"}


def _clean_columns(df):
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


def create_database():
    conn = sqlite3.connect(DB_PATH)

    traditional_df = _clean_columns(pd.read_csv(DATA_DIR / "traditional_stats_q1.csv"))
    advanced_df = _clean_columns(pd.read_csv(DATA_DIR / "advanced_stats_q1.csv"))
    matchups_df = _clean_columns(pd.read_csv(DATA_DIR / "prev_matchups.csv"))

    traditional_df.to_sql("traditional_stats", conn, if_exists="replace", index=False)
    advanced_df.to_sql("advanced_stats", conn, if_exists="replace", index=False)
    matchups_df.to_sql("matchups", conn, if_exists="replace", index=False)

    conn.commit()
    conn.close()
    print("Database created successfully", flush=True)


if not DB_PATH.exists():
    create_database()


def get_connection():
    return sqlite3.connect(DB_PATH)


def get_game_context():
    conn = get_connection()

    traditional = pd.read_sql_query("SELECT * FROM traditional_stats", conn)
    advanced = pd.read_sql_query("SELECT * FROM advanced_stats", conn)
    matchups = pd.read_sql_query("SELECT * FROM matchups", conn)

    conn.close()

    return {
        "traditional_stats": traditional.to_dict("records"),
        "advanced_stats": advanced.to_dict("records"),
        "matchups": matchups.to_dict("records"),
    }


record_unknown_question_json = {
    "name": "record_unknown_question",
    "description": """
    Use this tool whenever a question
    cannot be answered using the
    available statistics and matchup data.
    """,
    "parameters": {
        "type": "object",
        "properties": {"question": {"type": "string"}},
        "required": ["question"],
        "additionalProperties": False,
    },
}

get_game_context_json = {
    "name": "get_game_context",
    "description": """
    Retrieve all available game information.

    Includes:
    - Q1 traditional stats
    - Q1 advanced stats
    - historical matchup data

    Use this information to recommend
    lineups and strategies for the
    beginning of Q2.
    """,
    "parameters": {
        "type": "object",
        "properties": {},
        "required": [],
        "additionalProperties": False,
    },
}

tools = [
    {"type": "function", "function": get_game_context_json},
    {"type": "function", "function": record_unknown_question_json},
]

system_prompt = """
You are an NBA assistant coach for the New York Knicks.

Your job is to help the coaching staff
make decisions for the beginning of Q2.

You have access to:

1. Traditional Q1 player statistics
2. Advanced Q1 player statistics
3. Historical matchup data

When you need data,
use the get_game_context tool.

Your responsibilities include:

- Recommending the best lineup for Q2
- Identifying favorable matchups
- Suggesting offensive adjustments
- Suggesting defensive adjustments
- Recommending player substitutions
- Answering questions about the game

Always support your recommendations
with data from the tool.

If a question cannot be answered
using the available Q1 statistics,
advanced statistics,
or matchup information,
use the record_unknown_question tool.

Do not invent data.
Do not guess.
If you don't find the player name in the data, use the record_unknown_question tool.
"""

TOOLS_MAP = {
    "get_game_context": get_game_context,
    "record_unknown_question": record_unknown_question,
}


def handle_tool_calls(tool_calls):
    results = []
    for tool_call in tool_calls:
        tool_name = tool_call.function.name
        arguments = json.loads(tool_call.function.arguments)
        print(f"Tool called: {tool_name}", flush=True)
        tool = TOOLS_MAP.get(tool_name)
        result = tool(**arguments) if tool else {}
        results.append(
            {
                "role": "tool",
                "content": json.dumps(result),
                "tool_call_id": tool_call.id,
            }
        )
    return results


def chat(message, history):
    messages = [{"role": "system", "content": system_prompt}] + history + [
        {"role": "user", "content": message}
    ]

    while True:
        response = openai.chat.completions.create(
            model="gpt-4o-mini", messages=messages, tools=tools
        )
        finish_reason = response.choices[0].finish_reason

        if finish_reason == "tool_calls":
            assistant_message = response.choices[0].message
            results = handle_tool_calls(assistant_message.tool_calls)
            messages.append(assistant_message)
            messages.extend(results)
        else:
            break

    stream = openai.chat.completions.create(
        model="gpt-4o-mini", messages=messages, stream=True
    )
    result = ""
    for chunk in stream:
        result += chunk.choices[0].delta.content or ""
        yield result


demo = gr.ChatInterface(
    fn=chat,
    type="messages",
    title="🏀 Knicks Q2 Coaching Assistant",
    description="""
    AI assistant for lineup and strategy decisions.

    Uses:
    • Q1 Traditional Stats
    • Q1 Advanced Stats
    • Historical Matchups

    Ask questions about:
    • Best Q2 lineup
    • Matchup advantages
    • Offensive strategy
    • Defensive adjustments
    • Player rotations
    """,
)

if __name__ == "__main__":
    demo.launch()
