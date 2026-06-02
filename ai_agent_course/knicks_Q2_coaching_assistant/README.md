---
title: knicks_q2_coach
app_file: app.py
sdk: gradio
sdk_version: 5.49.1
---

# Knicks Q2 Coaching Assistant 

# Try it out! https://huggingface.co/spaces/Aradm1996/knicks_q2_coach

AI assistant for New York Knicks lineup and strategy decisions at the start of Q2.

Uses an agent loop: the model calls Q1 traditional stats, advanced stats, and matchup tools as needed before recommending.

- **`app.py`** — full app (deploy this folder to Hugging Face)
- **`knicks_assistant_coach.ipynb`** — local launcher only; imports from `app.py`
- **`data/`** — CSV source files; SQLite DB is built automatically if missing
