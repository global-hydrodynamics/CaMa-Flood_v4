# Architecture decisions

Use one `ADR-####-slug.md` file per significant technical or research-process decision. Assign ADR IDs globally across both `research/decisions/` and `research/records/decisions/`; do not restart numbering in either directory. Check existing headings and run `python3 research/validate_records.py --next-ids` before choosing an ID. Start from [`research/templates/adr.md`](../../templates/adr.md). Mark proposals as proposed until the maintainer confirms them; include alternatives, rationale, consequences, and evidence. Link the ADR from `events.csv`.
