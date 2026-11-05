# New Internship Assignment
## Automated Tutorial Generation for CaMa-Flood v5

### 1. Background and objective

Thank you for completing the introductory CaMa-Flood manual!

For your next assignment, I would like you to try something different: **develop a method to generate a CaMa-Flood tutorial automatically, rather than writing and updating the entire manual by hand.**

When we update CaMa-Flood, the file locations, model settings, and output files may change. Maintaining screenshots, file listings, and command examples manually requires considerable effort and can introduce outdated information.

Our goal is to develop a documentation system that collects information directly from the actual model, input datasets, configuration files, and simulation results.

This is an experimental assignment. I do not have a fully established solution in mind, so I would like you to explore possible approaches with the help of an AI coding assistant.

### 2. Starting point

I will prepare the following directory in the CaMa-Flood GitHub repository:

```text
proj_sample/
└── test-mek01/
    ├── mek01-default_run.sh
    └── Expected_Manual.md
```

The shell script runs a short, approximately 40-day CaMa-Flood simulation for the Mekong River Basin.

`Expected_Manual.md` illustrates the tutorial we would eventually like to produce. It is a reference for the desired content, not a document you need to reproduce word for word.

### 3. Learn the development environment

Before starting the implementation, please become familiar with the following tools.

**Git and GitHub**

- Install Git and learn how repositories work.
- Clone the CaMa-Flood repository to your PC.
- Learn the differences between clone, fetch, pull, commit, push, and branch.
- Create a local working branch, such as `clara-manual-dev`.
- Make small commits as you progress.
- Learn how to inspect file changes before accepting modifications.

For now, please work only on your local branch. Do not push changes or create Pull Requests unless I specifically ask you to.

Since you are using Windows with WSL/Ubuntu, I recommend keeping a single repository inside the WSL filesystem. You can access it through VS Code with WSL support.

**Markdown**

Learn the basics of Markdown (`.md` files), including headings, tables, code blocks, links, and images. Practice previewing Markdown files in VS Code.

**AI-assisted coding**

Set up either Claude Code or Codex so that it can access your local CaMa-Flood repository.

Learn how to ask the AI assistant to inspect files, explain existing code, propose changes, and implement scripts.

An important part of this assignment is learning to review and test AI-generated changes rather than accepting them automatically.

### 4. Understand the example simulation

Start by reading:

- `proj_sample/test-mek01/mek01-default_run.sh`
- `proj_sample/test-mek01/Expected_Manual.md`
- Relevant README files in the repository

Then investigate how the simulation works.

Find the source code and compilation instructions, the Mekong river map, the runoff forcing, and the configuration settings.

Run the example simulation and check the generated output files and execution logs.

You do not need to change the scientific model or develop new physical processes.

### 5. Develop an automated tutorial generator

The main challenge is to find a simple way to produce a Markdown tutorial from actual files and simulation results.

The tutorial should explain:

1. **Source code:** Where it is located, how to compile it, and how to confirm successful compilation.
2. **River map:** Where the map files are stored, which files are important, and what they contain.
3. **Runoff forcing:** Where the input data is stored, its format, and how the model reads it.
4. **Experiment configuration:** How the shell script specifies the simulation period, input data, experiment name, and output variables.
5. **Model execution:** How to run the script and confirm successful completion.
6. **Output files:** Where the results are saved, which files are generated, and how to check them.
7. **Simple exercises:** How changing the experiment name changes the output location, and how changing the output variable settings changes the saved files.

Where practical, file listings, configuration excerpts, logs, and output summaries should be collected automatically instead of copied manually.

You may write introductory explanations and scientific descriptions manually.

**The goal is not to eliminate all manual writing. It is to eliminate unnecessary manual updates.**

### 6. Suggested implementation approach

Please discuss the design with Claude Code or Codex before implementing anything.

A simple solution might use:

- A Markdown template containing explanatory text
- A Python or shell script to inspect the actual files
- A generator that inserts collected information into the template
- A generated Markdown manual

However, this is only a suggestion. You are welcome to propose a better or simpler approach.

Avoid introducing complicated frameworks unless they are clearly necessary.

The model execution and the documentation generation should preferably remain separate, so the model does not need to run every time the manual is updated.

### 7. Expected deliverables

By the end of the assignment, I would like to see:

- A Markdown tutorial based on the actual `test-mek01` experiment
- A reproducible method to regenerate its technical content
- The scripts needed for documentation generation
- A short README explaining how to use the generator
- A description of which parts are automatic and which are manually maintained
- A record of any difficulties, limitations, or improvements you identified

Initially, you do not need to generate scientific figures, create a PDF, develop CI tests, or integrate the system into the official CaMa-Flood distribution.

We will consider these in a later development stage.

### 8. Working rules

Please follow these rules during development:

- Work primarily inside `proj_sample/test-mek01/`.
- Do not modify the CaMa-Flood source code or shared map/runoff datasets without discussing it with me.
- Do not push changes to GitHub or create Pull Requests yet.
- Use AI assistance freely, but review the changes it makes.
- Keep your work in small, understandable commits.
- Test the commands and scripts yourself.
- Do not invent example results. Clearly distinguish actual results from expected or illustrative examples.

**Your first task is to investigate the repository and propose a simple implementation plan. Please share the plan with me before beginning major modifications.**

The main objective is to create a working proof of concept and learn how AI-assisted software development can improve the reproducibility and maintainability of scientific models.