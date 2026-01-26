---
name: release-notes-generator
description: Generates release notes in the ORE Studio format.
license: Complete terms in LICENSE.txt
---

This skill generates release notes from a ORE Sprint Backlog file.


# When to Use This Skill

Use this skill whenever the user requests the generation of release notes for a given sprint backlog.


# How to Use This Skill

1.  Locate the sprint backlog for which to generate release notes. These are in folder `doc/agile/v0/`. The latest backlog is the one with the highest sprint number.
2.  Read the sprint backlog and generate a summary following the instructions in section Detailed Instructions.
3.  Generate the notes in markdown format and place them in a org-mode markdown block.
4.  Place the resulting summary in a new section called "AI Generated Sprint Summary".


# Detailed Instructions

Follow the following template for the release notes, replacing with actual content from sprint backlog.

```markdown
# **ProjectName Sprint N – Release Notes**
*Month Year*

A brief 1–2 sentence summary of the sprint’s primary focus and achievements.
---

## ✅ **Highlights**

- Use bullet points to list **3–5 major accomplishments**.
- Focus on high-impact features, architectural decisions, or process improvements.
- Keep each item short and outcome-oriented.

## 🛠️ **Key Improvements**

Organize improvements into **thematic sections** (e.g., Architecture, UI, Testing, Infrastructure).

- Use bold section headers (e.g., `### **Authentication & Security**`).
- Under each, list specific changes as bullet points.
- Include technical details where relevant (e.g., libraries used, patterns applied).
- Group related stories logically; avoid listing every task.

## ⚠️ **Known Issues & Postponed**

- List **BLOCKED**, **POSTPONED**, or unresolved issues.
- For each, provide a **short reason** or status (e.g., "Deferred to next sprint", "Waiting on upstream fix").
- Do not include resolved or cancelled items.

## 📊 **Time Summary**

- Show total effort: `**Total effort**: Xh Ym`
- Show effort distribution: `**Code**: A% | **Infra**: B% | **Agile/Analysis/Doc**: C%`
- List top time-consuming tasks with durations (e.g., `Top tasks: Task A (8h), Task B (6h)`)

---

*Next sprint: One sentence summarizing the upcoming sprint mission or focus.*

---
```

Requirements:

-   Use clean Markdown formatting with consistent emoji (✅ 🛠️ ⚠️ 📊).
-   Keep the entire document **under 1 page** in length.
-   Use **bold** for emphasis and \`code\` for filenames, commands, or libraries.
-   Do not invent details — only include information present in the input.
-   Match the tone: professional, factual, and developer-focused.
-   Do not include headers, footers, or metadata outside the template.
-   Replace `ProjectName`, `Sprint N`, `Month Year`, and all content with actual values from the sprint data.
