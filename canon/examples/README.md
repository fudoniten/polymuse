# Polymuse Examples

This directory contains example files demonstrating Polymuse's canon integration and tool system.

## Example Canon Files

### story-canon.org

Demonstrates using canon for prose writing:
- Character tracking (Sarah Chen, Marcus Reynolds)
- Location descriptions (CyberGuard HQ, The Cipher)
- Story events and technology
- Properties for searching (Age, Role, City, etc.)

**Try it:**
1. Open a new text-mode buffer for your story
2. Enable canon-mode: `M-x canon-mode`
3. Set the canon file to `story-canon.org`
4. Enable polymuse-mode: `M-x polymuse-mode`
5. Add a reviewer: `M-x polymuse-add-reviewer`
6. View active tools: `M-x polymuse-show-active-tools`

The LLM will automatically have access to:
- `canon-lookup-entity`: Look up character/location details
- `canon-list-entities`: See all available entities
- `canon-search-by-property`: Find characters by role, age, etc.
- `canon-suggest-update`: Suggest character/location modifications

**Example workflow:**
Write a scene mentioning Sarah Chen. When Polymuse reviews your writing, it can:
- Look up Sarah's description and personality traits
- Ensure consistency with established canon
- Suggest updates if the story develops her character

### project-canon.org

Demonstrates using canon for code projects:
- Architecture documentation (system-overview, tool-system)
- Style guidelines (elisp-conventions, commit-conventions)
- Troubleshooting guides (common-issues)

**Try it:**
1. Open a code file in the Polymuse project
2. Enable canon-mode: `M-x canon-mode`
3. Set the canon file to `project-canon.org`
4. Enable polymuse-mode: `M-x polymuse-mode`
5. Add a reviewer: `M-x polymuse-add-reviewer`
6. View active tools: `M-x polymuse-show-active-tools`

The LLM will automatically have access to:
- `canon-lookup-doc`: Look up architecture docs
- `canon-list-docs`: See all available documentation
- `canon-suggest-doc-update`: Suggest improvements to docs

**Example workflow:**
Write a new feature. When Polymuse reviews your code, it can:
- Check against established architecture patterns
- Ensure compliance with style guidelines
- Reference troubleshooting docs for common issues
- Suggest doc updates if the architecture has changed

## Tool Profiles

Polymuse automatically selects the appropriate profile based on your mode:

- **prose-writing**: Active in text-mode with canon-mode
  - Tools: character/location lookup, entity search, suggest updates

- **code-review**: Active in prog-mode with canon-mode
  - Tools: architecture lookup, doc search, suggest doc improvements

- **architecture**: For high-level design work
  - Can be activated with: `M-x polymuse-switch-profile` → `architecture`

- **debugging**: For troubleshooting sessions
  - Can be activated with: `M-x polymuse-switch-profile` → `debugging`

## Safety Features

### Read-Only Lookups
All lookup tools (`canon-lookup-entity`, `canon-list-entities`, etc.) are read-only. They never modify your canon file.

### Append-Only Suggestions
Suggestion tools (`canon-suggest-update`, `canon-suggest-doc-update`) append to a "Suggestions" section. They never overwrite existing content.

**Example:** If the LLM calls `canon-suggest-update("sarah-chen", "Consider adding more about her technical skills")`, it creates:

```org
** [Characters] Sarah Chen
...existing content...
*** Suggestions
**** Suggestion (2025-01-09 14:30:00)
Consider adding more about her technical skills
```

You can review, edit, and manually apply these suggestions.

## Creating Your Own Canon

1. Create a new `.org` file
2. Add top-level headings for types (e.g., "Characters", "Locations", "Architecture")
3. Add entities as second-level headings with `[Type] name` format
4. Give each entity an `:ID:` property
5. Add subheadings for details (Description, Appearance, Technical Details, etc.)

**Example structure:**
```org
* Characters

** [Characters] protagonist-name
:PROPERTIES:
:ID: protagonist-id
:END:

*** Description
Character description here...

*** Personality
Personality traits here...

* Locations

** [Locations] main-city
:PROPERTIES:
:ID: main-city-id
:END:

*** Description
Location description here...
```

## Advanced: Custom Tools

You can define custom tools in `.polymuse-tools.el`:

```elisp
;; Add a custom tool to lookup character relationships
(push (cons 'canon-character-relationships
            (make-polymuse-tool
             :name "canon-character-relationships"
             :function #'my-custom-relationship-lookup
             :description "Look up relationships between characters"
             :arguments '(character-id)))
      polymuse-local-tools)
```

File-local tools have the highest precedence and override built-in tools.

## Tips

- Use `M-x polymuse-show-active-tools` to see what tools are available
- Use `M-x polymuse-switch-profile` to change tool sets
- Check the `*polymuse-debug*` buffer if tools aren't working
- Suggestions are timestamped so you can track when they were made
- You can have multiple canon files per project (one for story, one for worldbuilding, etc.)
