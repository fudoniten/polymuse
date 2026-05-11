# Polymuse — Recommended Fixes

## Progress

- [ ] Fix 1 — Serialize prompt to string (XML default, JSON option)
- [ ] Fix 2 — Split system prompt: prose persona vs. terse code reviewer
- [ ] Fix 3 — Insert `<CURSOR/>` marker inside focus region
- [ ] Fix 4 — Per-mode chars-per-token budget
- [ ] Fix 5 — Gate forward context for code mode
- [ ] Fix 6 — Structured findings format for code mode
- [ ] Fix 7 — Track flagged findings to avoid repetition (code mode)
- [ ] Fix 8 — Switch tool calls to gptel's native tool API
- [ ] Fix 9 — Enable real streaming; remove fake typewriter
- [ ] Fix 10 — Harden sexp unit grabber with size sanity check

---

## Context

Priority is **prose first, code second**. Design defaults should serve prose; the code reviewer should be a separate, terser persona that doesn't inherit prose-oriented framing. Below is the list of substantive fixes (annoyances and nits dropped).

The biggest issue, now confirmed: `gptel-request` is being handed an elisp alist at polymuse.el:991 and is sending its `prin1` representation as the user message. That's the first thing to fix, and it shapes everything else.

---

## Fix 1 — Serialize the prompt to a real string before sending

**Where:** polymuse.el:991 (`gptel-request request ...`) and polymuse.el:1678-1725 (`polymuse--compose-prompt-from-context`).

**Problem:** `polymuse--compose-prompt-from-context` returns an alist. `gptel-request` expects a string. Confirmed via `*polymuse-debug*`: the model receives literal elisp s-expressions including symbol names, dotted pairs, and parens.

**Approach:** Introduce a serializer abstraction so the alist is the internal representation and the wire format is pluggable.

- New custom variable `polymuse-prompt-format` with allowed values `'xml` (default) and `'json`.
- New generic `polymuse--serialize-prompt (alist format)` dispatching on `format`.
- Two concrete implementations:
  - `polymuse--serialize-prompt-xml` — emits readable tag-based layout (see below).
  - `polymuse--serialize-prompt-json` — emits `json-serialize` output, but with the *top-level alist flattened* so the wire form is a single JSON object, not nested `review-request → context → ...` alists where the keys carry no semantic weight.
- Per-backend override: `polymuse-gptel-backend` gets an optional `prompt-format` slot so an Ollama Llama-3 user can pick JSON while a Claude user keeps XML.
- Call site change at polymuse.el:1308: `(gptel-request (polymuse--serialize-prompt (polymuse--compose-prompt review) polymuse-prompt-format) ...)`.

**Recommended default: XML-ish tags.** Claude, GPT-4-class, and Llama-3-class models all handle tag-delimited sections reliably, tokens are cheap, and it reads well in the debug buffer. JSON works too but escaping prose content (quotes, newlines) wastes tokens and obscures the debug log.

Target XML layout:
```
<instructions>
{mode-prompt}

{user instructions}
</instructions>

<context mode="{major_mode}">
<before>
...
</before>
<focus>
... with <CURSOR/> marker ...
</focus>
<after>
...
</after>
</context>

<previous_review>
...
</previous_review>

<task>
{task description, including final-instructions if any}
</task>
```

**Verification:** With `polymuse-debug` on, the *gptel-log* buffer should now show the XML/JSON string verbatim instead of `((review-request . ((mode-prompt . "..."))) ...)`. Add an ERT test that calls the serializer on a known alist and asserts the exact string output.

---

## Fix 2 — Split the system prompt: keep prose persona, give code its own

**Where:** polymuse.el:1067-1082.

**Problem:** A single "creative assistant... share ideas, reactions, possibilities... conversational" system prompt is applied to every call including code review. The prose framing contaminates code feedback (chatty, speculative, easy to ignore).

**Approach:**
- Keep `polymuse-system-prompt` as today and make it the **prose** default — this is the priority persona.
- Add `polymuse-code-system-prompt` (new custom) that is terse and concrete: "You are a careful code reviewer. Flag only real problems: bugs, edge cases, unhandled errors, unclear naming. No praise, no speculation, no style nits. If there is nothing to flag, say so in one line."
- `polymuse--compose-prompt` picks system prompt by `derived-mode-p 'prog-mode'` (the same dispatch already used by `polymuse-get-mode-prompt`).
- Plumbed through to `:system` at polymuse.el:992.

**Design note:** This is the one place where the prose/code split should be explicit at the system-prompt layer, *not* just the mode-prompt layer. Mode prompts are user-overridable instructions; system prompts set persona. Mixing them is the current root cause of code reviews feeling unfocused.

---

## Fix 3 — Insert a cursor marker inside `<focus>`

**Where:** polymuse.el:1727-1751 (`polymuse--compose-prompt`), unit grabbers at polymuse.el:1275-1279.

**Problem:** "Real-time feedback as the user writes" — but the model doesn't know *where* in the focus region the user is. For prose especially, the in-flight sentence is the one worth reacting to.

**Approach:** When extracting the current unit, capture `(point)` relative to the unit's start and splice a sentinel into the string (e.g. `«CURSOR»` or `<CURSOR/>` matched to the chosen serializer). Update mode prompts to mention it: "The user's cursor is at `<CURSOR/>`; feedback should focus on the surrounding sentence/expression." This is high-leverage and cheap — a few lines.

---

## Fix 4 — Replace char-count budgeting with real token counting (or per-language factor)

**Where:** polymuse.el:113-117, polymuse.el:1639-1677 (`polymuse--calculate-context-regions`).

**Problem:** Char-count is the budget; 4-chars-per-token is the assumed ratio. Fine for English prose, wrong for code (~2.5–3) and for languages with non-ASCII content. Real risk: prompt overruns context window.

**Approach (prose-first, low-friction):**
- Keep char-budget but introduce `polymuse-chars-per-token` as a per-mode plist: `((prog-mode . 3) (text-mode . 4))`.
- Optional: a `polymuse--count-tokens` generic with a default heuristic and a hook for users who want to wire in an actual tokenizer (tiktoken subprocess, or whatever gptel exposes).
- Convert the configured **token** budget into a **char** budget at request time using the active mode's ratio. Variable names should switch to `polymuse-max-prompt-tokens` (with a backwards-compat shim if you care — but per CLAUDE-style conventions, just rename).

---

## Fix 5 — Drop or gate forward context for code; keep it generous for prose

**Where:** polymuse.el:1639-1677, polymuse.el:1242-1258.

**Problem:** Forward context (15%) is useful for prose continuity ("does the next paragraph follow from this?") but actively misleading for code mid-edit, where lines after point are often half-written.

**Approach:**
- Prose default: keep 15% forward, possibly bump to 20%.
- Code default: forward = 0 unless the focus region hasn't been edited for N seconds (track via a buffer-local last-edit time; cheap with `after-change-functions`).
- Expose `polymuse-forward-context-ratio` as a per-mode alist analogous to `polymuse-mode-prompts`.

---

## Fix 6 — Make the code reviewer return a structured findings list; leave prose free-form

**Where:** polymuse.el:1077-1082 (mode prompts), new parser for code responses.

**Problem (code only):** The four-bullet checklist + 200-word soft cap means the model picks two random items per review. Across reviews it's not clear what's been flagged.

**Approach (code mode only — prose stays conversational):**
- Code mode prompt asks for at most 3 findings, one per block, in a fixed shape:
  ```
  - [bug|risk|clarity|simplify] <Lxx-Lyy> <one-line summary>
    why: <one sentence>
    suggestion: <terse fix>
  ```
- Pre-number the `<focus>` content with `L<n>:` line prefixes when serializing for code mode so the `Lxx-Lyy` references resolve.
- A small parser (regex-driven, no AST) extracts findings into a list of structs. The displayed review keeps the human-readable form; the parsed list enables fix 7.

**Prose mode is explicitly excluded** — free-form reactions and ideas are the point.

---

## Fix 7 — Track findings to actually avoid repetition (code mode)

**Where:** polymuse.el:1716-1717, polymuse.el:1727-1751.

**Problem:** The current `previous-review` blob with "Don't repeat points" is unreliable.

**Approach:** Once Fix 6 lands, store the parsed findings on `polymuse-review-state` and inject a short `<already_flagged>` list (line refs + summaries) into the code-mode prompt. Much shorter than the full prior review, and concrete enough that the model actually complies.

Prose mode: keep the existing `previous-review` tail as-is — for prose, "don't repeat yourself" matters less and the conversational continuity is valuable.

---

## Fix 8 — Switch tool calls to gptel's native tool API

**Where:** polymuse.el:1707-1710 (homegrown JSON tool protocol).

**Problem:** "Respond with JSON ONLY: {...}" inside a conversational system prompt is contradictory. gptel 0.9+ supports native tool use across Ollama and OpenAI backends, which handles parsing, validation, and provider-side enforcement.

**Approach:** Translate `polymuse-tool` structs into gptel's tool definitions, drop the embedded JSON instruction block, and let gptel route tool calls. Concrete win: removes a whole class of "model emitted prose + JSON" failures and gets free retry/validation. Add tests for tool invocation as part of this.

---

## Fix 9 — Stream responses; remove the fake typewriter

**Where:** polymuse.el:1945-1961, polymuse.el:984-993.

**Problem:** 200-word response at 45 cps = ~25 seconds of typewriter animation *after* the network round-trip. For something marketed as "over-the-shoulder" this is hostile.

**Approach:** Use `gptel-request`'s streaming callback (`:stream t` or equivalent in current gptel API). Append chunks directly to the output buffer as they arrive. Delete `typewrite-enqueue-job` from the display path. If a non-streaming fallback is needed for some backends, gate it on a backend slot.

---

## Fix 10 — Harden the sexp unit grabber

**Where:** polymuse.el:1275.

**Problem:** Lower priority since you're prose-first, but worth flagging: mid-edit sexp parsing can grab a 3-char or 5000-char region depending on paren balance, which then dominates the prompt budget badly.

**Approach:** After grabbing, sanity-check the unit's char length. If it's outside `[20, max-chars/2]`, fall back to "lines around point" (e.g. ±N lines, configurable). Cheap defensive measure. No change needed for prose path.

---

## Out of scope (deliberately deferred)

- Single-file split into multiple .el files — works fine as-is.
- Output buffer truncation refinements — works fine.
- Typo at polymuse.el:1721 — fix when touching that function for Fix 1.
- Test reorganization — add tests *with* each fix above, don't refactor the suite separately.
- Inline overlays for findings in the source buffer — a real feature, separate effort, build on top of Fix 6.

---

## Suggested order

1. **Fix 1** (serializer) — prerequisite for almost everything else; nothing else matters if the prompt is malformed on the wire.
2. **Fix 2** (split system prompts) — biggest quality jump for both prose and code with the least code.
3. **Fix 3** (cursor marker) — small, high-leverage, both modes.
4. **Fix 9** (streaming) — UX win, independent of the others.
5. **Fix 4, 5** (token budget, forward context) — tuning passes.
6. **Fix 6, 7** (structured findings + tracking) — code-mode quality, lands together.
7. **Fix 8** (native tool API) — independent, can land any time.
8. **Fix 10** (sexp robustness) — opportunistic.

## Verification (applies to every fix)

- Toggle `polymuse-debug` and inspect `*polymuse-debug*` and `*gptel-log*` after each change to confirm what hits the wire.
- Pure functions (`polymuse--compose-prompt-from-context`, the new `polymuse--serialize-prompt`) get golden-file ERT tests — assert exact serialized strings for a known alist input.
- End-to-end: run `polymuse-mode` on (a) a prose buffer and (b) a code buffer; confirm the right system prompt, the right tags/format, and that responses look noticeably more on-target.
