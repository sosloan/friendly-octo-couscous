# 🧠 Lexical TRUE Accessible Editor & Brace Bridges

This page captures how to add a **Lexical**-based rich text editor that stays **TRUE** (Typed, Robust, Universal, Equitable) while bridging code-like braces and collaborating with the rest of the polyglot system. **TRUE** is the internal accessibility baseline: keep semantics typed, announce changes robustly, ensure universal keyboard reachability, and ship equitable visual contrast.

## Goals
- **Accessible by default**: keyboard-first navigation, aria-live announcements, and WCAG-friendly headings/landmarks.
- **Brace bridges**: highlight and pair braces/brackets for strategy notes and code snippets.
- **Collaboration-ready**: stream updates through the existing Akka/Erlang bridge so multiple operators can co-edit trading runbooks.
- **Drop-in**: lives beside the existing components; no changes to Ada/Java/Swift code paths.

## Reference implementation (Lexical)
The following Lexical setup keeps the surface minimal while wiring accessibility and brace support:

```tsx
import {EditorState} from "lexical";
import {LexicalComposer} from "@lexical/react/LexicalComposer";
import {RichTextPlugin} from "@lexical/react/LexicalRichTextPlugin";
import {ContentEditable} from "@lexical/react/LexicalContentEditable";
import {HistoryPlugin} from "@lexical/react/LexicalHistoryPlugin";
import {OnChangePlugin} from "@lexical/react/LexicalOnChangePlugin";
import {BraceMatchingPlugin} from "./plugins/BraceMatchingPlugin";
import {CollabBridgePlugin} from "./plugins/CollabBridgePlugin";

const editorConfig = {
  namespace: "hft-trading-notes",
  theme: {/* high-contrast theme, focus outlines */},
  onError: (error) => console.error(error),
  editable: true
};

export default function AccessibleEditor({onChange}: {onChange: (editorState: EditorState) => void}) {
  return (
    <LexicalComposer initialConfig={editorConfig}>
      <RichTextPlugin
        contentEditable={
          <ContentEditable
            aria-label="Trading runbook editor"
            spellCheck={true}
            role="textbox"
          />
        }
        placeholder={<span>Document the strategy…</span>}
      />
      <HistoryPlugin />
      <OnChangePlugin onChange={onChange} />
      <BraceMatchingPlugin />      {/* brace bridges */}
      <CollabBridgePlugin />       {/* Akka/Erlang broadcast */}
    </LexicalComposer>
  );
}
```

### Brace bridges
- Parse `()` `{}` `[]` tokens and visually indicate the matching partner whenever the caret lands on a brace.
- Announce matches via `aria-live="polite"` for screen readers.
- Keep contrast-friendly background on paired braces to satisfy WCAG 2.1 AA.

### Collaboration bridge
- Use the existing Akka reactive bridge to broadcast Lexical updates.
- Erlang supervisor tracks sessions and fan-outs to listeners.
- Latency target: sub-50ms local for keystroke-to-fanout sync, aligning with HFT operator playbooks.

## Accessibility checklist (TRUE)
- **Typed**: maintain semantic nodes (headings, lists, code) instead of spans.
- **Robust**: announce formatting changes with `aria-live`, validate HTML output.
- **Universal**: full keyboard reachability (Tab/Shift+Tab for toolbars, Esc to exit), RTL/LTR support.
- **Equitable**: high-contrast theme, reduced-motion friendly transitions, screen-reader cues for brace pairs.

## Integration steps
1. Add Lexical + plugins under a small React/TypeScript bundle (e.g., `ui/lexical`).
2. Expose a WebSocket endpoint on the Akka side to forward editor deltas.
3. Register an Erlang subscriber to persist snapshots or forward to SwiftUI dashboards.
4. Add a smoke test that mounts the editor, toggles focus states, and verifies brace announcements.

This keeps the editor grounded in accessibility while bridging the polyglot stack for collaborative runbooks and strategy notes.
