// Explicitly loaded offline fixture; NEVER put this in an auto-loaded extension directory.
// Exercises the real interactive Pi tool loop in a disposable Herdr pane.
import { createAssistantMessageEventStream } from "@earendil-works/pi-ai";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { execFileSync } from "node:child_process";
import { writeFileSync, realpathSync } from "node:fs";
import { join, isAbsolute } from "node:path";
import { tmpdir } from "node:os";
import { setTimeout as delay } from "node:timers/promises";

export default function (pi: ExtensionAPI) {
  const root = process.env.SHADY_SESSION_DIR;
  if (!root || !isAbsolute(root) || !realpathSync(root).startsWith(realpathSync(tmpdir()) + "/herdr-shady-session-")) {
    throw new Error("Fixture requires a disposable SHADY_SESSION_DIR under the system temp directory");
  }
  if (process.env.HERDR_ENV !== "1" || !process.env.HERDR_PANE_ID) throw new Error("Run this fixture inside Herdr");
  const pane = process.env.HERDR_PANE_ID;
  const sample = join(root, "sample.ts");
  writeFileSync(sample, '// Quiet syntax comment\nconst greeting: string = "hello";\nconst count = 42;\n');
  const observations: any[] = [];
  async function capture(label: string, ctx: any) {
    await delay(350);
    const result = JSON.parse(execFileSync("herdr", ["pane", "get", pane], { encoding: "utf8", timeout: 5000 }));
    observations.push({ label, theme: ctx.ui.theme.name, pane: result.result.pane });
    writeFileSync(join(root, `${label}.ansi`), execFileSync("herdr", ["pane", "read", pane, "--source", "visible", "--format", "ansi", "--lines", "120"], { timeout: 5000 }));
  }
  let turn = 0;
  pi.registerProvider("shady-fixture", {
    baseUrl: "http://127.0.0.1/unused",
    apiKey: "offline-fixture-no-network",
    api: "shady-fixture",
    models: [{ id: "session", name: "Shady offline session", reasoning: true, input: ["text"],
      cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 }, contextWindow: 32000, maxTokens: 4096 }],
    streamSimple(model, _context, options) {
      const stream = createAssistantMessageEventStream();
      const message: any = { role: "assistant", api: model.api, provider: model.provider, model: model.id,
        content: [], timestamp: Date.now(), stopReason: "pending",
        usage: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, totalTokens: 0,
          cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 } } };
      void (async () => {
        try {
          stream.push({ type: "start", partial: message });
          const n = ++turn;
          // Allow Herdr to consume partial output; cancellation must end the stream.
          await delay(600, undefined, { signal: options?.signal });
          const texts = [
            "I’ll inspect the TypeScript example before making a focused change.",
            "The example is readable. I’ll change only the greeting; the edit will show a diff.",
            "Now I’ll exercise an intentional failed check, without running project commands.",
            "## Shady session complete\n\n- Read TypeScript with comments, strings, types, and numbers.\n- Applied a focused **edit** and rendered its diff.\n- Observed the intentional `fixture check failed` tool error.\n\n```typescript\n// Quiet syntax comment\nconst greeting: string = \"shady\";\nconst count = 42;\n```\n\n[Fixture link](https://example.com) · Ready for input.\n\nThe attention state was simulated; no approval was requested or granted."
          ];
          message.content.push({ type: "text", text: "" });
          stream.push({ type: "text_start", contentIndex: 0, partial: message });
          message.content[0].text = texts[n - 1] ?? "Fixture complete.";
          stream.push({ type: "text_delta", contentIndex: 0, delta: message.content[0].text, partial: message });
          stream.push({ type: "text_end", contentIndex: 0, content: message.content[0].text, partial: message });
          const calls = [
            { name: "read", arguments: { path: sample } },
            { name: "edit", arguments: { path: sample, edits: [{ oldText: '"hello"', newText: '"shady"' }] } },
            { name: "bash", arguments: { command: "printf 'fixture check failed (intentional)\\n' >&2; exit 1" } },
          ];
          const call = calls[n - 1];
          if (call) {
            const toolCall = { type: "toolCall", id: `shady-${n}`, ...call };
            message.content.push(toolCall);
            stream.push({ type: "toolcall_start", contentIndex: 1, partial: message });
            stream.push({ type: "toolcall_end", contentIndex: 1, toolCall, partial: message });
          }
          message.stopReason = call ? "toolUse" : "stop";
          stream.push({ type: "done", reason: message.stopReason, message });
        } catch (error) {
          message.stopReason = options?.signal?.aborted ? "aborted" : "error";
          message.errorMessage = String(error);
          stream.push({ type: "error", reason: message.stopReason, error: message });
        } finally { stream.end(); }
      })();
      return stream;
    },
  });
  pi.on("session_start", async (_event, ctx) => {
    if (ctx.mode !== "tui") throw new Error("This is an interactive renderer test");
    await capture("idle", ctx);
  });
  pi.on("agent_start", async (_event, ctx) => { await capture("working", ctx); });
  pi.on("tool_execution_end", async (event, ctx) => {
    if (event.toolName === "read") {
      // Use the official integration's testable event channel, not an approval dialog.
      pi.events.emit("herdr:blocked", { active: true, label: "Shady fixture: simulated attention" });
      ctx.ui.setWidget("shady-fixture", [ctx.ui.theme.fg("error", "ATTENTION: simulated test state; no approval needed")]);
      await capture("blocked", ctx);
      pi.events.emit("herdr:blocked", { active: false });
      ctx.ui.setWidget("shady-fixture", undefined);
    }
    if (event.toolName === "edit") await capture("diff", ctx);
    if (event.toolName === "bash") await capture("error", ctx);
  });
  pi.on("agent_settled", async (_event, ctx) => {
    if (!turn) return;
    await capture("settled", ctx);
    writeFileSync(join(root, "completed.json"), JSON.stringify({ turns: turn, observations }, null, 2));
  });
}
