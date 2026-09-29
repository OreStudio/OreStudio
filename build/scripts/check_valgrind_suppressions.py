#!/usr/bin/env python3
"""Check the valgrind suppression file against the stacks a run reported.

Valgrind matches a suppression's frames against the top of a stack, in order,
where `...` skips any number of frames and the trailing frames of the stack are
ignored. This replays that rule over the suppression blocks a memcheck log or a
CDash dynamic-analysis page carries, and fails when a reported stack would not be
suppressed. It is the only way to check the categories a local build cannot
reproduce: the coverage runtime is linked into the CI binaries alone, so its
thousands of still-reachable blocks never appear locally.

    python3 build/scripts/check_valgrind_suppressions.py \
        build/valgrind/custom.supp <memcheck log or CDash page>
"""

from __future__ import annotations

import fnmatch
import pathlib
import sys

BLOCK_START = "{"
BLOCK_END = "}"


def parse_suppressions(path: pathlib.Path):
    rules, current, kinds, frames, name = [], None, set(), [], ""
    for line in path.read_text().splitlines():
        stripped = line.strip()
        if current is None:
            if stripped == BLOCK_START:
                current, kinds, frames, name = [], set(), [], ""
            continue
        if stripped == BLOCK_END:
            rules.append((name, kinds, frames))
            current = None
            continue
        current.append(stripped)
        if stripped.startswith("Memcheck:"):
            continue
        if stripped.startswith("match-leak-kinds:"):
            kinds = {k.strip() for k in stripped.split(":", 1)[1].split(",")}
            continue
        if stripped.startswith("fun:") or stripped.startswith("obj:"):
            frames.append(stripped)
            continue
        if stripped == "...":
            frames.append("...")
            continue
        if not name and not stripped.startswith("#"):
            name = stripped
    return rules


def parse_report(path: pathlib.Path):
    blocks, current = [], None
    for line in path.read_text(errors="ignore").splitlines():
        if current is None:
            if line.strip() == BLOCK_START:
                current = []
            continue
        if line.strip() == BLOCK_END:
            blocks.append(current)
            current = None
            continue
        current.append(line.strip())
    return blocks


def frame_matches(pattern: str, frame: str) -> bool:
    if pattern.startswith("fun:"):
        return frame.startswith("fun:") and fnmatch.fnmatch(frame[4:], pattern[4:])
    if pattern.startswith("obj:"):
        return frame.startswith("obj:") and fnmatch.fnmatch(frame[4:], pattern[4:])
    return False


def rule_matches(frames: list[str], stack: list[str], kinds: set[str], stack_kinds: set[str]) -> bool:
    if kinds and stack_kinds and not (kinds & stack_kinds):
        return False

    def walk(frame_index: int, stack_index: int) -> bool:
        while frame_index < len(frames):
            if frames[frame_index] == "...":
                if frame_index == len(frames) - 1:
                    return True
                return any(
                    walk(frame_index + 1, skip)
                    for skip in range(stack_index, len(stack) + 1)
                )
            if stack_index >= len(stack):
                return False
            if not frame_matches(frames[frame_index], stack[stack_index]):
                return False
            frame_index += 1
            stack_index += 1
        return True

    return walk(0, 0)


def main() -> int:
    suppressions = parse_suppressions(pathlib.Path(sys.argv[1]))
    report = pathlib.Path(sys.argv[2])
    blocks = parse_report(report)
    unmatched = 0
    for block in blocks:
        stack, kinds = [], set()
        for line in block:
            if line.startswith("match-leak-kinds:"):
                kinds = {k.strip() for k in line.split(":", 1)[1].split(",")}
            elif line.startswith(("fun:", "obj:")):
                stack.append(line)
        if not any(rule_matches(frames, stack, rk, kinds) for _, rk, frames in suppressions):
            unmatched += 1
            if unmatched <= 5:
                print("unmatched:", stack[1] if len(stack) > 1 else stack[:2])
    print(
        f"suppression rules: {len(suppressions)}; reported stacks: {len(blocks)}; "
        f"unmatched: {unmatched}"
    )
    return 1 if unmatched else 0


if __name__ == "__main__":
    sys.exit(main())
