/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

/**
 * What changed inside a value, not just that it changed.
 *
 * The history screen shows a field before and after. Saying "these two strings
 * differ" is not useful when one is a hundred characters and two of them moved,
 * so the differing parts are found and returned as runs, which is what lets the
 * screen mark them the way a code host marks an inline diff.
 *
 * Character-level rather than line-level, because the values here are names and
 * codes: a line differs from its replacement in a few characters, and a
 * line-level diff would mark the whole value on both sides, which tells a person
 * nothing.
 */

/** A run of text, and whether it differs from the other side. */
export interface Segment {
  readonly text: string;
  readonly changed: boolean;
}

export interface ValueDiff {
  readonly before: readonly Segment[];
  readonly after: readonly Segment[];
  /** True when the two are identical, which is a caller's business to skip. */
  readonly equal: boolean;
}

/**
 * Past this length the comparison is done by common prefix and suffix instead.
 *
 * The longest-common-subsequence table is quadratic in the two lengths, and a
 * field can legitimately hold a paragraph. Prefix and suffix still finds the
 * changed region in the ordinary case of an edit, and refuses to spend a second
 * doing it for a value nobody will read character by character.
 */
const LCS_LIMIT = 600;

/** Compares two values and returns them split into changed and unchanged runs. */
export function diffValues(before: string, after: string): ValueDiff {
  if (before === after) {
    return { before: [{ text: before, changed: false }], after: [{ text: after, changed: false }], equal: true };
  }
  if (before.length === 0) {
    return { before: [], after: [{ text: after, changed: true }], equal: false };
  }
  if (after.length === 0) {
    return { before: [{ text: before, changed: true }], after: [], equal: false };
  }
  if (before.length > LCS_LIMIT || after.length > LCS_LIMIT) {
    return byPrefixAndSuffix(before, after);
  }
  return byLongestCommonSubsequence(before, after);
}

/**
 * The cheap answer: everything up to the first difference is unchanged and so is
 * everything after the last, and the middle is the change.
 *
 * Exact when a value was edited once in one place, approximate when it was edited
 * in several, and always in time proportional to the length.
 */
function byPrefixAndSuffix(before: string, after: string): ValueDiff {
  let start = 0;
  const shortest = Math.min(before.length, after.length);
  while (start < shortest && before[start] === after[start]) start += 1;

  let end = 0;
  while (
    end < shortest - start &&
    before[before.length - 1 - end] === after[after.length - 1 - end]
  ) {
    end += 1;
  }

  const beforeMiddle = before.slice(start, before.length - end);
  const afterMiddle = after.slice(start, after.length - end);

  return {
    before: runs([
      { text: before.slice(0, start), changed: false },
      { text: beforeMiddle, changed: true },
      { text: before.slice(before.length - end), changed: false },
    ]),
    after: runs([
      { text: after.slice(0, start), changed: false },
      { text: afterMiddle, changed: true },
      { text: after.slice(after.length - end), changed: false },
    ]),
    equal: false,
  };
}

/**
 * The exact answer, by longest common subsequence.
 *
 * Marks the characters that are genuinely absent from the other side, so an edit
 * in the middle of a long value marks only what moved rather than everything
 * between the ends.
 */
function byLongestCommonSubsequence(before: string, after: string): ValueDiff {
  const rows = before.length + 1;
  const columns = after.length + 1;
  // The table is (length+1) squared of 32-bit counts. The limit above keeps this
  // to a few hundred thousand entries at most.
  const table = new Uint32Array(rows * columns);

  for (let i = before.length - 1; i >= 0; i -= 1) {
    for (let j = after.length - 1; j >= 0; j -= 1) {
      table[i * columns + j] =
        before[i] === after[j]
          ? (table[(i + 1) * columns + (j + 1)] ?? 0) + 1
          : Math.max(table[(i + 1) * columns + j] ?? 0, table[i * columns + (j + 1)] ?? 0);
    }
  }

  const beforeSegments: Segment[] = [];
  const afterSegments: Segment[] = [];
  let i = 0;
  let j = 0;
  while (i < before.length && j < after.length) {
    if (before[i] === after[j]) {
      push(beforeSegments, before[i] ?? '', false);
      push(afterSegments, after[j] ?? '', false);
      i += 1;
      j += 1;
    } else if ((table[(i + 1) * columns + j] ?? 0) >= (table[i * columns + (j + 1)] ?? 0)) {
      push(beforeSegments, before[i] ?? '', true);
      i += 1;
    } else {
      push(afterSegments, after[j] ?? '', true);
      j += 1;
    }
  }
  while (i < before.length) {
    push(beforeSegments, before[i] ?? '', true);
    i += 1;
  }
  while (j < after.length) {
    push(afterSegments, after[j] ?? '', true);
    j += 1;
  }

  return { before: runs(beforeSegments), after: runs(afterSegments), equal: false };
}

/** Adds to the last run when it has the same state, so runs stay whole. */
function push(segments: Segment[], text: string, changed: boolean): void {
  const last = segments[segments.length - 1];
  if (last !== undefined && last.changed === changed) {
    segments[segments.length - 1] = { text: last.text + text, changed };
    return;
  }
  segments.push({ text, changed });
}

/** Drops the empty runs, which the prefix and suffix walk produces. */
function runs(segments: readonly Segment[]): readonly Segment[] {
  return segments.filter((segment) => segment.text.length > 0);
}
