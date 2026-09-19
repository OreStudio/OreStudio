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

import { describe, expect, it } from 'vitest';
import { diffValues, type Segment } from './diff.js';

/** The text of the segments, so a case reads as the string it is about. */
function text(segments: readonly Segment[]): string {
  return segments.map((segment) => segment.text).join('');
}

/** The marked parts, so a case reads as what it claims changed. */
function changed(segments: readonly Segment[]): string {
  return segments
    .filter((segment) => segment.changed)
    .map((segment) => segment.text)
    .join('|');
}

describe('diffing two values', () => {
  it('reports identical values as unchanged', () => {
    const result = diffValues('Argentina', 'Argentina');
    expect(result.equal).toBe(true);
    expect(changed(result.before)).toBe('');
    expect(changed(result.after)).toBe('');
  });

  it('marks the character that changed, and nothing else', () => {
    const result = diffValues('Argentina', 'Argentine');
    expect(result.equal).toBe(false);
    expect(changed(result.before)).toBe('a');
    expect(changed(result.after)).toBe('e');
  });

  /*
   * The cases that matter for this data: a name extended rather than corrected,
   * where a line-level diff would mark the whole value on both sides.
   *
   * The marked parts are smaller than a prefix-and-suffix reading would give,
   * because the comparison keeps whatever the two values have in common wherever
   * it appears. "Argentina" to "Argentine Republic" is the last letter changing
   * from an a to an e and then a word being added: the shared "Argentin" is not
   * marked, and neither is anything else that survived.
   */
  it('marks only what did not survive when a value grows', () => {
    const result = diffValues('Argentina', 'Argentine Republic');
    expect(changed(result.before)).toBe('a');
    expect(changed(result.after)).toBe('e Republic');
  });

  it('marks only what did not survive when a value shrinks', () => {
    const result = diffValues('Argentine Republic', 'Argentina');
    expect(changed(result.before)).toBe('e Republic');
    expect(changed(result.after)).toBe('a');
  });

  it('keeps a character that merely moved, and marks the ones around it', () => {
    const before = 'The United Kingdom of Great Britain and Northern Ireland';
    const after = 'The United Kingdom of Great Britain and Southern Ireland';
    const result = diffValues(before, after);
    /*
     * The o of Northern and the o of Southern are the same character and are not
     * marked, so what is marked is N and r against S and u. Marking whole words
     * would be easier and would tell a person that more changed than did.
     */
    expect(changed(result.before)).toBe('N|r');
    expect(changed(result.after)).toBe('S|u');
  });

  it('handles a value that was empty', () => {
    expect(changed(diffValues('', 'Argentina').after)).toBe('Argentina');
    expect(changed(diffValues('Argentina', '').before)).toBe('Argentina');
  });

  it('handles values with nothing in common', () => {
    const result = diffValues('abc', 'xyz');
    expect(changed(result.before)).toBe('abc');
    expect(changed(result.after)).toBe('xyz');
  });

  // The property that matters more than any single case: whatever the segments
  // say, they have to be the value that went in. A diff that loses a character
  // shows a person a version of their data that never existed.
  it('reconstructs both values exactly', () => {
    const cases: readonly (readonly [string, string])[] = [
      ['Argentina', 'Argentine Republic'],
      ['032', '998'],
      ['', 'x'],
      ['x', ''],
      ['a', 'a'],
      ['one two three', 'one three'],
      ['aaa', 'aaaa'],
      ['Same', 'Same'],
    ];
    for (const [before, after] of cases) {
      const result = diffValues(before, after);
      expect(text(result.before)).toBe(before);
      expect(text(result.after)).toBe(after);
    }
  });

  it('gives whole runs rather than one per character', () => {
    // A segment per character renders correctly and is needless work; the runs
    // are what keeps the markup small.
    const result = diffValues('Argentina', 'Argentine Republic');
    expect(result.after.length).toBeLessThanOrEqual(3);
  });
});
