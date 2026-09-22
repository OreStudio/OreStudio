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
 */

/**
 * The TypeScript twins of the records in `ores::diff::domain` that a history
 * response carries.
 *
 * The C++ types are hand-written engine structs, not codegen models, so no
 * `ores.ts.domain` facet emits them. The generated history protocol imports
 * these interfaces instead. Keep the members in step with
 * `projects/ores.diff/include/ores.diff/domain/`.
 */

/**
 * One field of a rendered version: its display name and its value as a string.
 *
 * History compares rendered strings, so a value arrives as the text a reader
 * sees rather than as the type the row holds.
 */
export interface FieldValue {
    name: string;
    value: string;
}

/**
 * The byte range within a value that changed.
 *
 * Byte ranges rather than character counts, because the engine never decodes
 * the strings it compares: it finds the common prefix and suffix and reports
 * what lies between them.
 */
export interface DiffSpan {
    offset: number;
    length: number;
}

/**
 * One field that changed between two versions.
 *
 * `old_value` is empty when the field was added and `new_value` when it was
 * removed. A value that changed entirely still carries one span covering the
 * whole string, so an empty span list means "unchanged", never "wholly
 * different".
 */
export interface DiffEntry {
    field_name: string;
    old_value: string;
    new_value: string;
    old_spans: DiffSpan[];
    new_spans: DiffSpan[];
}

/**
 * Every field that changed between two versions; empty when they are
 * identical.
 */
export interface DiffResult {
    entries: DiffEntry[];
}
