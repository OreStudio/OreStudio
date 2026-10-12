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

import type { ReactNode } from 'react';
import type { RecordRow } from '../api/client.js';
import type { FlagSource } from '../images/flags.js';
import type { ListColumn } from '../refdata/RecordList.js';
import type { FieldSpec, FieldValues } from '../refdata/records.js';

/**
 * One kind of row a feed owns, such as the components of an FX feed's mixture
 * or the template entries of an IR curve feed. The screens read these rows for
 * the feed, delete them with it and draw their history beside its own.
 */
export interface FeedPartKind {
    /** The synthetic resource the rows are read and written through. */
    readonly resource: string;
    /** The entity type their history is read by. */
    readonly entity: string;
    /** The fields a revert offers. */
    readonly specs: readonly FieldSpec[];
    /** The fields a revert carries over from the row as it stands. */
    readonly keep: readonly string[];
}

/** What a tab of a feed's page is given to draw itself. */
export interface FeedTabContext {
    readonly feed: RecordRow;
    /** Whether the feed runs: undefined until the server says. */
    readonly running: boolean | undefined;
    /** Whether the person may write the feed. */
    readonly mayWrite: boolean;
    /** The rows of each part the feed owns, in the order of the kind's parts. */
    readonly parts: readonly (readonly RecordRow[])[];
}

/** A tab that is the kind's own, such as an FX feed's mixture or a curve's template. */
export interface FeedTab {
    /** The tab's name; its title is `refdata.records.tabs.<name>`. */
    readonly name: string;
    readonly render: (context: FeedTabContext) => ReactNode;
}

/**
 * Everything the feed screens need to know about one asset class. A feed
 * screen is a list of the party's feeds and a page for one: both are the same
 * for every asset class, and a kind states only what differs. A new asset
 * class is a new kind, not a new screen.
 */
export interface FeedKind {
    /** The synthetic resource of the feeds. */
    readonly resource: string;
    /** The entity type the feed's history is read by. */
    readonly entity: string;
    /** What the history timeline calls the stream. */
    readonly historySubject: string;
    /** Where the list is; a feed's page is `<path>/<id>`. */
    readonly path: string;
    /** The block of `marketdata.*` words the kind's screens use. */
    readonly words: string;
    /** The feed's own fields, as the add dialog asks them. */
    readonly fields: readonly FieldSpec[];
    /** The fields the edit dialog offers. */
    readonly editFields: readonly FieldSpec[];
    /** The fields the server needs that the form does not edit. */
    readonly kept: readonly string[];
    /** What a new feed starts with. */
    readonly initial: FieldValues;
    /** The fields that follow from the others, written with the rest. */
    readonly complete: (write: Readonly<Record<string, unknown>>) => Record<string, unknown>;
    /** The rules across fields, said before the save. */
    readonly useCheck: () => (values: FieldValues) => readonly string[];
    /** What the feed is named in a title and a crumb, such as a pair or a curve. */
    readonly nameOf: (feed: Readonly<Record<string, unknown>>) => string;
    /** The flag drawn beside the name, when the name is a pair. */
    readonly flag?: FlagSource;
    /** What else the key and version line says. */
    readonly detailOf: (feed: RecordRow, t: (key: string) => string) => string;
    /** The kind's own columns of the list, after the status. */
    readonly columns: (t: (key: string) => string) => readonly ListColumn<RecordRow>[];
    /** Facts the Details tab adds to the fields. */
    readonly extraDetails: (
        feed: RecordRow,
        t: (key: string) => string,
    ) => readonly (readonly [string, string])[];
    /** The rows a feed owns. */
    readonly parts: readonly FeedPartKind[];
    /** The tabs between Details and History. */
    readonly tabs: readonly FeedTab[];
    /** The tab a new feed opens on, where the person goes on. */
    readonly afterAdd: string;
}

/** The feed's page, for the kind. */
export function feedPath(kind: FeedKind, id?: string): string {
    return id === undefined ? kind.path : `${kind.path}/${encodeURIComponent(id)}`;
}
