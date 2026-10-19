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

import type { RecordRow } from '../api/client.js';
import { api, type ReportEdit } from '../api/client.js';
import type { RecordStore } from '../refdata/store.js';

/**
 * The store the reporting screens' shared parts read and write.
 *
 * A report definition is a record of its own, and the parts that draw one — the
 * list, the details, the edit dialog, the revert dialog — read a record store,
 * so reporting gives them one rather than building its own. The store states
 * the one resource the routes serve, and what that resource searches and sorts:
 * nothing, because the generated read declares neither, and a list that cannot
 * search says so rather than reading every row to look in.
 */

/** The one resource the reporting routes serve. */
export const REPORTS_RESOURCE = 'reports';

/** Where the services announce a change to a definition, so the list can hear one. */
export const REPORTS_EVENTS = { component: 'reporting', entity: 'report_definitions' } as const;

/** The permission namespace of reporting; its wildcard grants every kind it holds. */
export const REPORTING = 'reporting';

/**
 * The six fields an edit writes, from the record the form was filled with.
 *
 * The field table also holds the fields a save cannot change, so the write is
 * narrowed here: the server rebuilds the rest of the record from the read it
 * makes at write time, and a field this screen does not offer keeps its value.
 */
function editOf(row: Readonly<Record<string, unknown>>): ReportEdit {
    return {
        description: String(row['description'] ?? ''),
        schedule_expression: String(row['schedule_expression'] ?? ''),
        is_official: row['is_official'] === true,
        pre_processing: String(row['pre_processing'] ?? ''),
        prepared_input_key: String(row['prepared_input_key'] ?? ''),
        post_processing: String(row['post_processing'] ?? ''),
    };
}

export const reportingStore: RecordStore = {
    name: REPORTING,
    registry: async () => [
        {
            key: REPORTS_RESOURCE,
            writable: true,
            search: false,
            sortable: [],
            writePermission: `${REPORTING}::report_definitions:write`,
            deletePermission: `${REPORTING}::report_definitions:delete`,
            events: REPORTS_EVENTS,
        },
    ],
    /*
     * The list reads its own page, because a row states the definition's party
     * and its latest run as well as the record's own fields. What the store is
     * for is the dialog and the revert the page opens over one record.
     */
    page: async () => {
        throw new Error('The reports list reads its own page.');
    },
    records: async () => {
        throw new Error('The reports list reads its own page.');
    },
    /*
     * The dialog and the revert read one record by the id the page is open on,
     * so a name is never handed here: a shared name is a choice the page has
     * already resolved.
     */
    record: async (_resource, id): Promise<RecordRow> => {
        const opened = await api.report(id);
        if (opened.kind !== 'record') {
            throw new Error('A name that several reports carry has no record to open.');
        }
        return opened.detail.definition;
    },
    save: async (_resource, { write, version, reasonCode, commentary }) => {
        if (version === null) {
            throw new Error('A report definition is amended, never created here.');
        }
        await api.saveReport(String(write['id']), {
            version,
            change: editOf(write),
            reasonCode,
            commentary,
        });
    },
    remove: async () => {
        throw new Error('The reports screen removes no definition.');
    },
};
