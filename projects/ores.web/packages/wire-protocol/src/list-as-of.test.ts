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

import { existsSync, readdirSync, readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import { CLASSIFICATION_LISTS } from './classifications.js';
import { REFDATA_RECORDS } from './records.js';

/**
 * A list request carries `as_of` when the C++ request declares it.
 *
 * The services decode a list request strictly: a request that leaves the key
 * out is refused as bad_request, even though the field is optional, so a
 * screen built on that list shows an error and no rows. The generator added
 * the field to the list requests of whole components at once, and the flags
 * that say which TypeScript list sends it were left behind. This reads the C++
 * requests, which are the truth, and holds every flag to them.
 */
const PROJECTS = fileURLToPath(new URL('../../../../', import.meta.url));

/** Each list request's subject, and whether its struct declares `as_of`. */
function cppListRequests(): ReadonlyMap<string, boolean> {
    const found = new Map<string, boolean>();
    for (const component of readdirSync(PROJECTS)) {
        const directory = `${PROJECTS}${component}/api/include/${component}.api/messaging`;
        if (!component.startsWith('ores.') || !existsSync(directory)) continue;
        for (const file of readdirSync(directory)) {
            if (!file.endsWith('_protocol.hpp')) continue;
            const text = readFileSync(`${directory}/${file}`, 'utf8');
            for (const match of text.matchAll(/struct list_\w+_request \{([\s\S]*?)\n\};/g)) {
                const body = match[1] ?? '';
                const subject = /nats_subject = "([^"]+)"/.exec(body)?.[1];
                if (subject !== undefined) found.set(subject, body.includes('as_of'));
            }
        }
    }
    return found;
}

const REQUESTS = cppListRequests();

describe('the as_of flags of the lists', () => {
    it('read some C++ list requests, or the check proves nothing', () => {
        expect(REQUESTS.size).toBeGreaterThan(100);
    });

    it('send as_of from every reference data resource whose request declares it', () => {
        const wrong = REFDATA_RECORDS.filter(
            (resource) => REQUESTS.get(resource.subjects.list) !== undefined,
        )
            .filter((resource) => REQUESTS.get(resource.subjects.list) !== resource.asOf)
            .map((resource) => resource.key);

        expect(wrong).toEqual([]);
    });

    it('send as_of from every classification list whose request declares it', () => {
        const wrong = CLASSIFICATION_LISTS.filter(
            (list) => REQUESTS.get(list.subjects.list) !== undefined,
        )
            .filter((list) => REQUESTS.get(list.subjects.list) !== (list.asOf === true))
            .map((list) => list.key);

        expect(wrong).toEqual([]);
    });
});
