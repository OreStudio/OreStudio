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
import type { EntityDescriptor } from './descriptor.js';
import {
  entityRecordPath,
  keyFromParams,
  recordKeyFromValues,
  recordLabel,
} from './entityPaths.js';

/**
 * A descriptor with the members these helpers read.
 *
 * Only the key is stated, because the key is all they read: the path is built
 * from `keyFields` and the entity's own segment, and the helpers are the same
 * for every entity.
 */
function descriptor(keyFields: readonly string[]): EntityDescriptor {
  return {
    component: 'iam',
    entity: 'account_party',
    meta: {
      entity: 'account_party',
      collection: 'account_parties',
      displayField: 'account_id',
      keyField: keyFields[0] ?? '',
      columns: [],
      fields: [],
    },
    routeSegment: 'account-party',
    apiBase: '/api/account_parties',
    keyFields,
    capabilities: { create: true, edit: true, remove: true, history: false },
    searchFields: [],
    writeFields: keyFields,
  };
}

describe('a record path', () => {
  it('carries one segment per key member', () => {
    expect(
      entityRecordPath(descriptor(['account_id', 'party_id']), {
        account_id: 'ACC-1',
        party_id: 'PARTY-2',
      }),
    ).toBe('/iam/account-party/ACC-1/PARTY-2');
  });

  it('encodes a member that would otherwise change the path', () => {
    // A value with a slash is one segment, not two: the route reads a fixed
    // number of segments and an unencoded slash would shift every one after it.
    expect(
      entityRecordPath(descriptor(['code']), { code: 'a/b c' }),
    ).toBe('/iam/account-party/a%2Fb%20c');
  });

  it('states a member it does not have as empty rather than omitting it', () => {
    // The route matches a fixed number of segments, so a missing member has to
    // be an empty one; omitting it would build a path that does not match.
    expect(entityRecordPath(descriptor(['account_id', 'party_id']), {}))
      .toBe('/iam/account-party//');
  });
});

describe('a key read back from the path', () => {
  it('is the inverse of the path that carried it', () => {
    const key = keyFromParams(descriptor(['account_id', 'party_id']), {
      account_id: 'ACC-1',
      party_id: 'PARTY-2',
    });
    expect(key).toEqual({ account_id: 'ACC-1', party_id: 'PARTY-2' });
  });

  it('states every member, so a filter matches on all of them', () => {
    // Half a junction's key names one side of a link and not the link, so a
    // member the path did not carry must not go missing from the record.
    const key = keyFromParams(descriptor(['account_id', 'party_id']), {
      account_id: 'ACC-1',
    });
    expect(key).toEqual({ account_id: 'ACC-1', party_id: '' });
  });
});

describe('a key read from a form', () => {
  it('takes the same members as the path', () => {
    expect(
      recordKeyFromValues(descriptor(['account_id', 'party_id']), {
        account_id: 'ACC-1',
        party_id: 'PARTY-2',
        version: 4,
      }),
    ).toEqual({ account_id: 'ACC-1', party_id: 'PARTY-2' });
  });
});

describe('a record named for a person', () => {
  it('names every member, because half a key is not the record', () => {
    expect(
      recordLabel(descriptor(['account_id', 'party_id']), {
        account_id: 'ACC-1',
        party_id: 'PARTY-2',
      }),
    ).toBe('ACC-1 / PARTY-2');
  });
});
