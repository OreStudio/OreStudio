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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_bff_route.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * How the BFF reaches the session service.
 *
 * The descriptor holds values and no functions: the collection, the key, the
 * members a write record carries, the optional members the requests declare
 * and the subjects. The generic factory builds the canonical envelopes from
 * those, so the entity adds no handler of its own and no envelope of its own.
 */
import { subjects } from '@ores/wire-protocol/generated/iam/protocol/session_protocol';
import { MINTED_WRITE_DEFAULT, type EntityRouteDescriptor } from '../../entity-routes.js';

export const sessionRoute: EntityRouteDescriptor = {
  component: 'iam',
  entity: 'session',
  collection: 'sessions',
  keyFields: ['id'],
  writeFields: ['id', 'start_time', 'account_id', 'end_time', 'client_ip', 'client_identifier', 'client_version_major', 'client_version_minor', 'bytes_sent', 'bytes_received', 'country_code', 'protocol'],
  writeDefaults: { id: MINTED_WRITE_DEFAULT, start_time: '1970-01-01T00:00:00Z', account_id: null, end_time: '', client_ip: '', client_identifier: '', client_version_major: 0, client_version_minor: 0, bytes_sent: 0, bytes_received: 0, country_code: '', protocol: '' },
  listHasAsOf: false,
  listHasFilter: false,
  versionsHasFilter: false,
  subjects: {
    list: subjects.list_sessions_request,
    get: subjects.get_session_request,
    save: subjects.put_session_request,
    remove: subjects.delete_session_request,
  },
  rowsField: 'sessions',
  getRowField: 'session',
};
