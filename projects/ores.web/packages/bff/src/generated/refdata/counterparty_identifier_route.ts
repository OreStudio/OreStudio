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
 * How the BFF reaches the counterparty_identifier service.
 *
 * The descriptor holds values and no functions: the collection, the natural
 * key, the members a write record carries and the subjects. The generic
 * factory builds the canonical envelopes from those, so the entity adds no
 * handler of its own and no envelope of its own.
 */
import { subjects } from '@ores/wire-protocol/generated/refdata/protocol/counterparty_identifier_protocol';
import { MINTED_WRITE_DEFAULT, type EntityRouteDescriptor } from '../../entity-routes.js';

export const counterpartyIdentifierRoute: EntityRouteDescriptor = {
  collection: 'counterparty_identifiers',
  key: 'id',
  keyField: 'id_value',
  rowField: 'counterparty_identifier',
  writeFields: ['id', 'counterparty_id', 'id_scheme', 'id_value', 'description'],
  writeDefaults: { id: MINTED_WRITE_DEFAULT, counterparty_id: null, id_scheme: '', id_value: '', description: null },
  intentFields: {
    reason: 'change_reason_code',
    commentary: 'change_commentary',
  },
  listHasAsOf: false,
  listHasFilter: true,
  versionsHasFilter: true,
  subjects: {
    list: subjects.list_counterparty_identifiers_request,
    save: subjects.put_counterparty_identifier_request,
  },
  rowsField: 'counterparty_identifiers',
};
