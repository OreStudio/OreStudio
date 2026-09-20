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
 * How the BFF reaches the account_contact_information service.
 *
 * The descriptor holds values and no functions: the collection, the natural
 * key, the array fields and the subjects. The generic factory builds the
 * routes from it, so the entity adds no handler of its own.
 */
import { subjects } from '@ores/wire-protocol/generated/iam/protocol/account_contact_information_protocol';
import type { EntityRouteDescriptor } from '../../entity-routes.js';

export const accountContactInformationRoute: EntityRouteDescriptor = {
  collection: 'account_contact_informations',
  key: 'id',
  keyField: 'email',
  deleteKeysField: 'ids',
  subjects: {
    list: subjects.get_account_contact_informations_request,
    save: subjects.save_account_contact_information_request,
    remove: subjects.delete_account_contact_information_request,
    history: subjects.get_account_contact_information_history_request,
  },
  rowsField: 'account_contact_informations',
};
