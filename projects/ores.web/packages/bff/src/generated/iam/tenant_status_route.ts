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
 * How the BFF reaches the tenant_status service.
 *
 * The descriptor holds values and no functions: the collection, the natural
 * key, the array fields and the subjects. The generic factory builds the
 * routes from it, so the entity adds no handler of its own.
 */
import { subjects } from '@ores/wire-protocol/generated/iam/protocol/tenant_status_protocol';
import type { EntityRouteDescriptor } from '../../entity-routes.js';

export const tenantStatusRoute: EntityRouteDescriptor = {
  component: 'iam',
  entity: 'tenant_status',
  collection: 'tenant_statuses',
  key: 'id',
  keyField: 'status',
  subjects: {
    list: subjects.list_tenant_statuses_request,
    get: subjects.get_tenant_status_request,
    save: subjects.put_tenant_status_request,
    remove: subjects.delete_tenant_status_request,
    history: subjects.list_tenant_status_versions_request,
  },
  rowsField: 'statuses',
  getRowField: 'tenant_status',
  historyRowsField: 'versions',
};
