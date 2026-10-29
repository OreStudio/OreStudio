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
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
import type { Supplier } from '../domain/supplier.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SupplierKey {
    code: string;
}

export interface SupplierLookup {
    key: SupplierKey;
    supplier: Supplier | null;
}

export interface SuppliersFilter {
    code_one_of: string[] | null;
}

export interface SupplierEvent {
    event_id: string;
    key: SupplierKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SupplierVersionKey {
    supplier: SupplierKey;
    version: number;
}

export interface SupplierVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSuppliersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SuppliersFilter | null;
    as_of: string | null;
}

export interface ListSuppliersResponse {
    result: Result;
    suppliers: Supplier[];
    total: number;
}

export interface GetSupplierRequest {
    key: SupplierKey;
}

export interface GetSupplierResponse {
    result: Result;
    supplier: Supplier | null;
}

export interface GetManySuppliersRequest {
    keys: SupplierKey[];
}

export interface GetManySuppliersResponse {
    result: Result;
    entries: SupplierLookup[];
}

export interface ListSupplierVersionsRequest {
    key: SupplierKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SupplierVersionsFilter | null;
}

export interface ListSupplierVersionsResponse {
    result: Result;
    versions: Supplier[];
    total: number;
}

export interface GetSupplierVersionRequest {
    key: SupplierVersionKey;
}

export interface GetSupplierVersionResponse {
    result: Result;
    version: Supplier | null;
}

export const subjects = {
    list_suppliers_request: 'marketdata.v1.suppliers.list',
    get_supplier_request: 'marketdata.v1.suppliers.get',
    get_many_suppliers_request: 'marketdata.v1.suppliers.get_many',
    list_supplier_versions_request: 'marketdata.v1.suppliers_versions.list',
    get_supplier_version_request: 'marketdata.v1.suppliers_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_suppliers_request: true,
    get_supplier_request: true,
    get_many_suppliers_request: true,
    list_supplier_versions_request: true,
    get_supplier_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.suppliers_events.created',
    updated: 'marketdata.v1.suppliers_events.updated',
    deleted: 'marketdata.v1.suppliers_events.deleted',
} as const;
