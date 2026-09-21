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
import type { Catalog } from '../domain/catalog.js';

export interface GetCatalogsRequest {
    offset: number;
    limit: number;
}

export interface GetCatalogsResponse {
    catalogs: Catalog[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCatalogRequest {
    data: Catalog;
}

export interface SaveCatalogResponse {
    success: boolean;
    message: string;
}

export interface DeleteCatalogRequest {
    names: string[];
}

export interface DeleteCatalogResponse {
    success: boolean;
    message: string;
}

export interface GetCatalogHistoryRequest {
    name: string;
}

export interface GetCatalogHistoryResponse {
    history: Catalog[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_catalogs_request: "dq.v1.catalogs.list",
    save_catalog_request: "dq.v1.catalogs.save",
    delete_catalog_request: "dq.v1.catalogs.delete",
    get_catalog_history_request: "dq.v1.catalogs.history",
} as const;
