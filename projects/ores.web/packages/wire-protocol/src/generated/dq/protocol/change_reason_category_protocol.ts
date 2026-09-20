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
import type { ChangeReasonCategory } from '../domain/change_reason_category.js';

export interface GetChangeReasonCategoriesRequest {
    offset: number;
    limit: number;
}

export interface GetChangeReasonCategoriesResponse {
    categories: ChangeReasonCategory[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveChangeReasonCategoryRequest {
    data: ChangeReasonCategory;
}

export interface SaveChangeReasonCategoryResponse {
    success: boolean;
    message: string;
}

export interface DeleteChangeReasonCategoryRequest {
    codes: string[];
}

export interface DeleteChangeReasonCategoryResponse {
    success: boolean;
    message: string;
}

export interface GetChangeReasonCategoryHistoryRequest {
    code: string;
}

export interface GetChangeReasonCategoryHistoryResponse {
    history: ChangeReasonCategory[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_change_reason_categories_request: "dq.v1.change_reason_categories.list",
    save_change_reason_category_request: "dq.v1.change_reason_categories.save",
    delete_change_reason_category_request: "dq.v1.change_reason_categories.delete",
    get_change_reason_category_history_request: "dq.v1.change_reason_categories.history",
} as const;
