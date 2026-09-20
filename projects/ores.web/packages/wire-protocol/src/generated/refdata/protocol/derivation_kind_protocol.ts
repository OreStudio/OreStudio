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
import type { DerivationKind } from '../domain/derivation_kind.js';

export interface GetDerivationKindsRequest {
    offset: number;
    limit: number;
}

export interface GetDerivationKindsResponse {
    kinds: DerivationKind[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveDerivationKindRequest {
    data: DerivationKind;
}

export interface SaveDerivationKindResponse {
    success: boolean;
    message: string;
}

export interface DeleteDerivationKindRequest {
    codes: string[];
}

export interface DeleteDerivationKindResponse {
    success: boolean;
    message: string;
}

export interface GetDerivationKindHistoryRequest {
    code: string;
}

export interface GetDerivationKindHistoryResponse {
    history: DerivationKind[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_derivation_kinds_request: "refdata.v1.derivation_kinds.list",
    save_derivation_kind_request: "refdata.v1.derivation_kinds.save",
    delete_derivation_kind_request: "refdata.v1.derivation_kinds.delete",
    get_derivation_kind_history_request: "refdata.v1.derivation_kinds.history",
} as const;
