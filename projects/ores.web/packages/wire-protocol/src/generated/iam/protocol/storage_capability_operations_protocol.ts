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
import type { StorageGrant } from '../../../security/payload.js';

/**
 * @brief Asks IAM for a storage capability naming a tenant's objects.
 *
 * Sent by a service that dispatches work to a node, with its own token. The
 * caller must hold the mint permission, and the tenant must be one it may act
 * for. The grants are the exact rows the token will carry.
 */
export interface MintStorageCapabilityRequest {
    /**
     * @brief The tenant whose objects the capability reaches.
     */
    tenant_id: string;
    /**
     * @brief The grants the capability carries, one row per operation.
     */
    grants: StorageGrant[];
}

/**
 * @brief The capability, or why there is none.
 */
export interface MintStorageCapabilityResponse {
    success: boolean;
    message: string;
    token: string;
    /**
     * @brief When the capability stops working, in seconds since the epoch.
     */
    expires_at: number;
}

export const subjects = {
    mint_storage_capability_request: 'iam.v1.ops.mint_storage_capability',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    mint_storage_capability_request: true,
} as const;
