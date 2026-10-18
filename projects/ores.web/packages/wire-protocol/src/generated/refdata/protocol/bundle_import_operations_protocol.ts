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
import type { BundleImport } from '../domain/bundle_import.js';

/**
 * @brief What a commit did with one staged family.
 */
export interface BundleImportFamilyOutcome {
    family: string;
    sequence: number;
    applied: boolean;
    message: string;
}

/**
 * @brief Opens a staging session for a bundle.
 *
 * The caller supplies the id, so a retried begin names the same import. The
 * service stamps the session's party on the row.
 */
export interface BeginBundleImportRequest {
    id: string;
    source: string | null;
    families_expected: number | null;
    expires_at: string | null;
    /**
     * Refused when set: the service states the state, never the caller.
     */
    state: string | null;
}

/**
 * @brief The import that is open for staging.
 */
export interface BeginBundleImportResponse {
    success: boolean;
    message: string;
    import_row: BundleImport;
}

/**
 * @brief Stages one family's payload under an open import.
 *
 * The payload is the family's own generated value type, serialised as JSON.
 */
export interface StageBundleFamilyRequest {
    import_id: string;
    family: string;
    sequence: number;
    payload: string;
    checksum: string | null;
}

/**
 * @brief Whether the stage wrote its row.
 */
export interface StageBundleFamilyResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Applies every staged family of one import in one transaction.
 */
export interface CommitBundleImportRequest {
    import_id: string;
}

/**
 * @brief What the commit stored, and what it did not.
 *
 * The world conventions the tenant already held are kept, as the document save
 * keeps them. The family outcomes list one entry per applied staged family.
 */
export interface CommitBundleImportResponse {
    success: boolean;
    message: string;
    world_kept: string[];
    fx_skipped: string[];
    families: BundleImportFamilyOutcome[];
}

export const subjects = {
    begin_bundle_import_request: 'refdata.v1.ops.begin_bundle_import',
    stage_bundle_family_request: 'refdata.v1.ops.stage_bundle_family',
    commit_bundle_import_request: 'refdata.v1.ops.commit_bundle_import',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    begin_bundle_import_request: true,
    stage_bundle_family_request: true,
    commit_bundle_import_request: true,
} as const;
