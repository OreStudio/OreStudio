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
/**
 * @brief Command payload sent by the DQ workflow engine to a target service's
 *        publish-from-dq NATS subject.
 *
 * The receiving handler calls a SECURITY DEFINER SQL function that reads the
 * DQ artefact table for dataset_id and writes to the target service's tables.
 */
export interface PublishFromDqCommand {
    /**
     * @brief UUID of the DQ dataset to publish.
     */
    dataset_id: string;
    /**
     * @brief UUID of the target tenant.
     */
    tenant_id: string;
    /**
     * @brief How the target writes the rows: upsert, replace_all or insert_only.
     */
    mode: string;
    /**
     * @brief Extra per-artefact parameters, as JSON. May be an empty object.
     */
    params_json: string;
}

/**
 * @brief Result returned by the target service's publish-from-dq handler.
 *
 * Serialised as JSON and passed to wf->complete() so the workflow engine can
 * record counts and propagate them to the bundle publish result.
 */
export interface PublishFromDqResult {
    /**
     * @brief Whether the publication completed.
     */
    success: boolean;
    /**
     * @brief Why it failed, when it did.
     */
    error_message: string;
    /**
     * @brief How many rows the publication inserted.
     */
    records_inserted: number;
    /**
     * @brief How many rows the publication updated.
     */
    records_updated: number;
    /**
     * @brief How many rows the publication left alone.
     */
    records_skipped: number;
    /**
     * @brief How many rows the publication removed.
     */
    records_deleted: number;
}

export const subjects = {
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
} as const;
