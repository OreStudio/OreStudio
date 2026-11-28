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
 * @brief Asks for the runs that published one dataset.
 */
export interface DatasetPublicationsRequest {
    /**
     * @brief The dataset whose runs to read.
     */
    dataset_code: string;
}

/**
 * @brief One run that published a dataset.
 */
export interface DatasetPublication {
    /**
     * @brief When the run finished, as the store records it.
     */
    published_at: string;
    /**
     * @brief The person who asked for the run.
     */
    published_by: string;
    /**
     * @brief How the run wrote its rows: upsert, insert_only or replace_all.
     */
    mode: string;
    /**
     * @brief The table the run wrote into.
     */
    target_table: string;
    /**
     * @brief How many rows the run inserted.
     */
    records_inserted: number;
    /**
     * @brief How many rows the run updated.
     */
    records_updated: number;
    /**
     * @brief How many rows the run chose not to write.
     */
    records_skipped: number;
    /**
     * @brief How many rows the run deleted.
     */
    records_deleted: number;
    /**
     * @brief What the run did: published, published_with_warnings or failed.
     */
    outcome: string;
    /**
     * @brief What the run said, as the JSON array the log holds.
     */
    messages: string;
}

/**
 * @brief Reports the runs, or why they could not be read.
 */
export interface DatasetPublicationsResponse {
    /**
     * @brief Whether the runs were read.
     */
    success: boolean;
    /**
     * @brief Why the runs could not be read, when they could not.
     */
    error_message: string;
    /**
     * @brief The runs, newest first.
     */
    publications: DatasetPublication[];
}

export const subjects = {
    dataset_publications_request: 'dq.v1.ops.dataset_publications',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    dataset_publications_request: true,
} as const;
