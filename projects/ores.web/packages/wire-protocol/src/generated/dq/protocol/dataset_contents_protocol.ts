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
 * @brief Asks for the tables of one dataset, or for one page of one table's rows.
 */
export interface DatasetContentsRequest {
    /**
     * @brief The dataset whose contents to read.
     */
    dataset_code: string;
    /**
     * @brief The table whose rows to read, or empty for the list of tables alone.
     */
    table_name: string;
    /**
     * @brief How many rows to answer, at most.
     */
    limit: number;
    /**
     * @brief How many rows to skip before the ones answered.
     */
    offset: number;
}

/**
 * @brief One table a dataset stages into.
 */
export interface DatasetTable {
    /**
     * @brief The table's name, as the store states it.
     */
    table_name: string;
    /**
     * @brief Where the table sits in the family its artefact type names.
     */
    display_order: number;
    /**
     * @brief The columns this table carries, comma separated, without the two every
     * family shares.
     */
    columns: string;
    /**
     * @brief How many rows this table holds for the dataset.
     */
    staged_count: number;
}

/**
 * @brief Reports the tables and, when one was named, its rows.
 */
export interface DatasetContentsResponse {
    /**
     * @brief Whether the contents were read.
     */
    success: boolean;
    /**
     * @brief Why they could not be read, when they could not.
     */
    error_message: string;
    /**
     * @brief The tables the dataset stages into, in its family's order.
     */
    tables: DatasetTable[];
    /**
     * @brief One page of the named table's rows, as JSON: the columns it carries, the
     * rows in order, and the number the table holds.
     */
    rows: string;
}

export const subjects = {
    dataset_contents_request: 'dq.v1.ops.dataset_contents',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    dataset_contents_request: true,
} as const;
