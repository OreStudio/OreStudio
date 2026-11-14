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
 * @brief One package a run read or wrote.
 */
export interface RunFileArchive {
    /**
     * @brief "input" for a package the engine read, "output" for one it wrote.
     */
    direction: string;
    /**
     * @brief The workunit whose package this is.
     */
    workunit_id: string;
    /**
     * @brief The result that wrote an output package, empty for an input.
     */
    result_id: string;
    /**
     * @brief The object path the package is stored at.
     *
     * A path and not a bucket and a key, because that is what the workunit row
     * states and what a caller hands back to storage without rebuilding it.
     */
    uri: string;
}

/**
 * @brief Asks for the packages one report run read and wrote.
 */
export interface ListRunFilesRequest {
    /**
     * @brief The run the caller is showing. It is the batch's external reference.
     */
    run_id: string;
}

/**
 * @brief The run's packages, in the order the workunits were dispatched.
 */
export interface ListRunFilesResponse {
    success: boolean;
    message: string;
    run_id: string;
    /**
     * @brief The batch the run drove, empty when the run has none yet.
     */
    batch_id: string;
    archives: RunFileArchive[];
}

export const subjects = {
    list_run_files_request: 'compute.v1.ops.list_run_files',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_run_files_request: true,
} as const;
