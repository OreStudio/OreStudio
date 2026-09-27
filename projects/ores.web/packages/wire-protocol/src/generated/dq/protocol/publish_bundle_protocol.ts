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
 * @brief Asks for a bundle to be published.
 */
export interface PublishBundleRequest {
    /**
     * @brief The bundle to publish.
     */
    bundle_code: string;
    /**
     * @brief How records are written to the target tables.
     */
    mode: string;
    /**
     * @brief Who asked for the publication.
     */
    published_by: string;
    /**
     * @brief Whether the whole bundle must succeed or none of it.
     */
    atomic: boolean;
    /**
     * @brief Per-dataset parameters, as JSON.
     */
    params_json: string;
}

/**
 * @brief Reports what the publication dispatched.
 */
export interface PublishBundleResponse {
    /**
     * @brief Whether the publication was accepted.
     */
    success: boolean;
    /**
     * @brief Why it was refused, when it was.
     */
    error_message: string;
    /**
     * @brief The workflow instance the publication started.
     */
    instance_id: string;
    /**
     * @brief How many datasets were dispatched.
     */
    datasets_dispatched: number;
}

export const subjects = {
    publish_bundle_request: "dq.v1.bundles.publish",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    publish_bundle_request: true,
} as const;
