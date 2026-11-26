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
 * @brief Asks for the roster, or for one bundle of it with its members.
 */
export interface BundleRosterRequest {
    /**
     * @brief The bundle whose members to read, or empty for the roster alone.
     */
    bundle_code: string;
}

/**
 * @brief One dataset a bundle carries, and what its artefact table holds.
 */
export interface BundleRosterMember {
    /**
     * @brief Where the member sits in the bundle's own order.
     */
    display_order: number;
    /**
     * @brief The dataset the member names.
     */
    dataset_code: string;
    /**
     * @brief The dataset's name, or empty when no dataset row names its code.
     */
    dataset_name: string;
    /**
     * @brief The artefact type whose table the dataset stages into.
     */
    artefact_type: string;
    /**
     * @brief The table, or family of tables, the dataset stages into.
     */
    artefact_table: string;
    /**
     * @brief The subject a publish writes to, or empty when it names none.
     */
    target_subject: string;
    /**
     * @brief Whether a dataset row names the member's code.
     */
    has_dataset: boolean;
    /**
     * @brief Whether the dataset's artefact type is one the store holds.
     */
    has_artefact_type: boolean;
    /**
     * @brief How many rows the dataset has staged, ready to publish.
     */
    staged_count: number;
    /**
     * @brief Whether the member has both a dataset row and a target subject.
     */
    is_publishable: boolean;
    /**
     * @brief Whether the member publishes only when a caller opts in.
     */
    optional: boolean;
}

/**
 * @brief One bundle, the level it publishes at, and its members.
 */
export interface BundleRosterBundle {
    /**
     * @brief The bundle's code.
     */
    code: string;
    /**
     * @brief The bundle's name.
     */
    name: string;
    /**
     * @brief What the bundle carries.
     */
    description: string;
    /**
     * @brief The level the bundle publishes at: platform, world or tenant.
     */
    level: string;
    /**
     * @brief How many datasets the bundle carries.
     */
    dataset_count: number;
    /**
     * @brief The members, in the bundle's own order, when the request named it.
     */
    members: BundleRosterMember[];
}

/**
 * @brief Reports the roster, or why it could not be read.
 */
export interface BundleRosterResponse {
    /**
     * @brief Whether the roster was read.
     */
    success: boolean;
    /**
     * @brief Why it could not be read, when it could not.
     */
    error_message: string;
    /**
     * @brief Every bundle, ordered by code.
     */
    bundles: BundleRosterBundle[];
}

export const subjects = {
    bundle_roster_request: 'dq.v1.ops.bundle_roster',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    bundle_roster_request: true,
} as const;
