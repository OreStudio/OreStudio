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
 * @brief One legal entity, reduced to the fields the shell lists it by.
 */
export interface LeiEntitySummary {
    /**
     * @brief The entity's LEI.
     */
    lei: string;
    /**
     * @brief The entity's registered legal name.
     */
    entity_legal_name: string;
    /**
     * @brief The entity's category, as the registry classifies it.
     */
    entity_category: string;
    /**
     * @brief The country of the entity's legal address.
     */
    country: string;
}

/**
 * @brief Asks for the active root legal entities, optionally of one country.
 */
export interface GetLeiEntitiesSummaryRequest {
    /**
     * @brief The country to list, or empty for every country's count.
     */
    country_filter: string;
    /**
     * @brief How many rows to skip.
     */
    offset: number;
    /**
     * @brief How many rows to return.
     */
    limit: number;
}

/**
 * @brief Reports the summary rows, or why they could not be read.
 */
export interface GetLeiEntitiesSummaryResponse {
    /**
     * @brief Whether the read completed.
     */
    success: boolean;
    /**
     * @brief Why it failed, when it did.
     */
    error_message: string;
    /**
     * @brief The summarised entities.
     */
    entities: LeiEntitySummary[];
}

/**
 * @brief One legal entity a search matched, and the work it would bring.
 */
export interface LeiEntityMatch {
    /**
     * @brief The entity's LEI.
     */
    lei: string;
    /**
     * @brief The entity's registered legal name.
     */
    entity_legal_name: string;
    /**
     * @brief The entity's category, as the registry classifies it.
     */
    entity_category: string;
    /**
     * @brief The country of the entity's legal address.
     */
    country: string;
    /**
     * @brief How many parties importing this entity's hierarchy would create,
     * counting the entity itself.
     *
     * A person choosing the entity a tenant is built around is choosing that much
     * work, so the match states it before the choice is made. It is the size of the
     * hierarchy the deployment holds under the entity, which is what the
     * publication walks.
     */
    party_count: number;
}

/**
 * @brief Asks for the root legal entities that match a search.
 *
 * The read a search uses, as against the country browser: a name or an LEI is
 * what somebody looking for a particular entity knows, and the deployment holds
 * far more entities than one answer can carry.
 */
export interface SearchLeiEntitiesRequest {
    /**
     * @brief The text to match, or empty for every entity.
     *
     * A name matches anywhere in the legal name; an LEI matches from its start.
     */
    search: string;
    /**
     * @brief The country to restrict the matches to, or empty for every country.
     */
    country_filter: string;
    /**
     * @brief How many matches to skip.
     */
    offset: number;
    /**
     * @brief How many matches to return.
     */
    limit: number;
}

/**
 * @brief Reports the matches, or why they could not be read.
 */
export interface SearchLeiEntitiesResponse {
    /**
     * @brief Whether the read completed.
     */
    success: boolean;
    /**
     * @brief Why it failed, when it did.
     */
    error_message: string;
    /**
     * @brief The matches.
     */
    entities: LeiEntityMatch[];
}

export const subjects = {
    get_lei_entities_summary_request: 'dq.v1.ops.get_lei_entities_summary',
    search_lei_entities_request: 'dq.v1.ops.search_lei_entities',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_lei_entities_summary_request: true,
    search_lei_entities_request: true,
} as const;
