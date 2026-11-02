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
 * @brief One provider whose terms a person accepts, at the version they read.
 */
export interface ProviderTermsChoice {
    /**
     * @brief The provider whose terms are accepted.
     */
    provider_code: string;
    /**
     * @brief The version of the provider the person read, or zero for the latest.
     */
    provider_version: number;
}

/**
 * @brief What became of one provider in a list accept.
 */
export interface ProviderTermsOutcome {
    /**
     * @brief The provider the outcome is for.
     */
    provider_code: string;
    /**
     * @brief Whether its acceptance was recorded.
     */
    success: boolean;
    /**
     * @brief Why it was not, or what was recorded.
     */
    message: string;
    /**
     * @brief The account recorded as accepting.
     */
    accepted_by: string;
    /**
     * @brief When the acceptance was recorded.
     */
    accepted_at: string | null;
}

/**
 * @brief Accepts the terms of several providers in one request.
 *
 * Each provider is accepted as the single accept would accept it, with the same
 * checks, so one refusal does not stop the others: a setup that accepts every
 * public source, or a screen that accepts all it shows, learns which were recorded.
 */
export interface AcceptManyProviderTermsRequest {
    /**
     * @brief The providers whose terms are accepted, each at the version read.
     */
    providers: ProviderTermsChoice[];
}

/**
 * @brief One outcome a provider, in the order they were named.
 */
export interface AcceptManyProviderTermsResponse {
    /**
     * @brief Whether every provider's acceptance was recorded.
     */
    success: boolean;
    /**
     * @brief How many were recorded, and how many were not.
     */
    message: string;
    /**
     * @brief What became of each provider, in the order they were named.
     */
    outcomes: ProviderTermsOutcome[];
}

export const subjects = {
    accept_many_provider_terms_request: 'marketdata.v1.ops.accept_many_provider_terms',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    accept_many_provider_terms_request: true,
} as const;
