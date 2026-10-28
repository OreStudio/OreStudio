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
 * @brief Accepts the terms of a provider as they stand.
 *
 * The caller names the provider and nothing else: what is accepted is the terms
 * the service holds, by whom is the session's account, and when is the service's
 * clock. Terms that change afterwards no longer match, so they must be accepted
 * again.
 */
export interface AcceptProviderTermsRequest {
    /**
     * @brief The provider whose terms are accepted.
     */
    provider_code: string;
}

export interface AcceptProviderTermsResponse {
    /**
     * @brief Whether the acceptance was recorded.
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

export const subjects = {
    accept_provider_terms_request: 'marketdata.v1.ops.accept_provider_terms',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    accept_provider_terms_request: true,
} as const;
