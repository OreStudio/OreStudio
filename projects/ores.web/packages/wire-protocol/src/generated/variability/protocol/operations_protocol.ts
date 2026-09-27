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
import type { Result } from '../../../utility/protocol.js';

export interface ClearBootstrapModeRequest {}

export interface ClearBootstrapModeResponse {
    result: Result;
}

export interface CompletePartyOnboardingRequest {
    /*
     * The party being onboarded, which is not the caller's own party.
     */
    party_id: string;
}

export interface CompletePartyOnboardingResponse {
    result: Result;
}

export const subjects = {
    clear_bootstrap_mode_request: 'variability.v1.ops.clear_bootstrap_mode',
    complete_party_onboarding_request: 'variability.v1.ops.complete_party_onboarding',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    clear_bootstrap_mode_request: true,
    complete_party_onboarding_request: true,
} as const;
