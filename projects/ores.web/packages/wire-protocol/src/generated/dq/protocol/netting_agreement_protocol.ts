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
import type { NettingAgreement } from '../domain/netting_agreement.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface NettingAgreementKey {
    agreement_number: string;
}

export interface NettingAgreementLookup {
    key: NettingAgreementKey;
    netting_agreement: NettingAgreement | null;
}

export interface NettingAgreementsFilter {
    agreement_number_one_of: string[] | null;
}

export interface NettingAgreementEvent {
    event_id: string;
    key: NettingAgreementKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListNettingAgreementsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: NettingAgreementsFilter | null;
}

export interface ListNettingAgreementsResponse {
    result: Result;
    netting_agreements: NettingAgreement[];
    total: number;
}

export interface GetNettingAgreementRequest {
    key: NettingAgreementKey;
}

export interface GetNettingAgreementResponse {
    result: Result;
    netting_agreement: NettingAgreement | null;
}

export interface GetManyNettingAgreementsRequest {
    keys: NettingAgreementKey[];
}

export interface GetManyNettingAgreementsResponse {
    result: Result;
    entries: NettingAgreementLookup[];
}

export const subjects = {
    list_netting_agreements_request: 'dq.v1.netting_agreements.list',
    get_netting_agreement_request: 'dq.v1.netting_agreements.get',
    get_many_netting_agreements_request: 'dq.v1.netting_agreements.get_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_netting_agreements_request: true,
    get_netting_agreement_request: true,
    get_many_netting_agreements_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'dq.v1.netting_agreements_events.created',
    updated: 'dq.v1.netting_agreements_events.updated',
    deleted: 'dq.v1.netting_agreements_events.deleted',
} as const;
