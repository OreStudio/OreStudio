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
import type { RiskParticipationAgreementInstrument } from '../domain/risk_participation_agreement_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RiskParticipationAgreementInstrumentKey {
    trade_id: string;
}

export interface RiskParticipationAgreementInstrumentWrite {
    trade_id: string;
    trade_activity_id: string;
    credit_curve_id: string;
    issuer_id: string | null;
    recovery_rate: number | null;
    participation_rate: number;
    settles_accrual: boolean | null;
    naked_option: boolean | null;
}

export interface RiskParticipationAgreementInstrumentChange {
    write: RiskParticipationAgreementInstrumentWrite;
    precondition: Precondition;
}

export interface RiskParticipationAgreementInstrumentRemoval {
    key: RiskParticipationAgreementInstrumentKey;
    precondition: Precondition;
}

export interface RiskParticipationAgreementInstrumentLookup {
    key: RiskParticipationAgreementInstrumentKey;
    risk_participation_agreement_instrument: RiskParticipationAgreementInstrument | null;
}

export interface RiskParticipationAgreementInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface RiskParticipationAgreementInstrumentEvent {
    event_id: string;
    key: RiskParticipationAgreementInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RiskParticipationAgreementInstrumentVersionKey {
    risk_participation_agreement_instrument: RiskParticipationAgreementInstrumentKey;
    version: number;
}

export interface RiskParticipationAgreementInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRiskParticipationAgreementInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RiskParticipationAgreementInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListRiskParticipationAgreementInstrumentsResponse {
    result: Result;
    risk_participation_agreement_instruments: RiskParticipationAgreementInstrument[];
    total: number;
}

export interface GetRiskParticipationAgreementInstrumentRequest {
    key: RiskParticipationAgreementInstrumentKey;
}

export interface GetRiskParticipationAgreementInstrumentResponse {
    result: Result;
    risk_participation_agreement_instrument: RiskParticipationAgreementInstrument | null;
}

export interface GetManyRiskParticipationAgreementInstrumentsRequest {
    keys: RiskParticipationAgreementInstrumentKey[];
}

export interface GetManyRiskParticipationAgreementInstrumentsResponse {
    result: Result;
    entries: RiskParticipationAgreementInstrumentLookup[];
}

export interface PutRiskParticipationAgreementInstrumentRequest {
    change: RiskParticipationAgreementInstrumentChange;
    intent: ChangeIntent;
}

export interface PutRiskParticipationAgreementInstrumentResponse {
    result: Result;
    risk_participation_agreement_instrument: RiskParticipationAgreementInstrument | null;
}

export interface PutManyRiskParticipationAgreementInstrumentsRequest {
    changes: RiskParticipationAgreementInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyRiskParticipationAgreementInstrumentsResponse {
    result: Result;
    risk_participation_agreement_instruments: RiskParticipationAgreementInstrument[];
}

export interface DeleteRiskParticipationAgreementInstrumentRequest {
    removal: RiskParticipationAgreementInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteRiskParticipationAgreementInstrumentResponse {
    result: Result;
}

export interface DeleteManyRiskParticipationAgreementInstrumentsRequest {
    removals: RiskParticipationAgreementInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRiskParticipationAgreementInstrumentsResponse {
    result: Result;
}

export interface ListRiskParticipationAgreementInstrumentVersionsRequest {
    key: RiskParticipationAgreementInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RiskParticipationAgreementInstrumentVersionsFilter | null;
}

export interface ListRiskParticipationAgreementInstrumentVersionsResponse {
    result: Result;
    versions: RiskParticipationAgreementInstrument[];
    total: number;
}

export interface GetRiskParticipationAgreementInstrumentVersionRequest {
    key: RiskParticipationAgreementInstrumentVersionKey;
}

export interface GetRiskParticipationAgreementInstrumentVersionResponse {
    result: Result;
    version: RiskParticipationAgreementInstrument | null;
}

export const subjects = {
    list_risk_participation_agreement_instruments_request:
        'trading.v1.risk_participation_agreement_instruments.list',
    get_risk_participation_agreement_instrument_request:
        'trading.v1.risk_participation_agreement_instruments.get',
    get_many_risk_participation_agreement_instruments_request:
        'trading.v1.risk_participation_agreement_instruments.get_many',
    put_risk_participation_agreement_instrument_request:
        'trading.v1.risk_participation_agreement_instruments.put',
    put_many_risk_participation_agreement_instruments_request:
        'trading.v1.risk_participation_agreement_instruments.put_many',
    delete_risk_participation_agreement_instrument_request:
        'trading.v1.risk_participation_agreement_instruments.delete',
    delete_many_risk_participation_agreement_instruments_request:
        'trading.v1.risk_participation_agreement_instruments.delete_many',
    list_risk_participation_agreement_instrument_versions_request:
        'trading.v1.risk_participation_agreement_instruments_versions.list',
    get_risk_participation_agreement_instrument_version_request:
        'trading.v1.risk_participation_agreement_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_risk_participation_agreement_instruments_request: true,
    get_risk_participation_agreement_instrument_request: true,
    get_many_risk_participation_agreement_instruments_request: true,
    put_risk_participation_agreement_instrument_request: true,
    put_many_risk_participation_agreement_instruments_request: true,
    delete_risk_participation_agreement_instrument_request: true,
    delete_many_risk_participation_agreement_instruments_request: true,
    list_risk_participation_agreement_instrument_versions_request: true,
    get_risk_participation_agreement_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.risk_participation_agreement_instruments_events.created',
    updated: 'trading.v1.risk_participation_agreement_instruments_events.updated',
    deleted: 'trading.v1.risk_participation_agreement_instruments_events.deleted',
} as const;
