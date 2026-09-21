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
import type { DiffResult } from '../domain/diff_result.js';
import type { FieldValue } from '../domain/field_value.js';


/**
 * @brief One rendered, diffed version of an entity's history.
 *
 * fields is the full render of this version, in mapper order -- the
 * detail panel's need for complete values. changes is the field-level
 * diff, with intra-value spans, against the previous version; empty for
 * the oldest version.
 */
export interface EntityHistoryVersion {
    version: number;
    modified_by: string;
    recorded_at: string;
    fields: FieldValue[];
    changes: DiffResult;
}

/**
 * @brief The one generic history request every entity shares.
 *
 * entity_type is the dispatch key (e.g. "ores.refdata.currency"); entity_id
 * is the entity's own primary key rendered as a string, since key shapes vary
 * across entities.
 *
 * The subject names the component that OWNS the entity, which this model
 * cannot know: the same request reaches iam.v1.history.get for an IAM entity
 * and refdata.v1.history.get for a refdata one. So the segment is left open
 * and history_subject_for() derives it -- one rule, read by the service that
 * subscribes and by every client that sends.
 */
export interface GetEntityHistoryRequest {
    entity_type: string;
    entity_id: string;
}

export interface GetEntityHistoryResponse {
    versions: EntityHistoryVersion[];
    success: boolean;
    message: string;
}

export const subjects = {
} as const;
/**
 * The subject these messages are addressed at, derived from the dispatch key of
 * the resource being asked about. The pattern is stated once, in the model, and
 * this is its only derivation, so a client and a service cannot disagree about
 * a subject that is not a constant.
 *
 * The hole is the component segment, read from the dispatch key, which is
 * always "<product>.<component>.<entity>".
 */
export function historySubjectFor(entityType: string): string {
    const parts = entityType.split('.');
    const component = parts.length > 2 ? parts[1] : 'unknown';
    return `${component}.v1.history.get`;
}
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_entity_history_request: true,
} as const;
