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
 * Template: domain_types.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * The tenor basis swap convention wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface TenorBasisSwapConvention {
    version: number;
    tenant_id: string;
    workspace_id: string;
    id: string;
    party_id: string;
    pay_index: string | null;
    pay_frequency: string | null;
    receive_index: string | null;
    receive_frequency: string | null;
    spread_on_rec: boolean | null;
    include_spread: boolean | null;
    sub_periods_coupon_type: string | null;
    pay_is_averaged: boolean | null;
    rec_is_averaged: boolean | null;
    long_index: string | null;
    long_pay_tenor: string | null;
    short_index: string | null;
    short_pay_tenor: string | null;
    spread_on_short: boolean | null;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
