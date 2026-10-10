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
 * The CSA wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface Csa {
    version: number;
    tenant_id: string;
    id: string;
    netting_set_id: string;
    party_id: string;
    is_active: boolean;
    bilateral: string | null;
    csa_currency: string | null;
    index_name: string | null;
    threshold_pay: number | null;
    threshold_receive: number | null;
    minimum_transfer_amount_pay: number | null;
    minimum_transfer_amount_receive: number | null;
    independent_amount_held: number | null;
    independent_amount_type: string | null;
    call_frequency: string | null;
    post_frequency: string | null;
    margin_period_of_risk: string | null;
    collateral_compounding_spread_receive: number | null;
    collateral_compounding_spread_pay: number | null;
    apply_initial_margin: boolean | null;
    initial_margin_type: string | null;
    calculate_im_amount: boolean | null;
    calculate_vm_amount: boolean | null;
    non_exempt_im_regulations: string | null;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
