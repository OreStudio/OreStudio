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
 * The report run setup wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface ReportRunSetup {
    version: number;
    tenant_id: string;
    id: string;
    report_definition_id: string;
    party_id: string;
    asof_date: string | null;
    accrual_date: string | null;
    input_path: string | null;
    input_path_market: string | null;
    input_path_portfolio: string | null;
    output_path: string | null;
    log_file: string | null;
    log_mask: number | null;
    n_threads: number | null;
    observation_model: string | null;
    base_currency: string | null;
    date_calendar: string | null;
    date_convention: string | null;
    fixing_cutoff: string | null;
    continue_on_error: string | null;
    build_failed_trades: string | null;
    imply_todays_fixings: string | null;
    ignore_fixing_lag: number | null;
    include_todays_cash_flows: string | null;
    include_reference_date_events: string | null;
    lazy_market_building: string | null;
    enrich_index_fixings: string | null;
    use_analytics: string | null;
    csv_comment_report_header: string | null;
    default_mapping_to_identity: string | null;
    portfolio_recurse_into_sub_directories: string | null;
    curve_config_file: string | null;
    conventions_file: string | null;
    market_config_file: string | null;
    pricing_engines_file: string | null;
    pricing_engines_file_scenario: string | null;
    portfolio_file: string | null;
    market_data_file: string | null;
    market_data_mapping_file: string | null;
    fixing_data_file: string | null;
    fixing_data_mapping_file: string | null;
    calendar_adjustment: string | null;
    currency_configuration: string | null;
    reference_data_file: string | null;
    counterparty_file: string | null;
    script_library: string | null;
    ibor_fallback_config: string | null;
    additional_results: string | null;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
