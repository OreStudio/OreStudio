/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 * Template: cpp_domain_type_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.reporting.core/repository/report_run_setup_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.reporting.api/domain/report_run_setup_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::reporting::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::report_run_setup report_run_setup_mapper::map(const report_run_setup_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::report_run_setup r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.report_definition_id = boost::lexical_cast<boost::uuids::uuid>(v.report_definition_id);

    r.asof_date = v.asof_date;
    r.accrual_date = v.accrual_date;
    r.input_path = v.input_path;
    r.input_path_market = v.input_path_market;
    r.input_path_portfolio = v.input_path_portfolio;
    r.output_path = v.output_path;
    r.log_file = v.log_file;
    r.log_mask = v.log_mask;
    r.n_threads = v.n_threads;
    r.observation_model = v.observation_model;
    r.base_currency = v.base_currency;
    r.date_calendar = v.date_calendar;
    r.date_convention = v.date_convention;
    r.fixing_cutoff = v.fixing_cutoff;
    r.continue_on_error = v.continue_on_error;
    r.build_failed_trades = v.build_failed_trades;
    r.imply_todays_fixings = v.imply_todays_fixings;
    r.ignore_fixing_lag = v.ignore_fixing_lag;
    r.include_todays_cash_flows = v.include_todays_cash_flows;
    r.include_reference_date_events = v.include_reference_date_events;
    r.lazy_market_building = v.lazy_market_building;
    r.enrich_index_fixings = v.enrich_index_fixings;
    r.use_analytics = v.use_analytics;
    r.csv_comment_report_header = v.csv_comment_report_header;
    r.default_mapping_to_identity = v.default_mapping_to_identity;
    r.portfolio_recurse_into_sub_directories = v.portfolio_recurse_into_sub_directories;
    r.curve_config_file = v.curve_config_file;
    r.conventions_file = v.conventions_file;
    r.market_config_file = v.market_config_file;
    r.pricing_engines_file = v.pricing_engines_file;
    r.pricing_engines_file_scenario = v.pricing_engines_file_scenario;
    r.portfolio_file = v.portfolio_file;
    r.market_data_file = v.market_data_file;
    r.market_data_mapping_file = v.market_data_mapping_file;
    r.fixing_data_file = v.fixing_data_file;
    r.fixing_data_mapping_file = v.fixing_data_mapping_file;
    r.calendar_adjustment = v.calendar_adjustment;
    r.currency_configuration = v.currency_configuration;
    r.reference_data_file = v.reference_data_file;
    r.counterparty_file = v.counterparty_file;
    r.script_library = v.script_library;
    r.ibor_fallback_config = v.ibor_fallback_config;
    r.additional_results = v.additional_results;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

report_run_setup_entity report_run_setup_mapper::map(const domain::report_run_setup& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    report_run_setup_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.report_definition_id = boost::uuids::to_string(v.report_definition_id);

    r.asof_date = v.asof_date;
    r.accrual_date = v.accrual_date;
    r.input_path = v.input_path;
    r.input_path_market = v.input_path_market;
    r.input_path_portfolio = v.input_path_portfolio;
    r.output_path = v.output_path;
    r.log_file = v.log_file;
    r.log_mask = v.log_mask;
    r.n_threads = v.n_threads;
    r.observation_model = v.observation_model;
    r.base_currency = v.base_currency;
    r.date_calendar = v.date_calendar;
    r.date_convention = v.date_convention;
    r.fixing_cutoff = v.fixing_cutoff;
    r.continue_on_error = v.continue_on_error;
    r.build_failed_trades = v.build_failed_trades;
    r.imply_todays_fixings = v.imply_todays_fixings;
    r.ignore_fixing_lag = v.ignore_fixing_lag;
    r.include_todays_cash_flows = v.include_todays_cash_flows;
    r.include_reference_date_events = v.include_reference_date_events;
    r.lazy_market_building = v.lazy_market_building;
    r.enrich_index_fixings = v.enrich_index_fixings;
    r.use_analytics = v.use_analytics;
    r.csv_comment_report_header = v.csv_comment_report_header;
    r.default_mapping_to_identity = v.default_mapping_to_identity;
    r.portfolio_recurse_into_sub_directories = v.portfolio_recurse_into_sub_directories;
    r.curve_config_file = v.curve_config_file;
    r.conventions_file = v.conventions_file;
    r.market_config_file = v.market_config_file;
    r.pricing_engines_file = v.pricing_engines_file;
    r.pricing_engines_file_scenario = v.pricing_engines_file_scenario;
    r.portfolio_file = v.portfolio_file;
    r.market_data_file = v.market_data_file;
    r.market_data_mapping_file = v.market_data_mapping_file;
    r.fixing_data_file = v.fixing_data_file;
    r.fixing_data_mapping_file = v.fixing_data_mapping_file;
    r.calendar_adjustment = v.calendar_adjustment;
    r.currency_configuration = v.currency_configuration;
    r.reference_data_file = v.reference_data_file;
    r.counterparty_file = v.counterparty_file;
    r.script_library = v.script_library;
    r.ibor_fallback_config = v.ibor_fallback_config;
    r.additional_results = v.additional_results;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::report_run_setup>
report_run_setup_mapper::map(const std::vector<report_run_setup_entity>& v) {
    return map_vector<report_run_setup_entity, domain::report_run_setup>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<report_run_setup_entity>
report_run_setup_mapper::map(const std::vector<domain::report_run_setup>& v) {
    return map_vector<domain::report_run_setup, report_run_setup_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
