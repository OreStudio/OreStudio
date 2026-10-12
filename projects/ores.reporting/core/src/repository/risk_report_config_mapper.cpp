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
#include "ores.reporting.core/repository/risk_report_config_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.reporting.api/domain/risk_report_config.hpp"
#include "ores.reporting.api/domain/risk_report_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/risk_report_config_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <vector>

namespace ores::reporting::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::risk_report_config risk_report_config_mapper::map(const risk_report_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::risk_report_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);


    r.name = v.name;

    r.description = v.description.value_or("");
    r.configuration_id = boost::lexical_cast<boost::uuids::uuid>(v.configuration_id);
    r.base_currency = v.base_currency;
    r.observation_model = v.observation_model;
    r.n_threads = v.n_threads;
    r.market_data_type = v.market_data_type;
    r.market_data_date = v.market_data_date.value_or("");
    r.accrual_date = v.accrual_date;
    r.fixing_cutoff = v.fixing_cutoff;
    r.input_path = v.input_path;
    r.input_path_market = v.input_path_market;
    r.input_path_portfolio = v.input_path_portfolio;
    r.output_path = v.output_path;
    r.log_file = v.log_file;
    r.date_calendar = v.date_calendar;
    r.date_convention = v.date_convention;
    r.continue_on_error = v.continue_on_error;
    r.build_failed_trades = v.build_failed_trades;
    r.imply_todays_fixings = v.imply_todays_fixings;
    r.include_todays_cash_flows = v.include_todays_cash_flows;
    r.include_reference_date_events = v.include_reference_date_events;
    r.lazy_market_building = v.lazy_market_building;
    r.enrich_index_fixings = v.enrich_index_fixings;
    r.use_analytics = v.use_analytics;
    r.csv_comment_report_header = v.csv_comment_report_header;
    r.default_mapping_to_identity = v.default_mapping_to_identity;
    r.portfolio_recurse_into_sub_directories = v.portfolio_recurse_into_sub_directories;
    r.log_mask = v.log_mask;
    r.ignore_fixing_lag = v.ignore_fixing_lag;
    r.additional_results = v.additional_results;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

risk_report_config_entity risk_report_config_mapper::map(const domain::risk_report_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    risk_report_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);


    r.name = v.name;

    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.configuration_id = boost::uuids::to_string(v.configuration_id);
    r.base_currency = v.base_currency;
    r.observation_model = v.observation_model;
    r.n_threads = v.n_threads;
    r.market_data_type = v.market_data_type;
    r.market_data_date =
        v.market_data_date.empty() ? std::nullopt : std::optional(v.market_data_date);
    r.accrual_date = v.accrual_date;
    r.fixing_cutoff = v.fixing_cutoff;
    r.input_path = v.input_path;
    r.input_path_market = v.input_path_market;
    r.input_path_portfolio = v.input_path_portfolio;
    r.output_path = v.output_path;
    r.log_file = v.log_file;
    r.date_calendar = v.date_calendar;
    r.date_convention = v.date_convention;
    r.continue_on_error = v.continue_on_error;
    r.build_failed_trades = v.build_failed_trades;
    r.imply_todays_fixings = v.imply_todays_fixings;
    r.include_todays_cash_flows = v.include_todays_cash_flows;
    r.include_reference_date_events = v.include_reference_date_events;
    r.lazy_market_building = v.lazy_market_building;
    r.enrich_index_fixings = v.enrich_index_fixings;
    r.use_analytics = v.use_analytics;
    r.csv_comment_report_header = v.csv_comment_report_header;
    r.default_mapping_to_identity = v.default_mapping_to_identity;
    r.portfolio_recurse_into_sub_directories = v.portfolio_recurse_into_sub_directories;
    r.log_mask = v.log_mask;
    r.ignore_fixing_lag = v.ignore_fixing_lag;
    r.additional_results = v.additional_results;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::risk_report_config>
risk_report_config_mapper::map(const std::vector<risk_report_config_entity>& v) {
    return map_vector<risk_report_config_entity, domain::risk_report_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<risk_report_config_entity>
risk_report_config_mapper::map(const std::vector<domain::risk_report_config>& v) {
    return map_vector<domain::risk_report_config, risk_report_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
