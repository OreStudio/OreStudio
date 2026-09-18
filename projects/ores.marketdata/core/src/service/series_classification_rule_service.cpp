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
 * Template: cpp_service.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.marketdata.core/service/series_classification_rule_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::marketdata::service {

using namespace ores::logging;

series_classification_rule_service::series_classification_rule_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::series_classification_rule>
series_classification_rule_service::list_rules(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all series classification rules";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t series_classification_rule_service::count_rules() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total series classification rules count";
    return repo_.get_total_rule_count(ctx_);
}


std::optional<domain::series_classification_rule>
series_classification_rule_service::get_rule_at_version(const std::string& series_type,
                                                        const std::string& metric,
                                                        std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting series classification rule at version. "
                               << "series_type: " << series_type << " metric: " << metric
                               << " version: " << version;
    return repo_.read_at_version(ctx_, series_type, metric, version);
}

std::optional<domain::series_classification_rule>
series_classification_rule_service::get_rule(const std::string& series_type,
                                             const std::string& metric) {
    BOOST_LOG_SEV(lg(), debug) << "Getting series classification rule. "
                               << "series_type: " << series_type << " metric: " << metric;
    auto results = repo_.read_latest(ctx_, series_type, metric);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void series_classification_rule_service::save_rule(const domain::series_classification_rule& v) {
    if (v.series_type.empty())
        throw std::invalid_argument("Series Classification Rule series_type cannot be empty.");
    if (v.metric.empty())
        throw std::invalid_argument("Series Classification Rule metric cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving series classification rule. "
                               << "series_type: " << v.series_type << " metric: " << v.metric;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved series classification rule. "
                              << "series_type: " << v.series_type << " metric: " << v.metric;
}

void series_classification_rule_service::save_rules(
    const std::vector<domain::series_classification_rule>& rules) {
    for (const auto& e : rules) {
        if (e.series_type.empty())
            throw std::invalid_argument("Series Classification Rule series_type cannot be empty.");
        if (e.metric.empty())
            throw std::invalid_argument("Series Classification Rule metric cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << rules.size() << " series classification rules";
    auto ts = rules;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void series_classification_rule_service::delete_rule(const std::string& series_type,
                                                     const std::string& metric) {
    BOOST_LOG_SEV(lg(), debug) << "Removing series classification rule. "
                               << "series_type: " << series_type << " metric: " << metric;
    repo_.remove(ctx_, series_type, metric);
    BOOST_LOG_SEV(lg(), info) << "Removed series classification rule. "
                              << "series_type: " << series_type << " metric: " << metric;
}

void series_classification_rule_service::delete_rules(const std::vector<std::string>& series_types,
                                                      const std::vector<std::string>& metrics) {
    repo_.remove(ctx_, series_types, metrics);
}

std::vector<domain::series_classification_rule>
series_classification_rule_service::get_rule_history(const std::string& series_type,
                                                     const std::string& metric) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for series classification rule. "
                               << "series_type: " << series_type << " metric: " << metric;
    return repo_.read_all(ctx_, series_type, metric);
}

}
