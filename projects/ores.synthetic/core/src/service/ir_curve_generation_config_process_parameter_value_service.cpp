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
#include "ores.synthetic.core/service/ir_curve_generation_config_process_parameter_value_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::synthetic::service {

using namespace ores::logging;

ir_curve_generation_config_process_parameter_value_service::
    ir_curve_generation_config_process_parameter_value_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::ir_curve_generation_config_process_parameter_value>
ir_curve_generation_config_process_parameter_value_service::list_process_parameter_values(
    std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all IR curve generation config process parameter values";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t
ir_curve_generation_config_process_parameter_value_service::count_process_parameter_values() {
    BOOST_LOG_SEV(lg(), debug)
        << "Getting total IR curve generation config process parameter values count";
    return repo_.get_total_process_parameter_value_count(ctx_);
}


std::optional<domain::ir_curve_generation_config_process_parameter_value>
ir_curve_generation_config_process_parameter_value_service::get_process_parameter_value_at_version(
    const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug)
        << "Getting IR curve generation config process parameter value at version. " << "id: " << id
        << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::ir_curve_generation_config_process_parameter_value>
ir_curve_generation_config_process_parameter_value_service::get_process_parameter_value(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting IR curve generation config process parameter value. "
                               << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void ir_curve_generation_config_process_parameter_value_service::save_process_parameter_value(
    const domain::ir_curve_generation_config_process_parameter_value& v) {
    if (v.id.is_nil())
        throw std::invalid_argument(
            "IR Curve Generation Config Process Parameter Value id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving IR curve generation config process parameter value. "
                               << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved IR curve generation config process parameter value. "
                              << "id: " << v.id;
}

void ir_curve_generation_config_process_parameter_value_service::save_process_parameter_values(
    const std::vector<domain::ir_curve_generation_config_process_parameter_value>&
        process_parameter_values) {
    for (const auto& e : process_parameter_values) {
        if (e.id.is_nil())
            throw std::invalid_argument(
                "IR Curve Generation Config Process Parameter Value id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << process_parameter_values.size()
                               << " IR curve generation config process parameter values";
    auto ts = process_parameter_values;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void ir_curve_generation_config_process_parameter_value_service::delete_process_parameter_value(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing IR curve generation config process parameter value. "
                               << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed IR curve generation config process parameter value. "
                              << "id: " << id;
}

void ir_curve_generation_config_process_parameter_value_service::delete_process_parameter_values(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::ir_curve_generation_config_process_parameter_value>
ir_curve_generation_config_process_parameter_value_service::get_process_parameter_value_history(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Getting history for IR curve generation config process parameter value. "
        << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
