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
#include "ores.refdata.core/service/ir_curve_bootstrap_pillar_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

ir_curve_bootstrap_pillar_service::ir_curve_bootstrap_pillar_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::ir_curve_bootstrap_pillar>
ir_curve_bootstrap_pillar_service::list_pillars(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all IR curve bootstrap pillars";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t ir_curve_bootstrap_pillar_service::count_pillars() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total IR curve bootstrap pillars count";
    return repo_.get_total_pillar_count(ctx_);
}


std::optional<domain::ir_curve_bootstrap_pillar>
ir_curve_bootstrap_pillar_service::get_pillar_at_version(const std::string& id,
                                                         std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting IR curve bootstrap pillar at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::ir_curve_bootstrap_pillar>
ir_curve_bootstrap_pillar_service::get_pillar(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting IR curve bootstrap pillar. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void ir_curve_bootstrap_pillar_service::save_pillar(const domain::ir_curve_bootstrap_pillar& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("IR Curve Bootstrap Pillar id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving IR curve bootstrap pillar. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved IR curve bootstrap pillar. " << "id: " << v.id;
}

void ir_curve_bootstrap_pillar_service::save_pillars(
    const std::vector<domain::ir_curve_bootstrap_pillar>& pillars) {
    for (const auto& e : pillars) {
        if (e.id.is_nil())
            throw std::invalid_argument("IR Curve Bootstrap Pillar id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << pillars.size() << " IR curve bootstrap pillars";
    auto ts = pillars;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void ir_curve_bootstrap_pillar_service::delete_pillar(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing IR curve bootstrap pillar. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed IR curve bootstrap pillar. " << "id: " << id;
}

void ir_curve_bootstrap_pillar_service::delete_pillars(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::ir_curve_bootstrap_pillar>
ir_curve_bootstrap_pillar_service::get_pillar_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for IR curve bootstrap pillar. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
