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
#include "ores.synthetic.core/service/ir_curve_template_entry_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::synthetic::service {

using namespace ores::logging;

ir_curve_template_entry_service::ir_curve_template_entry_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::ir_curve_template_entry>
ir_curve_template_entry_service::list_ir_curve_template_entries(std::uint32_t offset,
                                                                std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all IR curve template entries";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t ir_curve_template_entry_service::count_ir_curve_template_entries() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total IR curve template entries count";
    return repo_.get_total_ir_curve_template_entry_count(ctx_);
}


std::optional<domain::ir_curve_template_entry>
ir_curve_template_entry_service::get_ir_curve_template_entry_at_version(const std::string& id,
                                                                        std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting IR curve template entry at version: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::ir_curve_template_entry>
ir_curve_template_entry_service::get_ir_curve_template_entry(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting IR curve template entry: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void ir_curve_template_entry_service::save_ir_curve_template_entry(
    const domain::ir_curve_template_entry& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("IR Curve Template Entry id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving IR curve template entry: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved IR curve template entry: " << v.id;
}

void ir_curve_template_entry_service::save_ir_curve_template_entries(
    const std::vector<domain::ir_curve_template_entry>& ir_curve_template_entries) {
    for (const auto& e : ir_curve_template_entries)
        if (e.id.is_nil())
            throw std::invalid_argument("IR Curve Template Entry id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << ir_curve_template_entries.size()
                               << " IR curve template entries";
    auto ts = ir_curve_template_entries;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void ir_curve_template_entry_service::delete_ir_curve_template_entry(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing IR curve template entry: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed IR curve template entry: " << id;
}

void ir_curve_template_entry_service::delete_ir_curve_template_entries(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::ir_curve_template_entry>
ir_curve_template_entry_service::get_ir_curve_template_entry_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for IR curve template entry: " << id;
    return repo_.read_all(ctx_, id);
}

}
