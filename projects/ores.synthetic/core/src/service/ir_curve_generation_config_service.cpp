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
#include "ores.synthetic.core/service/ir_curve_generation_config_service.hpp"
#include "ores.refdata.core/repository/ibor_index_convention_repository.hpp"
#include "ores.refdata.core/repository/overnight_index_convention_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <algorithm>
#include <cctype>
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::synthetic::service {

using namespace ores::logging;

namespace {

// (currency_code, index_family) has no single-column soft FK the codegen
// Insert-trigger Validations mechanism can express -- it must resolve
// against whichever of the two real convention tables backs that family,
// keyed on their shared "CCY-FAMILY" id shape (e.g. "USD-SOFR"). Validated
// here, not in SQL, matching the precedent already set by this entity's own
// tenor field (also a soft FK, also only resolved at the application layer).
void validate_index_family(ores::database::context ctx,
                           const std::string& currency_code,
                           const std::string& index_family) {
    auto upper = index_family;
    std::transform(upper.begin(), upper.end(), upper.begin(),
                   [](unsigned char c) { return std::toupper(c); });
    const auto id = currency_code + "-" + upper;

    refdata::repository::overnight_index_convention_repository overnight_repo;
    if (!overnight_repo.read_latest(ctx, id).empty())
        return;

    refdata::repository::ibor_index_convention_repository ibor_repo;
    if (!ibor_repo.read_latest(ctx, id).empty())
        return;

    throw std::invalid_argument(
        "Invalid (currency_code, index_family): no overnight_index_convention or "
        "ibor_index_convention found with id '" + id + "'.");
}

}

ir_curve_generation_config_service::ir_curve_generation_config_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::ir_curve_generation_config>
ir_curve_generation_config_service::list_ir_curve_generation_configs(std::uint32_t offset,
                                                                     std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all IR curve generation configs";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t ir_curve_generation_config_service::count_ir_curve_generation_configs() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total IR curve generation configs count";
    return repo_.get_total_ir_curve_generation_config_count(ctx_);
}


std::optional<domain::ir_curve_generation_config>
ir_curve_generation_config_service::get_ir_curve_generation_config_at_version(
    const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting IR curve generation config at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::ir_curve_generation_config>
ir_curve_generation_config_service::get_ir_curve_generation_config(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting IR curve generation config. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void ir_curve_generation_config_service::save_ir_curve_generation_config(
    const domain::ir_curve_generation_config& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("IR Curve Generation Config id cannot be empty.");
    validate_index_family(ctx_, v.currency_code, v.index_family);
    BOOST_LOG_SEV(lg(), debug) << "Saving IR curve generation config. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved IR curve generation config. " << "id: " << v.id;
}

void ir_curve_generation_config_service::save_ir_curve_generation_configs(
    const std::vector<domain::ir_curve_generation_config>& ir_curve_generation_configs) {
    for (const auto& e : ir_curve_generation_configs) {
        if (e.id.is_nil())
            throw std::invalid_argument("IR Curve Generation Config id cannot be empty.");
        validate_index_family(ctx_, e.currency_code, e.index_family);
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << ir_curve_generation_configs.size()
                               << " IR curve generation configs";
    auto ts = ir_curve_generation_configs;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void ir_curve_generation_config_service::delete_ir_curve_generation_config(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing IR curve generation config. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed IR curve generation config. " << "id: " << id;
}

void ir_curve_generation_config_service::delete_ir_curve_generation_configs(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::ir_curve_generation_config>
ir_curve_generation_config_service::get_ir_curve_generation_config_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for IR curve generation config. "
                               << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
