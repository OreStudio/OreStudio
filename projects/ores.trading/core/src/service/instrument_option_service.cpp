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
#include "ores.trading.core/service/instrument_option_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

instrument_option_service::instrument_option_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::instrument_option>
instrument_option_service::list_instrument_options(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all instrument options";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t instrument_option_service::count_instrument_options() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total instrument options count";
    return repo_.get_total_instrument_option_count(ctx_);
}


std::optional<domain::instrument_option>
instrument_option_service::get_instrument_option_at_version(const std::string& instrument_id,
                                                            std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting instrument option at version. "
                               << "instrument_id: " << instrument_id << " version: " << version;
    return repo_.read_at_version(ctx_, instrument_id, version);
}

std::optional<domain::instrument_option>
instrument_option_service::get_instrument_option(const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting instrument option. "
                               << "instrument_id: " << instrument_id;
    auto results = repo_.read_latest(ctx_, instrument_id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void instrument_option_service::save_instrument_option(const domain::instrument_option& v) {
    if (v.instrument_id.is_nil())
        throw std::invalid_argument("Instrument Option instrument_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving instrument option. "
                               << "instrument_id: " << v.instrument_id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved instrument option. "
                              << "instrument_id: " << v.instrument_id;
}

void instrument_option_service::save_instrument_options(
    const std::vector<domain::instrument_option>& instrument_options) {
    for (const auto& e : instrument_options) {
        if (e.instrument_id.is_nil())
            throw std::invalid_argument("Instrument Option instrument_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << instrument_options.size() << " instrument options";
    auto ts = instrument_options;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void instrument_option_service::delete_instrument_option(const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing instrument option. "
                               << "instrument_id: " << instrument_id;
    repo_.remove(ctx_, instrument_id);
    BOOST_LOG_SEV(lg(), info) << "Removed instrument option. "
                              << "instrument_id: " << instrument_id;
}

void instrument_option_service::delete_instrument_options(
    const std::vector<std::string>& instrument_ids) {
    repo_.remove(ctx_, instrument_ids);
}

std::vector<domain::instrument_option>
instrument_option_service::get_instrument_option_history(const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for instrument option. "
                               << "instrument_id: " << instrument_id;
    return repo_.read_all(ctx_, instrument_id);
}

}
