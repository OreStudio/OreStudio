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
#include "ores.trading.core/service/instrument_option_premium_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

instrument_option_premium_service::instrument_option_premium_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::instrument_option_premium>
instrument_option_premium_service::list_option_premiums(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all instrument option premiums";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t instrument_option_premium_service::count_option_premiums() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total instrument option premiums count";
    return repo_.get_total_option_premium_count(ctx_);
}


std::optional<domain::instrument_option_premium>
instrument_option_premium_service::get_option_premium_at_version(const std::string& instrument_id,
                                                                 const std::string& sequence_number,
                                                                 std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting instrument option premium at version. "
                               << "instrument_id: " << instrument_id
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    return repo_.read_at_version(ctx_, instrument_id, sequence_number, version);
}

std::optional<domain::instrument_option_premium>
instrument_option_premium_service::get_option_premium(const std::string& instrument_id,
                                                      const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting instrument option premium. "
                               << "instrument_id: " << instrument_id
                               << " sequence_number: " << sequence_number;
    auto results = repo_.read_latest(ctx_, instrument_id, sequence_number);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void instrument_option_premium_service::save_option_premium(
    const domain::instrument_option_premium& v) {
    if (v.instrument_id.is_nil())
        throw std::invalid_argument("Instrument Option Premium instrument_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving instrument option premium. "
                               << "instrument_id: " << v.instrument_id
                               << " sequence_number: " << v.sequence_number;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved instrument option premium. "
                              << "instrument_id: " << v.instrument_id
                              << " sequence_number: " << v.sequence_number;
}

void instrument_option_premium_service::save_option_premiums(
    const std::vector<domain::instrument_option_premium>& option_premiums) {
    for (const auto& e : option_premiums) {
        if (e.instrument_id.is_nil())
            throw std::invalid_argument("Instrument Option Premium instrument_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << option_premiums.size()
                               << " instrument option premiums";
    auto ts = option_premiums;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void instrument_option_premium_service::delete_option_premium(const std::string& instrument_id,
                                                              const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing instrument option premium. "
                               << "instrument_id: " << instrument_id
                               << " sequence_number: " << sequence_number;
    repo_.remove(ctx_, instrument_id, sequence_number);
    BOOST_LOG_SEV(lg(), info) << "Removed instrument option premium. "
                              << "instrument_id: " << instrument_id
                              << " sequence_number: " << sequence_number;
}

void instrument_option_premium_service::delete_option_premiums(
    const std::vector<std::string>& instrument_ids,
    const std::vector<std::string>& sequence_numbers) {
    repo_.remove(ctx_, instrument_ids, sequence_numbers);
}

std::vector<domain::instrument_option_premium>
instrument_option_premium_service::get_option_premium_history(const std::string& instrument_id,
                                                              const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for instrument option premium. "
                               << "instrument_id: " << instrument_id
                               << " sequence_number: " << sequence_number;
    return repo_.read_all(ctx_, instrument_id, sequence_number);
}

}
