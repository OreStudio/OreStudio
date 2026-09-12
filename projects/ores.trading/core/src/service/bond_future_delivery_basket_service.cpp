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
#include "ores.trading.core/service/bond_future_delivery_basket_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

bond_future_delivery_basket_service::bond_future_delivery_basket_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::bond_future_delivery_basket>
bond_future_delivery_basket_service::list_delivery_basket_ids(std::uint32_t offset,
                                                              std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all bond future delivery basket identifiers";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t bond_future_delivery_basket_service::count_delivery_basket_ids() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total bond future delivery basket identifiers count";
    return repo_.get_total_delivery_basket_id_count(ctx_);
}


std::optional<domain::bond_future_delivery_basket>
bond_future_delivery_basket_service::get_delivery_basket_id_at_version(
    const std::string& instrument_id, const std::string& sequence_number, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting bond future delivery basket identifier at version. "
                               << "instrument_id: " << instrument_id
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    return repo_.read_at_version(ctx_, instrument_id, sequence_number, version);
}

std::optional<domain::bond_future_delivery_basket>
bond_future_delivery_basket_service::get_delivery_basket_id(const std::string& instrument_id,
                                                            const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting bond future delivery basket identifier. "
                               << "instrument_id: " << instrument_id
                               << " sequence_number: " << sequence_number;
    auto results = repo_.read_latest(ctx_, instrument_id, sequence_number);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void bond_future_delivery_basket_service::save_delivery_basket_id(
    const domain::bond_future_delivery_basket& v) {
    if (v.instrument_id.is_nil())
        throw std::invalid_argument("Bond Future Delivery Basket instrument_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving bond future delivery basket identifier. "
                               << "instrument_id: " << v.instrument_id
                               << " sequence_number: " << v.sequence_number;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved bond future delivery basket identifier. "
                              << "instrument_id: " << v.instrument_id
                              << " sequence_number: " << v.sequence_number;
}

void bond_future_delivery_basket_service::save_delivery_basket_ids(
    const std::vector<domain::bond_future_delivery_basket>& delivery_basket_ids) {
    for (const auto& e : delivery_basket_ids) {
        if (e.instrument_id.is_nil())
            throw std::invalid_argument(
                "Bond Future Delivery Basket instrument_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << delivery_basket_ids.size()
                               << " bond future delivery basket identifiers";
    auto ts = delivery_basket_ids;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void bond_future_delivery_basket_service::delete_delivery_basket_id(
    const std::string& instrument_id, const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing bond future delivery basket identifier. "
                               << "instrument_id: " << instrument_id
                               << " sequence_number: " << sequence_number;
    repo_.remove(ctx_, instrument_id, sequence_number);
    BOOST_LOG_SEV(lg(), info) << "Removed bond future delivery basket identifier. "
                              << "instrument_id: " << instrument_id
                              << " sequence_number: " << sequence_number;
}

void bond_future_delivery_basket_service::delete_delivery_basket_ids(
    const std::vector<std::string>& instrument_ids,
    const std::vector<std::string>& sequence_numbers) {
    repo_.remove(ctx_, instrument_ids, sequence_numbers);
}

std::vector<domain::bond_future_delivery_basket>
bond_future_delivery_basket_service::get_delivery_basket_id_history(
    const std::string& instrument_id, const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for bond future delivery basket identifier. "
                               << "instrument_id: " << instrument_id
                               << " sequence_number: " << sequence_number;
    return repo_.read_all(ctx_, instrument_id, sequence_number);
}

}
