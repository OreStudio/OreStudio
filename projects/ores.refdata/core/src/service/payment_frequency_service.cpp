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
#include "ores.refdata.core/service/payment_frequency_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

payment_frequency_service::payment_frequency_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::payment_frequency>
payment_frequency_service::list_payment_frequencies(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all payment frequencies";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t payment_frequency_service::count_payment_frequencies() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total payment frequencies count";
    return repo_.get_total_payment_frequency_count(ctx_);
}

std::optional<domain::payment_frequency>
payment_frequency_service::get_payment_frequency_at_version(const std::string& code,
                                                            std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting payment frequency at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::payment_frequency>
payment_frequency_service::get_payment_frequency(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting payment frequency: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void payment_frequency_service::save_payment_frequency(const domain::payment_frequency& v) {
    if (v.code.empty())
        throw std::invalid_argument("Payment Frequency code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving payment frequency: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved payment frequency: " << v.code;
}

void payment_frequency_service::save_payment_frequencies(
    const std::vector<domain::payment_frequency>& payment_frequencies) {
    for (const auto& e : payment_frequencies)
        if (e.code.empty())
            throw std::invalid_argument("Payment Frequency code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << payment_frequencies.size() << " payment frequencies";
    auto ts = payment_frequencies;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void payment_frequency_service::delete_payment_frequency(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing payment frequency: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed payment frequency: " << code;
}

void payment_frequency_service::delete_payment_frequencies(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::payment_frequency>
payment_frequency_service::get_payment_frequency_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for payment frequency: " << code;
    return repo_.read_all(ctx_, code);
}

}
