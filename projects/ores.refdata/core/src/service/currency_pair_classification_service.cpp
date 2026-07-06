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
#include "ores.refdata.core/service/currency_pair_classification_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

currency_pair_classification_service::currency_pair_classification_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::currency_pair_classification>
currency_pair_classification_service::list_classifications(std::uint32_t offset,
                                                           std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all currency pair classifications";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t currency_pair_classification_service::count_classifications() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total currency pair classifications count";
    return repo_.get_total_classification_count(ctx_);
}


std::optional<domain::currency_pair_classification>
currency_pair_classification_service::get_classification(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting currency pair classification: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void currency_pair_classification_service::save_classification(
    const domain::currency_pair_classification& v) {
    if (v.code.empty())
        throw std::invalid_argument("Currency Pair Classification code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving currency pair classification: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved currency pair classification: " << v.code;
}

void currency_pair_classification_service::save_classifications(
    const std::vector<domain::currency_pair_classification>& classifications) {
    for (const auto& e : classifications)
        if (e.code.empty())
            throw std::invalid_argument("Currency Pair Classification code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << classifications.size()
                               << " currency pair classifications";
    auto ts = classifications;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void currency_pair_classification_service::delete_classification(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency pair classification: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed currency pair classification: " << code;
}

void currency_pair_classification_service::delete_classifications(
    const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::currency_pair_classification>
currency_pair_classification_service::get_classification_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for currency pair classification: " << code;
    return repo_.read_all(ctx_, code);
}

}
