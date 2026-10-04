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
#include "ores.dq.core/service/netting_agreement_service.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <algorithm>
#include <cstdint>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using ores::service::messaging::stamp;

namespace ores::dq::service {

using namespace ores::logging;

netting_agreement_service::netting_agreement_service(context ctx)
    : ctx_(std::move(ctx)) {}
namespace {

/**
 * @brief The current row a key names, or an empty vector when there is none.
 *
 * A key record carries each column with the column's own type, and the
 * repository takes the text form every one of its key parameters shares, so
 * the conversion lives here rather than at every call site.
 *
 * The key record carries the key the model declares, which is the one a caller
 * holds. When that is not the storage key the row is found by it and the
 * repository's storage-key read is not used at all.
 */
std::vector<domain::netting_agreement> read_one(repository::netting_agreement_repository& repo,
                                                const ores::database::context& ctx,
                                                const messaging::netting_agreement_key& key) {
    return repo.read_latest(ctx, key.agreement_number);
}

} // namespace

messaging::list_netting_agreements_response netting_agreement_service::list_netting_agreements(
    const messaging::list_netting_agreements_request& request) {
    messaging::list_netting_agreements_response response;
    if (!request.order.field.empty() &&
        !repository::netting_agreement_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of netting agreements cannot be ordered by " + request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->agreement_number_one_of &&
        request.filter->agreement_number_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message =
            "The filter lists more than 1000 values in agreement_number_one_of.";
        return response;
    }
    response.netting_agreements =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_netting_agreement_count(ctx_, request.filter);
    return response;
}

messaging::get_netting_agreement_response netting_agreement_service::get_netting_agreement(
    const messaging::get_netting_agreement_request& request) {
    messaging::get_netting_agreement_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.netting_agreement = std::move(found.front());
    return response;
}

messaging::get_many_netting_agreements_response
netting_agreement_service::get_many_netting_agreements(
    const messaging::get_many_netting_agreements_request& request) {
    messaging::get_many_netting_agreements_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::netting_agreement_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.netting_agreement = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}


std::vector<domain::netting_agreement>
netting_agreement_service::list_netting_agreements(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all netting agreements";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t netting_agreement_service::count_netting_agreements() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total netting agreements count";
    return repo_.get_total_netting_agreement_count(ctx_);
}


std::optional<domain::netting_agreement>
netting_agreement_service::get_netting_agreement(const std::string& agreement_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting netting agreement. "
                               << "agreement_number: " << agreement_number;
    auto results = repo_.read_latest(ctx_, agreement_number);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::netting_agreement> netting_agreement_service::get_netting_agreements(
    const std::vector<std::string>& agreement_numbers) {
    return repo_.read_latest(ctx_, agreement_numbers);
}

void netting_agreement_service::save_netting_agreement(const domain::netting_agreement& v) {
    if (v.agreement_number.empty())
        throw std::invalid_argument("Netting Agreement agreement_number cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving netting agreement. "
                               << "agreement_number: " << v.agreement_number;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved netting agreement. "
                              << "agreement_number: " << v.agreement_number;
}

void netting_agreement_service::save_netting_agreements(
    const std::vector<domain::netting_agreement>& netting_agreements) {
    for (const auto& e : netting_agreements) {
        if (e.agreement_number.empty())
            throw std::invalid_argument("Netting Agreement agreement_number cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << netting_agreements.size() << " netting agreements";
    auto ts = netting_agreements;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void netting_agreement_service::delete_netting_agreement(const std::string& agreement_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing netting agreement. "
                               << "agreement_number: " << agreement_number;
    repo_.remove(ctx_, agreement_number);
    BOOST_LOG_SEV(lg(), info) << "Removed netting agreement. "
                              << "agreement_number: " << agreement_number;
}

void netting_agreement_service::delete_netting_agreements(
    const std::vector<std::string>& agreement_numbers) {
    repo_.remove(ctx_, agreement_numbers);
}


}
