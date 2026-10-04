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
#include "ores.dq.core/service/csa_service.hpp"
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

csa_service::csa_service(context ctx)
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
std::vector<domain::csa> read_one(repository::csa_repository& repo,
                                  const ores::database::context& ctx,
                                  const messaging::csa_key& key) {
    return repo.read_latest(ctx, key.netting_set_code);
}

} // namespace

messaging::list_csas_response csa_service::list_csas(const messaging::list_csas_request& request) {
    messaging::list_csas_response response;
    if (!request.order.field.empty() &&
        !repository::csa_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of csas cannot be ordered by " + request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->netting_set_code_one_of &&
        request.filter->netting_set_code_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message =
            "The filter lists more than 1000 values in netting_set_code_one_of.";
        return response;
    }
    response.csas =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_csa_count(ctx_, request.filter);
    return response;
}

messaging::get_csa_response csa_service::get_csa(const messaging::get_csa_request& request) {
    messaging::get_csa_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.csa = std::move(found.front());
    return response;
}

messaging::get_many_csas_response
csa_service::get_many_csas(const messaging::get_many_csas_request& request) {
    messaging::get_many_csas_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::csa_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.csa = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}


std::vector<domain::csa> csa_service::list_csas(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all csas";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t csa_service::count_csas() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total csas count";
    return repo_.get_total_csa_count(ctx_);
}


std::optional<domain::csa> csa_service::get_csa(const std::string& netting_set_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting csa. " << "netting_set_code: " << netting_set_code;
    auto results = repo_.read_latest(ctx_, netting_set_code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::csa> csa_service::get_csas(const std::vector<std::string>& netting_set_codes) {
    return repo_.read_latest(ctx_, netting_set_codes);
}

void csa_service::save_csa(const domain::csa& v) {
    if (v.netting_set_code.empty())
        throw std::invalid_argument("CSA netting_set_code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving csa. " << "netting_set_code: " << v.netting_set_code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved csa. " << "netting_set_code: " << v.netting_set_code;
}

void csa_service::save_csas(const std::vector<domain::csa>& csas) {
    for (const auto& e : csas) {
        if (e.netting_set_code.empty())
            throw std::invalid_argument("CSA netting_set_code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << csas.size() << " csas";
    auto ts = csas;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void csa_service::delete_csa(const std::string& netting_set_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing csa. " << "netting_set_code: " << netting_set_code;
    repo_.remove(ctx_, netting_set_code);
    BOOST_LOG_SEV(lg(), info) << "Removed csa. " << "netting_set_code: " << netting_set_code;
}

void csa_service::delete_csas(const std::vector<std::string>& netting_set_codes) {
    repo_.remove(ctx_, netting_set_codes);
}


}
