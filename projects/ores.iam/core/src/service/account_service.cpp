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
#include "ores.iam.core/service/account_service.hpp"
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

namespace ores::iam::service {

using namespace ores::logging;

account_service::account_service(context ctx)
    : ctx_(std::move(ctx)) {}
namespace {

/**
 * @brief The current row a key names, or an empty vector when there is none.
 *
 * A key record carries each column with the column's own type, and the
 * repository takes the text form every one of its key parameters shares, so
 * the conversion lives here rather than at every call site.
 */
std::vector<domain::account> read_one(repository::account_repository& repo,
                                      const ores::database::context& ctx,
                                      const messaging::account_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

} // namespace

messaging::list_accounts_response
account_service::list_accounts(const messaging::list_accounts_request& request) {
    messaging::list_accounts_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "This store pages in key order and cannot order by a stated field.";
        return response;
    }
    response.accounts = repo_.read_latest(ctx_, request.offset, request.limit);
    response.total = repo_.get_total_account_count(ctx_);
    return response;
}

messaging::get_account_response
account_service::get_account(const messaging::get_account_request& request) {
    messaging::get_account_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.account = std::move(found.front());
    return response;
}

messaging::get_many_accounts_response
account_service::get_many_accounts(const messaging::get_many_accounts_request& request) {
    messaging::get_many_accounts_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::account_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.account = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::list_account_versions_response
account_service::list_account_versions(const messaging::list_account_versions_request& request) {
    messaging::list_account_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "This store pages in key order and cannot order by a stated field.";
        return response;
    }
    if (request.filter) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_not_supported";
        response.result.message = "Filtering is not served for this resource yet.";
        return response;
    }
    auto all = repo_.read_all(ctx_, boost::uuids::to_string(request.key.id));
    // The store reads versions newest first, and the order a caller gets when
    // it states none is key order, which for a version key is oldest first.
    std::reverse(all.begin(), all.end());
    response.total = all.size();
    const auto begin = std::min<std::size_t>(request.offset, all.size());
    const auto end = std::min<std::size_t>(begin + request.limit, all.size());
    response.versions.assign(std::make_move_iterator(all.begin() + begin),
                             std::make_move_iterator(all.begin() + end));
    return response;
}

messaging::get_account_version_response
account_service::get_account_version(const messaging::get_account_version_request& request) {
    messaging::get_account_version_response response;
    auto found = repo_.read_at_version(
        ctx_, boost::uuids::to_string(request.key.account.id), request.key.version);
    if (!found) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.version = std::move(*found);
    return response;
}


std::vector<domain::account> account_service::list_accounts(std::uint32_t offset,
                                                            std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all accounts";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t account_service::count_accounts() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total accounts count";
    return repo_.get_total_account_count(ctx_);
}


std::optional<domain::account> account_service::get_account_at_version(const std::string& id,
                                                                       std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting account at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::account> account_service::get_account(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting account. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::account> account_service::get_accounts(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void account_service::save_account(const domain::account& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Account id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving account. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved account. " << "id: " << v.id;
}

void account_service::save_accounts(const std::vector<domain::account>& accounts) {
    for (const auto& e : accounts) {
        if (e.id.is_nil())
            throw std::invalid_argument("Account id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << accounts.size() << " accounts";
    auto ts = accounts;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void account_service::delete_account(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed account. " << "id: " << id;
}

void account_service::delete_accounts(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::account> account_service::get_account_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for account. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
