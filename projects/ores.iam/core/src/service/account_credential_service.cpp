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
#include "ores.iam.core/service/account_credential_service.hpp"
#include "ores.iam.api/domain/account_credential.hpp"
#include "ores.iam.api/messaging/account_credential_protocol.hpp"
#include "ores.iam.core/repository/account_credential_repository.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>
// Log lines stream uuids with uuid_io's operator<<, which the include check
// does not count as a use.
#include "ores.database/domain/context.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp> // IWYU pragma: keep.

using ores::service::messaging::stamp;

namespace ores::iam::service {

using namespace ores::logging;

account_credential_service::account_credential_service(context ctx)
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
std::vector<domain::account_credential> read_one(repository::account_credential_repository& repo,
                                                 const ores::database::context& ctx,
                                                 const messaging::account_credential_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

}

messaging::list_account_credentials_response account_credential_service::list_account_credentials(
    const messaging::list_account_credentials_request& request) {
    messaging::list_account_credentials_response response;
    if (!request.order.field.empty() &&
        !repository::account_credential_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of account credentials cannot be ordered by " + request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in id_one_of.";
        return response;
    }
    if (request.filter && request.filter->account_id_one_of &&
        request.filter->account_id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in account_id_one_of.";
        return response;
    }
    // A stated instant is checked here, so a malformed one is the caller's
    // mistake rather than a database error. The caller's text is what the
    // store reads, so a fraction of a second is kept.
    std::optional<std::string> as_of;
    if (request.as_of) {
        as_of = ores::database::repository::parse_as_of(*request.as_of);
        if (!as_of) {
            response.result.outcome = ores::utility::domain::outcome::invalid;
            response.result.code = "as_of_invalid";
            response.result.message = "as_of is not a UTC timestamp: " + *request.as_of;
            return response;
        }
    }
    response.account_credentials = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_account_credential_count(ctx_, request.filter, as_of);
    return response;
}

messaging::list_by_account_id_account_credentials_response
account_credential_service::list_by_account_id_account_credentials(
    const messaging::list_by_account_id_account_credentials_request& request) {
    messaging::list_by_account_id_account_credentials_response response;
    if (!request.order.field.empty() &&
        !repository::account_credential_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of account credentials cannot be ordered by " + request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in id_one_of.";
        return response;
    }
    if (request.filter && request.filter->account_id_one_of &&
        request.filter->account_id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in account_id_one_of.";
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "scope_not_supported";
        response.result.message = "This resource reads its direct members; it has no subtree.";
        return response;
    }
    const auto relation = boost::uuids::to_string(request.account_id);
    response.account_credentials = repo_.read_latest_by_account_id(
        ctx_, relation, request.offset, request.limit, request.order, request.filter);
    response.total =
        repo_.get_total_account_credential_count_by_account_id(ctx_, relation, request.filter);
    return response;
}

messaging::get_account_credential_response account_credential_service::get_account_credential(
    const messaging::get_account_credential_request& request) {
    messaging::get_account_credential_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.account_credential = std::move(found.front());
    return response;
}

messaging::get_many_account_credentials_response
account_credential_service::get_many_account_credentials(
    const messaging::get_many_account_credentials_request& request) {
    messaging::get_many_account_credentials_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::account_credential_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.account_credential = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::list_account_credential_versions_response
account_credential_service::list_account_credential_versions(
    const messaging::list_account_credential_versions_request& request) {
    messaging::list_account_credential_versions_response response;
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

messaging::get_account_credential_version_response
account_credential_service::get_account_credential_version(
    const messaging::get_account_credential_version_request& request) {
    messaging::get_account_credential_version_response response;
    auto found = repo_.read_at_version(
        ctx_, boost::uuids::to_string(request.key.account_credential.id), request.key.version);
    if (!found) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.version = std::move(*found);
    return response;
}


std::vector<domain::account_credential>
account_credential_service::list_account_credentials(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all account credentials";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t account_credential_service::count_account_credentials() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total account credentials count";
    return repo_.get_total_account_credential_count(ctx_);
}


std::vector<domain::account_credential>
account_credential_service::list_account_credentials_by_account_id(const std::string& account_id,
                                                                   std::uint32_t offset,
                                                                   std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing account credentials by account_id: " << account_id;
    return repo_.read_latest_by_account_id(ctx_, account_id, offset, limit);
}

std::uint32_t
account_credential_service::count_account_credentials_by_account_id(const std::string& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total account credentials count by account_id: "
                               << account_id;
    return repo_.get_total_account_credential_count_by_account_id(ctx_, account_id);
}


std::optional<domain::account_credential>
account_credential_service::get_account_credential_at_version(const boost::uuids::uuid& id,
                                                              std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting account credential at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(id), version);
}

std::optional<domain::account_credential>
account_credential_service::get_account_credential(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting account credential. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::account_credential>
account_credential_service::get_account_credentials(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void account_credential_service::save_account_credential(const domain::account_credential& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Account Credential id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving account credential. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved account credential. " << "id: " << v.id;
}

void account_credential_service::save_account_credentials(
    const std::vector<domain::account_credential>& account_credentials) {
    for (const auto& e : account_credentials) {
        if (e.id.is_nil())
            throw std::invalid_argument("Account Credential id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << account_credentials.size() << " account credentials";
    auto ts = account_credentials;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void account_credential_service::delete_account_credential(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account credential. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed account credential. " << "id: " << id;
}

void account_credential_service::delete_account_credentials(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::account_credential>
account_credential_service::get_account_credential_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for account credential. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
