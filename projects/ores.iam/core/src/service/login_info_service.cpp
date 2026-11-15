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
#include "ores.iam.core/service/login_info_service.hpp"
#include "ores.iam.api/domain/login_info.hpp"
#include "ores.iam.api/messaging/login_info_protocol.hpp"
#include "ores.iam.core/repository/login_info_repository.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <tuple>
#include <utility>
#include <vector>
// Log lines stream uuids with uuid_io's operator<<, which the include check
// does not count as a use.
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp> // IWYU pragma: keep.

using ores::service::messaging::stamp;
// Every refusal names its outcome; the catalogue supplies the code and the
// sentence, so a service states what happened and nothing about how to say it.
using ores::database::domain::outcome_args;
using ores::database::domain::outcome_code;
using ores::database::domain::refuse;
using ores::utility::domain::max_batch_size;

namespace ores::iam::service {

using namespace ores::logging;

login_info_service::login_info_service(const context& ctx)
    : ctx_(ctx) {}
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
std::vector<domain::login_info> login_info_read_one(repository::login_info_repository& repo,
                                                    const ores::database::context& ctx,
                                                    const messaging::login_info_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.account_id));
}

/**
 * @brief The current rows a key set names, one entry per key in the keys' order.
 *
 * Each entry holds what the one-key read holds for its key, so a batch checks
 * each row against the same answer a single request gets. The repository
 * reads the key set in one batch read.
 *
 * A row is joined back to its key by the text form of each key column. Both
 * sides convert the typed value with the same function, and the store compares
 * key text exactly, so every row the read returns finds its key.
 *
 * When the declared key is not the storage key the batch is read by the
 * declared key instead. A model that states its own read by that key has no
 * batch form of it, so its keys are read one at a time.
 */
std::vector<std::vector<domain::login_info>>
login_info_read_many(repository::login_info_repository& repo,
                     const ores::database::context& ctx,
                     const std::vector<messaging::login_info_key>& keys) {
    std::vector<std::vector<domain::login_info>> found(keys.size());
    using key_tuple = std::tuple<std::string>;
    std::vector<std::string> account_id_keys;
    account_id_keys.reserve(keys.size());
    for (const auto& key : keys) {
        account_id_keys.push_back(boost::uuids::to_string(key.account_id));
    }
    std::map<key_tuple, std::vector<domain::login_info>> by_key;
    for (auto& row : repo.read_latest(ctx, account_id_keys)) {
        key_tuple named(boost::uuids::to_string(row.account_id));
        by_key[std::move(named)].push_back(std::move(row));
    }
    for (std::size_t i = 0; i < keys.size(); ++i) {
        const auto& key = keys[i];
        const auto rows = by_key.find(key_tuple(boost::uuids::to_string(key.account_id)));
        if (rows != by_key.end())
            found[i] = rows->second;
    }
    return found;
}

}

messaging::list_login_info_response
login_info_service::list_login_info(const messaging::list_login_info_request& request) {
    messaging::list_login_info_response response;
    if (!request.order.field.empty() &&
        !repository::login_info_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "login info", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->account_id_one_of &&
        request.filter->account_id_one_of->size() > max_batch_size) {
        response.result =
            refuse(outcome_code::filter_too_large,
                   {.field = "account_id_one_of", .limit = std::to_string(max_batch_size)});
        return response;
    }
    response.login_info =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_login_info_count(ctx_, request.filter);
    return response;
}

messaging::get_login_info_response
login_info_service::get_login_info(const messaging::get_login_info_request& request) {
    messaging::get_login_info_response response;
    auto found = login_info_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "login_info"});
        return response;
    }
    response.login_info = std::move(found.front());
    return response;
}

messaging::get_many_login_info_response
login_info_service::get_many_login_info(const messaging::get_many_login_info_request& request) {
    messaging::get_many_login_info_response response;
    // A key set carries at most as many keys as the protocol lets a batch
    // carry, so one request cannot ask for an unbounded reply.
    if (request.keys.size() > max_batch_size) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "keys", .limit = std::to_string(max_batch_size)});
        return response;
    }
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    auto found = login_info_read_many(repo_, ctx_, request.keys);
    for (std::size_t i = 0; i < request.keys.size(); ++i) {
        messaging::login_info_lookup entry;
        entry.key = request.keys[i];
        if (!found[i].empty())
            entry.login_info = std::move(found[i].front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}


std::vector<domain::login_info> login_info_service::list_login_info(std::uint32_t offset,
                                                                    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all login info";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t login_info_service::count_login_info() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total login info count";
    return repo_.get_total_login_info_count(ctx_);
}


std::optional<domain::login_info>
login_info_service::get_login_info(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting login info. " << "account_id: " << account_id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::login_info>
login_info_service::get_login_info(const std::vector<std::string>& account_ids) {
    return repo_.read_latest(ctx_, account_ids);
}

void login_info_service::save_login_info(const domain::login_info& v) {
    if (v.account_id.is_nil())
        throw std::invalid_argument("Login Info account_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving login info. " << "account_id: " << v.account_id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved login info. " << "account_id: " << v.account_id;
}

void login_info_service::save_login_info(const std::vector<domain::login_info>& login_info) {
    for (const auto& e : login_info) {
        if (e.account_id.is_nil())
            throw std::invalid_argument("Login Info account_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << login_info.size() << " login info";
    auto ts = login_info;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void login_info_service::delete_login_info(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing login info. " << "account_id: " << account_id;
    repo_.remove(ctx_, boost::uuids::to_string(account_id));
    BOOST_LOG_SEV(lg(), info) << "Removed login info. " << "account_id: " << account_id;
}

void login_info_service::delete_login_info(const std::vector<std::string>& account_ids) {
    repo_.remove(ctx_, account_ids);
}


}
