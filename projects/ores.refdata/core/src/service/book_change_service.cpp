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
#include "ores.refdata.core/service/book_change_service.hpp"
#include "ores.refdata.api/domain/book_change.hpp"
#include "ores.refdata.api/messaging/book_change_protocol.hpp"
#include "ores.refdata.core/repository/book_change_repository.hpp"
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
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/valid_at.hpp"
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

namespace ores::refdata::service {

using namespace ores::logging;

book_change_service::book_change_service(context ctx)
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
std::vector<domain::book_change> read_one(repository::book_change_repository& repo,
                                          const ores::database::context& ctx,
                                          const messaging::book_change_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

}

messaging::list_book_changes_response
book_change_service::list_book_changes(const messaging::list_book_changes_request& request) {
    messaging::list_book_changes_response response;
    if (!request.order.field.empty() &&
        !repository::book_change_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "book changes", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    // A stated instant is checked here, so a malformed one is the caller's
    // mistake rather than a database error. The caller's text is what the
    // store reads, so a fraction of a second is kept.
    std::optional<std::string> as_of;
    if (request.as_of) {
        as_of = ores::database::repository::parse_as_of(*request.as_of);
        if (!as_of) {
            response.result = refuse(outcome_code::as_of_invalid, {.value = *request.as_of});
            return response;
        }
    }
    response.book_changes = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_book_change_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_book_change_response
book_change_service::get_book_change(const messaging::get_book_change_request& request) {
    messaging::get_book_change_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "book_change"});
        return response;
    }
    response.book_change = std::move(found.front());
    return response;
}

messaging::get_many_book_changes_response book_change_service::get_many_book_changes(
    const messaging::get_many_book_changes_request& request) {
    messaging::get_many_book_changes_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::book_change_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.book_change = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::list_book_change_versions_response book_change_service::list_book_change_versions(
    const messaging::list_book_change_versions_request& request) {
    messaging::list_book_change_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "book changes", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result = refuse(outcome_code::filter_not_supported, {.entity = "book changes"});
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

messaging::get_book_change_version_response book_change_service::get_book_change_version(
    const messaging::get_book_change_version_request& request) {
    messaging::get_book_change_version_response response;
    auto found = repo_.read_at_version(
        ctx_, boost::uuids::to_string(request.key.book_change.id), request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "book_change"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}


std::vector<domain::book_change> book_change_service::list_book_changes(std::uint32_t offset,
                                                                        std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all book changes";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t book_change_service::count_book_changes() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total book changes count";
    return repo_.get_total_book_change_count(ctx_);
}


std::optional<domain::book_change>
book_change_service::get_book_change_at_version(const boost::uuids::uuid& id,
                                                std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting book change at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(id), version);
}

std::optional<domain::book_change>
book_change_service::get_book_change(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting book change. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::book_change>
book_change_service::get_book_changes(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void book_change_service::save_book_change(const domain::book_change& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Book Change id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving book change. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved book change. " << "id: " << v.id;
}

void book_change_service::save_book_changes(const std::vector<domain::book_change>& book_changes) {
    for (const auto& e : book_changes) {
        if (e.id.is_nil())
            throw std::invalid_argument("Book Change id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << book_changes.size() << " book changes";
    auto ts = book_changes;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void book_change_service::delete_book_change(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing book change. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed book change. " << "id: " << id;
}

void book_change_service::delete_book_changes(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::book_change>
book_change_service::get_book_change_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for book change. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
