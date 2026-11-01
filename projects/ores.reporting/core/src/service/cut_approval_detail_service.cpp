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
#include "ores.reporting.core/service/cut_approval_detail_service.hpp"
#include "ores.reporting.api/domain/cut_approval_detail.hpp"
#include "ores.reporting.api/messaging/cut_approval_detail_protocol.hpp"
#include "ores.reporting.core/repository/cut_approval_detail_repository.hpp"
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

namespace ores::reporting::service {

using namespace ores::logging;

cut_approval_detail_service::cut_approval_detail_service(context ctx)
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
std::vector<domain::cut_approval_detail>
cut_approval_detail_read_one(repository::cut_approval_detail_repository& repo,
                             const ores::database::context& ctx,
                             const messaging::cut_approval_detail_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

}

messaging::list_cut_approval_details_response
cut_approval_detail_service::list_cut_approval_details(
    const messaging::list_cut_approval_details_request& request) {
    messaging::list_cut_approval_details_response response;
    if (!request.order.field.empty() &&
        !repository::cut_approval_detail_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "cut approval details", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->request_id_one_of &&
        request.filter->request_id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "request_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->cut_id_one_of &&
        request.filter->cut_id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "cut_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->party_id_one_of &&
        request.filter->party_id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "party_id_one_of", .limit = "1000"});
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
    response.details = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_detail_count(ctx_, request.filter, as_of);
    return response;
}

messaging::list_by_request_id_cut_approval_details_response
cut_approval_detail_service::list_by_request_id_cut_approval_details(
    const messaging::list_by_request_id_cut_approval_details_request& request) {
    messaging::list_by_request_id_cut_approval_details_response response;
    if (!request.order.field.empty() &&
        !repository::cut_approval_detail_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "cut approval details", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->request_id_one_of &&
        request.filter->request_id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "request_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->cut_id_one_of &&
        request.filter->cut_id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "cut_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->party_id_one_of &&
        request.filter->party_id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "party_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result =
            refuse(outcome_code::scope_not_supported, {.entity = "cut approval details"});
        return response;
    }
    const auto relation = boost::uuids::to_string(request.request_id);
    response.details = repo_.read_latest_by_request_id(
        ctx_, relation, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_detail_count_by_request_id(ctx_, relation, request.filter);
    return response;
}

messaging::list_by_cut_id_cut_approval_details_response
cut_approval_detail_service::list_by_cut_id_cut_approval_details(
    const messaging::list_by_cut_id_cut_approval_details_request& request) {
    messaging::list_by_cut_id_cut_approval_details_response response;
    if (!request.order.field.empty() &&
        !repository::cut_approval_detail_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "cut approval details", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->request_id_one_of &&
        request.filter->request_id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "request_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->cut_id_one_of &&
        request.filter->cut_id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "cut_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->party_id_one_of &&
        request.filter->party_id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "party_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result =
            refuse(outcome_code::scope_not_supported, {.entity = "cut approval details"});
        return response;
    }
    const auto relation = boost::uuids::to_string(request.cut_id);
    response.details = repo_.read_latest_by_cut_id(
        ctx_, relation, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_detail_count_by_cut_id(ctx_, relation, request.filter);
    return response;
}

messaging::get_cut_approval_detail_response cut_approval_detail_service::get_cut_approval_detail(
    const messaging::get_cut_approval_detail_request& request) {
    messaging::get_cut_approval_detail_response response;
    auto found = cut_approval_detail_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "cut_approval_detail"});
        return response;
    }
    response.cut_approval_detail = std::move(found.front());
    return response;
}

messaging::get_many_cut_approval_details_response
cut_approval_detail_service::get_many_cut_approval_details(
    const messaging::get_many_cut_approval_details_request& request) {
    messaging::get_many_cut_approval_details_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::cut_approval_detail_lookup entry;
        entry.key = k;
        auto found = cut_approval_detail_read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.cut_approval_detail = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::list_cut_approval_detail_versions_response
cut_approval_detail_service::list_cut_approval_detail_versions(
    const messaging::list_cut_approval_detail_versions_request& request) {
    messaging::list_cut_approval_detail_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "cut approval details", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "cut approval details"});
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

messaging::get_cut_approval_detail_version_response
cut_approval_detail_service::get_cut_approval_detail_version(
    const messaging::get_cut_approval_detail_version_request& request) {
    messaging::get_cut_approval_detail_version_response response;
    auto found = repo_.read_at_version(
        ctx_, boost::uuids::to_string(request.key.cut_approval_detail.id), request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "cut_approval_detail"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}


std::vector<domain::cut_approval_detail>
cut_approval_detail_service::list_details(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all cut approval details";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t cut_approval_detail_service::count_details() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total cut approval details count";
    return repo_.get_total_detail_count(ctx_);
}


std::vector<domain::cut_approval_detail> cut_approval_detail_service::list_details_by_request_id(
    const std::string& request_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing cut approval details by request_id: " << request_id;
    return repo_.read_latest_by_request_id(ctx_, request_id, offset, limit);
}

std::uint32_t
cut_approval_detail_service::count_details_by_request_id(const std::string& request_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total cut approval details count by request_id: "
                               << request_id;
    return repo_.get_total_detail_count_by_request_id(ctx_, request_id);
}


std::vector<domain::cut_approval_detail> cut_approval_detail_service::list_details_by_cut_id(
    const std::string& cut_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing cut approval details by cut_id: " << cut_id;
    return repo_.read_latest_by_cut_id(ctx_, cut_id, offset, limit);
}

std::uint32_t cut_approval_detail_service::count_details_by_cut_id(const std::string& cut_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total cut approval details count by cut_id: " << cut_id;
    return repo_.get_total_detail_count_by_cut_id(ctx_, cut_id);
}


std::optional<domain::cut_approval_detail>
cut_approval_detail_service::get_detail_at_version(const boost::uuids::uuid& id,
                                                   std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting cut approval detail at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(id), version);
}

std::optional<domain::cut_approval_detail>
cut_approval_detail_service::get_detail(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting cut approval detail. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::cut_approval_detail>
cut_approval_detail_service::get_details(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void cut_approval_detail_service::save_detail(const domain::cut_approval_detail& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Cut Approval Detail id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving cut approval detail. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved cut approval detail. " << "id: " << v.id;
}

void cut_approval_detail_service::save_details(
    const std::vector<domain::cut_approval_detail>& details) {
    for (const auto& e : details) {
        if (e.id.is_nil())
            throw std::invalid_argument("Cut Approval Detail id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << details.size() << " cut approval details";
    auto ts = details;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void cut_approval_detail_service::delete_detail(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing cut approval detail. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed cut approval detail. " << "id: " << id;
}

void cut_approval_detail_service::delete_details(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::cut_approval_detail>
cut_approval_detail_service::get_detail_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for cut approval detail. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
