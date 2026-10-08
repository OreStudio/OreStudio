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
#include "ores.trading.core/service/structure_service.hpp"
#include "ores.trading.api/domain/structure.hpp"
#include "ores.trading.api/messaging/structure_protocol.hpp"
#include "ores.trading.core/repository/structure_repository.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
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

namespace ores::trading::service {

using namespace ores::logging;

structure_service::structure_service(context ctx)
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
std::vector<domain::structure> read_one(repository::structure_repository& repo,
                                        const ores::database::context& ctx,
                                        const messaging::structure_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

}

messaging::list_structures_response
structure_service::list_structures(const messaging::list_structures_request& request) {
    messaging::list_structures_response response;
    if (!request.order.field.empty() &&
        !repository::structure_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "structures", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    response.structures =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_structure_count(ctx_, request.filter);
    return response;
}

messaging::get_structure_response
structure_service::get_structure(const messaging::get_structure_request& request) {
    messaging::get_structure_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "structure"});
        return response;
    }
    response.structure = std::move(found.front());
    return response;
}

messaging::get_many_structures_response
structure_service::get_many_structures(const messaging::get_many_structures_request& request) {
    messaging::get_many_structures_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::structure_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.structure = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}


std::vector<domain::structure> structure_service::list_structures(std::uint32_t offset,
                                                                  std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all structures";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t structure_service::count_structures() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total structures count";
    return repo_.get_total_structure_count(ctx_);
}


std::optional<domain::structure> structure_service::get_structure(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting structure. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::structure>
structure_service::get_structures(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void structure_service::save_structure(const domain::structure& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Structure id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving structure. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved structure. " << "id: " << v.id;
}

void structure_service::save_structures(const std::vector<domain::structure>& structures) {
    for (const auto& e : structures) {
        if (e.id.is_nil())
            throw std::invalid_argument("Structure id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << structures.size() << " structures";
    auto ts = structures;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void structure_service::delete_structure(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing structure. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed structure. " << "id: " << id;
}

void structure_service::delete_structures(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}


}
