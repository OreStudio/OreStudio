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
#include "ores.reporting.core/service/ore_file_version_service.hpp"
#include "ores.reporting.api/domain/ore_file_version.hpp"
#include "ores.reporting.api/messaging/ore_file_version_protocol.hpp"
#include "ores.reporting.core/repository/ore_file_version_repository.hpp"
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

ore_file_version_service::ore_file_version_service(const context& ctx)
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
std::vector<domain::ore_file_version>
ore_file_version_read_one(repository::ore_file_version_repository& repo,
                          const ores::database::context& ctx,
                          const messaging::ore_file_version_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::ore_file_version_key ore_file_version_key_from(const domain::ore_file_version& v) {
    messaging::ore_file_version_key key;
    key.id = v.id;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::ore_file_version
ore_file_version_to_domain(const messaging::ore_file_version_write& write) {
    domain::ore_file_version v;
    v.id = write.id;
    v.ore_file_code = write.ore_file_code;
    v.engine_version = write.engine_version;
    v.content = write.content;
    v.sha256 = write.sha256;
    v.bytes = write.bytes;
    return v;
}


}

messaging::list_ore_file_versions_response ore_file_version_service::list_ore_file_versions(
    const messaging::list_ore_file_versions_request& request) {
    messaging::list_ore_file_versions_response response;
    if (!request.order.field.empty() &&
        !repository::ore_file_version_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "ORE file versions", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->ore_file_code_one_of &&
        request.filter->ore_file_code_one_of->size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "ore_file_code_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->engine_version_one_of &&
        request.filter->engine_version_one_of->size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "engine_version_one_of", .limit = "1000"});
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
    response.versions = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_version_count(ctx_, request.filter, as_of);
    return response;
}

messaging::list_by_ore_file_code_ore_file_versions_response
ore_file_version_service::list_by_ore_file_code_ore_file_versions(
    const messaging::list_by_ore_file_code_ore_file_versions_request& request) {
    messaging::list_by_ore_file_code_ore_file_versions_response response;
    if (!request.order.field.empty() &&
        !repository::ore_file_version_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "ORE file versions", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->ore_file_code_one_of &&
        request.filter->ore_file_code_one_of->size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "ore_file_code_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->engine_version_one_of &&
        request.filter->engine_version_one_of->size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "engine_version_one_of", .limit = "1000"});
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result =
            refuse(outcome_code::scope_not_supported, {.entity = "ORE file versions"});
        return response;
    }
    const auto relation = request.ore_file_code;
    response.versions = repo_.read_latest_by_ore_file_code(
        ctx_, relation, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_version_count_by_ore_file_code(ctx_, relation, request.filter);
    return response;
}

messaging::get_ore_file_version_response ore_file_version_service::get_ore_file_version(
    const messaging::get_ore_file_version_request& request) {
    messaging::get_ore_file_version_response response;
    auto found = ore_file_version_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "ore_file_version"});
        return response;
    }
    response.ore_file_version = std::move(found.front());
    return response;
}

messaging::get_many_ore_file_versions_response ore_file_version_service::get_many_ore_file_versions(
    const messaging::get_many_ore_file_versions_request& request) {
    messaging::get_many_ore_file_versions_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::ore_file_version_lookup entry;
        entry.key = k;
        auto found = ore_file_version_read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.ore_file_version = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_ore_file_version_response ore_file_version_service::put_ore_file_version(
    const messaging::put_ore_file_version_request& request) {
    messaging::put_ore_file_version_response response;
    domain::ore_file_version value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = ore_file_version_read_one(repo_, ctx_, ore_file_version_key_from(value));
    if (!written.empty())
        response.ore_file_version = std::move(written.front());
    return response;
}

messaging::put_many_ore_file_versions_response ore_file_version_service::put_many_ore_file_versions(
    const messaging::put_many_ore_file_versions_request& request) {
    messaging::put_many_ore_file_versions_response response;
    std::vector<domain::ore_file_version> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::ore_file_version value;
        const auto result = prepare_change(change, request.intent, value);
        if (result.outcome != ores::utility::domain::outcome::ok) {
            // Nothing has been written: the whole set is checked before any
            // of it lands, so a refused element refuses the batch.
            response.result = result;
            return response;
        }
        batch.push_back(std::move(value));
    }
    // One statement, so the set lands together. The store checks each row's
    // claim inside that statement, which is what makes the check above and the
    // write one decision rather than two.
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(request.changes.size());
    for (const auto& change : request.changes)
        claims.push_back(change.precondition);
    repo_.write(ctx_, batch, claims);
    response.versions.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = ore_file_version_read_one(repo_, ctx_, ore_file_version_key_from(value));
        response.versions.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_ore_file_version_response ore_file_version_service::delete_ore_file_version(
    const messaging::delete_ore_file_version_request& request) {
    messaging::delete_ore_file_version_response response;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    if (request.removal.precondition.kind == precondition_kind::must_not_exist) {
        response.result = refuse(outcome_code::precondition_not_supported);
        return response;
    }
    std::optional<std::uint32_t> expected;
    if (request.removal.precondition.kind == precondition_kind::must_match_version) {
        if (!request.removal.precondition.version) {
            response.result = refuse(outcome_code::precondition_incomplete);
            return response;
        }
        expected = request.removal.precondition.version;
    }
    switch (repo_.remove(ctx_, boost::uuids::to_string(request.removal.key.id), expected)) {
        case repository::ore_file_version_repository::remove_status::removed:
            break;
        case repository::ore_file_version_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "ore_file_version"});
            break;
        case repository::ore_file_version_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = ore_file_version_read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "ore_file_version",
                 .field = "id",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::ore_file_version_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_ore_file_versions_response
ore_file_version_service::delete_many_ore_file_versions(
    const messaging::delete_many_ore_file_versions_request& request) {
    messaging::delete_many_ore_file_versions_response response;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    for (const auto& removal : request.removals) {
        if (removal.precondition.kind != precondition_kind::any) {
            // The store removes a set in one statement, which carries no
            // per-row version. Refusing is the only answer that keeps the
            // batch atomic: serving it as a sequence of single removals would
            // leave a partial batch behind as soon as one row had moved on.
            response.result = refuse(outcome_code::batch_removal_is_unconditional);
            return response;
        }
    }
    if (request.removals.empty())
        return response;
    std::vector<std::string> id_keys;
    id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        id_keys.push_back(boost::uuids::to_string(removal.key.id));
    repo_.remove(ctx_, id_keys);
    return response;
}

messaging::list_ore_file_version_versions_response
ore_file_version_service::list_ore_file_version_versions(
    const messaging::list_ore_file_version_versions_request& request) {
    messaging::list_ore_file_version_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "ORE file versions", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "ORE file versions"});
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

messaging::get_ore_file_version_version_response
ore_file_version_service::get_ore_file_version_version(
    const messaging::get_ore_file_version_version_request& request) {
    messaging::get_ore_file_version_version_response response;
    auto found = repo_.read_at_version(
        ctx_, boost::uuids::to_string(request.key.ore_file_version.id), request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "ore_file_version"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
ore_file_version_service::prepare_change(const messaging::ore_file_version_change& change,
                                         const ores::utility::domain::change_intent& intent,
                                         domain::ore_file_version& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = ore_file_version_to_domain(change.write);
    const auto current = ore_file_version_read_one(repo_, ctx_, ore_file_version_key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "ore_file_version", .field = "id"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "ore_file_version"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "ore_file_version",
                               .field = "id",
                               .expected = change.precondition.version ?
                                               std::to_string(*change.precondition.version) :
                                               std::string{},
                               .current = std::to_string(current.front().version)});
            }
            break;
        case precondition_kind::any:
            break;
    }
    // The version is the repository's to state, from the claim: it is the one
    // thing the store's arbiter reads, and stating it in two places is how the
    // two come to disagree.
    stamp(out,
          ctx_,
          intent.reason_code.empty() ?
              std::string(ores::service::messaging::change_reasons::new_record) :
              intent.reason_code);
    out.change_commentary = intent.commentary;
    return result;
}


std::vector<domain::ore_file_version> ore_file_version_service::list_versions(std::uint32_t offset,
                                                                              std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all ORE file versions";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t ore_file_version_service::count_versions() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total ORE file versions count";
    return repo_.get_total_version_count(ctx_);
}


std::vector<domain::ore_file_version> ore_file_version_service::list_versions_by_ore_file_code(
    const std::string& ore_file_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing ORE file versions by ore_file_code: " << ore_file_code;
    return repo_.read_latest_by_ore_file_code(ctx_, ore_file_code, offset, limit);
}

std::uint32_t
ore_file_version_service::count_versions_by_ore_file_code(const std::string& ore_file_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total ORE file versions count by ore_file_code: "
                               << ore_file_code;
    return repo_.get_total_version_count_by_ore_file_code(ctx_, ore_file_code);
}


std::optional<domain::ore_file_version>
ore_file_version_service::get_version_at_version(const boost::uuids::uuid& id,
                                                 std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting ORE file version at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(id), version);
}

std::optional<domain::ore_file_version>
ore_file_version_service::get_version(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting ORE file version. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::ore_file_version>
ore_file_version_service::get_versions(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void ore_file_version_service::save_version(const domain::ore_file_version& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("ORE File Version id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving ORE file version. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved ORE file version. " << "id: " << v.id;
}

void ore_file_version_service::save_versions(
    const std::vector<domain::ore_file_version>& versions) {
    for (const auto& e : versions) {
        if (e.id.is_nil())
            throw std::invalid_argument("ORE File Version id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << versions.size() << " ORE file versions";
    auto ts = versions;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void ore_file_version_service::delete_version(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing ORE file version. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed ORE file version. " << "id: " << id;
}

void ore_file_version_service::delete_versions(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::ore_file_version>
ore_file_version_service::get_version_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for ORE file version. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
