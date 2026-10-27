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
#include "ores.trading.core/service/structure_identifier_service.hpp"
#include "ores.trading.api/domain/structure_identifier.hpp"
#include "ores.trading.api/messaging/structure_identifier_protocol.hpp"
#include "ores.trading.core/repository/structure_identifier_repository.hpp"
#include <boost/log/sources/severity_feature.hpp>
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

using ores::service::messaging::of_acting_party;
using ores::service::messaging::stamp;
// Every refusal names its outcome; the catalogue supplies the code and the
// sentence, so a service states what happened and nothing about how to say it.
using ores::database::domain::outcome_args;
using ores::database::domain::outcome_code;
using ores::database::domain::refuse;

namespace ores::trading::service {

using namespace ores::logging;

structure_identifier_service::structure_identifier_service(context ctx)
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
std::vector<domain::structure_identifier>
structure_identifier_read_one(repository::structure_identifier_repository& repo,
                              const ores::database::context& ctx,
                              const messaging::structure_identifier_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.structure_id), key.id_type);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::structure_identifier_key
structure_identifier_key_from(const domain::structure_identifier& v) {
    messaging::structure_identifier_key key;
    key.structure_id = v.structure_id;
    key.id_type = v.id_type;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::structure_identifier
structure_identifier_to_domain(const messaging::structure_identifier_write& write) {
    domain::structure_identifier v;
    v.structure_id = write.structure_id;
    v.id_type = write.id_type;
    v.id_value = write.id_value;
    v.issuing_party_id = write.issuing_party_id;
    return v;
}


}

messaging::list_structure_identifiers_response
structure_identifier_service::list_structure_identifiers(
    const messaging::list_structure_identifiers_request& request) {
    messaging::list_structure_identifiers_response response;
    if (!request.order.field.empty() &&
        !repository::structure_identifier_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "structure identifiers", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->structure_id_one_of &&
        request.filter->structure_id_one_of->size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "structure_id_one_of", .limit = "1000"});
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
    response.identifiers = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_identifier_count(ctx_, request.filter, as_of);
    return response;
}

messaging::list_by_structure_id_structure_identifiers_response
structure_identifier_service::list_by_structure_id_structure_identifiers(
    const messaging::list_by_structure_id_structure_identifiers_request& request) {
    messaging::list_by_structure_id_structure_identifiers_response response;
    if (!request.order.field.empty() &&
        !repository::structure_identifier_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "structure identifiers", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->structure_id_one_of &&
        request.filter->structure_id_one_of->size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "structure_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result =
            refuse(outcome_code::scope_not_supported, {.entity = "structure identifiers"});
        return response;
    }
    const auto relation = boost::uuids::to_string(request.structure_id);
    response.identifiers = repo_.read_latest_by_structure_id(
        ctx_, relation, request.offset, request.limit, request.order, request.filter);
    response.total =
        repo_.get_total_identifier_count_by_structure_id(ctx_, relation, request.filter);
    return response;
}

messaging::get_structure_identifier_response structure_identifier_service::get_structure_identifier(
    const messaging::get_structure_identifier_request& request) {
    messaging::get_structure_identifier_response response;
    auto found = structure_identifier_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "structure_identifier"});
        return response;
    }
    response.structure_identifier = std::move(found.front());
    return response;
}

messaging::get_many_structure_identifiers_response
structure_identifier_service::get_many_structure_identifiers(
    const messaging::get_many_structure_identifiers_request& request) {
    messaging::get_many_structure_identifiers_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::structure_identifier_lookup entry;
        entry.key = k;
        auto found = structure_identifier_read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.structure_identifier = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_structure_identifier_response structure_identifier_service::put_structure_identifier(
    const messaging::put_structure_identifier_request& request) {
    messaging::put_structure_identifier_response response;
    domain::structure_identifier value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = of_acting_party(
        structure_identifier_read_one(repo_, ctx_, structure_identifier_key_from(value)), ctx_);
    if (!written.empty())
        response.structure_identifier = std::move(written.front());
    return response;
}

messaging::put_many_structure_identifiers_response
structure_identifier_service::put_many_structure_identifiers(
    const messaging::put_many_structure_identifiers_request& request) {
    messaging::put_many_structure_identifiers_response response;
    std::vector<domain::structure_identifier> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::structure_identifier value;
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
    response.identifiers.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = of_acting_party(
            structure_identifier_read_one(repo_, ctx_, structure_identifier_key_from(value)), ctx_);
        response.identifiers.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_structure_identifier_response
structure_identifier_service::delete_structure_identifier(
    const messaging::delete_structure_identifier_request& request) {
    messaging::delete_structure_identifier_response response;
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
    switch (repo_.remove(ctx_,
                         boost::uuids::to_string(request.removal.key.structure_id),
                         request.removal.key.id_type,
                         expected)) {
        case repository::structure_identifier_repository::remove_status::removed:
            break;
        case repository::structure_identifier_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "structure_identifier"});
            break;
        case repository::structure_identifier_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = structure_identifier_read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "structure_identifier",
                 .field = "structure_id",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::structure_identifier_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_structure_identifiers_response
structure_identifier_service::delete_many_structure_identifiers(
    const messaging::delete_many_structure_identifiers_request& request) {
    messaging::delete_many_structure_identifiers_response response;
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
    std::vector<std::string> structure_id_keys;
    structure_id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        structure_id_keys.push_back(boost::uuids::to_string(removal.key.structure_id));
    std::vector<std::string> id_type_keys;
    id_type_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        id_type_keys.push_back(removal.key.id_type);
    repo_.remove(ctx_, structure_id_keys, id_type_keys);
    return response;
}

messaging::list_structure_identifier_versions_response
structure_identifier_service::list_structure_identifier_versions(
    const messaging::list_structure_identifier_versions_request& request) {
    messaging::list_structure_identifier_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "structure identifiers", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "structure identifiers"});
        return response;
    }
    auto all = repo_.read_all(
        ctx_, boost::uuids::to_string(request.key.structure_id), request.key.id_type);
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

messaging::get_structure_identifier_version_response
structure_identifier_service::get_structure_identifier_version(
    const messaging::get_structure_identifier_version_request& request) {
    messaging::get_structure_identifier_version_response response;
    auto found = repo_.read_at_version(
        ctx_,
        boost::uuids::to_string(request.key.structure_identifier.structure_id),
        request.key.structure_identifier.id_type,
        request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "structure_identifier"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
structure_identifier_service::prepare_change(const messaging::structure_identifier_change& change,
                                             const ores::utility::domain::change_intent& intent,
                                             domain::structure_identifier& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = structure_identifier_to_domain(change.write);
    // The write lands under the acting party, so its claim is checked against
    // that party's row, not another visible party's row of the same key.
    const auto current = of_acting_party(
        structure_identifier_read_one(repo_, ctx_, structure_identifier_key_from(out)), ctx_);
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "structure_identifier", .field = "structure_id"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "structure_identifier"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "structure_identifier",
                               .field = "structure_id",
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


std::vector<domain::structure_identifier>
structure_identifier_service::list_identifiers(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all structure identifiers";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t structure_identifier_service::count_identifiers() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total structure identifiers count";
    return repo_.get_total_identifier_count(ctx_);
}


std::vector<domain::structure_identifier>
structure_identifier_service::list_identifiers_by_structure_id(const std::string& structure_id,
                                                               std::uint32_t offset,
                                                               std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing structure identifiers by structure_id: " << structure_id;
    return repo_.read_latest_by_structure_id(ctx_, structure_id, offset, limit);
}

std::uint32_t
structure_identifier_service::count_identifiers_by_structure_id(const std::string& structure_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total structure identifiers count by structure_id: "
                               << structure_id;
    return repo_.get_total_identifier_count_by_structure_id(ctx_, structure_id);
}


std::optional<domain::structure_identifier> structure_identifier_service::get_identifier_at_version(
    const std::string& structure_id, const std::string& id_type, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting structure identifier at version. "
                               << "structure_id: " << structure_id << " id_type: " << id_type
                               << " version: " << version;
    return repo_.read_at_version(ctx_, structure_id, id_type, version);
}

std::optional<domain::structure_identifier>
structure_identifier_service::get_identifier(const std::string& structure_id,
                                             const std::string& id_type) {
    BOOST_LOG_SEV(lg(), debug) << "Getting structure identifier. "
                               << "structure_id: " << structure_id << " id_type: " << id_type;
    auto results = repo_.read_latest(ctx_, structure_id, id_type);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::structure_identifier>
structure_identifier_service::get_identifiers(const std::vector<std::string>& structure_ids,
                                              const std::vector<std::string>& id_types) {
    return repo_.read_latest(ctx_, structure_ids, id_types);
}

void structure_identifier_service::save_identifier(const domain::structure_identifier& v) {
    if (v.structure_id.is_nil())
        throw std::invalid_argument("Structure Identifier structure_id cannot be empty.");
    if (v.id_type.empty())
        throw std::invalid_argument("Structure Identifier id_type cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving structure identifier. "
                               << "structure_id: " << v.structure_id << " id_type: " << v.id_type;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved structure identifier. "
                              << "structure_id: " << v.structure_id << " id_type: " << v.id_type;
}

void structure_identifier_service::save_identifiers(
    const std::vector<domain::structure_identifier>& identifiers) {
    for (const auto& e : identifiers) {
        if (e.structure_id.is_nil())
            throw std::invalid_argument("Structure Identifier structure_id cannot be empty.");
        if (e.id_type.empty())
            throw std::invalid_argument("Structure Identifier id_type cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << identifiers.size() << " structure identifiers";
    auto ts = identifiers;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void structure_identifier_service::delete_identifier(const std::string& structure_id,
                                                     const std::string& id_type) {
    BOOST_LOG_SEV(lg(), debug) << "Removing structure identifier. "
                               << "structure_id: " << structure_id << " id_type: " << id_type;
    repo_.remove(ctx_, structure_id, id_type);
    BOOST_LOG_SEV(lg(), info) << "Removed structure identifier. "
                              << "structure_id: " << structure_id << " id_type: " << id_type;
}

void structure_identifier_service::delete_identifiers(const std::vector<std::string>& structure_ids,
                                                      const std::vector<std::string>& id_types) {
    repo_.remove(ctx_, structure_ids, id_types);
}

std::vector<domain::structure_identifier>
structure_identifier_service::get_identifier_history(const std::string& structure_id,
                                                     const std::string& id_type) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for structure identifier. "
                               << "structure_id: " << structure_id << " id_type: " << id_type;
    return repo_.read_all(ctx_, structure_id, id_type);
}

}
