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
#include "ores.refdata.core/service/inflation_seasonality_service.hpp"
#include "ores.refdata.api/domain/inflation_seasonality.hpp"
#include "ores.refdata.api/messaging/inflation_seasonality_protocol.hpp"
#include "ores.refdata.core/repository/inflation_seasonality_repository.hpp"
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

inflation_seasonality_service::inflation_seasonality_service(context ctx)
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
std::vector<domain::inflation_seasonality>
inflation_seasonality_read_one(repository::inflation_seasonality_repository& repo,
                               const ores::database::context& ctx,
                               const messaging::inflation_seasonality_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::inflation_seasonality_key
inflation_seasonality_key_from(const domain::inflation_seasonality& v) {
    messaging::inflation_seasonality_key key;
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
domain::inflation_seasonality
inflation_seasonality_to_domain(const messaging::inflation_seasonality_write& write) {
    domain::inflation_seasonality v;
    v.id = write.id;
    v.curve_definition_id = write.curve_definition_id;
    v.base_date = write.base_date;
    v.frequency = write.frequency;
    return v;
}


}

messaging::list_inflation_seasonalities_response
inflation_seasonality_service::list_inflation_seasonalities(
    const messaging::list_inflation_seasonalities_request& request) {
    messaging::list_inflation_seasonalities_response response;
    if (!request.order.field.empty() &&
        !repository::inflation_seasonality_repository::is_sortable(request.order.field)) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "inflation seasonalities", .field = request.order.field});
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
    response.inflation_seasonalities = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_inflation_seasonality_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_inflation_seasonality_response
inflation_seasonality_service::get_inflation_seasonality(
    const messaging::get_inflation_seasonality_request& request) {
    messaging::get_inflation_seasonality_response response;
    auto found = inflation_seasonality_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "inflation_seasonality"});
        return response;
    }
    response.inflation_seasonality = std::move(found.front());
    return response;
}

messaging::get_many_inflation_seasonalities_response
inflation_seasonality_service::get_many_inflation_seasonalities(
    const messaging::get_many_inflation_seasonalities_request& request) {
    messaging::get_many_inflation_seasonalities_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::inflation_seasonality_lookup entry;
        entry.key = k;
        auto found = inflation_seasonality_read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.inflation_seasonality = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_inflation_seasonality_response
inflation_seasonality_service::put_inflation_seasonality(
    const messaging::put_inflation_seasonality_request& request) {
    messaging::put_inflation_seasonality_response response;
    domain::inflation_seasonality value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written =
        inflation_seasonality_read_one(repo_, ctx_, inflation_seasonality_key_from(value));
    if (!written.empty())
        response.inflation_seasonality = std::move(written.front());
    return response;
}

messaging::put_many_inflation_seasonalities_response
inflation_seasonality_service::put_many_inflation_seasonalities(
    const messaging::put_many_inflation_seasonalities_request& request) {
    messaging::put_many_inflation_seasonalities_response response;
    std::vector<domain::inflation_seasonality> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::inflation_seasonality value;
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
    response.inflation_seasonalities.reserve(batch.size());
    for (const auto& value : batch) {
        auto written =
            inflation_seasonality_read_one(repo_, ctx_, inflation_seasonality_key_from(value));
        response.inflation_seasonalities.push_back(written.empty() ? value :
                                                                     std::move(written.front()));
    }
    return response;
}

messaging::delete_inflation_seasonality_response
inflation_seasonality_service::delete_inflation_seasonality(
    const messaging::delete_inflation_seasonality_request& request) {
    messaging::delete_inflation_seasonality_response response;
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
        case repository::inflation_seasonality_repository::remove_status::removed:
            break;
        case repository::inflation_seasonality_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "inflation_seasonality"});
            break;
        case repository::inflation_seasonality_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = inflation_seasonality_read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "inflation_seasonality",
                 .field = "id",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::inflation_seasonality_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_inflation_seasonalities_response
inflation_seasonality_service::delete_many_inflation_seasonalities(
    const messaging::delete_many_inflation_seasonalities_request& request) {
    messaging::delete_many_inflation_seasonalities_response response;
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

messaging::list_inflation_seasonality_versions_response
inflation_seasonality_service::list_inflation_seasonality_versions(
    const messaging::list_inflation_seasonality_versions_request& request) {
    messaging::list_inflation_seasonality_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "inflation seasonalities", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "inflation seasonalities"});
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

messaging::get_inflation_seasonality_version_response
inflation_seasonality_service::get_inflation_seasonality_version(
    const messaging::get_inflation_seasonality_version_request& request) {
    messaging::get_inflation_seasonality_version_response response;
    auto found = repo_.read_at_version(
        ctx_, boost::uuids::to_string(request.key.inflation_seasonality.id), request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "inflation_seasonality"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
inflation_seasonality_service::prepare_change(const messaging::inflation_seasonality_change& change,
                                              const ores::utility::domain::change_intent& intent,
                                              domain::inflation_seasonality& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = inflation_seasonality_to_domain(change.write);
    const auto current =
        inflation_seasonality_read_one(repo_, ctx_, inflation_seasonality_key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "inflation_seasonality", .field = "id"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "inflation_seasonality"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "inflation_seasonality",
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


std::vector<domain::inflation_seasonality>
inflation_seasonality_service::list_inflation_seasonalities(std::uint32_t offset,
                                                            std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all inflation seasonalities";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t inflation_seasonality_service::count_inflation_seasonalities() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total inflation seasonalities count";
    return repo_.get_total_inflation_seasonality_count(ctx_);
}


std::optional<domain::inflation_seasonality>
inflation_seasonality_service::get_inflation_seasonality_at_version(const boost::uuids::uuid& id,
                                                                    std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting inflation seasonality at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(id), version);
}

std::optional<domain::inflation_seasonality>
inflation_seasonality_service::get_inflation_seasonality(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting inflation seasonality. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::inflation_seasonality>
inflation_seasonality_service::get_inflation_seasonalities(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void inflation_seasonality_service::save_inflation_seasonality(
    const domain::inflation_seasonality& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Inflation Seasonality id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving inflation seasonality. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved inflation seasonality. " << "id: " << v.id;
}

void inflation_seasonality_service::save_inflation_seasonalities(
    const std::vector<domain::inflation_seasonality>& inflation_seasonalities) {
    for (const auto& e : inflation_seasonalities) {
        if (e.id.is_nil())
            throw std::invalid_argument("Inflation Seasonality id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << inflation_seasonalities.size()
                               << " inflation seasonalities";
    auto ts = inflation_seasonalities;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void inflation_seasonality_service::delete_inflation_seasonality(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing inflation seasonality. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed inflation seasonality. " << "id: " << id;
}

void inflation_seasonality_service::delete_inflation_seasonalities(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::inflation_seasonality>
inflation_seasonality_service::get_inflation_seasonality_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for inflation seasonality. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
