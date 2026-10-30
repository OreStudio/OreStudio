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
#include "ores.marketdata.core/service/rate_card_component_service.hpp"
#include "ores.marketdata.api/domain/rate_card_component.hpp"
#include "ores.marketdata.api/messaging/rate_card_component_protocol.hpp"
#include "ores.marketdata.core/repository/rate_card_component_repository.hpp"
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

namespace ores::marketdata::service {

using namespace ores::logging;

rate_card_component_service::rate_card_component_service(context ctx)
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
std::vector<domain::rate_card_component>
rate_card_component_read_one(repository::rate_card_component_repository& repo,
                             const ores::database::context& ctx,
                             const messaging::rate_card_component_key& key) {
    return repo.read_latest_by_driver(ctx, key.driver);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::rate_card_component_key
rate_card_component_key_from(const domain::rate_card_component& v) {
    messaging::rate_card_component_key key;
    key.driver = v.driver;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::rate_card_component
rate_card_component_to_domain(const messaging::rate_card_component_write& write) {
    domain::rate_card_component v;
    v.id = write.id;
    v.feed_id = write.feed_id;
    v.driver = write.driver;
    v.licence_right = write.licence_right;
    v.effective_from = write.effective_from;
    v.currency = write.currency;
    v.charge_micros = write.charge_micros;
    v.effective_to = write.effective_to;
    return v;
}


}

messaging::list_rate_card_components_response
rate_card_component_service::list_rate_card_components(
    const messaging::list_rate_card_components_request& request) {
    messaging::list_rate_card_components_response response;
    if (!request.order.field.empty() &&
        !repository::rate_card_component_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "rate card components", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->feed_id_one_of &&
        request.filter->feed_id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "feed_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->driver_one_of &&
        request.filter->driver_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "driver_one_of", .limit = "1000"});
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
    response.rate_card_components = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_rate_card_component_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_rate_card_component_response rate_card_component_service::get_rate_card_component(
    const messaging::get_rate_card_component_request& request) {
    messaging::get_rate_card_component_response response;
    auto found = rate_card_component_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "rate_card_component"});
        return response;
    }
    response.rate_card_component = std::move(found.front());
    return response;
}

messaging::get_many_rate_card_components_response
rate_card_component_service::get_many_rate_card_components(
    const messaging::get_many_rate_card_components_request& request) {
    messaging::get_many_rate_card_components_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::rate_card_component_lookup entry;
        entry.key = k;
        auto found = rate_card_component_read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.rate_card_component = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_rate_card_component_response rate_card_component_service::put_rate_card_component(
    const messaging::put_rate_card_component_request& request) {
    messaging::put_rate_card_component_response response;
    domain::rate_card_component value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = rate_card_component_read_one(repo_, ctx_, rate_card_component_key_from(value));
    if (!written.empty())
        response.rate_card_component = std::move(written.front());
    return response;
}

messaging::put_many_rate_card_components_response
rate_card_component_service::put_many_rate_card_components(
    const messaging::put_many_rate_card_components_request& request) {
    messaging::put_many_rate_card_components_response response;
    std::vector<domain::rate_card_component> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::rate_card_component value;
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
    response.rate_card_components.reserve(batch.size());
    for (const auto& value : batch) {
        auto written =
            rate_card_component_read_one(repo_, ctx_, rate_card_component_key_from(value));
        response.rate_card_components.push_back(written.empty() ? value :
                                                                  std::move(written.front()));
    }
    return response;
}

messaging::delete_rate_card_component_response
rate_card_component_service::delete_rate_card_component(
    const messaging::delete_rate_card_component_request& request) {
    messaging::delete_rate_card_component_response response;
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
    const auto named = rate_card_component_read_one(repo_, ctx_, request.removal.key);
    if (named.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "rate_card_component"});
        return response;
    }
    const auto& row = named.front();
    switch (repo_.remove(ctx_, boost::uuids::to_string(row.id), expected)) {
        case repository::rate_card_component_repository::remove_status::removed:
            break;
        case repository::rate_card_component_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "rate_card_component"});
            break;
        case repository::rate_card_component_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = rate_card_component_read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "rate_card_component",
                 .field = "id",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::rate_card_component_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_rate_card_components_response
rate_card_component_service::delete_many_rate_card_components(
    const messaging::delete_many_rate_card_components_request& request) {
    messaging::delete_many_rate_card_components_response response;
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
    // A removal names its row by the key a caller holds, and the repository
    // takes the storage key, so the two are joined once here rather than at
    // each column's conversion. A name that matches no row is skipped: the
    // batch reports what it removed, and a row that is already gone is not a
    // failure.
    std::vector<domain::rate_card_component> resolved;
    resolved.reserve(request.removals.size());
    for (const auto& removal : request.removals) {
        auto named = rate_card_component_read_one(repo_, ctx_, removal.key);
        if (!named.empty())
            resolved.push_back(std::move(named.front()));
    }
    std::vector<std::string> id_keys;
    id_keys.reserve(resolved.size());
    for (const auto& row : resolved)
        id_keys.push_back(boost::uuids::to_string(row.id));
    repo_.remove(ctx_, id_keys);
    return response;
}

messaging::list_rate_card_component_versions_response
rate_card_component_service::list_rate_card_component_versions(
    const messaging::list_rate_card_component_versions_request& request) {
    messaging::list_rate_card_component_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "rate card components", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "rate card components"});
        return response;
    }
    // The versions of the row the caller's key names. The repository reads by
    // the storage key, so the declared key is resolved once here.
    const auto named = rate_card_component_read_one(repo_, ctx_, request.key);
    if (named.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "rate_card_component"});
        return response;
    }
    const auto& row = named.front();
    auto all = repo_.read_all(ctx_, boost::uuids::to_string(row.id));
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

messaging::get_rate_card_component_version_response
rate_card_component_service::get_rate_card_component_version(
    const messaging::get_rate_card_component_version_request& request) {
    messaging::get_rate_card_component_version_response response;
    // The version key nests the entity's own key, which is the declared one.
    // The repository reads by the storage key, so it is resolved once here.
    const auto named = rate_card_component_read_one(repo_, ctx_, request.key.rate_card_component);
    if (named.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "rate_card_component"});
        return response;
    }
    const auto& row = named.front();
    auto found = repo_.read_at_version(ctx_, boost::uuids::to_string(row.id), request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "rate_card_component"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
rate_card_component_service::prepare_change(const messaging::rate_card_component_change& change,
                                            const ores::utility::domain::change_intent& intent,
                                            domain::rate_card_component& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = rate_card_component_to_domain(change.write);
    const auto current =
        rate_card_component_read_one(repo_, ctx_, rate_card_component_key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "rate_card_component", .field = "id"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "rate_card_component"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "rate_card_component",
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


std::vector<domain::rate_card_component>
rate_card_component_service::list_rate_card_components(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all rate card components";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t rate_card_component_service::count_rate_card_components() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total rate card components count";
    return repo_.get_total_rate_card_component_count(ctx_);
}


std::optional<domain::rate_card_component>
rate_card_component_service::get_rate_card_component_at_version(const boost::uuids::uuid& id,
                                                                std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting rate card component at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(id), version);
}

std::optional<domain::rate_card_component>
rate_card_component_service::get_rate_card_component(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting rate card component. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::optional<domain::rate_card_component>
rate_card_component_service::get_rate_card_component_by_driver(const std::string& driver) {
    BOOST_LOG_SEV(lg(), debug) << "Getting rate card component by driver: " << driver;
    messaging::rate_card_component_key k;
    k.driver = driver;
    auto found = rate_card_component_read_one(repo_, ctx_, k);
    if (found.empty())
        return std::nullopt;
    return found.front();
}

std::vector<domain::rate_card_component>
rate_card_component_service::get_rate_card_components(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void rate_card_component_service::save_rate_card_component(const domain::rate_card_component& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Rate Card Component id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving rate card component. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved rate card component. " << "id: " << v.id;
}

void rate_card_component_service::save_rate_card_components(
    const std::vector<domain::rate_card_component>& rate_card_components) {
    for (const auto& e : rate_card_components) {
        if (e.id.is_nil())
            throw std::invalid_argument("Rate Card Component id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << rate_card_components.size()
                               << " rate card components";
    auto ts = rate_card_components;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void rate_card_component_service::delete_rate_card_component(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing rate card component. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed rate card component. " << "id: " << id;
}

void rate_card_component_service::delete_rate_card_components(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::rate_card_component>
rate_card_component_service::get_rate_card_component_history(const std::string& key) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for rate card component. key: " << key;
    // The caller holds the key the model declares and this reads by the
    // storage key, so the two are joined here exactly as they are for any
    // other read. Without this step a provider looks the versions up under a
    // value the storage key never holds, and reports an entity that has a
    // history as having none.
    messaging::rate_card_component_key k;
    k.driver = key;
    // A delete here closes the transaction-time window and leaves every version
    // in place, so resolving through a latest read would lose the history at
    // exactly the moment it is wanted. This takes the newest row carrying the
    // declared key whether or not it is still current, which for a record that
    // still exists is the same row the latest read would have returned.
    const auto found = repo_.read_any_by_driver(ctx_, k.driver);
    if (found.empty())
        return {};
    const auto& row = found.front();
    return repo_.read_all(ctx_, boost::uuids::to_string(row.id));
}

}
