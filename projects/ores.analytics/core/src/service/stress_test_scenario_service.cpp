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
#include "ores.analytics.core/service/stress_test_scenario_service.hpp"
#include "ores.analytics.api/domain/stress_test_scenario.hpp"
#include "ores.analytics.api/messaging/stress_test_scenario_protocol.hpp"
#include "ores.analytics.core/repository/stress_test_scenario_repository.hpp"
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

namespace ores::analytics::service {

using namespace ores::logging;

stress_test_scenario_service::stress_test_scenario_service(context ctx)
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
std::vector<domain::stress_test_scenario>
read_one(repository::stress_test_scenario_repository& repo,
         const ores::database::context& ctx,
         const messaging::stress_test_scenario_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::stress_test_scenario_key key_from(const domain::stress_test_scenario& v) {
    messaging::stress_test_scenario_key key;
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
domain::stress_test_scenario to_domain(const messaging::stress_test_scenario_write& write) {
    domain::stress_test_scenario v;
    v.id = write.id;
    v.stress_test_library_id = write.stress_test_library_id;
    v.name = write.name;
    v.date = write.date;
    v.position = write.position;
    return v;
}

}

messaging::list_stress_test_scenarios_response
stress_test_scenario_service::list_stress_test_scenarios(
    const messaging::list_stress_test_scenarios_request& request) {
    messaging::list_stress_test_scenarios_response response;
    if (!request.order.field.empty() &&
        !repository::stress_test_scenario_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "stress test scenarios", .field = request.order.field});
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
    response.stress_test_scenarios = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_stress_test_scenario_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_stress_test_scenario_response stress_test_scenario_service::get_stress_test_scenario(
    const messaging::get_stress_test_scenario_request& request) {
    messaging::get_stress_test_scenario_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "stress_test_scenario"});
        return response;
    }
    response.stress_test_scenario = std::move(found.front());
    return response;
}

messaging::get_many_stress_test_scenarios_response
stress_test_scenario_service::get_many_stress_test_scenarios(
    const messaging::get_many_stress_test_scenarios_request& request) {
    messaging::get_many_stress_test_scenarios_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::stress_test_scenario_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.stress_test_scenario = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_stress_test_scenario_response stress_test_scenario_service::put_stress_test_scenario(
    const messaging::put_stress_test_scenario_request& request) {
    messaging::put_stress_test_scenario_response response;
    domain::stress_test_scenario value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.stress_test_scenario = std::move(written.front());
    return response;
}

messaging::put_many_stress_test_scenarios_response
stress_test_scenario_service::put_many_stress_test_scenarios(
    const messaging::put_many_stress_test_scenarios_request& request) {
    messaging::put_many_stress_test_scenarios_response response;
    std::vector<domain::stress_test_scenario> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::stress_test_scenario value;
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
    response.stress_test_scenarios.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.stress_test_scenarios.push_back(written.empty() ? value :
                                                                   std::move(written.front()));
    }
    return response;
}

messaging::delete_stress_test_scenario_response
stress_test_scenario_service::delete_stress_test_scenario(
    const messaging::delete_stress_test_scenario_request& request) {
    messaging::delete_stress_test_scenario_response response;
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
        case repository::stress_test_scenario_repository::remove_status::removed:
            break;
        case repository::stress_test_scenario_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "stress_test_scenario"});
            break;
        case repository::stress_test_scenario_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "stress_test_scenario",
                 .field = "id",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::stress_test_scenario_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_stress_test_scenarios_response
stress_test_scenario_service::delete_many_stress_test_scenarios(
    const messaging::delete_many_stress_test_scenarios_request& request) {
    messaging::delete_many_stress_test_scenarios_response response;
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

messaging::list_stress_test_scenario_versions_response
stress_test_scenario_service::list_stress_test_scenario_versions(
    const messaging::list_stress_test_scenario_versions_request& request) {
    messaging::list_stress_test_scenario_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "stress test scenarios", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "stress test scenarios"});
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

messaging::get_stress_test_scenario_version_response
stress_test_scenario_service::get_stress_test_scenario_version(
    const messaging::get_stress_test_scenario_version_request& request) {
    messaging::get_stress_test_scenario_version_response response;
    auto found = repo_.read_at_version(
        ctx_, boost::uuids::to_string(request.key.stress_test_scenario.id), request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "stress_test_scenario"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
stress_test_scenario_service::prepare_change(const messaging::stress_test_scenario_change& change,
                                             const ores::utility::domain::change_intent& intent,
                                             domain::stress_test_scenario& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain(change.write);
    const auto current = read_one(repo_, ctx_, key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "stress_test_scenario", .field = "id"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "stress_test_scenario"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "stress_test_scenario",
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


std::vector<domain::stress_test_scenario>
stress_test_scenario_service::list_stress_test_scenarios(std::uint32_t offset,
                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all stress test scenarios";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t stress_test_scenario_service::count_stress_test_scenarios() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total stress test scenarios count";
    return repo_.get_total_stress_test_scenario_count(ctx_);
}


std::optional<domain::stress_test_scenario>
stress_test_scenario_service::get_stress_test_scenario_at_version(const boost::uuids::uuid& id,
                                                                  std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting stress test scenario at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(id), version);
}

std::optional<domain::stress_test_scenario>
stress_test_scenario_service::get_stress_test_scenario(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting stress test scenario. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::stress_test_scenario>
stress_test_scenario_service::get_stress_test_scenarios(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void stress_test_scenario_service::save_stress_test_scenario(
    const domain::stress_test_scenario& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Stress Test Scenario id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving stress test scenario. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved stress test scenario. " << "id: " << v.id;
}

void stress_test_scenario_service::save_stress_test_scenarios(
    const std::vector<domain::stress_test_scenario>& stress_test_scenarios) {
    for (const auto& e : stress_test_scenarios) {
        if (e.id.is_nil())
            throw std::invalid_argument("Stress Test Scenario id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << stress_test_scenarios.size()
                               << " stress test scenarios";
    auto ts = stress_test_scenarios;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void stress_test_scenario_service::delete_stress_test_scenario(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing stress test scenario. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed stress test scenario. " << "id: " << id;
}

void stress_test_scenario_service::delete_stress_test_scenarios(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::stress_test_scenario>
stress_test_scenario_service::get_stress_test_scenario_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for stress test scenario. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
