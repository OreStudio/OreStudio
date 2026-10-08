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
#include "ores.refdata.core/service/crm_topology_config_service.hpp"
#include "ores.refdata.api/domain/crm_topology_config.hpp"
#include "ores.refdata.api/messaging/crm_topology_config_protocol.hpp"
#include "ores.refdata.core/repository/crm_topology_config_repository.hpp"
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

crm_topology_config_service::crm_topology_config_service(context ctx)
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
std::vector<domain::crm_topology_config> read_one(repository::crm_topology_config_repository& repo,
                                                  const ores::database::context& ctx,
                                                  const messaging::crm_topology_config_key& key) {
    return repo.read_latest_by_name(ctx, key.name);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::crm_topology_config_key key_from(const domain::crm_topology_config& v) {
    messaging::crm_topology_config_key key;
    key.name = v.name;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::crm_topology_config to_domain(const messaging::crm_topology_config_write& write) {
    domain::crm_topology_config v;
    v.id = write.id;
    v.party_id = write.party_id;
    v.name = write.name;
    v.pivot_currency_code = write.pivot_currency_code;
    v.enabled = write.enabled;
    return v;
}

}

messaging::list_crm_topology_configs_response
crm_topology_config_service::list_crm_topology_configs(
    const messaging::list_crm_topology_configs_request& request) {
    messaging::list_crm_topology_configs_response response;
    if (!request.order.field.empty() &&
        !repository::crm_topology_config_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "CRM topology configs", .field = request.order.field});
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
    response.crm_topology_configs = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_crm_topology_config_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_crm_topology_config_response crm_topology_config_service::get_crm_topology_config(
    const messaging::get_crm_topology_config_request& request) {
    messaging::get_crm_topology_config_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "crm_topology_config"});
        return response;
    }
    response.crm_topology_config = std::move(found.front());
    return response;
}

messaging::get_many_crm_topology_configs_response
crm_topology_config_service::get_many_crm_topology_configs(
    const messaging::get_many_crm_topology_configs_request& request) {
    messaging::get_many_crm_topology_configs_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::crm_topology_config_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.crm_topology_config = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_crm_topology_config_response crm_topology_config_service::put_crm_topology_config(
    const messaging::put_crm_topology_config_request& request) {
    messaging::put_crm_topology_config_response response;
    domain::crm_topology_config value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.crm_topology_config = std::move(written.front());
    return response;
}

messaging::put_many_crm_topology_configs_response
crm_topology_config_service::put_many_crm_topology_configs(
    const messaging::put_many_crm_topology_configs_request& request) {
    messaging::put_many_crm_topology_configs_response response;
    std::vector<domain::crm_topology_config> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::crm_topology_config value;
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
    response.crm_topology_configs.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.crm_topology_configs.push_back(written.empty() ? value :
                                                                  std::move(written.front()));
    }
    return response;
}

messaging::delete_crm_topology_config_response
crm_topology_config_service::delete_crm_topology_config(
    const messaging::delete_crm_topology_config_request& request) {
    messaging::delete_crm_topology_config_response response;
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
    const auto named = read_one(repo_, ctx_, request.removal.key);
    if (named.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "crm_topology_config"});
        return response;
    }
    const auto& row = named.front();
    switch (repo_.remove(ctx_, boost::uuids::to_string(row.id), expected)) {
        case repository::crm_topology_config_repository::remove_status::removed:
            break;
        case repository::crm_topology_config_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "crm_topology_config"});
            break;
        case repository::crm_topology_config_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "crm_topology_config",
                 .field = "id",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::crm_topology_config_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_crm_topology_configs_response
crm_topology_config_service::delete_many_crm_topology_configs(
    const messaging::delete_many_crm_topology_configs_request& request) {
    messaging::delete_many_crm_topology_configs_response response;
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
    std::vector<domain::crm_topology_config> resolved;
    resolved.reserve(request.removals.size());
    for (const auto& removal : request.removals) {
        auto named = read_one(repo_, ctx_, removal.key);
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

messaging::list_crm_topology_config_versions_response
crm_topology_config_service::list_crm_topology_config_versions(
    const messaging::list_crm_topology_config_versions_request& request) {
    messaging::list_crm_topology_config_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "CRM topology configs", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "CRM topology configs"});
        return response;
    }
    // The versions of the row the caller's key names. The repository reads by
    // the storage key, so the declared key is resolved once here.
    const auto named = read_one(repo_, ctx_, request.key);
    if (named.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "crm_topology_config"});
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

messaging::get_crm_topology_config_version_response
crm_topology_config_service::get_crm_topology_config_version(
    const messaging::get_crm_topology_config_version_request& request) {
    messaging::get_crm_topology_config_version_response response;
    // The version key nests the entity's own key, which is the declared one.
    // The repository reads by the storage key, so it is resolved once here.
    const auto named = read_one(repo_, ctx_, request.key.crm_topology_config);
    if (named.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "crm_topology_config"});
        return response;
    }
    const auto& row = named.front();
    auto found = repo_.read_at_version(ctx_, boost::uuids::to_string(row.id), request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "crm_topology_config"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
crm_topology_config_service::prepare_change(const messaging::crm_topology_config_change& change,
                                            const ores::utility::domain::change_intent& intent,
                                            domain::crm_topology_config& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain(change.write);
    const auto current = read_one(repo_, ctx_, key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "crm_topology_config", .field = "id"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "crm_topology_config"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "crm_topology_config",
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


std::vector<domain::crm_topology_config>
crm_topology_config_service::list_crm_topology_configs(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all CRM topology configs";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t crm_topology_config_service::count_crm_topology_configs() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total CRM topology configs count";
    return repo_.get_total_crm_topology_config_count(ctx_);
}


std::optional<domain::crm_topology_config>
crm_topology_config_service::get_crm_topology_config_at_version(const boost::uuids::uuid& id,
                                                                std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting CRM topology config at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(id), version);
}

std::optional<domain::crm_topology_config>
crm_topology_config_service::get_crm_topology_config(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting CRM topology config. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::optional<domain::crm_topology_config>
crm_topology_config_service::get_crm_topology_config_by_name(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting CRM topology config by name: " << name;
    messaging::crm_topology_config_key k;
    k.name = name;
    auto found = read_one(repo_, ctx_, k);
    if (found.empty())
        return std::nullopt;
    return found.front();
}

std::vector<domain::crm_topology_config>
crm_topology_config_service::get_crm_topology_configs(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void crm_topology_config_service::save_crm_topology_config(const domain::crm_topology_config& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("CRM Topology Config id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving CRM topology config. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved CRM topology config. " << "id: " << v.id;
}

void crm_topology_config_service::save_crm_topology_configs(
    const std::vector<domain::crm_topology_config>& crm_topology_configs) {
    for (const auto& e : crm_topology_configs) {
        if (e.id.is_nil())
            throw std::invalid_argument("CRM Topology Config id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << crm_topology_configs.size()
                               << " CRM topology configs";
    auto ts = crm_topology_configs;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void crm_topology_config_service::delete_crm_topology_config(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing CRM topology config. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed CRM topology config. " << "id: " << id;
}

void crm_topology_config_service::delete_crm_topology_configs(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::crm_topology_config>
crm_topology_config_service::get_crm_topology_config_history(const std::string& key) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for CRM topology config. key: " << key;
    // The caller holds the key the model declares and this reads by the
    // storage key, so the two are joined here exactly as they are for any
    // other read. Without this step a provider looks the versions up under a
    // value the storage key never holds, and reports an entity that has a
    // history as having none.
    messaging::crm_topology_config_key k;
    k.name = key;
    // A delete here closes the transaction-time window and leaves every version
    // in place, so resolving through a latest read would lose the history at
    // exactly the moment it is wanted. This takes the newest row carrying the
    // declared key whether or not it is still current, which for a record that
    // still exists is the same row the latest read would have returned.
    const auto found = repo_.read_any_by_name(ctx_, k.name);
    if (found.empty())
        return {};
    const auto& row = found.front();
    return repo_.read_all(ctx_, boost::uuids::to_string(row.id));
}

}
