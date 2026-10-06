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
#include "ores.refdata.core/service/cross_currency_basis_convention_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/cross_currency_basis_convention.hpp"
#include "ores.refdata.api/messaging/cross_currency_basis_convention_protocol.hpp"
#include "ores.refdata.core/repository/cross_currency_basis_convention_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
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

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

cross_currency_basis_convention_service::cross_currency_basis_convention_service(context ctx)
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
std::vector<domain::cross_currency_basis_convention>
read_one(repository::cross_currency_basis_convention_repository& repo,
         const ores::database::context& ctx,
         const messaging::cross_currency_basis_convention_key& key) {
    return repo.read_latest(ctx, key.id);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::cross_currency_basis_convention_key
key_from(const domain::cross_currency_basis_convention& v) {
    messaging::cross_currency_basis_convention_key key;
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
domain::cross_currency_basis_convention
to_domain(const messaging::cross_currency_basis_convention_write& write) {
    domain::cross_currency_basis_convention v;
    v.id = write.id;
    v.settlement_days = write.settlement_days;
    v.settlement_calendar = write.settlement_calendar;
    v.roll_convention = write.roll_convention;
    v.flat_index = write.flat_index;
    v.spread_index = write.spread_index;
    v.eom = write.eom;
    v.is_resettable = write.is_resettable;
    v.flat_index_is_resettable = write.flat_index_is_resettable;
    v.flat_tenor = write.flat_tenor;
    v.spread_tenor = write.spread_tenor;
    v.spread_payment_lag = write.spread_payment_lag;
    v.flat_payment_lag = write.flat_payment_lag;
    v.spread_include_spread = write.spread_include_spread;
    v.spread_lookback = write.spread_lookback;
    v.spread_fixing_days = write.spread_fixing_days;
    v.spread_rate_cutoff = write.spread_rate_cutoff;
    v.spread_is_averaged = write.spread_is_averaged;
    v.spread_observation_shift = write.spread_observation_shift;
    v.flat_include_spread = write.flat_include_spread;
    v.flat_lookback = write.flat_lookback;
    v.flat_fixing_days = write.flat_fixing_days;
    v.flat_rate_cutoff = write.flat_rate_cutoff;
    v.flat_is_averaged = write.flat_is_averaged;
    v.flat_observation_shift = write.flat_observation_shift;
    return v;
}

}

messaging::list_cross_currency_basis_conventions_response
cross_currency_basis_convention_service::list_cross_currency_basis_conventions(
    const messaging::list_cross_currency_basis_conventions_request& request) {
    messaging::list_cross_currency_basis_conventions_response response;
    if (!request.order.field.empty() &&
        !repository::cross_currency_basis_convention_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of cross-currency basis conventions cannot be ordered by " +
            request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in id_one_of.";
        return response;
    }
    response.cross_currency_basis_conventions =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_cross_currency_basis_convention_count(ctx_, request.filter);
    return response;
}

messaging::get_cross_currency_basis_convention_response
cross_currency_basis_convention_service::get_cross_currency_basis_convention(
    const messaging::get_cross_currency_basis_convention_request& request) {
    messaging::get_cross_currency_basis_convention_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.cross_currency_basis_convention = std::move(found.front());
    return response;
}

messaging::get_many_cross_currency_basis_conventions_response
cross_currency_basis_convention_service::get_many_cross_currency_basis_conventions(
    const messaging::get_many_cross_currency_basis_conventions_request& request) {
    messaging::get_many_cross_currency_basis_conventions_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::cross_currency_basis_convention_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.cross_currency_basis_convention = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_cross_currency_basis_convention_response
cross_currency_basis_convention_service::put_cross_currency_basis_convention(
    const messaging::put_cross_currency_basis_convention_request& request) {
    messaging::put_cross_currency_basis_convention_response response;
    domain::cross_currency_basis_convention value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.cross_currency_basis_convention = std::move(written.front());
    return response;
}

messaging::put_many_cross_currency_basis_conventions_response
cross_currency_basis_convention_service::put_many_cross_currency_basis_conventions(
    const messaging::put_many_cross_currency_basis_conventions_request& request) {
    messaging::put_many_cross_currency_basis_conventions_response response;
    std::vector<domain::cross_currency_basis_convention> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::cross_currency_basis_convention value;
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
    response.cross_currency_basis_conventions.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.cross_currency_basis_conventions.push_back(
            written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_cross_currency_basis_convention_response
cross_currency_basis_convention_service::delete_cross_currency_basis_convention(
    const messaging::delete_cross_currency_basis_convention_request& request) {
    messaging::delete_cross_currency_basis_convention_response response;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    if (request.removal.precondition.kind == precondition_kind::must_not_exist) {
        response.result.outcome = outcome::invalid;
        response.result.code = "precondition_not_supported";
        response.result.message = "A removal cannot require that a row is absent.";
        return response;
    }
    std::optional<std::uint32_t> expected;
    if (request.removal.precondition.kind == precondition_kind::must_match_version) {
        if (!request.removal.precondition.version) {
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_incomplete";
            response.result.message = "A versioned removal must state the version it expects.";
            return response;
        }
        expected = request.removal.precondition.version;
    }
    switch (repo_.remove(ctx_, request.removal.key.id, expected)) {
        case repository::cross_currency_basis_convention_repository::remove_status::removed:
            break;
        case repository::cross_currency_basis_convention_repository::remove_status::missing:
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            break;
        case repository::cross_currency_basis_convention_repository::remove_status::conflicting:
            response.result.outcome = outcome::conflict;
            response.result.code = "version_conflict";
            break;
        case repository::cross_currency_basis_convention_repository::remove_status::unsupported:
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_not_supported";
            response.result.message = "This resource keeps no version to match.";
            break;
    }
    return response;
}

messaging::delete_many_cross_currency_basis_conventions_response
cross_currency_basis_convention_service::delete_many_cross_currency_basis_conventions(
    const messaging::delete_many_cross_currency_basis_conventions_request& request) {
    messaging::delete_many_cross_currency_basis_conventions_response response;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    for (const auto& removal : request.removals) {
        if (removal.precondition.kind != precondition_kind::any) {
            // The store removes a set in one statement, which carries no
            // per-row version. Refusing is the only answer that keeps the
            // batch atomic: serving it as a sequence of single removals would
            // leave a partial batch behind as soon as one row had moved on.
            response.result.outcome = outcome::invalid;
            response.result.code = "batch_removal_is_unconditional";
            response.result.message =
                "A batch removal is unconditional; remove the rows one at a time "
                "to state a version.";
            return response;
        }
    }
    if (request.removals.empty())
        return response;
    std::vector<std::string> id_keys;
    id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        id_keys.push_back(removal.key.id);
    repo_.remove(ctx_, id_keys);
    return response;
}

messaging::list_cross_currency_basis_convention_versions_response
cross_currency_basis_convention_service::list_cross_currency_basis_convention_versions(
    const messaging::list_cross_currency_basis_convention_versions_request& request) {
    messaging::list_cross_currency_basis_convention_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "This store pages in key order and cannot order by a stated field.";
        return response;
    }
    if (request.filter) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_not_supported";
        response.result.message = "Filtering is not served for this resource yet.";
        return response;
    }
    auto all = repo_.read_all(ctx_, request.key.id);
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

messaging::get_cross_currency_basis_convention_version_response
cross_currency_basis_convention_service::get_cross_currency_basis_convention_version(
    const messaging::get_cross_currency_basis_convention_version_request& request) {
    messaging::get_cross_currency_basis_convention_version_response response;
    auto found = repo_.read_at_version(
        ctx_, request.key.cross_currency_basis_convention.id, request.key.version);
    if (!found) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result cross_currency_basis_convention_service::prepare_change(
    const messaging::cross_currency_basis_convention_change& change,
    const ores::utility::domain::change_intent& intent,
    domain::cross_currency_basis_convention& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain(change.write);
    const auto current = read_one(repo_, ctx_, key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty()) {
                result.outcome = outcome::conflict;
                result.code = "already_exists";
                return result;
            }
            break;
        case precondition_kind::must_match_version:
            if (current.empty()) {
                result.outcome = outcome::missing;
                result.code = "not_found";
                return result;
            }
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                result.outcome = outcome::conflict;
                result.code = "version_conflict";
                return result;
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


std::vector<domain::cross_currency_basis_convention>
cross_currency_basis_convention_service::list_cross_currency_basis_conventions(
    std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all cross-currency basis conventions";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t cross_currency_basis_convention_service::count_cross_currency_basis_conventions() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total cross-currency basis conventions count";
    return repo_.get_total_cross_currency_basis_convention_count(ctx_);
}


std::optional<domain::cross_currency_basis_convention>
cross_currency_basis_convention_service::get_cross_currency_basis_convention_at_version(
    const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting cross-currency basis convention at version. "
                               << "id: " << id << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::cross_currency_basis_convention>
cross_currency_basis_convention_service::get_cross_currency_basis_convention(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting cross-currency basis convention. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::cross_currency_basis_convention>
cross_currency_basis_convention_service::get_cross_currency_basis_conventions(
    const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void cross_currency_basis_convention_service::save_cross_currency_basis_convention(
    const domain::cross_currency_basis_convention& v) {
    if (v.id.empty())
        throw std::invalid_argument("Cross-Currency Basis Convention id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving cross-currency basis convention. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved cross-currency basis convention. " << "id: " << v.id;
}

void cross_currency_basis_convention_service::save_cross_currency_basis_conventions(
    const std::vector<domain::cross_currency_basis_convention>& cross_currency_basis_conventions) {
    for (const auto& e : cross_currency_basis_conventions) {
        if (e.id.empty())
            throw std::invalid_argument("Cross-Currency Basis Convention id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << cross_currency_basis_conventions.size()
                               << " cross-currency basis conventions";
    auto ts = cross_currency_basis_conventions;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void cross_currency_basis_convention_service::delete_cross_currency_basis_convention(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing cross-currency basis convention. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed cross-currency basis convention. " << "id: " << id;
}

void cross_currency_basis_convention_service::delete_cross_currency_basis_conventions(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::cross_currency_basis_convention>
cross_currency_basis_convention_service::get_cross_currency_basis_convention_history(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for cross-currency basis convention. "
                               << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
