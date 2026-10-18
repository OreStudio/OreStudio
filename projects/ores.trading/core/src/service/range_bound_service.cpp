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
#include "ores.trading.core/service/range_bound_service.hpp"
#include "ores.trading.api/domain/range_bound.hpp"
#include "ores.trading.api/messaging/range_bound_protocol.hpp"
#include "ores.trading.core/repository/range_bound_repository.hpp"
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

using ores::service::messaging::stamp;
// Every refusal names its outcome; the catalogue supplies the code and the
// sentence, so a service states what happened and nothing about how to say it.
using ores::database::domain::outcome_args;
using ores::database::domain::outcome_code;
using ores::database::domain::refuse;

namespace ores::trading::service {

using namespace ores::logging;

range_bound_service::range_bound_service(context ctx)
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
std::vector<domain::range_bound> read_one(repository::range_bound_repository& repo,
                                          const ores::database::context& ctx,
                                          const messaging::range_bound_key& key) {
    return repo.read_latest(
        ctx, boost::uuids::to_string(key.trade_id), std::to_string(key.sequence_number));
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::range_bound_key key_from(const domain::range_bound& v) {
    messaging::range_bound_key key;
    key.trade_id = v.trade_id;
    key.sequence_number = v.sequence_number;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::range_bound to_domain(const messaging::range_bound_write& write) {
    domain::range_bound v;
    v.trade_id = write.trade_id;
    v.sequence_number = write.sequence_number;
    v.trade_activity_id = write.trade_activity_id;
    v.start_date = write.start_date;
    v.range_from = write.range_from;
    v.range_to = write.range_to;
    v.leverage = write.leverage;
    v.strike = write.strike;
    v.strike_adjustment = write.strike_adjustment;
    return v;
}


}

messaging::list_range_bounds_response
range_bound_service::list_range_bounds(const messaging::list_range_bounds_request& request) {
    messaging::list_range_bounds_response response;
    if (!request.order.field.empty() &&
        !repository::range_bound_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "range bounds", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->trade_id_one_of &&
        request.filter->trade_id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "trade_id_one_of", .limit = "1000"});
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
    response.range_bounds = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_range_bound_count(ctx_, request.filter, as_of);
    return response;
}

messaging::list_by_trade_id_range_bounds_response
range_bound_service::list_by_trade_id_range_bounds(
    const messaging::list_by_trade_id_range_bounds_request& request) {
    messaging::list_by_trade_id_range_bounds_response response;
    if (!request.order.field.empty() &&
        !repository::range_bound_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "range bounds", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->trade_id_one_of &&
        request.filter->trade_id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "trade_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result = refuse(outcome_code::scope_not_supported, {.entity = "range bounds"});
        return response;
    }
    const auto relation = boost::uuids::to_string(request.trade_id);
    response.range_bounds = repo_.read_latest_by_trade_id(
        ctx_, relation, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_range_bound_count_by_trade_id(ctx_, relation, request.filter);
    return response;
}

messaging::get_range_bound_response
range_bound_service::get_range_bound(const messaging::get_range_bound_request& request) {
    messaging::get_range_bound_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "range_bound"});
        return response;
    }
    response.range_bound = std::move(found.front());
    return response;
}

messaging::get_many_range_bounds_response range_bound_service::get_many_range_bounds(
    const messaging::get_many_range_bounds_request& request) {
    messaging::get_many_range_bounds_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::range_bound_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.range_bound = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_range_bound_response
range_bound_service::put_range_bound(const messaging::put_range_bound_request& request) {
    messaging::put_range_bound_response response;
    domain::range_bound value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.range_bound = std::move(written.front());
    return response;
}

messaging::put_many_range_bounds_response range_bound_service::put_many_range_bounds(
    const messaging::put_many_range_bounds_request& request) {
    messaging::put_many_range_bounds_response response;
    std::vector<domain::range_bound> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::range_bound value;
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
    response.range_bounds.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.range_bounds.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_range_bound_response
range_bound_service::delete_range_bound(const messaging::delete_range_bound_request& request) {
    messaging::delete_range_bound_response response;
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
                         boost::uuids::to_string(request.removal.key.trade_id),
                         std::to_string(request.removal.key.sequence_number),
                         expected)) {
        case repository::range_bound_repository::remove_status::removed:
            break;
        case repository::range_bound_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "range_bound"});
            break;
        case repository::range_bound_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "range_bound",
                 .field = "trade_id",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::range_bound_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_range_bounds_response range_bound_service::delete_many_range_bounds(
    const messaging::delete_many_range_bounds_request& request) {
    messaging::delete_many_range_bounds_response response;
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
    std::vector<std::string> trade_id_keys;
    trade_id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        trade_id_keys.push_back(boost::uuids::to_string(removal.key.trade_id));
    std::vector<std::string> sequence_number_keys;
    sequence_number_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        sequence_number_keys.push_back(std::to_string(removal.key.sequence_number));
    repo_.remove(ctx_, trade_id_keys, sequence_number_keys);
    return response;
}

messaging::list_range_bound_versions_response range_bound_service::list_range_bound_versions(
    const messaging::list_range_bound_versions_request& request) {
    messaging::list_range_bound_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "range bounds", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result = refuse(outcome_code::filter_not_supported, {.entity = "range bounds"});
        return response;
    }
    auto all = repo_.read_all(ctx_,
                              boost::uuids::to_string(request.key.trade_id),
                              std::to_string(request.key.sequence_number));
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

messaging::get_range_bound_version_response range_bound_service::get_range_bound_version(
    const messaging::get_range_bound_version_request& request) {
    messaging::get_range_bound_version_response response;
    auto found = repo_.read_at_version(ctx_,
                                       boost::uuids::to_string(request.key.range_bound.trade_id),
                                       std::to_string(request.key.range_bound.sequence_number),
                                       request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "range_bound"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
range_bound_service::prepare_change(const messaging::range_bound_change& change,
                                    const ores::utility::domain::change_intent& intent,
                                    domain::range_bound& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain(change.write);
    const auto current = read_one(repo_, ctx_, key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "range_bound", .field = "trade_id"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "range_bound"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "range_bound",
                               .field = "trade_id",
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


std::vector<domain::range_bound> range_bound_service::list_range_bounds(std::uint32_t offset,
                                                                        std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all range bounds";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t range_bound_service::count_range_bounds() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total range bounds count";
    return repo_.get_total_range_bound_count(ctx_);
}


std::vector<domain::range_bound> range_bound_service::list_range_bounds_by_trade_id(
    const std::string& trade_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing range bounds by trade_id: " << trade_id;
    return repo_.read_latest_by_trade_id(ctx_, trade_id, offset, limit);
}

std::uint32_t range_bound_service::count_range_bounds_by_trade_id(const std::string& trade_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total range bounds count by trade_id: " << trade_id;
    return repo_.get_total_range_bound_count_by_trade_id(ctx_, trade_id);
}


std::optional<domain::range_bound> range_bound_service::get_range_bound_at_version(
    const std::string& trade_id, const std::string& sequence_number, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting range bound at version. " << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    return repo_.read_at_version(ctx_, trade_id, sequence_number, version);
}

std::optional<domain::range_bound>
range_bound_service::get_range_bound(const std::string& trade_id,
                                     const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting range bound. " << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    auto results = repo_.read_latest(ctx_, trade_id, sequence_number);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::range_bound>
range_bound_service::get_range_bounds(const std::vector<std::string>& trade_ids,
                                      const std::vector<std::string>& sequence_numbers) {
    return repo_.read_latest(ctx_, trade_ids, sequence_numbers);
}

void range_bound_service::save_range_bound(const domain::range_bound& v) {
    if (v.trade_id.is_nil())
        throw std::invalid_argument("Range Bound trade_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving range bound. " << "trade_id: " << v.trade_id
                               << " sequence_number: " << v.sequence_number;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved range bound. " << "trade_id: " << v.trade_id
                              << " sequence_number: " << v.sequence_number;
}

void range_bound_service::save_range_bounds(const std::vector<domain::range_bound>& range_bounds) {
    for (const auto& e : range_bounds) {
        if (e.trade_id.is_nil())
            throw std::invalid_argument("Range Bound trade_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << range_bounds.size() << " range bounds";
    auto ts = range_bounds;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void range_bound_service::delete_range_bound(const std::string& trade_id,
                                             const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing range bound. " << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    repo_.remove(ctx_, trade_id, sequence_number);
    BOOST_LOG_SEV(lg(), info) << "Removed range bound. " << "trade_id: " << trade_id
                              << " sequence_number: " << sequence_number;
}

void range_bound_service::delete_range_bounds(const std::vector<std::string>& trade_ids,
                                              const std::vector<std::string>& sequence_numbers) {
    repo_.remove(ctx_, trade_ids, sequence_numbers);
}

std::vector<domain::range_bound>
range_bound_service::get_range_bound_history(const std::string& trade_id,
                                             const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for range bound. " << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    return repo_.read_all(ctx_, trade_id, sequence_number);
}

}
