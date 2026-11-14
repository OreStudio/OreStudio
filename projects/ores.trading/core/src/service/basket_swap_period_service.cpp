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
#include "ores.trading.core/service/basket_swap_period_service.hpp"
#include "ores.trading.api/domain/basket_swap_period.hpp"
#include "ores.trading.api/messaging/basket_swap_period_protocol.hpp"
#include "ores.trading.core/repository/basket_swap_period_repository.hpp"
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

basket_swap_period_service::basket_swap_period_service(const context& ctx)
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
std::vector<domain::basket_swap_period>
basket_swap_period_read_one(repository::basket_swap_period_repository& repo,
                            const ores::database::context& ctx,
                            const messaging::basket_swap_period_key& key) {
    return repo.read_latest(ctx,
                            boost::uuids::to_string(key.trade_id),
                            boost::uuids::to_string(key.trade_activity_id),
                            std::to_string(key.sequence_number));
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::basket_swap_period_key basket_swap_period_key_from(const domain::basket_swap_period& v) {
    messaging::basket_swap_period_key key;
    key.trade_id = v.trade_id;
    key.trade_activity_id = v.trade_activity_id;
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
domain::basket_swap_period
basket_swap_period_to_domain(const messaging::basket_swap_period_write& write) {
    domain::basket_swap_period v;
    v.trade_id = write.trade_id;
    v.trade_activity_id = write.trade_activity_id;
    v.sequence_number = write.sequence_number;
    v.period_date = write.period_date;
    v.knock_out_level = write.knock_out_level;
    v.fixed_trigger_level = write.fixed_trigger_level;
    return v;
}


}

messaging::list_basket_swap_periods_response basket_swap_period_service::list_basket_swap_periods(
    const messaging::list_basket_swap_periods_request& request) {
    messaging::list_basket_swap_periods_response response;
    if (!request.order.field.empty() &&
        !repository::basket_swap_period_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "basket swap periods", .field = request.order.field});
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
    response.basket_swap_periods =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, as_of);
    response.total = repo_.get_total_basket_swap_period_count(ctx_, as_of);
    return response;
}

messaging::get_basket_swap_period_response basket_swap_period_service::get_basket_swap_period(
    const messaging::get_basket_swap_period_request& request) {
    messaging::get_basket_swap_period_response response;
    auto found = basket_swap_period_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "basket_swap_period"});
        return response;
    }
    response.basket_swap_period = std::move(found.front());
    return response;
}

messaging::get_many_basket_swap_periods_response
basket_swap_period_service::get_many_basket_swap_periods(
    const messaging::get_many_basket_swap_periods_request& request) {
    messaging::get_many_basket_swap_periods_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::basket_swap_period_lookup entry;
        entry.key = k;
        auto found = basket_swap_period_read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.basket_swap_period = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_basket_swap_period_response basket_swap_period_service::put_basket_swap_period(
    const messaging::put_basket_swap_period_request& request) {
    messaging::put_basket_swap_period_response response;
    domain::basket_swap_period value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = basket_swap_period_read_one(repo_, ctx_, basket_swap_period_key_from(value));
    if (!written.empty())
        response.basket_swap_period = std::move(written.front());
    return response;
}

messaging::put_many_basket_swap_periods_response
basket_swap_period_service::put_many_basket_swap_periods(
    const messaging::put_many_basket_swap_periods_request& request) {
    messaging::put_many_basket_swap_periods_response response;
    std::vector<domain::basket_swap_period> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::basket_swap_period value;
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
    response.basket_swap_periods.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = basket_swap_period_read_one(repo_, ctx_, basket_swap_period_key_from(value));
        response.basket_swap_periods.push_back(written.empty() ? value :
                                                                 std::move(written.front()));
    }
    return response;
}

messaging::delete_basket_swap_period_response basket_swap_period_service::delete_basket_swap_period(
    const messaging::delete_basket_swap_period_request& request) {
    messaging::delete_basket_swap_period_response response;
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
                         boost::uuids::to_string(request.removal.key.trade_activity_id),
                         std::to_string(request.removal.key.sequence_number),
                         expected)) {
        case repository::basket_swap_period_repository::remove_status::removed:
            break;
        case repository::basket_swap_period_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "basket_swap_period"});
            break;
        case repository::basket_swap_period_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = basket_swap_period_read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "basket_swap_period",
                 .field = "trade_id",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::basket_swap_period_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_basket_swap_periods_response
basket_swap_period_service::delete_many_basket_swap_periods(
    const messaging::delete_many_basket_swap_periods_request& request) {
    messaging::delete_many_basket_swap_periods_response response;
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
    std::vector<std::string> trade_activity_id_keys;
    trade_activity_id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        trade_activity_id_keys.push_back(boost::uuids::to_string(removal.key.trade_activity_id));
    std::vector<std::string> sequence_number_keys;
    sequence_number_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        sequence_number_keys.push_back(std::to_string(removal.key.sequence_number));
    repo_.remove(ctx_, trade_id_keys, trade_activity_id_keys, sequence_number_keys);
    return response;
}

messaging::list_basket_swap_period_versions_response
basket_swap_period_service::list_basket_swap_period_versions(
    const messaging::list_basket_swap_period_versions_request& request) {
    messaging::list_basket_swap_period_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "basket swap periods", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "basket swap periods"});
        return response;
    }
    auto all = repo_.read_all(ctx_,
                              boost::uuids::to_string(request.key.trade_id),
                              boost::uuids::to_string(request.key.trade_activity_id),
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

messaging::get_basket_swap_period_version_response
basket_swap_period_service::get_basket_swap_period_version(
    const messaging::get_basket_swap_period_version_request& request) {
    messaging::get_basket_swap_period_version_response response;
    auto found = repo_.read_at_version(
        ctx_,
        boost::uuids::to_string(request.key.basket_swap_period.trade_id),
        boost::uuids::to_string(request.key.basket_swap_period.trade_activity_id),
        std::to_string(request.key.basket_swap_period.sequence_number),
        request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "basket_swap_period"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
basket_swap_period_service::prepare_change(const messaging::basket_swap_period_change& change,
                                           const ores::utility::domain::change_intent& intent,
                                           domain::basket_swap_period& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = basket_swap_period_to_domain(change.write);
    const auto current = basket_swap_period_read_one(repo_, ctx_, basket_swap_period_key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "basket_swap_period", .field = "trade_id"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "basket_swap_period"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "basket_swap_period",
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


std::vector<domain::basket_swap_period>
basket_swap_period_service::list_basket_swap_periods(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all basket swap periods";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t basket_swap_period_service::count_basket_swap_periods() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total basket swap periods count";
    return repo_.get_total_basket_swap_period_count(ctx_);
}


std::optional<domain::basket_swap_period>
basket_swap_period_service::get_basket_swap_period_at_version(const std::string& trade_id,
                                                              const std::string& trade_activity_id,
                                                              const std::string& sequence_number,
                                                              std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting basket swap period at version. "
                               << "trade_id: " << trade_id
                               << " trade_activity_id: " << trade_activity_id
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    return repo_.read_at_version(ctx_, trade_id, trade_activity_id, sequence_number, version);
}

std::optional<domain::basket_swap_period>
basket_swap_period_service::get_basket_swap_period(const std::string& trade_id,
                                                   const std::string& trade_activity_id,
                                                   const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting basket swap period. " << "trade_id: " << trade_id
                               << " trade_activity_id: " << trade_activity_id
                               << " sequence_number: " << sequence_number;
    auto results = repo_.read_latest(ctx_, trade_id, trade_activity_id, sequence_number);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::basket_swap_period> basket_swap_period_service::get_basket_swap_periods(
    const std::vector<std::string>& trade_ids,
    const std::vector<std::string>& trade_activity_ids,
    const std::vector<std::string>& sequence_numbers) {
    return repo_.read_latest(ctx_, trade_ids, trade_activity_ids, sequence_numbers);
}

void basket_swap_period_service::save_basket_swap_period(const domain::basket_swap_period& v) {
    if (v.trade_id.is_nil())
        throw std::invalid_argument("Basket Swap Period trade_id cannot be empty.");
    if (v.trade_activity_id.is_nil())
        throw std::invalid_argument("Basket Swap Period trade_activity_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving basket swap period. " << "trade_id: " << v.trade_id
                               << " trade_activity_id: " << v.trade_activity_id
                               << " sequence_number: " << v.sequence_number;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved basket swap period. " << "trade_id: " << v.trade_id
                              << " trade_activity_id: " << v.trade_activity_id
                              << " sequence_number: " << v.sequence_number;
}

void basket_swap_period_service::save_basket_swap_periods(
    const std::vector<domain::basket_swap_period>& basket_swap_periods) {
    for (const auto& e : basket_swap_periods) {
        if (e.trade_id.is_nil())
            throw std::invalid_argument("Basket Swap Period trade_id cannot be empty.");
        if (e.trade_activity_id.is_nil())
            throw std::invalid_argument("Basket Swap Period trade_activity_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << basket_swap_periods.size() << " basket swap periods";
    auto ts = basket_swap_periods;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void basket_swap_period_service::delete_basket_swap_period(const std::string& trade_id,
                                                           const std::string& trade_activity_id,
                                                           const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing basket swap period. " << "trade_id: " << trade_id
                               << " trade_activity_id: " << trade_activity_id
                               << " sequence_number: " << sequence_number;
    repo_.remove(ctx_, trade_id, trade_activity_id, sequence_number);
    BOOST_LOG_SEV(lg(), info) << "Removed basket swap period. " << "trade_id: " << trade_id
                              << " trade_activity_id: " << trade_activity_id
                              << " sequence_number: " << sequence_number;
}

void basket_swap_period_service::delete_basket_swap_periods(
    const std::vector<std::string>& trade_ids,
    const std::vector<std::string>& trade_activity_ids,
    const std::vector<std::string>& sequence_numbers) {
    repo_.remove(ctx_, trade_ids, trade_activity_ids, sequence_numbers);
}

std::vector<domain::basket_swap_period>
basket_swap_period_service::get_basket_swap_period_history(const std::string& trade_id,
                                                           const std::string& trade_activity_id,
                                                           const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for basket swap period. "
                               << "trade_id: " << trade_id
                               << " trade_activity_id: " << trade_activity_id
                               << " sequence_number: " << sequence_number;
    return repo_.read_all(ctx_, trade_id, trade_activity_id, sequence_number);
}

}
