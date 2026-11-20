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
#include "ores.trading.core/service/double_digital_option_instrument_service.hpp"
#include "ores.trading.api/domain/double_digital_option_instrument.hpp"
#include "ores.trading.api/messaging/double_digital_option_instrument_protocol.hpp"
#include "ores.trading.core/repository/double_digital_option_instrument_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <tuple>
#include <utility>
#include <vector>
// Log lines stream uuids with uuid_io's operator<<, which the include check
// does not count as a use.
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp> // IWYU pragma: keep.

using ores::service::messaging::stamp;
// Every refusal names its outcome; the catalogue supplies the code and the
// sentence, so a service states what happened and nothing about how to say it.
using ores::database::domain::outcome_args;
using ores::database::domain::outcome_code;
using ores::database::domain::refuse;
using ores::utility::domain::max_batch_size;

namespace ores::trading::service {

using namespace ores::logging;

double_digital_option_instrument_service::double_digital_option_instrument_service(
    const context& ctx)
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
std::vector<domain::double_digital_option_instrument> double_digital_option_instrument_read_one(
    repository::double_digital_option_instrument_repository& repo,
    const ores::database::context& ctx,
    const messaging::double_digital_option_instrument_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.trade_id));
}

/**
 * @brief The current rows a key set names, one entry per key in the keys' order.
 *
 * Each entry holds what the one-key read holds for its key, so a batch checks
 * each row against the same answer a single request gets. The repository
 * reads the key set in one batch read.
 *
 * A row is joined back to its key by the text form of each key column. Both
 * sides convert the typed value with the same function, and the store compares
 * key text exactly, so every row the read returns finds its key.
 *
 * When the declared key is not the storage key the batch is read by the
 * declared key instead. A model that states its own read by that key has no
 * batch form of it, so its keys are read one at a time.
 */
std::vector<std::vector<domain::double_digital_option_instrument>>
double_digital_option_instrument_read_many(
    repository::double_digital_option_instrument_repository& repo,
    const ores::database::context& ctx,
    const std::vector<messaging::double_digital_option_instrument_key>& keys) {
    std::vector<std::vector<domain::double_digital_option_instrument>> found(keys.size());
    using key_tuple = std::tuple<std::string>;
    std::vector<std::string> trade_id_keys;
    trade_id_keys.reserve(keys.size());
    for (const auto& key : keys) {
        trade_id_keys.push_back(boost::uuids::to_string(key.trade_id));
    }
    std::map<key_tuple, std::vector<domain::double_digital_option_instrument>> by_key;
    for (auto& row : repo.read_latest(ctx, trade_id_keys)) {
        key_tuple named(boost::uuids::to_string(row.identity.trade_id));
        by_key[std::move(named)].push_back(std::move(row));
    }
    for (std::size_t i = 0; i < keys.size(); ++i) {
        const auto& key = keys[i];
        const auto rows = by_key.find(key_tuple(boost::uuids::to_string(key.trade_id)));
        if (rows != by_key.end())
            found[i] = rows->second;
    }
    return found;
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::double_digital_option_instrument_key
double_digital_option_instrument_key_from(const domain::double_digital_option_instrument& v) {
    messaging::double_digital_option_instrument_key key;
    key.trade_id = v.identity.trade_id;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::double_digital_option_instrument double_digital_option_instrument_to_domain(
    const messaging::double_digital_option_instrument_write& write) {
    domain::double_digital_option_instrument v;
    v.identity.trade_id = write.trade_id;
    v.identity.trade_type_code = write.trade_type_code;
    v.identity.trade_activity_id = write.trade_activity_id;
    v.expiry = write.expiry;
    v.settlement = write.settlement;
    v.binary_payout = write.binary_payout;
    v.binary_level_1 = write.binary_level_1;
    v.binary_level_2 = write.binary_level_2;
    v.binary_level_upper_1 = write.binary_level_upper_1;
    v.binary_level_upper_2 = write.binary_level_upper_2;
    v.type_1 = write.type_1;
    v.type_2 = write.type_2;
    v.long_short = write.long_short;
    v.name_1 = write.name_1;
    v.name_2 = write.name_2;
    v.pay_currency = write.pay_currency;
    v.description = write.description;
    return v;
}


}

messaging::list_double_digital_option_instruments_response
double_digital_option_instrument_service::list_double_digital_option_instruments(
    const messaging::list_double_digital_option_instruments_request& request) {
    messaging::list_double_digital_option_instruments_response response;
    if (!request.order.field.empty() &&
        !repository::double_digital_option_instrument_repository::is_sortable(
            request.order.field)) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "double digital option instruments", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->trade_id_one_of &&
        request.filter->trade_id_one_of->size() > max_batch_size) {
        response.result =
            refuse(outcome_code::filter_too_large,
                   {.field = "trade_id_one_of", .limit = std::to_string(max_batch_size)});
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
    response.double_digital_option_instruments = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total =
        repo_.get_total_double_digital_option_instrument_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_double_digital_option_instrument_response
double_digital_option_instrument_service::get_double_digital_option_instrument(
    const messaging::get_double_digital_option_instrument_request& request) {
    messaging::get_double_digital_option_instrument_response response;
    auto found = double_digital_option_instrument_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result =
            refuse(outcome_code::not_found, {.entity = "double_digital_option_instrument"});
        return response;
    }
    response.double_digital_option_instrument = std::move(found.front());
    return response;
}

messaging::get_many_double_digital_option_instruments_response
double_digital_option_instrument_service::get_many_double_digital_option_instruments(
    const messaging::get_many_double_digital_option_instruments_request& request) {
    messaging::get_many_double_digital_option_instruments_response response;
    // A key set carries at most as many keys as the protocol lets a batch
    // carry, so one request cannot ask for an unbounded reply.
    if (request.keys.size() > max_batch_size) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "keys", .limit = std::to_string(max_batch_size)});
        return response;
    }
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    auto found = double_digital_option_instrument_read_many(repo_, ctx_, request.keys);
    for (std::size_t i = 0; i < request.keys.size(); ++i) {
        messaging::double_digital_option_instrument_lookup entry;
        entry.key = request.keys[i];
        if (!found[i].empty())
            entry.double_digital_option_instrument = std::move(found[i].front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_double_digital_option_instrument_response
double_digital_option_instrument_service::put_double_digital_option_instrument(
    const messaging::put_double_digital_option_instrument_request& request) {
    messaging::put_double_digital_option_instrument_response response;
    domain::double_digital_option_instrument value;
    const auto current = double_digital_option_instrument_read_one(
        repo_,
        ctx_,
        double_digital_option_instrument_key_from(
            double_digital_option_instrument_to_domain(request.change.write)));
    response.result = prepare_change(request.change, request.intent, current, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = double_digital_option_instrument_read_one(
        repo_, ctx_, double_digital_option_instrument_key_from(value));
    if (!written.empty())
        response.double_digital_option_instrument = std::move(written.front());
    return response;
}

messaging::put_many_double_digital_option_instruments_response
double_digital_option_instrument_service::put_many_double_digital_option_instruments(
    const messaging::put_many_double_digital_option_instruments_request& request) {
    messaging::put_many_double_digital_option_instruments_response response;
    if (request.changes.size() > max_batch_size) {
        response.result = refuse(outcome_code::batch_too_large,
                                 {.entity = "double_digital_option_instrument",
                                  .limit = std::to_string(max_batch_size)});
        return response;
    }
    // The rows the set names are read once, before any change is checked, so
    // a set of n rows costs one read rather than n.
    std::vector<messaging::double_digital_option_instrument_key> keys;
    keys.reserve(request.changes.size());
    for (const auto& change : request.changes)
        keys.push_back(double_digital_option_instrument_key_from(
            double_digital_option_instrument_to_domain(change.write)));
    const auto current = double_digital_option_instrument_read_many(repo_, ctx_, keys);
    std::vector<domain::double_digital_option_instrument> batch;
    batch.reserve(request.changes.size());
    for (std::size_t i = 0; i < request.changes.size(); ++i) {
        domain::double_digital_option_instrument value;
        const auto result = prepare_change(request.changes[i], request.intent, current[i], value);
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
    response.double_digital_option_instruments.reserve(batch.size());
    // The keys of the written rows are the keys read above: a write keeps
    // the key its change states.
    auto written = double_digital_option_instrument_read_many(repo_, ctx_, keys);
    for (std::size_t i = 0; i < batch.size(); ++i) {
        response.double_digital_option_instruments.push_back(
            written[i].empty() ? batch[i] : std::move(written[i].front()));
    }
    return response;
}

messaging::delete_double_digital_option_instrument_response
double_digital_option_instrument_service::delete_double_digital_option_instrument(
    const messaging::delete_double_digital_option_instrument_request& request) {
    messaging::delete_double_digital_option_instrument_response response;
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
    switch (repo_.remove(ctx_, boost::uuids::to_string(request.removal.key.trade_id), expected)) {
        case repository::double_digital_option_instrument_repository::remove_status::removed:
            break;
        case repository::double_digital_option_instrument_repository::remove_status::missing:
            response.result =
                refuse(outcome_code::not_found, {.entity = "double_digital_option_instrument"});
            break;
        case repository::double_digital_option_instrument_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live =
                double_digital_option_instrument_read_one(repo_, ctx_, request.removal.key);
            response.result =
                refuse(outcome_code::version_conflict,
                       {.entity = "double_digital_option_instrument",
                        .field = "trade_id",
                        .expected = expected ? std::to_string(*expected) : std::string{},
                        .current = live.empty() ? std::string{} :
                                                  std::to_string(live.front().identity.version)});
            break;
        }
        case repository::double_digital_option_instrument_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_double_digital_option_instruments_response
double_digital_option_instrument_service::delete_many_double_digital_option_instruments(
    const messaging::delete_many_double_digital_option_instruments_request& request) {
    messaging::delete_many_double_digital_option_instruments_response response;
    if (request.removals.size() > max_batch_size) {
        response.result = refuse(outcome_code::batch_too_large,
                                 {.entity = "double_digital_option_instrument",
                                  .limit = std::to_string(max_batch_size)});
        return response;
    }
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
    repo_.remove(ctx_, trade_id_keys);
    return response;
}

messaging::list_double_digital_option_instrument_versions_response
double_digital_option_instrument_service::list_double_digital_option_instrument_versions(
    const messaging::list_double_digital_option_instrument_versions_request& request) {
    messaging::list_double_digital_option_instrument_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "double digital option instruments", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result = refuse(outcome_code::filter_not_supported,
                                 {.entity = "double digital option instruments"});
        return response;
    }
    auto all = repo_.read_all(ctx_, boost::uuids::to_string(request.key.trade_id));
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

messaging::get_double_digital_option_instrument_version_response
double_digital_option_instrument_service::get_double_digital_option_instrument_version(
    const messaging::get_double_digital_option_instrument_version_request& request) {
    messaging::get_double_digital_option_instrument_version_response response;
    auto found = repo_.read_at_version(
        ctx_,
        boost::uuids::to_string(request.key.double_digital_option_instrument.trade_id),
        request.key.version);
    if (!found) {
        response.result =
            refuse(outcome_code::not_found, {.entity = "double_digital_option_instrument"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result double_digital_option_instrument_service::prepare_change(
    const messaging::double_digital_option_instrument_change& change,
    const ores::utility::domain::change_intent& intent,
    const std::vector<domain::double_digital_option_instrument>& current,
    domain::double_digital_option_instrument& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = double_digital_option_instrument_to_domain(change.write);
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "double_digital_option_instrument", .field = "trade_id"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found,
                              {.entity = "double_digital_option_instrument"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().identity.version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "double_digital_option_instrument",
                               .field = "trade_id",
                               .expected = change.precondition.version ?
                                               std::to_string(*change.precondition.version) :
                                               std::string{},
                               .current = std::to_string(current.front().identity.version)});
            }
            break;
        case precondition_kind::any:
            break;
    }
    // The version is the repository's to state, from the claim: it is the one
    // thing the store's arbiter reads, and stating it in two places is how the
    // two come to disagree.
    // The audit reaches the row through whichever member carries it, so a
    // grouped entity is stamped member by member; stamping it whole would
    // find no audit column and leave the row without one.
    stamp(out.identity,
          ctx_,
          intent.reason_code.empty() ?
              std::string(ores::service::messaging::change_reasons::new_record) :
              intent.reason_code);
    stamp(out.audit,
          ctx_,
          intent.reason_code.empty() ?
              std::string(ores::service::messaging::change_reasons::new_record) :
              intent.reason_code);
    out.audit.change_commentary = intent.commentary;
    return result;
}


std::vector<domain::double_digital_option_instrument>
double_digital_option_instrument_service::list_double_digital_option_instruments(
    std::uint32_t offset, std::uint32_t limit) {
    ORES_LOG_SEV(lg(), debug) << "Listing all double digital option instruments";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t double_digital_option_instrument_service::count_double_digital_option_instruments() {
    ORES_LOG_SEV(lg(), debug) << "Getting total double digital option instruments count";
    return repo_.get_total_double_digital_option_instrument_count(ctx_);
}


std::optional<domain::double_digital_option_instrument>
double_digital_option_instrument_service::get_double_digital_option_instrument_at_version(
    const boost::uuids::uuid& trade_id, std::uint32_t version) {
    ORES_LOG_SEV(lg(), debug) << "Getting double digital option instrument at version. "
                              << "trade_id: " << trade_id << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(trade_id), version);
}

std::optional<domain::double_digital_option_instrument>
double_digital_option_instrument_service::get_double_digital_option_instrument(
    const boost::uuids::uuid& trade_id) {
    ORES_LOG_SEV(lg(), debug) << "Getting double digital option instrument. "
                              << "trade_id: " << trade_id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(trade_id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::double_digital_option_instrument>
double_digital_option_instrument_service::get_double_digital_option_instruments(
    const std::vector<std::string>& trade_ids) {
    return repo_.read_latest(ctx_, trade_ids);
}

void double_digital_option_instrument_service::save_double_digital_option_instrument(
    const domain::double_digital_option_instrument& v) {
    if (v.identity.trade_id.is_nil())
        throw std::invalid_argument("Double Digital Option Instrument trade_id cannot be empty.");
    ORES_LOG_SEV(lg(), debug) << "Saving double digital option instrument. "
                              << "trade_id: " << v.identity.trade_id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    ORES_LOG_SEV(lg(), info) << "Saved double digital option instrument. "
                             << "trade_id: " << v.identity.trade_id;
}

void double_digital_option_instrument_service::save_double_digital_option_instruments(
    const std::vector<domain::double_digital_option_instrument>&
        double_digital_option_instruments) {
    for (const auto& e : double_digital_option_instruments) {
        if (e.identity.trade_id.is_nil())
            throw std::invalid_argument(
                "Double Digital Option Instrument trade_id cannot be empty.");
    }
    ORES_LOG_SEV(lg(), debug) << "Saving " << double_digital_option_instruments.size()
                              << " double digital option instruments";
    auto ts = double_digital_option_instruments;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void double_digital_option_instrument_service::delete_double_digital_option_instrument(
    const boost::uuids::uuid& trade_id) {
    ORES_LOG_SEV(lg(), debug) << "Removing double digital option instrument. "
                              << "trade_id: " << trade_id;
    repo_.remove(ctx_, boost::uuids::to_string(trade_id));
    ORES_LOG_SEV(lg(), info) << "Removed double digital option instrument. "
                             << "trade_id: " << trade_id;
}

void double_digital_option_instrument_service::delete_double_digital_option_instruments(
    const std::vector<std::string>& trade_ids) {
    repo_.remove(ctx_, trade_ids);
}

std::vector<domain::double_digital_option_instrument>
double_digital_option_instrument_service::get_double_digital_option_instrument_history(
    const std::string& trade_id) {
    ORES_LOG_SEV(lg(), debug) << "Getting history for double digital option instrument. "
                              << "trade_id: " << trade_id;
    return repo_.read_all(ctx_, trade_id);
}

}
