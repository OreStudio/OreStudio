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
#include "ores.trading.core/service/commodity_basket_constituent_service.hpp"
#include "ores.trading.api/domain/commodity_basket_constituent.hpp"
#include "ores.trading.api/messaging/commodity_basket_constituent_protocol.hpp"
#include "ores.trading.core/repository/commodity_basket_constituent_repository.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>
// Log lines stream uuids with uuid_io's operator<<, which the include check
// does not count as a use.
#include "ores.database/domain/context.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp> // IWYU pragma: keep.

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

commodity_basket_constituent_service::commodity_basket_constituent_service(context ctx)
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
std::vector<domain::commodity_basket_constituent>
read_one(repository::commodity_basket_constituent_repository& repo,
         const ores::database::context& ctx,
         const messaging::commodity_basket_constituent_key& key) {
    return repo.read_latest(
        ctx, boost::uuids::to_string(key.trade_id), std::to_string(key.sequence_number));
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::commodity_basket_constituent_key
key_from(const domain::commodity_basket_constituent& v) {
    messaging::commodity_basket_constituent_key key;
    key.trade_id = v.trade_id;
    key.sequence_number = v.sequence_number;
    return key;
}

/**
 * @brief The record a refusal names, so a message says which row it refused.
 *
 * The key's fields are joined rather than rendered one at a time, because a
 * key of several columns has no single member a message could name.
 */
std::string describe_key(const domain::commodity_basket_constituent& v) {
    std::ostringstream text;
    bool first = true;
    const auto append = [&text, &first](const char* field, const auto& value) {
        if (!first)
            text << ", ";
        first = false;
        text << field << "=" << value;
    };
    append("trade_id", v.trade_id);
    append("sequence_number", v.sequence_number);
    return text.str();
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::commodity_basket_constituent
to_domain(const messaging::commodity_basket_constituent_write& write) {
    domain::commodity_basket_constituent v;
    v.trade_id = write.trade_id;
    v.sequence_number = write.sequence_number;
    v.trade_activity_id = write.trade_activity_id;
    v.underlying_code = write.underlying_code;
    v.weight = write.weight;
    return v;
}

}

messaging::list_commodity_basket_constituents_response
commodity_basket_constituent_service::list_commodity_basket_constituents(
    const messaging::list_commodity_basket_constituents_request& request) {
    messaging::list_commodity_basket_constituents_response response;
    if (!request.order.field.empty() &&
        !repository::commodity_basket_constituent_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message = "A list of commodity basket constituents cannot be ordered by " +
                                  request.order.field + ".";
        return response;
    }
    // A stated instant is checked here, so a malformed one is the caller's
    // mistake rather than a database error. The caller's text is what the
    // store reads, so a fraction of a second is kept.
    std::optional<std::string> as_of;
    if (request.as_of) {
        as_of = ores::database::repository::parse_as_of(*request.as_of);
        if (!as_of) {
            response.result.outcome = ores::utility::domain::outcome::invalid;
            response.result.code = "as_of_invalid";
            response.result.message = "as_of is not a UTC timestamp: " + *request.as_of;
            return response;
        }
    }
    response.commodity_basket_constituents =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, as_of);
    response.total = repo_.get_total_commodity_basket_constituent_count(ctx_, as_of);
    return response;
}

messaging::get_commodity_basket_constituent_response
commodity_basket_constituent_service::get_commodity_basket_constituent(
    const messaging::get_commodity_basket_constituent_request& request) {
    messaging::get_commodity_basket_constituent_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.commodity_basket_constituent = std::move(found.front());
    return response;
}

messaging::get_many_commodity_basket_constituents_response
commodity_basket_constituent_service::get_many_commodity_basket_constituents(
    const messaging::get_many_commodity_basket_constituents_request& request) {
    messaging::get_many_commodity_basket_constituents_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::commodity_basket_constituent_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.commodity_basket_constituent = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_commodity_basket_constituent_response
commodity_basket_constituent_service::put_commodity_basket_constituent(
    const messaging::put_commodity_basket_constituent_request& request) {
    messaging::put_commodity_basket_constituent_response response;
    domain::commodity_basket_constituent value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.commodity_basket_constituent = std::move(written.front());
    return response;
}

messaging::put_many_commodity_basket_constituents_response
commodity_basket_constituent_service::put_many_commodity_basket_constituents(
    const messaging::put_many_commodity_basket_constituents_request& request) {
    messaging::put_many_commodity_basket_constituents_response response;
    std::vector<domain::commodity_basket_constituent> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::commodity_basket_constituent value;
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
    response.commodity_basket_constituents.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.commodity_basket_constituents.push_back(
            written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_commodity_basket_constituent_response
commodity_basket_constituent_service::delete_commodity_basket_constituent(
    const messaging::delete_commodity_basket_constituent_request& request) {
    messaging::delete_commodity_basket_constituent_response response;
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
    switch (repo_.remove(ctx_,
                         boost::uuids::to_string(request.removal.key.trade_id),
                         std::to_string(request.removal.key.sequence_number),
                         expected)) {
        case repository::commodity_basket_constituent_repository::remove_status::removed:
            break;
        case repository::commodity_basket_constituent_repository::remove_status::missing:
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            break;
        case repository::commodity_basket_constituent_repository::remove_status::conflicting:
            response.result.outcome = outcome::conflict;
            response.result.code = "version_conflict";
            break;
        case repository::commodity_basket_constituent_repository::remove_status::unsupported:
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_not_supported";
            response.result.message = "This resource keeps no version to match.";
            break;
    }
    return response;
}

messaging::delete_many_commodity_basket_constituents_response
commodity_basket_constituent_service::delete_many_commodity_basket_constituents(
    const messaging::delete_many_commodity_basket_constituents_request& request) {
    messaging::delete_many_commodity_basket_constituents_response response;
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

messaging::list_commodity_basket_constituent_versions_response
commodity_basket_constituent_service::list_commodity_basket_constituent_versions(
    const messaging::list_commodity_basket_constituent_versions_request& request) {
    messaging::list_commodity_basket_constituent_versions_response response;
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

messaging::get_commodity_basket_constituent_version_response
commodity_basket_constituent_service::get_commodity_basket_constituent_version(
    const messaging::get_commodity_basket_constituent_version_request& request) {
    messaging::get_commodity_basket_constituent_version_response response;
    auto found = repo_.read_at_version(
        ctx_,
        boost::uuids::to_string(request.key.commodity_basket_constituent.trade_id),
        std::to_string(request.key.commodity_basket_constituent.sequence_number),
        request.key.version);
    if (!found) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result commodity_basket_constituent_service::prepare_change(
    const messaging::commodity_basket_constituent_change& change,
    const ores::utility::domain::change_intent& intent,
    domain::commodity_basket_constituent& out) {
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
                result.message =
                    "A commodity_basket_constituent with " + describe_key(out) +
                    " already exists. State the version you read to replace it, or ask "
                    "for a version replace.";
                result.fields.push_back(
                    {"trade_id", "already_exists", "This value is already taken."});
                result.fields.push_back(
                    {"sequence_number", "already_exists", "This value is already taken."});
                return result;
            }
            break;
        case precondition_kind::must_match_version:
            if (current.empty()) {
                result.outcome = outcome::missing;
                result.code = "not_found";
                result.message =
                    "No commodity_basket_constituent with " + describe_key(out) + " exists.";
                result.fields.push_back({"trade_id", "not_found", "No record holds this value."});
                result.fields.push_back(
                    {"sequence_number", "not_found", "No record holds this value."});
                return result;
            }
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                result.outcome = outcome::conflict;
                result.code = "version_conflict";
                result.message = "The commodity_basket_constituent with " + describe_key(out) +
                                 " is at version " + std::to_string(current.front().version) +
                                 ", and the write states " +
                                 std::to_string(*change.precondition.version) + ".";
                result.fields.push_back(
                    {"version",
                     "version_conflict",
                     "The record moved since it was read. Read it again and state the "
                     "version you read."});
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


std::vector<domain::commodity_basket_constituent>
commodity_basket_constituent_service::list_commodity_basket_constituents(std::uint32_t offset,
                                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all commodity basket constituents";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t commodity_basket_constituent_service::count_commodity_basket_constituents() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total commodity basket constituents count";
    return repo_.get_total_commodity_basket_constituent_count(ctx_);
}


std::optional<domain::commodity_basket_constituent>
commodity_basket_constituent_service::get_commodity_basket_constituent_at_version(
    const std::string& trade_id, const std::string& sequence_number, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting commodity basket constituent at version. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    return repo_.read_at_version(ctx_, trade_id, sequence_number, version);
}

std::optional<domain::commodity_basket_constituent>
commodity_basket_constituent_service::get_commodity_basket_constituent(
    const std::string& trade_id, const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting commodity basket constituent. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    auto results = repo_.read_latest(ctx_, trade_id, sequence_number);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::commodity_basket_constituent>
commodity_basket_constituent_service::get_commodity_basket_constituents(
    const std::vector<std::string>& trade_ids, const std::vector<std::string>& sequence_numbers) {
    return repo_.read_latest(ctx_, trade_ids, sequence_numbers);
}

void commodity_basket_constituent_service::save_commodity_basket_constituent(
    const domain::commodity_basket_constituent& v) {
    if (v.trade_id.is_nil())
        throw std::invalid_argument("Commodity Basket Constituent trade_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving commodity basket constituent. "
                               << "trade_id: " << v.trade_id
                               << " sequence_number: " << v.sequence_number;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved commodity basket constituent. "
                              << "trade_id: " << v.trade_id
                              << " sequence_number: " << v.sequence_number;
}

void commodity_basket_constituent_service::save_commodity_basket_constituents(
    const std::vector<domain::commodity_basket_constituent>& commodity_basket_constituents) {
    for (const auto& e : commodity_basket_constituents) {
        if (e.trade_id.is_nil())
            throw std::invalid_argument("Commodity Basket Constituent trade_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << commodity_basket_constituents.size()
                               << " commodity basket constituents";
    auto ts = commodity_basket_constituents;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void commodity_basket_constituent_service::delete_commodity_basket_constituent(
    const std::string& trade_id, const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing commodity basket constituent. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    repo_.remove(ctx_, trade_id, sequence_number);
    BOOST_LOG_SEV(lg(), info) << "Removed commodity basket constituent. "
                              << "trade_id: " << trade_id
                              << " sequence_number: " << sequence_number;
}

void commodity_basket_constituent_service::delete_commodity_basket_constituents(
    const std::vector<std::string>& trade_ids, const std::vector<std::string>& sequence_numbers) {
    repo_.remove(ctx_, trade_ids, sequence_numbers);
}

std::vector<domain::commodity_basket_constituent>
commodity_basket_constituent_service::get_commodity_basket_constituent_history(
    const std::string& trade_id, const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for commodity basket constituent. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    return repo_.read_all(ctx_, trade_id, sequence_number);
}

}
