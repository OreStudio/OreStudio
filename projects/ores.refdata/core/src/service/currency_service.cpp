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
#include "ores.refdata.core/service/currency_service.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

currency_service::currency_service(context ctx)
    : ctx_(std::move(ctx))
    , junction_repo_(ctx_) {}
namespace {

/**
 * @brief The current row a key names, or an empty vector when there is none.
 *
 * A key record carries each column with the column's own type, and the
 * repository takes the text form every one of its key parameters shares, so
 * the conversion lives here rather than at every call site.
 */
std::vector<domain::currency> read_one(repository::currency_repository& repo,
                                       const ores::database::context& ctx,
                                       const messaging::currency_key& key) {
    return repo.read_latest(ctx, key.iso_code);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::currency_key key_from(const domain::currency& v) {
    messaging::currency_key key;
    key.iso_code = v.iso_code;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::currency to_domain(const messaging::currency_write& write) {
    domain::currency v;
    v.iso_code = write.iso_code;
    v.name = write.name;
    v.numeric_code = write.numeric_code;
    v.symbol = write.symbol;
    v.fraction_symbol = write.fraction_symbol;
    v.fractions_per_unit = write.fractions_per_unit;
    v.rounding_type = write.rounding_type;
    v.rounding_precision = write.rounding_precision;
    v.format = write.format;
    v.monetary_nature = write.monetary_nature;
    v.market_tier = write.market_tier;
    v.image_id = write.image_id;
    v.spot_days = write.spot_days;
    v.day_basis = write.day_basis;
    v.base_precedence = write.base_precedence;
    return v;
}

} // namespace

messaging::list_currencies_response
currency_service::list_currencies(const messaging::list_currencies_request& request) {
    messaging::list_currencies_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "This store pages in key order and cannot order by a stated field.";
        return response;
    }
    response.currencies = repo_.read_latest(ctx_, request.offset, request.limit);
    response.total = repo_.get_total_currency_count(ctx_);
    return response;
}

messaging::get_currency_response
currency_service::get_currency(const messaging::get_currency_request& request) {
    messaging::get_currency_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.currency = std::move(found.front());
    return response;
}

messaging::get_many_currencies_response
currency_service::get_many_currencies(const messaging::get_many_currencies_request& request) {
    messaging::get_many_currencies_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::currency_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.currency = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_currency_response
currency_service::put_currency(const messaging::put_currency_request& request) {
    messaging::put_currency_response response;
    domain::currency value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.currency = std::move(written.front());
    return response;
}

messaging::put_many_currencies_response
currency_service::put_many_currencies(const messaging::put_many_currencies_request& request) {
    messaging::put_many_currencies_response response;
    std::vector<domain::currency> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::currency value;
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
    response.currencies.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.currencies.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_currency_response
currency_service::delete_currency(const messaging::delete_currency_request& request) {
    messaging::delete_currency_response response;
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
    switch (repo_.remove(ctx_, request.removal.key.iso_code, expected)) {
        case repository::currency_repository::remove_status::removed:
            break;
        case repository::currency_repository::remove_status::missing:
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            break;
        case repository::currency_repository::remove_status::conflicting:
            response.result.outcome = outcome::conflict;
            response.result.code = "version_conflict";
            break;
        case repository::currency_repository::remove_status::unsupported:
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_not_supported";
            response.result.message = "This resource keeps no version to match.";
            break;
    }
    return response;
}

messaging::delete_many_currencies_response
currency_service::delete_many_currencies(const messaging::delete_many_currencies_request& request) {
    messaging::delete_many_currencies_response response;
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
    std::vector<std::string> iso_code_keys;
    iso_code_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        iso_code_keys.push_back(removal.key.iso_code);
    repo_.remove(ctx_, iso_code_keys);
    return response;
}

messaging::list_currency_versions_response
currency_service::list_currency_versions(const messaging::list_currency_versions_request& request) {
    messaging::list_currency_versions_response response;
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
    auto all = repo_.read_all(ctx_, request.key.iso_code);
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

messaging::get_currency_version_response
currency_service::get_currency_version(const messaging::get_currency_version_request& request) {
    messaging::get_currency_version_response response;
    auto found = repo_.read_at_version(ctx_, request.key.currency.iso_code, request.key.version);
    if (!found) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
currency_service::prepare_change(const messaging::currency_change& change,
                                 const ores::utility::domain::change_intent& intent,
                                 domain::currency& out) {
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


std::vector<domain::currency> currency_service::list_currencies(std::uint32_t offset,
                                                                std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all currencies";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t currency_service::count_currencies() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total currencies count";
    return repo_.get_total_currency_count(ctx_);
}


std::optional<domain::currency>
currency_service::get_currency_at_version(const std::string& iso_code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting currency at version. " << "iso_code: " << iso_code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, iso_code, version);
}

std::optional<domain::currency> currency_service::get_currency(const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting currency. " << "iso_code: " << iso_code;
    auto results = repo_.read_latest(ctx_, iso_code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::currency>
currency_service::get_currencies(const std::vector<std::string>& iso_codes) {
    return repo_.read_latest(ctx_, iso_codes);
}

void currency_service::save_currency(const domain::currency& v) {
    if (v.iso_code.empty())
        throw std::invalid_argument("Currency iso_code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving currency. " << "iso_code: " << v.iso_code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved currency. " << "iso_code: " << v.iso_code;
}

void currency_service::save_currencies(const std::vector<domain::currency>& currencies) {
    for (const auto& e : currencies) {
        if (e.iso_code.empty())
            throw std::invalid_argument("Currency iso_code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << currencies.size() << " currencies";
    auto ts = currencies;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void currency_service::delete_currency(const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency. " << "iso_code: " << iso_code;
    repo_.remove(ctx_, iso_code);
    BOOST_LOG_SEV(lg(), info) << "Removed currency. " << "iso_code: " << iso_code;
}

void currency_service::delete_currencies(const std::vector<std::string>& iso_codes) {
    repo_.remove(ctx_, iso_codes);
}

std::vector<domain::currency> currency_service::get_currency_history(const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for currency. " << "iso_code: " << iso_code;
    return repo_.read_all(ctx_, iso_code);
}

std::vector<domain::currency> currency_service::list_currencies_for_party(
    const boost::uuids::uuid& party_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing currencies for party: " << party_id
                               << " offset=" << offset << " limit=" << limit;

    const auto junctions = junction_repo_.read_latest_by_party(party_id);
    std::unordered_set<std::string> visible;
    visible.reserve(junctions.size());
    for (const auto& j : junctions)
        visible.insert(j.currency_iso_code);

    auto all = repo_.read_latest(ctx_);
    std::vector<domain::currency> filtered;
    filtered.reserve(visible.size());
    for (auto& c : all) {
        if (visible.count(c.iso_code))
            filtered.push_back(std::move(c));
    }

    if (offset >= filtered.size())
        return {};
    const auto end = std::min<std::size_t>(offset + limit, filtered.size());
    return std::vector<domain::currency>(filtered.begin() + offset, filtered.begin() + end);
}

std::uint32_t currency_service::count_currencies_for_party(const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Counting currencies for party: " << party_id;
    const auto junctions = junction_repo_.read_latest_by_party(party_id);
    return static_cast<std::uint32_t>(junctions.size());
}
}
