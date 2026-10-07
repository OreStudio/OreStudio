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
#include "ores.marketdata.core/service/market_fixing_service.hpp"
#include "ores.marketdata.api/domain/market_fixing.hpp"
#include "ores.marketdata.api/messaging/market_fixing_protocol.hpp"
#include "ores.marketdata.core/repository/market_fixing_repository.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <cstdint>
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

namespace ores::marketdata::service {

using namespace ores::logging;

market_fixing_service::market_fixing_service(context ctx)
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
std::vector<domain::market_fixing> read_one(repository::market_fixing_repository& repo,
                                            const ores::database::context& ctx,
                                            const messaging::market_fixing_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::market_fixing_key key_from(const domain::market_fixing& v) {
    messaging::market_fixing_key key;
    key.id = v.id;
    return key;
}

/**
 * @brief The record a refusal names, so a message says which row it refused.
 *
 * The key's fields are joined rather than rendered one at a time, because a
 * key of several columns has no single member a message could name.
 */
std::string describe_key(const domain::market_fixing& v) {
    std::ostringstream text;
    bool first = true;
    const auto append = [&text, &first](const char* field, const auto& value) {
        if (!first)
            text << ", ";
        first = false;
        text << field << "=" << value;
    };
    append("id", v.id);
    return text.str();
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::market_fixing to_domain(const messaging::market_fixing_write& write) {
    domain::market_fixing v;
    v.id = write.id;
    v.party_id = write.party_id;
    v.series_id = write.series_id;
    v.fixing_date = write.fixing_date;
    v.value = write.value;
    v.source = write.source;
    return v;
}

}

messaging::list_market_fixings_response
market_fixing_service::list_market_fixings(const messaging::list_market_fixings_request& request) {
    messaging::list_market_fixings_response response;
    if (!request.order.field.empty() &&
        !repository::market_fixing_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of market fixings cannot be ordered by " + request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in id_one_of.";
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
    response.market_fixings = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_market_fixing_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_market_fixing_response
market_fixing_service::get_market_fixing(const messaging::get_market_fixing_request& request) {
    messaging::get_market_fixing_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.market_fixing = std::move(found.front());
    return response;
}

messaging::get_many_market_fixings_response market_fixing_service::get_many_market_fixings(
    const messaging::get_many_market_fixings_request& request) {
    messaging::get_many_market_fixings_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::market_fixing_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.market_fixing = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_market_fixing_response
market_fixing_service::put_market_fixing(const messaging::put_market_fixing_request& request) {
    messaging::put_market_fixing_response response;
    domain::market_fixing value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.market_fixing = std::move(written.front());
    return response;
}

messaging::put_many_market_fixings_response market_fixing_service::put_many_market_fixings(
    const messaging::put_many_market_fixings_request& request) {
    messaging::put_many_market_fixings_response response;
    std::vector<domain::market_fixing> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::market_fixing value;
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
    response.market_fixings.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.market_fixings.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_market_fixing_response market_fixing_service::delete_market_fixing(
    const messaging::delete_market_fixing_request& request) {
    messaging::delete_market_fixing_response response;
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
    switch (repo_.remove(ctx_, boost::uuids::to_string(request.removal.key.id), expected)) {
        case repository::market_fixing_repository::remove_status::removed:
            break;
        case repository::market_fixing_repository::remove_status::missing:
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            break;
        case repository::market_fixing_repository::remove_status::conflicting:
            response.result.outcome = outcome::conflict;
            response.result.code = "version_conflict";
            break;
        case repository::market_fixing_repository::remove_status::unsupported:
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_not_supported";
            response.result.message = "This resource keeps no version to match.";
            break;
    }
    return response;
}

messaging::delete_many_market_fixings_response market_fixing_service::delete_many_market_fixings(
    const messaging::delete_many_market_fixings_request& request) {
    messaging::delete_many_market_fixings_response response;
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
        id_keys.push_back(boost::uuids::to_string(removal.key.id));
    repo_.remove(ctx_, id_keys);
    return response;
}

ores::utility::domain::result
market_fixing_service::prepare_change(const messaging::market_fixing_change& change,
                                      const ores::utility::domain::change_intent& intent,
                                      domain::market_fixing& out) {
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
                    "A market_fixing with " + describe_key(out) +
                    " already exists. State the version you read to replace it, or ask "
                    "for a version replace.";
                result.fields.push_back({"id", "already_exists", "This value is already taken."});
                return result;
            }
            break;
        case precondition_kind::must_match_version:
            result.outcome = outcome::invalid;
            result.code = "precondition_not_supported";
            result.message = "This resource keeps no version to match.";
            return result;
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
    return result;
}


std::vector<domain::market_fixing> market_fixing_service::list_market_fixings(std::uint32_t offset,
                                                                              std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all market fixings";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t market_fixing_service::count_market_fixings() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total market fixings count";
    return repo_.get_total_market_fixing_count(ctx_);
}


std::optional<domain::market_fixing>
market_fixing_service::get_market_fixing(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting market fixing. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::market_fixing>
market_fixing_service::get_market_fixings(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void market_fixing_service::save_market_fixing(const domain::market_fixing& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Market Fixing id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving market fixing. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved market fixing. " << "id: " << v.id;
}

void market_fixing_service::save_market_fixings(
    const std::vector<domain::market_fixing>& market_fixings) {
    for (const auto& e : market_fixings) {
        if (e.id.is_nil())
            throw std::invalid_argument("Market Fixing id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << market_fixings.size() << " market fixings";
    auto ts = market_fixings;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void market_fixing_service::delete_market_fixing(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing market fixing. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed market fixing. " << "id: " << id;
}

void market_fixing_service::delete_market_fixings(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::market_fixing>
market_fixing_service::get_market_fixing_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for market fixing. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
