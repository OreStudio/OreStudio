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
#include "ores.trading.core/service/bond_issue_leg_rate_service.hpp"
#include "ores.trading.api/domain/bond_issue_leg_rate.hpp"
#include "ores.trading.api/messaging/bond_issue_leg_rate_protocol.hpp"
#include "ores.trading.core/repository/bond_issue_leg_rate_repository.hpp"
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

bond_issue_leg_rate_service::bond_issue_leg_rate_service(context ctx)
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
std::vector<domain::bond_issue_leg_rate> read_one(repository::bond_issue_leg_rate_repository& repo,
                                                  const ores::database::context& ctx,
                                                  const messaging::bond_issue_leg_rate_key& key) {
    return repo.read_latest(
        ctx, boost::uuids::to_string(key.issue_id), std::to_string(key.leg_number));
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::bond_issue_leg_rate_key key_from(const domain::bond_issue_leg_rate& v) {
    messaging::bond_issue_leg_rate_key key;
    key.issue_id = v.issue_id;
    key.leg_number = v.leg_number;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::bond_issue_leg_rate to_domain(const messaging::bond_issue_leg_rate_write& write) {
    domain::bond_issue_leg_rate v;
    v.issue_id = write.issue_id;
    v.leg_number = write.leg_number;
    v.rate_kind = write.rate_kind;
    v.index = write.index;
    v.is_in_arrears = write.is_in_arrears;
    v.fixing_days = write.fixing_days;
    v.fixing_calendar = write.fixing_calendar;
    v.last_recent_period = write.last_recent_period;
    v.last_recent_period_calendar = write.last_recent_period_calendar;
    v.lookback = write.lookback;
    v.rate_cutoff = write.rate_cutoff;
    v.is_averaged = write.is_averaged;
    v.has_sub_periods = write.has_sub_periods;
    v.include_spread = write.include_spread;
    v.is_not_resetting_xccy = write.is_not_resetting_xccy;
    v.naked_option = write.naked_option;
    v.local_cap_floor = write.local_cap_floor;
    v.stub_use_original_curve = write.stub_use_original_curve;
    v.observation_shift = write.observation_shift;
    v.front_stub_short_index = write.front_stub_short_index;
    v.front_stub_long_index = write.front_stub_long_index;
    v.front_stub_rounding_type = write.front_stub_rounding_type;
    v.front_stub_rounding_precision = write.front_stub_rounding_precision;
    v.back_stub_short_index = write.back_stub_short_index;
    v.back_stub_long_index = write.back_stub_long_index;
    v.back_stub_rounding_type = write.back_stub_rounding_type;
    v.back_stub_rounding_precision = write.back_stub_rounding_precision;
    return v;
}

}

messaging::list_bond_issue_leg_rates_response
bond_issue_leg_rate_service::list_bond_issue_leg_rates(
    const messaging::list_bond_issue_leg_rates_request& request) {
    messaging::list_bond_issue_leg_rates_response response;
    if (!request.order.field.empty() &&
        !repository::bond_issue_leg_rate_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "bond issue leg rates", .field = request.order.field});
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
    response.bond_issue_leg_rates =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, as_of);
    response.total = repo_.get_total_bond_issue_leg_rate_count(ctx_, as_of);
    return response;
}

messaging::get_bond_issue_leg_rate_response bond_issue_leg_rate_service::get_bond_issue_leg_rate(
    const messaging::get_bond_issue_leg_rate_request& request) {
    messaging::get_bond_issue_leg_rate_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "bond_issue_leg_rate"});
        return response;
    }
    response.bond_issue_leg_rate = std::move(found.front());
    return response;
}

messaging::get_many_bond_issue_leg_rates_response
bond_issue_leg_rate_service::get_many_bond_issue_leg_rates(
    const messaging::get_many_bond_issue_leg_rates_request& request) {
    messaging::get_many_bond_issue_leg_rates_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::bond_issue_leg_rate_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.bond_issue_leg_rate = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_bond_issue_leg_rate_response bond_issue_leg_rate_service::put_bond_issue_leg_rate(
    const messaging::put_bond_issue_leg_rate_request& request) {
    messaging::put_bond_issue_leg_rate_response response;
    domain::bond_issue_leg_rate value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.bond_issue_leg_rate = std::move(written.front());
    return response;
}

messaging::put_many_bond_issue_leg_rates_response
bond_issue_leg_rate_service::put_many_bond_issue_leg_rates(
    const messaging::put_many_bond_issue_leg_rates_request& request) {
    messaging::put_many_bond_issue_leg_rates_response response;
    std::vector<domain::bond_issue_leg_rate> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::bond_issue_leg_rate value;
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
    response.bond_issue_leg_rates.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.bond_issue_leg_rates.push_back(written.empty() ? value :
                                                                  std::move(written.front()));
    }
    return response;
}

messaging::delete_bond_issue_leg_rate_response
bond_issue_leg_rate_service::delete_bond_issue_leg_rate(
    const messaging::delete_bond_issue_leg_rate_request& request) {
    messaging::delete_bond_issue_leg_rate_response response;
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
                         boost::uuids::to_string(request.removal.key.issue_id),
                         std::to_string(request.removal.key.leg_number),
                         expected)) {
        case repository::bond_issue_leg_rate_repository::remove_status::removed:
            break;
        case repository::bond_issue_leg_rate_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "bond_issue_leg_rate"});
            break;
        case repository::bond_issue_leg_rate_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "bond_issue_leg_rate",
                 .field = "issue_id",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::bond_issue_leg_rate_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_bond_issue_leg_rates_response
bond_issue_leg_rate_service::delete_many_bond_issue_leg_rates(
    const messaging::delete_many_bond_issue_leg_rates_request& request) {
    messaging::delete_many_bond_issue_leg_rates_response response;
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
    std::vector<std::string> issue_id_keys;
    issue_id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        issue_id_keys.push_back(boost::uuids::to_string(removal.key.issue_id));
    std::vector<std::string> leg_number_keys;
    leg_number_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        leg_number_keys.push_back(std::to_string(removal.key.leg_number));
    repo_.remove(ctx_, issue_id_keys, leg_number_keys);
    return response;
}

messaging::list_bond_issue_leg_rate_versions_response
bond_issue_leg_rate_service::list_bond_issue_leg_rate_versions(
    const messaging::list_bond_issue_leg_rate_versions_request& request) {
    messaging::list_bond_issue_leg_rate_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "bond issue leg rates", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "bond issue leg rates"});
        return response;
    }
    auto all = repo_.read_all(ctx_,
                              boost::uuids::to_string(request.key.issue_id),
                              std::to_string(request.key.leg_number));
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

messaging::get_bond_issue_leg_rate_version_response
bond_issue_leg_rate_service::get_bond_issue_leg_rate_version(
    const messaging::get_bond_issue_leg_rate_version_request& request) {
    messaging::get_bond_issue_leg_rate_version_response response;
    auto found =
        repo_.read_at_version(ctx_,
                              boost::uuids::to_string(request.key.bond_issue_leg_rate.issue_id),
                              std::to_string(request.key.bond_issue_leg_rate.leg_number),
                              request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "bond_issue_leg_rate"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
bond_issue_leg_rate_service::prepare_change(const messaging::bond_issue_leg_rate_change& change,
                                            const ores::utility::domain::change_intent& intent,
                                            domain::bond_issue_leg_rate& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain(change.write);
    const auto current = read_one(repo_, ctx_, key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "bond_issue_leg_rate", .field = "issue_id"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "bond_issue_leg_rate"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "bond_issue_leg_rate",
                               .field = "issue_id",
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


std::vector<domain::bond_issue_leg_rate>
bond_issue_leg_rate_service::list_bond_issue_leg_rates(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all bond issue leg rates";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t bond_issue_leg_rate_service::count_bond_issue_leg_rates() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total bond issue leg rates count";
    return repo_.get_total_bond_issue_leg_rate_count(ctx_);
}


std::optional<domain::bond_issue_leg_rate>
bond_issue_leg_rate_service::get_bond_issue_leg_rate_at_version(const std::string& issue_id,
                                                                const std::string& leg_number,
                                                                std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting bond issue leg rate at version. "
                               << "issue_id: " << issue_id << " leg_number: " << leg_number
                               << " version: " << version;
    return repo_.read_at_version(ctx_, issue_id, leg_number, version);
}

std::optional<domain::bond_issue_leg_rate>
bond_issue_leg_rate_service::get_bond_issue_leg_rate(const std::string& issue_id,
                                                     const std::string& leg_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting bond issue leg rate. " << "issue_id: " << issue_id
                               << " leg_number: " << leg_number;
    auto results = repo_.read_latest(ctx_, issue_id, leg_number);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::bond_issue_leg_rate>
bond_issue_leg_rate_service::get_bond_issue_leg_rates(const std::vector<std::string>& issue_ids,
                                                      const std::vector<std::string>& leg_numbers) {
    return repo_.read_latest(ctx_, issue_ids, leg_numbers);
}

void bond_issue_leg_rate_service::save_bond_issue_leg_rate(const domain::bond_issue_leg_rate& v) {
    if (v.issue_id.is_nil())
        throw std::invalid_argument("Bond Issue Leg Rate issue_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving bond issue leg rate. " << "issue_id: " << v.issue_id
                               << " leg_number: " << v.leg_number;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved bond issue leg rate. " << "issue_id: " << v.issue_id
                              << " leg_number: " << v.leg_number;
}

void bond_issue_leg_rate_service::save_bond_issue_leg_rates(
    const std::vector<domain::bond_issue_leg_rate>& bond_issue_leg_rates) {
    for (const auto& e : bond_issue_leg_rates) {
        if (e.issue_id.is_nil())
            throw std::invalid_argument("Bond Issue Leg Rate issue_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << bond_issue_leg_rates.size()
                               << " bond issue leg rates";
    auto ts = bond_issue_leg_rates;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void bond_issue_leg_rate_service::delete_bond_issue_leg_rate(const std::string& issue_id,
                                                             const std::string& leg_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing bond issue leg rate. " << "issue_id: " << issue_id
                               << " leg_number: " << leg_number;
    repo_.remove(ctx_, issue_id, leg_number);
    BOOST_LOG_SEV(lg(), info) << "Removed bond issue leg rate. " << "issue_id: " << issue_id
                              << " leg_number: " << leg_number;
}

void bond_issue_leg_rate_service::delete_bond_issue_leg_rates(
    const std::vector<std::string>& issue_ids, const std::vector<std::string>& leg_numbers) {
    repo_.remove(ctx_, issue_ids, leg_numbers);
}

std::vector<domain::bond_issue_leg_rate>
bond_issue_leg_rate_service::get_bond_issue_leg_rate_history(const std::string& issue_id,
                                                             const std::string& leg_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for bond issue leg rate. "
                               << "issue_id: " << issue_id << " leg_number: " << leg_number;
    return repo_.read_all(ctx_, issue_id, leg_number);
}

}
