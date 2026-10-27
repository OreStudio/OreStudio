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
#include "ores.refdata.core/service/tenor_basis_swap_long_short_service.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_long_short.hpp"
#include "ores.refdata.api/messaging/tenor_basis_swap_long_short_protocol.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_repository.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <chrono>
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

using ores::service::messaging::of_acting_party;
using ores::service::messaging::stamp;
// Every refusal names its outcome; the catalogue supplies the code and the
// sentence, so a service states what happened and nothing about how to say it.
using ores::database::domain::outcome_args;
using ores::database::domain::outcome_code;
using ores::database::domain::refuse;

namespace ores::refdata::service {

using namespace ores::logging;

tenor_basis_swap_long_short_service::tenor_basis_swap_long_short_service(context ctx)
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
std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_read_one(repository::tenor_basis_swap_long_short_repository& repo,
                                     const ores::database::context& ctx,
                                     const messaging::tenor_basis_swap_long_short_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::tenor_basis_swap_long_short_key
tenor_basis_swap_long_short_key_from(const domain::tenor_basis_swap_long_short& v) {
    messaging::tenor_basis_swap_long_short_key key;
    key.id = v.identity.id;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::tenor_basis_swap_long_short
tenor_basis_swap_long_short_to_domain(const messaging::tenor_basis_swap_long_short_write& write) {
    domain::tenor_basis_swap_long_short v;
    v.identity.id = write.id;
    v.identity.convention_id = write.convention_id;
    v.long_leg.long_index = write.long_index;
    v.long_leg.long_pay_tenor = write.long_pay_tenor;
    v.short_leg.short_index = write.short_index;
    v.short_leg.short_pay_tenor = write.short_pay_tenor;
    v.short_leg.spread_on_short = write.spread_on_short;
    v.coupon.include_spread = write.include_spread;
    v.coupon.sub_periods_coupon_type = write.sub_periods_coupon_type;
    return v;
}


}

messaging::list_tenor_basis_swap_long_shorts_response
tenor_basis_swap_long_short_service::list_tenor_basis_swap_long_shorts(
    const messaging::list_tenor_basis_swap_long_shorts_request& request) {
    messaging::list_tenor_basis_swap_long_shorts_response response;
    if (!request.order.field.empty() &&
        !repository::tenor_basis_swap_long_short_repository::is_sortable(request.order.field)) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "tenor basis swap long shorts", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->convention_id_one_of &&
        request.filter->convention_id_one_of->size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "convention_id_one_of", .limit = "1000"});
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
    response.tenor_basis_swap_long_shorts = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_tenor_basis_swap_long_short_count(ctx_, request.filter, as_of);
    return response;
}

messaging::list_by_convention_id_tenor_basis_swap_long_shorts_response
tenor_basis_swap_long_short_service::list_by_convention_id_tenor_basis_swap_long_shorts(
    const messaging::list_by_convention_id_tenor_basis_swap_long_shorts_request& request) {
    messaging::list_by_convention_id_tenor_basis_swap_long_shorts_response response;
    if (!request.order.field.empty() &&
        !repository::tenor_basis_swap_long_short_repository::is_sortable(request.order.field)) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "tenor basis swap long shorts", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->convention_id_one_of &&
        request.filter->convention_id_one_of->size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "convention_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result =
            refuse(outcome_code::scope_not_supported, {.entity = "tenor basis swap long shorts"});
        return response;
    }
    const auto relation = boost::uuids::to_string(request.convention_id);
    response.tenor_basis_swap_long_shorts = repo_.read_latest_by_convention_id(
        ctx_, relation, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_tenor_basis_swap_long_short_count_by_convention_id(
        ctx_, relation, request.filter);
    return response;
}

messaging::get_tenor_basis_swap_long_short_response
tenor_basis_swap_long_short_service::get_tenor_basis_swap_long_short(
    const messaging::get_tenor_basis_swap_long_short_request& request) {
    messaging::get_tenor_basis_swap_long_short_response response;
    auto found = tenor_basis_swap_long_short_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result =
            refuse(outcome_code::not_found, {.entity = "tenor_basis_swap_long_short"});
        return response;
    }
    response.tenor_basis_swap_long_short = std::move(found.front());
    return response;
}

messaging::get_many_tenor_basis_swap_long_shorts_response
tenor_basis_swap_long_short_service::get_many_tenor_basis_swap_long_shorts(
    const messaging::get_many_tenor_basis_swap_long_shorts_request& request) {
    messaging::get_many_tenor_basis_swap_long_shorts_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::tenor_basis_swap_long_short_lookup entry;
        entry.key = k;
        auto found = tenor_basis_swap_long_short_read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.tenor_basis_swap_long_short = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_tenor_basis_swap_long_short_response
tenor_basis_swap_long_short_service::put_tenor_basis_swap_long_short(
    const messaging::put_tenor_basis_swap_long_short_request& request) {
    messaging::put_tenor_basis_swap_long_short_response response;
    domain::tenor_basis_swap_long_short value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = of_acting_party(tenor_basis_swap_long_short_read_one(
                                       repo_, ctx_, tenor_basis_swap_long_short_key_from(value)),
                                   ctx_);
    if (!written.empty())
        response.tenor_basis_swap_long_short = std::move(written.front());
    return response;
}

messaging::put_many_tenor_basis_swap_long_shorts_response
tenor_basis_swap_long_short_service::put_many_tenor_basis_swap_long_shorts(
    const messaging::put_many_tenor_basis_swap_long_shorts_request& request) {
    messaging::put_many_tenor_basis_swap_long_shorts_response response;
    std::vector<domain::tenor_basis_swap_long_short> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::tenor_basis_swap_long_short value;
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
    response.tenor_basis_swap_long_shorts.reserve(batch.size());
    for (const auto& value : batch) {
        auto written =
            of_acting_party(tenor_basis_swap_long_short_read_one(
                                repo_, ctx_, tenor_basis_swap_long_short_key_from(value)),
                            ctx_);
        response.tenor_basis_swap_long_shorts.push_back(
            written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_tenor_basis_swap_long_short_response
tenor_basis_swap_long_short_service::delete_tenor_basis_swap_long_short(
    const messaging::delete_tenor_basis_swap_long_short_request& request) {
    messaging::delete_tenor_basis_swap_long_short_response response;
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
        case repository::tenor_basis_swap_long_short_repository::remove_status::removed:
            break;
        case repository::tenor_basis_swap_long_short_repository::remove_status::missing:
            response.result =
                refuse(outcome_code::not_found, {.entity = "tenor_basis_swap_long_short"});
            break;
        case repository::tenor_basis_swap_long_short_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live =
                tenor_basis_swap_long_short_read_one(repo_, ctx_, request.removal.key);
            response.result =
                refuse(outcome_code::version_conflict,
                       {.entity = "tenor_basis_swap_long_short",
                        .field = "id",
                        .expected = expected ? std::to_string(*expected) : std::string{},
                        .current = live.empty() ? std::string{} :
                                                  std::to_string(live.front().identity.version)});
            break;
        }
        case repository::tenor_basis_swap_long_short_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_tenor_basis_swap_long_shorts_response
tenor_basis_swap_long_short_service::delete_many_tenor_basis_swap_long_shorts(
    const messaging::delete_many_tenor_basis_swap_long_shorts_request& request) {
    messaging::delete_many_tenor_basis_swap_long_shorts_response response;
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

messaging::list_tenor_basis_swap_long_short_versions_response
tenor_basis_swap_long_short_service::list_tenor_basis_swap_long_short_versions(
    const messaging::list_tenor_basis_swap_long_short_versions_request& request) {
    messaging::list_tenor_basis_swap_long_short_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "tenor basis swap long shorts", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "tenor basis swap long shorts"});
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

messaging::get_tenor_basis_swap_long_short_version_response
tenor_basis_swap_long_short_service::get_tenor_basis_swap_long_short_version(
    const messaging::get_tenor_basis_swap_long_short_version_request& request) {
    messaging::get_tenor_basis_swap_long_short_version_response response;
    auto found =
        repo_.read_at_version(ctx_,
                              boost::uuids::to_string(request.key.tenor_basis_swap_long_short.id),
                              request.key.version);
    if (!found) {
        response.result =
            refuse(outcome_code::not_found, {.entity = "tenor_basis_swap_long_short"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result tenor_basis_swap_long_short_service::prepare_change(
    const messaging::tenor_basis_swap_long_short_change& change,
    const ores::utility::domain::change_intent& intent,
    domain::tenor_basis_swap_long_short& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = tenor_basis_swap_long_short_to_domain(change.write);
    // The write lands under the acting party, so its claim is checked against
    // that party's row, not another visible party's row of the same key.
    const auto current =
        of_acting_party(tenor_basis_swap_long_short_read_one(
                            repo_, ctx_, tenor_basis_swap_long_short_key_from(out)),
                        ctx_);
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "tenor_basis_swap_long_short", .field = "id"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "tenor_basis_swap_long_short"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().identity.version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "tenor_basis_swap_long_short",
                               .field = "id",
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


std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_service::list_tenor_basis_swap_long_shorts(std::uint32_t offset,
                                                                       std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all tenor basis swap long shorts";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t tenor_basis_swap_long_short_service::count_tenor_basis_swap_long_shorts() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total tenor basis swap long shorts count";
    return repo_.get_total_tenor_basis_swap_long_short_count(ctx_);
}


std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_service::list_tenor_basis_swap_long_shorts_by_convention_id(
    const std::string& convention_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing tenor basis swap long shorts by convention_id: "
                               << convention_id;
    return repo_.read_latest_by_convention_id(ctx_, convention_id, offset, limit);
}

std::uint32_t
tenor_basis_swap_long_short_service::count_tenor_basis_swap_long_shorts_by_convention_id(
    const std::string& convention_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Getting total tenor basis swap long shorts count by convention_id: " << convention_id;
    return repo_.get_total_tenor_basis_swap_long_short_count_by_convention_id(ctx_, convention_id);
}

std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_service::list_tenor_basis_swap_long_shorts_by_convention_id(
    const boost::uuids::uuid& convention_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing tenor basis swap long shorts by convention_id: "
                               << convention_id;
    return repo_.read_latest_by_convention_id(
        ctx_, boost::uuids::to_string(convention_id), offset, limit);
}

std::uint32_t
tenor_basis_swap_long_short_service::count_tenor_basis_swap_long_shorts_by_convention_id(
    const boost::uuids::uuid& convention_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Getting total tenor basis swap long shorts count by convention_id: " << convention_id;
    return repo_.get_total_tenor_basis_swap_long_short_count_by_convention_id(
        ctx_, boost::uuids::to_string(convention_id));
}


std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_service::list_tenor_basis_swap_long_shorts_by_convention_id_as_of(
    const std::string& convention_id,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug)
        << "Listing tenor basis swap long shorts by convention_id as of window: " << convention_id;
    return repo_.read_by_convention_id_as_of(ctx_, convention_id, valid_from_bound, valid_to_bound);
}

std::optional<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_service::get_tenor_basis_swap_long_short_at_version(
    const boost::uuids::uuid& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting tenor basis swap long short at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(id), version);
}

std::optional<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_service::get_tenor_basis_swap_long_short(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting tenor basis swap long short. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_service::get_tenor_basis_swap_long_shorts(
    const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void tenor_basis_swap_long_short_service::save_tenor_basis_swap_long_short(
    const domain::tenor_basis_swap_long_short& v) {
    if (v.identity.id.is_nil())
        throw std::invalid_argument("Tenor Basis Swap Long Short id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving tenor basis swap long short. " << "id: " << v.identity.id;
    auto t = v;
    stamp(t.identity, ctx_);
    stamp(t.audit, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved tenor basis swap long short. " << "id: " << v.identity.id;
}

void tenor_basis_swap_long_short_service::save_tenor_basis_swap_long_shorts(
    const std::vector<domain::tenor_basis_swap_long_short>& tenor_basis_swap_long_shorts) {
    for (const auto& e : tenor_basis_swap_long_shorts) {
        if (e.identity.id.is_nil())
            throw std::invalid_argument("Tenor Basis Swap Long Short id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << tenor_basis_swap_long_shorts.size()
                               << " tenor basis swap long shorts";
    auto ts = tenor_basis_swap_long_shorts;
    for (auto& e : ts) {
        stamp(e.identity, ctx_);
        stamp(e.audit, ctx_);
    }
    repo_.write(ctx_, ts);
}

void tenor_basis_swap_long_short_service::delete_tenor_basis_swap_long_short(
    const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenor basis swap long short. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed tenor basis swap long short. " << "id: " << id;
}

void tenor_basis_swap_long_short_service::delete_tenor_basis_swap_long_shorts(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_service::get_tenor_basis_swap_long_short_history(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for tenor basis swap long short. "
                               << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
