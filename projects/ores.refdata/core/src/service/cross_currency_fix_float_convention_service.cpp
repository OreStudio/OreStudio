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
#include "ores.refdata.core/service/cross_currency_fix_float_convention_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/convention.hpp"
#include "ores.refdata.api/domain/cross_currency_fix_float_fixed_leg.hpp"
#include "ores.refdata.api/domain/cross_currency_fix_float_float_leg.hpp"
#include "ores.refdata.api/domain/cross_currency_fix_float_swap_terms.hpp"
#include "ores.refdata.api/messaging/cross_currency_fix_float_convention_protocol.hpp"
#include "ores.refdata.core/repository/convention_repository.hpp"
#include "ores.refdata.core/repository/cross_currency_fix_float_fixed_leg_repository.hpp"
#include "ores.refdata.core/repository/cross_currency_fix_float_float_leg_repository.hpp"
#include "ores.refdata.core/repository/cross_currency_fix_float_swap_terms_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <limits>
#include <optional>
#include <ranges>
#include <string>
#include <utility>
#include <vector>

using ores::database::domain::outcome_code;
using ores::database::domain::refuse;
using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

namespace {

/**
 * @brief The current central row a family key names, or an empty vector.
 */
std::vector<domain::convention>
read_convention(repository::convention_repository& repo,
                const ores::database::context& ctx,
                const messaging::cross_currency_fix_float_convention_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

/**
 * @brief The key a central domain row states, so the row read back is
 * addressed exactly as the request addressed it.
 */
messaging::cross_currency_fix_float_convention_key
cross_currency_fix_float_convention_key_from(const domain::convention& v) {
    messaging::cross_currency_fix_float_convention_key key;
    key.id = v.identity.id;
    return key;
}

/**
 * @brief Builds the central domain object a change's write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy, the
 * provenance, the version and the validity window are the service's and the
 * database's to state.
 */
domain::convention
cross_currency_fix_float_convention_to_domain(const messaging::convention_write& write) {
    domain::convention v;
    v.identity.id = write.id;
    v.identity.kind_code = write.kind_code;
    v.payload.name = write.name;
    v.payload.oresmd_uri = write.oresmd_uri;
    return v;
}

/**
 * @brief The live rows of one part kind that belong to one central row.
 */
std::vector<domain::cross_currency_fix_float_fixed_leg>
read_fixed_leg(repository::cross_currency_fix_float_fixed_leg_repository& repo,
               const ores::database::context& ctx,
               const domain::convention& central) {
    return repo.read_latest_by_convention_id(ctx,
                                             boost::uuids::to_string(central.identity.id),
                                             0,
                                             std::numeric_limits<std::uint32_t>::max(),
                                             {},
                                             std::nullopt);
}

/**
 * @brief Builds the part domain object a change's write record states.
 */
domain::cross_currency_fix_float_fixed_leg
to_domain_fixed_leg(const messaging::cross_currency_fix_float_fixed_leg_write& write) {
    domain::cross_currency_fix_float_fixed_leg v;
    v.id = write.id;
    v.convention_id = write.convention_id;
    v.fixed_currency = write.fixed_currency;
    v.fixed_frequency = write.fixed_frequency;
    v.fixed_convention = write.fixed_convention;
    v.fixed_day_count_fraction = write.fixed_day_count_fraction;
    return v;
}

/**
 * @brief The key a part domain row states.
 */
messaging::cross_currency_fix_float_fixed_leg_key
key_from_fixed_leg(const domain::cross_currency_fix_float_fixed_leg& v) {
    messaging::cross_currency_fix_float_fixed_leg_key key;
    key.id = v.id;
    return key;
}

/**
 * @brief The live rows of one part kind that belong to one central row.
 */
std::vector<domain::cross_currency_fix_float_float_leg>
read_float_leg(repository::cross_currency_fix_float_float_leg_repository& repo,
               const ores::database::context& ctx,
               const domain::convention& central) {
    return repo.read_latest_by_convention_id(ctx,
                                             boost::uuids::to_string(central.identity.id),
                                             0,
                                             std::numeric_limits<std::uint32_t>::max(),
                                             {},
                                             std::nullopt);
}

/**
 * @brief Builds the part domain object a change's write record states.
 */
domain::cross_currency_fix_float_float_leg
to_domain_float_leg(const messaging::cross_currency_fix_float_float_leg_write& write) {
    domain::cross_currency_fix_float_float_leg v;
    v.id = write.id;
    v.convention_id = write.convention_id;
    v.index = write.index;
    v.float_index_is_resettable = write.float_index_is_resettable;
    v.include_spread = write.include_spread;
    v.lookback = write.lookback;
    v.fixing_days = write.fixing_days;
    v.rate_cutoff = write.rate_cutoff;
    v.is_averaged = write.is_averaged;
    v.observation_shift = write.observation_shift;
    return v;
}

/**
 * @brief The key a part domain row states.
 */
messaging::cross_currency_fix_float_float_leg_key
key_from_float_leg(const domain::cross_currency_fix_float_float_leg& v) {
    messaging::cross_currency_fix_float_float_leg_key key;
    key.id = v.id;
    return key;
}

/**
 * @brief The live rows of one part kind that belong to one central row.
 */
std::vector<domain::cross_currency_fix_float_swap_terms>
read_swap_terms(repository::cross_currency_fix_float_swap_terms_repository& repo,
                const ores::database::context& ctx,
                const domain::convention& central) {
    return repo.read_latest_by_convention_id(ctx,
                                             boost::uuids::to_string(central.identity.id),
                                             0,
                                             std::numeric_limits<std::uint32_t>::max(),
                                             {},
                                             std::nullopt);
}

/**
 * @brief Builds the part domain object a change's write record states.
 */
domain::cross_currency_fix_float_swap_terms
to_domain_swap_terms(const messaging::cross_currency_fix_float_swap_terms_write& write) {
    domain::cross_currency_fix_float_swap_terms v;
    v.id = write.id;
    v.convention_id = write.convention_id;
    v.settlement_days = write.settlement_days;
    v.settlement_calendar = write.settlement_calendar;
    v.settlement_convention = write.settlement_convention;
    v.eom = write.eom;
    v.is_resettable = write.is_resettable;
    return v;
}

/**
 * @brief The key a part domain row states.
 */
messaging::cross_currency_fix_float_swap_terms_key
key_from_swap_terms(const domain::cross_currency_fix_float_swap_terms& v) {
    messaging::cross_currency_fix_float_swap_terms_key key;
    key.id = v.id;
    return key;
}

}

cross_currency_fix_float_convention_service::cross_currency_fix_float_convention_service(
    context ctx)
    : ctx_(std::move(ctx)) {}

messaging::get_cross_currency_fix_float_convention_response
cross_currency_fix_float_convention_service::get_cross_currency_fix_float_convention(
    const messaging::get_cross_currency_fix_float_convention_request& request) {
    messaging::get_cross_currency_fix_float_convention_response response;
    auto found = read_convention(central_repo_, ctx_, request.key);
    if (found.empty()) {
        response.result =
            refuse(outcome_code::not_found, {.entity = "cross_currency_fix_float_convention"});
        return response;
    }
    response.item = assemble(ctx_, request.key, found.front());
    return response;
}

messaging::get_many_cross_currency_fix_float_conventions_response
cross_currency_fix_float_convention_service::get_many_cross_currency_fix_float_conventions(
    const messaging::get_many_cross_currency_fix_float_conventions_request& request) {
    messaging::get_many_cross_currency_fix_float_conventions_response response;
    // One request answers the whole set: every central row and every part it
    // owns is read and assembled in this one call.
    response.items.reserve(request.keys.size());
    for (const auto& key : request.keys) {
        auto found = read_convention(central_repo_, ctx_, key);
        if (found.empty())
            continue;
        response.items.push_back(assemble(ctx_, key, found.front()));
    }
    return response;
}

messaging::put_cross_currency_fix_float_convention_response
cross_currency_fix_float_convention_service::put_cross_currency_fix_float_convention(
    const messaging::put_cross_currency_fix_float_convention_request& request) {
    messaging::put_cross_currency_fix_float_convention_response response;
    // The central row and every part land in one transaction, so no reader
    // sees a part disagree with the central version.
    ores::database::repository::unit_of_work uow(ctx_);
    const auto& ctx = uow.ctx();

    domain::convention central;
    response.result = prepare_convention(request.item.change, request.intent, ctx, central);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    central_repo_.write(ctx, central, request.item.change.precondition);

    // The store owns the key and the window, so the key the row was written
    // under is the one the write stated rather than assumed from the request.
    const auto key = cross_currency_fix_float_convention_key_from(central);
    // A part write bumps the central row through the part table's own trigger,
    // so the central row is read back after the parts land and the item carries
    // the version the write produced.
    response.result = reconcile_fixed_leg(ctx, central, request.item.fixed_leg, request.intent);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    response.result = reconcile_float_leg(ctx, central, request.item.float_leg, request.intent);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    response.result = reconcile_swap_terms(ctx, central, request.item.swap_terms, request.intent);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;

    auto stored = read_convention(central_repo_, ctx, key);
    if (stored.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "convention"});
        return response;
    }
    uow.commit();
    response.item = assemble(ctx_, key, stored.front());
    return response;
}

messaging::put_many_cross_currency_fix_float_conventions_response
cross_currency_fix_float_convention_service::put_many_cross_currency_fix_float_conventions(
    const messaging::put_many_cross_currency_fix_float_conventions_request& request) {
    messaging::put_many_cross_currency_fix_float_conventions_response response;
    // One transaction for the whole batch. Each element states its own claim,
    // and the store checks each row's claim as it is written; an element that
    // is refused leaves the transaction uncommitted and nothing lands.
    ores::database::repository::unit_of_work uow(ctx_);
    const auto& ctx = uow.ctx();

    response.items.reserve(request.items.size());
    for (const auto& write : request.items) {
        domain::convention central;
        response.result = prepare_convention(write.change, request.intent, ctx, central);
        if (response.result.outcome != ores::utility::domain::outcome::ok)
            return response;
        central_repo_.write(ctx, central, write.change.precondition);

        const auto key = cross_currency_fix_float_convention_key_from(central);
        // A part write bumps the central row, so it is read back after them.
        response.result = reconcile_fixed_leg(ctx, central, write.fixed_leg, request.intent);
        if (response.result.outcome != ores::utility::domain::outcome::ok)
            return response;
        response.result = reconcile_float_leg(ctx, central, write.float_leg, request.intent);
        if (response.result.outcome != ores::utility::domain::outcome::ok)
            return response;
        response.result = reconcile_swap_terms(ctx, central, write.swap_terms, request.intent);
        if (response.result.outcome != ores::utility::domain::outcome::ok)
            return response;
        auto stored = read_convention(central_repo_, ctx, key);
        if (stored.empty()) {
            response.result = refuse(outcome_code::not_found, {.entity = "convention"});
            return response;
        }
        response.items.push_back(assemble(ctx_, key, stored.front()));
    }

    uow.commit();
    return response;
}

messaging::cross_currency_fix_float_convention_item
cross_currency_fix_float_convention_service::assemble(
    const context& ctx,
    const messaging::cross_currency_fix_float_convention_key& key,
    const std::optional<domain::convention>& central) {
    messaging::cross_currency_fix_float_convention_item item;
    item.key = key;
    item.convention = central;
    if (central)
        item.fixed_leg = read_fixed_leg(fixed_leg_repo_, ctx, *central);
    if (central)
        item.float_leg = read_float_leg(float_leg_repo_, ctx, *central);
    if (central)
        item.swap_terms = read_swap_terms(swap_terms_repo_, ctx, *central);
    return item;
}

ores::utility::domain::result cross_currency_fix_float_convention_service::prepare_convention(
    const messaging::convention_change& change,
    const ores::utility::domain::change_intent& intent,
    const context& ctx,
    domain::convention& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = cross_currency_fix_float_convention_to_domain(change.write);
    const auto current =
        read_convention(central_repo_, ctx, cross_currency_fix_float_convention_key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists, {.entity = "convention"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "convention"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().identity.version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "convention",
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
    stamp(out,
          ctx,
          intent.reason_code.empty() ?
              std::string(ores::service::messaging::change_reasons::new_record) :
              intent.reason_code);
    out.audit.change_commentary = intent.commentary;
    return result;
}

ores::utility::domain::result cross_currency_fix_float_convention_service::prepare_fixed_leg(
    const messaging::cross_currency_fix_float_fixed_leg_change& change,
    const ores::utility::domain::change_intent& intent,
    const context& ctx,
    domain::cross_currency_fix_float_fixed_leg& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain_fixed_leg(change.write);
    const auto key = key_from_fixed_leg(out);
    const auto current = fixed_leg_repo_.read_latest(ctx, boost::uuids::to_string(key.id));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "cross_currency_fix_float_fixed_leg"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found,
                              {.entity = "cross_currency_fix_float_fixed_leg"});
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "cross_currency_fix_float_fixed_leg",
                               .expected = change.precondition.version ?
                                               std::to_string(*change.precondition.version) :
                                               std::string{},
                               .current = std::to_string(current.front().version)});
            }
            break;
        case precondition_kind::any:
            break;
    }
    stamp(out,
          ctx,
          intent.reason_code.empty() ?
              std::string(ores::service::messaging::change_reasons::new_record) :
              intent.reason_code);
    out.change_commentary = intent.commentary;
    return result;
}

ores::utility::domain::result cross_currency_fix_float_convention_service::reconcile_fixed_leg(
    const context& ctx,
    const domain::convention& central,
    const std::vector<messaging::cross_currency_fix_float_fixed_leg_change>& requested,
    const ores::utility::domain::change_intent& intent) {
    using ores::utility::domain::outcome;
    // Every requested row is checked before any of them is written, so a
    // refused row refuses the family element rather than landing half of it.
    std::vector<domain::cross_currency_fix_float_fixed_leg> wanted;
    wanted.reserve(requested.size());
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(requested.size());
    for (const auto& change : requested) {
        domain::cross_currency_fix_float_fixed_leg row;
        const auto result = prepare_fixed_leg(change, intent, ctx, row);
        if (result.outcome != outcome::ok)
            return result;
        row.convention_id = central.identity.id;
        // A part that carries the owning party takes it from the central row,
        // which is the row the session stamped. The part table's own policy
        // reads that column, so it must agree with the central row even when
        // the session has no party of its own.
        row.party_id = central.identity.party_id;
        wanted.push_back(std::move(row));
        claims.push_back(change.precondition);
    }
    // A row that is live now but absent from the request is closed, so this
    // part's rows for the central row become exactly the rows the request
    // carries. The close bumps the central version through the part table's
    // own trigger, so the C++ never touches it.
    const auto live = read_fixed_leg(fixed_leg_repo_, ctx, central);
    for (const auto& row : live) {
        const bool present =
            std::ranges::any_of(wanted, [&](const auto& w) { return w.id == row.id; });
        if (!present)
            fixed_leg_repo_.remove(ctx, boost::uuids::to_string(row.id));
    }
    // One statement, so the requested set for this part lands together. The
    // store checks each row's claim inside it.
    if (!wanted.empty())
        fixed_leg_repo_.write(ctx, wanted, claims);
    return {};
}

ores::utility::domain::result cross_currency_fix_float_convention_service::prepare_float_leg(
    const messaging::cross_currency_fix_float_float_leg_change& change,
    const ores::utility::domain::change_intent& intent,
    const context& ctx,
    domain::cross_currency_fix_float_float_leg& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain_float_leg(change.write);
    const auto key = key_from_float_leg(out);
    const auto current = float_leg_repo_.read_latest(ctx, boost::uuids::to_string(key.id));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "cross_currency_fix_float_float_leg"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found,
                              {.entity = "cross_currency_fix_float_float_leg"});
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "cross_currency_fix_float_float_leg",
                               .expected = change.precondition.version ?
                                               std::to_string(*change.precondition.version) :
                                               std::string{},
                               .current = std::to_string(current.front().version)});
            }
            break;
        case precondition_kind::any:
            break;
    }
    stamp(out,
          ctx,
          intent.reason_code.empty() ?
              std::string(ores::service::messaging::change_reasons::new_record) :
              intent.reason_code);
    out.change_commentary = intent.commentary;
    return result;
}

ores::utility::domain::result cross_currency_fix_float_convention_service::reconcile_float_leg(
    const context& ctx,
    const domain::convention& central,
    const std::vector<messaging::cross_currency_fix_float_float_leg_change>& requested,
    const ores::utility::domain::change_intent& intent) {
    using ores::utility::domain::outcome;
    // Every requested row is checked before any of them is written, so a
    // refused row refuses the family element rather than landing half of it.
    std::vector<domain::cross_currency_fix_float_float_leg> wanted;
    wanted.reserve(requested.size());
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(requested.size());
    for (const auto& change : requested) {
        domain::cross_currency_fix_float_float_leg row;
        const auto result = prepare_float_leg(change, intent, ctx, row);
        if (result.outcome != outcome::ok)
            return result;
        row.convention_id = central.identity.id;
        // A part that carries the owning party takes it from the central row,
        // which is the row the session stamped. The part table's own policy
        // reads that column, so it must agree with the central row even when
        // the session has no party of its own.
        row.party_id = central.identity.party_id;
        wanted.push_back(std::move(row));
        claims.push_back(change.precondition);
    }
    // A row that is live now but absent from the request is closed, so this
    // part's rows for the central row become exactly the rows the request
    // carries. The close bumps the central version through the part table's
    // own trigger, so the C++ never touches it.
    const auto live = read_float_leg(float_leg_repo_, ctx, central);
    for (const auto& row : live) {
        const bool present =
            std::ranges::any_of(wanted, [&](const auto& w) { return w.id == row.id; });
        if (!present)
            float_leg_repo_.remove(ctx, boost::uuids::to_string(row.id));
    }
    // One statement, so the requested set for this part lands together. The
    // store checks each row's claim inside it.
    if (!wanted.empty())
        float_leg_repo_.write(ctx, wanted, claims);
    return {};
}

ores::utility::domain::result cross_currency_fix_float_convention_service::prepare_swap_terms(
    const messaging::cross_currency_fix_float_swap_terms_change& change,
    const ores::utility::domain::change_intent& intent,
    const context& ctx,
    domain::cross_currency_fix_float_swap_terms& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain_swap_terms(change.write);
    const auto key = key_from_swap_terms(out);
    const auto current = swap_terms_repo_.read_latest(ctx, boost::uuids::to_string(key.id));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "cross_currency_fix_float_swap_terms"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found,
                              {.entity = "cross_currency_fix_float_swap_terms"});
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "cross_currency_fix_float_swap_terms",
                               .expected = change.precondition.version ?
                                               std::to_string(*change.precondition.version) :
                                               std::string{},
                               .current = std::to_string(current.front().version)});
            }
            break;
        case precondition_kind::any:
            break;
    }
    stamp(out,
          ctx,
          intent.reason_code.empty() ?
              std::string(ores::service::messaging::change_reasons::new_record) :
              intent.reason_code);
    out.change_commentary = intent.commentary;
    return result;
}

ores::utility::domain::result cross_currency_fix_float_convention_service::reconcile_swap_terms(
    const context& ctx,
    const domain::convention& central,
    const std::vector<messaging::cross_currency_fix_float_swap_terms_change>& requested,
    const ores::utility::domain::change_intent& intent) {
    using ores::utility::domain::outcome;
    // Every requested row is checked before any of them is written, so a
    // refused row refuses the family element rather than landing half of it.
    std::vector<domain::cross_currency_fix_float_swap_terms> wanted;
    wanted.reserve(requested.size());
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(requested.size());
    for (const auto& change : requested) {
        domain::cross_currency_fix_float_swap_terms row;
        const auto result = prepare_swap_terms(change, intent, ctx, row);
        if (result.outcome != outcome::ok)
            return result;
        row.convention_id = central.identity.id;
        // A part that carries the owning party takes it from the central row,
        // which is the row the session stamped. The part table's own policy
        // reads that column, so it must agree with the central row even when
        // the session has no party of its own.
        row.party_id = central.identity.party_id;
        wanted.push_back(std::move(row));
        claims.push_back(change.precondition);
    }
    // A row that is live now but absent from the request is closed, so this
    // part's rows for the central row become exactly the rows the request
    // carries. The close bumps the central version through the part table's
    // own trigger, so the C++ never touches it.
    const auto live = read_swap_terms(swap_terms_repo_, ctx, central);
    for (const auto& row : live) {
        const bool present =
            std::ranges::any_of(wanted, [&](const auto& w) { return w.id == row.id; });
        if (!present)
            swap_terms_repo_.remove(ctx, boost::uuids::to_string(row.id));
    }
    // One statement, so the requested set for this part lands together. The
    // store checks each row's claim inside it.
    if (!wanted.empty())
        swap_terms_repo_.write(ctx, wanted, claims);
    return {};
}
}
