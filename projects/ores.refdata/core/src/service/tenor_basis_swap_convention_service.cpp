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
#include "ores.refdata.core/service/tenor_basis_swap_convention_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/convention.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_long_short.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive.hpp"
#include "ores.refdata.api/messaging/tenor_basis_swap_convention_protocol.hpp"
#include "ores.refdata.core/repository/convention_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_pay_receive_repository.hpp"
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
                const messaging::tenor_basis_swap_convention_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

/**
 * @brief The key a central domain row states, so the row read back is
 * addressed exactly as the request addressed it.
 */
messaging::tenor_basis_swap_convention_key key_from(const domain::convention& v) {
    messaging::tenor_basis_swap_convention_key key;
    key.id = v.id;
    return key;
}

/**
 * @brief Builds the central domain object a change's write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy, the
 * provenance, the version and the validity window are the service's and the
 * database's to state.
 */
domain::convention to_domain(const messaging::convention_write& write) {
    domain::convention v;
    v.id = write.id;
    v.kind_code = write.kind_code;
    v.ores_id = write.ores_id;
    v.oresmd_uri = write.oresmd_uri;
    return v;
}

/**
 * @brief The live rows of one part kind that belong to one central row.
 */
std::vector<domain::tenor_basis_swap_pay_receive>
read_pay_receive(repository::tenor_basis_swap_pay_receive_repository& repo,
                 const ores::database::context& ctx,
                 const domain::convention& central) {
    return repo.read_latest_by_convention_id(ctx,
                                             boost::uuids::to_string(central.id),
                                             0,
                                             std::numeric_limits<std::uint32_t>::max(),
                                             {},
                                             std::nullopt);
}

/**
 * @brief Builds the part domain object a change's write record states.
 */
domain::tenor_basis_swap_pay_receive
to_domain_pay_receive(const messaging::tenor_basis_swap_pay_receive_write& write) {
    domain::tenor_basis_swap_pay_receive v;
    v.id = write.id;
    v.convention_id = write.convention_id;
    v.pay_index = write.pay_index;
    v.pay_frequency = write.pay_frequency;
    v.receive_index = write.receive_index;
    v.receive_frequency = write.receive_frequency;
    v.spread_on_rec = write.spread_on_rec;
    v.include_spread = write.include_spread;
    v.sub_periods_coupon_type = write.sub_periods_coupon_type;
    v.pay_is_averaged = write.pay_is_averaged;
    v.rec_is_averaged = write.rec_is_averaged;
    return v;
}

/**
 * @brief The key a part domain row states.
 */
messaging::tenor_basis_swap_pay_receive_key
key_from_pay_receive(const domain::tenor_basis_swap_pay_receive& v) {
    messaging::tenor_basis_swap_pay_receive_key key;
    key.id = v.id;
    return key;
}

/**
 * @brief The live rows of one part kind that belong to one central row.
 */
std::vector<domain::tenor_basis_swap_long_short>
read_long_short(repository::tenor_basis_swap_long_short_repository& repo,
                const ores::database::context& ctx,
                const domain::convention& central) {
    return repo.read_latest_by_convention_id(ctx,
                                             boost::uuids::to_string(central.id),
                                             0,
                                             std::numeric_limits<std::uint32_t>::max(),
                                             {},
                                             std::nullopt);
}

/**
 * @brief Builds the part domain object a change's write record states.
 */
domain::tenor_basis_swap_long_short
to_domain_long_short(const messaging::tenor_basis_swap_long_short_write& write) {
    domain::tenor_basis_swap_long_short v;
    v.id = write.id;
    v.convention_id = write.convention_id;
    v.long_index = write.long_index;
    v.long_pay_tenor = write.long_pay_tenor;
    v.short_index = write.short_index;
    v.short_pay_tenor = write.short_pay_tenor;
    v.spread_on_short = write.spread_on_short;
    return v;
}

/**
 * @brief The key a part domain row states.
 */
messaging::tenor_basis_swap_long_short_key
key_from_long_short(const domain::tenor_basis_swap_long_short& v) {
    messaging::tenor_basis_swap_long_short_key key;
    key.id = v.id;
    return key;
}

}

tenor_basis_swap_convention_service::tenor_basis_swap_convention_service(context ctx)
    : ctx_(std::move(ctx)) {}

messaging::get_tenor_basis_swap_convention_response
tenor_basis_swap_convention_service::get_tenor_basis_swap_convention(
    const messaging::get_tenor_basis_swap_convention_request& request) {
    messaging::get_tenor_basis_swap_convention_response response;
    auto found = read_convention(central_repo_, ctx_, request.key);
    if (found.empty()) {
        response.result =
            refuse(outcome_code::not_found, {.entity = "tenor_basis_swap_convention"});
        return response;
    }
    response.item = assemble(ctx_, request.key, found.front());
    return response;
}

messaging::get_many_tenor_basis_swap_conventions_response
tenor_basis_swap_convention_service::get_many_tenor_basis_swap_conventions(
    const messaging::get_many_tenor_basis_swap_conventions_request& request) {
    messaging::get_many_tenor_basis_swap_conventions_response response;
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

messaging::put_tenor_basis_swap_convention_response
tenor_basis_swap_convention_service::put_tenor_basis_swap_convention(
    const messaging::put_tenor_basis_swap_convention_request& request) {
    messaging::put_tenor_basis_swap_convention_response response;
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
    const auto key = key_from(central);
    // A part write bumps the central row through the part table's own trigger,
    // so the central row is read back after the parts land and the item carries
    // the version the write produced.
    response.result = reconcile_pay_receive(ctx, central, request.item.pay_receive, request.intent);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    response.result = reconcile_long_short(ctx, central, request.item.long_short, request.intent);
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

messaging::put_many_tenor_basis_swap_conventions_response
tenor_basis_swap_convention_service::put_many_tenor_basis_swap_conventions(
    const messaging::put_many_tenor_basis_swap_conventions_request& request) {
    messaging::put_many_tenor_basis_swap_conventions_response response;
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

        const auto key = key_from(central);
        // A part write bumps the central row, so it is read back after them.
        response.result = reconcile_pay_receive(ctx, central, write.pay_receive, request.intent);
        if (response.result.outcome != ores::utility::domain::outcome::ok)
            return response;
        response.result = reconcile_long_short(ctx, central, write.long_short, request.intent);
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

messaging::tenor_basis_swap_convention_item
tenor_basis_swap_convention_service::assemble(const context& ctx,
                                              const messaging::tenor_basis_swap_convention_key& key,
                                              const std::optional<domain::convention>& central) {
    messaging::tenor_basis_swap_convention_item item;
    item.key = key;
    item.convention = central;
    if (central)
        item.pay_receive = read_pay_receive(pay_receive_repo_, ctx, *central);
    if (central)
        item.long_short = read_long_short(long_short_repo_, ctx, *central);
    return item;
}

ores::utility::domain::result tenor_basis_swap_convention_service::prepare_convention(
    const messaging::convention_change& change,
    const ores::utility::domain::change_intent& intent,
    const context& ctx,
    domain::convention& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain(change.write);
    const auto current = read_convention(central_repo_, ctx, key_from(out));
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
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "convention",
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
          ctx,
          intent.reason_code.empty() ?
              std::string(ores::service::messaging::change_reasons::new_record) :
              intent.reason_code);
    out.change_commentary = intent.commentary;
    return result;
}

ores::utility::domain::result tenor_basis_swap_convention_service::prepare_pay_receive(
    const messaging::tenor_basis_swap_pay_receive_change& change,
    const ores::utility::domain::change_intent& intent,
    const context& ctx,
    domain::tenor_basis_swap_pay_receive& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain_pay_receive(change.write);
    const auto key = key_from_pay_receive(out);
    const auto current = pay_receive_repo_.read_latest(ctx, boost::uuids::to_string(key.id));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "tenor_basis_swap_pay_receive"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "tenor_basis_swap_pay_receive"});
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "tenor_basis_swap_pay_receive",
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

ores::utility::domain::result tenor_basis_swap_convention_service::reconcile_pay_receive(
    const context& ctx,
    const domain::convention& central,
    const std::vector<messaging::tenor_basis_swap_pay_receive_change>& requested,
    const ores::utility::domain::change_intent& intent) {
    using ores::utility::domain::outcome;
    // Every requested row is checked before any of them is written, so a
    // refused row refuses the family element rather than landing half of it.
    std::vector<domain::tenor_basis_swap_pay_receive> wanted;
    wanted.reserve(requested.size());
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(requested.size());
    for (const auto& change : requested) {
        domain::tenor_basis_swap_pay_receive row;
        const auto result = prepare_pay_receive(change, intent, ctx, row);
        if (result.outcome != outcome::ok)
            return result;
        row.convention_id = central.id;
        // A part that carries the owning party takes it from the central row,
        // which is the row the session stamped. The part table's own policy
        // reads that column, so it must agree with the central row even when
        // the session has no party of its own.
        row.party_id = central.party_id;
        wanted.push_back(std::move(row));
        claims.push_back(change.precondition);
    }
    // A row that is live now but absent from the request is closed, so this
    // part's rows for the central row become exactly the rows the request
    // carries. The close bumps the central version through the part table's
    // own trigger, so the C++ never touches it.
    const auto live = read_pay_receive(pay_receive_repo_, ctx, central);
    for (const auto& row : live) {
        const bool present =
            std::ranges::any_of(wanted, [&](const auto& w) { return w.id == row.id; });
        if (!present)
            pay_receive_repo_.remove(ctx, boost::uuids::to_string(row.id));
    }
    // One statement, so the requested set for this part lands together. The
    // store checks each row's claim inside it.
    if (!wanted.empty())
        pay_receive_repo_.write(ctx, wanted, claims);
    return {};
}

ores::utility::domain::result tenor_basis_swap_convention_service::prepare_long_short(
    const messaging::tenor_basis_swap_long_short_change& change,
    const ores::utility::domain::change_intent& intent,
    const context& ctx,
    domain::tenor_basis_swap_long_short& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain_long_short(change.write);
    const auto key = key_from_long_short(out);
    const auto current = long_short_repo_.read_latest(ctx, boost::uuids::to_string(key.id));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "tenor_basis_swap_long_short"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "tenor_basis_swap_long_short"});
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "tenor_basis_swap_long_short",
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

ores::utility::domain::result tenor_basis_swap_convention_service::reconcile_long_short(
    const context& ctx,
    const domain::convention& central,
    const std::vector<messaging::tenor_basis_swap_long_short_change>& requested,
    const ores::utility::domain::change_intent& intent) {
    using ores::utility::domain::outcome;
    // Every requested row is checked before any of them is written, so a
    // refused row refuses the family element rather than landing half of it.
    std::vector<domain::tenor_basis_swap_long_short> wanted;
    wanted.reserve(requested.size());
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(requested.size());
    for (const auto& change : requested) {
        domain::tenor_basis_swap_long_short row;
        const auto result = prepare_long_short(change, intent, ctx, row);
        if (result.outcome != outcome::ok)
            return result;
        row.convention_id = central.id;
        // A part that carries the owning party takes it from the central row,
        // which is the row the session stamped. The part table's own policy
        // reads that column, so it must agree with the central row even when
        // the session has no party of its own.
        row.party_id = central.party_id;
        wanted.push_back(std::move(row));
        claims.push_back(change.precondition);
    }
    // A row that is live now but absent from the request is closed, so this
    // part's rows for the central row become exactly the rows the request
    // carries. The close bumps the central version through the part table's
    // own trigger, so the C++ never touches it.
    const auto live = read_long_short(long_short_repo_, ctx, central);
    for (const auto& row : live) {
        const bool present =
            std::ranges::any_of(wanted, [&](const auto& w) { return w.id == row.id; });
        if (!present)
            long_short_repo_.remove(ctx, boost::uuids::to_string(row.id));
    }
    // One statement, so the requested set for this part lands together. The
    // store checks each row's claim inside it.
    if (!wanted.empty())
        long_short_repo_.write(ctx, wanted, claims);
    return {};
}
}
