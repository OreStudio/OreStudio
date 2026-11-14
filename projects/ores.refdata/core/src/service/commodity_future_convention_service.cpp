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
#include "ores.refdata.core/service/commodity_future_convention_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/commodity_future_averaging_terms.hpp"
#include "ores.refdata.api/domain/commodity_future_contract_terms.hpp"
#include "ores.refdata.api/domain/commodity_future_expiry_anchor.hpp"
#include "ores.refdata.api/domain/commodity_future_off_peak_index.hpp"
#include "ores.refdata.api/domain/commodity_future_option_expiry.hpp"
#include "ores.refdata.api/domain/convention.hpp"
#include "ores.refdata.api/messaging/commodity_future_convention_protocol.hpp"
#include "ores.refdata.core/repository/commodity_future_averaging_terms_repository.hpp"
#include "ores.refdata.core/repository/commodity_future_contract_terms_repository.hpp"
#include "ores.refdata.core/repository/commodity_future_expiry_anchor_repository.hpp"
#include "ores.refdata.core/repository/commodity_future_off_peak_index_repository.hpp"
#include "ores.refdata.core/repository/commodity_future_option_expiry_repository.hpp"
#include "ores.refdata.core/repository/convention_repository.hpp"
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
                const messaging::commodity_future_convention_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

/**
 * @brief The key a central domain row states, so the row read back is
 * addressed exactly as the request addressed it.
 */
messaging::commodity_future_convention_key
commodity_future_convention_key_from(const domain::convention& v) {
    messaging::commodity_future_convention_key key;
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
domain::convention commodity_future_convention_to_domain(const messaging::convention_write& write) {
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
std::vector<domain::commodity_future_averaging_terms>
read_averaging_terms(repository::commodity_future_averaging_terms_repository& repo,
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
domain::commodity_future_averaging_terms
to_domain_averaging_terms(const messaging::commodity_future_averaging_terms_write& write) {
    domain::commodity_future_averaging_terms v;
    v.id = write.id;
    v.convention_id = write.convention_id;
    v.averaging_commodity_name = write.averaging_commodity_name;
    v.averaging_period = write.averaging_period;
    v.averaging_pricing_calendar = write.averaging_pricing_calendar;
    v.averaging_conventions = write.averaging_conventions;
    v.averaging_use_business_days = write.averaging_use_business_days;
    v.averaging_delivery_roll_days = write.averaging_delivery_roll_days;
    v.averaging_future_month_offset = write.averaging_future_month_offset;
    v.averaging_daily_expiry_offset = write.averaging_daily_expiry_offset;
    return v;
}

/**
 * @brief The key a part domain row states.
 */
messaging::commodity_future_averaging_terms_key
key_from_averaging_terms(const domain::commodity_future_averaging_terms& v) {
    messaging::commodity_future_averaging_terms_key key;
    key.id = v.id;
    return key;
}

/**
 * @brief The live rows of one part kind that belong to one central row.
 */
std::vector<domain::commodity_future_contract_terms>
read_contract_terms(repository::commodity_future_contract_terms_repository& repo,
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
domain::commodity_future_contract_terms
to_domain_contract_terms(const messaging::commodity_future_contract_terms_write& write) {
    domain::commodity_future_contract_terms v;
    v.id = write.id;
    v.convention_id = write.convention_id;
    v.contract_frequency = write.contract_frequency;
    v.calendar = write.calendar;
    v.expiry_calendar = write.expiry_calendar;
    v.expiry_month_lag = write.expiry_month_lag;
    v.one_contract_month = write.one_contract_month;
    v.offset_days = write.offset_days;
    v.business_day_convention = write.business_day_convention;
    v.adjust_before_offset = write.adjust_before_offset;
    v.is_averaging = write.is_averaging;
    v.valid_contract_months = write.valid_contract_months;
    v.hours_per_day = write.hours_per_day;
    v.index_name = write.index_name;
    v.savings_time = write.savings_time;
    v.delivery_location = write.delivery_location;
    v.balance_of_the_month = write.balance_of_the_month;
    v.balance_of_the_month_pricing_calendar = write.balance_of_the_month_pricing_calendar;
    v.prohibited_expiries = write.prohibited_expiries;
    v.future_continuation_mappings = write.future_continuation_mappings;
    return v;
}

/**
 * @brief The key a part domain row states.
 */
messaging::commodity_future_contract_terms_key
key_from_contract_terms(const domain::commodity_future_contract_terms& v) {
    messaging::commodity_future_contract_terms_key key;
    key.id = v.id;
    return key;
}

/**
 * @brief The live rows of one part kind that belong to one central row.
 */
std::vector<domain::commodity_future_expiry_anchor>
read_expiry_anchor(repository::commodity_future_expiry_anchor_repository& repo,
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
domain::commodity_future_expiry_anchor
to_domain_expiry_anchor(const messaging::commodity_future_expiry_anchor_write& write) {
    domain::commodity_future_expiry_anchor v;
    v.id = write.id;
    v.convention_id = write.convention_id;
    v.anchor_day_of_month = write.anchor_day_of_month;
    v.anchor_calendar_days_before = write.anchor_calendar_days_before;
    v.anchor_business_days_after = write.anchor_business_days_after;
    v.anchor_nth_nth = write.anchor_nth_nth;
    v.anchor_nth_weekday = write.anchor_nth_weekday;
    v.anchor_last_weekday = write.anchor_last_weekday;
    v.anchor_weekly_day_of_the_week = write.anchor_weekly_day_of_the_week;
    return v;
}

/**
 * @brief The key a part domain row states.
 */
messaging::commodity_future_expiry_anchor_key
key_from_expiry_anchor(const domain::commodity_future_expiry_anchor& v) {
    messaging::commodity_future_expiry_anchor_key key;
    key.id = v.id;
    return key;
}

/**
 * @brief The live rows of one part kind that belong to one central row.
 */
std::vector<domain::commodity_future_off_peak_index>
read_off_peak_index(repository::commodity_future_off_peak_index_repository& repo,
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
domain::commodity_future_off_peak_index
to_domain_off_peak_index(const messaging::commodity_future_off_peak_index_write& write) {
    domain::commodity_future_off_peak_index v;
    v.id = write.id;
    v.convention_id = write.convention_id;
    v.off_peak_index = write.off_peak_index;
    v.peak_index = write.peak_index;
    v.off_peak_hours = write.off_peak_hours;
    v.peak_calendar = write.peak_calendar;
    return v;
}

/**
 * @brief The key a part domain row states.
 */
messaging::commodity_future_off_peak_index_key
key_from_off_peak_index(const domain::commodity_future_off_peak_index& v) {
    messaging::commodity_future_off_peak_index_key key;
    key.id = v.id;
    return key;
}

/**
 * @brief The live rows of one part kind that belong to one central row.
 */
std::vector<domain::commodity_future_option_expiry>
read_option_expiry(repository::commodity_future_option_expiry_repository& repo,
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
domain::commodity_future_option_expiry
to_domain_option_expiry(const messaging::commodity_future_option_expiry_write& write) {
    domain::commodity_future_option_expiry v;
    v.id = write.id;
    v.convention_id = write.convention_id;
    v.option_expiry_month_lag = write.option_expiry_month_lag;
    v.option_contract_frequency = write.option_contract_frequency;
    v.option_expiry_offset = write.option_expiry_offset;
    v.option_calendar_days_before = write.option_calendar_days_before;
    v.option_min_business_days_before = write.option_min_business_days_before;
    v.option_expiry_day = write.option_expiry_day;
    v.option_nth_nth = write.option_nth_nth;
    v.option_nth_weekday = write.option_nth_weekday;
    v.option_expiry_last_weekday_of_month = write.option_expiry_last_weekday_of_month;
    v.option_expiry_weekly_day_of_the_week = write.option_expiry_weekly_day_of_the_week;
    v.option_business_day_convention = write.option_business_day_convention;
    v.option_underlying_future_convention = write.option_underlying_future_convention;
    v.option_continuation_mappings = write.option_continuation_mappings;
    return v;
}

/**
 * @brief The key a part domain row states.
 */
messaging::commodity_future_option_expiry_key
key_from_option_expiry(const domain::commodity_future_option_expiry& v) {
    messaging::commodity_future_option_expiry_key key;
    key.id = v.id;
    return key;
}

}

commodity_future_convention_service::commodity_future_convention_service(const context& ctx)
    : ctx_(ctx) {}

messaging::get_commodity_future_convention_response
commodity_future_convention_service::get_commodity_future_convention(
    const messaging::get_commodity_future_convention_request& request) {
    messaging::get_commodity_future_convention_response response;
    auto found = read_convention(central_repo_, ctx_, request.key);
    if (found.empty()) {
        response.result =
            refuse(outcome_code::not_found, {.entity = "commodity_future_convention"});
        return response;
    }
    response.item = assemble(ctx_, request.key, found.front());
    return response;
}

messaging::get_many_commodity_future_conventions_response
commodity_future_convention_service::get_many_commodity_future_conventions(
    const messaging::get_many_commodity_future_conventions_request& request) {
    messaging::get_many_commodity_future_conventions_response response;
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

messaging::put_commodity_future_convention_response
commodity_future_convention_service::put_commodity_future_convention(
    const messaging::put_commodity_future_convention_request& request) {
    messaging::put_commodity_future_convention_response response;
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
    const auto key = commodity_future_convention_key_from(central);
    // A part write bumps the central row through the part table's own trigger,
    // so the central row is read back after the parts land and the item carries
    // the version the write produced.
    response.result =
        reconcile_averaging_terms(ctx, central, request.item.averaging_terms, request.intent);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    response.result =
        reconcile_contract_terms(ctx, central, request.item.contract_terms, request.intent);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    response.result =
        reconcile_expiry_anchor(ctx, central, request.item.expiry_anchor, request.intent);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    response.result =
        reconcile_off_peak_index(ctx, central, request.item.off_peak_index, request.intent);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    response.result =
        reconcile_option_expiry(ctx, central, request.item.option_expiry, request.intent);
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

messaging::put_many_commodity_future_conventions_response
commodity_future_convention_service::put_many_commodity_future_conventions(
    const messaging::put_many_commodity_future_conventions_request& request) {
    messaging::put_many_commodity_future_conventions_response response;
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

        const auto key = commodity_future_convention_key_from(central);
        // A part write bumps the central row, so it is read back after them.
        response.result =
            reconcile_averaging_terms(ctx, central, write.averaging_terms, request.intent);
        if (response.result.outcome != ores::utility::domain::outcome::ok)
            return response;
        response.result =
            reconcile_contract_terms(ctx, central, write.contract_terms, request.intent);
        if (response.result.outcome != ores::utility::domain::outcome::ok)
            return response;
        response.result =
            reconcile_expiry_anchor(ctx, central, write.expiry_anchor, request.intent);
        if (response.result.outcome != ores::utility::domain::outcome::ok)
            return response;
        response.result =
            reconcile_off_peak_index(ctx, central, write.off_peak_index, request.intent);
        if (response.result.outcome != ores::utility::domain::outcome::ok)
            return response;
        response.result =
            reconcile_option_expiry(ctx, central, write.option_expiry, request.intent);
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

messaging::commodity_future_convention_item
commodity_future_convention_service::assemble(const context& ctx,
                                              const messaging::commodity_future_convention_key& key,
                                              const std::optional<domain::convention>& central) {
    messaging::commodity_future_convention_item item;
    item.key = key;
    item.convention = central;
    if (central)
        item.averaging_terms = read_averaging_terms(averaging_terms_repo_, ctx, *central);
    if (central)
        item.contract_terms = read_contract_terms(contract_terms_repo_, ctx, *central);
    if (central)
        item.expiry_anchor = read_expiry_anchor(expiry_anchor_repo_, ctx, *central);
    if (central)
        item.off_peak_index = read_off_peak_index(off_peak_index_repo_, ctx, *central);
    if (central)
        item.option_expiry = read_option_expiry(option_expiry_repo_, ctx, *central);
    return item;
}

ores::utility::domain::result commodity_future_convention_service::prepare_convention(
    const messaging::convention_change& change,
    const ores::utility::domain::change_intent& intent,
    const context& ctx,
    domain::convention& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = commodity_future_convention_to_domain(change.write);
    const auto current =
        read_convention(central_repo_, ctx, commodity_future_convention_key_from(out));
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

ores::utility::domain::result commodity_future_convention_service::prepare_averaging_terms(
    const messaging::commodity_future_averaging_terms_change& change,
    const ores::utility::domain::change_intent& intent,
    const context& ctx,
    domain::commodity_future_averaging_terms& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain_averaging_terms(change.write);
    const auto key = key_from_averaging_terms(out);
    const auto current = averaging_terms_repo_.read_latest(ctx, boost::uuids::to_string(key.id));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "commodity_future_averaging_terms"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found,
                              {.entity = "commodity_future_averaging_terms"});
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "commodity_future_averaging_terms",
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

ores::utility::domain::result commodity_future_convention_service::reconcile_averaging_terms(
    const context& ctx,
    const domain::convention& central,
    const std::vector<messaging::commodity_future_averaging_terms_change>& requested,
    const ores::utility::domain::change_intent& intent) {
    using ores::utility::domain::outcome;
    // Every requested row is checked before any of them is written, so a
    // refused row refuses the family element rather than landing half of it.
    std::vector<domain::commodity_future_averaging_terms> wanted;
    wanted.reserve(requested.size());
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(requested.size());
    for (const auto& change : requested) {
        domain::commodity_future_averaging_terms row;
        const auto result = prepare_averaging_terms(change, intent, ctx, row);
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
    const auto live = read_averaging_terms(averaging_terms_repo_, ctx, central);
    for (const auto& row : live) {
        const bool present =
            std::ranges::any_of(wanted, [&](const auto& w) { return w.id == row.id; });
        if (!present)
            averaging_terms_repo_.remove(ctx, boost::uuids::to_string(row.id));
    }
    // One statement, so the requested set for this part lands together. The
    // store checks each row's claim inside it.
    if (!wanted.empty())
        averaging_terms_repo_.write(ctx, wanted, claims);
    return {};
}

ores::utility::domain::result commodity_future_convention_service::prepare_contract_terms(
    const messaging::commodity_future_contract_terms_change& change,
    const ores::utility::domain::change_intent& intent,
    const context& ctx,
    domain::commodity_future_contract_terms& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain_contract_terms(change.write);
    const auto key = key_from_contract_terms(out);
    const auto current = contract_terms_repo_.read_latest(ctx, boost::uuids::to_string(key.id));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "commodity_future_contract_terms"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found,
                              {.entity = "commodity_future_contract_terms"});
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "commodity_future_contract_terms",
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

ores::utility::domain::result commodity_future_convention_service::reconcile_contract_terms(
    const context& ctx,
    const domain::convention& central,
    const std::vector<messaging::commodity_future_contract_terms_change>& requested,
    const ores::utility::domain::change_intent& intent) {
    using ores::utility::domain::outcome;
    // Every requested row is checked before any of them is written, so a
    // refused row refuses the family element rather than landing half of it.
    std::vector<domain::commodity_future_contract_terms> wanted;
    wanted.reserve(requested.size());
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(requested.size());
    for (const auto& change : requested) {
        domain::commodity_future_contract_terms row;
        const auto result = prepare_contract_terms(change, intent, ctx, row);
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
    const auto live = read_contract_terms(contract_terms_repo_, ctx, central);
    for (const auto& row : live) {
        const bool present =
            std::ranges::any_of(wanted, [&](const auto& w) { return w.id == row.id; });
        if (!present)
            contract_terms_repo_.remove(ctx, boost::uuids::to_string(row.id));
    }
    // One statement, so the requested set for this part lands together. The
    // store checks each row's claim inside it.
    if (!wanted.empty())
        contract_terms_repo_.write(ctx, wanted, claims);
    return {};
}

ores::utility::domain::result commodity_future_convention_service::prepare_expiry_anchor(
    const messaging::commodity_future_expiry_anchor_change& change,
    const ores::utility::domain::change_intent& intent,
    const context& ctx,
    domain::commodity_future_expiry_anchor& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain_expiry_anchor(change.write);
    const auto key = key_from_expiry_anchor(out);
    const auto current = expiry_anchor_repo_.read_latest(ctx, boost::uuids::to_string(key.id));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "commodity_future_expiry_anchor"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found,
                              {.entity = "commodity_future_expiry_anchor"});
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "commodity_future_expiry_anchor",
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

ores::utility::domain::result commodity_future_convention_service::reconcile_expiry_anchor(
    const context& ctx,
    const domain::convention& central,
    const std::vector<messaging::commodity_future_expiry_anchor_change>& requested,
    const ores::utility::domain::change_intent& intent) {
    using ores::utility::domain::outcome;
    // Every requested row is checked before any of them is written, so a
    // refused row refuses the family element rather than landing half of it.
    std::vector<domain::commodity_future_expiry_anchor> wanted;
    wanted.reserve(requested.size());
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(requested.size());
    for (const auto& change : requested) {
        domain::commodity_future_expiry_anchor row;
        const auto result = prepare_expiry_anchor(change, intent, ctx, row);
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
    const auto live = read_expiry_anchor(expiry_anchor_repo_, ctx, central);
    for (const auto& row : live) {
        const bool present =
            std::ranges::any_of(wanted, [&](const auto& w) { return w.id == row.id; });
        if (!present)
            expiry_anchor_repo_.remove(ctx, boost::uuids::to_string(row.id));
    }
    // One statement, so the requested set for this part lands together. The
    // store checks each row's claim inside it.
    if (!wanted.empty())
        expiry_anchor_repo_.write(ctx, wanted, claims);
    return {};
}

ores::utility::domain::result commodity_future_convention_service::prepare_off_peak_index(
    const messaging::commodity_future_off_peak_index_change& change,
    const ores::utility::domain::change_intent& intent,
    const context& ctx,
    domain::commodity_future_off_peak_index& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain_off_peak_index(change.write);
    const auto key = key_from_off_peak_index(out);
    const auto current = off_peak_index_repo_.read_latest(ctx, boost::uuids::to_string(key.id));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "commodity_future_off_peak_index"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found,
                              {.entity = "commodity_future_off_peak_index"});
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "commodity_future_off_peak_index",
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

ores::utility::domain::result commodity_future_convention_service::reconcile_off_peak_index(
    const context& ctx,
    const domain::convention& central,
    const std::vector<messaging::commodity_future_off_peak_index_change>& requested,
    const ores::utility::domain::change_intent& intent) {
    using ores::utility::domain::outcome;
    // Every requested row is checked before any of them is written, so a
    // refused row refuses the family element rather than landing half of it.
    std::vector<domain::commodity_future_off_peak_index> wanted;
    wanted.reserve(requested.size());
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(requested.size());
    for (const auto& change : requested) {
        domain::commodity_future_off_peak_index row;
        const auto result = prepare_off_peak_index(change, intent, ctx, row);
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
    const auto live = read_off_peak_index(off_peak_index_repo_, ctx, central);
    for (const auto& row : live) {
        const bool present =
            std::ranges::any_of(wanted, [&](const auto& w) { return w.id == row.id; });
        if (!present)
            off_peak_index_repo_.remove(ctx, boost::uuids::to_string(row.id));
    }
    // One statement, so the requested set for this part lands together. The
    // store checks each row's claim inside it.
    if (!wanted.empty())
        off_peak_index_repo_.write(ctx, wanted, claims);
    return {};
}

ores::utility::domain::result commodity_future_convention_service::prepare_option_expiry(
    const messaging::commodity_future_option_expiry_change& change,
    const ores::utility::domain::change_intent& intent,
    const context& ctx,
    domain::commodity_future_option_expiry& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain_option_expiry(change.write);
    const auto key = key_from_option_expiry(out);
    const auto current = option_expiry_repo_.read_latest(ctx, boost::uuids::to_string(key.id));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "commodity_future_option_expiry"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found,
                              {.entity = "commodity_future_option_expiry"});
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "commodity_future_option_expiry",
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

ores::utility::domain::result commodity_future_convention_service::reconcile_option_expiry(
    const context& ctx,
    const domain::convention& central,
    const std::vector<messaging::commodity_future_option_expiry_change>& requested,
    const ores::utility::domain::change_intent& intent) {
    using ores::utility::domain::outcome;
    // Every requested row is checked before any of them is written, so a
    // refused row refuses the family element rather than landing half of it.
    std::vector<domain::commodity_future_option_expiry> wanted;
    wanted.reserve(requested.size());
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(requested.size());
    for (const auto& change : requested) {
        domain::commodity_future_option_expiry row;
        const auto result = prepare_option_expiry(change, intent, ctx, row);
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
    const auto live = read_option_expiry(option_expiry_repo_, ctx, central);
    for (const auto& row : live) {
        const bool present =
            std::ranges::any_of(wanted, [&](const auto& w) { return w.id == row.id; });
        if (!present)
            option_expiry_repo_.remove(ctx, boost::uuids::to_string(row.id));
    }
    // One statement, so the requested set for this part lands together. The
    // store checks each row's claim inside it.
    if (!wanted.empty())
        option_expiry_repo_.write(ctx, wanted, claims);
    return {};
}
}
