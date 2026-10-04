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
#include "ores.ore.service/messaging/ore_import_execute_handler.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.ore.api/messaging/ore_import_protocol.hpp"
#include "ores.ore.api/net/ore_storage.hpp"
#include "ores.ore.core/domain/trade_mapper.hpp"
#include "ores.ore.core/planner/ore_import_planner.hpp"
#include "ores.ore.core/scanner/ore_directory_scanner.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_identifier_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.api/messaging/netting_set_identifier_protocol.hpp"
#include "ores.refdata.api/messaging/portfolio_protocol.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.storage.core/net/storage_transfer.hpp"
#include "ores.trading.api/messaging/ascot_protocol.hpp"
#include "ores.trading.api/messaging/balance_guaranteed_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/bond_forward_protocol.hpp"
#include "ores.trading.api/messaging/bond_future_protocol.hpp"
#include "ores.trading.api/messaging/bond_instrument_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_call_date_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_conversion_target_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_leg_amortization_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_leg_amount_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_leg_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_leg_rate_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_leg_schedule_date_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_leg_schedule_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_protocol.hpp"
#include "ores.trading.api/messaging/bond_leg_amortization_protocol.hpp"
#include "ores.trading.api/messaging/bond_leg_amount_protocol.hpp"
#include "ores.trading.api/messaging/bond_leg_protocol.hpp"
#include "ores.trading.api/messaging/bond_leg_rate_protocol.hpp"
#include "ores.trading.api/messaging/bond_option_protocol.hpp"
#include "ores.trading.api/messaging/bond_repo_protocol.hpp"
#include "ores.trading.api/messaging/bond_trs_protocol.hpp"
#include "ores.trading.api/messaging/callable_swap_call_date_protocol.hpp"
#include "ores.trading.api/messaging/callable_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/cap_floor_instrument_protocol.hpp"
#include "ores.trading.api/messaging/commodity_basket_constituent_protocol.hpp"
#include "ores.trading.api/messaging/commodity_instrument_protocol.hpp"
#include "ores.trading.api/messaging/composite_instrument_protocol.hpp"
#include "ores.trading.api/messaging/credit_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_accumulator_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_asian_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_barrier_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_digital_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_forward_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_position_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_position_option_underlying_protocol.hpp"
#include "ores.trading.api/messaging/equity_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_variance_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fra_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_accumulator_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_asian_forward_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_barrier_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_digital_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_forward_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_vanilla_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_variance_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/inflation_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/instrument_option_exercise_fee_protocol.hpp"
#include "ores.trading.api/messaging/instrument_option_payment_date_protocol.hpp"
#include "ores.trading.api/messaging/instrument_option_premium_protocol.hpp"
#include "ores.trading.api/messaging/instrument_option_protocol.hpp"
#include "ores.trading.api/messaging/instrument_schedule_date_protocol.hpp"
#include "ores.trading.api/messaging/instrument_schedule_protocol.hpp"
#include "ores.trading.api/messaging/instrument_strike_protocol.hpp"
#include "ores.trading.api/messaging/knock_out_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/rpa_instrument_protocol.hpp"
#include "ores.trading.api/messaging/scripted_instrument_protocol.hpp"
#include "ores.trading.api/messaging/swaption_instrument_protocol.hpp"
#include "ores.trading.api/messaging/trade_additional_field_protocol.hpp"
#include "ores.trading.api/messaging/trade_envelope_additional_field_protocol.hpp"
#include "ores.trading.api/messaging/trade_envelope_portfolio_id_protocol.hpp"
#include "ores.trading.api/messaging/trade_envelope_protocol.hpp"
#include "ores.trading.api/messaging/trade_identifier_protocol.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.trading.api/messaging/vanilla_swap_instrument_protocol.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <format>
#include <rfl/json.hpp>
#include <set>
#include <unordered_map>
#include <unordered_set>

namespace ores::ore::service::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

namespace {

/**
 * @brief Makes an authenticated NATS request and deserialises the response.
 *
 * Returns nullopt and populates out_error on any error.
 */
template <typename Req>
std::optional<typename Req::response_type>
nats_call(ores::nats::service::nats_client& nats, const Req& request, std::string& out_error) {
    using Resp = typename Req::response_type;
    try {
        const auto& codec = ores::nats::default_wire_codec();
        const auto msg = nats.authenticated_request(Req::nats_subject, codec.encode(request));

        const auto err_it = msg.headers.find("X-Error");
        if (err_it != msg.headers.end()) {
            out_error = std::format("Service error on {}: {}", Req::nats_subject, err_it->second);
            return std::nullopt;
        }
        auto result = codec.decode<Resp>(msg.data);
        if (!result) {
            out_error = std::format(
                "Failed to parse response from {}: {}", Req::nats_subject, result.error().what());
            return std::nullopt;
        }
        // A canonical response states its outcome in a result; an older
        // response states it in success and message.
        if constexpr (requires { result->result.outcome; }) {
            if (result->result.outcome != ores::utility::domain::outcome::ok)
                out_error = result->result.message;
        } else if constexpr (requires {
                                 result->success;
                                 result->message;
                             }) {
            if (!result->success)
                out_error = result->message;
        }
        return *result;
    } catch (const std::exception& e) {
        out_error = std::format("Exception calling {}: {}", Req::nats_subject, e.what());
        return std::nullopt;
    }
}

// The document mirror types carry a date as the ISO spelling the XML
// states, while the flat protocol writes carry a calendar date. These
// helpers cross that boundary in both directions; an absent or empty
// spelling stays an unengaged optional.
std::optional<std::chrono::year_month_day> parse_date(const std::string& text) {
    if (text.empty())
        return std::nullopt;
    return ores::platform::time::datetime::from_iso8601_date(text);
}

std::optional<std::chrono::year_month_day>
parse_optional_date(const std::optional<std::string>& text) {
    return text ? parse_date(*text) : std::nullopt;
}

std::string iso_or_empty(const std::optional<std::chrono::year_month_day>& d) {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

std::string iso_or_empty(const std::optional<std::chrono::system_clock::time_point>& t) {
    return t ? ores::platform::time::datetime::to_iso8601_utc(*t) : std::string{};
}

// The canonical write record carries what the caller owns. The imported
// domain objects carry the server's fields too, so each projects by name.
ores::refdata::messaging::currency_write to_write(const ores::refdata::domain::currency& c) {
    return {.iso_code = c.iso_code,
            .name = c.name,
            .numeric_code = c.numeric_code,
            .symbol = c.symbol,
            .fraction_symbol = c.fraction_symbol,
            .fractions_per_unit = c.fractions_per_unit,
            .rounding_type = c.rounding_type,
            .rounding_precision = c.rounding_precision,
            .format = c.format,
            .monetary_nature = c.monetary_nature,
            .market_tier = c.market_tier,
            .image_id = c.image_id,
            .spot_days = c.spot_days,
            .day_basis = c.day_basis,
            .base_precedence = c.base_precedence};
}

ores::refdata::messaging::portfolio_write to_write(const ores::refdata::domain::portfolio& p) {
    return {.id = p.id,
            .name = p.name,
            .description = p.description,
            .parent_portfolio_id = p.parent_portfolio_id,
            .owner_unit_id = p.owner_unit_id,
            .purpose_type = p.purpose_type,
            .aggregation_ccy = p.aggregation_ccy,
            .is_virtual = p.is_virtual,
            .status = p.status};
}

ores::refdata::messaging::book_write to_write(const ores::refdata::domain::book& b) {
    return {.id = b.id,
            .name = b.name,
            .description = b.description,
            .parent_portfolio_id = b.parent_portfolio_id,
            .owner_unit_id = b.owner_unit_id,
            .functional_currency = b.functional_currency,
            .gl_account_ref = b.gl_account_ref,
            .cost_center = b.cost_center,
            .book_status = b.book_status,
            .regulatory_book_type = b.regulatory_book_type,
            .is_sweepable = b.is_sweepable,
            .rates_centre_code = b.rates_centre_code};
}

/**
 * @brief Saves one trade's envelope and the two lists it carries.
 *
 * The envelope row is keyed by the trade, so the trade must be saved
 * first. A document that stated no envelope writes no row. The list
 * ordinals are the document's order and start at one.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string
save_envelope(Nats& nats,
              const boost::uuids::uuid& trade_id,
              const std::optional<ores::trading::domain::trade_envelope_data>& envelope) {
    if (!envelope)
        return {};

    using ores::trading::messaging::put_trade_envelope_additional_field_request;
    using ores::trading::messaging::put_trade_envelope_portfolio_id_request;
    using ores::trading::messaging::put_trade_envelope_request;

    std::string error;
    put_trade_envelope_request envelope_req;
    envelope_req.change.write.trade_id = trade_id;
    envelope_req.change.write.counter_party = envelope->counter_party;
    envelope_req.change.write.netting_set_id = envelope->netting_set_id;
    envelope_req.change.write.has_portfolio_ids = envelope->portfolio_ids.has_value();
    envelope_req.change.write.has_additional_fields = envelope->additional_fields.has_value();
    auto resp = nats_call(nats, envelope_req, error);
    if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
        return error.empty() ? "save_trade_envelope failed" : error;

    if (envelope->portfolio_ids) {
        int sequence_number = 0;
        for (const auto& portfolio_id : *envelope->portfolio_ids) {
            put_trade_envelope_portfolio_id_request child_req;
            child_req.change.write.trade_id = trade_id;
            child_req.change.write.sequence_number = ++sequence_number;
            child_req.change.write.portfolio_id = portfolio_id;
            auto child_resp = nats_call(nats, child_req, error);
            if (!child_resp || child_resp->result.outcome != ores::utility::domain::outcome::ok)
                return error.empty() ? "save_trade_envelope_portfolio_id failed" : error;
        }
    }

    if (envelope->additional_fields) {
        int sequence_number = 0;
        for (const auto& field : *envelope->additional_fields) {
            put_trade_envelope_additional_field_request child_req;
            child_req.change.write.trade_id = trade_id;
            child_req.change.write.sequence_number = ++sequence_number;
            child_req.change.write.name = field.name;
            child_req.change.write.value = field.value;
            auto child_resp = nats_call(nats, child_req, error);
            if (!child_resp || child_resp->result.outcome != ores::utility::domain::outcome::ok)
                return error.empty() ? "save_trade_envelope_additional_field failed" : error;
        }
    }

    return {};
}

/**
 * @brief The counterparty an ORE envelope's CounterParty names.
 *
 * An ORE document names a counterparty by a string of its own, so the name is
 * resolved as an ORE identifier of a counterparty first, then as a short code.
 *
 * @return The counterparty's id, or nullopt when the name matches none; on a
 * failed read, nullopt with out_error set.
 */
template <typename Nats>
std::optional<boost::uuids::uuid>
resolve_counterparty(Nats& nats, const std::string& name, std::string& out_error) {
    using ores::utility::domain::outcome;

    std::string error;
    ores::refdata::messaging::get_counterparty_identifier_request alias_req;
    alias_req.key.id_value = name;
    auto alias = nats_call(nats, alias_req, error);
    if (!alias) {
        out_error = error;
        return std::nullopt;
    }
    if (alias->result.outcome == outcome::ok && alias->counterparty_identifier &&
        alias->counterparty_identifier->id_scheme == "ORE")
        return alias->counterparty_identifier->counterparty_id;

    error.clear();
    ores::refdata::messaging::get_counterparty_request code_req;
    code_req.key.short_code = name;
    auto counterparty = nats_call(nats, code_req, error);
    if (!counterparty) {
        out_error = error;
        return std::nullopt;
    }
    if (counterparty->result.outcome == outcome::ok && counterparty->counterparty)
        return counterparty->counterparty->id;
    return std::nullopt;
}

/**
 * @brief The netting set an ORE envelope's NettingSetId names.
 *
 * An ORE document names a netting set by a string of its own, which a netting
 * set answers to as an ORE identifier, as a counterparty does.
 *
 * @return The netting set's id, or nullopt when the id matches none; on a
 * failed read, nullopt with out_error set.
 */
template <typename Nats>
std::optional<boost::uuids::uuid>
resolve_netting_set(Nats& nats, const std::string& netting_set_id, std::string& out_error) {
    using ores::utility::domain::outcome;

    std::string error;
    ores::refdata::messaging::get_netting_set_identifier_request alias_req;
    alias_req.key.id_value = netting_set_id;
    auto alias = nats_call(nats, alias_req, error);
    if (!alias) {
        out_error = error;
        return std::nullopt;
    }
    if (alias->result.outcome == outcome::ok && alias->netting_set_identifier &&
        alias->netting_set_identifier->id_scheme == "ORE")
        return alias->netting_set_identifier->netting_set_id;
    return std::nullopt;
}

/**
 * @brief Books an imported trade into its anchor and components.
 *
 * The anchor, booking and state are one write. ORE's Trade/@id becomes an
 * identifier under the ORE scheme, and each envelope additional field a row
 * of its own, in the document's order.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string
book_imported_trade(Nats& nats,
                    const ores::trading::domain::trade& trade,
                    const std::optional<boost::uuids::uuid>& counterparty_id,
                    const std::optional<boost::uuids::uuid>& netting_set_id,
                    const std::optional<ores::trading::domain::trade_envelope_data>& envelope) {
    using namespace ores::trading::domain;
    using ores::utility::domain::outcome;

    std::string error;
    ores::trading::messaging::book_trade_request book_req;
    book_req.anchor.id = trade.identity.id;
    book_req.anchor.counterparty_id = counterparty_id;
    book_req.anchor.trade_type = trade.classification.trade_type;
    book_req.anchor.counterparty_scope =
        counterparty_id ? counterparty_scope::external : counterparty_scope::intra_entity;
    book_req.anchor.booking_nature = booking_nature::actual;
    book_req.anchor.entry_channel = entry_channel::stp;
    book_req.booking.book_id = trade.parties.book_id;
    book_req.booking.netting_set_id = netting_set_id;
    book_req.booking.trade_date = trade.lifecycle.trade_date;
    book_req.booking.execution_timestamp = trade.lifecycle.execution_timestamp;
    book_req.booking.change_reason_code = "system.external_data_import";
    book_req.activity_type_code = "new_booking";
    auto booked = nats_call(nats, book_req, error);
    if (!booked || booked->result.outcome != outcome::ok)
        return error.empty() ? "book_trade failed" : error;

    error.clear();
    ores::trading::messaging::put_trade_identifier_request id_req;
    id_req.change.write.trade_id = trade.identity.id;
    id_req.change.write.id_type = "ORE";
    id_req.change.write.id_value = trade.identity.external_id;
    auto identified = nats_call(nats, id_req, error);
    if (!identified || identified->result.outcome != outcome::ok)
        return error.empty() ? "save_trade_identifier failed" : error;

    if (envelope && envelope->additional_fields) {
        int sequence_number = 0;
        for (const auto& field : *envelope->additional_fields) {
            error.clear();
            ores::trading::messaging::put_trade_additional_field_request field_req;
            field_req.change.write.trade_id = trade.identity.id;
            field_req.change.write.sequence_number = ++sequence_number;
            field_req.change.write.name = field.name;
            field_req.change.write.value = field.value;
            auto saved = nats_call(nats, field_req, error);
            if (!saved || saved->result.outcome != outcome::ok)
                return error.empty() ? "save_trade_additional_field failed" : error;
        }
    }
    return {};
}

/**
 * @brief Reads the identifier of every stored bond issue, keyed by its security id.
 *
 * One issue row serves every trade of an ISIN and its security_id is
 * unique among the current rows, so an import that meets an ISIN already
 * stored adopts that row instead of minting a second one. The read is
 * paged, and one pass covers the whole run.
 *
 * @return The map, empty when the read fails, with out_error set.
 */
template <typename Nats>
std::unordered_map<std::string, std::string>
read_bond_issue_ids_by_security(Nats& nats, std::string& out_error) {
    using ores::trading::messaging::list_bond_issues_request;

    constexpr std::uint32_t page_size = 200;
    constexpr int max_pages = 500;

    std::unordered_map<std::string, std::string> result;
    std::uint32_t offset = 0;
    for (int page = 0; page < max_pages; ++page) {
        list_bond_issues_request req;
        req.offset = offset;
        req.limit = page_size;
        auto resp = nats_call(nats, req, out_error);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
            return {};
        for (const auto& issue : resp->issues)
            result[issue.security_id] = boost::uuids::to_string(issue.issue_id);
        if (resp->issues.size() < page_size)
            break;
        offset += page_size;
    }
    return result;
}

/**
 * @brief Saves one schedule of one owner: the entries and their dates.
 *
 * The entries are numbered from one within their role, so a reader
 * reassembles the document's order. Each entry writes its own row and
 * each of its dates writes a child row keyed to that entry's ordinal.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_schedule(Nats& nats,
                          const boost::uuids::uuid& trade_id,
                          const std::string& owner_role,
                          int owner_number,
                          const std::string& schedule_role,
                          const ores::trading::domain::bond_schedule_data& schedule) {
    using ores::trading::messaging::put_instrument_schedule_date_request;
    using ores::trading::messaging::put_instrument_schedule_request;

    std::string error;
    int sequence_number = 0;
    for (const auto& rule : schedule.rules) {
        put_instrument_schedule_request req;
        req.change.write.trade_id = trade_id;
        req.change.write.owner_role = owner_role;
        req.change.write.owner_number = owner_number;
        req.change.write.schedule_role = schedule_role;
        req.change.write.sequence_number = ++sequence_number;
        req.change.write.schedule_kind = "rules";
        req.change.write.start_date = parse_date(rule.start_date);
        req.change.write.end_date = parse_optional_date(rule.end_date);
        req.change.write.adjust_end_date_to_previous_month_end =
            rule.adjust_end_date_to_previous_month_end;
        req.change.write.tenor = rule.tenor;
        req.change.write.calendar = rule.calendar;
        req.change.write.convention = rule.convention;
        req.change.write.term_convention = rule.term_convention;
        req.change.write.rule = rule.rule;
        req.change.write.end_of_month = rule.end_of_month;
        req.change.write.end_of_month_convention = rule.end_of_month_convention;
        req.change.write.first_date = parse_optional_date(rule.first_date);
        req.change.write.last_date = parse_optional_date(rule.last_date);
        req.change.write.remove_first_date = rule.remove_first_date;
        req.change.write.remove_last_date = rule.remove_last_date;
        auto resp = nats_call(nats, req, error);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_instrument_schedule failed" : error;
    }

    for (const auto& dates : schedule.dates) {
        put_instrument_schedule_request req;
        req.change.write.trade_id = trade_id;
        req.change.write.owner_role = owner_role;
        req.change.write.owner_number = owner_number;
        req.change.write.schedule_role = schedule_role;
        req.change.write.sequence_number = ++sequence_number;
        req.change.write.schedule_kind = "dates";
        req.change.write.calendar = dates.calendar;
        req.change.write.convention = dates.convention;
        req.change.write.tenor = dates.tenor;
        req.change.write.end_of_month = dates.end_of_month;
        req.change.write.include_duplicate_dates = dates.include_duplicate_dates;
        auto resp = nats_call(nats, req, error);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_instrument_schedule failed" : error;

        int date_number = 0;
        for (const auto& date : dates.dates) {
            put_instrument_schedule_date_request date_req;
            date_req.change.write.trade_id = trade_id;
            date_req.change.write.owner_role = owner_role;
            date_req.change.write.owner_number = owner_number;
            date_req.change.write.schedule_role = schedule_role;
            date_req.change.write.schedule_sequence_number = req.change.write.sequence_number;
            date_req.change.write.sequence_number = ++date_number;
            date_req.change.write.schedule_date =
                ores::platform::time::datetime::from_iso8601_date(date);
            auto date_resp = nats_call(nats, date_req, error);
            if (!date_resp || date_resp->result.outcome != ores::utility::domain::outcome::ok)
                return error.empty() ? "save_instrument_schedule_date failed" : error;
        }
    }

    return {};
}

/**
 * @brief Saves one of a leg's six amount lists under the role that names it.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_leg_amounts(Nats& nats,
                             const boost::uuids::uuid& trade_id,
                             const std::string& leg_role,
                             int leg_number,
                             const std::string& amount_role,
                             const std::vector<ores::trading::domain::bond_float_data>& amounts) {
    using ores::trading::messaging::put_bond_leg_amount_request;

    std::string error;
    int sequence_number = 0;
    for (const auto& amount : amounts) {
        put_bond_leg_amount_request req;
        req.change.write.trade_id = trade_id;
        req.change.write.leg_role = leg_role;
        req.change.write.leg_number = leg_number;
        req.change.write.amount_role = amount_role;
        req.change.write.sequence_number = ++sequence_number;
        // The ORE XML number is a binary float and the amount is a decimal
        // from here on, so the value is converted once, at the boundary.
        req.change.write.value = ores::utility::decimal::decimal::from_double(amount.value).value();
        req.change.write.start_date = amount.start_date;
        auto resp = nats_call(nats, req, error);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_leg_amount failed" : error;
    }
    return {};
}

/**
 * @brief Saves one bond leg: its row, its amounts, its rate, its
 * amortizations and its five schedules.
 *
 * A leg that carries nothing writes no row, which is what a container
 * that came from a row set rather than from a document holds.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_leg(Nats& nats,
                     const boost::uuids::uuid& trade_id,
                     const std::string& leg_role,
                     int leg_number,
                     const ores::trading::domain::bond_leg_data& leg) {
    using ores::trading::messaging::put_bond_leg_amortization_request;
    using ores::trading::messaging::put_bond_leg_rate_request;
    using ores::trading::messaging::put_bond_leg_request;

    if (leg.is_empty())
        return {};

    std::string error;
    put_bond_leg_request leg_req;
    leg_req.change.write.trade_id = trade_id;
    leg_req.change.write.leg_role = leg_role;
    leg_req.change.write.leg_number = leg_number;
    leg_req.change.write.payer = leg.payer;
    leg_req.change.write.leg_type = leg.leg_type;
    leg_req.change.write.currency = leg.currency;
    leg_req.change.write.payment_convention = leg.payment_convention;
    leg_req.change.write.payment_lag = leg.payment_lag;
    leg_req.change.write.payment_calendar = leg.payment_calendar;
    leg_req.change.write.day_counter = leg.day_counter;
    leg_req.change.write.last_period_day_counter = leg.last_period_day_counter;
    leg_req.change.write.notional_payment_lag = leg.notional_payment_lag;
    leg_req.change.write.strict_notional_dates = leg.strict_notional_dates;
    leg_req.change.write.indexings_from_asset_leg = leg.indexings_from_asset_leg;
    if (leg.settlement) {
        leg_req.change.write.settlement_fx_index = leg.settlement->fx_index;
        leg_req.change.write.settlement_fixing_date = leg.settlement->fixing_date;
    }
    auto leg_resp = nats_call(nats, leg_req, error);
    if (!leg_resp || leg_resp->result.outcome != ores::utility::domain::outcome::ok)
        return error.empty() ? "save_bond_leg failed" : error;

    if (auto failure =
            save_leg_amounts(nats, trade_id, leg_role, leg_number, "notional", leg.notionals);
        !failure.empty())
        return failure;

    if (leg.rate && leg.rate->fixed) {
        if (auto failure = save_leg_amounts(
                nats, trade_id, leg_role, leg_number, "rate", leg.rate->fixed->rates);
            !failure.empty())
            return failure;
    }

    if (leg.rate && leg.rate->floating) {
        const auto& floating = *leg.rate->floating;
        const std::pair<std::string, const std::vector<ores::trading::domain::bond_float_data>*>
            lists[] = {{"spread", &floating.spreads},
                       {"cap", &floating.caps},
                       {"floor", &floating.floors},
                       {"gearing", &floating.gearings}};
        for (const auto& [role, amounts] : lists) {
            if (auto failure =
                    save_leg_amounts(nats, trade_id, leg_role, leg_number, role, *amounts);
                !failure.empty())
                return failure;
        }

        put_bond_leg_rate_request rate_req;
        rate_req.change.write.trade_id = trade_id;
        rate_req.change.write.leg_role = leg_role;
        rate_req.change.write.leg_number = leg_number;
        rate_req.change.write.rate_kind = "floating";
        rate_req.change.write.index = floating.index;
        rate_req.change.write.is_in_arrears = floating.is_in_arrears;
        rate_req.change.write.last_recent_period = floating.last_recent_period;
        rate_req.change.write.last_recent_period_calendar = floating.last_recent_period_calendar;
        if (floating.fixing_days)
            rate_req.change.write.fixing_days = static_cast<std::int64_t>(*floating.fixing_days);
        rate_req.change.write.lookback = floating.lookback;
        rate_req.change.write.rate_cutoff = floating.rate_cutoff;
        rate_req.change.write.is_averaged = floating.is_averaged;
        rate_req.change.write.has_sub_periods = floating.has_sub_periods;
        rate_req.change.write.include_spread = floating.include_spread;
        rate_req.change.write.is_not_resetting_xccy = floating.is_not_resetting_xccy;
        rate_req.change.write.naked_option = floating.naked_option;
        rate_req.change.write.local_cap_floor = floating.local_cap_floor;
        rate_req.change.write.stub_use_original_curve = floating.stub_use_original_curve;
        rate_req.change.write.observation_shift = floating.observation_shift;
        if (floating.front_stub_interpolation) {
            const auto& stub = *floating.front_stub_interpolation;
            rate_req.change.write.front_stub_short_index = stub.short_index;
            rate_req.change.write.front_stub_long_index = stub.long_index;
            rate_req.change.write.front_stub_rounding_type = stub.rounding_type;
            rate_req.change.write.front_stub_rounding_precision = stub.rounding_precision;
        }
        if (floating.back_stub_interpolation) {
            const auto& stub = *floating.back_stub_interpolation;
            rate_req.change.write.back_stub_short_index = stub.short_index;
            rate_req.change.write.back_stub_long_index = stub.long_index;
            rate_req.change.write.back_stub_rounding_type = stub.rounding_type;
            rate_req.change.write.back_stub_rounding_precision = stub.rounding_precision;
        }
        auto rate_resp = nats_call(nats, rate_req, error);
        if (!rate_resp || rate_resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_leg_rate failed" : error;

        if (auto failure = save_schedule(
                nats, trade_id, leg_role, leg_number, "fixing_schedule", floating.fixing_schedule);
            !failure.empty())
            return failure;
        if (auto failure = save_schedule(
                nats, trade_id, leg_role, leg_number, "reset_schedule", floating.reset_schedule);
            !failure.empty())
            return failure;
    } else if (leg.rate && leg.rate->formula_based) {
        const auto& formula = *leg.rate->formula_based;
        put_bond_leg_rate_request rate_req;
        rate_req.change.write.trade_id = trade_id;
        rate_req.change.write.leg_role = leg_role;
        rate_req.change.write.leg_number = leg_number;
        rate_req.change.write.rate_kind = "formula_based";
        rate_req.change.write.index = formula.index;
        rate_req.change.write.is_in_arrears = formula.is_in_arrears;
        rate_req.change.write.fixing_days = formula.fixing_days;
        rate_req.change.write.fixing_calendar = formula.fixing_calendar;
        auto rate_resp = nats_call(nats, rate_req, error);
        if (!rate_resp || rate_resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_leg_rate failed" : error;
    } else if (leg.rate && leg.rate->fixed) {
        put_bond_leg_rate_request rate_req;
        rate_req.change.write.trade_id = trade_id;
        rate_req.change.write.leg_role = leg_role;
        rate_req.change.write.leg_number = leg_number;
        rate_req.change.write.rate_kind = "fixed";
        auto rate_resp = nats_call(nats, rate_req, error);
        if (!rate_resp || rate_resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_leg_rate failed" : error;
    }

    int sequence_number = 0;
    for (const auto& amortization : leg.amortizations) {
        put_bond_leg_amortization_request req;
        req.change.write.trade_id = trade_id;
        req.change.write.leg_role = leg_role;
        req.change.write.leg_number = leg_number;
        req.change.write.sequence_number = ++sequence_number;
        req.change.write.amortization_type = amortization.type;
        // The ORE XML number is a binary float and the amount is a decimal
        // from here on, so the value is converted once, at the boundary.
        req.change.write.value =
            amortization.value ?
                std::optional(
                    ores::utility::decimal::decimal::from_double(*amortization.value).value()) :
                std::nullopt;
        req.change.write.start_date = amortization.start_date;
        req.change.write.end_date = amortization.end_date;
        req.change.write.frequency = amortization.frequency;
        req.change.write.underflow = amortization.underflow;
        auto resp = nats_call(nats, req, error);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_leg_amortization failed" : error;
    }

    if (auto failure =
            save_schedule(nats, trade_id, leg_role, leg_number, "schedule", leg.schedule);
        !failure.empty())
        return failure;
    if (auto failure = save_schedule(
            nats, trade_id, leg_role, leg_number, "payment_schedule", leg.payment_schedule);
        !failure.empty())
        return failure;

    if (!leg.payment_dates.empty()) {
        ores::trading::domain::bond_schedule_data dates;
        ores::trading::domain::bond_schedule_dates block;
        block.dates = leg.payment_dates;
        dates.dates.push_back(std::move(block));
        if (auto failure =
                save_schedule(nats, trade_id, leg_role, leg_number, "payment_dates", dates);
            !failure.empty())
            return failure;
    }

    return {};
}

/**
 * @brief Saves one schedule of one bond issue leg: the entries and their dates.
 *
 * A bond's legs belong to the security, so their schedules key on the
 * issue rather than on the trade. One schedule then serves every
 * instrument of the ISIN.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_issue_schedule(Nats& nats,
                                const boost::uuids::uuid& issue_id,
                                int leg_number,
                                const std::string& schedule_role,
                                const ores::trading::domain::bond_schedule_data& schedule) {
    using ores::trading::messaging::put_bond_issue_leg_schedule_date_request;
    using ores::trading::messaging::put_bond_issue_leg_schedule_request;

    std::string error;
    int sequence_number = 0;
    for (const auto& rule : schedule.rules) {
        put_bond_issue_leg_schedule_request req;
        req.change.write.issue_id = issue_id;
        req.change.write.leg_number = leg_number;
        req.change.write.schedule_role = schedule_role;
        req.change.write.sequence_number = ++sequence_number;
        req.change.write.schedule_kind = "rules";
        req.change.write.start_date = parse_date(rule.start_date);
        req.change.write.end_date = parse_optional_date(rule.end_date);
        req.change.write.adjust_end_date_to_previous_month_end =
            rule.adjust_end_date_to_previous_month_end;
        req.change.write.tenor = rule.tenor;
        req.change.write.calendar = rule.calendar;
        req.change.write.convention = rule.convention;
        req.change.write.term_convention = rule.term_convention;
        req.change.write.rule = rule.rule;
        req.change.write.end_of_month = rule.end_of_month;
        req.change.write.end_of_month_convention = rule.end_of_month_convention;
        req.change.write.first_date = parse_optional_date(rule.first_date);
        req.change.write.last_date = parse_optional_date(rule.last_date);
        req.change.write.remove_first_date = rule.remove_first_date;
        req.change.write.remove_last_date = rule.remove_last_date;
        auto resp = nats_call(nats, req, error);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_issue_leg_schedule failed" : error;
    }

    for (const auto& dates : schedule.dates) {
        put_bond_issue_leg_schedule_request req;
        req.change.write.issue_id = issue_id;
        req.change.write.leg_number = leg_number;
        req.change.write.schedule_role = schedule_role;
        req.change.write.sequence_number = ++sequence_number;
        req.change.write.schedule_kind = "dates";
        req.change.write.calendar = dates.calendar;
        req.change.write.convention = dates.convention;
        req.change.write.tenor = dates.tenor;
        req.change.write.end_of_month = dates.end_of_month;
        req.change.write.include_duplicate_dates = dates.include_duplicate_dates;
        auto resp = nats_call(nats, req, error);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_issue_leg_schedule failed" : error;

        int date_number = 0;
        for (const auto& date : dates.dates) {
            put_bond_issue_leg_schedule_date_request date_req;
            date_req.change.write.issue_id = issue_id;
            date_req.change.write.leg_number = leg_number;
            date_req.change.write.schedule_role = schedule_role;
            date_req.change.write.schedule_sequence_number = req.change.write.sequence_number;
            date_req.change.write.sequence_number = ++date_number;
            date_req.change.write.schedule_date =
                ores::platform::time::datetime::from_iso8601_date(date);
            auto date_resp = nats_call(nats, date_req, error);
            if (!date_resp || date_resp->result.outcome != ores::utility::domain::outcome::ok)
                return error.empty() ? "save_bond_issue_leg_schedule_date failed" : error;
        }
    }

    return {};
}

/**
 * @brief Saves one of an issue leg's six amount lists under the role that names it.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string
save_issue_leg_amounts(Nats& nats,
                       const boost::uuids::uuid& issue_id,
                       int leg_number,
                       const std::string& amount_role,
                       const std::vector<ores::trading::domain::bond_float_data>& amounts) {
    using ores::trading::messaging::put_bond_issue_leg_amount_request;

    std::string error;
    int sequence_number = 0;
    for (const auto& amount : amounts) {
        put_bond_issue_leg_amount_request req;
        req.change.write.issue_id = issue_id;
        req.change.write.leg_number = leg_number;
        req.change.write.amount_role = amount_role;
        req.change.write.sequence_number = ++sequence_number;
        // The ORE XML number is a binary float and the amount is a decimal
        // from here on, so the value is converted once, at the boundary.
        req.change.write.value = ores::utility::decimal::decimal::from_double(amount.value).value();
        req.change.write.start_date = amount.start_date;
        auto resp = nats_call(nats, req, error);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_issue_leg_amount failed" : error;
    }
    return {};
}

/**
 * @brief Saves one bond issue leg: its row, its amounts, its rate, its
 * amortizations and its five schedules.
 *
 * The security owns the bond's legs, so the leg keys on the issue and
 * every child it states does too.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_issue_leg(Nats& nats,
                           const boost::uuids::uuid& issue_id,
                           int leg_number,
                           const ores::trading::domain::bond_leg_data& leg) {
    using ores::trading::messaging::put_bond_issue_leg_amortization_request;
    using ores::trading::messaging::put_bond_issue_leg_rate_request;
    using ores::trading::messaging::put_bond_issue_leg_request;

    if (leg.is_empty())
        return {};

    std::string error;
    put_bond_issue_leg_request leg_req;
    leg_req.change.write.issue_id = issue_id;
    leg_req.change.write.leg_number = leg_number;
    leg_req.change.write.payer = leg.payer;
    leg_req.change.write.leg_type = leg.leg_type;
    leg_req.change.write.currency = leg.currency;
    leg_req.change.write.payment_convention = leg.payment_convention;
    leg_req.change.write.payment_lag = leg.payment_lag;
    leg_req.change.write.payment_calendar = leg.payment_calendar;
    leg_req.change.write.day_counter = leg.day_counter;
    leg_req.change.write.last_period_day_counter = leg.last_period_day_counter;
    leg_req.change.write.notional_payment_lag = leg.notional_payment_lag;
    leg_req.change.write.strict_notional_dates = leg.strict_notional_dates;
    leg_req.change.write.indexings_from_asset_leg = leg.indexings_from_asset_leg;
    if (leg.settlement) {
        leg_req.change.write.settlement_fx_index = leg.settlement->fx_index;
        leg_req.change.write.settlement_fixing_date = leg.settlement->fixing_date;
    }
    auto leg_resp = nats_call(nats, leg_req, error);
    if (!leg_resp || leg_resp->result.outcome != ores::utility::domain::outcome::ok)
        return error.empty() ? "save_bond_issue_leg failed" : error;

    if (auto failure =
            save_issue_leg_amounts(nats, issue_id, leg_number, "notional", leg.notionals);
        !failure.empty())
        return failure;

    if (leg.rate && leg.rate->fixed) {
        if (auto failure =
                save_issue_leg_amounts(nats, issue_id, leg_number, "rate", leg.rate->fixed->rates);
            !failure.empty())
            return failure;
    }

    if (leg.rate && leg.rate->floating) {
        const auto& floating = *leg.rate->floating;
        const std::pair<std::string, const std::vector<ores::trading::domain::bond_float_data>*>
            lists[] = {{"spread", &floating.spreads},
                       {"cap", &floating.caps},
                       {"floor", &floating.floors},
                       {"gearing", &floating.gearings}};
        for (const auto& [role, amounts] : lists) {
            if (auto failure = save_issue_leg_amounts(nats, issue_id, leg_number, role, *amounts);
                !failure.empty())
                return failure;
        }

        put_bond_issue_leg_rate_request rate_req;
        rate_req.change.write.issue_id = issue_id;
        rate_req.change.write.leg_number = leg_number;
        rate_req.change.write.rate_kind = "floating";
        rate_req.change.write.index = floating.index;
        rate_req.change.write.is_in_arrears = floating.is_in_arrears;
        rate_req.change.write.last_recent_period = floating.last_recent_period;
        rate_req.change.write.last_recent_period_calendar = floating.last_recent_period_calendar;
        if (floating.fixing_days)
            rate_req.change.write.fixing_days = static_cast<std::int64_t>(*floating.fixing_days);
        rate_req.change.write.lookback = floating.lookback;
        rate_req.change.write.rate_cutoff = floating.rate_cutoff;
        rate_req.change.write.is_averaged = floating.is_averaged;
        rate_req.change.write.has_sub_periods = floating.has_sub_periods;
        rate_req.change.write.include_spread = floating.include_spread;
        rate_req.change.write.is_not_resetting_xccy = floating.is_not_resetting_xccy;
        rate_req.change.write.naked_option = floating.naked_option;
        rate_req.change.write.local_cap_floor = floating.local_cap_floor;
        rate_req.change.write.stub_use_original_curve = floating.stub_use_original_curve;
        rate_req.change.write.observation_shift = floating.observation_shift;
        if (floating.front_stub_interpolation) {
            const auto& stub = *floating.front_stub_interpolation;
            rate_req.change.write.front_stub_short_index = stub.short_index;
            rate_req.change.write.front_stub_long_index = stub.long_index;
            rate_req.change.write.front_stub_rounding_type = stub.rounding_type;
            rate_req.change.write.front_stub_rounding_precision = stub.rounding_precision;
        }
        if (floating.back_stub_interpolation) {
            const auto& stub = *floating.back_stub_interpolation;
            rate_req.change.write.back_stub_short_index = stub.short_index;
            rate_req.change.write.back_stub_long_index = stub.long_index;
            rate_req.change.write.back_stub_rounding_type = stub.rounding_type;
            rate_req.change.write.back_stub_rounding_precision = stub.rounding_precision;
        }
        auto rate_resp = nats_call(nats, rate_req, error);
        if (!rate_resp || rate_resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_issue_leg_rate failed" : error;

        if (auto failure = save_issue_schedule(
                nats, issue_id, leg_number, "fixing_schedule", floating.fixing_schedule);
            !failure.empty())
            return failure;
        if (auto failure = save_issue_schedule(
                nats, issue_id, leg_number, "reset_schedule", floating.reset_schedule);
            !failure.empty())
            return failure;
    } else if (leg.rate && leg.rate->formula_based) {
        const auto& formula = *leg.rate->formula_based;
        put_bond_issue_leg_rate_request rate_req;
        rate_req.change.write.issue_id = issue_id;
        rate_req.change.write.leg_number = leg_number;
        rate_req.change.write.rate_kind = "formula_based";
        rate_req.change.write.index = formula.index;
        rate_req.change.write.is_in_arrears = formula.is_in_arrears;
        rate_req.change.write.fixing_days = formula.fixing_days;
        rate_req.change.write.fixing_calendar = formula.fixing_calendar;
        auto rate_resp = nats_call(nats, rate_req, error);
        if (!rate_resp || rate_resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_issue_leg_rate failed" : error;
    } else if (leg.rate && leg.rate->fixed) {
        put_bond_issue_leg_rate_request rate_req;
        rate_req.change.write.issue_id = issue_id;
        rate_req.change.write.leg_number = leg_number;
        rate_req.change.write.rate_kind = "fixed";
        auto rate_resp = nats_call(nats, rate_req, error);
        if (!rate_resp || rate_resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_issue_leg_rate failed" : error;
    }

    int sequence_number = 0;
    for (const auto& amortization : leg.amortizations) {
        put_bond_issue_leg_amortization_request req;
        req.change.write.issue_id = issue_id;
        req.change.write.leg_number = leg_number;
        req.change.write.sequence_number = ++sequence_number;
        req.change.write.amortization_type = amortization.type;
        // The ORE XML number is a binary float and the amount is a decimal
        // from here on, so the value is converted once, at the boundary.
        req.change.write.value =
            amortization.value ?
                std::optional(
                    ores::utility::decimal::decimal::from_double(*amortization.value).value()) :
                std::nullopt;
        req.change.write.start_date = amortization.start_date;
        req.change.write.end_date = amortization.end_date;
        req.change.write.frequency = amortization.frequency;
        req.change.write.underflow = amortization.underflow;
        auto resp = nats_call(nats, req, error);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_issue_leg_amortization failed" : error;
    }

    if (auto failure = save_issue_schedule(nats, issue_id, leg_number, "schedule", leg.schedule);
        !failure.empty())
        return failure;
    if (auto failure = save_issue_schedule(
            nats, issue_id, leg_number, "payment_schedule", leg.payment_schedule);
        !failure.empty())
        return failure;

    if (!leg.payment_dates.empty()) {
        ores::trading::domain::bond_schedule_data dates;
        ores::trading::domain::bond_schedule_dates block;
        block.dates = leg.payment_dates;
        dates.dates.push_back(std::move(block));
        if (auto failure = save_issue_schedule(nats, issue_id, leg_number, "payment_dates", dates);
            !failure.empty())
            return failure;
    }

    return {};
}

/**
 * @brief Saves an option block: its row, its three child lists and the two
 * spellings of its exercise dates.
 *
 * Every product that states an option block writes the row, not only the
 * one whose fact row carries the type and the strike.
 *
 * The row carries a flag per optional sub-block, because a set of null
 * columns cannot say whether the document stated the sub-block and left
 * it bare or omitted it. An empty block writes nothing.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_option_block(Nats& nats,
                              const boost::uuids::uuid& trade_id,
                              const ores::trading::domain::bond_instrument_data& data) {
    using ores::trading::messaging::put_instrument_option_exercise_fee_request;
    using ores::trading::messaging::put_instrument_option_payment_date_request;
    using ores::trading::messaging::put_instrument_option_premium_request;
    using ores::trading::messaging::put_instrument_option_request;

    std::string error;
    if (data.option_data) {
        const auto& block = *data.option_data;
        put_instrument_option_request req;
        req.change.write.trade_id = trade_id;
        req.change.write.long_short = block.long_short;
        req.change.write.option_type = block.option_type;
        req.change.write.payoff_type = block.payoff_type;
        req.change.write.payoff_type_2 = block.payoff_type_2;
        req.change.write.style = block.style;
        req.change.write.notice_period = block.notice_period;
        req.change.write.notice_calendar = block.notice_calendar;
        req.change.write.notice_convention = block.notice_convention;
        req.change.write.mid_coupon_exercise = block.mid_coupon_exercise;
        req.change.write.settlement = block.settlement;
        req.change.write.settlement_method = block.settlement_method;
        req.change.write.pay_off_at_expiry = block.pay_off_at_expiry;
        req.change.write.premium_amount = block.premium_amount;
        req.change.write.premium_currency = block.premium_currency;
        req.change.write.premium_pay_date = block.premium_pay_date;
        req.change.write.exercise_prices = block.exercise_prices;
        req.change.write.exercise_fee_settlement_period = block.exercise_fee_settlement_period;
        req.change.write.exercise_fee_settlement_calendar = block.exercise_fee_settlement_calendar;
        req.change.write.exercise_fee_settlement_convention =
            block.exercise_fee_settlement_convention;
        req.change.write.automatic_exercise = block.automatic_exercise;

        req.change.write.has_exercise_data = block.exercise_data.has_value();
        if (block.exercise_data) {
            req.change.write.exercise_date = parse_date(block.exercise_data->date);
            req.change.write.exercise_price =
                block.exercise_data->price ?
                    std::optional(
                        ores::utility::decimal::decimal::from_double(*block.exercise_data->price)
                            .value()) :
                    std::nullopt;
        }

        req.change.write.has_payment_data = block.payment_data.has_value();
        if (block.payment_data && block.payment_data->rules) {
            const auto& rules = *block.payment_data->rules;
            req.change.write.payment_lag = static_cast<std::int64_t>(rules.lag);
            req.change.write.payment_calendar = rules.calendar;
            req.change.write.payment_convention = rules.convention;
            req.change.write.payment_relative_to = rules.relative_to;
        }

        req.change.write.has_settlement_data = block.settlement_data.has_value();
        if (block.settlement_data) {
            req.change.write.settlement_pay_currency = block.settlement_data->pay_currency;
            req.change.write.settlement_fx_index = block.settlement_data->fx_index;
            req.change.write.settlement_fixing_date = block.settlement_data->fixing_date;
        }

        auto resp = nats_call(nats, req, error);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_instrument_option failed" : error;

        int sequence_number = 0;
        for (const auto& premium : block.premiums) {
            put_instrument_option_premium_request child;
            child.change.write.trade_id = trade_id;
            child.change.write.sequence_number = ++sequence_number;
            child.change.write.amount =
                ores::utility::decimal::decimal::from_double(premium.amount).value();
            child.change.write.currency = premium.currency;
            child.change.write.pay_date =
                ores::platform::time::datetime::from_iso8601_date(premium.pay_date);
            child.change.write.has_settlement = premium.settlement.has_value();
            if (premium.settlement) {
                child.change.write.settlement_pay_currency = premium.settlement->pay_currency;
                child.change.write.settlement_fx_index = premium.settlement->fx_index;
                child.change.write.settlement_fixing_date = premium.settlement->fixing_date;
            }
            auto child_resp = nats_call(nats, child, error);
            if (!child_resp || child_resp->result.outcome != ores::utility::domain::outcome::ok)
                return error.empty() ? "save_instrument_option_premium failed" : error;
        }

        sequence_number = 0;
        for (const auto& fee : block.exercise_fees) {
            put_instrument_option_exercise_fee_request child;
            child.change.write.trade_id = trade_id;
            child.change.write.sequence_number = ++sequence_number;
            child.change.write.amount =
                ores::utility::decimal::decimal::from_double(fee.amount).value();
            child.change.write.type = fee.type;
            child.change.write.start_date = fee.start_date;
            child.change.write.currency = fee.currency;
            auto child_resp = nats_call(nats, child, error);
            if (!child_resp || child_resp->result.outcome != ores::utility::domain::outcome::ok)
                return error.empty() ? "save_instrument_option_exercise_fee failed" : error;
        }

        if (block.payment_data) {
            int payment_number = 0;
            for (const auto& date : block.payment_data->dates) {
                put_instrument_option_payment_date_request child;
                child.change.write.trade_id = trade_id;
                child.change.write.sequence_number = ++payment_number;
                child.change.write.payment_date =
                    ores::platform::time::datetime::from_iso8601_date(date);
                auto child_resp = nats_call(nats, child, error);
                if (!child_resp || child_resp->result.outcome != ores::utility::domain::outcome::ok)
                    return error.empty() ? "save_instrument_option_payment_date failed" : error;
            }
        }
    }

    if (!data.option_exercise_dates.empty()) {
        ores::trading::domain::bond_schedule_data schedule;
        ores::trading::domain::bond_schedule_dates dates;
        dates.dates = data.option_exercise_dates;
        schedule.dates.push_back(std::move(dates));
        if (auto failure = save_schedule(nats, trade_id, "option", 1, "exercise_dates", schedule);
            !failure.empty())
            return failure;
    }

    if (data.option_exercise_schedule) {
        if (auto failure = save_schedule(
                nats, trade_id, "option", 1, "exercise_schedule", *data.option_exercise_schedule);
            !failure.empty())
            return failure;
    }

    return {};
}

/**
 * @brief Saves the strike group, which the option row has no column for.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_strike(Nats& nats,
                        const boost::uuids::uuid& trade_id,
                        const ores::trading::domain::bond_strike_data& strike) {
    using ores::trading::messaging::put_instrument_strike_request;

    put_instrument_strike_request req;
    req.change.write.trade_id = trade_id;
    req.change.write.price_value =
        strike.price_value ?
            std::optional(
                ores::utility::decimal::decimal::from_double(*strike.price_value).value()) :
            std::nullopt;
    req.change.write.price_currency = strike.price_currency;
    req.change.write.yield_value = strike.yield_value;
    req.change.write.yield_compounding = strike.yield_compounding;
    req.change.write.bare_value =
        strike.bare_value ?
            std::optional(
                ores::utility::decimal::decimal::from_double(*strike.bare_value).value()) :
            std::nullopt;
    req.change.write.bare_currency = strike.bare_currency;

    std::string error;
    auto resp = nats_call(nats, req, error);
    if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
        return error.empty() ? "save_instrument_strike failed" : error;
    return {};
}

/**
 * @brief Saves the forward terms the fact row has no column for.
 *
 * A container that states no forward member writes no row, which is what
 * a container built from a row set holds.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_forward(Nats& nats,
                         const boost::uuids::uuid& trade_id,
                         const ores::trading::domain::bond_instrument_data& data) {
    using ores::trading::messaging::put_bond_forward_request;

    if (!data.forward_long_in_forward && !data.forward_settlement && !data.forward_premium)
        return {};

    put_bond_forward_request req;
    req.change.write.trade_id = trade_id;
    req.change.write.long_in_forward = data.forward_long_in_forward;
    if (data.forward_settlement) {
        const auto& settlement = *data.forward_settlement;
        req.change.write.forward_maturity_date = settlement.forward_maturity_date;
        req.change.write.forward_settlement_date = settlement.forward_settlement_date;
        req.change.write.settlement = settlement.settlement;
        req.change.write.amount =
            settlement.amount ?
                std::optional(
                    ores::utility::decimal::decimal::from_double(*settlement.amount).value()) :
                std::nullopt;
        req.change.write.lock_rate = settlement.lock_rate;
        req.change.write.dv01 =
            settlement.dv01 ?
                std::optional(
                    ores::utility::decimal::decimal::from_double(*settlement.dv01).value()) :
                std::nullopt;
        req.change.write.lock_rate_day_counter = settlement.lock_rate_day_counter;
        req.change.write.settlement_dirty = settlement.settlement_dirty;
    }
    if (data.forward_premium) {
        req.change.write.premium_amount = data.forward_premium->amount;
        req.change.write.premium_date = data.forward_premium->date;
    }

    std::string error;
    auto resp = nats_call(nats, req, error);
    if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
        return error.empty() ? "save_bond_forward failed" : error;
    return {};
}

/**
 * @brief Saves one bond instrument: its issue row, its header row, the
 * issue's child rows, its legs, its option block and the product's fact row.
 *
 * The issue row comes first because both the header and the child rows
 * reference it. A document that stated no call dates and no conversion
 * targets writes none of either, and a trade type with no product row
 * writes none.
 *
 * @param issue_ids_by_security The stored issue identifiers, keyed by
 * security id. A miss mints a row and records it here for the trades that
 * follow.
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string
save_bond_instrument(Nats& nats,
                     const ores::trading::domain::bond_instrument_data& data,
                     std::unordered_map<std::string, std::string>& issue_ids_by_security) {
    using ores::trading::messaging::put_ascot_request;
    using ores::trading::messaging::put_bond_future_request;
    using ores::trading::messaging::put_bond_instrument_request;
    using ores::trading::messaging::put_bond_issue_call_date_request;
    using ores::trading::messaging::put_bond_issue_conversion_target_request;
    using ores::trading::messaging::put_bond_issue_request;
    using ores::trading::messaging::put_bond_option_request;
    using ores::trading::messaging::put_bond_repo_request;
    using ores::trading::messaging::put_bond_trs_request;

    std::string error;
    auto instrument = data.instrument;
    auto issue = data.issue;

    const auto found = issue_ids_by_security.find(issue.security_id);
    const bool issue_is_new = found == issue_ids_by_security.end();
    if (!issue_is_new) {
        issue.issue_id = boost::lexical_cast<boost::uuids::uuid>(found->second);
        instrument.issue_id = issue.issue_id;
    }

    // A failed save leaves the security id out of the map, so the next
    // trade of it tries the issue again rather than opening an instrument
    // against a row that is not there.
    if (issue_is_new) {
        put_bond_issue_request issue_req;
        issue_req.change.write.issue_id = issue.issue_id;
        issue_req.change.write.security_id = issue.security_id;
        issue_req.change.write.issuer = issue.issuer;
        issue_req.change.write.face_value = issue.face_value;
        issue_req.change.write.issue_date = issue.issue_date;
        issue_req.change.write.settlement_days = issue.settlement_days;
        issue_req.change.write.calendar = issue.calendar;
        issue_req.change.write.credit_curve_id = issue.credit_curve_id;
        issue_req.change.write.reference_curve_id = issue.reference_curve_id;
        issue_req.change.write.income_curve_id = issue.income_curve_id;
        issue_req.change.write.credit_group = issue.credit_group;
        issue_req.change.write.volatility_curve_id = issue.volatility_curve_id;
        issue_req.change.write.price_quote_method = issue.price_quote_method;
        issue_req.change.write.price_quote_base_value = issue.price_quote_base_value;
        issue_req.change.write.sub_type = issue.sub_type;
        issue_req.change.write.price_type = issue.price_type;
        issue_req.change.write.payer = issue.payer;
        issue_req.change.write.credit_risk = issue.credit_risk;
        auto resp = nats_call(nats, issue_req, error);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_issue failed" : error;
        issue_ids_by_security[issue.security_id] = boost::uuids::to_string(issue.issue_id);
    }

    put_bond_instrument_request instrument_req;
    instrument_req.change.write.trade_id = instrument.identity.trade_id;
    instrument_req.change.write.trade_type_code = instrument.identity.trade_type_code;
    instrument_req.change.write.issue_id = instrument.issue_id;
    instrument_req.change.write.notional = instrument.notional;
    auto resp = nats_call(nats, instrument_req, error);
    if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
        return error.empty() ? "save_bond_instrument failed" : error;

    int sequence_number = 0;
    for (const auto& call_date : data.call_dates) {
        put_bond_issue_call_date_request child_req;
        child_req.change.write.issue_id = call_date.issue_id;
        child_req.change.write.sequence_number = call_date.sequence_number;
        child_req.change.write.call_date = call_date.call_date;
        child_req.change.write.issue_id = issue.issue_id;
        child_req.change.write.sequence_number = ++sequence_number;
        auto child_resp = nats_call(nats, child_req, error);
        if (!child_resp || child_resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_issue_call_date failed" : error;
    }

    sequence_number = 0;
    for (const auto& target : data.conversion_targets) {
        put_bond_issue_conversion_target_request child_req;
        child_req.change.write.issue_id = target.issue_id;
        child_req.change.write.sequence_number = target.sequence_number;
        child_req.change.write.underlying_id = target.underlying_id;
        child_req.change.write.conversion_ratio = target.conversion_ratio;
        child_req.change.write.issue_id = issue.issue_id;
        child_req.change.write.sequence_number = ++sequence_number;
        auto child_resp = nats_call(nats, child_req, error);
        if (!child_resp || child_resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_issue_conversion_target failed" : error;
    }

    const auto trade_id = instrument.identity.trade_id;

    // The security owns its legs, so they are written once, when the
    // issue row is minted: a second trade on the same ISIN would collide
    // with the first trade's rows, because a leg's parent is the issue.
    // The three product legs below are the trade's own and are always
    // written.
    if (issue_is_new) {
        int leg_number = 0;
        for (const auto& leg : data.bond_legs) {
            if (auto failure = save_issue_leg(nats, issue.issue_id, ++leg_number, leg);
                !failure.empty())
                return failure;
        }
    }
    if (auto failure = save_leg(nats, trade_id, "trs_funding", 1, data.trs_funding_leg);
        !failure.empty())
        return failure;
    if (auto failure = save_leg(nats, trade_id, "repo", 1, data.repo_leg); !failure.empty())
        return failure;
    if (auto failure = save_leg(nats, trade_id, "ascot_swap", 1, data.ascot_swap_leg);
        !failure.empty())
        return failure;

    if (auto failure = save_option_block(nats, trade_id, data); !failure.empty())
        return failure;

    if (data.strike_data) {
        if (auto failure = save_strike(nats, trade_id, *data.strike_data); !failure.empty())
            return failure;
    }

    if (auto failure = save_forward(nats, trade_id, data); !failure.empty())
        return failure;

    if (auto failure = save_schedule(nats, trade_id, "trs", 1, "schedule", data.trs_schedule);
        !failure.empty())
        return failure;

    const auto& ttc = instrument.identity.trade_type_code;
    if (ttc == "BondOption" && data.option) {
        put_bond_option_request fact_req;
        fact_req.change.write.trade_id = (*data.option).trade_id;
        fact_req.change.write.option_type = (*data.option).option_type;
        fact_req.change.write.option_strike = (*data.option).option_strike;
        fact_req.change.write.redemption = (*data.option).redemption;
        fact_req.change.write.price_type = (*data.option).price_type;
        fact_req.change.write.knocks_out = (*data.option).knocks_out;
        fact_req.change.write.trade_id = instrument.identity.trade_id;
        fact_req.change.write.redemption = data.option_redemption;
        fact_req.change.write.price_type = data.option_price_type;
        fact_req.change.write.knocks_out = data.option_knocks_out;
        auto fact_resp = nats_call(nats, fact_req, error);
        if (!fact_resp || fact_resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_option failed" : error;
    } else if (ttc == "BondTRS" && data.trs) {
        put_bond_trs_request fact_req;
        fact_req.change.write.trade_id = (*data.trs).trade_id;
        fact_req.change.write.return_type = (*data.trs).return_type;
        fact_req.change.write.funding_leg_type = (*data.trs).funding_leg_type;
        fact_req.change.write.funding_rate = (*data.trs).funding_rate;
        fact_req.change.write.funding_index = (*data.trs).funding_index;
        fact_req.change.write.payer = (*data.trs).payer;
        fact_req.change.write.price_type = (*data.trs).price_type;
        fact_req.change.write.initial_price = (*data.trs).initial_price;
        fact_req.change.write.trade_id = instrument.identity.trade_id;
        fact_req.change.write.payer = data.trs_payer;
        fact_req.change.write.initial_price =
            data.trs_initial_price ?
                std::optional(
                    ores::utility::decimal::decimal::from_double(*data.trs_initial_price).value()) :
                std::nullopt;
        if (!data.trs_price_type.empty())
            fact_req.change.write.price_type = data.trs_price_type;
        auto fact_resp = nats_call(nats, fact_req, error);
        if (!fact_resp || fact_resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_trs failed" : error;
    } else if (ttc == "BondRepo" && data.repo) {
        put_bond_repo_request fact_req;
        fact_req.change.write.trade_id = (*data.repo).trade_id;
        fact_req.change.write.repo_type = (*data.repo).repo_type;
        fact_req.change.write.repo_rate = (*data.repo).repo_rate;
        fact_req.change.write.repo_index = (*data.repo).repo_index;
        fact_req.change.write.trade_id = instrument.identity.trade_id;
        auto fact_resp = nats_call(nats, fact_req, error);
        if (!fact_resp || fact_resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_repo failed" : error;
    } else if (ttc == "BondFuture" && data.future) {
        put_bond_future_request fact_req;
        fact_req.change.write.trade_id = instrument.identity.trade_id;
        fact_req.change.write.contract_name = (*data.future).contract_name;
        fact_req.change.write.contract_notional = (*data.future).contract_notional;
        fact_req.change.write.long_short = (*data.future).long_short;
        fact_req.change.write.apply_conversion_factor = (*data.future).apply_conversion_factor;
        fact_req.change.write.use_future_price = (*data.future).use_future_price;
        auto fact_resp = nats_call(nats, fact_req, error);
        if (!fact_resp || fact_resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_bond_future failed" : error;
    } else if (ttc == "Ascot" && data.ascot_row) {
        put_ascot_request fact_req;
        fact_req.change.write.trade_id = (*data.ascot_row).trade_id;
        fact_req.change.write.ascot_option_type = (*data.ascot_row).ascot_option_type;
        fact_req.change.write.trade_id = instrument.identity.trade_id;
        auto fact_resp = nats_call(nats, fact_req, error);
        if (!fact_resp || fact_resp->result.outcome != ores::utility::domain::outcome::ok)
            return error.empty() ? "save_ascot failed" : error;
    }

    return {};
}

} // namespace

ore_import_execute_handler::ore_import_execute_handler(
    ores::nats::service::client& nats,
    ores::nats::service::nats_client outbound_nats,
    std::string http_base_url,
    std::string work_dir)
    : nats_(nats)
    , outbound_nats_(std::move(outbound_nats))
    , http_base_url_(std::move(http_base_url))
    , work_dir_(std::move(work_dir)) {}

void ore_import_execute_handler::execute(ores::nats::message msg) {
    using ores::ore::messaging::ore_import_execute_request;
    using ores::ore::messaging::ore_import_execute_result;

    const auto step_id = extract_workflow_header(msg, ores::workflow::messaging::step_id_header);
    const auto inst_id =
        extract_workflow_header(msg, ores::workflow::messaging::instance_id_header);

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<ore_import_execute_request>(sv);
    if (!parsed) {
        BOOST_LOG_SEV(lg(), error)
            << "ore.import.execute: failed to decode request | step=" << step_id;
        publish_step_completion(nats_,
                                step_id,
                                inst_id,
                                ores::workflow::messaging::step_outcome::failed,
                                "",
                                "Failed to decode ore_import_execute_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute starting | corr=" << req.correlation_id
                              << " request_id=" << req.request_id << " step=" << step_id;

    auto delegated_nats =
        outbound_nats_.with_delegation(req.bearer_token).with_correlation_id(req.correlation_id);

    ore_import_execute_result result;
    result.correlation_id = req.correlation_id;

    // -------------------------------------------------------------------------
    // Step 0: fetch tarball from storage and extract to work directory
    // -------------------------------------------------------------------------
    const auto import_dir = work_dir_ / req.request_id;
    BOOST_LOG_SEV(lg(), debug) << "ore.import.execute step 0: fetch_and_unpack | corr="
                               << req.correlation_id
                               << " bucket=" << ores::ore::net::ore_storage::bucket
                               << " key=" << ores::ore::net::ore_storage::import_key(req.request_id)
                               << " dest=" << import_dir.string();

    try {
        std::filesystem::create_directories(import_dir);
        ores::storage::net::storage_transfer transfer(http_base_url_, req.bearer_token);
        transfer.fetch_and_unpack(std::string(ores::ore::net::ore_storage::bucket),
                                  ores::ore::net::ore_storage::import_key(req.request_id),
                                  import_dir);
    } catch (const std::exception& e) {
        const auto failure = std::format("fetch_and_unpack failed: {}", e.what());
        BOOST_LOG_SEV(lg(), error)
            << "ore.import.execute step 0 failed | corr=" << req.correlation_id
            << " error=" << failure;
        publish_step_completion(
            nats_, step_id, inst_id, ores::workflow::messaging::step_outcome::failed, "", failure);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 0 complete | corr=" << req.correlation_id
                              << " import_dir=" << import_dir.string();

    // -------------------------------------------------------------------------
    // Step 1: scan the extracted directory
    // -------------------------------------------------------------------------
    ores::ore::scanner::scan_result scan;
    try {
        ores::ore::scanner::ore_directory_scanner scanner(import_dir);
        scan = scanner.scan();
    } catch (const std::exception& e) {
        const auto failure = std::format("Directory scan failed: {}", e.what());
        BOOST_LOG_SEV(lg(), error)
            << "ore.import.execute step 1 failed | corr=" << req.correlation_id
            << " error=" << failure;
        publish_step_completion(
            nats_, step_id, inst_id, ores::workflow::messaging::step_outcome::failed, "", failure);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 1 complete | corr=" << req.correlation_id
                              << " currency_files=" << scan.currency_files.size()
                              << " portfolio_files=" << scan.portfolio_files.size()
                              << " ignored_files=" << scan.ignored_files.size();

    // -------------------------------------------------------------------------
    // Step 2: list existing currency ISO codes
    // -------------------------------------------------------------------------
    std::set<std::string> existing_iso_codes;
    {
        ores::refdata::messaging::list_currencies_request list_req;
        constexpr int max_currency_fetch = 10'000;
        list_req.offset = 0;
        list_req.limit = max_currency_fetch;
        std::string err;
        auto list_resp = nats_call(delegated_nats, list_req, err);
        if (!list_resp) {
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.execute step 2 failed | corr=" << req.correlation_id
                << " error=" << err;
            publish_step_completion(
                nats_, step_id, inst_id, ores::workflow::messaging::step_outcome::failed, "", err);
            return;
        }
        for (const auto& c : list_resp->currencies)
            existing_iso_codes.insert(c.iso_code);
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 2 complete | corr=" << req.correlation_id
                              << " existing_iso_codes=" << existing_iso_codes.size();

    // -------------------------------------------------------------------------
    // Step 3: build the import plan
    // -------------------------------------------------------------------------
    ores::ore::planner::import_choices choices;
    if (!req.import_choices_json.empty()) {
        auto parsed_choices =
            rfl::json::read<ores::ore::planner::import_choices>(req.import_choices_json);
        if (!parsed_choices) {
            const auto failure = std::format("Failed to parse import_choices_json: {}",
                                             parsed_choices.error().what());
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.execute step 3 failed | corr=" << req.correlation_id
                << " error=" << failure;
            publish_step_completion(nats_,
                                    step_id,
                                    inst_id,
                                    ores::workflow::messaging::step_outcome::failed,
                                    "",
                                    failure);
            return;
        }
        choices = std::move(*parsed_choices);
    }

    ores::ore::planner::ore_import_plan plan;
    try {
        ores::ore::planner::ore_import_planner planner(
            std::move(scan), std::move(existing_iso_codes), std::move(choices));
        plan = planner.plan();
    } catch (const std::exception& e) {
        const auto failure = std::format("Import plan failed: {}", e.what());
        BOOST_LOG_SEV(lg(), error)
            << "ore.import.execute step 3 failed | corr=" << req.correlation_id
            << " error=" << failure;
        publish_step_completion(
            nats_, step_id, inst_id, ores::workflow::messaging::step_outcome::failed, "", failure);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 3 complete | corr=" << req.correlation_id
                              << " currencies=" << plan.currencies.size()
                              << " portfolios=" << plan.portfolios.size()
                              << " books=" << plan.books.size() << " trades=" << plan.trades.size();

    // -------------------------------------------------------------------------
    // Step 4: save currencies
    // -------------------------------------------------------------------------
    for (auto& currency : plan.currencies) {
        const auto iso = currency.iso_code;
        ores::refdata::messaging::put_currency_request save_req{
            .change = {.write = to_write(currency)},
            .intent = ores::utility::domain::change_intent{
                .reason_code = "system.import", .commentary = "Imported from an ORE directory"}};
        std::string err;
        auto resp = nats_call(delegated_nats, save_req, err);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok) {
            const auto failure = err.empty() ?
                                     std::format("save_currency failed for {}: {}",
                                                 iso,
                                                 resp ? resp->result.message : "(no response)") :
                                     err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.execute step 4 failed | corr=" << req.correlation_id
                << " iso_code=" << iso << " error=" << failure;
            publish_step_completion(nats_,
                                    step_id,
                                    inst_id,
                                    ores::workflow::messaging::step_outcome::failed,
                                    "",
                                    failure);
            return;
        }
        result.saved_currency_iso_codes.push_back(iso);
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 4 complete | corr=" << req.correlation_id
                              << " saved=" << result.saved_currency_iso_codes.size();

    // -------------------------------------------------------------------------
    // Step 5: save portfolios (parents-first from planner)
    // -------------------------------------------------------------------------
    for (auto& portfolio : plan.portfolios) {
        const auto pid = boost::uuids::to_string(portfolio.id);
        const auto name = portfolio.name;
        ores::refdata::messaging::put_portfolio_request save_req{
            .change = {.write = to_write(portfolio)},
            .intent = ores::utility::domain::change_intent{
                .reason_code = "system.import", .commentary = "Imported from an ORE directory"}};
        std::string err;
        auto resp = nats_call(delegated_nats, save_req, err);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok) {
            const auto failure = err.empty() ?
                                     std::format("save_portfolio failed for '{}' ({}): {}",
                                                 name,
                                                 pid,
                                                 resp ? resp->result.message : "(no response)") :
                                     err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.execute step 5 failed | corr=" << req.correlation_id
                << " portfolio=" << pid << " error=" << failure;
            publish_step_completion(nats_,
                                    step_id,
                                    inst_id,
                                    ores::workflow::messaging::step_outcome::failed,
                                    "",
                                    failure);
            return;
        }
        result.saved_portfolio_names.push_back(name);
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 5 complete | corr=" << req.correlation_id
                              << " saved=" << result.saved_portfolio_names.size();

    // -------------------------------------------------------------------------
    // Step 6: save books
    // -------------------------------------------------------------------------
    for (auto& book : plan.books) {
        const auto bid = boost::uuids::to_string(book.id);
        const auto name = book.name;
        ores::refdata::messaging::put_book_request save_req{
            .change = {.write = to_write(book)},
            .intent = ores::utility::domain::change_intent{
                .reason_code = "system.import", .commentary = "Imported from an ORE directory"}};
        std::string err;
        auto resp = nats_call(delegated_nats, save_req, err);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok) {
            const auto failure = err.empty() ?
                                     std::format("save_book failed for '{}' ({}): {}",
                                                 name,
                                                 bid,
                                                 resp ? resp->result.message : "(no response)") :
                                     err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.execute step 6 failed | corr=" << req.correlation_id
                << " book=" << bid << " error=" << failure;
            publish_step_completion(nats_,
                                    step_id,
                                    inst_id,
                                    ores::workflow::messaging::step_outcome::failed,
                                    "",
                                    failure);
            return;
        }
        result.saved_book_names.push_back(name);
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 6 complete | corr=" << req.correlation_id
                              << " saved=" << result.saved_book_names.size();

    // -------------------------------------------------------------------------
    // Step 7: save trades (failures collected; saga continues)
    // -------------------------------------------------------------------------
    for (auto& item : plan.trades) {
        const auto trade_id = item.trade.identity.id;
        const auto tid = boost::uuids::to_string(trade_id);
        const auto src = item.source_file.string();
        const auto ext_id = item.trade.identity.external_id;

        // The trade's declared key is required, so an ORE trade with no id
        // cannot be written. Report it as an item error rather than saving a
        // row no read by key can find.
        if (ext_id.empty()) {
            const auto failure = std::string("Trade has no external id: the ORE trade id is "
                                             "required.");
            BOOST_LOG_SEV(lg(), warn)
                << "ore.import.execute trade save failed | corr=" << req.correlation_id
                << " trade_id=" << tid << " source=" << src << " error=" << failure;
            result.item_errors.push_back({.source_file = src, .item_id = tid, .message = failure});
            continue;
        }

        // The envelope names the counterparty in the document's own words. A
        // name that resolves to no counterparty rejects the trade before
        // anything is written; the import's default applies only when the
        // envelope names none.
        auto counterparty_id = item.trade.parties.counterparty_id;
        const auto& envelope = item.envelope;
        if (envelope && envelope->counter_party && !envelope->counter_party->empty()) {
            std::string resolve_error;
            const auto resolved =
                resolve_counterparty(delegated_nats, *envelope->counter_party, resolve_error);
            if (!resolved) {
                const auto failure =
                    resolve_error.empty() ?
                        std::format("Counterparty {} matches no counterparty: give one an ORE "
                                    "identifier with this name.",
                                    *envelope->counter_party) :
                        resolve_error;
                BOOST_LOG_SEV(lg(), warn)
                    << "ore.import.execute counterparty unresolved | corr=" << req.correlation_id
                    << " trade_id=" << tid << " source=" << src << " error=" << failure;
                result.item_errors.push_back(
                    {.source_file = src, .item_id = ext_id, .message = failure});
                continue;
            }
            counterparty_id = resolved;
        }

        std::optional<boost::uuids::uuid> netting_set_id;
        if (envelope && envelope->netting_set_id && !envelope->netting_set_id->empty()) {
            std::string netting_error;
            netting_set_id =
                resolve_netting_set(delegated_nats, *envelope->netting_set_id, netting_error);
            if (!netting_set_id) {
                const auto failure =
                    netting_error.empty() ?
                        std::format("NettingSetId {} matches no netting set: give one an ORE "
                                    "identifier with this name.",
                                    *envelope->netting_set_id) :
                        netting_error;
                BOOST_LOG_SEV(lg(), warn)
                    << "ore.import.execute netting set unresolved | corr=" << req.correlation_id
                    << " trade_id=" << tid << " source=" << src << " error=" << failure;
                result.item_errors.push_back(
                    {.source_file = src, .item_id = ext_id, .message = failure});
                continue;
            }
        }

        // The booking is written first: it checks the trade's book, counterparty
        // and netting set against one another, so a trade it refuses leaves
        // nothing behind.
        const auto booking_error = book_imported_trade(
            delegated_nats, item.trade, counterparty_id, netting_set_id, item.envelope);
        if (!booking_error.empty()) {
            BOOST_LOG_SEV(lg(), warn)
                << "ore.import.execute trade booking failed | corr=" << req.correlation_id
                << " trade_id=" << tid << " source=" << src << " error=" << booking_error;
            result.item_errors.push_back(
                {.source_file = src, .item_id = ext_id, .message = booking_error});
            continue;
        }

        ores::trading::messaging::put_many_trades_request save_req;
        {
            ores::trading::messaging::trade_change change;
            const auto& trade = item.trade;
            change.write.id = trade.identity.id;
            change.write.external_id = trade.identity.external_id;
            change.write.book_id = trade.parties.book_id;
            change.write.portfolio_id = trade.parties.portfolio_id;
            change.write.successor_trade_id = trade.parties.successor_trade_id;
            change.write.trade_type = trade.classification.trade_type;
            change.write.counterparty_id = counterparty_id;
            change.write.product_type =
                ores::trading::domain::to_string(trade.classification.product_type);
            change.write.asset_class = trade.classification.asset_class;
            change.write.netting_set_id = trade.classification.netting_set_id;
            change.write.activity_type_code = trade.classification.activity_type_code;
            change.write.status_id = trade.classification.status_id;
            change.write.trade_date = iso_or_empty(trade.lifecycle.trade_date);
            change.write.execution_timestamp = iso_or_empty(trade.lifecycle.execution_timestamp);
            change.write.effective_date = iso_or_empty(trade.lifecycle.effective_date);
            change.write.termination_date = iso_or_empty(trade.lifecycle.termination_date);
            save_req.changes.push_back(std::move(change));
        }

        std::string trade_error;
        auto resp = nats_call(delegated_nats, save_req, trade_error);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok) {
            const auto trade_msg = resp ? resp->result.message : trade_error;
            BOOST_LOG_SEV(lg(), warn)
                << "ore.import.execute trade save failed | corr=" << req.correlation_id
                << " trade_id=" << tid << " source=" << src << " error=" << trade_msg;
            result.item_errors.push_back(
                {.source_file = src, .item_id = ext_id, .message = trade_msg});
        } else {
            result.saved_trade_external_ids.push_back(ext_id);
            const auto envelope_error = save_envelope(delegated_nats, trade_id, item.envelope);
            if (!envelope_error.empty()) {
                BOOST_LOG_SEV(lg(), warn)
                    << "ore.import.execute envelope save failed | corr=" << req.correlation_id
                    << " trade_id=" << tid << " source=" << src << " error=" << envelope_error;
                result.item_errors.push_back(
                    {.source_file = src, .item_id = ext_id, .message = envelope_error});
            }
        }
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 7 complete | corr=" << req.correlation_id
                              << " saved=" << result.saved_trade_external_ids.size()
                              << " failed=" << result.item_errors.size();

    // -------------------------------------------------------------------------
    // Step 8: save instruments (non-fatal — collect errors, continue)
    // -------------------------------------------------------------------------
    int instruments_saved = 0;
    std::unordered_map<std::string, std::string> issue_ids_by_security;
    bool bond_issues_loaded = false;
    const std::unordered_set<std::string> saved_trades(result.saved_trade_external_ids.begin(),
                                                       result.saved_trade_external_ids.end());
    for (const auto& item : plan.trades) {
        if (!saved_trades.contains(item.trade.identity.external_id))
            continue;
        using namespace ores::trading::messaging;
        using ores::trading::domain::swap_instrument_data;
        using ores::trading::domain::fx_instrument_variant;
        using ores::trading::domain::bond_instrument_data;
        using ores::trading::domain::credit_instrument;
        using ores::trading::domain::equity_instrument_data;
        using ores::trading::domain::commodity_instrument_data;
        using ores::trading::domain::composite_instrument_data;
        using ores::trading::domain::scripted_instrument;
        std::string instr_error;

        const auto save_ok = std::visit(
            [&](const auto& r) -> bool {
                using T = std::decay_t<decltype(r)>;
                if constexpr (std::is_same_v<T, std::monostate>) {
                    return true; // no instrument for this trade type
                } else if constexpr (std::is_same_v<T, swap_instrument_data>) {
                    return std::visit(
                        [&](const auto& instr) -> bool {
                            using InstrT = std::decay_t<decltype(instr)>;
                            using namespace ores::trading::domain;
                            if constexpr (std::is_same_v<InstrT, fra_instrument>) {
                                put_fra_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.start_date = instr.start_date;
                                req.change.write.end_date = instr.end_date;
                                req.change.write.currency = instr.currency;
                                req.change.write.rate_index = instr.rate_index;
                                req.change.write.long_short = instr.long_short;
                                req.change.write.strike = instr.strike;
                                req.change.write.notional = instr.notional;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT, vanilla_swap_instrument>) {
                                put_vanilla_swap_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.start_date = instr.start_date;
                                req.change.write.maturity_date = instr.maturity_date;
                                req.change.write.settlement_lag = instr.settlement_lag;
                                req.change.write.netting_set_id = instr.netting_set_id;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT, cap_floor_instrument>) {
                                put_cap_floor_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.start_date = instr.start_date;
                                req.change.write.maturity_date = instr.maturity_date;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT, swaption_instrument>) {
                                put_swaption_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.expiry_date = instr.expiry_date;
                                req.change.write.exercise_type = instr.exercise_type;
                                req.change.write.settlement_type = instr.settlement_type;
                                req.change.write.long_short = instr.long_short;
                                req.change.write.start_date = instr.start_date;
                                req.change.write.maturity_date = instr.maturity_date;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<
                                                     InstrT,
                                                     balance_guaranteed_swap_instrument>) {
                                put_balance_guaranteed_swap_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.start_date = instr.start_date;
                                req.change.write.maturity_date = instr.maturity_date;
                                req.change.write.lockout_days = instr.lockout_days;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT, callable_swap_instrument>) {
                                put_callable_swap_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.start_date = instr.start_date;
                                req.change.write.maturity_date = instr.maturity_date;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                if (!resp ||
                                    resp->result.outcome != ores::utility::domain::outcome::ok)
                                    return false;
                                int sequence_number = 0;
                                for (const auto& call_date : r.call_dates) {
                                    put_callable_swap_call_date_request date_req;
                                    date_req.change.write.trade_id = instr.identity.trade_id;
                                    date_req.change.write.sequence_number = ++sequence_number;
                                    date_req.change.write.call_date = call_date.call_date;
                                    auto date_resp =
                                        nats_call(delegated_nats, date_req, instr_error);
                                    if (!date_resp || date_resp->result.outcome !=
                                                          ores::utility::domain::outcome::ok)
                                        return false;
                                }
                                return true;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                knock_out_swap_instrument>) {
                                put_knock_out_swap_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.start_date = instr.start_date;
                                req.change.write.maturity_date = instr.maturity_date;
                                req.change.write.barrier_start_date = instr.barrier_start_date;
                                req.change.write.barrier_level = instr.barrier_level;
                                req.change.write.barrier_type = instr.barrier_type;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                inflation_swap_instrument>) {
                                put_inflation_swap_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.start_date = instr.start_date;
                                req.change.write.maturity_date = instr.maturity_date;
                                req.change.write.inflation_index_code = instr.inflation_index_code;
                                req.change.write.base_cpi = instr.base_cpi;
                                req.change.write.lag_convention = instr.lag_convention;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT, rpa_instrument>) {
                                put_rpa_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.start_date = instr.start_date;
                                req.change.write.maturity_date = instr.maturity_date;
                                req.change.write.reference_counterparty =
                                    instr.reference_counterparty;
                                req.change.write.participation_rate = instr.participation_rate;
                                req.change.write.protection_fee = instr.protection_fee;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else {
                                return true;
                            }
                        },
                        r.instrument);
                } else if constexpr (std::is_same_v<T, fx_instrument_variant>) {
                    return std::visit(
                        [&](const auto& instr) -> bool {
                            using InstrT = std::decay_t<decltype(instr)>;
                            using namespace ores::trading::domain;
                            if constexpr (std::is_same_v<InstrT, fx_forward_instrument>) {
                                put_fx_forward_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.bought_currency = instr.bought_currency;
                                req.change.write.bought_amount = instr.bought_amount;
                                req.change.write.sold_currency = instr.sold_currency;
                                req.change.write.sold_amount = instr.sold_amount;
                                req.change.write.value_date = instr.value_date;
                                req.change.write.settlement = instr.settlement;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                fx_vanilla_option_instrument>) {
                                put_fx_vanilla_option_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.bought_currency = instr.bought_currency;
                                req.change.write.bought_amount = instr.bought_amount;
                                req.change.write.sold_currency = instr.sold_currency;
                                req.change.write.sold_amount = instr.sold_amount;
                                req.change.write.option_type = instr.option_type;
                                req.change.write.expiry_date = instr.expiry_date;
                                req.change.write.exercise_style = instr.exercise_style;
                                req.change.write.settlement = instr.settlement;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                fx_barrier_option_instrument>) {
                                put_fx_barrier_option_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.bought_currency = instr.bought_currency;
                                req.change.write.bought_amount = instr.bought_amount;
                                req.change.write.sold_currency = instr.sold_currency;
                                req.change.write.sold_amount = instr.sold_amount;
                                req.change.write.option_type = instr.option_type;
                                req.change.write.expiry_date = instr.expiry_date;
                                req.change.write.settlement = instr.settlement;
                                req.change.write.barrier_type = instr.barrier_type;
                                req.change.write.lower_barrier = instr.lower_barrier;
                                req.change.write.upper_barrier = instr.upper_barrier;
                                req.change.write.underlying_code = instr.underlying_code;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                fx_digital_option_instrument>) {
                                put_fx_digital_option_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.foreign_currency = instr.foreign_currency;
                                req.change.write.domestic_currency = instr.domestic_currency;
                                req.change.write.payoff_currency = instr.payoff_currency;
                                req.change.write.payoff_amount = instr.payoff_amount;
                                req.change.write.option_type = instr.option_type;
                                req.change.write.expiry_date = instr.expiry_date;
                                req.change.write.long_short = instr.long_short;
                                req.change.write.strike = instr.strike;
                                req.change.write.barrier_type = instr.barrier_type;
                                req.change.write.lower_barrier = instr.lower_barrier;
                                req.change.write.upper_barrier = instr.upper_barrier;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                fx_asian_forward_instrument>) {
                                put_fx_asian_forward_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.fx_index = instr.fx_index;
                                req.change.write.reference_currency = instr.reference_currency;
                                req.change.write.reference_notional = instr.reference_notional;
                                req.change.write.settlement_currency = instr.settlement_currency;
                                req.change.write.settlement_notional = instr.settlement_notional;
                                req.change.write.payment_date = instr.payment_date;
                                req.change.write.long_short = instr.long_short;
                                req.change.write.currency = instr.currency;
                                req.change.write.fixing_amount = instr.fixing_amount;
                                req.change.write.target_amount = instr.target_amount;
                                req.change.write.strike = instr.strike;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                fx_accumulator_instrument>) {
                                put_fx_accumulator_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.currency = instr.currency;
                                req.change.write.fixing_amount = instr.fixing_amount;
                                req.change.write.strike = instr.strike;
                                req.change.write.underlying_code = instr.underlying_code;
                                req.change.write.long_short = instr.long_short;
                                req.change.write.start_date = instr.start_date;
                                req.change.write.knock_out_barrier = instr.knock_out_barrier;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                fx_variance_swap_instrument>) {
                                put_fx_variance_swap_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.start_date = instr.start_date;
                                req.change.write.end_date = instr.end_date;
                                req.change.write.currency = instr.currency;
                                req.change.write.underlying_code = instr.underlying_code;
                                req.change.write.long_short = instr.long_short;
                                req.change.write.strike = instr.strike;
                                req.change.write.notional = instr.notional;
                                req.change.write.moment_type = instr.moment_type;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else {
                                return true;
                            }
                        },
                        r);
                } else if constexpr (std::is_same_v<T, bond_instrument_data>) {
                    if (!bond_issues_loaded) {
                        bond_issues_loaded = true;
                        issue_ids_by_security =
                            read_bond_issue_ids_by_security(delegated_nats, instr_error);
                        if (!instr_error.empty())
                            return false;
                    }
                    instr_error = save_bond_instrument(delegated_nats, r, issue_ids_by_security);
                    return instr_error.empty();
                } else if constexpr (std::is_same_v<T, credit_instrument>) {
                    put_credit_instrument_request req;
                    req.change.write.trade_id = r.identity.trade_id;
                    req.change.write.trade_type_code = r.identity.trade_type_code;
                    req.change.write.reference_entity = r.reference_entity;
                    req.change.write.currency = r.currency;
                    req.change.write.notional = r.notional;
                    req.change.write.spread = r.spread;
                    req.change.write.recovery_rate = r.recovery_rate;
                    req.change.write.tenor = r.tenor;
                    req.change.write.start_date = r.start_date;
                    req.change.write.maturity_date = r.maturity_date;
                    req.change.write.day_count_fraction_code = r.day_count_fraction_code;
                    req.change.write.payment_frequency_code = r.payment_frequency_code;
                    req.change.write.index_name = r.index_name;
                    req.change.write.index_series = r.index_series;
                    req.change.write.seniority = r.seniority;
                    req.change.write.restructuring = r.restructuring;
                    req.change.write.description = r.description;
                    req.change.write.option_type = r.option_type;
                    req.change.write.option_expiry_date = r.option_expiry_date;
                    req.change.write.option_strike = r.option_strike;
                    req.change.write.linked_asset_code = r.linked_asset_code;
                    req.change.write.tranche_attachment = r.tranche_attachment;
                    req.change.write.tranche_detachment = r.tranche_detachment;
                    auto resp = nats_call(delegated_nats, req, instr_error);
                    return resp && resp->result.outcome == ores::utility::domain::outcome::ok;
                } else if constexpr (std::is_same_v<T, equity_instrument_data>) {
                    boost::uuids::uuid equity_trade_id{};
                    const auto equity_saved = std::visit(
                        [&](const auto& instr) -> bool {
                            using InstrT = std::decay_t<decltype(instr)>;
                            using namespace ores::trading::domain;
                            equity_trade_id = instr.identity.trade_id;
                            if constexpr (std::is_same_v<InstrT, equity_option_instrument>) {
                                put_equity_option_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.underlying_name = instr.underlying_name;
                                req.change.write.currency = instr.currency;
                                req.change.write.notional = instr.notional;
                                req.change.write.option_type = instr.option_type;
                                req.change.write.strike = instr.strike;
                                req.change.write.expiry_date = instr.expiry_date;
                                req.change.write.exercise_type = instr.exercise_type;
                                req.change.write.long_short = instr.long_short;
                                req.change.write.settlement_type = instr.settlement_type;
                                req.change.write.cliquet_frequency = instr.cliquet_frequency;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                equity_digital_option_instrument>) {
                                put_equity_digital_option_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.underlying_name = instr.underlying_name;
                                req.change.write.currency = instr.currency;
                                req.change.write.notional = instr.notional;
                                req.change.write.option_type = instr.option_type;
                                req.change.write.strike = instr.strike;
                                req.change.write.barrier_level = instr.barrier_level;
                                req.change.write.barrier_type = instr.barrier_type;
                                req.change.write.expiry_date = instr.expiry_date;
                                req.change.write.long_short = instr.long_short;
                                req.change.write.payout_amount = instr.payout_amount;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                equity_barrier_option_instrument>) {
                                put_equity_barrier_option_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.underlying_name = instr.underlying_name;
                                req.change.write.currency = instr.currency;
                                req.change.write.notional = instr.notional;
                                req.change.write.option_type = instr.option_type;
                                req.change.write.strike = instr.strike;
                                req.change.write.expiry_date = instr.expiry_date;
                                req.change.write.exercise_type = instr.exercise_type;
                                req.change.write.long_short = instr.long_short;
                                req.change.write.lower_barrier = instr.lower_barrier;
                                req.change.write.lower_barrier_type = instr.lower_barrier_type;
                                req.change.write.upper_barrier = instr.upper_barrier;
                                req.change.write.upper_barrier_type = instr.upper_barrier_type;
                                req.change.write.rebate = instr.rebate;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                equity_asian_option_instrument>) {
                                put_equity_asian_option_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.underlying_name = instr.underlying_name;
                                req.change.write.currency = instr.currency;
                                req.change.write.notional = instr.notional;
                                req.change.write.option_type = instr.option_type;
                                req.change.write.strike = instr.strike;
                                req.change.write.expiry_date = instr.expiry_date;
                                req.change.write.exercise_type = instr.exercise_type;
                                req.change.write.long_short = instr.long_short;
                                req.change.write.average_type = instr.average_type;
                                req.change.write.averaging_start_date = instr.averaging_start_date;
                                req.change.write.averaging_end_date = instr.averaging_end_date;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                equity_forward_instrument>) {
                                put_equity_forward_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.underlying_name = instr.underlying_name;
                                req.change.write.currency = instr.currency;
                                req.change.write.quantity = instr.quantity;
                                req.change.write.forward_price = instr.forward_price;
                                req.change.write.expiry_date = instr.expiry_date;
                                req.change.write.long_short = instr.long_short;
                                req.change.write.settlement_type = instr.settlement_type;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                equity_variance_swap_instrument>) {
                                put_equity_variance_swap_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.underlying_name = instr.underlying_name;
                                req.change.write.currency = instr.currency;
                                req.change.write.notional = instr.notional;
                                req.change.write.variance_strike = instr.variance_strike;
                                req.change.write.start_date = instr.start_date;
                                req.change.write.maturity_date = instr.maturity_date;
                                req.change.write.long_short = instr.long_short;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT, equity_swap_instrument>) {
                                put_equity_swap_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.underlying_name = instr.underlying_name;
                                req.change.write.basket_json = instr.basket_json;
                                req.change.write.currency = instr.currency;
                                req.change.write.notional = instr.notional;
                                req.change.write.return_type = instr.return_type;
                                req.change.write.start_date = instr.start_date;
                                req.change.write.maturity_date = instr.maturity_date;
                                req.change.write.long_short = instr.long_short;
                                req.change.write.payment_frequency_code =
                                    instr.payment_frequency_code;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                equity_accumulator_instrument>) {
                                put_equity_accumulator_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.underlying_name = instr.underlying_name;
                                req.change.write.currency = instr.currency;
                                req.change.write.strike = instr.strike;
                                req.change.write.fixing_amount = instr.fixing_amount;
                                req.change.write.start_date = instr.start_date;
                                req.change.write.expiry_date = instr.expiry_date;
                                req.change.write.fixing_frequency = instr.fixing_frequency;
                                req.change.write.long_short = instr.long_short;
                                req.change.write.knock_out_level = instr.knock_out_level;
                                req.change.write.target_amount = instr.target_amount;
                                req.change.write.target_type = instr.target_type;
                                req.change.write.payoff_type = instr.payoff_type;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                equity_position_instrument>) {
                                put_equity_position_instrument_request req;
                                req.change.write.trade_id = instr.identity.trade_id;
                                req.change.write.trade_type_code = instr.identity.trade_type_code;
                                req.change.write.underlying_name = instr.underlying_name;
                                req.change.write.currency = instr.currency;
                                req.change.write.quantity = instr.quantity;
                                req.change.write.price = instr.price;
                                req.change.write.description = instr.description;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp &&
                                       resp->result.outcome == ores::utility::domain::outcome::ok;
                            } else {
                                // Unknown per-type alternative — fail loudly so a new
                                // variant added without updating this dispatch surfaces
                                // at import time instead of silently skipping saves.
                                instr_error = "equity variant alternative not handled "
                                              "by import dispatch";
                                return false;
                            }
                        },
                        r.instrument);
                    if (!equity_saved)
                        return false;
                    int equity_sequence_number = 0;
                    for (const auto& underlying : r.underlyings) {
                        put_equity_position_option_underlying_request underlying_req;
                        underlying_req.change.write.trade_id = equity_trade_id;
                        underlying_req.change.write.sequence_number = ++equity_sequence_number;
                        underlying_req.change.write.underlying_name = underlying.underlying_name;
                        underlying_req.change.write.strike = underlying.strike;
                        underlying_req.change.write.weight = underlying.weight;
                        underlying_req.change.write.long_short = underlying.long_short;
                        underlying_req.change.write.option_type = underlying.option_type;
                        underlying_req.change.write.exercise_type = underlying.exercise_type;
                        underlying_req.change.write.settlement_type = underlying.settlement_type;
                        auto underlying_resp =
                            nats_call(delegated_nats, underlying_req, instr_error);
                        if (!underlying_resp ||
                            underlying_resp->result.outcome != ores::utility::domain::outcome::ok)
                            return false;
                    }
                    return true;
                } else if constexpr (std::is_same_v<T, commodity_instrument_data>) {
                    const auto& instr = r.instrument;
                    put_commodity_instrument_request req;
                    req.change.write.trade_id = instr.identity.trade_id;
                    req.change.write.trade_type_code = instr.identity.trade_type_code;
                    req.change.write.commodity_code = instr.commodity_code;
                    req.change.write.currency = instr.currency;
                    req.change.write.quantity = instr.quantity;
                    req.change.write.unit = instr.unit;
                    req.change.write.start_date = instr.start_date;
                    req.change.write.maturity_date = instr.maturity_date;
                    req.change.write.fixed_price = instr.fixed_price;
                    req.change.write.option_type = instr.option_type;
                    req.change.write.strike_price = instr.strike_price;
                    req.change.write.exercise_type = instr.exercise_type;
                    req.change.write.average_type = instr.average_type;
                    req.change.write.averaging_start_date = instr.averaging_start_date;
                    req.change.write.averaging_end_date = instr.averaging_end_date;
                    req.change.write.spread_commodity_code = instr.spread_commodity_code;
                    req.change.write.spread_amount = instr.spread_amount;
                    req.change.write.strip_frequency_code = instr.strip_frequency_code;
                    req.change.write.variance_strike = instr.variance_strike;
                    req.change.write.accumulation_amount = instr.accumulation_amount;
                    req.change.write.knock_out_barrier = instr.knock_out_barrier;
                    req.change.write.barrier_type = instr.barrier_type;
                    req.change.write.lower_barrier = instr.lower_barrier;
                    req.change.write.upper_barrier = instr.upper_barrier;
                    req.change.write.day_count_fraction_code = instr.day_count_fraction_code;
                    req.change.write.payment_frequency_code = instr.payment_frequency_code;
                    req.change.write.swaption_expiry_date = instr.swaption_expiry_date;
                    req.change.write.description = instr.description;
                    auto resp = nats_call(delegated_nats, req, instr_error);
                    if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
                        return false;
                    int sequence_number = 0;
                    for (const auto& constituent : r.constituents) {
                        put_commodity_basket_constituent_request constituent_req;
                        constituent_req.change.write.trade_id = instr.identity.trade_id;
                        constituent_req.change.write.sequence_number = ++sequence_number;
                        constituent_req.change.write.underlying_code = constituent.underlying_code;
                        constituent_req.change.write.weight = constituent.weight;
                        auto constituent_resp =
                            nats_call(delegated_nats, constituent_req, instr_error);
                        if (!constituent_resp ||
                            constituent_resp->result.outcome != ores::utility::domain::outcome::ok)
                            return false;
                    }
                    return true;
                } else if constexpr (std::is_same_v<T, composite_instrument_data>) {
                    put_composite_instrument_with_legs_request req;
                    req.instrument = r.instrument;
                    req.legs = r.legs;
                    auto resp = nats_call(delegated_nats, req, instr_error);
                    return resp && resp->result.outcome == ores::utility::domain::outcome::ok;
                } else if constexpr (std::is_same_v<T, scripted_instrument>) {
                    put_scripted_instrument_request req;
                    req.change.write.trade_id = r.identity.trade_id;
                    req.change.write.trade_type_code = r.identity.trade_type_code;
                    req.change.write.script_name = r.script_name;
                    req.change.write.script_body = r.script_body;
                    req.change.write.events_json = r.events_json;
                    req.change.write.underlyings_json = r.underlyings_json;
                    req.change.write.parameters_json = r.parameters_json;
                    req.change.write.description = r.description;
                    auto resp = nats_call(delegated_nats, req, instr_error);
                    return resp && resp->result.outcome == ores::utility::domain::outcome::ok;
                } else {
                    return true;
                }
            },
            item.instrument);

        if (save_ok) {
            ++instruments_saved;
        } else {
            BOOST_LOG_SEV(lg(), warn)
                << "ore.import.execute instrument save failed | corr=" << req.correlation_id
                << " trade=" << item.trade.identity.external_id << " error=" << instr_error;
            result.item_errors.push_back({.source_file = item.source_file.string(),
                                          .item_id = item.trade.identity.external_id,
                                          .message = "Instrument save failed: " + instr_error});
        }
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 8 complete | corr=" << req.correlation_id
                              << " instruments_saved=" << instruments_saved;

    // -------------------------------------------------------------------------
    // Done — determine step outcome and publish
    // -------------------------------------------------------------------------
    // Total failure: all planned trades failed to save (and at least one was attempted).
    // Adjust this constant to change the threshold for saga compensation.
    const bool all_trades_failed = !plan.trades.empty() &&
                                   result.saved_trade_external_ids.empty() &&
                                   !result.item_errors.empty();

    using wf_outcome = ores::workflow::messaging::step_outcome;
    using wf_log_level = ores::workflow::messaging::step_log_level;
    using wf_log_entry = ores::workflow::messaging::step_log_entry;

    wf_outcome outcome;
    if (result.item_errors.empty()) {
        outcome = wf_outcome::completed;
    } else if (all_trades_failed) {
        outcome = wf_outcome::failed;
    } else {
        outcome = wf_outcome::completed_with_warnings;
    }

    std::vector<wf_log_entry> step_log;
    if (!result.saved_trade_external_ids.empty()) {
        step_log.push_back(
            {.level = wf_log_level::info,
             .message = std::format("Saved {} trade(s).", result.saved_trade_external_ids.size()),
             .context = {}});
    }
    for (const auto& ie : result.item_errors) {
        step_log.push_back(
            {.level = (outcome == wf_outcome::failed) ? wf_log_level::error : wf_log_level::warn,
             .message = ie.message,
             .context = ie.item_id.empty() ? ie.source_file : ie.item_id});
    }

    result.success = outcome != wf_outcome::failed;
    result.message =
        result.item_errors.empty() ?
            "ORE import completed." :
            std::format("ORE import completed with {} error(s).", result.item_errors.size());

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute complete | corr=" << req.correlation_id
                              << " currencies=" << result.saved_currency_iso_codes.size()
                              << " portfolios=" << result.saved_portfolio_names.size()
                              << " books=" << result.saved_book_names.size()
                              << " trades=" << result.saved_trade_external_ids.size()
                              << " item_errors=" << result.item_errors.size()
                              << " outcome=" << ores::workflow::messaging::to_string(outcome);

    if (outcome == wf_outcome::failed) {
        publish_step_completion(
            nats_,
            step_id,
            inst_id,
            wf_outcome::failed,
            rfl::json::write(result),
            std::format("All {} trade save(s) failed.", result.item_errors.size()),
            step_log);
    } else {
        publish_step_completion(
            nats_, step_id, inst_id, outcome, rfl::json::write(result), "", step_log);
    }
}

void ore_import_execute_handler::rollback(ores::nats::message msg) {
    using ores::ore::messaging::ore_import_rollback_request;

    const auto step_id = extract_workflow_header(msg, ores::workflow::messaging::step_id_header);
    const auto inst_id =
        extract_workflow_header(msg, ores::workflow::messaging::instance_id_header);

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<ore_import_rollback_request>(sv);
    if (!parsed) {
        BOOST_LOG_SEV(lg(), error)
            << "ore.import.rollback: failed to decode request | step=" << step_id;
        publish_step_completion(nats_,
                                step_id,
                                inst_id,
                                ores::workflow::messaging::step_outcome::failed,
                                "",
                                "Failed to decode ore_import_rollback_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "ore.import.rollback starting | corr=" << req.correlation_id
                              << " step=" << step_id;

    auto delegated_nats =
        outbound_nats_.with_delegation(req.bearer_token).with_correlation_id(req.correlation_id);

    // ── Delete trades ────────────────────────────────────────────────────────
    if (!req.saved_trade_external_ids.empty()) {
        BOOST_LOG_SEV(lg(), info) << "ore.import.rollback: delete trades | corr="
                                  << req.correlation_id
                                  << " count=" << req.saved_trade_external_ids.size();
        ores::trading::messaging::delete_many_trades_request del_req;
        for (const auto& saved_id : req.saved_trade_external_ids) {
            ores::trading::messaging::trade_removal removal;
            removal.key.external_id = saved_id;
            del_req.removals.push_back(std::move(removal));
        }
        std::string err;
        auto r = nats_call(delegated_nats, del_req, err);
        if (!r || r->result.outcome != ores::utility::domain::outcome::ok) {
            const auto reason = (r && !r->result.message.empty()) ? r->result.message : err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.rollback delete_trades failed | corr=" << req.correlation_id
                << " error=" << reason;
        }
    }

    // ── Delete books ─────────────────────────────────────────────────────────
    if (!req.saved_book_names.empty()) {
        BOOST_LOG_SEV(lg(), info) << "ore.import.rollback: delete books | corr="
                                  << req.correlation_id << " count=" << req.saved_book_names.size();
        ores::refdata::messaging::delete_many_books_request del_req{
            .intent = ores::utility::domain::change_intent{.reason_code = "ore_import_rollback",
                                                           .commentary =
                                                               "Rolling back a failed ORE import"}};
        for (const auto& name : req.saved_book_names)
            del_req.removals.push_back({.key = {.name = name}});
        std::string err;
        auto r = nats_call(delegated_nats, del_req, err);
        if (!r || r->result.outcome != ores::utility::domain::outcome::ok) {
            const auto reason = (r && !r->result.message.empty()) ? r->result.message : err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.rollback delete_books failed | corr=" << req.correlation_id
                << " error=" << reason;
        }
    }

    // ── Delete portfolios (reverse order — children before parents) ──────────
    if (!req.saved_portfolio_names.empty()) {
        BOOST_LOG_SEV(lg(), info) << "ore.import.rollback: delete portfolios | corr="
                                  << req.correlation_id
                                  << " count=" << req.saved_portfolio_names.size();
        ores::refdata::messaging::delete_many_portfolios_request del_req{
            .intent = ores::utility::domain::change_intent{.reason_code = "ore_import_rollback",
                                                           .commentary =
                                                               "Rolling back a failed ORE import"}};
        for (auto it = req.saved_portfolio_names.rbegin(); it != req.saved_portfolio_names.rend();
             ++it)
            del_req.removals.push_back({.key = {.name = *it}});
        std::string err;
        auto r = nats_call(delegated_nats, del_req, err);
        if (!r || r->result.outcome != ores::utility::domain::outcome::ok) {
            const auto reason = (r && !r->result.message.empty()) ? r->result.message : err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.rollback delete_portfolios failed | corr=" << req.correlation_id
                << " error=" << reason;
        }
    }

    // ── Delete currencies ────────────────────────────────────────────────────
    if (!req.saved_currency_iso_codes.empty()) {
        BOOST_LOG_SEV(lg(), info) << "ore.import.rollback: delete currencies | corr="
                                  << req.correlation_id
                                  << " count=" << req.saved_currency_iso_codes.size();
        ores::refdata::messaging::delete_many_currencies_request del_req{
            .intent = ores::utility::domain::change_intent{.reason_code = "ore_import_rollback",
                                                           .commentary =
                                                               "Rolling back a failed ORE import"}};
        for (const auto& iso : req.saved_currency_iso_codes)
            del_req.removals.push_back({.key = {.iso_code = iso}});
        std::string err;
        auto r = nats_call(delegated_nats, del_req, err);
        if (!r || r->result.outcome != ores::utility::domain::outcome::ok) {
            const auto reason = (r && !r->result.message.empty()) ? r->result.message : err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.rollback delete_currencies failed | corr=" << req.correlation_id
                << " error=" << reason;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.rollback complete | corr=" << req.correlation_id;
    publish_step_completion(nats_,
                            step_id,
                            inst_id,
                            ores::workflow::messaging::step_outcome::completed,
                            "{\"success\":true}",
                            "");
}

}
