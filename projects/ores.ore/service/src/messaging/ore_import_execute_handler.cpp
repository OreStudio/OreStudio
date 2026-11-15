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
#include "ores.nats/domain/delivery.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.ore.api/messaging/ore_import_protocol.hpp"
#include "ores.ore.api/net/ore_storage.hpp"
#include "ores.ore.core/domain/portfolio_projection.hpp"
#include "ores.ore.core/planner/ore_import_planner.hpp"
#include "ores.ore.core/scanner/ore_directory_scanner.hpp"
#include "ores.ore.service/messaging/nats_call.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_identifier_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.api/messaging/netting_set_identifier_protocol.hpp"
#include "ores.refdata.api/messaging/netting_set_protocol.hpp"
#include "ores.refdata.api/messaging/portfolio_protocol.hpp"
#include "ores.refdata.api/messaging/sandbox_protocol.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.storage.core/net/storage_transfer.hpp"
#include "ores.trading.api/domain/bond_document_ops.hpp"
#include "ores.trading.api/domain/bond_instrument_rows.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include "ores.trading.api/messaging/balance_guaranteed_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/balance_guaranteed_swap_tranche_notional_protocol.hpp"
#include "ores.trading.api/messaging/balance_guaranteed_swap_tranche_protocol.hpp"
#include "ores.trading.api/messaging/callable_swap_call_date_protocol.hpp"
#include "ores.trading.api/messaging/callable_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/cap_floor_instrument_protocol.hpp"
#include "ores.trading.api/messaging/commodity_basket_constituent_protocol.hpp"
#include "ores.trading.api/messaging/commodity_instrument_protocol.hpp"
#include "ores.trading.api/messaging/composite_instrument_protocol.hpp"
#include "ores.trading.api/messaging/credit_basket_constituent_protocol.hpp"
#include "ores.trading.api/messaging/credit_default_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/credit_instrument_protocol.hpp"
#include "ores.trading.api/messaging/credit_linked_swap_instrument_protocol.hpp"
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
#include "ores.trading.api/messaging/flexi_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/flexi_swap_lower_notional_protocol.hpp"
#include "ores.trading.api/messaging/fra_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_accumulator_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_asian_forward_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_barrier_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_digital_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_forward_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_vanilla_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_variance_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/index_credit_default_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/index_credit_default_swap_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/inflation_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/knock_out_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/range_bound_protocol.hpp"
#include "ores.trading.api/messaging/rate_instrument_protocol.hpp"
#include "ores.trading.api/messaging/risk_participation_agreement_instrument_protocol.hpp"
#include "ores.trading.api/messaging/scripted_instrument_protocol.hpp"
#include "ores.trading.api/messaging/structure_member_protocol.hpp"
#include "ores.trading.api/messaging/structure_protocol.hpp"
#include "ores.trading.api/messaging/swap_leg_amount_protocol.hpp"
#include "ores.trading.api/messaging/swap_leg_protocol.hpp"
#include "ores.trading.api/messaging/swap_leg_rate_protocol.hpp"
#include "ores.trading.api/messaging/swaption_instrument_protocol.hpp"
#include "ores.trading.api/messaging/synthetic_cdo_instrument_protocol.hpp"
#include "ores.trading.api/messaging/trade_additional_field_protocol.hpp"
#include "ores.trading.api/messaging/trade_booking_protocol.hpp"
#include "ores.trading.api/messaging/trade_identifier_protocol.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.trading.api/messaging/trade_portfolio_protocol.hpp"
#include "ores.trading.api/messaging/vanilla_swap_instrument_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/name_generator_sha1.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <format>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <utility>

namespace ores::ore::service::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

namespace {

// The canonical write record carries what the caller owns. The imported
// domain objects carry the server's fields too, so each projects by name.
ores::refdata::messaging::currency_write
ore_import_execute_handler_to_write(const ores::refdata::domain::currency& c) {
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

ores::refdata::messaging::portfolio_write
ore_import_execute_handler_to_write(const ores::refdata::domain::portfolio& p) {
    return {.id = p.id,
            .name = p.name,
            .description = p.description,
            .parent_portfolio_id = p.parent_portfolio_id,
            .owner_unit_id = p.owner_unit_id,
            .purpose_type = p.purpose_type,
            .aggregation_ccy = p.aggregation_ccy,
            .is_virtual = p.is_virtual,
            .sandbox_id = p.sandbox_id,
            .status = p.status};
}

ores::refdata::messaging::book_write
ore_import_execute_handler_to_write(const ores::refdata::domain::book& b) {
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
            .book_purpose_type = b.book_purpose_type,
            .ledger_feed_type = b.ledger_feed_type,
            .is_sweepable = b.is_sweepable,
            .rates_centre_code = b.rates_centre_code,
            .sandbox_id = b.sandbox_id};
}

/**
 * @brief The UUID a document names an entity by, when its text is one.
 *
 * A package the system writes names every entity by its UUID, so the import
 * reads that form before it reads a name of the document's own.
 */
std::optional<boost::uuids::uuid> uuid_named(const std::string& text) {
    if (text.size() != 36)
        return std::nullopt;
    try {
        return boost::uuids::string_generator()(text);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

/**
 * @brief Whether the tenant holds an entity with this id.
 *
 * @return Whether it is held, or nullopt on a failed read, with out_error set.
 */
template <typename Request, typename Filter, typename Nats, typename Count>
std::optional<bool>
id_is_held(Nats& nats, const boost::uuids::uuid& id, Count count, std::string& out_error) {
    Request req;
    req.limit = 1;
    req.filter = Filter{.id_one_of = std::vector<boost::uuids::uuid>{id}};
    auto resp = nats_call(nats, req, out_error);
    if (!resp)
        return std::nullopt;
    if (resp->result.outcome != ores::utility::domain::outcome::ok) {
        out_error = resp->result.message;
        return std::nullopt;
    }
    return count(*resp) != 0;
}

/**
 * @brief An entity an ORE envelope names, and the identifier that named it.
 *
 * The identifier is absent when the name matched the entity's own code rather
 * than one of its identifiers.
 */
struct resolved_name {
    boost::uuids::uuid entity_id;
    std::optional<boost::uuids::uuid> identifier_id;
};

/**
 * @brief The entities an ORE envelope names, resolved before the trade is
 * written.
 */
struct resolved_envelope {
    std::optional<boost::uuids::uuid> counterparty_id;
    std::optional<boost::uuids::uuid> counterparty_identifier_id;
    std::optional<boost::uuids::uuid> netting_set_id;
    std::optional<boost::uuids::uuid> netting_set_identifier_id;
    std::vector<boost::uuids::uuid> portfolio_ids;
};

/**
 * @brief Finds the party's open sample sandbox.
 *
 * A sandbox has no party of its own. It belongs to the party of its anchor
 * portfolio, so the lookup reads the anchors of the open samples and keeps
 * those of the importing party. Several open samples resolve to the first by
 * name, so the choice is stable.
 *
 * @return The sandbox id, or nullopt when none is open; on a failed read,
 * nullopt with out_error set.
 */
template <typename Nats>
std::optional<boost::uuids::uuid>
find_sample_sandbox(Nats& nats, const boost::uuids::uuid& party_id, std::string& out_error) {
    using ores::utility::domain::outcome;

    ores::refdata::messaging::list_sandboxes_request req;
    constexpr std::uint32_t max_sandbox_fetch = 1'000;
    req.limit = max_sandbox_fetch;
    auto resp = nats_call(nats, req, out_error);
    if (!resp)
        return std::nullopt;
    if (resp->result.outcome != outcome::ok) {
        out_error = std::format(
            "Reading the sandboxes failed: {} {}", resp->result.code, resp->result.message);
        return std::nullopt;
    }

    std::vector<const ores::refdata::domain::sandbox*> open_samples;
    std::vector<boost::uuids::uuid> anchors;
    for (const auto& sandbox : resp->sandboxes) {
        if (sandbox.purpose != "sample" || sandbox.status != "open")
            continue;
        open_samples.push_back(&sandbox);
        anchors.push_back(sandbox.anchor_portfolio_id);
    }
    if (open_samples.empty()) {
        out_error = std::format(
            "The session sees {} sandboxes and none is an open sample. Name a sandbox, or set "
            "official to import into the official portfolio tree.",
            resp->sandboxes.size());
        return std::nullopt;
    }

    ores::refdata::messaging::list_portfolios_request anchor_req;
    anchor_req.limit = max_sandbox_fetch;
    anchor_req.filter = ores::refdata::messaging::portfolios_filter{.id_one_of = anchors};
    auto anchor_resp = nats_call(nats, anchor_req, out_error);
    if (!anchor_resp)
        return std::nullopt;
    if (anchor_resp->result.outcome != outcome::ok) {
        out_error = std::format("Reading the sandbox anchors failed: {} {}",
                                anchor_resp->result.code,
                                anchor_resp->result.message);
        return std::nullopt;
    }
    std::set<boost::uuids::uuid> party_anchors;
    for (const auto& portfolio : anchor_resp->portfolios)
        if (portfolio.party_id == party_id)
            party_anchors.insert(portfolio.id);

    const ores::refdata::domain::sandbox* first = nullptr;
    for (const auto* sandbox : open_samples) {
        if (!party_anchors.contains(sandbox->anchor_portfolio_id))
            continue;
        if (!first || sandbox->name < first->name)
            first = sandbox;
    }
    if (!first) {
        out_error = std::format(
            "None of the {} open sample sandboxes belongs to the importing party. Name a "
            "sandbox, or set official to import into the official portfolio tree.",
            open_samples.size());
        return std::nullopt;
    }
    return first->id;
}

/**
 * @brief The counterparty an ORE envelope's CounterParty names.
 *
 * A UUID names a counterparty by its id. Any other text is a string of the
 * document's own, which is resolved as an ORE identifier of a counterparty
 * first, then as a short code.
 *
 * @return The counterparty, or nullopt when the name matches none; on a
 * failed read, nullopt with out_error set.
 */
template <typename Nats>
std::optional<resolved_name>
resolve_counterparty(Nats& nats, const std::string& name, std::string& out_error) {
    using ores::utility::domain::outcome;

    std::string error;
    if (const auto id = uuid_named(name)) {
        const auto held = id_is_held<ores::refdata::messaging::list_counterparties_request,
                                     ores::refdata::messaging::counterparties_filter>(
            nats, *id, [](const auto& r) { return r.counterparties.size(); }, error);
        if (!held) {
            out_error = error;
            return std::nullopt;
        }
        if (*held)
            return resolved_name{.entity_id = *id};
        error.clear();
    }
    ores::refdata::messaging::get_counterparty_identifier_request alias_req;
    alias_req.key.id_value = name;
    auto alias = nats_call(nats, alias_req, error);
    if (!alias) {
        out_error = error;
        return std::nullopt;
    }
    if (alias->result.outcome != outcome::ok && alias->result.outcome != outcome::missing) {
        out_error = alias->result.message;
        return std::nullopt;
    }
    if (alias->result.outcome == outcome::ok && alias->counterparty_identifier &&
        alias->counterparty_identifier->id_scheme == "ORE")
        return resolved_name{.entity_id = alias->counterparty_identifier->counterparty_id,
                             .identifier_id = alias->counterparty_identifier->id};

    error.clear();
    ores::refdata::messaging::get_counterparty_request code_req;
    code_req.key.short_code = name;
    auto counterparty = nats_call(nats, code_req, error);
    if (!counterparty) {
        out_error = error;
        return std::nullopt;
    }
    if (counterparty->result.outcome != outcome::ok &&
        counterparty->result.outcome != outcome::missing) {
        out_error = counterparty->result.message;
        return std::nullopt;
    }
    if (counterparty->result.outcome == outcome::ok && counterparty->counterparty)
        return resolved_name{.entity_id = counterparty->counterparty->id};
    return std::nullopt;
}

/**
 * @brief The netting set an ORE envelope's NettingSetId names.
 *
 * A UUID names a netting set by its id. Any other text is a string of the
 * document's own, which a netting set answers to as an ORE identifier, as a
 * counterparty does.
 *
 * @return The netting set, or nullopt when the id matches none; on a failed
 * read, nullopt with out_error set.
 */
template <typename Nats>
std::optional<resolved_name>
resolve_netting_set(Nats& nats, const std::string& netting_set_id, std::string& out_error) {
    using ores::utility::domain::outcome;

    std::string error;
    if (const auto id = uuid_named(netting_set_id)) {
        const auto held = id_is_held<ores::refdata::messaging::list_netting_sets_request,
                                     ores::refdata::messaging::netting_sets_filter>(
            nats, *id, [](const auto& r) { return r.netting_sets.size(); }, error);
        if (!held) {
            out_error = error;
            return std::nullopt;
        }
        if (*held)
            return resolved_name{.entity_id = *id};
        error.clear();
    }
    ores::refdata::messaging::get_netting_set_identifier_request alias_req;
    alias_req.key.id_value = netting_set_id;
    auto alias = nats_call(nats, alias_req, error);
    if (!alias) {
        out_error = error;
        return std::nullopt;
    }
    if (alias->result.outcome != outcome::ok && alias->result.outcome != outcome::missing) {
        out_error = alias->result.message;
        return std::nullopt;
    }
    if (alias->result.outcome == outcome::ok && alias->netting_set_identifier &&
        alias->netting_set_identifier->id_scheme == "ORE")
        return resolved_name{.entity_id = alias->netting_set_identifier->netting_set_id,
                             .identifier_id = alias->netting_set_identifier->id};
    return std::nullopt;
}

/**
 * @brief The portfolio an ORE envelope's PortfolioId names: a portfolio of the
 * importing party with that name.
 *
 * @return The portfolio's id, or nullopt when the name matches none; on a
 * failed read, nullopt with out_error set.
 */
template <typename Nats>
std::optional<boost::uuids::uuid>
resolve_portfolio(Nats& nats, const std::string& name, std::string& out_error) {
    using ores::utility::domain::outcome;

    std::string error;
    if (const auto id = uuid_named(name)) {
        const auto held = id_is_held<ores::refdata::messaging::list_portfolios_request,
                                     ores::refdata::messaging::portfolios_filter>(
            nats, *id, [](const auto& r) { return r.portfolios.size(); }, error);
        if (!held) {
            out_error = error;
            return std::nullopt;
        }
        if (*held)
            return *id;
        error.clear();
    }
    ores::refdata::messaging::get_portfolio_request req;
    req.key.name = name;
    auto resp = nats_call(nats, req, error);
    if (!resp) {
        out_error = error;
        return std::nullopt;
    }
    if (resp->result.outcome != outcome::ok && resp->result.outcome != outcome::missing) {
        out_error = resp->result.message;
        return std::nullopt;
    }
    if (resp->result.outcome == outcome::ok && resp->portfolio)
        return resp->portfolio->id;
    return std::nullopt;
}

/**
 * @brief Sends a delete-many in parts of a batch's size, in the order its
 * removals are listed.
 *
 * A removal states no version, so each part stands alone.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats, typename Request>
std::string remove_in_parts(Nats& nats, Request& req) {
    const auto removals = std::move(req.removals);
    for (const auto part : ores::utility::domain::batches_of(removals)) {
        req.removals.assign(part.begin(), part.end());
        std::string error;
        auto r = nats_call(nats, req, error);
        if (!r || r->result.outcome != ores::utility::domain::outcome::ok)
            return (r && !r->result.message.empty()) ? r->result.message : error;
    }
    return {};
}

/**
 * @brief Closes the live bookings of the given trades.
 *
 * The anchor is immutable, so this is how an import takes a trade it booked
 * off its book again: the anchor stays, and a trade with no live booking is
 * in no book and no export.
 *
 * @return An empty string on success, or the failure.
 */
template <typename Nats>
std::string close_bookings(Nats& nats, const std::vector<boost::uuids::uuid>& trade_ids) {
    ores::trading::messaging::delete_many_trade_bookings_request req{
        .intent =
            ores::utility::domain::change_intent{.reason_code = "ore_import_rollback",
                                                 .commentary = "Rolling back a failed ORE import"}};
    for (const auto& id : trade_ids)
        req.removals.push_back({.key = {.trade_id = id}});
    return remove_in_parts(nats, req);
}

/**
 * @brief Writes a booked trade's ORE identifier, portfolios and additional
 * fields, in the document's order.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string write_imported_components(Nats& nats,
                                      const ores::ore::xml::trade_import_item& item,
                                      const resolved_envelope& resolved,
                                      const boost::uuids::uuid& activity_id) {
    using ores::utility::domain::outcome;

    std::string error;
    ores::trading::messaging::put_trade_identifier_request id_req;
    id_req.change.write.trade_id = item.anchor.id;
    id_req.change.write.trade_activity_id = activity_id;
    id_req.change.write.id_type = "ORE";
    id_req.change.write.id_value = item.ore_id;
    auto identified = nats_call(nats, id_req, error);
    if (!identified || identified->result.outcome != outcome::ok)
        return error.empty() ? "save_trade_identifier failed" : error;

    int portfolio_sequence = 0;
    for (const auto& portfolio_id : resolved.portfolio_ids) {
        error.clear();
        ores::trading::messaging::put_trade_portfolio_request portfolio_req;
        portfolio_req.change.write.trade_id = item.anchor.id;
        portfolio_req.change.write.trade_activity_id = activity_id;
        portfolio_req.change.write.sequence_number = ++portfolio_sequence;
        portfolio_req.change.write.portfolio_id = portfolio_id;
        auto saved = nats_call(nats, portfolio_req, error);
        if (!saved || saved->result.outcome != outcome::ok)
            return error.empty() ? "save_trade_portfolio failed" : error;
    }

    if (item.envelope && item.envelope->additional_fields) {
        int sequence_number = 0;
        for (const auto& field : *item.envelope->additional_fields) {
            error.clear();
            ores::trading::messaging::put_trade_additional_field_request field_req;
            field_req.change.write.trade_id = item.anchor.id;
            field_req.change.write.trade_activity_id = activity_id;
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
 * @brief Books an imported trade into its anchor and components.
 *
 * The anchor, booking and state are one write. ORE's Trade/@id becomes an
 * identifier under the ORE scheme, and each envelope portfolio and additional
 * field a row of its own, in the document's order. When one of those later
 * writes fails, the booking is closed again, so a half-written trade is in
 * no book. On success the instrument is stamped with the booking's activity,
 * so every instrument version names the activity that wrote it.
 *
 * @return An empty string on success, or the first failure.
 */
/**
 * @brief The id of the structure that joins a composite trade to its
 * components.
 *
 * Named after the composite's own id, which the planner names after the
 * document, so a second run of the import finds the structure it made rather
 * than making another.
 */
boost::uuids::uuid composite_structure_id(const boost::uuids::uuid& composite_id) {
    static const auto name_space =
        boost::uuids::string_generator()("a3c1d6f2-5b8e-4c71-9f20-6e4d2b8a1c53");
    const auto name = boost::uuids::to_string(composite_id);
    const boost::uuids::name_generator_sha1 generate(name_space);
    return generate(name.c_str(), name.size());
}

/**
 * @brief Joins each booked parent trade to the trades it states inline.
 *
 * A composite trade states its components, and a total return swap or a
 * contract for difference its underlyings. The import books each as a trade
 * of its own. A structure of the parent type's template holds the parent in
 * its parent role and each embedded trade in the role and place it held in
 * the document, which is what the export nests them back by.
 */
template <typename Nats>
std::vector<ores::ore::messaging::ore_import_item_error>
join_composites(Nats& nats,
                const std::vector<ores::ore::xml::trade_import_item>& trades,
                const std::unordered_map<std::string, std::optional<boost::uuids::uuid>>& booked) {
    using ores::utility::domain::outcome;
    std::vector<ores::ore::messaging::ore_import_item_error> errors;
    std::unordered_map<std::string, const ores::ore::xml::trade_import_item*> composite_of;
    for (const auto& item : trades)
        if (!item.membership &&
            ores::ore::domain::portfolio_projection::structure_of(item.anchor.trade_type))
            composite_of.emplace(item.ore_id, &item);

    const auto intent = ores::utility::domain::change_intent{
        .reason_code = "system.import", .commentary = "Imported from an ORE directory"};
    std::unordered_set<std::string> made;
    const auto join = [&](const boost::uuids::uuid& trade_id,
                          const boost::uuids::uuid& structure_id,
                          const std::string& role,
                          int sequence_number,
                          const std::optional<std::string>& label) -> std::string {
        ores::trading::messaging::put_structure_member_request req;
        req.change.write.trade_id = trade_id;
        req.change.write.structure_id = structure_id;
        req.change.write.role = role;
        req.change.write.sequence_number = sequence_number;
        req.change.write.member_label = label;
        req.intent = intent;
        std::string error;
        auto resp = nats_call(nats, req, error);
        if (resp && resp->result.outcome == outcome::ok)
            return {};
        return error.empty() ? resp ? resp->result.message : "put_structure_member failed" : error;
    };

    for (const auto& item : trades) {
        if (!item.membership)
            continue;
        const auto tid = boost::uuids::to_string(item.anchor.id);
        if (!booked.contains(tid))
            continue;
        const auto found = composite_of.find(item.membership->parent_ore_id);
        const auto failed = [&](std::string message) {
            errors.push_back({.source_file = item.source_file.string(),
                              .item_id = item.ore_id.empty() ? tid : item.ore_id,
                              .message = std::move(message)});
        };
        if (found == composite_of.end()) {
            failed("The trade " + item.membership->parent_ore_id +
                   " this one is stated inside is not in the import.");
            continue;
        }
        const auto& composite = *found->second;
        const auto composite_tid = boost::uuids::to_string(composite.anchor.id);
        const auto counterparty = booked.find(composite_tid);
        if (counterparty == booked.end()) {
            failed("The trade " + composite.ore_id + " this one is stated inside was not booked.");
            continue;
        }
        const auto structure_id = composite_structure_id(composite.anchor.id);
        const auto joined =
            ores::ore::domain::portfolio_projection::structure_of(composite.anchor.trade_type);
        if (made.insert(composite_tid).second) {
            ores::trading::messaging::put_structure_request req;
            req.change.write.id = structure_id;
            req.change.write.counterparty_id =
                counterparty->second.value_or(boost::uuids::nil_uuid());
            req.change.write.kind = "Typed";
            req.change.write.template_code = std::string(joined->template_code);
            req.intent = intent;
            std::string error;
            auto resp = nats_call(nats, req, error);
            if (!resp || resp->result.outcome != outcome::ok) {
                failed(error.empty() ? resp ? resp->result.message : "put_structure failed" :
                                       error);
                continue;
            }
            if (auto join_error = join(composite.anchor.id,
                                       structure_id,
                                       std::string(joined->parent_role),
                                       1,
                                       std::nullopt);
                !join_error.empty()) {
                failed(join_error);
                continue;
            }
        }
        if (auto error = join(item.anchor.id,
                              structure_id,
                              item.membership->role,
                              item.membership->sequence_number,
                              item.membership->label);
            !error.empty())
            failed(error);
    }
    return errors;
}

/**
 * @brief The terms one security is held with, and the legs first seen for it.
 *
 * One issue row serves every trade of an ISIN, so the first statement of a
 * security's terms decides that row and every later statement has to agree
 * with it. An entry seeded from the database carries the stored row and no
 * legs: the import did not state them, so it cannot compare against them.
 * The entry the import fills in also carries the legs, so the rest of the
 * run's trades of the same security are checked against those.
 */
struct bond_security_terms {
    ores::trading::domain::bond_issue issue;
    std::vector<ores::trading::domain::bond_leg_data> legs;
    bool legs_known = false;
};

/**
 * @brief Names the first term two statements of one security disagree on.
 *
 * Only the terms a document states take part: the identity the write
 * assigns and the audit the caller stamps are not the document's to state,
 * so two trades of one security differ on those and on nothing else.
 *
 * @return The field's name, or an empty string when the terms agree.
 */
std::string issue_terms_difference(const ores::trading::domain::bond_issue& left,
                                   const ores::trading::domain::bond_issue& right) {
    if (left.issuer != right.issuer)
        return "issuer";
    if (left.face_value != right.face_value)
        return "face_value";
    if (left.issue_date != right.issue_date)
        return "issue_date";
    if (left.settlement_days != right.settlement_days)
        return "settlement_days";
    if (left.calendar != right.calendar)
        return "calendar";
    if (left.credit_curve_id != right.credit_curve_id)
        return "credit_curve_id";
    if (left.reference_curve_id != right.reference_curve_id)
        return "reference_curve_id";
    if (left.income_curve_id != right.income_curve_id)
        return "income_curve_id";
    if (left.credit_group != right.credit_group)
        return "credit_group";
    if (left.volatility_curve_id != right.volatility_curve_id)
        return "volatility_curve_id";
    if (left.price_quote_method != right.price_quote_method)
        return "price_quote_method";
    if (left.price_quote_base_value != right.price_quote_base_value)
        return "price_quote_base_value";
    if (left.sub_type != right.sub_type)
        return "sub_type";
    if (left.price_type != right.price_type)
        return "price_type";
    if (left.payer != right.payer)
        return "payer";
    if (left.credit_risk != right.credit_risk)
        return "credit_risk";
    return {};
}

/**
 * @brief Reads every stored bond issue, keyed by its security id.
 *
 * One issue serves every trade that agrees with its terms, and a security
 * id may carry several: ORE's own example corpus states one security id
 * for trades whose terms differ, and an import has to hold each shape it
 * meets rather than refuse the document or overwrite the first. The row is
 * read whole, because agreeing on the terms is what decides which issue a
 * trade joins. The read is paged, and one pass covers the whole run.
 *
 * @return The map, empty when the read fails, with out_error set.
 */
template <typename Nats>
std::unordered_map<std::string, std::vector<bond_security_terms>>
read_bond_issues_by_security(Nats& nats, std::string& out_error) {
    using ores::trading::messaging::list_bond_issues_request;

    constexpr std::uint32_t page_size = 200;
    constexpr int max_pages = 500;

    std::unordered_map<std::string, std::vector<bond_security_terms>> result;
    std::uint32_t offset = 0;
    for (int page_index = 0; page_index < max_pages; ++page_index) {
        list_bond_issues_request req;
        req.offset = offset;
        req.limit = page_size;
        auto resp = nats_call(nats, req, out_error);
        if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
            return {};
        for (const auto& issue : resp->issues) {
            bond_security_terms terms;
            terms.issue = issue;
            result[issue.security_id].push_back(std::move(terms));
        }
        if (resp->issues.size() < page_size)
            break;
        offset += page_size;
    }
    return result;
}

/**
 * @brief Settles the issue a bond trade is held under, writing it when new.
 *
 * A trade joins the issue of its security whose terms and legs it agrees
 * with. When it agrees with none, the issue is minted and written with every
 * row the security owns, in one call, so the trade is booked against an issue
 * that is already whole. The trade's container then names that issue.
 *
 * @param security_terms The issues each security is held under, keyed by
 * security id, seeded from the database. A trade joins the one it agrees
 * with, mints an issue when it agrees with none, and warns when it had to.
 * @param warnings Collects what the run should report without failing.
 * @return An empty string on success, or the failure.
 */
template <typename Nats>
std::string
settle_bond_issue(Nats& nats,
                  ores::trading::domain::bond_instrument_data& data,
                  std::unordered_map<std::string, std::vector<bond_security_terms>>& security_terms,
                  std::vector<std::string>& warnings) {
    auto& issue = data.issue;

    // A bond position on a basket names no issue: its bonds are underlying
    // rows of the trade.
    if (data.instrument.identity.trade_type_code == "BondPosition" &&
        data.instrument.issue_id == boost::uuids::uuid{})
        return {};

    // A package the system writes names a security by its issue's id. The
    // issue the tenant holds under that id keeps its own security id.
    if (const auto id = uuid_named(issue.security_id)) {
        for (const auto& [code, candidates] : security_terms) {
            if (std::ranges::any_of(candidates,
                                    [&](const auto& c) { return c.issue.issue_id == *id; })) {
                issue.security_id = code;
                break;
            }
        }
    }

    const auto found = security_terms.find(issue.security_id);
    bond_security_terms* adopted = nullptr;
    if (found != security_terms.end()) {
        for (auto& candidate : found->second) {
            if (!issue_terms_difference(issue, candidate.issue).empty())
                continue;
            // An issue seeded from the database carries its terms and not its
            // legs, so this trade's shape cannot be confirmed against it.
            // Minting is the safe answer: adopting would store the trade
            // against an issue whose legs it may not share, and the round
            // trip would hand it back the other trade's coupon.
            if (!candidate.legs_known || candidate.legs != data.bond_legs)
                continue;
            adopted = &candidate;
            break;
        }
    }
    if (adopted != nullptr) {
        issue.issue_id = adopted->issue.issue_id;
        data.instrument.issue_id = issue.issue_id;
        return {};
    }
    if (found != security_terms.end() && !found->second.empty()) {
        // The security id names a shape the tenant holds no issue for, so the
        // trade gets an issue of its own. The document is still imported, and
        // the warning says what to do about it.
        warnings.push_back("Bond security " + issue.security_id +
                           " is reused: this trade's shape matches no issue held for it, so it "
                           "was given its own. Give the trades distinct security ids to share "
                           "one.");
    }

    // The security's rows travel in one write with the issue, so a refusal
    // in any of them leaves no issue behind.
    auto rows = ores::trading::domain::disassemble_bond(data).issue;
    ores::trading::messaging::put_bond_issue_composite_request req;
    req.issue = issue;
    req.legs = std::move(rows.legs);
    req.amounts = std::move(rows.amounts);
    req.rates = std::move(rows.rates);
    req.schedules = std::move(rows.schedules);
    req.schedule_dates = std::move(rows.schedule_dates);
    req.amortizations = std::move(rows.amortizations);
    req.call_dates = data.call_dates;
    req.conversion_targets = data.conversion_targets;
    int sequence_number = 0;
    for (auto& row : req.call_dates) {
        row.issue_id = issue.issue_id;
        row.sequence_number = ++sequence_number;
    }
    sequence_number = 0;
    for (auto& row : req.conversion_targets) {
        row.issue_id = issue.issue_id;
        row.sequence_number = ++sequence_number;
    }

    std::string error;
    auto resp = nats_call(nats, req, error);
    if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok)
        return error.empty() ? "put_bond_issue_composite failed" : error;

    // Recording the terms once the write has landed is what lets the run's
    // later trades of this security be checked against them.
    data.instrument.issue_id = issue.issue_id;
    bond_security_terms terms;
    terms.issue = issue;
    terms.legs = data.bond_legs;
    terms.legs_known = true;
    security_terms[issue.security_id].push_back(std::move(terms));
    return {};
}

/**
 * @brief The issues the import has seen, read from the database on the first
 * bond and grown as the run mints new ones.
 */
struct bond_issues_seen {
    std::unordered_map<std::string, std::vector<bond_security_terms>> by_security;
    std::vector<std::string> warnings;
    bool loaded = false;
};

template <typename Nats>
std::string book_imported_trade(Nats& nats,
                                ores::ore::xml::trade_import_item& item,
                                const resolved_envelope& resolved,
                                bool into_sandbox,
                                bond_issues_seen& issues) {
    using namespace ores::trading::domain;
    using ores::utility::domain::outcome;

    std::string error;
    ores::trading::messaging::book_trade_request book_req;
    book_req.anchor = item.anchor;
    book_req.anchor.counterparty_id = resolved.counterparty_id;
    book_req.anchor.counterparty_scope =
        resolved.counterparty_id ? counterparty_scope::external : counterparty_scope::intra_entity;
    // A virtual book of a sandbox holds only drafts, tests and hypotheticals,
    // so a trade imported into a sandbox is a test, not an actual deal.
    book_req.anchor.booking_nature = into_sandbox ? booking_nature::test : booking_nature::actual;
    book_req.anchor.entry_channel = entry_channel::stp;
    /*
     * The trade names the ORE id it was imported from, so a rollback or a
     * report can trace it back to the document it came out of.
     */
    book_req.anchor.origin = origin::ore_import;
    book_req.anchor.origin_reference = item.ore_id;
    book_req.booking = item.booking;
    book_req.booking.netting_set_id = resolved.netting_set_id;
    book_req.booking.counterparty_identifier_id = resolved.counterparty_identifier_id;
    book_req.booking.netting_set_identifier_id = resolved.netting_set_identifier_id;
    book_req.booking.change_reason_code = "system.external_data_import";
    book_req.activity_type_code = item.activity_type_code;
    // A bond's issue is shared by every trade of the security, so it is
    // settled first: joined when the tenant holds it, written whole when not.
    if (auto* bond = std::get_if<bond_instrument_data>(&item.instrument)) {
        if (!issues.loaded) {
            issues.by_security = read_bond_issues_by_security(nats, error);
            if (!error.empty())
                return error;
            issues.loaded = true;
        }
        if (auto failure = settle_bond_issue(nats, *bond, issues.by_security, issues.warnings);
            !failure.empty())
            return failure;
    }
    // The instrument is booked with the trade, through the writer every booking
    // uses, so each row of every family lands with it.
    append_instrument(book_req.instruments, item.instrument);
    auto booked = nats_call(nats, book_req, error);
    if (!booked || booked->result.outcome != outcome::ok || !booked->activity_id)
        return error.empty() ? "book_trade failed" : error;

    const auto activity_id = *booked->activity_id;
    const auto written = write_imported_components(nats, item, resolved, activity_id);
    if (written.empty()) {
        stamp_ids(item.instrument, item.anchor.id, activity_id);
        return {};
    }
    const auto closed = close_bookings(nats, {item.anchor.id});
    return closed.empty() ? written : written + " The booking could not be closed: " + closed;
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

void ore_import_execute_handler::execute(ores::nats::delivery delivery) {
    const auto& msg = delivery.msg();
    using ores::ore::messaging::ore_import_execute_request;
    using ores::ore::messaging::ore_import_execute_result;


    const auto step_id = extract_workflow_header(msg, ores::workflow::messaging::step_id_header);
    const auto inst_id =
        extract_workflow_header(msg, ores::workflow::messaging::instance_id_header);

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = ores::utility::codec::json::try_read<ore_import_execute_request>(sv);
    if (!parsed) {
        BOOST_LOG_SEV(lg(), error)
            << "ore.import.execute: failed to decode request | step=" << step_id;
        publish_step_completion(nats_,
                                step_id,
                                inst_id,
                                ores::workflow::messaging::step_outcome::failed,
                                "",
                                "Failed to decode ore_import_execute_request");
        std::move(delivery).commit();
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
        std::move(delivery).commit();
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
        std::move(delivery).commit();
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
            std::move(delivery).commit();
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
        std::string failure;
        try {
            choices = ores::utility::codec::json::read<ores::ore::planner::import_choices>(
                req.import_choices_json);
        } catch (const std::exception& e) {
            failure = std::format("Failed to parse import_choices_json: {}", e.what());
        }
        if (!failure.empty()) {
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.execute step 3 failed | corr=" << req.correlation_id
                << " error=" << failure;
            publish_step_completion(nats_,
                                    step_id,
                                    inst_id,
                                    ores::workflow::messaging::step_outcome::failed,
                                    "",
                                    failure);
            std::move(delivery).commit();
            return;
        }
    }

    const bool names_official_target = choices.official.value_or(false) ||
                                       choices.existing_parent_portfolio_id ||
                                       choices.existing_target_book_id;
    if (!choices.sandbox_id && !names_official_target) {
        std::string err;
        choices.sandbox_id = find_sample_sandbox(delegated_nats, choices.party_id, err);
        if (!choices.sandbox_id) {
            const auto failure = err.empty() ?
                                     std::string("The party has no open sample sandbox. Name a "
                                                 "sandbox, or set official to import into the "
                                                 "official portfolio tree.") :
                                     err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.execute step 3 failed | corr=" << req.correlation_id
                << " error=" << failure;
            publish_step_completion(nats_,
                                    step_id,
                                    inst_id,
                                    ores::workflow::messaging::step_outcome::failed,
                                    "",
                                    failure);
            std::move(delivery).commit();
            return;
        }
    }

    const bool into_sandbox = choices.sandbox_id.has_value();

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
        std::move(delivery).commit();
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
            .change = {.write = ore_import_execute_handler_to_write(currency)},
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
            std::move(delivery).commit();
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
            .change = {.write = ore_import_execute_handler_to_write(portfolio)},
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
            std::move(delivery).commit();
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
            .change = {.write = ore_import_execute_handler_to_write(book)},
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
            std::move(delivery).commit();
            return;
        }
        result.saved_book_names.push_back(name);
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 6 complete | corr=" << req.correlation_id
                              << " saved=" << result.saved_book_names.size();

    // -------------------------------------------------------------------------
    // Step 7: save trades (failures collected; saga continues)
    // -------------------------------------------------------------------------
    std::unordered_map<std::string, std::optional<boost::uuids::uuid>> booked_counterparty;
    bond_issues_seen bond_issues;
    // A parent is its embedded trades as much as its own terms, so they are
    // booked first and a parent any of them failed is not booked at all.
    std::ranges::stable_partition(plan.trades,
                                  [](const auto& item) { return item.membership.has_value(); });
    std::unordered_map<std::string, std::vector<std::string>> members_of;
    for (const auto& item : plan.trades)
        if (item.membership)
            members_of[item.membership->parent_ore_id].push_back(
                boost::uuids::to_string(item.anchor.id));
    for (auto& item : plan.trades) {
        const auto tid = boost::uuids::to_string(item.anchor.id);
        const auto src = item.source_file.string();
        const auto& ext_id = item.ore_id;

        if (!item.membership) {
            if (const auto members = members_of.find(ext_id); members != members_of.end()) {
                const auto unbooked = std::ranges::count_if(members->second, [&](const auto& id) {
                    return !booked_counterparty.contains(id);
                });
                if (unbooked > 0) {
                    const auto failure = std::format(
                        "{} of the trades stated inside this one were not booked, so it is not "
                        "booked either: see their errors.",
                        unbooked);
                    BOOST_LOG_SEV(lg(), warn)
                        << "ore.import.execute trade save failed | corr=" << req.correlation_id
                        << " trade_id=" << tid << " source=" << src << " error=" << failure;
                    result.item_errors.push_back(
                        {.source_file = src, .item_id = ext_id, .message = failure});
                    continue;
                }
            }
        }

        // The ORE trade id becomes the trade's ORE identifier, the name the
        // export writes back. A trade without one is reported as an item
        // error rather than booked under no name.
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
        resolved_envelope resolved{.counterparty_id = item.anchor.counterparty_id};
        const auto& envelope = item.envelope;
        if (envelope && envelope->counter_party && !envelope->counter_party->empty()) {
            std::string resolve_error;
            const auto counterparty =
                resolve_counterparty(delegated_nats, *envelope->counter_party, resolve_error);
            if (!counterparty) {
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
            resolved.counterparty_id = counterparty->entity_id;
            resolved.counterparty_identifier_id = counterparty->identifier_id;
        }

        if (envelope && envelope->netting_set_id && !envelope->netting_set_id->empty()) {
            std::string netting_error;
            const auto netting_set =
                resolve_netting_set(delegated_nats, *envelope->netting_set_id, netting_error);
            if (!netting_set) {
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
            resolved.netting_set_id = netting_set->entity_id;
            resolved.netting_set_identifier_id = netting_set->identifier_id;
        }

        if (envelope && envelope->portfolio_ids) {
            std::string unresolved;
            for (const auto& name : *envelope->portfolio_ids) {
                std::string portfolio_error;
                const auto portfolio = resolve_portfolio(delegated_nats, name, portfolio_error);
                if (!portfolio) {
                    unresolved =
                        portfolio_error.empty() ?
                            std::format("PortfolioId {} matches no portfolio of the party: give "
                                        "the party a portfolio with this name.",
                                        name) :
                            portfolio_error;
                    break;
                }
                resolved.portfolio_ids.push_back(*portfolio);
            }
            if (!unresolved.empty()) {
                BOOST_LOG_SEV(lg(), warn)
                    << "ore.import.execute portfolio unresolved | corr=" << req.correlation_id
                    << " trade_id=" << tid << " source=" << src << " error=" << unresolved;
                result.item_errors.push_back(
                    {.source_file = src, .item_id = ext_id, .message = unresolved});
                continue;
            }
        }

        // The booking is written first: it checks the trade's book, counterparty
        // and netting set against one another, so a trade it refuses leaves
        // nothing behind.
        const auto booking_error =
            book_imported_trade(delegated_nats, item, resolved, into_sandbox, bond_issues);
        if (!booking_error.empty()) {
            BOOST_LOG_SEV(lg(), warn)
                << "ore.import.execute trade booking failed | corr=" << req.correlation_id
                << " trade_id=" << tid << " source=" << src << " error=" << booking_error;
            result.item_errors.push_back(
                {.source_file = src, .item_id = ext_id, .message = booking_error});
            continue;
        }
        result.saved_trade_ids.push_back(tid);
        booked_counterparty.emplace(tid, resolved.counterparty_id);
    }

    // A parent is its embedded trades as much as its own terms, so an embedded
    // trade whose parent did not reach the book comes off it again: the parent
    // and its trades land together or not at all.
    std::unordered_set<std::string> booked_parents;
    for (const auto& item : plan.trades)
        if (!item.membership &&
            booked_counterparty.contains(boost::uuids::to_string(item.anchor.id)))
            booked_parents.insert(item.ore_id);
    std::vector<boost::uuids::uuid> orphaned;
    for (const auto& item : plan.trades) {
        if (!item.membership || booked_parents.contains(item.membership->parent_ore_id))
            continue;
        const auto id = boost::uuids::to_string(item.anchor.id);
        if (booked_counterparty.erase(id) == 0)
            continue;
        std::erase(result.saved_trade_ids, id);
        orphaned.push_back(item.anchor.id);
        result.item_errors.push_back(
            {.source_file = item.source_file.string(),
             .item_id = item.ore_id,
             .message = "The trade this one is stated inside was not booked, so this one was "
                        "taken off the book again."});
    }
    if (!orphaned.empty()) {
        if (const auto reason = close_bookings(delegated_nats, orphaned); !reason.empty()) {
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.execute embedded trades not closed | corr=" << req.correlation_id
                << " error=" << reason;
            result.item_errors.push_back(
                {.source_file = {},
                 .item_id = {},
                 .message = "The embedded trades of a parent that was not booked could not be "
                            "taken off the book: " +
                            reason});
        }
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 7 complete | corr=" << req.correlation_id
                              << " saved=" << result.saved_trade_ids.size()
                              << " failed=" << result.item_errors.size();

    // -------------------------------------------------------------------------
    // Step 7b: join each composite trade to its components in a structure
    // -------------------------------------------------------------------------
    const auto structure_errors = join_composites(delegated_nats, plan.trades, booked_counterparty);
    for (const auto& error : structure_errors) {
        BOOST_LOG_SEV(lg(), warn) << "ore.import.execute structure failed | corr="
                                  << req.correlation_id << " error=" << error.message;
        result.item_errors.push_back(error);
    }

    const auto& import_warnings = bond_issues.warnings;

    // -------------------------------------------------------------------------
    // Done — determine step outcome and publish
    // -------------------------------------------------------------------------
    // Total failure: all planned trades failed to save (and at least one was attempted).
    // Adjust this constant to change the threshold for saga compensation.
    const bool all_trades_failed =
        !plan.trades.empty() && result.saved_trade_ids.empty() && !result.item_errors.empty();

    using wf_outcome = ores::workflow::messaging::step_outcome;
    using wf_log_level = ores::workflow::messaging::step_log_level;
    using wf_log_entry = ores::workflow::messaging::step_log_entry;

    wf_outcome outcome;
    if (all_trades_failed) {
        outcome = wf_outcome::failed;
    } else if (!result.item_errors.empty() || !import_warnings.empty()) {
        outcome = wf_outcome::completed_with_warnings;
    } else {
        outcome = wf_outcome::completed;
    }

    for (const auto& warning : import_warnings)
        BOOST_LOG_SEV(lg(), warn) << "ore.import.execute warning | corr=" << req.correlation_id
                                  << " warning=" << warning;

    std::vector<wf_log_entry> step_log;
    if (!result.saved_trade_ids.empty()) {
        step_log.push_back(
            {.level = wf_log_level::info,
             .message = std::format("Saved {} trade(s).", result.saved_trade_ids.size()),
             .context = {}});
    }
    for (const auto& ie : result.item_errors) {
        step_log.push_back(
            {.level = (outcome == wf_outcome::failed) ? wf_log_level::error : wf_log_level::warn,
             .message = ie.message,
             .context = ie.item_id.empty() ? ie.source_file : ie.item_id});
    }
    for (const auto& warning : import_warnings)
        step_log.push_back({.level = wf_log_level::warn, .message = warning, .context = {}});

    result.success = outcome != wf_outcome::failed;
    result.message =
        result.item_errors.empty() ?
            "ORE import completed." :
            std::format("ORE import completed with {} error(s).", result.item_errors.size());

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute complete | corr=" << req.correlation_id
                              << " currencies=" << result.saved_currency_iso_codes.size()
                              << " portfolios=" << result.saved_portfolio_names.size()
                              << " books=" << result.saved_book_names.size()
                              << " trades=" << result.saved_trade_ids.size()
                              << " item_errors=" << result.item_errors.size()
                              << " outcome=" << ores::workflow::messaging::to_string(outcome);

    if (outcome == wf_outcome::failed) {
        publish_step_completion(
            nats_,
            step_id,
            inst_id,
            wf_outcome::failed,
            ores::utility::codec::json::write(result),
            std::format("All {} trade save(s) failed.", result.item_errors.size()),
            step_log);
        std::move(delivery).commit();
    } else {
        publish_step_completion(nats_,
                                step_id,
                                inst_id,
                                outcome,
                                ores::utility::codec::json::write(result),
                                "",
                                step_log);
        std::move(delivery).commit();
    }
}

void ore_import_execute_handler::rollback(ores::nats::delivery delivery) {
    const auto& msg = delivery.msg();
    using ores::ore::messaging::ore_import_rollback_request;


    const auto step_id = extract_workflow_header(msg, ores::workflow::messaging::step_id_header);
    const auto inst_id =
        extract_workflow_header(msg, ores::workflow::messaging::instance_id_header);

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = ores::utility::codec::json::try_read<ore_import_rollback_request>(sv);
    if (!parsed) {
        BOOST_LOG_SEV(lg(), error)
            << "ore.import.rollback: failed to decode request | step=" << step_id;
        publish_step_completion(nats_,
                                step_id,
                                inst_id,
                                ores::workflow::messaging::step_outcome::failed,
                                "",
                                "Failed to decode ore_import_rollback_request");
        std::move(delivery).commit();
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "ore.import.rollback starting | corr=" << req.correlation_id
                              << " step=" << step_id;

    auto delegated_nats =
        outbound_nats_.with_delegation(req.bearer_token).with_correlation_id(req.correlation_id);

    // ── Close the trades' bookings ──────────────────────────────────────────
    // The anchor is immutable, so a rolled-back trade keeps its anchor and
    // loses its live booking, which takes it off its book.
    if (!req.saved_trade_ids.empty()) {
        BOOST_LOG_SEV(lg(), info) << "ore.import.rollback: close bookings | corr="
                                  << req.correlation_id << " count=" << req.saved_trade_ids.size();
        std::vector<boost::uuids::uuid> trade_ids;
        trade_ids.reserve(req.saved_trade_ids.size());
        for (const auto& saved_id : req.saved_trade_ids) {
            try {
                trade_ids.push_back(boost::lexical_cast<boost::uuids::uuid>(saved_id));
            } catch (const boost::bad_lexical_cast&) {
                BOOST_LOG_SEV(lg(), error)
                    << "ore.import.rollback skipping unparsable trade id | corr="
                    << req.correlation_id << " id=" << saved_id;
            }
        }
        const auto reason = close_bookings(delegated_nats, trade_ids);
        if (!reason.empty())
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.rollback close_bookings failed | corr=" << req.correlation_id
                << " error=" << reason;
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
        const auto reason = remove_in_parts(delegated_nats, del_req);
        if (!reason.empty())
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.rollback delete_books failed | corr=" << req.correlation_id
                << " error=" << reason;
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
        const auto reason = remove_in_parts(delegated_nats, del_req);
        if (!reason.empty())
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.rollback delete_portfolios failed | corr=" << req.correlation_id
                << " error=" << reason;
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
        const auto reason = remove_in_parts(delegated_nats, del_req);
        if (!reason.empty())
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.rollback delete_currencies failed | corr=" << req.correlation_id
                << " error=" << reason;
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.rollback complete | corr=" << req.correlation_id;
    publish_step_completion(nats_,
                            step_id,
                            inst_id,
                            ores::workflow::messaging::step_outcome::completed,
                            "{\"success\":true}",
                            "");
    std::move(delivery).commit();
}

}
