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
#include "ores.synthetic.core/trading/trading_trade_population_writer.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/commodity_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/currency_pair_protocol.hpp"
#include "ores.refdata.api/messaging/equity_underlying_protocol.hpp"
#include "ores.refdata.api/messaging/floating_index_type_protocol.hpp"
#include "ores.refdata.api/messaging/legal_entity_protocol.hpp"
#include "ores.refdata.api/messaging/netting_set_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_book_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_counterparty_protocol.hpp"
#include "ores.trading.api/domain/balance_guaranteed_swap_instrument.hpp"
#include "ores.trading.api/domain/bond_instruments.hpp"
#include "ores.trading.api/domain/callable_swap_instrument.hpp"
#include "ores.trading.api/domain/cap_floor_instrument.hpp"
#include "ores.trading.api/domain/commodity_instruments.hpp"
#include "ores.trading.api/domain/credit_instruments.hpp"
#include "ores.trading.api/domain/equity_instruments.hpp"
#include "ores.trading.api/domain/flexi_swap_instrument.hpp"
#include "ores.trading.api/domain/fra_instrument.hpp"
#include "ores.trading.api/domain/fx_accumulator_instrument.hpp"
#include "ores.trading.api/domain/fx_asian_forward_instrument.hpp"
#include "ores.trading.api/domain/fx_barrier_option_instrument.hpp"
#include "ores.trading.api/domain/fx_digital_option_instrument.hpp"
#include "ores.trading.api/domain/fx_forward_instrument.hpp"
#include "ores.trading.api/domain/fx_instruments.hpp"
#include "ores.trading.api/domain/fx_vanilla_option_instrument.hpp"
#include "ores.trading.api/domain/fx_variance_swap_instrument.hpp"
#include "ores.trading.api/domain/knock_out_swap_instrument.hpp"
#include "ores.trading.api/domain/rate_instruments.hpp"
#include "ores.trading.api/domain/swaption_instrument.hpp"
#include "ores.trading.api/domain/vanilla_swap_instrument.hpp"
#include "ores.trading.api/messaging/structure_member_protocol.hpp"
#include "ores.trading.api/messaging/structure_protocol.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <array>
#include <format>
#include <optional>
#include <string>
#include <type_traits>
#include <variant>
#include <vector>

namespace ores::synthetic::service {

namespace {

auto& lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.core.trading_trade_population_writer");
    return instance;
}

namespace domain = ores::synthetic::domain;
using ores::utility::domain::outcome;

constexpr std::uint32_t page = 10000;

/*
 * The reason a generated trade states: that a record was created. It is the
 * ordinary reason for a write and deliberately not a mark of generated data,
 * because the origin says who wrote the trade and the reason says what the
 * write was. It stays a system reason: the digest fold reads it to decide
 * whether an amendment moved the external version, and a population claiming
 * an agreement would report every generated trade as customer-amended.
 */
constexpr std::string_view population_reason = "system.new_record";

/*
 * The scheme the population's own identifier travels under. UTI and USI are
 * the regimes' identifiers and ORE is the engine's; the firm's own name for a
 * trade is the Internal one.
 */
constexpr std::string_view internal_id_type = "Internal";

/*
 * The permissions a booking of a generated trade needs, which are the ones the booking operation
 * checks: the trade write, and the service-origin write that a population's origin claims. A
 * person who holds both may enter a generated trade for their party.
 */
constexpr std::array<std::string_view, 2> booking_permissions{
    "trading::trades:write", "trading::trades:write_service_origin"};

}

trading_trade_population_writer::trading_trade_population_writer(
    ores::nats::service::nats_client& nats,
    booking_client booker,
    std::optional<domain::trade_population_trader> fallback)
    : nats_(nats)
    , booker_(std::move(booker))
    , fallback_(std::move(fallback)) {}

domain::trade_generation_scope
trading_trade_population_writer::resolve_scope(const boost::uuids::uuid& config_id) {
    const auto& codec = ores::nats::default_wire_codec();
    /*
     * One read helper for every reference this run resolves. A refusal names
     * the subject, because the caller's next move is to ask why the tenant's
     * own data is not readable rather than to change the config.
     */
    const auto read = [&](const auto& request) {
        using request_type = std::decay_t<decltype(request)>;
        const auto reply =
            nats_.authenticated_request(request_type::nats_subject, codec.encode(request));
        const auto error = reply.headers.find("X-Error");
        if (error != reply.headers.end())
            throw trade_population_write_error(
                std::format("{} was refused: {}", request_type::nats_subject, error->second));
        auto decoded = codec.decode<typename request_type::response_type>(reply.data);
        if (!decoded)
            throw trade_population_write_error(std::format("{} answered something unreadable: {}",
                                                           request_type::nats_subject,
                                                           decoded.error().what()));
        return *decoded;
    };

    namespace synthetic = ores::synthetic::messaging;

    synthetic::list_trade_generation_configs_request config_req;
    config_req.limit = page;
    synthetic::trade_generation_configs_filter config_filter;
    config_filter.id_one_of = std::vector<boost::uuids::uuid>{config_id};
    config_req.filter = config_filter;
    const auto configs = read(config_req);
    if (configs.trade_generation_configs.empty())
        throw trade_population_write_error("No trade generation config " +
                                           boost::uuids::to_string(config_id) +
                                           " is on the books.");
    const auto& config = configs.trade_generation_configs.front();

    domain::trade_generation_scope scope;
    scope.config_id = config.id;
    scope.party_id = config.party_id;
    scope.asset_class_code = config.asset_class_code;
    scope.min_tenor_code = config.min_tenor_code;
    scope.max_tenor_code = config.max_tenor_code;
    scope.min_notional = config.min_notional.to_double();
    scope.max_notional = config.max_notional.to_double();
    scope.as_of_date = config.as_of_date;
    scope.seed = config.seed;
    scope.trades_per_book = config.trades_per_book;
    scope.structures_per_book = static_cast<std::uint32_t>(config.structures_per_book);
    scope.reference_curve_id = config.reference_curve_id;

    synthetic::list_trade_generation_books_request book_scope_req;
    book_scope_req.limit = page;
    synthetic::trade_generation_books_filter book_scope_filter;
    book_scope_filter.config_id = config_id;
    book_scope_req.filter = book_scope_filter;
    for (const auto& row : read(book_scope_req).trade_generation_books)
        scope.book_ids.push_back(row.book_id);

    synthetic::list_trade_generation_counterparties_request cp_scope_req;
    cp_scope_req.limit = page;
    synthetic::trade_generation_counterparties_filter cp_scope_filter;
    cp_scope_filter.config_id = config_id;
    cp_scope_req.filter = cp_scope_filter;
    for (const auto& row : read(cp_scope_req).trade_generation_counterparties)
        scope.counterparty_ids.push_back(row.counterparty_id);

    /*
     * The books are checked against the tenant's own, so a scope naming a book
     * that does not exist fails here rather than at the booking trigger, which
     * would report it as the trade's fault.
     */
    ores::refdata::messaging::list_books_request books_req;
    books_req.limit = page;
    const auto books = read(books_req);
    for (const auto& book_id : scope.book_ids) {
        const auto known = std::ranges::any_of(
            books.books, [&](const auto& candidate) { return candidate.id == book_id; });
        if (!known)
            throw trade_population_write_error("Book " + boost::uuids::to_string(book_id) +
                                               " is in the config's scope and is not a book of "
                                               "this tenant.");
    }
    /*
     * The currency the population is kept in is the first book's own, because a
     * book is where a currency is decided and the config does not state one.
     */
    for (const auto& book : books.books) {
        if (book.id == scope.book_ids.front() && !book.functional_currency.empty()) {
            scope.currency_code = book.functional_currency;
            break;
        }
    }

    /*
     * The commodity catalogue is the tenant's own, so a population draws the
     * commodities that tenant holds and the unit each is quoted in. An empty
     * catalogue is not this call's to refuse: the planner says so, because it
     * is the planner that cannot state a trade without one.
     */
    ores::refdata::messaging::list_commodities_request commodity_req;
    commodity_req.limit = page;
    for (const auto& commodity : read(commodity_req).commodities)
        scope.commodities.push_back(
            domain::commodity_reference{.code = commodity.code,
                                        .unit = commodity.unit_code,
                                        .quotation_currency = commodity.quotation_currency_code});

    /*
     * A floating leg names an index from the floating index catalogue, which
     * is the tenant's own. It is not the benchmark rate set: a leg states
     * =GBP-SONIA=, the FpML name of the index, and the store refuses anything
     * outside that vocabulary. An empty set is the planner's to refuse, because
     * a leg naming an unheld index would book and then fail to price.
     */
    ores::refdata::messaging::list_floating_index_types_request index_req;
    index_req.limit = page;
    for (const auto& index : read(index_req).types)
        scope.floating_index_codes.push_back(index.code);

    /*
     * A currency pair is the tenant's own reference data too, and an FX trade
     * names one. As with commodities and indices, an empty set is the planner's
     * to refuse, because a pair the store does not hold cannot be resolved.
     */
    ores::refdata::messaging::list_currency_pairs_request pair_req;
    pair_req.limit = page;
    for (const auto& pair : read(pair_req).pairs)
        scope.currency_pairs.push_back(domain::currency_pair_reference{
            .base = pair.base_currency, .quote = pair.quote_currency});

    /*
     * An equity trade names an underlying from the tenant's equity catalogue,
     * and a worst-of basket states the kind beside each name because the store
     * refuses a code its kind's catalogue does not hold. An empty catalogue is
     * the planner's to refuse, as with the lists above.
     */
    ores::refdata::messaging::list_equity_underlyings_request underlying_req;
    underlying_req.limit = page;
    for (const auto& underlying : read(underlying_req).equity_underlyings)
        scope.equity_underlyings.push_back(domain::equity_underlying_reference{
            .code = underlying.code,
            /*
             * The vocabulary the code belongs to is the equity catalogue
             * itself. Whether the row is an index or a share is that
             * catalogue's own classification and not the kind the store
             * validates a basket constituent against.
             */
            .kind = "equity",
            .currency = underlying.currency_code});

    /*
     * A credit trade references a legal entity by its LEI. The entities belong
     * to the DQ dataset rather than to a trading table, so the population reads
     * them from where they live and states the LEI as the reference.
     */
    ores::refdata::messaging::list_legal_entities_request legal_entity_req;
    legal_entity_req.limit = page;
    for (const auto& entity : read(legal_entity_req).legal_entities)
        if (!entity.lei.empty())
            scope.credit_reference_entities.push_back(entity.lei);

    ores::refdata::messaging::list_counterparties_request cp_req;
    cp_req.limit = page;
    const auto counterparties = read(cp_req);
    for (const auto& counterparty : counterparties.counterparties)
        scope.issuer_ids.push_back(counterparty.id);
    for (const auto& counterparty_id : scope.counterparty_ids) {
        const auto known =
            std::ranges::any_of(counterparties.counterparties, [&](const auto& candidate) {
                return candidate.id == counterparty_id;
            });
        if (!known)
            throw trade_population_write_error(
                "Counterparty " + boost::uuids::to_string(counterparty_id) +
                " is in the config's scope and is not a counterparty of this tenant.");
    }

    /*
     * The netting set is not a fact of the config: it belongs to the party and
     * the counterparty together, which is what the booking trigger checks. So
     * it is read from the sets the party actually clears against and a
     * counterparty with none simply books without one.
     */
    ores::refdata::messaging::list_netting_sets_request sets_req;
    sets_req.limit = page;
    const auto sets = read(sets_req);
    for (const auto& set : sets.netting_sets) {
        if (set.party_id != scope.party_id || !set.counterparty_id)
            continue;
        scope.netting_sets.emplace_back(*set.counterparty_id, set.id);
    }

    /*
     * The people who may enter a trade come from the tenant's own staff: the party's accounts that
     * hold the permissions a booking of a generated trade needs. They are asked for by account,
     * with the same check the booking makes, because a person the run cannot book as is a person
     * who cannot ask for a trade's authorisation either.
     *
     * Each of them is named with their account, because the population is entered as those people
     * and not as whatever principal runs the generator: the booking carries the trader's session,
     * so the trade states them as its author and the authorisation request the booking raises is
     * theirs.
     */
    ores::iam::messaging::list_accounts_request accounts_req;
    accounts_req.limit = page;
    const auto accounts = read(accounts_req);

    for (const auto& account : accounts.accounts) {
        if (account.account_type != "user")
            continue;
        if (!account.default_party_id || *account.default_party_id != scope.party_id)
            continue;

        ores::iam::messaging::get_account_roles_request roles_req;
        roles_req.account_id = boost::uuids::to_string(account.id);
        const auto access = read(roles_req);
        std::set<std::string> held;
        for (const auto& role : access.roles)
            held.insert(role.permission_codes.begin(), role.permission_codes.end());
        const auto may_book = std::ranges::all_of(booking_permissions, [&held](const auto& one) {
            return held.contains(std::string(one));
        });
        if (!may_book)
            continue;

        scope.traders.push_back(domain::trade_population_trader{account.id, account.username});
    }

    // A party whose staff holds no trader is entered by the run's own principal, which the caller
    // states. The population is then booked as nobody of the party, so the run says so rather than
    // leaving the party's staff with trades none of them can own.
    if (scope.traders.empty() && fallback_) {
        BOOST_LOG_SEV(lg(), ores::logging::warn)
            << "Party " << boost::uuids::to_string(scope.party_id)
            << " holds no account with the Trading role, so its population is entered by '"
            << fallback_->username << "' and no person of the party owns its requests.";
        scope.traders.push_back(*fallback_);
    }

    return scope;
}

std::optional<trade_population_stored>
trading_trade_population_writer::read_trade(const boost::uuids::uuid& trade_id) {
    const auto& codec = ores::nats::default_wire_codec();
    ores::trading::messaging::list_trades_request req;
    req.limit = 1;
    ores::trading::messaging::trades_filter filter;
    filter.id_one_of = std::vector<boost::uuids::uuid>{trade_id};
    req.filter = filter;

    const auto reply = nats_.authenticated_request(
        ores::trading::messaging::list_trades_request::nats_subject, codec.encode(req));
    const auto error = reply.headers.find("X-Error");
    if (error != reply.headers.end())
        throw trade_population_write_error(std::format(
            "Reading trade {} was refused: {}", boost::uuids::to_string(trade_id), error->second));
    auto decoded = codec.decode<ores::trading::messaging::list_trades_response>(reply.data);
    if (!decoded)
        throw trade_population_write_error(
            std::format("Reading trade {} answered something unreadable: {}",
                        boost::uuids::to_string(trade_id),
                        decoded.error().what()));
    if (decoded->trades.empty())
        return std::nullopt;
    return trade_population_stored{
        .version = static_cast<std::uint32_t>(decoded->trades.front().version),
        .external_version = static_cast<std::uint32_t>(decoded->trades.front().external_version)};
}

void trading_trade_population_writer::ensure_issue(const domain::trade_population_slot& slot) {
    if (!slot.bond_issue)
        return;
    const auto& issue_row = *slot.bond_issue;
    if (issues_written_.contains(issue_row.issue_id))
        return;

    const auto& codec = ores::nats::default_wire_codec();
    const std::string commentary = "Generated trade population.";
    const auto reason = std::string(population_reason);

    /*
     * The plan states the security as the rows the composite write takes, so
     * the writer packs them, stamps them and sends. It decides nothing about
     * the issue: its terms were drawn where the security was drawn.
     */
    ores::trading::messaging::put_bond_issue_composite_request req;
    req.issue = issue_row;
    req.legs = slot.bond_issue_legs;
    req.amounts = slot.bond_issue_leg_amounts;
    req.rates = slot.bond_issue_leg_rates;
    req.schedules = slot.bond_issue_leg_schedules;
    /*
     * A callable security's calls and a convertible's targets are the issue's
     * own rows, so they cross with it and not with the trade.
     */
    req.call_dates = slot.bond_issue_call_dates;
    req.conversion_targets = slot.bond_issue_conversion_targets;

    const auto mark = [&](auto& row) {
        row.change_reason_code = reason;
        row.change_commentary = commentary;
    };
    mark(req.issue);
    for (auto& row : req.legs)
        mark(row);
    for (auto& row : req.amounts)
        mark(row);
    for (auto& row : req.rates)
        mark(row);
    for (auto& row : req.schedules)
        mark(row);

    const auto reply = nats_.authenticated_request(
        ores::trading::messaging::put_bond_issue_composite_request::nats_subject,
        codec.encode(req));
    const auto error = reply.headers.find("X-Error");
    if (error != reply.headers.end())
        throw trade_population_write_error(std::format(
            "Writing bond issue {} was refused: {}", issue_row.security_id, error->second));
    auto decoded =
        codec.decode<ores::trading::messaging::put_bond_issue_composite_response>(reply.data);
    if (!decoded)
        throw trade_population_write_error(
            std::format("Writing bond issue {} answered something unreadable: {}",
                        issue_row.security_id,
                        decoded.error().what()));

    /*
     * The issue is whole or absent, so a stored issue with equal terms is the
     * issue this run needs. Stored with other terms, it is a different
     * security under the same id, and the run stops.
     */
    const auto& result = decoded->result;
    const bool present = result.outcome == outcome::conflict && result.code == "already_exists";
    if (result.outcome != outcome::ok && !present)
        throw trade_population_write_error(
            std::format("Writing bond issue {} failed: {}", issue_row.security_id, result.message));
    issues_written_.insert(issue_row.issue_id);
}

void trading_trade_population_writer::write_trade(const domain::trade_population_slot& slot,
                                                  const domain::trade_generation_scope& scope) {
    const auto& codec = ores::nats::default_wire_codec();
    /*
     * The security is in place before the trade is booked. A run cut short in
     * the issue then leaves no trade, so the next run writes the issue again
     * and not a booked trade that no run revisits.
     */
    ensure_issue(slot);

    ores::trading::messaging::book_trade_request book_req;
    book_req.anchor.id = slot.trade_id;
    book_req.anchor.party_id = scope.party_id;
    book_req.anchor.counterparty_id = slot.counterparty_id;
    book_req.anchor.trade_type = slot.trade_type_code;
    book_req.anchor.counterparty_scope = ores::trading::domain::counterparty_scope::external;
    book_req.anchor.booking_nature = ores::trading::domain::booking_nature::actual;
    book_req.anchor.entry_channel = ores::trading::domain::entry_channel::stp;
    /*
     * The population states its own origin and the config it came from, so a
     * report or a rollback separates a generated trade from one a person
     * entered, and names the recipe that produced it.
     */
    book_req.anchor.origin = ores::trading::domain::origin::synthetic;
    book_req.anchor.origin_reference = boost::uuids::to_string(scope.config_id);
    book_req.anchor.modified_by = slot.trader.username;
    book_req.anchor.performed_by = slot.trader.username;
    book_req.anchor.change_reason_code = std::string(population_reason);
    book_req.anchor.change_commentary = "Generated trade population.";
    book_req.booking.book_id = slot.book_id;
    book_req.booking.netting_set_id = slot.netting_set_id;
    book_req.booking.trade_date = slot.trade_date;
    book_req.booking.modified_by = slot.trader.username;
    book_req.booking.performed_by = slot.trader.username;
    book_req.booking.change_reason_code = std::string(population_reason);
    book_req.booking.change_commentary = "Generated trade population.";
    book_req.activity_type_code = "new_booking";

    /*
     * The identifier and the instrument travel with the trade. The service
     * writes them in the booking's transaction and states the trade id and the
     * activity id on each, so a trade is never on the books without them.
     */
    ores::trading::domain::trade_identifier identifier;
    identifier.id_type = std::string(internal_id_type);
    identifier.id_value = slot.internal_identifier;
    identifier.issuing_party_id = scope.party_id;
    identifier.modified_by = slot.trader.username;
    identifier.performed_by = slot.trader.username;
    identifier.change_reason_code = std::string(population_reason);
    identifier.change_commentary = "Generated trade population.";
    book_req.identifiers.push_back(identifier);

    /*
     * A family that states its rows states all of them, so the writer persists
     * what the plan carries and reads no bag for it.
     */
    if (!slot.instruments.commodity_instruments.empty()) {
        for (auto row : slot.instruments.commodity_instruments) {
            row.audit.change_reason_code = std::string(population_reason);
            row.audit.change_commentary = "Generated trade population.";
            book_req.instruments.commodity_instruments.push_back(row);
        }
        for (auto constituent : slot.instruments.commodity_basket_constituents) {
            constituent.change_reason_code = std::string(population_reason);
            constituent.change_commentary = "Generated trade population.";
            book_req.instruments.commodity_basket_constituents.push_back(constituent);
        }
    } else if (!slot.instruments.bond_instruments.empty()) {
        /*
         * The header is not the product: what makes a repo a repo, or a future a
         * future, is the row set beside it. Each is stamped flat, as its table
         * states its audit columns.
         */
        for (auto row : slot.instruments.bond_instruments) {
            row.instrument.audit.change_reason_code = std::string(population_reason);
            row.instrument.audit.change_commentary = "Generated trade population.";
            book_req.instruments.bond_instruments.push_back(row);
        }
        const auto append_bond_facts = [&](const auto& rows, auto& into) {
            for (auto row : rows) {
                row.change_reason_code = std::string(population_reason);
                row.change_commentary = "Generated trade population.";
                into.push_back(row);
            }
        };
        append_bond_facts(slot.instruments.bond_options, book_req.instruments.bond_options);
        append_bond_facts(slot.instruments.bond_forwards, book_req.instruments.bond_forwards);
        append_bond_facts(slot.instruments.bond_futures, book_req.instruments.bond_futures);
        append_bond_facts(slot.instruments.bond_repos, book_req.instruments.bond_repos);
        append_bond_facts(slot.instruments.bond_trs, book_req.instruments.bond_trs);
        append_bond_facts(slot.instruments.ascots, book_req.instruments.ascots);
    } else if (!slot.instruments.rate_instruments.empty()) {
        /*
         * The rows were built in the planner, so the writer states the audit
         * columns on each and sends. The strikes carry the reason and the
         * commentary themselves rather than an audit block.
         */
        for (auto row : slot.instruments.rate_instruments) {
            row.audit.change_reason_code = std::string(population_reason);
            row.audit.change_commentary = "Generated trade population.";
            book_req.instruments.rate_instruments.push_back(row);
        }
        for (auto leg : slot.instruments.swap_legs) {
            leg.audit.change_reason_code = std::string(population_reason);
            leg.audit.change_commentary = "Generated trade population.";
            book_req.instruments.swap_legs.push_back(leg);
        }
        for (auto amount : slot.instruments.swap_leg_amounts) {
            amount.change_reason_code = std::string(population_reason);
            amount.change_commentary = "Generated trade population.";
            book_req.instruments.swap_leg_amounts.push_back(amount);
        }
        for (auto leg_rate : slot.instruments.swap_leg_rates) {
            leg_rate.change_reason_code = std::string(population_reason);
            leg_rate.change_commentary = "Generated trade population.";
            book_req.instruments.swap_leg_rates.push_back(leg_rate);
        }
        for (auto strike : slot.instruments.instrument_strikes) {
            strike.change_reason_code = std::string(population_reason);
            strike.change_commentary = "Generated trade population.";
            book_req.instruments.instrument_strikes.push_back(strike);
        }
        const auto append_facts = [&](const auto& facts, auto& into) {
            for (auto fact : facts) {
                fact.change_reason_code = std::string(population_reason);
                fact.change_commentary = "Generated trade population.";
                into.push_back(fact);
            }
        };
        append_facts(slot.instruments.vanilla_swap_instruments,
                     book_req.instruments.vanilla_swap_instruments);
        append_facts(slot.instruments.fra_instruments, book_req.instruments.fra_instruments);
        append_facts(slot.instruments.cap_floor_instruments,
                     book_req.instruments.cap_floor_instruments);
        append_facts(slot.instruments.swaption_instruments,
                     book_req.instruments.swaption_instruments);
        append_facts(slot.instruments.callable_swap_instruments,
                     book_req.instruments.callable_swap_instruments);
        append_facts(slot.instruments.knock_out_swap_instruments,
                     book_req.instruments.knock_out_swap_instruments);
        append_facts(slot.instruments.flexi_swap_instruments,
                     book_req.instruments.flexi_swap_instruments);
        append_facts(slot.instruments.balance_guaranteed_swap_instruments,
                     book_req.instruments.balance_guaranteed_swap_instruments);
    } else if (!slot.instruments.fx_forward_instruments.empty() ||
               !slot.instruments.fx_barrier_option_instruments.empty() ||
               !slot.instruments.fx_digital_option_instruments.empty() ||
               !slot.instruments.fx_vanilla_option_instruments.empty() ||
               !slot.instruments.fx_asian_forward_instruments.empty() ||
               !slot.instruments.fx_variance_swap_instruments.empty() ||
               !slot.instruments.fx_accumulator_instruments.empty()) {
        /*
         * The rows were built in the planner, so the writer states the audit
         * columns on each and sends. The accumulator's range bounds carry the
         * reason and the commentary themselves.
         */
        const auto append_rows = [&](const auto& rows, auto& into) {
            for (auto row : rows) {
                row.audit.change_reason_code = std::string(population_reason);
                row.audit.change_commentary = "Generated trade population.";
                into.push_back(row);
            }
        };
        append_rows(slot.instruments.fx_forward_instruments,
                    book_req.instruments.fx_forward_instruments);
        append_rows(slot.instruments.fx_barrier_option_instruments,
                    book_req.instruments.fx_barrier_option_instruments);
        append_rows(slot.instruments.fx_digital_option_instruments,
                    book_req.instruments.fx_digital_option_instruments);
        append_rows(slot.instruments.fx_vanilla_option_instruments,
                    book_req.instruments.fx_vanilla_option_instruments);
        append_rows(slot.instruments.fx_asian_forward_instruments,
                    book_req.instruments.fx_asian_forward_instruments);
        append_rows(slot.instruments.fx_variance_swap_instruments,
                    book_req.instruments.fx_variance_swap_instruments);
        append_rows(slot.instruments.fx_accumulator_instruments,
                    book_req.instruments.fx_accumulator_instruments);
        for (auto bound : slot.instruments.range_bounds) {
            bound.change_reason_code = std::string(population_reason);
            bound.change_commentary = "Generated trade population.";
            book_req.instruments.range_bounds.push_back(bound);
        }
    } else if (!slot.instruments.equity_forward_instruments.empty() ||
               !slot.instruments.equity_option_instruments.empty() ||
               !slot.instruments.equity_asian_option_instruments.empty() ||
               !slot.instruments.equity_barrier_option_instruments.empty() ||
               !slot.instruments.equity_digital_option_instruments.empty() ||
               !slot.instruments.equity_swap_instruments.empty() ||
               !slot.instruments.equity_variance_swap_instruments.empty() ||
               !slot.instruments.equity_accumulator_instruments.empty() ||
               !slot.instruments.equity_position_instruments.empty()) {
        /*
         * The rows were built in the planner, so the writer states the audit
         * columns on each and sends. A child row states its audit columns flat
         * rather than as a struct, so it is stamped by its own walk.
         */
        const auto append_rows = [&](const auto& rows, auto& into) {
            for (auto row : rows) {
                row.audit.change_reason_code = std::string(population_reason);
                row.audit.change_commentary = "Generated trade population.";
                into.push_back(row);
            }
        };
        const auto append_children = [&](const auto& rows, auto& into) {
            for (auto row : rows) {
                row.change_reason_code = std::string(population_reason);
                row.change_commentary = "Generated trade population.";
                into.push_back(row);
            }
        };
        append_rows(slot.instruments.equity_forward_instruments,
                    book_req.instruments.equity_forward_instruments);
        append_rows(slot.instruments.equity_option_instruments,
                    book_req.instruments.equity_option_instruments);
        append_rows(slot.instruments.equity_asian_option_instruments,
                    book_req.instruments.equity_asian_option_instruments);
        append_rows(slot.instruments.equity_barrier_option_instruments,
                    book_req.instruments.equity_barrier_option_instruments);
        append_rows(slot.instruments.equity_digital_option_instruments,
                    book_req.instruments.equity_digital_option_instruments);
        append_rows(slot.instruments.equity_swap_instruments,
                    book_req.instruments.equity_swap_instruments);
        append_rows(slot.instruments.equity_variance_swap_instruments,
                    book_req.instruments.equity_variance_swap_instruments);
        append_rows(slot.instruments.equity_accumulator_instruments,
                    book_req.instruments.equity_accumulator_instruments);
        append_rows(slot.instruments.equity_position_instruments,
                    book_req.instruments.equity_position_instruments);
        append_children(slot.instruments.equity_basket_constituents,
                        book_req.instruments.equity_basket_constituents);
        append_children(slot.instruments.equity_swap_periods,
                        book_req.instruments.equity_swap_periods);
        append_children(slot.instruments.equity_position_option_underlyings,
                        book_req.instruments.equity_position_option_underlyings);
        append_children(slot.instruments.range_bounds, book_req.instruments.range_bounds);
    } else if (!slot.instruments.credit_instruments.empty()) {
        for (auto row : slot.instruments.credit_instruments) {
            row.audit.change_reason_code = std::string(population_reason);
            row.audit.change_commentary = "Generated trade population.";
            book_req.instruments.credit_instruments.push_back(row);
        }
    } else {
        throw trade_population_write_error(
            "The slot states no rows this writer knows how to book.");
    }

    /*
     * The booking is the person's, not the run's: the trade states its author and the
     * authorisation request the booking raises is theirs.
     */
    auto& booker = booker_ ? booker_(slot) : nats_;
    const auto reply = booker.authenticated_request(
        ores::trading::messaging::book_trade_request::nats_subject, codec.encode(book_req));
    const auto error = reply.headers.find("X-Error");
    if (error != reply.headers.end())
        throw trade_population_write_error(std::format("Booking trade {} was refused: {}",
                                                       boost::uuids::to_string(slot.trade_id),
                                                       error->second));
    auto booked = codec.decode<ores::trading::messaging::book_trade_response>(reply.data);
    if (!booked)
        throw trade_population_write_error(
            std::format("Booking trade {} answered something unreadable.",
                        boost::uuids::to_string(slot.trade_id)));
    if (booked->result.outcome != outcome::ok)
        throw trade_population_write_error(std::format("Booking trade {} failed: {}",
                                                       boost::uuids::to_string(slot.trade_id),
                                                       booked->result.message));
}


bool trading_trade_population_writer::structure_exists(const boost::uuids::uuid& structure_id) {
    const auto& codec = ores::nats::default_wire_codec();
    ores::trading::messaging::get_structure_request req;
    req.key.id = structure_id;

    const auto reply = nats_.authenticated_request(
        ores::trading::messaging::get_structure_request::nats_subject, codec.encode(req));
    const auto error = reply.headers.find("X-Error");
    if (error != reply.headers.end())
        throw trade_population_write_error(std::format("Reading structure {} was refused: {}",
                                                       boost::uuids::to_string(structure_id),
                                                       error->second));
    auto decoded = codec.decode<ores::trading::messaging::get_structure_response>(reply.data);
    if (!decoded)
        throw trade_population_write_error(
            std::format("Reading structure {} answered something unreadable.",
                        boost::uuids::to_string(structure_id)));
    return decoded->structure.has_value();
}

void trading_trade_population_writer::write_structure(
    const domain::trade_population_structure& structure,
    const domain::trade_generation_scope& /*scope*/) {
    const auto& codec = ores::nats::default_wire_codec();
    ores::trading::messaging::put_structure_request req;
    req.change.write.id = structure.structure_id;
    req.change.write.counterparty_id = structure.counterparty_id;
    req.change.write.kind = structure.kind;
    req.change.write.template_code = structure.template_code;
    req.change.precondition = {ores::utility::domain::precondition_kind::must_not_exist,
                               std::nullopt};
    req.intent.reason_code = std::string(population_reason);
    req.intent.commentary = "Grouped by the generated trade population.";

    const auto reply = nats_.authenticated_request(
        ores::trading::messaging::put_structure_request::nats_subject, codec.encode(req));
    const auto error = reply.headers.find("X-Error");
    if (error != reply.headers.end())
        throw trade_population_write_error(
            std::format("Writing structure {} was refused: {}",
                        boost::uuids::to_string(structure.structure_id),
                        error->second));
    auto decoded = codec.decode<ores::trading::messaging::put_structure_response>(reply.data);
    if (!decoded)
        throw trade_population_write_error(
            std::format("Writing structure {} answered something unreadable.",
                        boost::uuids::to_string(structure.structure_id)));
    if (decoded->result.outcome != outcome::ok)
        throw trade_population_write_error(
            std::format("Writing structure {} failed: {}",
                        boost::uuids::to_string(structure.structure_id),
                        decoded->result.message));
}

void trading_trade_population_writer::write_structure_member(
    const boost::uuids::uuid& structure_id, const domain::trade_population_slot& slot) {
    const auto& codec = ores::nats::default_wire_codec();
    ores::trading::messaging::put_structure_member_request req;
    req.change.write.trade_id = slot.trade_id;
    req.change.write.structure_id = structure_id;
    req.change.write.role = slot.structure_role;
    req.change.write.sequence_number = slot.structure_sequence;
    req.intent.reason_code = std::string(population_reason);
    req.intent.commentary = "Grouped by the generated trade population.";

    const auto reply = nats_.authenticated_request(
        ores::trading::messaging::put_structure_member_request::nats_subject, codec.encode(req));
    const auto error = reply.headers.find("X-Error");
    if (error != reply.headers.end())
        throw trade_population_write_error(
            std::format("Adding trade {} to structure {} was refused: {}",
                        boost::uuids::to_string(slot.trade_id),
                        boost::uuids::to_string(structure_id),
                        error->second));
    auto decoded =
        codec.decode<ores::trading::messaging::put_structure_member_response>(reply.data);
    if (!decoded)
        throw trade_population_write_error(
            std::format("Adding trade {} to structure {} answered something unreadable.",
                        boost::uuids::to_string(slot.trade_id),
                        boost::uuids::to_string(structure_id)));
    if (decoded->result.outcome != outcome::ok)
        throw trade_population_write_error(std::format("Adding trade {} to structure {} failed: {}",
                                                       boost::uuids::to_string(slot.trade_id),
                                                       boost::uuids::to_string(structure_id),
                                                       decoded->result.message));
}


}
