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
#include "ores.refdata.api/messaging/cds_convention_protocol.hpp"
#include "ores.refdata.api/messaging/commodity_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/currency_pair_protocol.hpp"
#include "ores.refdata.api/messaging/curve_definition_protocol.hpp"
#include "ores.refdata.api/messaging/equity_underlying_protocol.hpp"
#include "ores.refdata.api/messaging/floating_index_type_protocol.hpp"
#include "ores.refdata.api/messaging/ibor_index_convention_protocol.hpp"
#include "ores.refdata.api/messaging/inflation_swap_convention_protocol.hpp"
#include "ores.refdata.api/messaging/legal_entity_protocol.hpp"
#include "ores.refdata.api/messaging/netting_set_protocol.hpp"
#include "ores.refdata.api/messaging/ois_convention_protocol.hpp"
#include "ores.refdata.api/messaging/overnight_index_convention_protocol.hpp"
#include "ores.refdata.api/messaging/swap_convention_protocol.hpp"
#include "ores.refdata.api/messaging/yield_curve_config_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_book_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_counterparty_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_trade_type_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_protocol.hpp"
#include "ores.trading.api/messaging/structure_member_protocol.hpp"
#include "ores.trading.api/messaging/structure_protocol.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include <algorithm>
#include <array>
#include <chrono>
#include <format>
#include <optional>
#include <set>
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

/*
 * States the population's reason on a row of the batch. A document container
 * states it on the instrument it holds, a header on its audit group, and a
 * child on its own flat columns.
 */
template <typename Row>
void state_reason(Row& row) {
    if constexpr (requires { row.instrument; }) {
        state_reason(row.instrument);
    } else if constexpr (requires { row.audit.change_reason_code; }) {
        row.audit.change_reason_code = std::string(population_reason);
        row.audit.change_commentary = "Generated trade population.";
    } else if constexpr (requires { row.change_reason_code; }) {
        row.change_reason_code = std::string(population_reason);
        row.change_commentary = "Generated trade population.";
    }
}

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
    /*
     * A config may state no anchor, in which case its population is meant to
     * sit relative to the present. A default-constructed date is not a date: it
     * encodes as the empty string, so an unstated anchor would reach the store
     * as a zero date and every term computed from it would be refused.
     */
    const auto today = std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now());
    scope.as_of_date =
        config.as_of_date.ok() ? config.as_of_date : std::chrono::year_month_day{today};
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

    synthetic::list_trade_generation_trade_types_request type_scope_req;
    type_scope_req.limit = page;
    synthetic::trade_generation_trade_types_filter type_scope_filter;
    type_scope_filter.config_id = config_id;
    type_scope_req.filter = type_scope_filter;
    for (const auto& row : read(type_scope_req).trade_generation_trade_types)
        scope.trade_type_codes.push_back(row.trade_type_code);

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
    const auto pairs = read(pair_req).pairs;

    /*
     * A desk books the pairs it can price, and a pair prices on a discount
     * curve in each of its currencies. So the pairs are kept to those whose two
     * currencies the party holds a yield curve for. A party that holds no yield
     * curve keeps every pair, and pricing is then the engine's to refuse.
     */
    ores::refdata::messaging::list_yield_curve_configs_request curve_config_req;
    curve_config_req.limit = page;
    std::set<std::string> priced;
    for (const auto& curve : read(curve_config_req).yield_curve_configs)
        if (curve.party_id == scope.party_id)
            priced.insert(curve.currency);
    const auto prices = [&](const std::string& currency) {
        return priced.empty() || priced.contains(currency);
    };
    for (const auto& pair : pairs)
        if (prices(pair.base_currency) && prices(pair.quote_currency))
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
     * An inflation trade names the index it observes, and the tenant resolves
     * that name through its curve configuration: the curve whose id is the
     * index. The curves are the tenant's own reference data, so the population
     * reads them where they live rather than inventing an index name.
     */
    ores::refdata::messaging::list_curve_definitions_request curve_req;
    curve_req.limit = page;
    for (const auto& curve : read(curve_req).curves)
        if (curve.section_code == "InflationCurves" && !curve.curve_id.empty())
            scope.inflation_index_codes.push_back(curve.curve_id);

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

    /*
     * A trade builder reads the terms it does not draw from the tenant's
     * conventions, so the scope carries those the builders ask for.
     */
    ores::refdata::messaging::list_ibor_index_conventions_request ibor_req;
    ibor_req.limit = page;
    scope.conventions.ibor_indices = read(ibor_req).ibor_index_conventions;
    ores::refdata::messaging::list_overnight_index_conventions_request overnight_req;
    overnight_req.limit = page;
    scope.conventions.overnight_indices = read(overnight_req).overnight_index_conventions;
    ores::refdata::messaging::list_swap_conventions_request swap_req;
    swap_req.limit = page;
    scope.conventions.swaps = read(swap_req).swap_conventions;
    ores::refdata::messaging::list_ois_conventions_request ois_req;
    ois_req.limit = page;
    scope.conventions.ois_swaps = read(ois_req).ois_conventions;
    ores::refdata::messaging::list_cds_conventions_request cds_req;
    cds_req.limit = page;
    scope.conventions.cds = read(cds_req).cds_conventions;
    ores::refdata::messaging::list_inflation_swap_conventions_request inflation_req;
    inflation_req.limit = page;
    scope.conventions.inflation_swaps = read(inflation_req).inflation_swap_conventions;

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
     * A security is named by its currency, its anchor and its term, and not by
     * a party, so two parties of the tenant can state the same one. Their
     * product rows differ -- one may draw a callable and the other a plain
     * bond -- but the issue does not: the first writer states it, and every
     * later trade references the terms already stored.
     */
    {
        ores::trading::messaging::get_bond_issue_request exists_req;
        exists_req.key.issue_id = issue_row.issue_id;
        const auto exists_reply = nats_.authenticated_request(
            ores::trading::messaging::get_bond_issue_request::nats_subject,
            codec.encode(exists_req));
        if (exists_reply.headers.find("X-Error") == exists_reply.headers.end()) {
            auto exists =
                codec.decode<ores::trading::messaging::get_bond_issue_response>(exists_reply.data);
            if (exists && exists->bond_issue) {
                issues_written_.insert(issue_row.issue_id);
                return;
            }
        }
    }

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

ores::trading::messaging::book_trade_request
trading_trade_population_writer::book_request_for(const domain::trade_population_slot& slot,
                                                  const domain::trade_generation_scope& scope) {
    ores::trading::messaging::book_trade_request book_req;
    book_req.anchor.id = slot.trade_id;
    book_req.anchor.party_id = scope.party_id;
    book_req.anchor.counterparty_id = slot.counterparty_id;
    book_req.anchor.trade_type = slot.trade_type_code;
    book_req.anchor.counterparty_scope = ores::trading::domain::counterparty_scope::external;
    /*
     * A generated trade is nobody's deal, so it states the nature that asks
     * nobody to authorise it. The trading service raises a request only for an
     * actual booking, and the trade summary reads not_required for the rest.
     */
    book_req.anchor.booking_nature = ores::trading::domain::booking_nature::test;
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
     * The plan states every row of the trade, so the request carries all of
     * them: a row the writer left behind would be a term the trade never
     * holds. Each row states why it was written, as its table states its
     * audit columns.
     *
     * The issue's own rows do not travel with the trade. They belong to the
     * security, and =ensure_issue= has already written them, so a booking that
     * carried them would ask the store to create them a second time and be
     * refused.
     */
    book_req.instruments = slot.instruments;
    book_req.instruments.bond_issue_call_dates.clear();
    book_req.instruments.bond_issue_conversion_targets.clear();
    std::size_t rows = 0;
    std::apply(
        [&](const auto&... fields) {
            (
                [&] {
                    for (auto& row : book_req.instruments.*(fields.member)) {
                        state_reason(row);
                        ++rows;
                    }
                }(),
                ...);
        },
        wire_fields(std::type_identity<ores::trading::domain::instrument_batch>{}));
    if (rows == 0)
        throw trade_population_write_error(
            "The slot states no rows this writer knows how to book.");

    return book_req;
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

    const auto book_req = book_request_for(slot, scope);

    /*
     * The booking is the person's, not the run's: the trade states the person it
     * was entered by. No authorisation request follows, because a generated
     * trade states no actual booking.
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
