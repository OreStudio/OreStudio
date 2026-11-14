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
#ifndef ORES_SYNTHETIC_SERVICE_MESSAGING_PROVISIONING_STEP_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_MESSAGING_PROVISIONING_STEP_HANDLER_HPP

#include "ores.iam.api/workflow/population_knobs.hpp"
#include "ores.iam.api/workflow/provision_step_arguments.hpp"
#include "ores.iam.api/workflow/provision_tenant_workflow.hpp"
#include "ores.iam.client/client/service_token_provider.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.nats/domain/delivery.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/party_counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.synthetic.api/messaging/trade_generation_book_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_counterparty_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_trade_type_protocol.hpp"
#include "ores.synthetic.core/service/trade_population_planner.hpp"
#include "ores.synthetic.core/service/trade_population_service.hpp"
#include "ores.synthetic.core/trading/trading_trade_population_writer.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <cstdint>
#include <exception>
#include <optional>
#include <ranges>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief The result a generate_trades step reports, so the run's record says
 * what it acted on.
 */
struct provision_step_result {
    std::string kind;
    /// The trade generation configs the step recorded.
    std::vector<std::string> configs;
    /// The parties the configs book for.
    std::vector<std::string> parties;
    std::uint32_t trades_planned = 0;
    std::uint32_t trades_written = 0;
    std::uint32_t trades_already_present = 0;
};

/**
 * @brief The members of provision_step_result, in the order it declares them.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<provision_step_result>) {
    using ::ores::utility::codec::field;
    return ::ores::utility::codec::field_list{
        field{"kind", &provision_step_result::kind},
        field{"configs", &provision_step_result::configs},
        field{"parties", &provision_step_result::parties},
        field{"trades_planned", &provision_step_result::trades_planned},
        field{"trades_written", &provision_step_result::trades_written},
        field{"trades_already_present", &provision_step_result::trades_already_present}};
}

static_assert(::ores::utility::codec::names_every_member<provision_step_result>(),
              "provision_step_result gained a member its wire_fields does not name.");

/**
 * @brief Answers the provisioning steps whose work ores.synthetic owns.
 *
 * A provisioning run is one workflow whose steps act on different services'
 * data. The trade population belongs to synthetic, so the step that writes it
 * is dispatched here and reaches the reference data and the trading operations
 * over NATS. The command is the same payload every step of the run carries; the
 * kind says which work it is, and a kind this service does not own is refused
 * rather than worked wrongly.
 *
 * The run acts for a tenant and not for a person. The step therefore holds one
 * session inside the tenant the command names, entered as the service account,
 * and makes every request through it: the reads, the config and scope writes and
 * the booking. The trading write path takes the instrument's party from that
 * session, and the session acts as the tenant's system party.
 */
class provisioning_step_handler {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.synthetic.messaging.provisioning_step_handler");
        return instance;
    }

public:
    /**
     * @param nats           The service's raw client, which signs the request to
     *                       enter the tenant and carries every scoped request.
     * @param service_token  Supplies the service's own token, which the tenant
     *                       session is entered with.
     */
    provisioning_step_handler(ores::nats::service::client& nats,
                              ores::nats::service::nats_client::token_provider service_token)
        : nats_(nats)
        , service_token_(std::move(service_token)) {}

    /// Serves synthetic.v1.ops.provision_tenant_step.
    void step(ores::nats::delivery delivery) {
        const auto& msg = delivery.msg();

        auto wf = ores::service::messaging::workflow_step_context::from_message(nats_, msg);
        if (!wf) {
            BOOST_LOG_SEV(lg(), ores::logging::warn)
                << "Ignored a command with no workflow headers: " << msg.subject;
            std::move(delivery).reject();
            return;
        }
        try {
            // A step the engine already holds a terminal record of is answered
            // from that record, so a redelivery writes no second population.
            if (const auto cached = ores::service::messaging::check_step_idempotency(
                    nats_, wf->step_id, wf->tenant_id)) {
                ores::service::messaging::publish_step_completion(nats_,
                                                                  wf->step_id,
                                                                  wf->instance_id,
                                                                  cached->outcome,
                                                                  cached->result_json,
                                                                  cached->error_message,
                                                                  cached->log);
                std::move(delivery).commit();
                return;
            }

            const std::string_view payload(reinterpret_cast<const char*>(msg.data.data()),
                                           msg.data.size());
            auto command = ores::utility::codec::json::read_expected<
                ores::iam::workflow::provision_tenant_step_command>(payload);
            if (!command) {
                wf->fail("The provisioning step command could not be read: " +
                         std::string(command.error()));
                std::move(delivery).commit();
                return;
            }
            run(*wf, *command);
        } catch (const std::exception& e) {
            // The work could not be done, and nothing here says it never will
            // be. The step fails, which stops the run or starts its
            // compensation; a person or a resume decides whether to try again.
            BOOST_LOG_SEV(lg(), ores::logging::error)
                << "Step " << wf->step_id << " failed: " << e.what();
            wf->fail(e.what());
        }
        std::move(delivery).commit();
    }

private:
    /// One request over the tenant-scoped session, refused loudly when the
    /// server rejects it at the protocol level.
    template <typename Request>
    static typename Request::response_type ask(ores::nats::service::nats_client& nats,
                                               const Request& req) {
        const auto& codec = ores::nats::default_wire_codec();
        const auto bytes = codec.encode(req);
        const auto reply =
            nats.authenticated_request(Request::nats_subject, std::span<const std::byte>(bytes));
        if (const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
            it != reply.headers.end())
            throw std::runtime_error("Request to " + std::string(Request::nats_subject) +
                                     " rejected by server: " + it->second);

        auto result = codec.decode<typename Request::response_type>(reply.data);
        if (!result)
            throw std::runtime_error("Failed to parse response to " +
                                     std::string(Request::nats_subject) + ": " +
                                     result.error().what());
        return *result;
    }

    void run(ores::service::messaging::workflow_step_context& wf,
             const ores::iam::workflow::provision_tenant_step_command& command) {
        using ores::iam::workflow::generate_trades_step_kind;
        if (command.kind != generate_trades_step_kind) {
            wf.fail("The step kind '" + command.kind +
                    "' is not one ores.synthetic executes; its work belongs to another service.");
            return;
        }

        auto assignments =
            ores::iam::workflow::parse_trade_generation_assignments(command.arguments_json);

        /*
         * A run may state the population it wants rather than take the counts
         * its recipe states: a total and a percentage per class, which become
         * the count each config books. The floor is the codes the class's
         * planner cycles, because a config shorter than its code list states a
         * prefix and drops the rest.
         */
        ores::iam::workflow::apply_population_knobs(
            assignments,
            ores::iam::workflow::read_population_knobs(command.parameters),
            [](const std::string& asset_class) {
                return ores::synthetic::service::routed_code_count(asset_class);
            });

        // One session inside the tenant serves every request of the step, the
        // booking included. The provider enters it once and re-enters before it
        // expires, so a long population holds one service identity throughout.
        ores::nats::service::nats_client tenant_nats(
            nats_,
            ores::iam::client::make_tenant_session_provider(
                nats_, service_token_, command.tenant_id));

        provision_step_result result;
        result.kind = command.kind;

        const auto seconds_since = [](std::chrono::steady_clock::time_point from) {
            return std::chrono::duration<double>(std::chrono::steady_clock::now() - from).count();
        };
        BOOST_LOG_SEV(lg(), ores::logging::info)
            << "generate_trades: " << assignments.size() << " entr(y/ies) to write";
        std::size_t position = 0;
        for (const auto& entry : assignments) {
            ++position;
            const auto entry_started = std::chrono::steady_clock::now();
            const auto label = entry.name + " (" + std::to_string(position) + " of " +
                               std::to_string(assignments.size()) + ")";
            const auto phase = [&](std::string_view what) {
                BOOST_LOG_SEV(lg(), ores::logging::info)
                    << "generate_trades: " << label << ": " << what << " after "
                    << seconds_since(entry_started) << "s";
                wf.progress(label + ": " + std::string(what));
            };
            phase("finding the party " + entry.party);
            const auto party = find_party(tenant_nats, entry.party);
            if (!party)
                throw std::runtime_error("The tenant holds no party named '" + entry.party + "'.");

            /*
             * The population is kept in the book's own currency, so the curve
             * its bonds are discounted on is resolved from that same book. The
             * curve configuration the party published is what the resolution
             * reads, so the config names a series the party actually holds.
             */
            phase("finding the book " + entry.book);
            const auto book = find_party_book(tenant_nats, party->id, entry.book);
            if (!book)
                throw std::runtime_error("The party holds no book named '" + entry.book +
                                         "', so the population has nowhere to land.");
            phase("finding the reference curve in " + book->functional_currency);
            const auto reference_curve_id =
                find_reference_curve_id(tenant_nats, party->id, book->functional_currency);

            phase("recording the trade generation config");
            const auto config_id =
                record_trade_generation_config(tenant_nats, party->id, entry, reference_curve_id);
            phase("recording the config's scope");
            record_trade_generation_scope(tenant_nats, config_id, party->id, entry);

            /*
             * The population is booked in the step's own session, so a trade
             * names the service as its author and the run holds one identity
             * throughout.
             */
            ores::synthetic::service::trading_trade_population_writer writer(tenant_nats);
            ores::synthetic::service::trade_population_service service(writer);
            phase("writing the population of config " + boost::uuids::to_string(config_id));
            const auto population =
                service.run(config_id, [&](std::uint32_t done, std::uint32_t planned) {
                    wf.progress(label + ": trades", done, planned);
                });

            result.configs.push_back(boost::uuids::to_string(config_id));
            result.parties.push_back(boost::uuids::to_string(party->id));
            result.trades_planned += population.planned;
            result.trades_written += population.written;
            result.trades_already_present += population.already_present;

            BOOST_LOG_SEV(lg(), ores::logging::info)
                << "generate_trades: " << entry.name << " for party " << entry.party << ": planned "
                << population.planned << ", wrote " << population.written << ", already present "
                << population.already_present << ", in " << seconds_since(entry_started) << "s";
        }

        BOOST_LOG_SEV(lg(), ores::logging::info)
            << "generate_trades: every entry is written (" << result.trades_written << " written, "
            << result.trades_already_present << " already present); completing the step";
        wf.complete(ores::utility::codec::json::write(result));
    }

    /// The book the step names, among the party's own.
    ///
    /// The book is resolved by the name the party's own reference data gives
    /// it, so a recipe names no identifier that only one environment holds and
    /// a book the party does not have fails here rather than at the booking
    /// trigger, which would report it as the trade's fault.
    static std::optional<ores::refdata::domain::book>
    find_party_book(ores::nats::service::nats_client& client,
                    const boost::uuids::uuid& party_id,
                    const std::string& name) {
        ores::refdata::messaging::list_books_request request;
        request.limit = 10000;
        const auto response = ask(client, request);
        require_read(response.result, "the books of the tenant");
        for (const auto& book : response.books)
            if (book.party_id == party_id && book.name == name)
                return book;
        return std::nullopt;
    }

    /// The counterparties a party may face, each once and in a stable order.
    ///
    /// The links are the set a population should face: a counterparty outside
    /// it inserts, because nothing in the schema checks the pair, and a trade
    /// against one is wrong rather than merely unusual. The read is the
    /// party's own and carries no filter, because this component refuses a
    /// filtered read of the table rather than answering it.
    static std::vector<boost::uuids::uuid>
    party_counterparties(ores::nats::service::nats_client& client,
                         const boost::uuids::uuid& party_id) {
        ores::refdata::messaging::list_by_party_id_party_counterparties_request request;
        request.party_id = party_id;
        request.limit = 10000;

        const auto response = ask(client, request);
        require_read(response.result,
                     "the counterparty links of party " + boost::uuids::to_string(party_id));

        std::vector<boost::uuids::uuid> result;
        for (const auto& link : response.party_counterparties)
            result.push_back(link.counterparty_id);
        std::ranges::sort(result);
        const auto duplicates = std::ranges::unique(result);
        result.erase(duplicates.begin(), result.end());
        return result;
    }

    /// The counterparties a config already scopes, in a stable order.
    static std::vector<boost::uuids::uuid>
    scoped_counterparties(ores::nats::service::nats_client& client,
                          const boost::uuids::uuid& config_id) {
        ores::synthetic::messaging::list_trade_generation_counterparties_request request;
        request.limit = 10000;
        request.filter = ores::synthetic::messaging::trade_generation_counterparties_filter{
            .config_id = config_id};

        const auto response = ask(client, request);
        require_read(response.result, "the scope of config " + boost::uuids::to_string(config_id));

        std::vector<boost::uuids::uuid> result;
        for (const auto& row : response.trade_generation_counterparties)
            result.push_back(row.counterparty_id);
        std::ranges::sort(result);
        return result;
    }

    /// Whether a config already lands its population in the book named.
    static bool book_is_scoped(ores::nats::service::nats_client& client,
                               const boost::uuids::uuid& config_id,
                               const boost::uuids::uuid& book_id) {
        ores::synthetic::messaging::list_trade_generation_books_request request;
        request.limit = 1000;
        request.filter =
            ores::synthetic::messaging::trade_generation_books_filter{.config_id = config_id};
        const auto response = ask(client, request);
        require_read(response.result,
                     "the book scope of config " + boost::uuids::to_string(config_id));
        for (const auto& row : response.trade_generation_books)
            if (row.book_id == book_id)
                return true;
        return false;
    }

    /// The config the step's party already holds under the name the entry
    /// states, so a second run reuses the recipe rather than recording a
    /// second one.
    static std::optional<ores::synthetic::domain::trade_generation_config>
    find_trade_generation_config(ores::nats::service::nats_client& client,
                                 const std::string& name) {
        ores::synthetic::messaging::list_trade_generation_configs_request request;
        request.limit = 1000;
        const auto response = ask(client, request);
        require_read(response.result, "the trade generation configs of the party");
        for (const auto& config : response.trade_generation_configs)
            if (config.name == name)
                return config;
        return std::nullopt;
    }

    /// A read a step cannot act without, refused loudly.
    ///
    /// The store reports a refusal in the response and not as a transport
    /// error, so a step that read the refusal as an empty answer would record
    /// a config that scopes nobody and write nothing against it.
    static void require_read(const ores::utility::domain::result& result, const std::string& what) {
        if (result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("Reading " + what + " failed: " + result.message);
    }

    static void require_recorded(const ores::utility::domain::result& result,
                                 const std::string& what) {
        if (result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("Recording " + what + " failed: " + result.message);
    }

    /// The day the population is anchored on. An entry that states a date pins
    /// the population to it; an entry that states none anchors on the day the
    /// step runs, because the population sits relative to the present.
    static std::chrono::year_month_day
    trade_anchor_date(const ores::iam::workflow::trade_generation_assignment& assignment) {
        if (assignment.as_of_date)
            return *assignment.as_of_date;
        return std::chrono::year_month_day(
            std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now()));
    }

    /// Records one entry's config against the party it books for, and returns
    /// the id it carries. A config the party already holds under that name is
    /// left as it stands, so re-provisioning writes no version and a
    /// population that has already run keeps the anchor it ran on.
    boost::uuids::uuid
    record_trade_generation_config(ores::nats::service::nats_client& client,
                                   const boost::uuids::uuid& party_id,
                                   const ores::iam::workflow::trade_generation_assignment& entry,
                                   const std::string& reference_curve_id) {
        if (const auto existing = find_trade_generation_config(client, entry.name)) {
            BOOST_LOG_SEV(lg(), ores::logging::info)
                << "generate_trades: the config '" << entry.name << "' is already recorded.";
            return existing->id;
        }

        const auto config_id = ores::synthetic::service::config_id_for(party_id, entry.name);
        ores::synthetic::messaging::put_trade_generation_config_request request;
        request.change.write.id = config_id;
        request.change.write.name = entry.name;
        request.change.write.description =
            entry.description.empty() ? entry.name : entry.description;
        request.change.write.asset_class_code = entry.asset_class_code;
        request.change.write.min_tenor_code = entry.min_tenor_code;
        request.change.write.max_tenor_code = entry.max_tenor_code;
        request.change.write.min_notional = *entry.min_notional;
        request.change.write.max_notional = *entry.max_notional;
        request.change.write.as_of_date = trade_anchor_date(entry);
        request.change.write.seed = *entry.seed;
        request.change.write.trades_per_book = entry.trades_per_book.value_or(8);
        request.change.write.structures_per_book = entry.structures_per_book.value_or(2);
        request.change.write.enabled = true;
        request.change.write.reference_curve_id = reference_curve_id;
        request.change.precondition = {ores::utility::domain::precondition_kind::must_not_exist,
                                       std::nullopt};
        request.intent.reason_code = "system.initial_load";
        request.intent.commentary = "Recorded by the tenant's seed profile.";

        const auto response = ask(client, request);
        require_recorded(response.result, "the trade generation config '" + entry.name + "'");
        return config_id;
    }

    /// The curve a party's book is discounted on, as the oresmd URI of the
    /// market series the party's own curve configuration published for the
    /// book's currency.
    ///
    /// The curve belongs to the market, so the config names the series the
    /// party holds rather than spelling ORE's name for it: ORE's own name for
    /// one index differs between the reference data and the market's
    /// forwarding, and only the latter is what the engine resolves. A party
    /// with no curve for the book's currency gets an empty name, and the
    /// population's bonds then state none and are refused by the engine's
    /// builder rather than priced on another party's curve.
    static std::string find_reference_curve_id(ores::nats::service::nats_client& client,
                                               const boost::uuids::uuid& party_id,
                                               const std::string& currency) {
        if (currency.empty())
            return {};

        ores::marketdata::messaging::list_market_series_request request;
        request.limit = 10000;
        const auto response = ask(client, request);
        require_read(response.result, "the market series of the tenant");

        const auto prefix = "oresmd://ir/" + currency + "?type=series&instrument=";
        for (const auto& s : response.market_series) {
            if (s.party_id != party_id)
                continue;
            if (!s.oresmd_uri.starts_with(prefix))
                continue;
            if (s.oresmd_uri.find("&curve_id=") == std::string::npos)
                continue;
            return s.oresmd_uri;
        }
        return {};
    }

    /// Records the trade types the entry states, each once. A type already
    /// recorded is left alone, so the step stays re-runnable.
    static void record_trade_types(ores::nats::service::nats_client& client,
                                   const boost::uuids::uuid& config_id,
                                   const ores::iam::workflow::trade_generation_assignment& entry) {
        if (entry.trade_type_codes.empty())
            return;

        ores::synthetic::messaging::list_trade_generation_trade_types_request list;
        list.filter =
            ores::synthetic::messaging::trade_generation_trade_types_filter{.config_id = config_id};
        list.limit = 1000;
        const auto listed = ask(client, list);
        require_read(listed.result, "the trade types of '" + entry.name + "'");

        ores::synthetic::messaging::put_many_trade_generation_trade_types_request request;
        request.intent.reason_code = "system.initial_load";
        request.intent.commentary = "Trade types of the population.";
        for (const auto& code : entry.trade_type_codes) {
            const auto recorded =
                std::ranges::any_of(listed.trade_generation_trade_types,
                                    [&](const auto& row) { return row.trade_type_code == code; });
            if (recorded)
                continue;
            ores::synthetic::messaging::trade_generation_trade_type_change change;
            change.write.id = ores::synthetic::service::trade_type_scope_id_for(config_id, code);
            change.write.config_id = config_id;
            change.write.trade_type_code = code;
            change.precondition = {ores::utility::domain::precondition_kind::must_not_exist,
                                   std::nullopt};
            request.changes.push_back(std::move(change));
        }
        if (!request.changes.empty())
            require_recorded(ask(client, request).result,
                             "the trade types of '" + entry.name + "'");
    }

    /// Records the scope the entry describes: the party's book of that name,
    /// and every counterparty the party may face. A row that is already there
    /// is left alone, so the step is re-runnable and a scope that grew between
    /// two runs gains only what it gained.
    static void
    record_trade_generation_scope(ores::nats::service::nats_client& client,
                                  const boost::uuids::uuid& config_id,
                                  const boost::uuids::uuid& party_id,
                                  const ores::iam::workflow::trade_generation_assignment& entry) {
        const auto book = find_party_book(client, party_id, entry.book);
        if (!book)
            throw std::runtime_error("The party holds no book named '" + entry.book +
                                     "', so the population has nowhere to land.");

        ores::synthetic::messaging::put_many_trade_generation_books_request book_request;
        ores::synthetic::messaging::trade_generation_book_change book_change;
        book_change.write.id = ores::synthetic::service::book_scope_id_for(config_id, book->id);
        book_change.write.config_id = config_id;
        book_change.write.book_id = book->id;
        book_change.precondition = {ores::utility::domain::precondition_kind::must_not_exist,
                                    std::nullopt};
        book_request.changes.push_back(book_change);
        book_request.intent.reason_code = "system.initial_load";
        book_request.intent.commentary = "Book scope of the population.";

        if (!book_is_scoped(client, config_id, book->id))
            require_recorded(ask(client, book_request).result,
                             "the book scope of '" + entry.name + "'");

        record_trade_types(client, config_id, entry);

        const auto scoped = scoped_counterparties(client, config_id);
        std::vector<boost::uuids::uuid> missing;
        for (const auto& counterparty_id : party_counterparties(client, party_id))
            if (!std::ranges::binary_search(scoped, counterparty_id))
                missing.push_back(counterparty_id);
        if (missing.empty())
            return;

        ores::synthetic::messaging::put_many_trade_generation_counterparties_request request;
        request.intent.reason_code = "system.initial_load";
        request.intent.commentary = "Counterparty scope of the population.";
        for (const auto& counterparty_id : missing) {
            ores::synthetic::messaging::trade_generation_counterparty_change change;
            change.write.id =
                ores::synthetic::service::counterparty_scope_id_for(config_id, counterparty_id);
            change.write.config_id = config_id;
            change.write.counterparty_id = counterparty_id;
            change.precondition = {ores::utility::domain::precondition_kind::must_not_exist,
                                   std::nullopt};
            request.changes.push_back(std::move(change));
        }
        require_recorded(ask(client, request).result,
                         "the counterparty scope of '" + entry.name + "'");
    }

    /// Finds a party by the exact full name the LEI import created it with.
    static std::optional<ores::refdata::domain::party>
    find_party(ores::nats::service::nats_client& client, const std::string& full_name) {
        ores::refdata::messaging::list_parties_request req;
        req.limit = 1000;
        const auto response = ask(client, req);
        for (const auto& party : response.parties)
            if (party.full_name == full_name)
                return party;
        return std::nullopt;
    }

    ores::nats::service::client& nats_;
    ores::nats::service::nats_client::token_provider service_token_;
};

}

#endif
