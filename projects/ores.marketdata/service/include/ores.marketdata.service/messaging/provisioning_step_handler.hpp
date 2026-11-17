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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_PROVISIONING_STEP_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_PROVISIONING_STEP_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/pq/store_error.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.api/workflow/provision_step_arguments.hpp"
#include "ores.iam.api/workflow/provision_tenant_workflow.hpp"
#include "ores.iam.client/client/service_token_provider.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/curve_publish_operations_protocol.hpp"
#include "ores.marketdata.api/messaging/feed_protocol.hpp"
#include "ores.marketdata.api/messaging/fx_forward_operation_protocol.hpp"
#include "ores.marketdata.api/messaging/fx_vol_surface_operation_protocol.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.api/messaging/published_curves_operations_protocol.hpp"
#include "ores.marketdata.api/messaging/route_protocol.hpp"
#include "ores.marketdata.api/messaging/subscription_protocol.hpp"
#include "ores.marketdata.core/messaging/party_curve_blueprints.hpp"
#include "ores.marketdata.core/messaging/party_surface_blueprints.hpp"
#include "ores.marketdata.core/messaging/party_surface_hold_back.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.service/app/fx_vol_surface_calibration_service.hpp"
#include "ores.marketdata.service/app/fx_vol_surface_publish_service.hpp"
#include "ores.nats/domain/delivery.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/messaging/currency_pair_convention_protocol.hpp"
#include "ores.refdata.api/messaging/fx_vol_surface_blueprint_protocol.hpp"
#include "ores.refdata.api/messaging/fx_vol_surface_pillar_protocol.hpp"
#include "ores.refdata.api/messaging/fx_vol_surface_pillar_source_protocol.hpp"
#include "ores.refdata.api/messaging/ir_curve_bootstrap_config_protocol.hpp"
#include "ores.refdata.api/messaging/ir_curve_bootstrap_pillar_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.refdata.core/repository/fx_vol_surface_blueprint_repository.hpp"
#include "ores.refdata.core/repository/fx_vol_surface_pillar_repository.hpp"
#include "ores.refdata.core/repository/fx_vol_surface_pillar_source_repository.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_config_repository.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_pillar_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/messaging/request_retry.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cstdint>
#include <exception>
#include <functional>
#include <iterator>
#include <map>
#include <optional>
#include <ranges>
#include <set>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <thread>
#include <type_traits>
#include <utility>
#include <vector>

namespace ores::marketdata::service::messaging {

/**
 * @brief The result a marketdata step reports, so the run's record says what it
 * acted on.
 */
struct provision_step_result {
    std::string kind;
    /// The parties the step gave a subscription and a route.
    std::vector<std::string> parties;
    /// The curves a publish_party_curves step published, and the ones it could
    /// not, each with the reason it was last refused.
    std::vector<std::string> curves;
    std::vector<std::string> curves_not_published;
    /// The forward sets a publish_party_forwards step published, the ones a
    /// party already held, the pairs the service refused to build, and the sets
    /// it raised and did not publish.
    std::vector<std::string> forwards;
    std::vector<std::string> forwards_already_published;
    std::vector<std::string> forwards_skipped;
    std::vector<std::string> forwards_not_published;
    /// The surfaces a publish_party_surfaces step published, the ones a party
    /// already held, the ones held back for a decider, and the ones it could not
    /// publish.
    std::vector<std::string> surfaces;
    std::vector<std::string> surfaces_already_published;
    std::vector<std::string> surfaces_held_back;
    std::vector<std::string> surfaces_not_published;
};

/**
 * @brief The members of provision_step_result, in the order it declares them.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<provision_step_result>) {
    using ::ores::utility::codec::field;
    return ::ores::utility::codec::field_list{
        field{"kind", &provision_step_result::kind},
        field{"parties", &provision_step_result::parties},
        field{"curves", &provision_step_result::curves},
        field{"curves_not_published", &provision_step_result::curves_not_published},
        field{"forwards", &provision_step_result::forwards},
        field{"forwards_already_published", &provision_step_result::forwards_already_published},
        field{"forwards_skipped", &provision_step_result::forwards_skipped},
        field{"forwards_not_published", &provision_step_result::forwards_not_published},
        field{"surfaces", &provision_step_result::surfaces},
        field{"surfaces_already_published", &provision_step_result::surfaces_already_published},
        field{"surfaces_held_back", &provision_step_result::surfaces_held_back},
        field{"surfaces_not_published", &provision_step_result::surfaces_not_published}};
}

static_assert(::ores::utility::codec::names_every_member<provision_step_result>(),
              "provision_step_result gained a member its wire_fields does not name.");

/**
 * @brief Answers the provisioning steps whose work ores.marketdata owns.
 *
 * A provisioning run is one workflow whose steps act on different services'
 * data. The subscriptions and routes belong to marketdata, so the step that
 * writes them is dispatched here and reaches the reference data and its own
 * catalogue over NATS. The command is the same payload every step of the run
 * carries; the kind says which work it is, and a kind this service does not
 * own is refused rather than worked wrongly.
 *
 * The run acts for a tenant and not for a person. The step therefore holds one
 * session inside the tenant the command names, entered as the service account,
 * and makes every lookup through it. A subscription and a route are stamped
 * with the party they belong to, so each party the step subscribes gets a
 * session scoped to itself, and the session names no person.
 */
class provisioning_step_handler {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.marketdata.messaging.provisioning_step_handler");
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
                              const ores::database::context& ctx,
                              ores::nats::service::nats_client::token_provider service_token)
        : nats_(nats)
        , ctx_(ctx)
        , service_token_(std::move(service_token)) {}

    /**
     * @brief Serves marketdata.v1.ops.provision_tenant_step.
     *
     * The step states, for each party it names, the provider and the source
     * that party subscribes to. A party named twice takes both; a party the
     * step does not name takes the step's own provider and source; and a party
     * the tenant does not hold is refused, so a profile that names a wrong one
     * fails rather than leaving a party unsubscribed. The generators a
     * subscription reaches belong to the system, so nothing of the party's own
     * data is created by this step.
     */

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
            // from that record, so a redelivery writes no second subscription.
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
    /// One request over a scoped session, refused loudly when the server
    /// rejects it at the protocol level.
    template <typename Request>
    static typename Request::response_type
    ask(ores::nats::service::nats_client& nats,
        const Request& req,
        std::chrono::milliseconds patience = ores::nats::service::default_request_timeout) {
        const auto& codec = ores::nats::default_wire_codec();
        const auto bytes = codec.encode(req);
        const auto reply = nats.authenticated_request(
            Request::nats_subject, std::span<const std::byte>(bytes), patience);
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
        using ores::iam::workflow::give_party_surface_blueprints_step_kind;
        using ores::iam::workflow::publish_party_curves_step_kind;
        using ores::iam::workflow::publish_party_forwards_step_kind;
        using ores::iam::workflow::publish_party_surfaces_step_kind;
        using ores::iam::workflow::subscribe_market_data_step_kind;
        if (command.kind == subscribe_market_data_step_kind) {
            subscribe_market_data(wf, command);
            return;
        }
        if (command.kind == publish_party_curves_step_kind) {
            publish_party_curves(wf, command);
            return;
        }
        if (command.kind == give_party_surface_blueprints_step_kind) {
            give_party_surface_blueprints(wf, command);
            return;
        }
        if (command.kind == publish_party_forwards_step_kind) {
            publish_party_forwards(wf, command);
            return;
        }
        if (command.kind == publish_party_surfaces_step_kind) {
            publish_party_surfaces(wf, command);
            return;
        }
        wf.fail("The step kind '" + command.kind +
                "' is not one ores.marketdata executes; its work belongs to another service.");
    }

    void subscribe_market_data(ores::service::messaging::workflow_step_context& wf,
                               const ores::iam::workflow::provision_tenant_step_command& command) {
        const auto arguments =
            ores::iam::workflow::parse_market_data_arguments(command.arguments_json);

        // The providers and the parties are found across the tenant; everything
        // that follows belongs to one party and runs in that party's session.
        ores::nats::service::nats_client tenant_nats(
            nats_,
            ores::iam::client::make_tenant_session_provider(
                nats_, service_token_, command.tenant_id));

        // One lookup for each provider and source named, so a party on another
        // feed costs no more reads than the parties on the step's own.
        std::map<std::string, boost::uuids::uuid> sources;
        const auto source_of = [&](const std::string& provider, const std::string& source) {
            const auto key = provider + "/" + source;
            const auto found = sources.find(key);
            if (found != sources.end())
                return found->second;
            const auto id = find_source_id(tenant_nats, provider, source);
            sources.emplace(key, id);
            return id;
        };
        // The step's own source is looked up first, so a profile that names a
        // wrong one fails before any party is subscribed.
        source_of(arguments.provider, arguments.source);

        std::vector<std::string> subscribed;
        std::set<std::string> matched;
        std::uint32_t offset = 0;
        constexpr std::uint32_t page_size = 1000;
        while (true) {
            ores::refdata::messaging::list_parties_request request;
            request.offset = offset;
            request.limit = page_size;
            const auto page = ask(tenant_nats, request).parties;
            for (const auto& party : page) {
                const auto id_text = boost::uuids::to_string(party.id);
                for (const auto& assignment : arguments.parties)
                    if (assignment.party == id_text || assignment.party == party.full_name)
                        matched.insert(assignment.party);
                const auto feeds =
                    ores::iam::workflow::feeds_of_party(arguments, id_text, party.full_name);
                // A subscription and a route name the party they belong to, so
                // they are written in a session scoped to that party. A party
                // with no feed is given no session.
                if (!feeds.empty()) {
                    ores::nats::service::nats_client party_nats(
                        nats_,
                        ores::iam::client::make_tenant_session_provider(nats_,
                                                                        service_token_,
                                                                        command.tenant_id,
                                                                        std::chrono::seconds{60},
                                                                        id_text));
                    std::string named;
                    for (const auto& feed : feeds) {
                        subscribe_party(party_nats,
                                        party.id,
                                        feed.provider,
                                        source_of(feed.provider, feed.source));
                        named += (named.empty() ? "" : ", ") + feed.provider;
                    }
                    wf.record(party.full_name + ": subscribed to " + named);
                }
                subscribed.push_back(id_text);
            }
            if (page.size() < page_size)
                break;
            offset += page_size;
        }

        for (const auto& assignment : arguments.parties)
            if (!matched.contains(assignment.party))
                throw std::runtime_error("The tenant holds no party the market data step names: '" +
                                         assignment.party + "'.");

        BOOST_LOG_SEV(lg(), ores::logging::info) << "subscribe_market_data: " << subscribed.size()
                                                 << " part(y/ies) subscribed; completing the step";
        // The result states the kind and the parties subscribed: nothing else of
        // the shared result shape is filled for this kind, and no reader of it
        // expects more.
        wf.complete(ores::utility::codec::json::write(
            provision_step_result{.kind = command.kind, .parties = subscribed}));
    }

    /// The id of the source the step names, which must belong to the provider.
    static boost::uuids::uuid find_source_id(ores::nats::service::nats_client& client,
                                             const std::string& provider,
                                             const std::string& source) {
        ores::marketdata::messaging::list_feeds_request request;
        request.limit = 1000;
        const auto response = ask(client, request);
        if (response.result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("The market data feeds were not read: " +
                                     response.result.message);
        for (const auto& candidate : response.feeds)
            if (candidate.name == source && candidate.publisher_code == provider)
                return candidate.id;
        throw std::runtime_error("The catalogue holds no feed '" + source + "' of the provider '" +
                                 provider + "'.");
    }

    /// Gives one party a subscription to a provider and a route to a source.
    /// The session speaks as the party, because the service stamps the party
    /// from the session. A subscription or a route the party already holds is
    /// left as it is, so the step can run again.
    static void subscribe_party(ores::nats::service::nats_client& party_client,
                                const boost::uuids::uuid& party_id,
                                const std::string& provider,
                                const boost::uuids::uuid& source_id) {
        const auto party_text = boost::uuids::to_string(party_id);
        boost::uuids::random_generator uuid_gen;

        ores::marketdata::messaging::list_subscriptions_request held_subscriptions;
        held_subscriptions.limit = 1000;
        const auto subscriptions = ask(party_client, held_subscriptions);
        if (subscriptions.result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("The subscriptions of party " + party_text +
                                     " were not read: " + subscriptions.result.message);
        // The session of a parent party also sees the rows of its child parties, so the
        // check names the party and a child's subscription does not count as the parent's.
        const bool has_subscription =
            std::ranges::any_of(subscriptions.subscriptions, [&](const auto& s) {
                return s.party_id == party_id && s.provider_code == provider;
            });
        if (!has_subscription) {
            ores::marketdata::messaging::put_subscription_request put;
            put.change.write.id = uuid_gen();
            put.change.write.party_id = party_id;
            put.change.write.provider_code = provider;
            put.change.write.display = true;
            put.change.write.non_display = true;
            put.change.write.derived = true;
            put.change.precondition.kind = ores::utility::domain::precondition_kind::any;
            put.intent.reason_code = "system.new_record";
            put.intent.commentary =
                "Created by provisioning: the party takes the shared market data";
            const auto saved = ask(party_client, put);
            if (saved.result.outcome != ores::utility::domain::outcome::ok)
                throw std::runtime_error("The subscription of party " + party_text + " to '" +
                                         provider + "' was not saved: " + saved.result.message);
        }

        ores::marketdata::messaging::list_routes_request held_routes;
        held_routes.limit = 1000;
        const auto routes = ask(party_client, held_routes);
        if (routes.result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("The routes of party " + party_text +
                                     " were not read: " + routes.result.message);
        const bool has_route = std::ranges::any_of(routes.routes, [&](const auto& r) {
            return r.party_id == party_id && r.publisher_code == provider;
        });
        if (!has_route) {
            ores::marketdata::messaging::put_route_request put;
            put.change.write.id = uuid_gen();
            put.change.write.party_id = party_id;
            put.change.write.publisher_code = provider;
            put.change.write.asset_class = "fx";
            put.change.write.feed_id = source_id;
            put.change.precondition.kind = ores::utility::domain::precondition_kind::any;
            put.intent.reason_code = "system.new_record";
            put.intent.commentary =
                "Created by provisioning: the party takes the shared market data";
            const auto saved = ask(party_client, put);
            if (saved.result.outcome != ores::utility::domain::outcome::ok)
                throw std::runtime_error("The route of party " + party_text + " to '" + provider +
                                         "' was not saved: " + saved.result.message);
        }
    }


    /// The instant a step's requests must stop being tried.
    ///
    /// The engine gives a step a budget of standing still, not of wall clock
    /// time, so this is that budget read from the moment of the call.
    static std::chrono::steady_clock::time_point deadline_of(const std::string& kind) {
        return std::chrono::steady_clock::now() +
               ores::iam::workflow::words_for_step_kind(kind).timeout;
    }

    /// Records a retry attempt where a person watching the run can read it.
    static std::function<void(const std::string&)>
    attempts_recorded_in(std::vector<ores::workflow::messaging::step_log_entry>& step_log,
                         const ores::service::messaging::workflow_step_context& wf) {
        return [&step_log, &wf](const std::string& line) {
            step_log.push_back({.level = ores::workflow::messaging::step_log_level::info,
                                .message = line,
                                .context = {}});
            wf.progress(line);
        };
    }

    /// The party the step names, by its exact full name, one page at a time.
    static std::optional<ores::refdata::domain::party>
    find_party(ores::nats::service::nats_client& client, const std::string& full_name) {
        std::uint32_t offset = 0;
        constexpr std::uint32_t page_size = 1000;
        while (true) {
            ores::refdata::messaging::list_parties_request request;
            request.offset = offset;
            request.limit = page_size;
            const auto response = ask(client, request);
            if (response.result.outcome != ores::utility::domain::outcome::ok)
                throw std::runtime_error("The parties of the tenant were not read: " +
                                         response.result.message);
            for (const auto& party : response.parties)
                if (party.full_name == full_name)
                    return party;
            if (response.parties.size() < page_size)
                return std::nullopt;
            offset += page_size;
        }
    }

    /**
     * @brief The curve blueprints the installation keeps, with every series they
     * name.
     *
     * The blueprints are the system tenant's, and this reads them as the service
     * in that tenant, where they live: no session inside the system tenant is
     * entered and no person is impersonated. A tenant's own copies are not in
     * this read, so a second run copies the template and never a copy of a copy.
     */
    ores::marketdata::messaging::curve_blueprints read_curve_catalogue() {
        const auto system = ores::database::service::tenant_context::with_system_tenant(ctx_);
        ores::marketdata::messaging::curve_blueprints catalogue;
        catalogue.configs =
            ores::refdata::repository::ir_curve_bootstrap_config_repository{}.read_latest(system);
        if (catalogue.configs.empty())
            return catalogue;

        std::set<boost::uuids::uuid> config_ids;
        std::vector<std::string> series_ids;
        for (const auto& c : catalogue.configs) {
            config_ids.insert(c.id);
            series_ids.push_back(boost::uuids::to_string(c.output_series_id));
        }
        // The system tenant holds the catalogue and nothing else of these two
        // tables, so the pillars are read whole and kept by their blueprint.
        for (auto& p :
             ores::refdata::repository::ir_curve_bootstrap_pillar_repository{}.read_latest(
                 system)) {
            if (!config_ids.contains(p.bootstrap_config_id))
                continue;
            series_ids.push_back(boost::uuids::to_string(p.market_series_id));
            catalogue.pillars.push_back(std::move(p));
        }
        std::ranges::sort(series_ids);
        series_ids.erase(std::unique(series_ids.begin(), series_ids.end()), series_ids.end());
        catalogue.series = ores::marketdata::repository::market_series_repository{}.read_latest(
            system, series_ids);
        return catalogue;
    }

    /// The series and curve configs one party holds, one page at a time.
    ///
    /// A page that comes back short is the last: a party with more rows than a
    /// page holds is read whole, because a holding mistaken for absent would be
    /// written a second time under new ids.
    static ores::marketdata::messaging::party_curve_holdings
    read_party_curve_holdings(ores::nats::service::nats_client& client,
                              const boost::uuids::uuid& party_id) {
        constexpr std::uint32_t page_size = 1000;
        ores::marketdata::messaging::party_curve_holdings holdings;
        holdings.party_id = party_id;
        for (std::uint32_t offset = 0;; offset += page_size) {
            ores::marketdata::messaging::list_market_series_request series;
            series.offset = offset;
            series.limit = page_size;
            const auto page = ask(client, series).market_series;
            holdings.series.insert(holdings.series.end(), page.begin(), page.end());
            if (page.size() < page_size)
                break;
        }
        for (std::uint32_t offset = 0;; offset += page_size) {
            ores::refdata::messaging::list_ir_curve_bootstrap_configs_request configs;
            configs.offset = offset;
            configs.limit = page_size;
            const auto page = ask(client, configs).bootstrap_configs;
            for (const auto& c : page)
                if (c.party_id == party_id)
                    holdings.config_ids.insert(c.id);
            if (page.size() < page_size)
                break;
        }
        return holdings;
    }

    /// The most rows one write request may carry.
    ///
    /// A party's plan is one act and may hold more rows than a request does, so
    /// the rows go in as many requests as the store needs rather than being
    /// refused for the size of the plan.
    static constexpr std::size_t max_rows_per_write = 1000;

    /// Sends @p changes in batches, @p what naming them in a failure.
    template <typename Request, typename Change>
    static void write_in_batches(ores::nats::service::nats_client& client,
                                 const std::vector<Change>& changes,
                                 std::string_view reason,
                                 std::string_view commentary,
                                 std::string_view what,
                                 const std::string& party_name) {
        for (std::size_t from = 0; from < changes.size(); from += max_rows_per_write) {
            const auto to = std::min(from + max_rows_per_write, changes.size());
            Request put;
            put.changes.assign(changes.begin() + static_cast<std::ptrdiff_t>(from),
                               changes.begin() + static_cast<std::ptrdiff_t>(to));
            put.intent.reason_code = std::string(reason);
            put.intent.commentary = std::string(commentary);
            const auto saved = ask(client, put);
            if (saved.result.outcome != ores::utility::domain::outcome::ok)
                throw std::runtime_error("The " + std::string(what) + " of '" + party_name +
                                         "' were not saved: " + saved.result.message);
        }
    }

    /// Writes the rows a curve plan names, series first, because a config names
    /// its output series and a pillar names its config.
    static void write_party_curve_plan(ores::nats::service::nats_client& client,
                                       const ores::marketdata::messaging::party_curve_plan& plan,
                                       const std::string& party_name) {
        constexpr std::string_view commentary =
            "Created by provisioning: the party's own copy of the system's curve blueprint";
        constexpr std::string_view reason = "system.new_record";
        const ores::utility::domain::precondition any{
            .kind = ores::utility::domain::precondition_kind::any};

        std::vector<ores::marketdata::messaging::market_series_change> series;
        for (const auto& s : plan.series_to_write)
            series.push_back({.write = {.id = s.id,
                                        .party_id = s.party_id,
                                        .oresmd_uri = s.oresmd_uri,
                                        .provider_code = s.provider_code,
                                        .series_subclass = s.series_subclass,
                                        .derivation_kind = s.derivation_kind,
                                        .derivation_config_id = s.derivation_config_id,
                                        .derivation_config_version = s.derivation_config_version},
                              .precondition = any});
        write_in_batches<ores::marketdata::messaging::put_many_market_series_request>(
            client, series, reason, commentary, "output series", party_name);

        std::vector<ores::refdata::messaging::ir_curve_bootstrap_config_change> configs;
        for (const auto& c : plan.configs_to_write)
            configs.push_back({.write = {.id = c.id,
                                         .output_series_id = c.output_series_id,
                                         .name = c.name,
                                         .currency_code = c.currency_code,
                                         .index_tenor = c.index_tenor,
                                         .curve_family_role = c.curve_family_role,
                                         .discount_curve_config_id = c.discount_curve_config_id,
                                         .interpolation_method = c.interpolation_method,
                                         .day_count_convention = c.day_count_convention,
                                         .tenor_convention_code = c.tenor_convention_code,
                                         .split_tenor_code = c.split_tenor_code},
                               .precondition = any});
        write_in_batches<ores::refdata::messaging::put_many_ir_curve_bootstrap_configs_request>(
            client, configs, reason, commentary, "curve configs", party_name);

        std::vector<ores::refdata::messaging::ir_curve_bootstrap_pillar_change> pillars;
        for (const auto& p : plan.pillars_to_write)
            pillars.push_back({.write = {.id = p.id,
                                         .party_id = p.party_id,
                                         .bootstrap_config_id = p.bootstrap_config_id,
                                         .sequence_index = p.sequence_index,
                                         .start_tenor_code = p.start_tenor_code,
                                         .end_tenor_code = p.end_tenor_code,
                                         .curve_role_code = p.curve_role_code,
                                         .market_series_id = p.market_series_id},
                               .precondition = any});
        write_in_batches<ores::refdata::messaging::put_many_ir_curve_bootstrap_pillars_request>(
            client, pillars, reason, commentary, "curve pillars", party_name);
    }

    /// The bootstrap configs a party already has a current published curve for,
    /// one page at a time.
    static std::set<std::string> published_configs_of(ores::nats::service::nats_client& client,
                                                      const boost::uuids::uuid& party_id) {
        constexpr std::uint32_t page_size = 1000;
        std::set<std::string> out;
        for (std::uint32_t offset = 0;; offset += page_size) {
            ores::marketdata::messaging::list_published_curves_request request;
            request.party_id = boost::uuids::to_string(party_id);
            request.offset = offset;
            request.limit = page_size;
            const auto page = ask(client, request).sets;
            for (const auto& set : page)
                out.insert(set.bootstrap_config_id);
            if (page.size() < page_size)
                break;
        }
        return out;
    }

    /// The set one curve publish is about, once an earlier attempt wrote it.
    static std::optional<ores::marketdata::messaging::published_curve_set>
    published_curve_of(ores::nats::service::nats_client& client,
                       const boost::uuids::uuid& party_id,
                       const std::string& config_id,
                       std::chrono::system_clock::time_point as_of,
                       std::chrono::milliseconds patience) {
        ores::marketdata::messaging::list_published_curves_request request;
        request.party_id = boost::uuids::to_string(party_id);
        request.include_candidates = true;
        request.limit = 1000;
        const auto wanted = std::chrono::floor<std::chrono::microseconds>(as_of);
        for (const auto& set : ask(client, request, patience).sets) {
            if (set.bootstrap_config_id != config_id)
                continue;
            if (std::chrono::floor<std::chrono::microseconds>(set.as_of) != wanted)
                continue;
            // A withdrawn or superseded set is not the one this publish is
            // about: the work it names has since been undone or replaced.
            if (set.state_code == "withdrawn" || set.state_code == "superseded")
                continue;
            return set;
        }
        return std::nullopt;
    }

    /// What one round of curve publishes did, a line for each curve.
    struct curve_publish_outcome {
        std::vector<std::string> published;
        std::vector<std::string> not_published;
    };

    /**
     * @brief Previews and publishes each curve, and asks again while its quotes
     * have not arrived.
     *
     * A curve whose preview fails is asked again every ten seconds for two
     * minutes, because its feeds may not have ticked yet. A curve whose request
     * waits for a person is not asked again, since a second ask would raise a
     * second request for one act.
     */
    curve_publish_outcome
    publish_curves(const ores::service::messaging::workflow_step_context& wf,
                   ores::nats::service::nats_client& client,
                   const boost::uuids::uuid& party_id,
                   const std::vector<std::pair<std::string, std::string>>& curves,
                   std::string_view reason,
                   std::string_view label,
                   std::vector<ores::workflow::messaging::step_log_entry>& step_log,
                   std::chrono::steady_clock::time_point deadline) {
        constexpr auto curve_quote_patience = std::chrono::minutes{2};
        constexpr auto curve_retry_interval = std::chrono::seconds{10};
        constexpr auto work_request_deadline = std::chrono::seconds{120};

        std::map<std::string, std::string> pending;
        std::map<std::string, std::string> names;
        for (const auto& [id, name] : curves) {
            pending.emplace(id, "not yet asked");
            names.emplace(id, name);
        }

        curve_publish_outcome outcome;
        const auto started = std::chrono::steady_clock::now();
        const auto give_up = started + curve_quote_patience;
        while (!pending.empty()) {
            for (auto it = pending.begin(); it != pending.end();) {
                // The as-of moves with each attempt, so quotes a feed writes
                // while the step waits are inside the snapshot the next attempt
                // reads. A preview and its publish state the same instant.
                const auto as_of = std::chrono::system_clock::now();
                const auto curve_work = std::string(label) + ": " + names.at(it->first);
                wf.progress(curve_work,
                            static_cast<std::int64_t>(curves.size() - pending.size()),
                            static_cast<std::int64_t>(curves.size()));

                ores::marketdata::messaging::compute_curve_request preview;
                preview.bootstrap_config_id = it->first;
                preview.as_of = as_of;
                const auto computed = ores::service::messaging::call_with_retry(
                    [&](const ores::service::messaging::retry_attempt& attempt)
                        -> ores::marketdata::messaging::compute_curve_response {
                        const auto patience =
                            std::max(attempt.budget_left, std::chrono::milliseconds{1});
                        return ask(client,
                                   preview,
                                   std::min(std::chrono::duration_cast<std::chrono::milliseconds>(
                                                work_request_deadline),
                                            patience));
                    },
                    {.deadline = deadline,
                     .what = curve_work + " curve bootstrap",
                     .note = attempts_recorded_in(step_log, wf)});
                if (!computed.success) {
                    it->second = computed.message;
                    ++it;
                    continue;
                }

                ores::marketdata::messaging::publish_curve_request publish;
                publish.bootstrap_config_id = it->first;
                publish.as_of = as_of;
                publish.reason = std::string(reason);
                const auto published = ores::service::messaging::call_with_retry(
                    [&](const ores::service::messaging::retry_attempt& attempt)
                        -> ores::marketdata::messaging::publish_curve_response {
                        return ores::service::messaging::do_unless_already_done(
                            attempt,
                            [&]() -> std::optional<
                                      ores::marketdata::messaging::publish_curve_response> {
                                const auto found = published_curve_of(
                                    client, party_id, it->first, as_of, attempt.budget_left);
                                if (!found)
                                    return std::nullopt;
                                return ores::marketdata::messaging::publish_curve_response{
                                    .success = true,
                                    .message = "Published by an earlier attempt.",
                                    .approval_request_id = found->request_id,
                                    .state_code = found->state_code,
                                    .published_at = found->published_at,
                                    .published_by = found->published_by};
                            },
                            [&] {
                                const auto patience =
                                    std::max(attempt.budget_left, std::chrono::milliseconds{1});
                                return ask(
                                    client,
                                    publish,
                                    std::min(std::chrono::duration_cast<std::chrono::milliseconds>(
                                                 work_request_deadline),
                                             patience));
                            });
                    },
                    {.deadline = deadline,
                     .what = curve_work + " curve publish",
                     .note = attempts_recorded_in(step_log, wf)});
                if (published.success && published.published_at) {
                    BOOST_LOG_SEV(lg(), ores::logging::info)
                        << label << ": published " << names.at(it->first) << " at "
                        << ores::platform::time::datetime::to_iso8601_utc(*published.published_at)
                        << " by " << published.published_by << ", " << computed.points.size()
                        << " point(s)";
                    outcome.published.push_back(
                        names.at(it->first) + " as of " +
                        ores::platform::time::datetime::to_iso8601_utc(as_of));
                } else if (published.success) {
                    BOOST_LOG_SEV(lg(), ores::logging::warn)
                        << label << ": " << names.at(it->first) << " waits for approval: request "
                        << published.approval_request_id << " is " << published.state_code;
                    outcome.not_published.push_back(
                        names.at(it->first) + ": the request " + published.approval_request_id +
                        " is " + published.state_code + " and no person has answered it");
                } else {
                    // The preview passed and the publish did not, which no wait
                    // puts right: the reason is reported and the curve is done.
                    BOOST_LOG_SEV(lg(), ores::logging::warn)
                        << label << ": did not publish " << names.at(it->first) << ": "
                        << published.message;
                    outcome.not_published.push_back(names.at(it->first) + ": " + published.message);
                }
                it = pending.erase(it);
            }
            if (pending.empty() || std::chrono::steady_clock::now() >= give_up)
                break;
            const auto waited = std::chrono::duration_cast<std::chrono::seconds>(
                                    std::chrono::steady_clock::now() - started)
                                    .count();
            const auto waiting = std::to_string(pending.size()) + " curve(s) have no quotes yet (" +
                                 std::to_string(waited) + "s of " +
                                 std::to_string(curve_quote_patience.count() * 60) +
                                 "s), asking again in " +
                                 std::to_string(curve_retry_interval.count()) + "s";
            BOOST_LOG_SEV(lg(), ores::logging::info) << label << ": " << waiting;
            step_log.push_back({.level = ores::workflow::messaging::step_log_level::info,
                                .message = waiting,
                                .context = {}});
            std::this_thread::sleep_for(curve_retry_interval);
        }

        for (const auto& [id, last_reason] : pending) {
            BOOST_LOG_SEV(lg(), ores::logging::warn)
                << label << ": did not publish " << names.at(id) << ": " << last_reason;
            outcome.not_published.push_back(names.at(id) + ": " + last_reason);
        }
        return outcome;
    }

    /**
     * @brief Gives each party the step names its own copy of the curve
     * blueprints, and publishes the curves it can bootstrap.
     *
     * A curve is derived data, so it belongs to the party that bootstraps it and
     * not to the system. The party copies every blueprint the catalogue holds,
     * and each pillar reads the party's own series of the same item from the
     * same provider. The step waits up to three minutes for a party's quotes to
     * reach its series, then publishes each curve the party has not published
     * yet. Under the tenant's auto-approval the publish closes at once.
     *
     * A curve that does not publish fails the step, named with its party. A
     * second run writes no row twice and publishes only the curves that are
     * still missing.
     */
    void publish_party_curves(ores::service::messaging::workflow_step_context& wf,
                              const ores::iam::workflow::provision_tenant_step_command& command) {
        constexpr auto quote_patience = std::chrono::minutes{3};
        constexpr auto retry_interval = std::chrono::seconds{10};
        constexpr std::string_view publish_reason =
            "Provisioning bootstraps the party's own funding curve from the quotes its "
            "subscriptions deliver.";

        const auto parties =
            ores::iam::workflow::parse_party_curves_arguments(command.arguments_json);

        ores::nats::service::nats_client tenant_nats(
            nats_,
            ores::iam::client::make_tenant_session_provider(
                nats_, service_token_, command.tenant_id));
        const auto catalogue = read_curve_catalogue();
        if (catalogue.configs.empty()) {
            wf.fail("The system tenant holds no curve blueprint to copy.");
            return;
        }

        provision_step_result result;
        result.kind = command.kind;
        std::vector<ores::workflow::messaging::step_log_entry> step_log;
        for (const auto& party_name : parties) {
            const auto party = find_party(tenant_nats, party_name);
            if (!party)
                throw std::runtime_error("The tenant holds no party named '" + party_name + "'.");

            // A curve is the party's own work, so it is read and written in a
            // session scoped to that party.
            const auto party_text = boost::uuids::to_string(party->id);
            ores::nats::service::nats_client party_nats(
                nats_,
                ores::iam::client::make_tenant_session_provider(nats_,
                                                                service_token_,
                                                                command.tenant_id,
                                                                std::chrono::seconds{60},
                                                                party_text));

            const auto started = std::chrono::steady_clock::now();
            auto plan = ores::marketdata::messaging::plan_party_curves(
                catalogue, read_party_curve_holdings(party_nats, party->id));
            while (!plan.waiting.empty() &&
                   std::chrono::steady_clock::now() < started + quote_patience) {
                const auto line = party_name + ": " + std::to_string(plan.waiting.size()) +
                                  " curve(s) wait for their quotes, asking again in " +
                                  std::to_string(retry_interval.count()) + "s";
                BOOST_LOG_SEV(lg(), ores::logging::info) << "publish_party_curves: " << line;
                // The count ready moves only as quotes arrive, so waiting is
                // seen to progress or to stand still.
                wf.progress(party_name + ": curves with their quotes",
                            static_cast<std::int64_t>(plan.ready.size()),
                            static_cast<std::int64_t>(plan.ready.size() + plan.waiting.size()));
                step_log.push_back({.level = ores::workflow::messaging::step_log_level::info,
                                    .message = line,
                                    .context = {}});
                std::this_thread::sleep_for(retry_interval);
                plan = ores::marketdata::messaging::plan_party_curves(
                    catalogue, read_party_curve_holdings(party_nats, party->id));
            }
            write_party_curve_plan(party_nats, plan, party_name);
            for (const auto& waiting : plan.waiting)
                result.curves_not_published.push_back(party_name + ": " + waiting);

            const auto published = published_configs_of(party_nats, party->id);
            std::vector<std::pair<std::string, std::string>> curves;
            for (const auto& curve : plan.ready) {
                const auto id = boost::uuids::to_string(curve.config_id);
                if (!published.contains(id))
                    curves.emplace_back(id, curve.name);
            }
            auto outcome = publish_curves(wf,
                                          party_nats,
                                          party->id,
                                          curves,
                                          publish_reason,
                                          "publish_party_curves " + party_name,
                                          step_log,
                                          deadline_of(command.kind));
            BOOST_LOG_SEV(lg(), ores::logging::info)
                << "publish_party_curves: " << party_name << ": published "
                << outcome.published.size() << ", already published "
                << plan.ready.size() - curves.size() << ", not published "
                << outcome.not_published.size() + plan.waiting.size();
            for (auto& line : outcome.published)
                result.curves.push_back(party_name + ": " + line);
            for (auto& line : outcome.not_published)
                result.curves_not_published.push_back(party_name + ": " + line);
            result.parties.push_back(party_text);
        }

        for (const auto& not_published : result.curves_not_published)
            step_log.push_back({.level = ores::workflow::messaging::step_log_level::warn,
                                .message = not_published,
                                .context = {}});
        if (!result.curves_not_published.empty()) {
            std::string reason = std::to_string(result.curves_not_published.size()) +
                                 " party curve(s) were not published. A rerun is safe: it "
                                 "writes no row twice and publishes only the missing curves:";
            for (const auto& line : result.curves_not_published)
                reason += " " + line + ";";
            wf.fail(reason, step_log);
            return;
        }
        wf.complete(ores::utility::codec::json::write(result), step_log);
    }

    /// The pair codes of every currency pair convention the tenant holds, one
    /// page at a time.
    static std::vector<std::string>
    pair_convention_codes(ores::nats::service::nats_client& client) {
        constexpr std::uint32_t page_size = 1000;
        std::vector<std::string> out;
        for (std::uint32_t offset = 0;; offset += page_size) {
            ores::refdata::messaging::list_currency_pair_conventions_request request;
            request.offset = offset;
            request.limit = page_size;
            const auto page = ask(client, request).conventions;
            for (const auto& convention : page)
                out.push_back(convention.pair_code);
            if (page.size() < page_size)
                break;
        }
        std::ranges::sort(out);
        return out;
    }

    /**
     * @brief Publishes each party's FX forward sets.
     *
     * A forward set is derived data, so it belongs to the party whose spot and
     * funding curves it reads. Each party is asked for every currency pair
     * convention the tenant holds, and the forward service decides which it can
     * build: a pair it refuses before raising a request is skipped, with its
     * reason. Under the tenant's auto-approval the raise closes and the set
     * publishes at once.
     *
     * A set raised and not published fails the step, named with its party. A
     * second run publishes only the pairs that are still missing.
     */
    void publish_party_forwards(ores::service::messaging::workflow_step_context& wf,
                                const ores::iam::workflow::provision_tenant_step_command& command) {
        constexpr std::string_view publish_reason =
            "Provisioning publishes the party's FX forward set from its spot and its published "
            "funding curves.";

        const auto parties =
            ores::iam::workflow::parse_party_forwards_arguments(command.arguments_json);

        ores::nats::service::nats_client tenant_nats(
            nats_,
            ores::iam::client::make_tenant_session_provider(
                nats_, service_token_, command.tenant_id));
        const auto pairs = pair_convention_codes(tenant_nats);
        if (pairs.empty()) {
            wf.fail("The tenant holds no currency pair convention to publish a forward set for.");
            return;
        }

        provision_step_result result;
        result.kind = command.kind;
        std::vector<ores::workflow::messaging::step_log_entry> step_log;
        for (const auto& party_name : parties) {
            const auto party = find_party(tenant_nats, party_name);
            if (!party)
                throw std::runtime_error("The tenant holds no party named '" + party_name + "'.");

            // A forward set is the party's own work, so it is read and
            // published in a session scoped to that party.
            const auto party_text = boost::uuids::to_string(party->id);
            ores::nats::service::nats_client party_nats(
                nats_,
                ores::iam::client::make_tenant_session_provider(nats_,
                                                                service_token_,
                                                                command.tenant_id,
                                                                std::chrono::seconds{60},
                                                                party_text));

            std::size_t published = 0;
            std::size_t held = 0;
            const auto step_deadline = deadline_of(command.kind);
            std::int64_t done = 0;
            for (const auto& pair : pairs) {
                const auto as_of = std::chrono::system_clock::now();
                const auto line = party_name + ": " + pair;
                wf.progress(line, done++, static_cast<std::int64_t>(pairs.size()));

                // The read answers the newest set at or before the as-of, so a
                // set an earlier run published is found by a later one.
                ores::marketdata::messaging::get_published_fx_forward_request published_before;
                published_before.pair_code = pair;
                published_before.as_of = as_of;
                if (ask(party_nats, published_before).success) {
                    ++held;
                    result.forwards_already_published.push_back(line);
                    continue;
                }

                ores::marketdata::messaging::publish_fx_forward_request publish;
                publish.pair_code = pair;
                publish.as_of = as_of;
                publish.reason = std::string(publish_reason);
                const auto answer = ores::service::messaging::call_with_retry(
                    [&](const ores::service::messaging::retry_attempt& attempt)
                        -> ores::marketdata::messaging::publish_fx_forward_response {
                        return ores::service::messaging::do_unless_already_done(
                            attempt,
                            [&]() -> std::optional<
                                      ores::marketdata::messaging::publish_fx_forward_response> {
                                ores::marketdata::messaging::get_published_fx_forward_request read;
                                read.pair_code = pair;
                                read.as_of = as_of;
                                const auto found = ask(party_nats, read, attempt.budget_left);
                                if (!found.success)
                                    return std::nullopt;
                                return ores::marketdata::messaging::publish_fx_forward_response{
                                    .success = true,
                                    .message = "Published by an earlier attempt.",
                                    .approval_request_id = found.request_id,
                                    .state_code = "published",
                                    .published_at = found.published_at,
                                    .published_by = found.published_by};
                            },
                            [&] { return ask(party_nats, publish, attempt.budget_left); });
                    },
                    {.deadline = step_deadline,
                     .what = line + " forward set",
                     .note = attempts_recorded_in(step_log, wf)});
                if (answer.success && answer.published_at) {
                    ++published;
                    result.forwards.push_back(
                        line + ": as of " + ores::platform::time::datetime::to_iso8601_utc(as_of));
                } else if (answer.approval_request_id.empty()) {
                    result.forwards_skipped.push_back(line + ": " + answer.message);
                } else if (answer.success) {
                    result.forwards_not_published.push_back(
                        line + ": the request " + answer.approval_request_id + " is " +
                        answer.state_code + " and no person has answered it");
                } else {
                    result.forwards_not_published.push_back(line + ": " + answer.message);
                }
            }
            BOOST_LOG_SEV(lg(), ores::logging::info)
                << "publish_party_forwards: " << party_name << ": published " << published << " of "
                << pairs.size() << " pair(s)";
            wf.record(party_name + ": published forward sets for " + std::to_string(published) +
                      " of " + std::to_string(pairs.size()) + " pair(s)");
            // A refusal reads as a skip, so a service that refuses every pair
            // for a reason of its own, such as a permission, would otherwise
            // pass the step with nothing published.
            if (published == 0 && held == 0)
                result.forwards_not_published.push_back(
                    party_name + ": no pair could be built, and the party holds no forward set");
            result.parties.push_back(party_text);
        }

        for (const auto& skipped : result.forwards_skipped)
            step_log.push_back({.level = ores::workflow::messaging::step_log_level::info,
                                .message = "skipped " + skipped,
                                .context = {}});
        for (const auto& not_published : result.forwards_not_published)
            step_log.push_back({.level = ores::workflow::messaging::step_log_level::warn,
                                .message = not_published,
                                .context = {}});
        if (!result.forwards_not_published.empty()) {
            std::string reason = std::to_string(result.forwards_not_published.size()) +
                                 " forward set(s) were not published. A rerun is safe: it "
                                 "publishes only the missing pairs:";
            for (const auto& line : result.forwards_not_published)
                reason += " " + line + ";";
            wf.fail(reason, step_log);
            return;
        }
        wf.complete(ores::utility::codec::json::write(result), step_log);
    }


    /**
     * @brief The surface blueprints the installation keeps, with every series
     * they name.
     *
     * Read as the service in the system tenant, where the catalogue lives, on
     * the same terms as the curve blueprints: no session inside that tenant, and
     * no tenant copy among the rows.
     */
    ores::marketdata::messaging::surface_blueprints read_surface_catalogue() {
        const auto system = ores::database::service::tenant_context::with_system_tenant(ctx_);
        ores::marketdata::messaging::surface_blueprints catalogue;
        catalogue.blueprints =
            ores::refdata::repository::fx_vol_surface_blueprint_repository{}.read_latest(system);
        if (catalogue.blueprints.empty())
            return catalogue;

        std::set<boost::uuids::uuid> blueprint_ids;
        std::vector<std::string> series_ids;
        for (const auto& b : catalogue.blueprints) {
            blueprint_ids.insert(b.id);
            series_ids.push_back(boost::uuids::to_string(b.output_series_id));
        }
        for (auto& p :
             ores::refdata::repository::fx_vol_surface_pillar_repository{}.read_latest(system)) {
            if (!blueprint_ids.contains(p.blueprint_id))
                continue;
            catalogue.pillars.push_back(std::move(p));
        }
        std::set<boost::uuids::uuid> pillar_ids;
        for (const auto& p : catalogue.pillars)
            pillar_ids.insert(p.id);
        for (auto& s :
             ores::refdata::repository::fx_vol_surface_pillar_source_repository{}.read_latest(
                 system)) {
            if (!pillar_ids.contains(s.pillar_id))
                continue;
            series_ids.push_back(boost::uuids::to_string(s.market_series_id));
            catalogue.sources.push_back(std::move(s));
        }
        std::ranges::sort(series_ids);
        series_ids.erase(std::unique(series_ids.begin(), series_ids.end()), series_ids.end());
        catalogue.series = ores::marketdata::repository::market_series_repository{}.read_latest(
            system, series_ids);
        return catalogue;
    }

    /// The series and surface blueprints one party holds, one page at a time.
    static ores::marketdata::messaging::party_surface_holdings
    read_party_surface_holdings(ores::nats::service::nats_client& client,
                                const boost::uuids::uuid& party_id) {
        constexpr std::uint32_t page_size = 1000;
        ores::marketdata::messaging::party_surface_holdings holdings;
        holdings.party_id = party_id;
        for (std::uint32_t offset = 0;; offset += page_size) {
            ores::marketdata::messaging::list_market_series_request series;
            series.offset = offset;
            series.limit = page_size;
            const auto page = ask(client, series).market_series;
            holdings.series.insert(holdings.series.end(), page.begin(), page.end());
            if (page.size() < page_size)
                break;
        }
        for (std::uint32_t offset = 0;; offset += page_size) {
            ores::refdata::messaging::list_fx_vol_surface_blueprints_request blueprints;
            blueprints.offset = offset;
            blueprints.limit = page_size;
            const auto page = ask(client, blueprints).blueprints;
            for (const auto& b : page)
                if (b.party_id == party_id)
                    holdings.blueprint_ids.insert(b.id);
            if (page.size() < page_size)
                break;
        }
        return holdings;
    }

    /// Writes the rows a surface plan names: the output series, then the
    /// blueprints, their pillars and the pillars' sources, each of which names
    /// the row before it.
    static void
    write_party_surface_plan(ores::nats::service::nats_client& client,
                             const ores::marketdata::messaging::party_surface_plan& plan,
                             const std::string& party_name) {
        constexpr std::string_view commentary =
            "Created by provisioning: the party's own copy of the system's surface blueprint";
        constexpr std::string_view reason = "system.new_record";
        const ores::utility::domain::precondition any{
            .kind = ores::utility::domain::precondition_kind::any};

        std::vector<ores::marketdata::messaging::market_series_change> series;
        for (const auto& s : plan.series_to_write)
            series.push_back({.write = {.id = s.id,
                                        .party_id = s.party_id,
                                        .oresmd_uri = s.oresmd_uri,
                                        .provider_code = s.provider_code,
                                        .series_subclass = s.series_subclass,
                                        .derivation_kind = s.derivation_kind,
                                        .derivation_config_id = s.derivation_config_id,
                                        .derivation_config_version = s.derivation_config_version},
                              .precondition = any});
        write_in_batches<ores::marketdata::messaging::put_many_market_series_request>(
            client, series, reason, commentary, "output series", party_name);

        std::vector<ores::refdata::messaging::fx_vol_surface_blueprint_change> blueprints;
        for (const auto& b : plan.blueprints_to_write)
            blueprints.push_back({.write = {.id = b.id,
                                            .output_series_id = b.output_series_id,
                                            .name = b.name,
                                            .base_currency_code = b.base_currency_code,
                                            .quote_currency_code = b.quote_currency_code,
                                            .fx_option_convention_id = b.fx_option_convention_id,
                                            .smile_model = b.smile_model,
                                            .expiry_interpolation = b.expiry_interpolation,
                                            .output_expiries = b.output_expiries,
                                            .output_strikes = b.output_strikes},
                                  .precondition = any});
        write_in_batches<ores::refdata::messaging::put_many_fx_vol_surface_blueprints_request>(
            client, blueprints, reason, commentary, "blueprints", party_name);

        std::vector<ores::refdata::messaging::fx_vol_surface_pillar_change> pillars;
        for (const auto& p : plan.pillars_to_write)
            pillars.push_back({.write = {.id = p.id,
                                         .party_id = p.party_id,
                                         .blueprint_id = p.blueprint_id,
                                         .expiry_code = p.expiry_code,
                                         .strike_label = p.strike_label},
                               .precondition = any});
        write_in_batches<ores::refdata::messaging::put_many_fx_vol_surface_pillars_request>(
            client, pillars, reason, commentary, "pillars", party_name);

        std::vector<ores::refdata::messaging::fx_vol_surface_pillar_source_change> sources;
        for (const auto& s : plan.sources_to_write)
            sources.push_back({.write = {.id = s.id,
                                         .party_id = s.party_id,
                                         .pillar_id = s.pillar_id,
                                         .market_series_id = s.market_series_id,
                                         .contributor_code = s.contributor_code,
                                         .weight = s.weight},
                               .precondition = any});
        write_in_batches<ores::refdata::messaging::put_many_fx_vol_surface_pillar_sources_request>(
            client, sources, reason, commentary, "pillar sources", party_name);
    }

    /**
     * @brief Gives each party the step names its own FX volatility surface
     * blueprints.
     *
     * A surface is derived data, so it belongs to the party that calibrates it.
     * The party copies every blueprint the catalogue holds, and each pillar
     * source reads the party's own series of the same item from the same
     * provider, which its subscription to the derived source fills. The step
     * waits up to three minutes for those series, writes the rows, and fails
     * naming each blueprint still waiting. A second run writes no row twice.
     */
    void give_party_surface_blueprints(
        ores::service::messaging::workflow_step_context& wf,
        const ores::iam::workflow::provision_tenant_step_command& command) {
        constexpr auto quote_patience = std::chrono::minutes{3};
        constexpr auto retry_interval = std::chrono::seconds{10};

        const auto parties =
            ores::iam::workflow::parse_party_curves_arguments(command.arguments_json);

        ores::nats::service::nats_client tenant_nats(
            nats_,
            ores::iam::client::make_tenant_session_provider(
                nats_, service_token_, command.tenant_id));
        const auto catalogue = read_surface_catalogue();
        if (catalogue.blueprints.empty()) {
            wf.fail("The system tenant holds no surface blueprint to copy.");
            return;
        }

        provision_step_result result;
        result.kind = command.kind;
        std::vector<ores::workflow::messaging::step_log_entry> step_log;
        std::vector<std::string> waiting;
        for (const auto& party_name : parties) {
            const auto party = find_party(tenant_nats, party_name);
            if (!party)
                throw std::runtime_error("The tenant holds no party named '" + party_name + "'.");

            const auto party_text = boost::uuids::to_string(party->id);
            ores::nats::service::nats_client party_nats(
                nats_,
                ores::iam::client::make_tenant_session_provider(nats_,
                                                                service_token_,
                                                                command.tenant_id,
                                                                std::chrono::seconds{60},
                                                                party_text));

            const auto started = std::chrono::steady_clock::now();
            auto plan = ores::marketdata::messaging::plan_party_surfaces(
                catalogue, read_party_surface_holdings(party_nats, party->id));
            while (!plan.waiting.empty() &&
                   std::chrono::steady_clock::now() < started + quote_patience) {
                const auto line = party_name + ": " + std::to_string(plan.waiting.size()) +
                                  " surface blueprint(s) wait for their quotes, asking again in " +
                                  std::to_string(retry_interval.count()) + "s";
                BOOST_LOG_SEV(lg(), ores::logging::info)
                    << "give_party_surface_blueprints: " << line;
                wf.progress(party_name + ": surface blueprints with their quotes",
                            static_cast<std::int64_t>(plan.ready.size()),
                            static_cast<std::int64_t>(plan.ready.size() + plan.waiting.size()));
                step_log.push_back({.level = ores::workflow::messaging::step_log_level::info,
                                    .message = line,
                                    .context = {}});
                std::this_thread::sleep_for(retry_interval);
                plan = ores::marketdata::messaging::plan_party_surfaces(
                    catalogue, read_party_surface_holdings(party_nats, party->id));
            }
            write_party_surface_plan(party_nats, plan, party_name);
            BOOST_LOG_SEV(lg(), ores::logging::info)
                << "give_party_surface_blueprints: " << party_name << ": "
                << plan.blueprints_to_write.size() << " written, " << plan.ready.size() << " held, "
                << plan.waiting.size() << " waiting";
            for (const auto& w : plan.waiting)
                waiting.push_back(party_name + ": " + w);
            result.parties.push_back(party_text);
        }
        if (!waiting.empty()) {
            std::string reason = std::to_string(waiting.size()) +
                                 " surface blueprint(s) were not given. A rerun is safe:";
            for (const auto& line : waiting)
                reason += " " + line + ";";
            wf.fail(reason, step_log);
            return;
        }
        wf.complete(ores::utility::codec::json::write(result), step_log);
    }

    /**
     * @brief The context one party's own work runs in.
     *
     * The step acts as the service and never as a person, so its context is the
     * tenant's, narrowed to one party and to the parties that party sees: the
     * same scope the session a request carried gave it, without a token. The
     * policy refuses a read for anything outside it, so the narrowing is the
     * authority and not a convention.
     */
    ores::database::context context_for_party(const ores::database::context& tenant_ctx,
                                              const boost::uuids::uuid& party_id) const {
        const auto visible = ores::database::query_builder::execute_parameterized_string_query(
            tenant_ctx,
            "SELECT unnest(ores_refdata_visible_party_ids_fn($1::uuid, $2::uuid))::text",
            {tenant_ctx.tenant_id().to_string(), boost::uuids::to_string(party_id)},
            lg(),
            "Reading the parties the party the step serves sees");
        std::vector<boost::uuids::uuid> visible_ids;
        visible_ids.reserve(visible.size());
        boost::uuids::string_generator parse;
        for (const auto& text : visible)
            visible_ids.push_back(parse(text));
        return tenant_ctx.with_party(
            tenant_ctx.tenant_id(), party_id, std::move(visible_ids), tenant_ctx.service_account());
    }

    /// The party the step names, by its exact full name, one page at a time,
    /// read in the tenant rather than asked of refdata.
    static std::optional<ores::refdata::domain::party>
    read_party_named(const ores::database::context& ctx, const std::string& full_name) {
        constexpr std::uint32_t page_size = 1000;
        for (std::uint32_t offset = 0;; offset += page_size) {
            const auto page =
                ores::refdata::repository::party_repository{}.read_latest(ctx, offset, page_size);
            for (const auto& party : page)
                if (party.full_name == full_name)
                    return party;
            if (page.size() < page_size)
                return std::nullopt;
        }
    }

    /// The surface blueprints one party holds, one page at a time.
    static std::vector<ores::refdata::domain::fx_vol_surface_blueprint>
    read_party_blueprints(const ores::database::context& ctx, const boost::uuids::uuid& party_id) {
        constexpr std::uint32_t page_size = 1000;
        std::vector<ores::refdata::domain::fx_vol_surface_blueprint> blueprints;
        for (std::uint32_t offset = 0;; offset += page_size) {
            const auto page =
                ores::refdata::repository::fx_vol_surface_blueprint_repository{}.read_latest(
                    ctx, offset, page_size);
            for (const auto& blueprint : page)
                if (blueprint.party_id == party_id)
                    blueprints.push_back(blueprint);
            if (page.size() < page_size)
                break;
        }
        return blueprints;
    }

    /// What one surface of a party came to, in the step's own terms.
    struct surface_outcome {
        enum class state { published, already_published, held_back, not_published } how;
        /// The blueprint's name, as the party reads it.
        std::string name;
        /// The detail a person reads for the state, when the state has one.
        std::string detail;
    };

    /**
     * @brief Calibrates and publishes one surface, where the step already is.
     *
     * The calibration and the publish are the service's own work, so they are
     * called rather than asked for: a request to itself would queue behind the
     * tick ingest and the sweep, which is what the step spent most of its time
     * doing.
     */
    static surface_outcome
    fit_and_publish_surface(const ores::database::context& ctx,
                            const ores::refdata::domain::fx_vol_surface_blueprint& blueprint,
                            std::string_view publish_reason) {
        const auto id = boost::uuids::to_string(blueprint.id);
        const auto as_of = std::chrono::system_clock::now();

        // The read answers the newest published surface at or before the as-of,
        // so a surface an earlier run published is found by a later one, even
        // when a newer publish has since superseded it: the blueprint has a
        // surface either way. A candidate is not one of those.
        ores::marketdata::messaging::get_published_fx_vol_surface_request published_before;
        published_before.blueprint_id = id;
        published_before.as_of = as_of;
        if (ores::marketdata::service::app::fx_vol_surface_publish_service::read_published(
                ctx, published_before)
                .success)
            return {.how = surface_outcome::state::already_published,
                    .name = blueprint.name,
                    .detail = {}};

        ores::marketdata::messaging::calibrate_fx_vol_surface_request calibrate;
        calibrate.blueprint_id = id;
        calibrate.as_of = as_of;
        const auto calibrated =
            ores::marketdata::service::app::fx_vol_surface_calibration_service::calibrate(
                ctx, calibrate);
        if (!calibrated.success)
            return {.how = surface_outcome::state::not_published,
                    .name = blueprint.name,
                    .detail = calibrated.message};
        if (const auto reason = ores::marketdata::messaging::party_surface_hold_back(calibrated))
            return {.how = surface_outcome::state::held_back,
                    .name = blueprint.name,
                    .detail = *reason};

        ores::marketdata::messaging::publish_fx_vol_surface_request publish;
        publish.blueprint_id = id;
        publish.as_of = calibrated.as_of;
        publish.reason = std::string(publish_reason);
        const auto answer =
            ores::marketdata::service::app::fx_vol_surface_publish_service::publish(ctx, publish);
        if (answer.success && answer.published_at)
            return {.how = surface_outcome::state::published,
                    .name = blueprint.name,
                    .detail = "as of " +
                              ores::platform::time::datetime::to_iso8601_utc(calibrated.as_of) +
                              ", from forward set " + calibrated.forward_publish_id};
        if (answer.success)
            return {.how = surface_outcome::state::not_published,
                    .name = blueprint.name,
                    .detail = "the request " + answer.approval_request_id + " is " +
                              answer.state_code + " and no person has answered it"};
        return {.how = surface_outcome::state::not_published,
                .name = blueprint.name,
                .detail = answer.message};
    }

    /// The most attempts one surface gets when the store makes it wait for a
    /// connection. A pool that is full for a moment is not a surface that
    /// cannot be published, and lanes share the pool with the ingest.
    static constexpr int surface_attempts = 3;

    /**
     * @brief Fits and publishes one surface, asking again when the store was
     * only busy.
     *
     * A lane holds a unit of work, and the service's other work holds its own,
     * so a statement can wait for a connection and give up. That is the store
     * saying "not now", not the surface saying "never", so the surface is
     * offered again rather than taking the party's step down with it.
     */
    static surface_outcome fit_and_publish_surface_with_retry(
        const ores::database::context& ctx,
        const ores::refdata::domain::fx_vol_surface_blueprint& blueprint,
        std::string_view publish_reason) {
        for (int attempt = 1;; ++attempt) {
            try {
                return fit_and_publish_surface(ctx, blueprint, publish_reason);
            } catch (const ores::database::pq::store_error& e) {
                if (!e.retryable() || attempt >= surface_attempts)
                    throw;
                std::this_thread::sleep_for(std::chrono::milliseconds{200 * attempt});
            }
        }
    }


    /**
     * @brief Fits and publishes a party's surfaces, several at a time.
     *
     * A surface is the party's own work and no surface reads another, so the
     * party's surfaces run in parallel lanes: the party costs the slowest lane
     * and not the sum of them. Each lane reports the progress the engine reads
     * as the step's heartbeat. A lane that throws is one surface that did not
     * publish, named with its reason, so one bad blueprint cannot take the
     * party's other surfaces with it.
     */
    std::vector<surface_outcome> run_party_surfaces(
        ores::service::messaging::workflow_step_context& wf,
        const ores::database::context& party_ctx,
        const std::vector<ores::refdata::domain::fx_vol_surface_blueprint>& blueprints,
        std::string_view publish_reason,
        const std::string& party_name) const {
        constexpr std::size_t lanes = 4;
        const auto lane_count = std::min(lanes, blueprints.size());
        const auto total = static_cast<std::int64_t>(blueprints.size());
        std::vector<surface_outcome> outcomes(blueprints.size());
        std::atomic<std::size_t> next{0};
        std::atomic<std::int64_t> done{0};

        const auto lane = [&] {
            for (;;) {
                const auto i = next.fetch_add(1);
                if (i >= blueprints.size())
                    return;
                try {
                    outcomes[i] = fit_and_publish_surface_with_retry(
                        party_ctx, blueprints[i], publish_reason);
                } catch (const std::exception& e) {
                    outcomes[i] = {.how = surface_outcome::state::not_published,
                                   .name = blueprints[i].name,
                                   .detail = e.what()};
                }
                const auto finished = done.fetch_add(1) + 1;
                wf.progress(party_name + ": " + blueprints[i].name, finished, total);
            }
        };

        std::vector<std::thread> workers;
        workers.reserve(lane_count);
        for (std::size_t l = 0; l < lane_count; ++l)
            workers.emplace_back(lane);
        for (auto& worker : workers)
            worker.join();
        return outcomes;
    }

    /**
     * @brief Calibrates and publishes each party's volatility surfaces.
     *
     * A surface is derived data, so it belongs to the party whose blueprint,
     * quotes and forward sets it reads. Each blueprint the party holds is
     * calibrated at the present instant and published under the tenant's
     * auto-approval.
     *
     * The step does the work itself rather than asking its own service for it:
     * the context is the tenant's, narrowed to the party, and the party's
     * surfaces run in lanes beside each other.
     *
     * A surface with arbitrage, a fit that did not converge or a pillar beyond
     * the tolerance is held back and named: a decider would weigh it, and
     * provisioning has none. A surface that cannot be calibrated, or is raised
     * and not published, fails the step, named with its party. A second run
     * publishes only the surfaces that are still missing.
     */
    void publish_party_surfaces(ores::service::messaging::workflow_step_context& wf,
                                const ores::iam::workflow::provision_tenant_step_command& command) {
        constexpr std::string_view publish_reason =
            "Provisioning calibrates the party's surface from its volatility quotes and its "
            "published forward sets.";

        const auto parties =
            ores::iam::workflow::parse_party_surfaces_arguments(command.arguments_json);

        // The step acts in the tenant as its own service account, and then in
        // each party it names.
        const auto tenant_ctx =
            ores::database::service::tenant_context::with_tenant(ctx_, command.tenant_id);
        const auto service_ctx =
            tenant_ctx.with_tenant(tenant_ctx.tenant_id(), ctx_.service_account());

        provision_step_result result;
        result.kind = command.kind;
        std::vector<ores::workflow::messaging::step_log_entry> step_log;
        for (const auto& party_name : parties) {
            const auto party = read_party_named(service_ctx, party_name);
            if (!party)
                throw std::runtime_error("The tenant holds no party named '" + party_name + "'.");

            const auto party_text = boost::uuids::to_string(party->id);
            result.parties.push_back(party_text);

            const auto party_ctx = context_for_party(service_ctx, party->id);
            const auto blueprints = read_party_blueprints(party_ctx, party->id);
            if (blueprints.empty()) {
                result.surfaces_not_published.push_back(party_name +
                                                        ": the party holds no surface blueprint");
                continue;
            }

            const auto outcomes =
                run_party_surfaces(wf, party_ctx, blueprints, publish_reason, party_name);
            std::size_t published = 0;
            for (const auto& outcome : outcomes) {
                const auto line = party_name + ": " + outcome.name;
                switch (outcome.how) {
                    case surface_outcome::state::published:
                        ++published;
                        result.surfaces.push_back(line + ": " + outcome.detail);
                        break;
                    case surface_outcome::state::already_published:
                        result.surfaces_already_published.push_back(line);
                        break;
                    case surface_outcome::state::held_back:
                        result.surfaces_held_back.push_back(line + ": " + outcome.detail);
                        break;
                    case surface_outcome::state::not_published:
                        result.surfaces_not_published.push_back(line + ": " + outcome.detail);
                        break;
                }
            }
            BOOST_LOG_SEV(lg(), ores::logging::info)
                << "publish_party_surfaces: " << party_name << ": published " << published << " of "
                << blueprints.size() << " surface(s)";
            wf.record(party_name + ": published " + std::to_string(published) + " of " +
                      std::to_string(blueprints.size()) + " surface(s)");
        }

        for (const auto& held : result.surfaces_held_back)
            step_log.push_back({.level = ores::workflow::messaging::step_log_level::warn,
                                .message = "held back " + held,
                                .context = {}});
        for (const auto& not_published : result.surfaces_not_published)
            step_log.push_back({.level = ores::workflow::messaging::step_log_level::warn,
                                .message = not_published,
                                .context = {}});
        if (!result.surfaces_not_published.empty()) {
            std::string reason = std::to_string(result.surfaces_not_published.size()) +
                                 " surface(s) were not published. A rerun is safe: it publishes "
                                 "only the missing surfaces:";
            for (const auto& line : result.surfaces_not_published)
                reason += " " + line + ";";
            wf.fail(reason, step_log);
            return;
        }
        wf.complete(ores::utility::codec::json::write(result), step_log);
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::nats::service::nats_client::token_provider service_token_;
};

}

#endif
