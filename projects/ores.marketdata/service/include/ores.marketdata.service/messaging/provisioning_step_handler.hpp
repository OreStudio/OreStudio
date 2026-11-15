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

#include "ores.iam.api/workflow/provision_step_arguments.hpp"
#include "ores.iam.api/workflow/provision_tenant_workflow.hpp"
#include "ores.iam.client/client/service_token_provider.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/feed_protocol.hpp"
#include "ores.marketdata.api/messaging/route_protocol.hpp"
#include "ores.marketdata.api/messaging/subscription_protocol.hpp"
#include "ores.nats/domain/delivery.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <cstdint>
#include <exception>
#include <map>
#include <ranges>
#include <set>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

namespace ores::marketdata::service::messaging {

/**
 * @brief The result a subscribe_market_data step reports, so the run's record
 * says what it acted on.
 */
struct provision_step_result {
    std::string kind;
    /// The parties the step gave a subscription and a route.
    std::vector<std::string> parties;
};

/**
 * @brief The members of provision_step_result, in the order it declares them.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<provision_step_result>) {
    using ::ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"kind", &provision_step_result::kind},
                                              field{"parties", &provision_step_result::parties}};
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
                              ores::nats::service::nats_client::token_provider service_token)
        : nats_(nats)
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
        using ores::iam::workflow::subscribe_market_data_step_kind;
        if (command.kind != subscribe_market_data_step_kind) {
            wf.fail("The step kind '" + command.kind +
                    "' is not one ores.marketdata executes; its work belongs to another service.");
            return;
        }

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
                    for (const auto& feed : feeds)
                        subscribe_party(party_nats,
                                        party.id,
                                        feed.provider,
                                        source_of(feed.provider, feed.source));
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

    ores::nats::service::client& nats_;
    ores::nats::service::nats_client::token_provider service_token_;
};

}

#endif
