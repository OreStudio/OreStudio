/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro
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
#ifndef ORES_SYNTHETIC_SERVICE_SIMULATED_SOURCE_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_SIMULATED_SOURCE_HANDLER_HPP

#include "client_message.hpp"
#include "feed_kind_registry.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.core/service/stopped_feed_node_service.hpp"
#include "source_control.hpp"
#include <algorithm>
#include <cctype>
#include <exception>
#include <optional>
#include <string>
#include <string_view>

namespace ores::synthetic::service {

namespace {
inline auto& simulated_source_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.simulated_source_handler");
    return instance;
}
} // namespace

/**
 * @brief NATS handler for the simulated sources: their states, and stopping and starting one.
 *
 * A source serves every tenant, so only the system tenant stops or starts it, and only with the
 * run permission. A stop is recorded before the adapter stops, with the caller's reason, so a
 * restart of the service keeps it stopped and the record says who stopped it and why; a start
 * removes the record before the adapter starts.
 */
class simulated_source_handler {
public:
    simulated_source_handler(ores::nats::service::client& nats,
                             const ores::database::context& ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier,
                             source_control& sources)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(std::move(verifier))
        , sources_(sources) {}

    void list(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        [[maybe_unused]] const auto cid =
            ores::service::messaging::log_handler_entry(simulated_source_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            ores::service::messaging::error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        try {
            ores::service::messaging::reply(
                nats_,
                msg,
                list_simulated_sources_response{.success = true, .sources = sources_.sources()});
        } catch (const std::exception& e) {
            ORES_LOG_SEV(simulated_source_handler_lg(), ores::logging::error)
                << msg.subject << " failed: " << e.what();
            ores::service::messaging::reply(
                nats_,
                msg,
                list_simulated_sources_response{.success = false, .message = client_message(e)});
        }
    }

    // The graph is read as the source list is: any session may read it.
    void graph(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        [[maybe_unused]] const auto cid =
            ores::service::messaging::log_handler_entry(simulated_source_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            ores::service::messaging::error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto req = ores::service::messaging::decode<get_feed_graph_request>(msg);
        if (!req) {
            ores::service::messaging::reply(
                nats_, msg, get_feed_graph_response{.message = "Malformed graph request"});
            return;
        }
        try {
            auto graph = sources_.graph(req->node_name);
            ores::service::messaging::reply(
                nats_,
                msg,
                get_feed_graph_response{.success = true,
                                        .nodes = std::move(graph.nodes),
                                        .edges = std::move(graph.edges)});
        } catch (const std::exception& e) {
            ORES_LOG_SEV(simulated_source_handler_lg(), ores::logging::error)
                << msg.subject << " failed: " << e.what();
            ores::service::messaging::reply(
                nats_, msg, get_feed_graph_response{.message = client_message(e)});
        }
    }

    void stop(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        [[maybe_unused]] const auto cid =
            ores::service::messaging::log_handler_entry(simulated_source_handler_lg(), msg);
        const auto ctx = operator_context("stop", msg);
        if (!ctx)
            return;
        const auto req = ores::service::messaging::decode<stop_feed_node_request>(msg);
        const auto refuse = [&](std::string why) {
            ores::service::messaging::reply(
                nats_, msg, stop_feed_node_response{.message = std::move(why)});
        };
        if (!req) {
            refuse("Malformed stop request");
            return;
        }
        try {
            const auto node = find_node(req->node_name);
            if (!node) {
                refuse("The feed graph holds no node named " + req->node_name + ".");
                return;
            }
            if (node->stopped) {
                refuse("The " + node->kind + " " + req->node_name + " is already stopped.");
                return;
            }
            if (std::ranges::all_of(req->reason_code,
                                    [](unsigned char c) { return std::isspace(c) != 0; })) {
                refuse("A stop states its reason.");
                return;
            }
            ores::synthetic::domain::stopped_feed_node row;
            row.node_name = node->name;
            row.kind = node->kind;
            row.provider_code = node->provider_code;
            row.change_reason_code = req->reason_code;
            row.change_commentary = req->commentary;
            // The stop is recorded before the node stops, so a crash between the two leaves the
            // node stopped after a restart. A node that throws undoes the record, so no record
            // says a running node is stopped.
            ores::synthetic::service::stopped_feed_node_service records(*ctx);
            records.save_stopped_node(row);
            try {
                halt(*node);
            } catch (...) {
                records.delete_stopped_node(node->name);
                throw;
            }
            ores::service::messaging::reply(
                nats_,
                msg,
                stop_feed_node_response{.success = true, .message = "Stopped " + node->name});
        } catch (const std::exception& e) {
            ORES_LOG_SEV(simulated_source_handler_lg(), ores::logging::error)
                << msg.subject << " failed: " << e.what();
            refuse(client_message(e));
        }
    }

    void start(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        [[maybe_unused]] const auto cid =
            ores::service::messaging::log_handler_entry(simulated_source_handler_lg(), msg);
        const auto ctx = operator_context("start", msg);
        if (!ctx)
            return;
        const auto req = ores::service::messaging::decode<start_feed_node_request>(msg);
        const auto refuse = [&](std::string why) {
            ores::service::messaging::reply(
                nats_, msg, start_feed_node_response{.message = std::move(why)});
        };
        if (!req) {
            refuse("Malformed start request");
            return;
        }
        try {
            const auto node = find_node(req->node_name);
            if (!node) {
                refuse("The feed graph holds no node named " + req->node_name + ".");
                return;
            }
            if (!node->stopped) {
                refuse("The " + node->kind + " " + req->node_name + " is not stopped.");
                return;
            }
            // The node starts before the record goes, so a node that does not start stays
            // stopped and recorded as stopped. A record that cannot be removed stops the node
            // again, so the two agree whichever step fails.
            resume(*node);
            try {
                ores::synthetic::service::stopped_feed_node_service(*ctx).delete_stopped_node(
                    node->name);
            } catch (...) {
                halt(*node);
                throw;
            }
            ores::service::messaging::reply(
                nats_,
                msg,
                start_feed_node_response{.success = true, .message = "Started " + node->name});
        } catch (const std::exception& e) {
            ORES_LOG_SEV(simulated_source_handler_lg(), ores::logging::error)
                << msg.subject << " failed: " << e.what();
            refuse(client_message(e));
        }
    }

private:
    struct node final {
        std::string name;
        std::string kind;
        std::string provider_code;
        bool stopped = false;
    };

    // A name is a source's when the service serves a source of that name, and otherwise the
    // graph's node of that name. The two kinds of name never meet, and must not: a source is a
    // catalogue name such as bloomberg-a, and a feed or a view carries the synthetic mark and the
    // scheme's dotted segments, so one name always names one node.
    std::optional<node> find_node(const std::string& name) const {
        const auto all = sources_.sources();
        const auto found = std::ranges::find(
            all, name, &ores::synthetic::messaging::simulated_source::source_name);
        if (found != all.end())
            return node{.name = found->source_name,
                        .kind = std::string(node_kind::source),
                        .provider_code = found->provider_code,
                        .stopped = found->stopped};
        if (const auto other = sources_.node(name))
            return node{.name = other->name,
                        .kind = other->kind,
                        .provider_code = other->provider_code,
                        .stopped = other->stopped};
        return std::nullopt;
    }

    void halt(const node& n) {
        if (n.kind == node_kind::source)
            sources_.stop(n.name);
        else
            sources_.stop_node(n.name);
    }

    void resume(const node& n) {
        if (n.kind == node_kind::source)
            sources_.start(n.name);
        else
            sources_.start_node(n.name);
    }

    // A source serves every tenant, so the caller is the system tenant, with the run permission.
    std::optional<ores::database::context> operator_context(std::string_view verb,
                                                            const ores::nats::message& msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            ORES_LOG_SEV(simulated_source_handler_lg(), ores::logging::warn)
                << "Rejecting " << verb
                << " request: auth failed: " << static_cast<int>(ctx_expected.error());
            ores::service::messaging::error_reply(nats_, msg, ctx_expected.error());
            return std::nullopt;
        }
        if (!feed_kind_registry::permits_run(*ctx_expected) ||
            !feed_kind_registry::is_system_caller(*ctx_expected)) {
            ORES_LOG_SEV(simulated_source_handler_lg(), ores::logging::warn)
                << "Rejecting " << verb
                << " request: a node is stopped and started by the system tenant, with the "
                   "run permission.";
            ores::service::messaging::error_reply(nats_, msg, ores::service::error_code::forbidden);
            return std::nullopt;
        }
        return std::optional<ores::database::context>(std::move(*ctx_expected));
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    source_control& sources_;
};

}

#endif
