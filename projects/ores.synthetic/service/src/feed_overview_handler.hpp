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
#ifndef ORES_SYNTHETIC_SERVICE_FEED_OVERVIEW_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_FEED_OVERVIEW_HANDLER_HPP

#include "feed_controller.hpp"
#include "feed_kind_registry.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/feeds/feed_filter.hpp"
#include "ores.synthetic.api/feeds/feed_overview.hpp"
#include "ores.synthetic.api/messaging/feed_overview_protocol.hpp"
#include <algorithm>
#include <exception>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace ores::synthetic::service {

namespace {
inline auto& feed_overview_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.feed_overview_handler");
    return instance;
}

inline std::string text_of(const std::optional<std::string>& value) {
    return value.value_or("");
}

inline int number_of(const std::optional<std::string>& value) {
    return value ? std::stoi(*value) : 0;
}
}

/**
 * @brief Answers a screen's question "what is under this folder, and what runs".
 *
 * Counts in the database and takes the running feeds from the controller, so no
 * caller reads the feeds to count them. A caller needs to read the feeds of at
 * least one kind. Nothing is stored.
 */
class feed_overview_handler {
public:
    feed_overview_handler(ores::nats::service::client& nats,
                          std::shared_ptr<feed_controller> ctrl,
                          ores::database::context ctx,
                          std::optional<ores::security::jwt::jwt_authenticator> verifier,
                          const feed_kind_registry& registry)
        : nats_(nats)
        , ctrl_(std::move(ctrl))
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , registry_(registry) {}

    void list(ores::nats::message msg) {
        using namespace ores::logging;
        using namespace ores::synthetic::messaging;
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::has_permission;
        using ores::service::messaging::reply;

        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            error_reply(nats_, msg, ctx.error());
            return;
        }
        const auto& kinds = registry_.all();
        const bool may = std::ranges::any_of(
            kinds, [&](const auto* k) { return has_permission(*ctx, k->config_permission); });
        if (!may) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        list_feed_folders_response resp;
        const auto req = decode<list_feed_folders_request>(msg);
        if (!req) {
            resp.message = "Failed to decode the folder request.";
            BOOST_LOG_SEV(feed_overview_handler_lg(), warn)
                << "list_feed_folders refused: the request did not decode";
            reply(nats_, msg, resp);
            return;
        }

        try {
            resp.folders = ores::synthetic::feed::children_with_counts(
                folders(*ctx), direct_counts(*ctx), req->parent_id);
            resp.success = true;
        } catch (const std::exception& e) {
            resp.message = "The request failed. The service log says why.";
            BOOST_LOG_SEV(feed_overview_handler_lg(), error)
                << "list_feed_folders failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

    void list_feeds(ores::nats::message msg) {
        using namespace ores::logging;
        using namespace ores::synthetic::messaging;
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::has_permission;
        using ores::service::messaging::reply;

        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            error_reply(nats_, msg, ctx.error());
            return;
        }
        const auto& kinds = registry_.all();
        const bool may = std::ranges::any_of(
            kinds, [&](const auto* k) { return has_permission(*ctx, k->config_permission); });
        if (!may) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        list_folder_feeds_response resp;
        const auto req = decode<list_folder_feeds_request>(msg);
        if (!req) {
            resp.message = "Failed to decode the feed request.";
            BOOST_LOG_SEV(feed_overview_handler_lg(), warn)
                << "list_folder_feeds refused: the request did not decode";
            reply(nats_, msg, resp);
            return;
        }
        const ores::synthetic::feed::feed_filter filter{.folder_id = req->folder_id,
                                                        .provider_code = req->provider_code,
                                                        .asset_class = req->asset_class,
                                                        .search = req->search,
                                                        .state = req->state,
                                                        .should_run = req->should_run};
        if (const auto problem = ores::synthetic::feed::filter_problem(filter); !problem.empty()) {
            resp.message = problem;
            reply(nats_, msg, resp);
            return;
        }

        try {
            feeds_page(*ctx, filter, req->offset, req->limit, resp);
            resp.success = true;
        } catch (const std::exception& e) {
            resp.message = "The request failed. The service log says why.";
            BOOST_LOG_SEV(feed_overview_handler_lg(), error)
                << "list_folder_feeds failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

    void resolve_series_owners(ores::nats::message msg) {
        using namespace ores::logging;
        using namespace ores::synthetic::messaging;
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::has_permission;
        using ores::service::messaging::reply;

        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            error_reply(nats_, msg, ctx.error());
            return;
        }
        const auto& kinds = registry_.all();
        const bool may = std::ranges::any_of(
            kinds, [&](const auto* k) { return has_permission(*ctx, k->config_permission); });
        if (!may) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        resolve_series_owners_response resp;
        const auto req = decode<resolve_series_owners_request>(msg);
        if (!req) {
            resp.message = "Failed to decode the series request.";
            BOOST_LOG_SEV(feed_overview_handler_lg(), warn)
                << "resolve_series_owners refused: the request did not decode";
            reply(nats_, msg, resp);
            return;
        }

        try {
            resp.owners.reserve(req->series.size());
            for (const auto& series : req->series) {
                series_owner owner;
                owner.series = series;
                if (const auto held = ctrl_->running_source_name_for_series({series}))
                    owner.source_name = *held;
                resp.owners.push_back(std::move(owner));
            }
            resp.success = true;
        } catch (const std::exception& e) {
            resp.message = "The request failed. The service log says why.";
            BOOST_LOG_SEV(feed_overview_handler_lg(), error)
                << "resolve_series_owners failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    using rows_t = std::vector<std::vector<std::optional<std::string>>>;

    /**
     * A page of the feeds the filter matches, in label order, and how many match in
     * all. Both kinds are read in one query, so a person pages through one list. Every
     * part of the filter is checked or escaped by the filter itself, so nothing the
     * caller sends is run as SQL.
     */
    void feeds_page(const ores::database::context& ctx,
                    const ores::synthetic::feed::feed_filter& filter,
                    int offset,
                    int limit,
                    ores::synthetic::messaging::list_folder_feeds_response& resp) const {
        const auto running = ctrl_->list();
        const std::unordered_set<std::string> running_names(running.begin(), running.end());
        const auto feeds = ores::synthetic::feed::feeds_query();
        const auto where = ores::synthetic::feed::where_clause(
            filter, running, feed_kind_registry::is_system_caller(ctx));

        const auto counted = query(ctx, "select count(*) from (" + feeds + ") f where " + where);
        resp.total = counted.empty() ? 0 : number_of(counted.front().front());

        const auto rows = query(ctx,
                                "select id, kind, label, source_name, oresmd_uri, "
                                "coalesce(folder_id::text, ''), enabled::text, auto_start::text, "
                                "asset_class_code, ticks_per_hour::text, engine, underlying, "
                                "provider_code from (" +
                                    feeds + ") f where " + where + " order by label, id limit " +
                                    std::to_string(std::clamp(limit, 1, 200)) + " offset " +
                                    std::to_string(std::max(offset, 0)));
        resp.feeds.reserve(rows.size());
        for (const auto& r : rows) {
            ores::synthetic::messaging::folder_feed f;
            f.id = text_of(r[0]);
            f.kind = text_of(r[1]);
            f.label = text_of(r[2]);
            f.source_name = text_of(r[3]);
            f.oresmd_uri = text_of(r[4]);
            f.folder_id = text_of(r[5]);
            f.enabled = text_of(r[6]) == "true";
            f.auto_start = text_of(r[7]) == "true";
            f.asset_class_code = text_of(r[8]);
            f.ticks_per_hour = number_of(r[9]);
            f.engine = text_of(r[10]);
            f.underlying = text_of(r[11]);
            f.provider_code = text_of(r[12]);
            f.running = running_names.contains(f.source_name);
            resp.feeds.push_back(std::move(f));
        }
    }

    rows_t query(const ores::database::context& ctx, const std::string& sql) const {
        return ores::database::repository::execute_raw_multi_column_query(
            ctx, sql, feed_overview_handler_lg(), "Reading the feed folders");
    }

    /// Every folder the caller sees, with the collection's own words on a collection folder.
    std::vector<ores::synthetic::messaging::feed_folder>
    folders(const ores::database::context& ctx) const {
        const auto rows = query(ctx, R"sql(
            select f.id::text, coalesce(f.parent_id::text, ''), f.name, f.kind,
                   coalesce(c.provider_code, '')
            from ores_synthetic_folders_tbl f
            left join ores_synthetic_market_data_generation_configs_tbl c
              on c.id = f.collection_id
             and c.valid_to = ores_utility_infinity_timestamp_fn()
            where f.valid_to = ores_utility_infinity_timestamp_fn())sql");
        std::vector<ores::synthetic::messaging::feed_folder> out;
        out.reserve(rows.size());
        for (const auto& r : rows) {
            ores::synthetic::messaging::feed_folder f;
            f.id = text_of(r[0]);
            f.parent_id = text_of(r[1]);
            f.name = text_of(r[2]);
            f.kind = text_of(r[3]);
            f.provider_code = text_of(r[4]);
            out.push_back(std::move(f));
        }
        return out;
    }

    /// Which of a folder's counts a row of the grouped queries adds to.
    enum class tally { fx, ir, running, auto_start, auto_running };

    /**
     * The feeds each folder holds itself, and how many of them run.
     *
     * The queries run in the request's context, so row-level security limits
     * them to the folders and feeds the caller sees. The controller names every
     * running feed by source name, and a source name is one running feed's, as
     * the controller holds it, so a running name is matched to the rows of the
     * caller that carry it.
     */
    std::vector<ores::synthetic::feed::folder_feed_count>
    direct_counts(const ores::database::context& ctx) const {
        std::unordered_map<std::string, ores::synthetic::feed::folder_feed_count> by_folder;
        const auto add = [&](const rows_t& rows, tally which) {
            for (const auto& r : rows) {
                const auto id = text_of(r[0]);
                const auto n = number_of(r[1]);
                auto& count = by_folder[id];
                count.folder_id = id;
                switch (which) {
                    case tally::fx:
                        count.fx_feeds += n;
                        break;
                    case tally::ir:
                        count.ir_feeds += n;
                        break;
                    case tally::running:
                        count.running_feeds += n;
                        break;
                    case tally::auto_start:
                        count.idle_auto_feeds += n;
                        break;
                    case tally::auto_running:
                        count.idle_auto_feeds -= n;
                        break;
                }
            }
        };
        add(query(ctx, R"sql(
            select folder_id::text, count(*) from ores_synthetic_fx_spot_generation_configs_tbl
            where valid_to = ores_utility_infinity_timestamp_fn() and folder_id is not null
            group by folder_id)sql"),
            tally::fx);
        add(query(ctx, R"sql(
            select folder_id::text, count(*) from ores_synthetic_ir_curve_generation_configs_tbl
            where valid_to = ores_utility_infinity_timestamp_fn() and folder_id is not null
            group by folder_id)sql"),
            tally::ir);

        // A system feed is the system tenant's to start, so another tenant does not count it
        // among the feeds that should run.
        const std::string runnable = feed_kind_registry::is_system_caller(ctx) ?
                                         std::string() :
                                         " and tenant_id <> ores_utility_system_tenant_id_fn()";
        for (const auto* table : {"ores_synthetic_fx_spot_generation_configs_tbl",
                                  "ores_synthetic_ir_curve_generation_configs_tbl"}) {
            add(query(ctx,
                      std::string("select folder_id::text, count(*) from ") + table +
                          " where valid_to = ores_utility_infinity_timestamp_fn()"
                          " and folder_id is not null and enabled and auto_start" +
                          runnable + " group by folder_id"),
                tally::auto_start);
        }

        const auto running = ctrl_->list();
        if (!running.empty()) {
            std::string names;
            for (const auto& name : running)
                names += (names.empty() ? "" : ", ") + ores::synthetic::feed::sql_literal(name);
            for (const auto* table : {"ores_synthetic_fx_spot_generation_configs_tbl",
                                      "ores_synthetic_ir_curve_generation_configs_tbl"}) {
                add(query(ctx,
                          std::string("select folder_id::text, count(*) from ") + table +
                              " where valid_to = ores_utility_infinity_timestamp_fn()"
                              " and folder_id is not null and source_name in (" +
                              names + ") group by folder_id"),
                    tally::running);
                add(query(ctx,
                          std::string("select folder_id::text, count(*) from ") + table +
                              " where valid_to = ores_utility_infinity_timestamp_fn()"
                              " and folder_id is not null and enabled and auto_start" +
                              runnable + " and source_name in (" + names + ") group by folder_id"),
                    tally::auto_running);
            }
        }

        std::vector<ores::synthetic::feed::folder_feed_count> out;
        out.reserve(by_folder.size());
        for (auto& [id, count] : by_folder)
            out.push_back(std::move(count));
        return out;
    }

    ores::nats::service::client& nats_;
    std::shared_ptr<feed_controller> ctrl_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    const feed_kind_registry& registry_;
};

}

#endif
