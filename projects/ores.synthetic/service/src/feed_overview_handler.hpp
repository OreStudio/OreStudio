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
#include "ores.synthetic.api/feeds/feed_overview.hpp"
#include "ores.synthetic.api/messaging/feed_overview_protocol.hpp"
#include <algorithm>
#include <exception>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::synthetic::service {

namespace {
inline auto& feed_overview_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.feed_overview_handler");
    return instance;
}

/// A text as an escape string literal: a quote and a backslash are doubled, so the
/// text means the same whatever the server's setting of standard_conforming_strings.
inline std::string sql_literal(const std::string& text) {
    std::string out = "E'";
    for (const char c : text) {
        if (c == '\'' || c == '\\')
            out.push_back(c);
        out.push_back(c);
    }
    out.push_back('\'');
    return out;
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
            resp.message = e.what();
            BOOST_LOG_SEV(feed_overview_handler_lg(), error)
                << "list_feed_folders failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    using rows_t = std::vector<std::vector<std::optional<std::string>>>;

    rows_t query(const ores::database::context& ctx, const std::string& sql) const {
        return ores::database::repository::execute_raw_multi_column_query(
            ctx, sql, feed_overview_handler_lg(), "Reading the feed folders");
    }

    /// Every folder the caller sees, with the collection's own words on a collection folder.
    std::vector<ores::synthetic::messaging::feed_folder>
    folders(const ores::database::context& ctx) const {
        const auto rows = query(ctx, R"sql(
            select f.id::text, coalesce(f.parent_id::text, ''), f.name, f.kind,
                   coalesce(c.scope, ''), coalesce(c.provider_code, ''),
                   coalesce(c.binding_mode, '')
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
            f.scope = text_of(r[4]);
            f.provider_code = text_of(r[5]);
            f.binding_mode = text_of(r[6]);
            out.push_back(std::move(f));
        }
        return out;
    }

    /// Which of a folder's counts a row of the grouped queries adds to.
    enum class tally { fx, ir, running };

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
                }
            }
        };
        add(query(ctx, R"sql(
            select folder_id::text, count(*) from ores_synthetic_fx_spot_generation_configs_tbl
            where valid_to = ores_utility_infinity_timestamp_fn() and folder_id is not null
            group by folder_id)sql"), tally::fx);
        add(query(ctx, R"sql(
            select folder_id::text, count(*) from ores_synthetic_ir_curve_generation_configs_tbl
            where valid_to = ores_utility_infinity_timestamp_fn() and folder_id is not null
            group by folder_id)sql"), tally::ir);

        const auto running = ctrl_->list();
        if (!running.empty()) {
            std::string names;
            for (const auto& name : running)
                names += (names.empty() ? "" : ", ") + sql_literal(name);
            for (const auto* table : {"ores_synthetic_fx_spot_generation_configs_tbl",
                                      "ores_synthetic_ir_curve_generation_configs_tbl"}) {
                add(query(ctx,
                          std::string("select folder_id::text, count(*) from ") + table +
                              " where valid_to = ores_utility_infinity_timestamp_fn()"
                              " and folder_id is not null and source_name in (" + names +
                              ") group by folder_id"),
                    tally::running);
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
