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
#ifndef ORES_SYNTHETIC_SERVICE_FEED_RUN_IDENTITY_HPP
#define ORES_SYNTHETIC_SERVICE_FEED_RUN_IDENTITY_HPP

#include "feed_kind_registry.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.iam.client/client/run_grant_creator.hpp"
#include "ores.iam.client/client/run_token_minter.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/cache/run_token_cache.hpp"
#include "ores.synthetic.core/service/market_data_generation_config_service.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <expected>
#include <functional>
#include <map>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

namespace ores::synthetic::service {

/**
 * @brief The role a feed acts with, and the service role that exchanges its
 * grant.
 *
 * IAM's exchange reads the caller's service name from its role, so the audience
 * carries a role name and not a host name.
 */
inline constexpr std::string_view feed_run_role = "SyntheticFeedRun";
inline constexpr std::string_view feed_run_audience = "SyntheticService";

/**
 * @brief The three round trips the identity makes, as a seam.
 *
 * The identity owns the rules and the ports carry them out, so a test drives
 * the rules without IAM or a database.
 */
struct feed_run_identity_ports final {
    /** Exchanges a grant for a run token in a tenant. Empty when IAM refuses. */
    ores::service::service::cache::run_token_minter mint;
    /** Gives a person's consent to a resource's runs and answers the grant id. */
    std::function<std::expected<std::string, std::string>(const std::string& resource,
                                                          const std::string& person_bearer)>
        create_grant;
    /** Records a grant on its collection, in the tenant and party of @p ctx. */
    std::function<std::expected<void, std::string>(const ores::database::context& ctx,
                                                   const boost::uuids::uuid& collection_id,
                                                   const boost::uuids::uuid& grant_id)>
        record_grant;
};

/**
 * @brief The identity a feed runs as when it starts.
 *
 * A feed acts for the tenant that owns it, under the run grant of its
 * collection, whoever or whatever starts it. A person's Start gives the
 * consent when the collection holds none. A restore has no person, so it uses
 * the grant that a person gave. Only a feed that starts from a vintage price
 * reads marketdata at its start, so only such a feed needs a token.
 *
 * @see The identity a feed runs as (doc/knowledge/architecture/feed_execution_identity.org)
 */
class feed_run_identity final {
public:
    explicit feed_run_identity(feed_run_identity_ports ports)
        : ports_(std::move(ports))
        , tokens_(ports_.mint) {}

    /** The production ports: IAM over NATS and the collection's own service. */
    static feed_run_identity make(ores::nats::service::nats_client& svc_nats) {
        feed_run_identity_ports ports;
        ports.mint = ores::iam::client::make_run_token_minter(svc_nats);
        ports.create_grant = [&svc_nats](const std::string& resource,
                                         const std::string& person_bearer) {
            auto person_nats = svc_nats.with_delegation(person_bearer);
            return ores::iam::client::create_run_grant(
                person_nats, resource, feed_run_role, feed_run_audience);
        };
        ports.record_grant =
            [](const ores::database::context& ctx,
               const boost::uuids::uuid& collection_id,
               const boost::uuids::uuid& grant_id) -> std::expected<void, std::string> {
            const auto rows =
                ores::synthetic::repository::market_data_generation_config_repository().read_latest(
                    ctx, boost::uuids::to_string(collection_id));
            if (rows.empty())
                return std::unexpected("The collection " + boost::uuids::to_string(collection_id) +
                                       " is not visible, so its run grant is not recorded.");
            auto updated = rows.front();
            updated.run_grant_id = grant_id;
            updated.change_reason_code =
                std::string(ores::service::messaging::change_reasons::update);
            updated.change_commentary = "Recorded the consent that its feeds run under";
            try {
                market_data_generation_config_service(ctx).save_market_data_generation_config(
                    updated);
            } catch (const std::exception& e) {
                return std::unexpected(std::string("The run grant is not recorded: ") + e.what());
            }
            return {};
        };
        return feed_run_identity(std::move(ports));
    }

    /**
     * @brief The run token a start of @p target acts with.
     *
     * Empty, and no error, for a feed that needs none. A refusal carries the
     * reason the start reports or logs.
     *
     * @param ctx The tenant and party that the collection is read and written in.
     * @param person_bearer The token of the person who starts the feed, or empty
     * for a restore. It gives the consent and does nothing else.
     */
    [[nodiscard]] std::expected<std::string, std::string>
    run_token_for(const feed_start_target& target,
                  const ores::database::context& ctx,
                  const std::string& person_bearer) {
        const auto& feed = target.row.candidate;
        if (!feed.vintage_priced)
            return std::string{};

        const auto tenant = ctx.tenant_id().to_string();
        const auto resource =
            "synthetic.market_data_generation_config/" + boost::uuids::to_string(feed.container_id);

        auto grant = target.run_grant_id ? target.run_grant_id : given_for(feed.container_id);
        if (!grant) {
            const auto given = consent(resource, feed.container_id, ctx, person_bearer);
            if (!given)
                return std::unexpected(given.error());
            grant = *given;
        }

        auto key = key_of(*grant, feed);
        auto token = tokens_.token_for(key, tenant);
        if (token.empty() && target.run_grant_id && !person_bearer.empty()) {
            // The grant the collection holds is lapsed or revoked. A person's
            // Start is a new consent that replaces it.
            tokens_.invalidate(key);
            const auto given = consent(resource, feed.container_id, ctx, person_bearer);
            if (!given)
                return std::unexpected(given.error());
            key = key_of(*given, feed);
            token = tokens_.token_for(key, tenant);
        }
        if (token.empty())
            return std::unexpected(
                "IAM refused a run token for the run grant of " + feed.display_name +
                ". The grant is lapsed or revoked: a person who holds the role's permissions "
                "starts the collection to give a new consent.");
        return token;
    }

private:
    [[nodiscard]] static ores::service::service::cache::run_token_key
    key_of(const boost::uuids::uuid& grant, const feed_kind_candidate& feed) {
        return {.grant_id = boost::uuids::to_string(grant), .run_id = feed.feed_config_id};
    }

    [[nodiscard]] std::expected<boost::uuids::uuid, std::string>
    consent(const std::string& resource,
            const boost::uuids::uuid& collection_id,
            const ores::database::context& ctx,
            const std::string& person_bearer) {
        if (person_bearer.empty())
            return std::unexpected("The collection holds no run grant, and no person is present "
                                   "to give one. A person who holds the role's permissions "
                                   "starts the collection once to give the consent.");
        const auto created = ports_.create_grant(resource, person_bearer);
        if (!created)
            return std::unexpected(created.error());
        boost::uuids::uuid grant;
        try {
            grant = boost::uuids::string_generator()(*created);
        } catch (const std::exception&) {
            return std::unexpected("IAM answered the run grant with an unreadable id: " + *created);
        }
        const auto recorded = ports_.record_grant(ctx, collection_id, grant);
        if (!recorded)
            return std::unexpected(recorded.error());
        std::lock_guard lock(mutex_);
        given_[collection_id] = grant;
        return grant;
    }

    // A start of a whole folder reads its targets once, so the feeds after the
    // first still see no grant on their collection. The grant given a moment ago
    // serves them.
    [[nodiscard]] std::optional<boost::uuids::uuid>
    given_for(const boost::uuids::uuid& collection_id) const {
        std::lock_guard lock(mutex_);
        const auto found = given_.find(collection_id);
        return found == given_.end() ? std::nullopt : std::optional(found->second);
    }

    feed_run_identity_ports ports_;
    ores::service::service::cache::run_token_cache tokens_;
    mutable std::mutex mutex_;
    std::map<boost::uuids::uuid, boost::uuids::uuid> given_;
};

}

#endif
